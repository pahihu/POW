(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     FileHnd                                     V 2.00.37         *)
(*                                                         2005JUN10         *)
(*  PURPOSE:   This module contains the procedures which read from and write *)
(*             to text files.                                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(* Author(s):                                                                *)
(*                     Michael Bogner, Max Mayrbäurl                         *)
(*             BL      Bernhard Leisch                                       *)
(*                     Alexander Bergsmann                                   *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   1998                                                                    *)
(*                                                                           *)
(*  update                                                                   *)
(*   2001OCT03 KlS     SaveFile:                                             *)
(*                     automatic version incrementation "V 2.00.37" is       *)
(*                     incremented while saving the file                     *)
(*   2004OCT17 KlS     LoadFile:                                             *)
(*                     a file with a line length of more than 2048 is loaded *)
(*                     without crashing POW!                                 *)
(*                     but you cannot do anything with this file             *)
(*   2005FEB09 KlS     LoadFile:                                             *)
(*                     sets the language configuration                       *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(* Comments                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE FileHnd;


IMPORT
  ListSt, Syntax, TextWin, Options, GlobWin, 
  Strings, Utils, 
  WinBase, WinDef, WinNT, WinUser, 
  SYSTEM;


CONST
  FILEPARTLEN          =               32000;
  CR                   =               0DX;
  LF                   =               0AX;
  EOF                  =               1AX;


VAR
  filepart:                            ARRAY FILEPARTLEN OF CHAR;
  (* This variable should be local to LoadFile. It has been declared 
       as a global variable to reduce stack usage. *)


  (*****************************************************************************)
PROCEDURE LoadFile*                   (hEdit:              WinDef.HWND;
                                       name:               WinDef.LPSTR)
                                      :INTEGER;
  (* öffnet bestimmte Datei und lädt sie in den Speicher unter Verwendung des  *)
  (* Moduls ListStruct                                                         *)
  (* Rückgabewert : 1 (erfolgreich), 0 (Fehler)                                *)

VAR
  Finished:                            BOOLEAN;
  i,
  j:                                   LONGINT;
  char:                                CHAR;
  filepos:                             LONGINT;
  pos:                                 INTEGER;
  lcnt:                                LONGINT;
  hf:                                  WinDef.HANDLE;
  buffer:                              ARRAY ListSt.MAXLENGTH OF CHAR;
  FileName:                            ARRAY 256 OF CHAR;
  fplen:                               WinDef.UINT;
  win:                                 TextWin.WinDesc;
  valstr:                              ARRAY 10 OF CHAR;
  InfoString:                          ARRAY 60 OF CHAR;
  InfoStr2:                            ARRAY 30 OF CHAR;
  Done:                                WinDef.BOOL;
  dwRead:                              WinDef.DWORD;
  (* Anzahl der gelesenen Bytes *)
  MySyntaxColouring:                   Options.SyntaxColouringP;

  (*-------------------------------------------------------------------------*)
PROCEDURE NextChar                    (VAR ch:             CHAR)
                                      :BOOLEAN;
  (* liest Datei von Festplatte in Teilen und liefert ein Zeichen *)
BEGIN
  IF filepos>=FILEPARTLEN THEN
    (* nächsten Teil lesen *)
    Done       := WinBase.ReadFile(hf, SYSTEM.ADR(filepart), FILEPARTLEN, dwRead, WinDef.NULL);
    IF (dwRead=0) THEN
      RETURN FALSE
    END (* IF (dwRead=0) *);
    filepos    := 0;
    (* Position auf 1.Zeichen in filepart setzen *)
  END (* IF filepos>=FILEPARTLEN *);
  IF filepos>=dwRead THEN
    RETURN FALSE
  END (* IF filepos>=dwRead *);
  ch           := filepart[filepos];
  INC(filepos);
  RETURN TRUE;
END NextChar;

(*-------------------------------------------------------------------------*)
PROCEDURE PrepExit                    ();
(* Vorbereitung für plötzliches Exit der Prozedur *)
BEGIN
  IF (WinBase.CloseHandle(hf)=0) THEN
    GlobWin.DisplayError("error in filehandling", "could not close file");
  END (* IF (WinBase.CloseHandle(hf)=0) *);
END PrepExit;

(*---------------------------------------------------------------------------*)
BEGIN
  win          := SYSTEM.VAL(TextWin.WinDesc, WinUser.GetWindowLongA(hEdit, 0));
  win.ScreenConfig;
  lcnt         := 0;
  SYSTEM.MOVE(name, SYSTEM.ADR(FileName), LEN(FileName));
  (* POW initialisiert die Listenstruktur bevor Öffnen der Datei durch WM_CREATE Nachricht *)
  hf           := WinBase.CreateFileA(name, 
  WinNT.GENERIC_READ, 
  0, 
  NIL, 
  WinBase.OPEN_EXISTING, 
  WinNT.FILE_ATTRIBUTE_NORMAL, 
  WinDef.NULL);

  IF hf=WinBase.INVALID_HANDLE_VALUE THEN
    GlobWin.DisplayError("Error in MODULE Filehandling", "Could not open file for reading");
    RETURN 0;
  END (* IF hf=WinBase.INVALID_HANDLE_VA *);
  filepos      := FILEPARTLEN;
  buffer       := "";

  pos          := 0;
  WHILE NextChar(char) DO
    IF pos>=ListSt.MAXLENGTH THEN
      Strings.Str(LONG(ListSt.MAXLENGTH), valstr);
      InfoString := "FileHnd: Line is longer than ";
      Strings.Append(InfoString, valstr);
      Strings.Append(InfoString, " chars; file is truncated.");
      GlobWin.DisplayError("Do Not Edit!", InfoString);
      pos      := ListSt.MAXLENGTH - 5;
      (* 2004OCT17, KlS *)
      buffer[pos] := ">";
      INC(pos);
      buffer[pos] := ">";
      INC(pos);
      buffer[pos] := ">";
      INC(pos);
      buffer[pos] := 0X;
      IF ~win.text.AddLine(buffer) THEN
        PrepExit;
        RETURN 0
      END (* IF ~win.text.AddLine(buffer) *) ;
      pos      := 0;
      INC(lcnt);
      (* 2004OCT17, KlS *)
    END (* IF pos>=ListSt.MAXLENGTH *) ;
    IF char=LF THEN
      (* Zeilenende ist erreicht *)
      IF (pos>0) & (buffer[pos - 1]=CR) THEN
        DEC(pos)
      END (* IF (pos>0) & (buffer[pos - 1]=C *) ;
      buffer[pos] := 0X;
      IF ~win.text.AddLine(buffer) THEN
        PrepExit;
        RETURN 0
      END (* IF ~win.text.AddLine(buffer) *) ;
      pos      := 0;
      INC(lcnt);
    ELSIF (char=09X) & Options.useTabs THEN
      i        := 0;
      WHILE (pos<ListSt.MAXLENGTH - 1) & (i<Options.tabsize) DO
        buffer[pos] := " ";
        INC(pos);
        INC(i);
      END (* WHILE (pos<ListSt.MAXLENGTH - 1 *);
    ELSE
      buffer[pos] := char;
      INC(pos);
    END (* IF char=LF *) ;
  END (* WHILE NextChar(char) *);

  (* Ende der Datei ist erreicht *)
  IF (pos>0) & (buffer[pos - 1]=EOF) THEN
    DEC(pos);
  END (* IF (pos>0) & (buffer[pos - 1]=E *) ;
  IF pos#0 THEN
    buffer[pos] := 0X;
    IF ~win.text.AddLine(buffer) THEN
      PrepExit();
      RETURN 0;
    END (* IF ~win.text.AddLine(buffer) *) ;
  END (* IF pos#0 *);

  SYSTEM.MOVE(name, SYSTEM.ADR(win.Name), LEN(win.Name));
  i            := Strings.Length(win.Name) - 1;
  (* set language configuration *)
  Options.ActSyntaxColouring := Options.AnchorSyntaxColouring;
  win.SyntaxColouring := Options.AnchorSyntaxColouring;
  MySyntaxColouring := Options.AnchorSyntaxColouring;
  i            := Strings.Length(FileName) - 1;
  Finished     := FALSE;
  WHILE i>0 DO
    ;
    (* extract file extension => InfoString *)
    IF FileName[i]="." THEN
      Strings.Copy(FileName, InfoString, i + 1, Strings.Length(FileName) - i);
      Strings.Copy(FileName, win.Extension, i + 1, Strings.Length(FileName) - i);
      i        := 1;
    END (* IF FileName[i]=" *) ;
    DEC(i);
  END (* WHILE i>0 *) ;
  WHILE MySyntaxColouring#NIL DO
    i          := Strings.PosChar(".", MySyntaxColouring.Extension, 0);
    WHILE i>0 DO
      j        := Strings.PosChar(",", MySyntaxColouring.Extension, i);
      IF j=0 THEN
        j      := Strings.Length(MySyntaxColouring.Extension) + 1;
      END (* IF j=0 *) ;
      Strings.Copy(MySyntaxColouring.Extension, InfoStr2, i, j-i);
      IF (Strings.Length(InfoString)=Strings.Length(InfoStr2))
       & (Syntax.PosInFileName(InfoString, InfoStr2, 1)>0) THEN
        Options.ActSyntaxColouring := MySyntaxColouring;
        win.SyntaxColouring := MySyntaxColouring;
        MySyntaxColouring := NIL;
        i      := 0;
      ELSE
        i      := Strings.PosChar(".", MySyntaxColouring.Extension, i + 1);
      END (* IF (Strings.Length(InfoString)= *) ;
    END (* WHILE i>0 *) ;
    IF MySyntaxColouring#NIL THEN
      MySyntaxColouring := MySyntaxColouring.Next;
    END (* IF MySyntaxColouring#NIL *) ;
  END (* WHILE MySyntaxColouring#NIL *) ;

  PrepExit();
  win.ScreenConfig();
  (* configure output-preferences *)
  Done         := WinUser.InvalidateRect(hEdit, NIL, WinDef.True);
  Done         := WinUser.UpdateWindow(hEdit);
  (* forciert WM_PAINT-Nachricht *)
  win.ShowTextRange(1, win.text.lines);
  RETURN 1;
END LoadFile;


(*****************************************************************************)
PROCEDURE SaveFile*                   (hEdit:              WinDef.HWND;
                                       name:               WinDef.LPSTR)
                                      :INTEGER;
(* Speichert bestimmte Datei vom Speicher auf Festplatte *)
(* Rückgabewert : 1 (erfolgreich), 0 (Fehler)            *)
(*  2001OCT03  KlS     automatic version incrementation  *)
(*                                                       *)

VAR
  hf:                                  WinDef.HANDLE;
  len:                                 LONGINT;
  char:                                CHAR;
  buffer:                              ARRAY ListSt.MAXLENGTH OF CHAR;
  win:                                 TextWin.WinDesc;
  crlf:                                ARRAY 3 OF CHAR;
  dwWritten:                           WinDef.DWORD;
  (* Anzahl geschriebener Bytes *)
  (* 2001OCT03, KlS *)
  i:                                   INTEGER;
  (* Index counts the lines *)
  j:                                   INTEGER;
  Version,
  Release,
  Build:                               LONGINT;
  Found,
  SetDate:                             BOOLEAN;
  Day,
  Month,
  Year:                                INTEGER;
  Number:                              ARRAY 32 OF CHAR;
  Position,
  Position0:                           LONGINT;


BEGIN
  crlf[0]      := CR;
  crlf[1]      := LF;
  crlf[2]      := 0X;
  win          := SYSTEM.VAL(TextWin.WinDesc, WinUser.GetWindowLongA(hEdit, 0));

  hf           := WinBase.CreateFileA(name, WinNT.GENERIC_WRITE, 0, NIL, WinBase.CREATE_ALWAYS, 
  WinNT.FILE_ATTRIBUTE_NORMAL, WinDef.NULL);

  IF hf=WinBase.INVALID_HANDLE_VALUE THEN
    GlobWin.DisplayError("Error in MODULE Filehandling.", "Could not open file for saving. Save aborted.");
    RETURN 0;
  END (* IF hf=WinBase.INVALID_HANDLE_VA *);

  IF ~(win.text.GetLine(1, buffer, len)) THEN
    RETURN 0
  END (* IF ~(win.text.GetLine(1, buffer *);

  IF (WinBase.WriteFile(hf, SYSTEM.ADR(buffer), len, dwWritten, WinDef.NULL)=0) THEN
    GlobWin.DisplayError("Error in MODULE Filehandling", "Could not write into opened file");
    RETURN 0;
  END (* IF (WinBase.WriteFile(hf, SYSTE *);
  IF (WinBase.WriteFile(hf, SYSTEM.ADR(crlf), 2, dwWritten, WinDef.NULL)=0) THEN
    GlobWin.DisplayError("Error in MODULE Filehandling", "Could not write into opened file");
    RETURN 0;
  END (* IF (WinBase.WriteFile(hf, SYSTE *);

  i            := 100;
  Position0    := 0;
  Found        := FALSE;
  SetDate      := FALSE;
  Utils.GetDate(Day, Month, Year, j);

  WHILE win.text.GetNextLine(buffer, len) DO
    (* zeilenweise schreiben in Datei *)
    IF i>0 THEN
      IF SetDate THEN
        (* KlS, 2001OCT03 *)
        Position := Position0 - 1;
        LOOP
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END (* IF ((buffer[Position]<") OR (bu *);
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END (* IF ((buffer[Position]<") OR (bu *);
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END (* IF ((buffer[Position]<") OR (bu *);
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END (* IF ((buffer[Position]<") OR (bu *);
          (* four digits in a row: must be the year *)

          Position := Position0 - 1;
          (* write actual year *)
          Strings.Str(Year, Number);
          buffer[Position] := Number[0];
          buffer[Position + 1] := Number[1];
          buffer[Position + 2] := Number[2];
          buffer[Position + 3] := Number[3];
          (* write actual month *)
          CASE Month OF
            1:
              (* january *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "A";
              buffer[Position+6] := "N";
            | (* 1 *)
            2:
              (* february *)
              buffer[Position+4] := "F";
              buffer[Position+5] := "E";
              buffer[Position+6] := "B";
            | (* 2 *)
            3:
              (* marcg *)
              buffer[Position+4] := "M";
              buffer[Position+5] := "A";
              buffer[Position+6] := "R";
            | (* 3 *)
            4:
              (* april *)
              buffer[Position+4] := "A";
              buffer[Position+5] := "P";
              buffer[Position+6] := "R";
            | (* 4 *)
            5:
              (* may *)
              buffer[Position+4] := "M";
              buffer[Position+5] := "A";
              buffer[Position+6] := "Y";
            | (* 5 *)
            6:
              (* june *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "U";
              buffer[Position+6] := "N";
            | (* 6 *)
            7:
              (* july *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "U";
              buffer[Position+6] := "L";
            | (* 7 *)
            8:
              (* august *)
              buffer[Position+4] := "A";
              buffer[Position+5] := "U";
              buffer[Position+6] := "G";
            | (* 8 *)
            9:
              (* september *)
              buffer[Position+4] := "S";
              buffer[Position+5] := "E";
              buffer[Position+6] := "P";
            | (* 9 *)
            10:
              (* october *)
              buffer[Position+4] := "O";
              buffer[Position+5] := "C";
              buffer[Position+6] := "T";
            | (* 10 *)
            11:
              (* november *)
              buffer[Position+4] := "N";
              buffer[Position+5] := "O";
              buffer[Position+6] := "V";
            | (* 11 *)
            12:
              (* december *)
              buffer[Position+4] := "D";
              buffer[Position+5] := "E";
              buffer[Position+6] := "C";
              (* 12 *)
            ELSE
              (* unknown *)
              buffer[Position+4] := "x";
              buffer[Position+5] := "x";
              buffer[Position+6] := "x";
          END (* CASE Month *);
          (* write actual day *)
          Strings.Str(Day, Number);
          IF Day<10 THEN
            buffer[Position + 7] := "0";
            buffer[Position + 8] := Number[0];
          ELSE
            buffer[Position + 7] := Number[0];
            buffer[Position + 8] := Number[1];
          END (* IF Day<10 *) ;
          EXIT;

        END (* LOOP *) ;
        SetDate := FALSE;
      END (* IF SetDate *);

      (* find version string *)
      Position := Strings.Pos("V ", buffer, 0);
      IF Position>0 THEN
        LOOP
          Position0 := Position;
          INC(Position, 2);
          IF buffer[Position]#"." THEN
            EXIT
          END (* IF buffer[Position]#" *);
          INC(Position, 3);
          IF buffer[Position]#"." THEN
            EXIT
          END (* IF buffer[Position]#" *);
          IF ~Found THEN
            (* read version, release, build *)
            Position := Position0 + 1;
            Number[0] := buffer[Position];
            Number[1] := 0X;
            Version := Strings.Val(Number);
            INC(Position, 2);
            Number[0] := buffer[Position];
            Number[1] := buffer[Position + 1];
            Number[2] := 0X;
            Release := Strings.Val(Number);
            INC(Position, 3);
            Number[0] := buffer[Position];
            Number[1] := buffer[Position + 1];
            Number[2] := 0X;
            Build := Strings.Val(Number);

            (* increment the version info *)
            INC(Build);
            IF Build>99 THEN
              Build := 0;
              INC(Release);
              IF Release>99 THEN
                Release := 0;
                INC(Version)
              END (* IF Release>99 *);
            END (* IF Build>99 *);
            Found := TRUE;
            SetDate := TRUE;
          END (* IF ~Found *) ;

          (* write version, release, build *)
          Position := Position0 + 1;
          Strings.Str(Version, Number);
          buffer[Position] := Number[0];
          INC(Position, 2);
          Strings.Str(Release, Number);
          IF Number[1]=0X THEN
            buffer[Position] := "0";
            buffer[Position + 1] := Number[0];
          ELSE
            buffer[Position] := Number[0];
            buffer[Position + 1] := Number[1];
          END (* IF Number[1]=0X *);
          INC(Position, 3);
          Strings.Str(Build, Number);
          IF Number[1]=0X THEN
            buffer[Position] := "0";
            buffer[Position + 1] := Number[0];
          ELSE
            buffer[Position] := Number[0];
            buffer[Position + 1] := Number[1];
          END (* IF Number[1]=0X *);
          EXIT;

        END (* LOOP *) ;
      END (* IF Position>0 *);
      DEC(i);
    END (* IF i>0 *);

    IF (WinBase.WriteFile(hf, SYSTEM.ADR(buffer), len, dwWritten, WinDef.NULL)=0) THEN
      GlobWin.DisplayError("Error in MODULE Filehandling", "Could not write into opened file");
      RETURN 0;
    END (* IF (WinBase.WriteFile(hf, SYSTE *);
    (* Zeilenumbruch schreiben *)
    IF (WinBase.WriteFile(hf, SYSTEM.ADR(crlf), 2, dwWritten, WinDef.NULL)=0) THEN
      GlobWin.DisplayError("Error in MODULE Filehandling", "Could not write into opened file");
      RETURN 0;
    END (* IF (WinBase.WriteFile(hf, SYSTE *);

  END (* WHILE win.text.GetNextLine(buff *);

  IF WinBase.CloseHandle(hf)=0 THEN
    GlobWin.DisplayError("Error in MODULE Filehandling", "Could not close specified file");
    RETURN 0;
  END (* IF WinBase.CloseHandle(hf)=0 *);

  RETURN 1;

END SaveFile;


(*****************************************************************************)
(* holt Edit Buffer in Teilen, Maximal size Bytes werden in buf kopiert               *)
(* liefert aktuelle Größe zurück, EOF ist erreicht, wenn zurückgeliefert Wert < Größe *)
PROCEDURE GetNextBuffer*              (hEdit:              WinDef.HWND;
                                       VAR buf:            ARRAY OF CHAR;
                                       size:               LONGINT)
                                      :LONGINT;

VAR
  i,
  min:                                 LONGINT;
  len:                                 LONGINT;
  buffer:                              ARRAY ListSt.MAXLENGTH OF CHAR;
  win:                                 TextWin.WinDesc;

BEGIN
  win          := SYSTEM.VAL(TextWin.WinDesc, WinUser.GetWindowLongA(hEdit, 0));
  i            := 0;
  IF win.position= - 1 THEN
    buf[0]     := 0DX;
    buf[1]     := 0AX;
    win.position := 0;
    i          := 2;
  ELSIF win.position= - 2 THEN
    buf[0]     := 0AX;
    win.position := 0;
    i          := 1;
  ELSIF win.position= - 3 THEN
    buf[0]     := 0X;
    RETURN 1;
  END (* IF win.position= - 1 *);
  IF ~(win.text.GetLine(win.lineNbr, buffer, len)) THEN
    buf[0]     := 0X;
    RETURN 1;
  END (* IF ~(win.text.GetLine(win.lineN *);
  WHILE i<size DO
    WHILE (i<size) & (win.position<len) DO
      buf[i]   := buffer[win.position];
      INC(i);
      INC(win.position);
    END (* WHILE (i<size) & (win.position< *);
    IF win.position>=len THEN
      win.position := 0;
      INC(win.lineNbr);
      IF i<size THEN
        buf[i] := 0DX;
        INC(i)
      ELSE
       win.position :=  - 1;
        RETURN i
      END (* IF i<size *);
      IF i<size THEN
        buf[i] := 0AX;
        INC(i)
      ELSE
       win.position :=  - 2;
        RETURN i
      END (* IF i<size *);
    END (* IF win.position>=len *);
    IF (i<size) & ~(win.text.GetNextLine(buffer, len)) THEN
      IF i<size THEN
        buf[i] := 0X;
        INC(i)
      ELSE
       win.position :=  - 3
      END (* IF i<size *);
      RETURN i;
    END (* IF (i<size) & ~(win.text.GetNex *);

  END (* WHILE i<size *);
  RETURN i;
END GetNextBuffer;


(*****************************************************************************)
(* holt Edit Buffer in Teilen, Maximal size Bytes werden in buf kopiert               *)
(* liefert aktuelle Größe zurück, EOF ist erreicht, wenn zurückgeliefert Wert < Größe *)
(* win.position : 0- : inx in Line Buffer                                             *)
(*                -1 : CR LF fehlen                                                   *)
(*                -2 : LF fehlt                                                       *)
(*                -3 : OX fehlt                                                       *)
PROCEDURE GetFirstBuffer*             (hEdit:              WinDef.HWND;
                                       VAR buf:            ARRAY OF CHAR;
                                       size:               LONGINT)
                                      :LONGINT;

VAR
  win:                                 TextWin.WinDesc;
BEGIN
  win          := SYSTEM.VAL(TextWin.WinDesc, WinUser.GetWindowLongA(hEdit, 0));
  win.lineNbr  := 1;
  win.position := 0;
  RETURN GetNextBuffer(hEdit, buf, size);
END GetFirstBuffer;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
  ;
END FileHnd.

