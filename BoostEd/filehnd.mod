(******************************************************************************)
(*  Module FileHnd                                                            *)
(*                                                                            *)
(*  This module contains the procedures which read from and write to          *)
(*  text files.                                                               *)
(*----------------------------------------------------------------------------*)
(*  1.10   2001OCT03   KlS     SaveFile:                                      *)
(*                             automatic version incrementation               *)
(*                             "V 1.23.46" is incremented while saving        *)
(*                             the file                                       *)
(*                                                                            *)
(******************************************************************************)

MODULE FileHnd;


IMPORT 
  SYSTEM, 
  WU:=WinUser, WD:=WinDef, WB:=WinBase, WN:=WinNT,
  Strings, Utils,
  List:=ListSt, TWin:=TextWin, Options, GlobWin;


CONST 
  FILEPARTLEN          =               32000;
  CR                   =               0DX;
  LF                   =               0AX;
  EOF                  =               1AX;


VAR
  filepart:                            ARRAY FILEPARTLEN OF CHAR; 
    (* This variable should be local to LoadFile. It has been declared 
       as a global variable to reduce stack usage. *)
    

(**********************************************************************************************)
PROCEDURE DisplayError(title: ARRAY OF CHAR; msg: ARRAY OF CHAR);
(* zeigt eine Messagebox an mit einer Fehlermeldung *)

VAR r: LONGINT;

BEGIN
  (* Messagebox anzeigen *)
  r := WU.MessageBoxA(WD.NULL, SYSTEM.ADR(msg), SYSTEM.ADR(title), WU.MB_OK);
END DisplayError;


(**********************************************************************************************)
PROCEDURE LoadFile*(hEdit:WD.HWND;name:WD.LPSTR):INTEGER;
(* öffnet bestimmte Datei und lädt sie in den Speicher unter Verwendung des Moduls ListStruct *)
(* Rückgabewert : 1 (erfolgreich), 0 (Fehler)                                                 *)

VAR
  i            : INTEGER; 
  char         : CHAR;
  filepos      : LONGINT;
  pos          : INTEGER;   
  lcnt         : LONGINT;
  hf           : WD.HANDLE;
  buffer       : ARRAY List.MAXLENGTH OF CHAR;
  filename     : ARRAY 100 OF CHAR;
  fplen        : WD.UINT;
  win          : TWin.WinDesc;
  valstr       : ARRAY 10 OF CHAR;
  infostr      : ARRAY 60 OF CHAR;
  infostr2     : ARRAY 30 OF CHAR;
  done         : WD.BOOL;
  dwRead       : WD.DWORD; (* Anzahl der gelesenen Bytes *)
  
  PROCEDURE NextChar(VAR ch:CHAR):BOOLEAN;
  (* liest Datei von Festplatte in Teilen und liefert ein Zeichen *)
  BEGIN
    IF filepos>=FILEPARTLEN THEN (* nächsten Teil lesen *)
        done := WB.ReadFile(hf, SYSTEM.ADR(filepart), FILEPARTLEN, dwRead, WD.NULL);
        IF (dwRead = 0) THEN RETURN FALSE END;
        filepos:=0; (* Position auf 1.Zeichen in filepart setzen *)
    END;
    IF filepos >= dwRead THEN RETURN FALSE END;
    ch:= filepart[filepos]; 
    INC(filepos);
    RETURN TRUE;
  END NextChar;
    
  PROCEDURE PrepExit();
  (* Vorbereitung für plötzliches Exit der Prozedur *)
  BEGIN
    IF (WB.CloseHandle(hf) = 0) THEN
      DisplayError("error in filehandling","could not close file");
    END;
  END PrepExit;
  
BEGIN   
  win:=SYSTEM.VAL(TWin.WinDesc,WU.GetWindowLongA(hEdit,0));
  win.ScreenConfig; 
  lcnt:=0;
  (* POW initialisiert die Listenstruktur bevor Öffnen der Datei durch WM_CREATE Nachricht *)
  hf := WB.CreateFileA(name, WN.GENERIC_READ, 0, NIL, WB.OPEN_EXISTING,
                       WN.FILE_ATTRIBUTE_NORMAL, WD.NULL);

  IF hf = WB.INVALID_HANDLE_VALUE THEN
    DisplayError("Error in MODULE Filehandling","Could not open file for reading");
    RETURN 0;
  END;
  filepos:=FILEPARTLEN; 
  buffer:="";

  pos:=0;
  WHILE NextChar(char) DO
    IF pos >=List.MAXLENGTH THEN   
      Strings.Str(LONG(List.MAXLENGTH), valstr);
      infostr:="Line is longer than ";
      infostr2:=" chars. Loading aborted.";
      Strings.Append(infostr,valstr);Strings.Append(infostr,infostr2);
      GlobWin.DisplayError("Error in MODULE Filehandling",infostr);
      PrepExit;
    END;
    IF char=LF THEN  (* Zeilenende ist erreicht *)
      IF (pos>0) & (buffer[pos-1]=CR) THEN DEC(pos) END;
      buffer[pos]:=0X;
      IF ~win.text.AddLine(buffer) THEN PrepExit; RETURN 0 END;     
      pos:=0;
      INC(lcnt);
    ELSIF (char=09X) & Options.useTabs THEN 
      i:=0;
      WHILE (pos<List.MAXLENGTH-1) & (i<Options.tabsize) DO
        buffer[pos]:=" ";
        INC(pos);
        INC(i);
      END;
    ELSE
      buffer[pos]:=char;      
      INC(pos);
    END;
  END; (* WHILE *)    
  (* Ende der Datei ist erreicht *)
  IF (pos>0) & (buffer[pos-1]=EOF) THEN DEC(pos) END;
  IF pos#0 THEN
    buffer[pos]:= 0X; 
    IF ~win.text.AddLine(buffer) THEN PrepExit; RETURN 0 END;
  END;    
  PrepExit;
  win.ScreenConfig;  (* configure output-preferences *) 
  done := WU.InvalidateRect(hEdit,NIL,WD.True);  
  done := WU.UpdateWindow(hEdit); (* forciert WM_PAINT-Nachricht *)
  win.ShowTextRange(1,win.text.lines);
  RETURN 1;
END LoadFile;


(**********************************************************************************************)
PROCEDURE SaveFile*(hEdit:WD.HWND; name: WD.LPSTR):INTEGER;
(* Speichert bestimmte Datei vom Speicher auf Festplatte *)
(* Rückgabewert : 1 (erfolgreich), 0 (Fehler)            *)
(*  2001OCT03  KlS     automatic version incrementation  *)
(*                                                       *)
     
VAR 
  hf:                                  WD.HANDLE;
  len:                                 LONGINT;
  char:                                CHAR;
  buffer:                              ARRAY List.MAXLENGTH OF CHAR;
  win:                                 TWin.WinDesc;
  crlf:                                ARRAY 3 OF CHAR;     
  dwWritten:                           WD.DWORD;  (* Anzahl geschriebener Bytes *)
                                                           (* 2001OCT03, KlS *)
  i:                                   INTEGER;            (* Index counts the lines *)
  j:                                   INTEGER;
  Version,
  Release,
  Build:                               LONGINT;
  Found,
  SetDate:                             BOOLEAN;
  Day,
  Month,
  Year:                                INTEGER;
  Number:                              ARRAY   32 OF CHAR;
  Position,
  Position0:                           LONGINT;
  

BEGIN
  crlf[0]:=CR;
  crlf[1]:=LF;
  crlf[2]:=0X;
  win:=SYSTEM.VAL(TWin.WinDesc,WU.GetWindowLongA(hEdit,0));
 
  hf       := WB.CreateFileA(name, WN.GENERIC_WRITE, 0, NIL, WB.CREATE_ALWAYS,
                             WN.FILE_ATTRIBUTE_NORMAL, WD.NULL);

  IF hf=WB.INVALID_HANDLE_VALUE THEN
    GlobWin.DisplayError("Error in MODULE Filehandling.","Could not open file for saving. Save aborted.");
    RETURN 0;    
  END;
              
  IF ~(win.text.GetLine(1, buffer, len)) THEN 
    RETURN 0 
  END;
      
  IF (WB.WriteFile(hf, SYSTEM.ADR(buffer), len, dwWritten, WD.NULL)=0) THEN
    GlobWin.DisplayError("Error in MODULE Filehandling","Could not write into opened file");
    RETURN 0; 
  END;
  IF (WB.WriteFile(hf, SYSTEM.ADR(crlf), 2, dwWritten, WD.NULL)=0) THEN
    GlobWin.DisplayError("Error in MODULE Filehandling","Could not write into opened file");
    RETURN 0; 
  END;     
  
  i            := 100;
  Position0    :=   0;
  Found        := FALSE;
  SetDate      := FALSE;
  Utils.GetDate (Day, Month, Year, j);

  WHILE win.text.GetNextLine( buffer, len) DO
    (* zeilenweise schreiben in Datei *)
    IF i>0 THEN
      IF SetDate THEN
        Position := Position0 - 1;
        LOOP
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END;
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END;
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END;
          INC(Position);
          IF ((buffer[Position]<"0") OR (buffer[Position]>"9")) THEN
            EXIT
          END;
          (* four digits in a row: must be the year *)
  
          Position               := Position0 - 1;
          (* write actual year *)
          Strings.Str(Year, Number);
          buffer[Position]   := Number[0];
          buffer[Position+1] := Number[1];
          buffer[Position+2] := Number[2];
          buffer[Position+3] := Number[3];
          (* write actual month *)
          CASE Month OF
            1:                                               (* january *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "A";
              buffer[Position+6] := "N";
            | (*  1 *)
            2:                                               (* february *)
              buffer[Position+4] := "F";
              buffer[Position+5] := "E";
              buffer[Position+6] := "B";
            | (*  2 *)
            3:                                               (* marcg *)
              buffer[Position+4] := "M";
              buffer[Position+5] := "A";
              buffer[Position+6] := "R";
            | (*  3 *)
            4:                                               (* april *)
              buffer[Position+4] := "A";
              buffer[Position+5] := "P";
              buffer[Position+6] := "R";
            | (*  4 *)
            5:                                               (* may *)
              buffer[Position+4] := "M";
              buffer[Position+5] := "A";
              buffer[Position+6] := "Y";
            | (*  5 *)
            6:                                               (* june *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "U";
              buffer[Position+6] := "N";
            | (*  6 *)
            7:                                               (* july *)
              buffer[Position+4] := "J";
              buffer[Position+5] := "U";
              buffer[Position+6] := "L";
            | (*  7 *)
            8:                                               (* august *)
              buffer[Position+4] := "A";
              buffer[Position+5] := "U";
              buffer[Position+6] := "G";
            | (*  8 *)
            9:                                               (* september *)
              buffer[Position+4] := "S";
              buffer[Position+5] := "E";
              buffer[Position+6] := "P";
            | (*  9 *)
            10:                                              (* october *)
              buffer[Position+4] := "O";
              buffer[Position+5] := "C";
              buffer[Position+6] := "T";
            | (* 10 *)
            11:                                              (* november *)
              buffer[Position+4] := "N";
              buffer[Position+5] := "O";
              buffer[Position+6] := "V";
            | (* 11 *)
            12:                                              (* december *)
              buffer[Position+4] := "D";
              buffer[Position+5] := "E";
              buffer[Position+6] := "C";
            (* 12 *)
            ELSE                                             (* unknown *)
              buffer[Position+4] := "x";
              buffer[Position+5] := "x";
              buffer[Position+6] := "x";
          END;
          (* write actual day *)
          Strings.Str(Day, Number);
          IF Day<10 THEN
            buffer[Position+7] := "0";
            buffer[Position+8] := Number[0];
          ELSE
            buffer[Position+7] := Number[0];
            buffer[Position+8] := Number[1];
          END (* IF Day<10 *);
          EXIT;
          
        END (* LOOP *);
        SetDate  := FALSE;
      END;
      
      (* find version string *)
      Position := Strings.Pos("V ", buffer, 0);
      IF Position>0 THEN
        LOOP
          Position0 := Position;
          INC(Position, 2);
          IF buffer[Position]#"." THEN
            EXIT
          END;
          INC(Position, 3);
          IF buffer[Position]#"." THEN
            EXIT
          END;
          IF ~Found THEN
            (* read version, release, build *)
            Position   := Position0 + 1;
            Number[0]  := buffer[Position];
            Number[1]  := 0X;
            Version    := Strings.Val(Number);
            INC(Position, 2);
            Number[0]  := buffer[Position];
            Number[1]  := buffer[Position+1];
            Number[2]  := 0X;
            Release    := Strings.Val(Number);
            INC(Position, 3);
            Number[0]  := buffer[Position];
            Number[1]  := buffer[Position+1];
            Number[2]  := 0X;
            Build      := Strings.Val(Number);
            
            (* increment the version info *)
            INC(Build);
            IF Build>99 THEN
              Build := 0;
              INC(Release);
              IF Release>99 THEN
                Release := 0;
                INC(Version)
              END;
            END;
            Found    := TRUE;
            SetDate  := TRUE;
          END (* IF ~Found *);
  
          (* write version, release, build *)
          Position               := Position0 + 1;
          Strings.Str(Version, Number);
          buffer[Position]   := Number[0];
          INC(Position, 2);
          Strings.Str(Release, Number);
          IF Number[1]=0X THEN
            buffer[Position]   := "0";
            buffer[Position+1] := Number[0];
          ELSE
            buffer[Position]   := Number[0];
            buffer[Position+1] := Number[1];
          END;
          INC(Position, 3);
          Strings.Str(Build, Number);
          IF Number[1]=0X THEN
            buffer[Position]   := "0";
            buffer[Position+1] := Number[0];
          ELSE
            buffer[Position]   := Number[0];
            buffer[Position+1] := Number[1];
          END;
          EXIT;
  
        END (* LOOP *);
      END;
      DEC(i);
    END;
    
    IF (WB.WriteFile(hf, SYSTEM.ADR(buffer), len, dwWritten, WD.NULL) = 0) THEN
         GlobWin.DisplayError("Error in MODULE Filehandling","Could not write into opened file");
         RETURN 0; END;                 
    (* Zeilenumbruch schreiben *)
    IF (WB.WriteFile(hf, SYSTEM.ADR(crlf), 2, dwWritten, WD.NULL) = 0) THEN
         GlobWin.DisplayError("Error in MODULE Filehandling","Could not write into opened file");
         RETURN 0; END;     
   
  END;    

  IF WB.CloseHandle(hf) = 0 THEN 
      GlobWin.DisplayError("Error in MODULE Filehandling","Could not close specified file");
      RETURN 0;
  END;

  RETURN 1;
    
END SaveFile;

(**********************************************************************************************)
PROCEDURE GetNextBuffer*(hEdit:WD.HWND; 
                         VAR buf:ARRAY OF CHAR; 
                         size:LONGINT):LONGINT;
(* holt Edit Buffer in Teilen, Maximal size Bytes werden in buf kopiert               *)
(* liefert aktuelle Größe zurück, EOF ist erreicht, wenn zurückgeliefert Wert < Größe *)

VAR
  i,min    : LONGINT;
  len      : LONGINT;
  buffer   : ARRAY List.MAXLENGTH OF CHAR;   
  win      : TWin.WinDesc;

BEGIN
  win:=SYSTEM.VAL(TWin.WinDesc,WU.GetWindowLongA(hEdit,0));
  i:=0;
  IF win.position=-1 THEN
    buf[0]:=0DX;
    buf[1]:=0AX;
    win.position:=0;
    i:=2;
  ELSIF win.position=-2 THEN
    buf[0]:=0AX;
    win.position:=0;
    i:=1;
  ELSIF win.position=-3 THEN
    buf[0]:=0X;
    RETURN 1;
  END;
  IF ~(win.text.GetLine(win.lineNbr, buffer, len)) THEN 
    buf[0]:=0X;
    RETURN 1;
  END;
  WHILE i<size DO
    WHILE (i<size) & (win.position<len) DO
      buf[i]:=buffer[win.position];
      INC(i);
      INC(win.position);
    END;
    IF win.position>=len THEN
      win.position:=0;
      INC(win.lineNbr);
      IF i<size THEN buf[i]:=0DX; INC(i) ELSE win.position:=-1; RETURN i END;
      IF i<size THEN buf[i]:=0AX; INC(i) ELSE win.position:=-2; RETURN i END;
    END;
    IF (i<size) & ~(win.text.GetNextLine(buffer, len)) THEN 
      IF i<size THEN buf[i]:=0X; INC(i) ELSE win.position:=-3 END;
      RETURN i;
    END;  (* last Elem is reached *)
  END;
  RETURN i;
END GetNextBuffer;

(**********************************************************************************************)

PROCEDURE GetFirstBuffer*(hEdit:WD.HWND;VAR buf:ARRAY OF CHAR; size:LONGINT):LONGINT;
(* holt Edit Buffer in Teilen, Maximal size Bytes werden in buf kopiert               *)
(* liefert aktuelle Größe zurück, EOF ist erreicht, wenn zurückgeliefert Wert < Größe *)
(* win.position : 0- : inx in Line Buffer                                             *)
(*                -1 : CR LF fehlen                                                   *)
(*                -2 : LF fehlt                                                       *)
(*                -3 : OX fehlt                                                       *)

VAR 
  win : TWin.WinDesc;
BEGIN
  win:=SYSTEM.VAL(TWin.WinDesc,WU.GetWindowLongA(hEdit,0));
  win.lineNbr :=1;
  win.position:=0;
  RETURN GetNextBuffer(hEdit,buf,size);
END GetFirstBuffer;


(***********************************************************************************************)   

END FileHnd.
