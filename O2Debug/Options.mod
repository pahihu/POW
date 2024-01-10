(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Output                                      V 1.45.19         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   reading and writing the INI file                              *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   RememberFile                                                            *)
(*             stores a filename                                             *)
(*   ReadINI   loads the parameters                                          *)
(*   WriteINI  saves the parameters                                          *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  CREATED    2003APR06                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000NOV08                                                               *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Options;

IMPORT
  Common, Resource, UIStatusLine,
  Strings, SYSTEM,
  WinBase, WinCon, WinDef, WinNT, WinUser;


CONST
  Version              =               "V 1.45.19";
  Module*              =               "Options";
  ErrorNoOffset        =               Resource.IDM_Options * 100;
  
  MyINIFile            =               "O2Debug.ini";
  

TYPE
  TokenString  =                       ARRAY 80 OF CHAR;
  

VAR
  (* global option parameters *)
  (* ActualPath        =               Common.MyFileDescription.Path*)
  FileNames:                           ARRAY 8 OF ARRAY 256 OF CHAR;

  (* local variables *)
  CrLf:                                ARRAY 3 OF CHAR;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  INIFile:                             WinDef.HANDLE;
  INIFileName:                         ARRAY 256 OF CHAR;
  File_Security:                       WinBase.SECURITY_ATTRIBUTES;
  
  MyTime:                              WinBase.SYSTEMTIME;
  NameOfMonth:                         ARRAY 13 OF ARRAY 4 OF CHAR;


(*****************************************************************************)
(*                                                                           *)
(* RememberFile                                                              *)
(* Reads until a lf character is found                                       *)
(* all characters are stored in the variable, the "cursor"                   *)
(* stands behind the 0DX character                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  FileName   path and filename                                             *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  stored                                                    *)
(*              1  file existed already                                      *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE RememberFile*               (FileName:           ARRAY OF CHAR)
                                      :                    LONGINT;
                                      
VAR
  BytesRead:                           LONGINT;
  Done:                                BOOLEAN;
  Index:                               INTEGER;
  Result:                              LONGINT;
  
BEGIN
  
  Index            :=  0;
  
  WHILE FileNames[Index, 0]#0X DO
    INC(Index);
  END (* WHILE FileNames[Index, 0]#0X  *);
  IF Index<8 THEN
    COPY (FileName, FileNames[Index]);
  END (* IF Index>8  *);
  
  RETURN 0
   
END RememberFile;


(*****************************************************************************)
(*                                                                           *)
(*             Read  INI File                                                *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(* ReadLine                                                                  *)
(* Reads until a lf character is found                                       *)
(* all characters are stored in the variable, the "cursor"                   *)
(* stands behind the 0DX character                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  AString    the string which was read until the lf was found              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadLine                    (VAR AString:        ARRAY OF CHAR)
                                      :                    LONGINT;
                                      
VAR
  BytesRead:                           LONGINT;
  Done:                                BOOLEAN;
  Index:                               INTEGER;
  Result:                              LONGINT;
  
BEGIN
  
  Index            :=  0;
  
  REPEAT
    ResultBool := WinBase.ReadFile(INIFile,
                                   SYSTEM.ADR(AString[Index]),
                                   1,
                                   BytesRead,
                                   0);
    IF BytesRead=0 THEN
      AString[Index] := 0X;
      Done           := TRUE;
    ELSIF AString[Index]=0DX THEN
      AString[Index] := 0X;
      Done           := TRUE;
    ELSE
      INC(Index);
    END (* IF BytesRead=0 *);
  UNTIL Done;

  RETURN Index;
  
END ReadLine;


(*****************************************************************************)
(*                                                                           *)
(* ReadINI                                                                   *)
(* reads the INI file into the "global option parameters"                    *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadINI*                    ()
                                      :LONGINT;
                                      
VAR
  ErrorCode,
  Result,
  i, j:                                LONGINT;
  MyString:                            ARRAY 256 OF CHAR;
  MyValue:                             ARRAY 256 OF CHAR;

  
BEGIN;

  Result         := WinBase.GetModuleFileNameA(WinDef.NULL, SYSTEM.ADR(MyString), 256);
  Result         := WinBase.GetFullPathNameA(SYSTEM.ADR(MyString), 256, SYSTEM.ADR(INIFileName), SYSTEM.ADR(i));
  i              := i - SYSTEM.ADR(INIFileName);
  INIFileName[i] := 0X;
  Strings.Append (INIFileName, MyINIFile); 

  INIFile        := WinBase.CreateFileA (SYSTEM.ADR(INIFileName),
                                         WinNT.GENERIC_READ,
                                         0,
                                         File_Security,
                                         WinBase.OPEN_EXISTING,
                                         WinNT.FILE_ATTRIBUTE_NORMAL,
                                         0);

  IF INIFile=-1 THEN                                    (* Fehler beim Öffnen *)
    ErrorCode := WinBase.GetLastError();
    IF ErrorCode=1400 THEN
      UIStatusLine.ShowMessage ("No INI File found.");
    ELSE
      UIStatusLine.DisplayError (1, 90010);
    END;
    RETURN WinBase.GetLastError();
  END;

  WHILE ReadLine(MyString)>0 DO
    i  := Strings.Pos("=", MyString, 1);
    IF i>0 THEN
      Strings.Copy(MyString, MyValue, i+1, Strings.Length(MyString)-i);
    END (* IF i>0  *);

    (* read entries *)
    IF Strings.Pos("ActualPath", MyString, 0)>0 THEN
      COPY(MyValue, Common.MyFileDescription.Path);
    ELSE
      ;
    END (* IF Strings.Pos("ActualPath", MyString)>0  *);

  END (* WHILE ReadLine(MyString>0  *);
  
  ResultBool := WinBase.CloseHandle(INIFile);
  
  RETURN 0

END ReadINI;


(*****************************************************************************)
(*                                                                           *)
(*             Write INI File                                                *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(* WriteChar                                                                 *)
(* Writes a char to the INI file                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  TheChar    Character to write                                            *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteChar                   (TheChar:            CHAR)
                                      :                    LONGINT;

VAR 
  BytesWritten:                        LONGINT;
  ResultBool:                          WinDef.BOOL;

BEGIN

  ResultBool   := WinBase.WriteFile(INIFile,
                                    SYSTEM.ADR(TheChar),
                                    1,
                                    BytesWritten,
                                    0);

  RETURN 0;
  
END WriteChar;


(*****************************************************************************)
(*                                                                           *)
(* WriteLn                                                                   *)
(* Writes a carriage return & linefeed to the output file                    *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  -                                                                        *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLn                     ()
                                      :LONGINT;

VAR
  Result:                              LONGINT;
  
BEGIN;
  
  Result   := WriteChar(CrLf[0]);
  Result   := WriteChar(CrLf[1]);

  RETURN Result;
  
END WriteLn;


(*****************************************************************************)
(*                                                                           *)
(* WriteString                                                               *)
(* Writes a string to the output file                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  TheString  String to write to file                                       *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteString                 (VAR TheString-:     ARRAY OF CHAR)
                                      :LONGINT;

VAR
  Index,
  Result:                              LONGINT;
  
BEGIN;
  
  FOR Index:=0 TO Strings.Length(TheString)-1 DO
    Result   := WriteChar(TheString[Index]);
  END;
  
  RETURN Result;
  
END WriteString;


(*****************************************************************************)
(*                                                                           *)
(* WriteNumber                                                               *)
(* Writes a number to the output file                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  TheNumber  Number to write to file                                       *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteNumber                 (Number:             LONGINT)
                                      :                    LONGINT;

VAR
  Index,
  Result:                              LONGINT;
  NumberString:                        ARRAY 32 OF CHAR;
  
BEGIN;

  Strings.Str(Number, NumberString);
  RETURN WriteString(NumberString);
  
END WriteNumber;


(*****************************************************************************)
(*                                                                           *)
(* WriteINI                                                                  *)
(* writes the "global option parameters" to the INI file                     *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteINI*                   ()
                                      :LONGINT;
                                      
VAR
  i, j:                                LONGINT;

  
BEGIN;

  COPY (Common.CurrentDirectory, INIFileName); 
  Strings.Append (INIFileName, MyINIFile);

  WinBase.GetLocalTime(MyTime);
  INIFile      := WinBase.CreateFileA (SYSTEM.ADR(INIFileName),
                                       WinNT.GENERIC_WRITE,
                                       0,
                                       File_Security,
                                       WinBase.CREATE_ALWAYS,
                                       WinNT.FILE_ATTRIBUTE_NORMAL,
                                       0);

  IF INIFile=-1 THEN                                    (* Fehler beim Öffnen *)
    UIStatusLine.DisplayError (1, 90020);
    HALT(0);
    RETURN WinBase.GetLastError();
  END (* IF INIFile=-1 *);

(* Write Header *)
  Result     := WriteString("[* O2Debug INI file / ");
  Result     := WriteNumber(MyTime.wYear);
  Result     := WriteString(NameOfMonth[MyTime.wMonth]);
  IF MyTime.wDay<10 THEN
    Result := WriteChar("0")
  END;
  Result     := WriteNumber(MyTime.wDay);
  Result     := WriteString(" *]");
  Result     := WriteLn();
  
(* Write Files section *)
  Result     := WriteString("[Files]");
  Result     := WriteLn();
  Result     := WriteString("ActualPath=");
  Result     := WriteString(Common.MyFileDescription.Path);
  Result     := WriteLn();
  FOR i:= 0 TO 7 DO
    IF FileNames[i, 0]#0X THEN
      Result     := WriteString("File[");
      Result     := WriteNumber(i);
      Result     := WriteString("]=");
      Result     := WriteString(FileNames[i]);
      Result     := WriteLn();
    END (* IF FileNames[i, 0]#0X *);
  END (* FOR i:= 0 TO 7  *);

  ResultBool := WinBase.CloseHandle(INIFile);
  
  UIStatusLine.ShowMessage (INIFileName);

  RETURN 0
  
END WriteINI;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN
  
  FileNames[0, 0]                    :=  0X;
  FileNames[1, 0]                    :=  0X;
  FileNames[2, 0]                    :=  0X;
  FileNames[3, 0]                    :=  0X;
  FileNames[4, 0]                    :=  0X;
  FileNames[5, 0]                    :=  0X;
  FileNames[6, 0]                    :=  0X;
  FileNames[7, 0]                    :=  0X;
             
  CrLf[0]                            := CHR(0DH);
  CrLf[1]                            := CHR(0AH);
  CrLf[2]                            := CHR(0);

  NameOfMonth[01]                    := "JAN";
  NameOfMonth[02]                    := "FEB";
  NameOfMonth[03]                    := "MAR";
  NameOfMonth[04]                    := "APR";
  NameOfMonth[05]                    := "MAY";
  NameOfMonth[06]                    := "JUN";
  NameOfMonth[07]                    := "JUL";
  NameOfMonth[08]                    := "AUG";
  NameOfMonth[09]                    := "SEP";
  NameOfMonth[10]                    := "OCT";
  NameOfMonth[11]                    := "NOV";
  NameOfMonth[12]                    := "DEC";
  
END Options.

