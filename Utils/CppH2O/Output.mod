(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Convert Cpp Header Into Oberon-2 Definition Module            *)
(*                                                                           *)
(* MODULE:     Output                                      V 1.40.13         *)
(*                                                         2001OCT23         *)
(*  PURPOSE:   Procedures to write to the output file, an oberon-2 module    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   WriteChar                                                               *)
(*   WriteLn                                                                 *)
(*   WriteString                                                             *) 
(*   WriteNumber                                                             *)
(*   WriteIndent                                                             *)
(*   WritePosition                                                           *)
(*   WriteToken                                                              *)
(*   Open                                                                    *)
(*   Close                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    UGNS    Eugene Shcherbatyuk, ugns@mail.ru                     *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  CREATED    2000NOV08                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000NOV08                                                               *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Output;

IMPORT
  Strings, SYSTEM,
  WinBase, WinCon, WinDef, WinNT,
  ConvertParam;


CONST
  FirstLine            =                1;
  FirstColumn          =                1;
  CommentColumn        =               60;
  ParameterColumn      =               40;
  ProcedureColumn      =               39;

  TokenO2Comment1*     =                1;
  TokenO2Comment2*     =                2;
  TokenO2Const*        =                3;
  TokenO2Type*         =                4;
  TokenO2Record*       =                5;
  TokenO2Var*          =                6;
  TokenO2End*          =                7;
  TokenO2Proc*         =               10;
  

TYPE
  TPosition  = RECORD
     Line,
     Column:                           LONGINT;
  END (* TPosition *);
  TLineP     =                         POINTER TO TLine;
  TLine      = RECORD
    Text:                              ARRAY 256 OF CHAR;
    Next:                              TLineP;
  END (* TLine *);
  

VAR
  CrLf:                                ARRAY 3 OF CHAR;
  ActIndentation:                      INTEGER;
  FirstConstLineP,
  FirstTypeLineP,
  FirstVarLineP,
  FirstProcLineP:                 TLineP;
  ActConstLineP,
  ActTypeLineP,
  ActVarLineP,
  ActProcLineP:                   TLineP;
  MyMode:                              LONGINT;
  Position:                            TPosition;
  m, mode:                             LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  StatusQuo:                           LONGINT;
  OutputFile:                          WinDef.HANDLE;
  OutputFileName,
  OberonModule:                        ARRAY 132 OF CHAR;
  File_Security:                       WinBase.SECURITY_ATTRIBUTES;
  TokenO2List:                         ARRAY 100 OF ConvertParam.TokenString;
  
  MyTime:                              WinBase.SYSTEMTIME;
  NameOfMonth:                         ARRAY 13 OF ARRAY 4 OF CHAR;


(*****************************************************************************)
(*                                                                           *)
(* WriteChar                                                                 *)
(* Writes a char to the output file                                          *)
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

PROCEDURE WriteChar*                  (TheChar:            CHAR)
                                      :                    LONGINT;

VAR 
  BytesWritten:                        LONGINT;
  ResultBool:                          WinDef.BOOL;

BEGIN

  CASE MyMode OF
    TokenO2Const:
      Strings.AppendChar(ActConstLineP^.Text, TheChar);
    | (* TokenO2Const *)
    TokenO2Type:
      Strings.AppendChar(ActTypeLineP^.Text, TheChar);
    | (* TokenO2Type *)
    TokenO2Var:
      Strings.AppendChar(ActVarLineP^.Text, TheChar);
    | (* TokenO2Var *)
    TokenO2Proc:
      Strings.AppendChar(ActProcLineP^.Text, TheChar);
    (* TokenO2Proc *)
    ELSE
      ;
  END (* CASE MyMode  *);
  
  INC(Position.Column);

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

PROCEDURE WriteLn*                    ()
                                      :                    LONGINT;

VAR
  Result:                              LONGINT;
  
BEGIN;
  
  CASE MyMode OF
    TokenO2Const:
      NEW(ActConstLineP^.Next);
      ActConstLineP            := ActConstLineP^.Next;
      ActConstLineP^.Text[0]   := 0X;
      ActConstLineP^.Next      := NIL;
    | (* TokenO2Const *)
    TokenO2Type:
      NEW(ActTypeLineP^.Next);
      ActTypeLineP             := ActTypeLineP^.Next;
      ActTypeLineP^.Text[0]    := 0X;
      ActTypeLineP^.Next       := NIL;
    | (* TokenO2Type *)
    TokenO2Var:
      NEW(ActVarLineP^.Next);
      ActVarLineP              := ActVarLineP^.Next;
      ActVarLineP^.Text[0]     := 0X;
      ActVarLineP^.Next        := NIL;
    | (* TokenO2Var *)
    TokenO2Proc:
      NEW(ActProcLineP^.Next);
      ActProcLineP            := ActProcLineP^.Next;
      ActProcLineP^.Text[0]   := 0X;
      ActProcLineP^.Next      := NIL;
    (* TokenO2Proc *)
    ELSE
      ;
  END (* CASE MyMode  *);
  
  INC(Position.Line);
  Position.Column      :=  FirstColumn;

  RETURN 0;
  
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

PROCEDURE WriteString*                (VAR TheString-:     ARRAY OF CHAR)
                                      :                    LONGINT;

BEGIN;
  
  CASE MyMode OF
    TokenO2Const:
      Strings.Append(ActConstLineP^.Text, TheString);
    | (* TokenO2Const *)
    TokenO2Type:
      Strings.Append(ActTypeLineP^.Text, TheString);
    | (* TokenO2Type *)
    TokenO2Var:
      Strings.Append(ActVarLineP^.Text, TheString);
    | (* TokenO2Var *)
    TokenO2Proc:
      Strings.Append(ActProcLineP^.Text, TheString);
    (* TokenO2Proc *)
    ELSE
      ;
  END (* CASE MyMode *);
  
  INC(Position.Column, Strings.Length(TheString));

  RETURN 0;
  
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

PROCEDURE WriteNumber*                (Number:             LONGINT)
                                      :                    LONGINT;

VAR
  Index,
  Result:                              LONGINT;
  NumberString:                        ARRAY 32 OF CHAR;
  
BEGIN;

  CASE MyMode OF
    TokenO2Const:
      Strings.Str(Number, NumberString);
      Strings.Append(ActConstLineP^.Text, NumberString);
    | (* TokenO2Type *)
    TokenO2Type:
      Strings.Str(Number, NumberString);
      Strings.Append(ActTypeLineP^.Text, NumberString);
    | (* TokenO2Type *)
    TokenO2Var:
      Strings.Str(Number, NumberString);
      Strings.Append(ActVarLineP^.Text, NumberString);
    | (* TokenO2Type *)
    TokenO2Proc:
      Strings.Str(Number, NumberString);
      Strings.Append(ActProcLineP^.Text, NumberString);
    (* TokenO2Type *)
    ELSE
      ;
  END;
  
  INC(Position.Column, Strings.Length(NumberString));
  
  RETURN 0;
  
END WriteNumber;


(*****************************************************************************)
(*                                                                           *)
(* WriteIndent                                                               *)
(* Writes some blanks to set indentation                                     *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   Mode      use one value out of TokenO2...                               *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteIndent*                (Mode:               LONGINT)
                                      :                    LONGINT;

VAR 
  Index,
  EndIndex, 
  Result:                              LONGINT;

BEGIN

  CASE Mode OF
    TokenO2Comment1, TokenO2Comment2:
      EndIndex := CommentColumn-Position.Column;
      IF EndIndex>0 THEN
        FOR Index:=1 TO EndIndex DO
          Result := WriteChar(" ");
        END (* FOR Index:=1 TO ... *);
      ELSE
        Result := WriteString("     ");
      END (* IF EndIndex>0 *);
    |
    TokenO2Const, TokenO2Type, TokenO2Record:
      EndIndex := ParameterColumn-Position.Column;
      IF EndIndex>0 THEN
        FOR Index:=1 TO EndIndex DO
          Result := WriteChar(" ");
          END (* FOR Index:=1 TO ... *);
      ELSE
        Result := WriteString(" ");
      END (* IF EndIndex>0 *);
    |
    TokenO2Proc:
      EndIndex := ProcedureColumn-Position.Column;
      IF EndIndex>0 THEN
        FOR Index:=1 TO EndIndex DO
          Result := WriteChar(" ");
          END (* FOR Index:=1 TO ... *);
      ELSE
        Result := WriteString(" ");
      END (* IF EndIndex>0 *);
    ELSE
      FOR Index:=1 TO ActIndentation DO
        Result := WriteChar(" ");
      END;
    END (* CASE Mode OF *);
    
  RETURN 0;
  
END WriteIndent;


(*****************************************************************************)
(*                                                                           *)
(* WritePosition                                                             *)
(* Writes a position information into the output file                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  ActLine    actual position (line)                                        *)
(*  ActColumn                  (column)                                      *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WritePosition*              (ActLine,
                                       ActColumn:          LONGINT)
                                      :                    LONGINT;

VAR
  ActLineP:                            TLineP;
  NumberString:                        ARRAY 32 OF CHAR;
  Result:                              LONGINT;
  
BEGIN;

  CASE MyMode OF
    TokenO2Const:
      ActLineP := ActConstLineP;
    | (* TokenO2Const *)
    TokenO2Type:
      ActLineP := ActTypeLineP;
    | (* TokenO2Type *)
    TokenO2Var:
      ActLineP := ActVarLineP;
    | (* TokenO2Var *)
    TokenO2Proc:
      ActLineP := ActProcLineP;
    (* TokenO2Proc *)
    ELSE
      RETURN 0;
  END;
  Strings.Append(ActLineP^.Text, " [");
  Strings.Str(ActLine, NumberString);
  Strings.Append(ActLineP^.Text, NumberString);
  INC(Position.Column, Strings.Length(NumberString)+2);
  Strings.Append(ActLineP^.Text, ", ");
  Strings.Str(ActColumn, NumberString);
  Strings.Append(ActLineP^.Text, NumberString);
  INC(Position.Column, Strings.Length(NumberString)+4);
  Strings.Append(ActLineP^.Text, "] ");
  
  RETURN Result
  
END WritePosition;


(*****************************************************************************)
(*                                                                           *)
(* WriteToken (TheToken: LONGINT)                                            *)
(* Writes a Token to the output file                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  TheToken   LONGINT designating the token                                 *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteToken*                 (TheToken:           LONGINT)
                                      :                    LONGINT;

VAR
  BytesWritten:                        LONGINT;
  ResultBool:                          WinDef.BOOL;
  AString:                             ConvertParam.TokenString;
  
BEGIN;

  IF StatusQuo=TheToken THEN
    RETURN 0;
  END;

  CASE TheToken OF
    TokenO2Comment1:
      AString          := TokenO2List[TheToken];
      Result           := WriteString(AString);
      INC(Position.Column, Strings.Length(TokenO2List[TheToken]));
    |
    TokenO2Comment2:
      AString          := TokenO2List[TheToken];
      Result           := WriteString(AString);
      INC(Position.Column, Strings.Length(TokenO2List[TheToken]));
    |
    TokenO2Const:
      MyMode           := TheToken;
(*      AString          := TokenO2List[TheToken];                           *)
(*      Result           := WriteString(AString);                            *)
(*      Result           := WriteLn();                                       *)
      StatusQuo        := TheToken;
      ActIndentation   :=  2;
    |
    TokenO2End:
      ActIndentation   :=  2;
      Result           := WriteIndent(0);
      AString          := TokenO2List[TheToken];
      Result           := WriteString(AString);
      INC(Position.Column, Strings.Length(TokenO2List[TheToken]));
    |
    TokenO2Type:
      MyMode           := TheToken;
(*      AString          := TokenO2List[TheToken];                           *)
(*      Result           := WriteString(AString);                            *)
(*      Result           := WriteLn();                                       *)
      StatusQuo        := TheToken;
      ActIndentation   :=  2;
    |
    TokenO2Record:
      AString          := TokenO2List[TheToken];
      Result           := WriteString(AString);
      INC(Position.Column, Strings.Length(TokenO2List[TheToken]));
      ActIndentation   :=  6;
    |
    TokenO2Var:
      MyMode           := TheToken;
(*      AString          := TokenO2List[TheToken];                           *)
(*      Result           := WriteString(AString);                            *)
(*      Result           := WriteLn();                                       *)
      StatusQuo        := TheToken;
      ActIndentation   :=  2;
    |  
    TokenO2Proc:
      MyMode           := TheToken;
      Result           := WriteLn();
      AString          := TokenO2List[TheToken];
      Result           := WriteString(AString);
      Position.Column  := Strings.Length(TokenO2List[TheToken]) + 1;
      StatusQuo        :=  0;
      ActIndentation   :=  0;

    ELSE
(*      AString          := TokenO2List[TheToken];                           *)
(*      Result           := WriteString(AString);                            *)
    ;
  END (* CASE TheToken OF *);

  RETURN 0;
  
END WriteToken;


(*****************************************************************************)
(*                                                                           *)
(* Open                                                                      *)
(* open output file                                                          *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  FileName                                                                 *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Open*                       (FileName:           ARRAY OF CHAR)
                                      :                    LONGINT;
                                      
VAR
  i, j:                                LONGINT;

  
BEGIN;

  COPY (FileName, OberonModule);
  Strings.Delete(OberonModule, Strings.Length(OberonModule)-3, 4);
  COPY (FileName, OutputFileName); 

  WinBase.GetLocalTime(MyTime);
  OutputFile := WinBase.CreateFileA    (SYSTEM.ADR(FileName),
                                       WinNT.GENERIC_WRITE,
                                       0,
                                       File_Security,
                                       WinBase.CREATE_ALWAYS,
                                       WinNT.FILE_ATTRIBUTE_NORMAL,
                                       0);

  IF OutputFile=-1 THEN                                    (* Fehler beim Öffnen *)
    RETURN WinBase.GetLastError();
  ELSE
    (* Write Header for Oberon-2 Definition module *)
    Result := WriteString("(*****************************************************************************)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(* Project:    WindowsProgram                                                *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(* Module:     ");
    Result := WriteString(OberonModule);
    j      := 32 - Strings.Length(OberonModule);
    IF j>0 THEN
      FOR i:=1 TO j DO
        Result := WriteChar(" ");
      END;
    END;
    Result := WriteString("            V 1.00.00         *)");
    Result := WriteLn();
    Result := WriteString("(*                                                         ");
    Result := WriteNumber(MyTime.wYear);
    Result := WriteString(NameOfMonth[MyTime.wMonth]);
    IF MyTime.wDay<10 THEN
      Result := WriteChar("0")
    END;
    Result := WriteNumber(MyTime.wDay);
    Result := WriteString("         *)");
    Result := WriteLn();
    Result := WriteString("(*  PURPOSE:   interface to WinAPI library                                   *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(*  FUNCTIONS:                                                               *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(* Authors:    KlS    schultze-schoenberg@t-online.de                        *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(* Configuration Management                                                  *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(*  created    2000SEP11                                                     *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(*  update                                                                   *)");
    Result := WriteLn();
    Result := WriteString("(*   2000SEP16 KlS    change request 01.001 fixed                            *)");
    Result := WriteLn();
    Result := WriteString("(*                    short description                                      *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(*  release                                                                  *)");
    Result := WriteLn();
    Result := WriteString("(*                                                                           *)");
    Result := WriteLn();
    Result := WriteString("(*****************************************************************************)");
    Result := WriteLn();
    Result := WriteLn();
    Result := WriteString("DEFINITION ");
    Result := WriteString(OberonModule);
    Result := WriteChar(";");
    Result := WriteLn();
    Result := WriteLn();
    Result := WriteString("IMPORT");
    Result := WriteLn();
    Result := WriteString("  SYSTEM, WinDef;");
    Result := WriteLn();
    Result := WriteLn();
    Result := WriteLn();
    Result := WriteString("CONST");
    Result := WriteLn();

    RETURN 0
  END;
  
END Open;


(*****************************************************************************)
(*                                                                           *)
(* Close                                                                     *)
(* close output file                                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Close*                      ();

VAR
  BytesWritten:                        LONGINT;
  
BEGIN;

  ActConstLineP  := FirstConstLineP;
  REPEAT
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(ActConstLineP^.Text),
                                      Strings.Length(ActConstLineP^.Text),
                                      BytesWritten,
                                      0);
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(CrLf),
                                      2,
                                      BytesWritten,
                                      0);
    ActConstLineP := ActConstLineP^.Next;
  UNTIL ActConstLineP=NIL;
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);
  
  ActTypeLineP   := FirstTypeLineP;
  REPEAT
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(ActTypeLineP^.Text),
                                      Strings.Length(ActTypeLineP^.Text),
                                      BytesWritten,
                                      0);
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(CrLf),
                                      2,
                                      BytesWritten,
                                      0);
    ActTypeLineP := ActTypeLineP^.Next;
  UNTIL ActTypeLineP=NIL;
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);
  
  ActVarLineP    := FirstVarLineP;
  REPEAT
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(ActVarLineP^.Text),
                                      Strings.Length(ActVarLineP^.Text),
                                      BytesWritten,
                                      0);
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(CrLf),
                                      2,
                                      BytesWritten,
                                      0);
    ActVarLineP  := ActVarLineP^.Next;
  UNTIL ActVarLineP=NIL;
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);
  
  ActProcLineP  := FirstProcLineP;
  REPEAT
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(ActProcLineP^.Text),
                                      Strings.Length(ActProcLineP^.Text),
                                      BytesWritten,
                                      0);
    ResultBool   := WinBase.WriteFile(OutputFile,
                                      SYSTEM.ADR(CrLf),
                                      2,
                                      BytesWritten,
                                      0);
    ActProcLineP := ActProcLineP^.Next;
  UNTIL ActProcLineP=NIL;
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);

  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR("END "),
                                    4,
                                    BytesWritten,
                                    0);
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(OberonModule),
                                    Strings.Length(OberonModule),
                                    BytesWritten,
                                    0);
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(". "),
                                    1,
                                    BytesWritten,
                                    0);
  ResultBool   := WinBase.WriteFile(OutputFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);
  ResultBool := WinBase.CloseHandle(OutputFile);
  
END Close;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN

  CrLf[0]                      := CHR(0DH);
  CrLf[1]                      := CHR(0AH);
  CrLf[2]                      := CHR(0);
  NameOfMonth[01]              := "JAN";
  NameOfMonth[02]              := "FEB";
  NameOfMonth[03]              := "MAR";
  NameOfMonth[04]              := "APR";
  NameOfMonth[05]              := "MAY";
  NameOfMonth[06]              := "JUN";
  NameOfMonth[07]              := "JUL";
  NameOfMonth[08]              := "AUG";
  NameOfMonth[09]              := "SEP";
  NameOfMonth[10]              := "OCT";
  NameOfMonth[11]              := "NOV";
  NameOfMonth[12]              := "DEC";
  
  ActIndentation               :=  2;
  StatusQuo                    :=  0;
  MyMode                       :=  TokenO2Const;
  
  Position.Line                := FirstLine;
  Position.Column              := FirstColumn;

  FOR m:=0 TO LEN(TokenO2List)-1 DO;
    TokenO2List[m] := CHR(0);
  END;
  
  TokenO2List[TokenO2Comment1] := "(* ";
  TokenO2List[TokenO2Comment2] := " *)";
  TokenO2List[TokenO2Const]    := "CONST";
  TokenO2List[TokenO2Type]     := "TYPE";
  TokenO2List[TokenO2Proc]     := "PROCEDURE [_APICALL] ";
  TokenO2List[TokenO2Record]   := " = RECORD [_NOTALIGNED]";
  TokenO2List[TokenO2Var]      := "VAR";
  TokenO2List[TokenO2End]      := "END";
  
  NEW(FirstConstLineP);
  FirstConstLineP^.Text[0]     := 0X;
  ActConstLineP                := FirstConstLineP;
  ActConstLineP^.Next          := NIL;
  
  NEW(FirstTypeLineP);
  FirstTypeLineP^.Text[0]      := 0X;
  NEW(FirstTypeLineP^.Next);
  ActTypeLineP                 := FirstTypeLineP^.Next;
  ActTypeLineP^.Text[0]        := 0X;
  NEW(ActTypeLineP^.Next);
  ActTypeLineP                 := ActTypeLineP^.Next;
  ActTypeLineP^.Text           := "TYPE";
  ActTypeLineP^.Next           := NIL;
  NEW(ActTypeLineP^.Next);
  ActTypeLineP                 := ActTypeLineP^.Next;
  ActTypeLineP^.Text[0]        := 0X;
  ActTypeLineP^.Next           := NIL;
  
  NEW(FirstVarLineP);
  FirstVarLineP^.Text[0]       := 0X;
  NEW(FirstVarLineP^.Next);
  ActVarLineP                  := FirstVarLineP^.Next;
  ActVarLineP^.Text[0]         := 0X;
  NEW(ActVarLineP^.Next);
  ActVarLineP                  := ActVarLineP^.Next;
  ActVarLineP^.Text            := "VAR";
  ActVarLineP^.Next            := NIL;
  NEW(ActVarLineP^.Next);
  ActVarLineP                  := ActVarLineP^.Next;
  ActVarLineP^.Text[0]         := 0X;
  ActVarLineP^.Next            := NIL;
  
  NEW(FirstProcLineP);
  FirstProcLineP^.Text[0]      := 0X;
  FirstProcLineP^.Next         := NIL;
  ActProcLineP                 := FirstProcLineP;
  
END Output.

