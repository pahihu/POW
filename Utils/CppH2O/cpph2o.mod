(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Convert Cpp Header Into Oberon-2 Definition Module            *)
(*                                                                           *)
(* MODULE:     CppH2O                                      V 1.40.11         *)
(*                                                         2001NOV05         *)
(*  PURPOSE:   Main Program C++ Header File Conversion                       *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Run                                                                     *)
(*   WinMain() calls initialization function, processes message loop         *)
(*                                                                           *)
(*  USAGE:     CppH2O example.h                                              *)
(*                                                                           *)
(*  INPUT:     Cpp Header File                                               *)
(*                                                                           *)
(*  OUTPUT:    Oberon-2 Definition Module                                    *)
(*                                                                           *)
(* COMMENT:    a simple tool to convert C++ header files into Oberon-2.      *)
(*             converts                                                      *)
(*             define                                                        *)
(*             typedef struct, enum,                                         *)
(*             __stdcall       ( needs manual work)                          *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:            Eugene Shcherbatyuk, ugns@mail.ru                     *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  CREATED    2000NOV08                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2001JAN05 KlS     in case of an error the position is printed           *)
(*   2001JAN06 KlS     array handling implemented                            *)
(*   2001MAR13 KlS     procedure handling implemented                        *)
(*   2001OCT14 KlS     groups CONST, TYPE and VAR declarations               *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE CppH2O;

IMPORT
  Param, Strings, SYSTEM,
  WinBase, WinCon, WinDef,
  ConvertParam, Input, Output;

TYPE
  FileNameT    =                       POINTER TO FileNameS;
  FileNameS    =                       ARRAY 256 OF CHAR;

VAR
  CrLf:                                ARRAY  3 OF CHAR;
  EmptyString:                         ARRAY 64 OF CHAR;
  Result:                              LONGINT;
  ResultBool:                          BOOLEAN;
  ResultWinBool:                       WinDef.BOOL;
  LastToken:                           ARRAY ConvertParam.TokenLength OF CHAR;
  StatusQuo:                           LONGINT;
  m, mode:                             LONGINT;
  StdOut:                              WinDef.HANDLE;
  

(*****************************************************************************)
(*                                                                           *)
(* StatusQuo00                                                               *)
(* Analyse token that was read from file                                     *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  MyToken    Number of token which was read                                *)
(*  MyTokenString                                                            *)
(*  MyStopChar next character after token                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    number of next status                                         *)
(*                                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE StatusQuo00                 (MyToken:            LONGINT;
                                       MyTokenString:      ARRAY OF CHAR;
                                       MyStopChar:         CHAR)
                                      :                    LONGINT;
                                      
VAR
  Result:                              LONGINT;
  ResultBool:                          BOOLEAN;
  TokenString:                         ARRAY ConvertParam.TokenLength OF CHAR;

BEGIN;

  CASE MyToken OF
    Input.TokenCppComment1:
      RETURN 21;
    |
    Input.TokenCppComment3:
      Result    := Output.WriteIndent(0);
      Result    := Output.WriteToken(Output.TokenO2Comment1);
      IF MyStopChar=0DX THEN
        RETURN 28
      ELSE
        RETURN 27
      END;
    |
    Input.TokenCppDefine:
      RETURN 110;
    |
    Input.TokenCppIfDef:
      RETURN 120;
    |
    Input.TokenCppIfnDef:
      RETURN 130;
    |
    Input.TokenCppElse:
      RETURN 140;
    |
    Input.TokenCppElsIf:
      RETURN 150;
    |
    Input.TokenCppEndIf:
      RETURN 160;
    |
    Input.TokenCppTypeDef:
      RETURN 200;
    |
    Input.TokenCppStruct:
      RETURN 210;
    |
    Input.TokenCppEnum:
      RETURN 220;
    |
    Input.TokenCppIn:
      RETURN 230;
    |
    Input.TokenCppOut:
      RETURN 240;
    |
    Input.TokenCpp_CALLBACK:
      RETURN 250;
    |
    Input.TokenCpp_StdCall:
      RETURN 310;
    |
    Input.TokenCpp_DeclSpec:
      RETURN 310;
    ELSE
      ResultBool := ConvertParam.GetParam(MyTokenString, TokenString);
      IF ResultBool THEN
        COPY(TokenString, LastToken);
        RETURN 310
      ELSE
        COPY(TokenString, LastToken);
      END;
      RETURN 0;
  END;
  
END StatusQuo00;


(*****************************************************************************)
(*                                                                           *)
(* Run                                                                       *)
(* Loop to read and convert the data                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  -                                                                        *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  -                                                                        *)
(*                                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Run*                        ();

VAR 
  Done:                                BOOLEAN;
  MyChar:                              CHAR;
  MyString:                            ARRAY 1024 OF CHAR;
  MyNumber,
  MyToken:                             LONGINT;
  MyTokenString,
  MyTokenString2,
  MyTokenString3:                      ARRAY  132 OF CHAR;
  ResultBool:                          BOOLEAN;
  Result:                              LONGINT;

  (***************************************************************************)
  PROCEDURE CharIsLetter(MyChar: CHAR): BOOLEAN;
  BEGIN
    IF ((MyChar>="A") & (MyChar<="Z")) OR
       ((MyChar>="a") & (MyChar<="z")) THEN
      RETURN TRUE
    END;
    RETURN FALSE
  END CharIsLetter;

  (***************************************************************************)
  PROCEDURE CharIsNumber(MyChar: CHAR): BOOLEAN;
  BEGIN
    IF ((MyChar>="0") & (MyChar<="9")) THEN
      RETURN TRUE
    END;
    RETURN FALSE
  END CharIsNumber;

BEGIN;

  Done   := FALSE;

  REPEAT
    
    CASE StatusQuo OF
      0:                                                   (* Read a Token *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF Result=0 THEN
          StatusQuo := StatusQuo00(MyToken, MyTokenString, MyChar);
        ELSE                                               (* Done *)
          StatusQuo := 777;
        END;
      |
      21:                                                  (* Read multi line comment *)
(*        Result     := Output.WriteIndent(0);                               *)
(*        Result     := Output.WriteToken(Output.TokenO2Comment1);           *)
        REPEAT
          Result := Input.ReadChar();
(*          Result := Output.WriteChar(Input.CharacterRead);                 *)
        UNTIL Input.CharacterRead="*";
        StatusQuo  := 28;
      |
      27:                                                  (* Read single line comment *)
        Result     := Input.ReadChar();
        REPEAT
          Result := Output.WriteChar(Input.CharacterRead);
          Result := Input.ReadChar();
        UNTIL Input.CharacterRead=0DX;
        StatusQuo  := 28;
      |
      28:                                                  (* Close comment *)
        Result     := Output.WriteChar(" ");
        Result     := Output.WriteToken(Output.TokenO2Comment2);
        Result     := Output.WriteLn();
        StatusQuo  :=  0;
      |
      110:                                                 (* define *)
        Result     := Input.ReadToken(MyToken, MyTokenString2, MyChar);
        IF MyChar=0DX THEN
          StatusQuo    := 112;
        ELSE
          Result       := Input.ReadToken(MyToken, MyTokenString, MyChar);
          IF MyChar="(" THEN
            Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
            StatusQuo := 111;
          ELSIF MyTokenString="" THEN
            StatusQuo  := 112;
          END (* IF MyChar="(" *);
          StatusQuo := 111;
        END (* IF MyChar=0DX *);
      |
      111:
        CASE MyToken OF
          1..99:                                           (* Obacht, redefining a keyword *)
            StatusQuo  := 116;
          |
          Input.TokenCppAString:
            StatusQuo  := 116;
          |
          Input.TokenCppANumber:
            StatusQuo  := 115;
          ELSE
        Result       := Output.WriteLn();
        Result       := Output.WriteString("##### Neither String nor Number: '");
        Result       := Output.WriteString(MyTokenString);
        Result       := Output.WriteString("'; ");
        Strings.Str(MyToken, MyTokenString3);
        Result       := Output.WriteString(MyTokenString3);
        Result       := Output.WriteLn();
            StatusQuo  := 118;
        END (* CASE MyToken OF *);
      |
      112:                                                 (* Precompiler directive *)
        ResultBool   := ConvertParam.PutParam(MyTokenString2, EmptyString);
        StatusQuo    :=  0;
      |
      115:
        ResultBool   := ConvertParam.PutParam(MyTokenString2, MyTokenString);
        Result       := Output.WriteToken(Output.TokenO2Const);
        Result       := Output.WriteIndent(0);
        Result       := Output.WriteString(MyTokenString2);
        Result       := Output.WriteString("  =  ");
        Result       := Output.WriteIndent(Output.TokenO2Const);
                                                           (* Search for numbers like '0x000001' *)
        MyNumber   := Strings.PosChar("x", MyTokenString, 1);
        IF MyNumber>0 THEN
          Strings.Delete(MyTokenString, 1, MyNumber); 
          Strings.InsertChar("0", MyTokenString, 1); 
          Strings.AppendChar(MyTokenString, "H"); 
        END;
        Result     := Output.WriteString(MyTokenString);
        Result     := Output.WriteChar(";");
        IF MyChar=0DX THEN
          Result     := Output.WriteLn();                  (* Done *)
          StatusQuo  :=   0;
        ELSE
          StatusQuo  := 119;
        END;
      |
      116:                                                 (* Precompiler directive *)
        ResultBool   := ConvertParam.PutParam(MyTokenString2, MyTokenString);
        Result       := Output.WriteIndent(0);
        Result       := Output.WriteToken(Output.TokenO2Comment1);
        Result       := Output.WriteString("##### Precompiler Directive:  ");
        Result       := Output.WriteString(MyTokenString2);
        Result       := Output.WriteString(" ==> ");
        Result       := Output.WriteString(MyTokenString);
        Result       := Output.WriteToken(Output.TokenO2Comment2);
        Result       := Output.WriteLn();
        IF MyChar=0DX THEN
          ;
        ELSE
          Result       := Input.ReadLn(MyString);
        END;
        StatusQuo  :=   0;
      |
      118:
        Result     := Output.WriteLn();
        Result     := Output.WriteString("(* #####  unknown TOKEN '");
          Result    := Output.WriteString(MyTokenString);
          Result    := Output.WriteString("' after #define.");
        Result     := Output.WritePosition(Input.Position.Line, Input.Position.Column);
        Result     := Output.WriteString("*)");
        Result     := Output.WriteLn();
        StatusQuo  :=  0;
      |
      119:
        Result     := Input.ReadLn(MyString);
(*        MyNumber   := Strings.Pos("//", MyString, 1);
        IF MyNumber>0 THEN
          Strings.Delete(MyString, 1, MyNumber+2);
          Result     := Output.WriteIndent(Output.TokenO2Comment1);
          Result     := Output.WriteToken(Output.TokenO2Comment1);
          Result     := Output.WriteString(MyString);
          Result     := Output.WriteToken(Output.TokenO2Comment2);
        END;                                                               *)
        Result     := Output.WriteLn();                                    
        StatusQuo  :=  0;
      |
      120:                                                 (* ifdef *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF ConvertParam.GetParam(MyTokenString, MyTokenString2) THEN
          StatusQuo  :=   0;                               (* token found, do the THEN part *)
        ELSE
          StatusQuo  := 121;                               (* unknown token, read until elese, elsif found *)
        END;
        IF MyChar#0DX THEN
          Result     := Input.ReadLn(MyString)
        END;
      |
      121:                                                 (* read token until else, elsif found *)
        ResultBool := FALSE;
        REPEAT
          Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
          IF MyChar#0DX THEN
            Result := Input.ReadLn(MyString);
          END;
          CASE MyToken OF
            Input.TokenCppElse, Input.TokenCppElsIf, Input.TokenCppEndIf:
              ResultBool := TRUE;
            ELSE
              ;
          END;
        UNTIL ResultBool;
        StatusQuo  :=   0;
      |
      130:                                                 (* ifndef *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF ConvertParam.GetParam(MyTokenString, MyTokenString2) THEN
          StatusQuo  := 131;                               (* known token, read until else, elsif found *)
        ELSE
          StatusQuo  :=   0;                               (* unknown token, do the THEN part *)
        END;
        IF MyChar#0DX THEN
          Result     := Input.ReadLn(MyString);
        END;
      |
      131:                                                 (* read token until else, elsif found *)
        ResultBool := FALSE;
        REPEAT
          Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
          IF MyChar#0DX THEN
            Result     := Input.ReadLn(MyString);
          END;
          CASE MyToken OF
            Input.TokenCppElse, Input.TokenCppElsIf, Input.TokenCppEndIf:
              ResultBool := TRUE;
            ELSE
              ;
          END;
        UNTIL ResultBool;
        StatusQuo  :=   0;
      |
      140:                                                 (* else *)
        Result     := Input.ReadLn(MyString);
        ResultBool := FALSE;
        REPEAT
          Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
          IF MyChar#0DX THEN
            Result     := Input.ReadLn(MyString);
          END;
          IF MyToken=Input.TokenCppEndIf THEN
            ResultBool := TRUE;
          END;
        UNTIL ResultBool;
        StatusQuo  :=   0;
      |
      150:                                                 (* elsif *)
        Result     := Input.ReadLn(MyString);
        StatusQuo  :=  0;
      |
      160:                                                 (* endif *)
        Result     := Input.ReadLn(MyString);              (* Oops! *)
        StatusQuo  :=  0;
      |
      200:                                                 (* typedef *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        CASE MyToken OF
          Input.TokenCppStruct:
            StatusQuo := 210;
          |
          Input.TokenCppEnum:
            StatusQuo := 220;
          |
          Input.TokenCppAString:
            COPY(MyTokenString, LastToken);
            StatusQuo := 230;
          ELSE
            Result     := Output.WriteLn();
            Result     := Output.WriteString("(* #####  unknown TOKEN '");
            Result     := Output.WriteString(MyTokenString);
            Result     := Output.WriteString("' behind typedef.");
            Result     := Output.WritePosition(Input.Position.Line, Input.Position.Column);
            Result     := Output.WriteString("*)");
            Result     := Output.WriteLn();
            StatusQuo  :=  0;
        END;
      |
      210:                                                 (* struct *)
        Result     := Output.WriteToken(Output.TokenO2Type);
        StatusQuo  := 211;
      |
      211:                                                 (* RECORD *)
        Result     := Input.ReadToken(MyToken, MyTokenString3, MyChar);
        IF CharIsLetter(MyTokenString3[0]) THEN
          ;
        ELSE
          Strings.Delete(MyTokenString3, 1, 1);
        END;
        Result     := Output.WriteIndent(0);
        Result     := Output.WriteString(MyTokenString3);
        Result     := Output.WriteToken(Output.TokenO2Record);
        Result     := Output.WriteLn();
        ResultBool := ConvertParam.PutParam(MyTokenString3, MyTokenString3);
        StatusQuo  := 212;
      |
      212:
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyChar="{" THEN
          Result    := Input.ReadLn(MyString);
          StatusQuo := 213;
        ELSE
          Result    := Output.WriteLn();
          Result    := Output.WriteString("(* #####  unknown TOKEN '");
          Result    := Output.WriteString(MyTokenString);
          Result    := Output.WriteString("' in TYPEDEF STRUCT sequence.");
          Result    := Output.WritePosition(Input.Position.Line, Input.Position.Column);
          Result    := Output.WriteString("*)");
          Result    := Output.WriteLn();
          StatusQuo :=  0;
        END;
      |
      213:                                                 (* Read Parameter *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyChar="}" THEN
          Result    := Input.ReadToken(MyToken, MyTokenString3, MyChar);
          StatusQuo := 218;
        ELSIF MyTokenString="" THEN
          ;
        ELSE
          Result     := Input.ReadToken(MyToken, MyTokenString2,  MyChar);
          Result     := Output.WriteIndent(0);
          Result     := Output.WriteString(MyTokenString2);
          Result     := Output.WriteString(":  ");
          Result     := Output.WriteIndent(Output.TokenO2Record);
          ResultBool := ConvertParam.GetParam(MyTokenString, MyTokenString2);
          Result     := Input.ReadLn(MyString);
          MyNumber   := Strings.Pos("[", MyString, 1);
          IF ((MyNumber>0) OR (MyChar="[")) THEN           (* Variable is an array *)
            StatusQuo := 214
          ELSE
            StatusQuo := 215;
          END (* IF ((MyNumber>0) OR (MyChar="[")) *);
        END (* IF MyChar="}" *);
      |
      214:                                                 (* create an array *)
        Result     := Output.WriteString("ARRAY ");
        ResultBool := FALSE;
        MyNumber   :=  0;
        REPEAT
          IF CharIsNumber(MyString[MyNumber]) THEN
            ResultBool := TRUE;
          ELSE
            INC(MyNumber);
          END;
        UNTIL ResultBool;
        ResultBool := FALSE;
        REPEAT
          IF CharIsNumber(MyString[MyNumber]) THEN
            Result     := Output.WriteChar(MyString[MyNumber]);
            INC(MyNumber);
          ELSE
            ResultBool := TRUE;
          END;
        UNTIL ResultBool;
        Result     := Output.WriteString(" OF ");
        StatusQuo  := 215;
      |
      215:                                                 (* write variable's type *)
        Result     := Output.WriteString(MyTokenString2);
        Result     := Output.WriteChar(";");
        Result     := Output.WriteLn();
        StatusQuo  := 213;
      |
      218:
        Result     := Output.WriteToken(Output.TokenO2End);
        Result     := Output.WriteString(" (* ");
        Result     := Output.WriteString(MyTokenString3);
        Result     := Output.WriteString(" *) ;");
        Result     := Output.WriteLn();
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyTokenString[0]="*" THEN
          StatusQuo  := 219;
        ELSE
          Result     := Output.WriteLn();
          StatusQuo  :=   0;
        END;
      |
      219:
        Result     := Output.WriteIndent(0);
        Strings.Delete(MyTokenString, 1, 1);
        Result     := Output.WriteString(MyTokenString);
        Result     := Output.WriteString(" = ");
        Result     := Output.WriteIndent(Output.TokenO2Type);
        Result     := Output.WriteString("POINTER TO ");
        Result     := Output.WriteString(MyTokenString3);
        Result     := Output.WriteChar(";");
        Result     := Output.WriteLn();
        Result     := Output.WriteLn();
        Result     := Input.ReadLn(MyString);
        StatusQuo  :=   0;
      |
      220:                                                 (* enum *)
        Result     := Output.WriteToken(Output.TokenO2Const);
        MyNumber   :=   0;                                 (* the first value of an enumerator is 0 *)
        StatusQuo  := 221;
      |
      221:                                                 (* CONST *)
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyTokenString="" THEN
          Result     := Output.WriteLn();
          Result     := Output.WriteIndent(0);
          Result     := Output.WriteToken(Output.TokenO2Comment1);
          Result     := Output.WriteString("Enumerator");
          Result     := Output.WriteToken(Output.TokenO2Comment2);
          Result     := Output.WriteLn();
          IF MyChar=0DX THEN
            ;
          ELSE
            Result    := Input.ReadLn(MyString);
          END;
          StatusQuo    := 223;
        ELSE
          Strings.Delete(MyTokenString, 1, 1);
          Result     := Output.WriteLn();
          Result     := Output.WriteIndent(0);
          Result     := Output.WriteToken(Output.TokenO2Comment1);
          Result     := Output.WriteString("Enumerator ");
          Result     := Output.WriteString(MyTokenString);
          Result     := Output.WriteToken(Output.TokenO2Comment2);
          Result     := Output.WriteLn();
          StatusQuo  := 222;
        END;
      |
      222:
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyChar="{" THEN
          Result    := Input.ReadLn(MyString);
          StatusQuo := 223;
        ELSE
          Result    := Output.WriteLn();
          Result    := Output.WriteString("(* #####  unknown TOKEN '");
          Result    := Output.WriteString(MyTokenString);
          Result    := Output.WriteString("' in TYPEDEF ENUM sequence.");
          Result    := Output.WritePosition(Input.Position.Line, Input.Position.Column);
          Result    := Output.WriteString("*)");
          Result    := Output.WriteLn();
          StatusQuo :=  0;
        END;
      |
      223:
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF MyChar="}" THEN
            Result    := Input.ReadToken(MyToken, MyTokenString, MyChar);
            IF MyToken=Input.TokenCppAString THEN
              ResultBool := ConvertParam.PutParam(MyTokenString, "LONGINT");
            END;
          Result    := Input.ReadLn(MyString);
          Result    := Output.WriteLn();
          StatusQuo :=  0;
        ELSE
          Result    := Output.WriteIndent(0);
          Result    := Output.WriteString(MyTokenString);
          ResultBool   := ConvertParam.PutParam(MyTokenString, MyTokenString);
          Result    := Output.WriteString("  =  ");
          Result    := Output.WriteIndent(Output.TokenO2Const);
          Result    := Output.WriteNumber(MyNumber);
          Result    := Output.WriteChar(";");
          Result    := Output.WriteLn();
          IF MyChar=CHR(0DH) THEN
            ;
          ELSE
            Result    := Input.ReadLn(MyString);
          END;
          INC(MyNumber);
        END;
      |
      230:                                                 (* IN *)
        Result     := Input.ReadLn(MyString);
        StatusQuo  :=  0;
      |
      240:                                                 (* OUT *)
        Result     := Input.ReadLn(MyString);
        StatusQuo  :=  0;
      |
      250:                                                 (* CALLBACK *)
        StatusQuo  := 310;
      |
      310:                                                 (* PROCEDURE *)
        Result     := Output.WriteToken(Output.TokenO2Proc);
        Result     := Input.ReadToken(MyToken, MyTokenString, MyChar);
        IF Result>0 THEN
          StatusQuo := 777;
        ELSE
          IF MyTokenString[0]="*" THEN
            Strings.Delete(MyTokenString, 1, 1);
          END (*  IF MyTokenString[0]="*" *);
          Result     := Output.WriteString(MyTokenString);
          Result     := Output.WriteIndent(Output.TokenO2Proc);
          Result     := Output.WriteString("(");
          ResultBool := ConvertParam.PutParam(MyTokenString, MyTokenString);
          StatusQuo  := 312;
        END;                        
      |
      311:                                                 (* Indent parameter *)
        Result     := Output.WriteIndent(Output.TokenO2Type);
        StatusQuo  := 312;
      |
      312:                                                 (* Do parameter *)
        Result     := Input.ReadToken(MyToken, MyTokenString2, MyChar);
        IF Result>0 THEN
          StatusQuo := 777;
        ELSE
          CASE MyToken OF
            Input.TokenCppAString:
              ResultBool := ConvertParam.GetParam(MyTokenString2, MyTokenString);
              Result     := Input.ReadToken(MyToken, MyTokenString2, MyChar);
              IF Result>0 THEN
                StatusQuo := 777;
              ELSE
                IF MyTokenString2[0]="*" THEN
                  Result := Output.WriteString("VAR ");
                  Strings.Delete(MyTokenString2, 1, 1);
                  IF Strings.Length(MyTokenString2)=0 THEN
                    Result := Input.ReadToken(MyToken, MyTokenString2, MyChar);
                  END (* IF Strings.Length(MyTokenString2)=0 *);
                END (*  IF MyTokenString2[0]="*" *);
                Result     := Output.WriteString(MyTokenString2);
                Result     := Output.WriteString(":");
                Result     := Output.WriteIndent(Output.TokenO2Comment2);
                Result     := Output.WriteString(MyTokenString);
                IF MyChar="," THEN
                  Result     := Output.WriteString(";");
                  Result     := Input.ReadLn(MyString);
                  Result     := Output.WriteLn();
                  StatusQuo  := 311;
                ELSE
                  StatusQuo := 318
                END (* IF MyChar="," *);
              END (* IF Result>0 *);
            |
            Input.TokenCppBracket2:
              StatusQuo := 318
            |
            Input.TokenCppStar:
              HALT(0);
            ELSE
              CASE MyChar OF
                ")":
                  StatusQuo := 318
                ELSE
                  ;
              END;
          END (* CASE MyToken OF *);
        END;
      |
      318:                                                 (* Procedure Done *)
        Result     := Output.WriteString(")");
        Result     := Output.WriteLn();
        Result     := Output.WriteIndent(Output.TokenO2Proc);
        Result     := Output.WriteString(":");
        Result     := Output.WriteIndent(Output.TokenO2Comment2);
        Result     := Output.WriteString(LastToken);
        Result     := Output.WriteString(";");
        Result     := Output.WriteLn();
        StatusQuo  := 0;
      |
      400:                                                 (* Read a Token *)
        Result     := Output.WriteString("(* #####  Unknown Token '");
        Result     := Output.WriteString(MyTokenString);
        Result     := Output.WritePosition(Input.Position.Line, Input.Position.Column);
        Result     := Output.WriteString(" *)");
        Result     := Output.WriteLn();
        StatusQuo  :=  0;
      |
      777:                                                 (* Done *)
        Done       := TRUE;
      |
      ELSE
        Result     := Output.WriteString("(* ######: wrong status!.");
        Result     := Output.WritePosition(Input.Position.Line, Input.Position.Column);
        Result     := Output.WriteString("*)");
        Result     := Output.WriteLn();
        StatusQuo  :=  0;
    END (* CASE StatusQuo OF *);
    
  UNTIL Done;
  
END Run;

  
(*****************************************************************************)
(*                                                                           *)
(* WinMain                                                                   *)
(* Main Procedure, reading the commandline and open the appropriate files    *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  lpCmdLine  Pointer to Commandline                                        *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] WinMain*         (lpCmdLine:          FileNameT);

VAR
  FileName,
  MyText,
  Parameter:                           ARRAY 132 OF CHAR;
  Position,
  MyLength,
  Result:                              LONGINT;
  i:                                   INTEGER;
  Done:                                BOOLEAN;

BEGIN;

  FileName[0]  := 0X;                  (* read commandline to get the filename *)
  
  FOR i:=1 TO Param.Count() DO
    Param.Str (i, Parameter);
    Position := Strings.Pos(".h", Parameter, 1);
    IF Position=0 THEN                 (* second chance *)
      Position := Strings.Pos(".H", Parameter, 1);
    END;
    IF Position>0 THEN
      Result := Input.Open(Parameter);
      IF Result>0 THEN
        RETURN 
      END;
      IF Strings.Length(FileName)=0 THEN
        COPY (Parameter, FileName);
        Strings.Delete(FileName, Position, 2);
        Strings.Append(FileName, ".MOD");
      END;
    END;
  END;
  
  IF FileName[0]=0X THEN
    MyText         := "FileName not found!";
    ResultWinBool  := WinBase.WriteFile(StdOut,
                                        SYSTEM.ADR(MyText),
                                        Strings.Length(MyText),
                                        Result,
                                        0);
    ResultWinBool  := WinBase.WriteFile(StdOut,
                                        SYSTEM.ADR(CrLf),
                                        2,
                                        Result,
                                        0);
    MyText         := "nor any parameter!!";
    ResultWinBool  := WinBase.WriteFile(StdOut,
                                        SYSTEM.ADR(MyText),
                                        Strings.Length(MyText),
                                        Result,
                                        0);

  ELSE
    Result         := Output.Open(FileName);
    Strings.Delete(FileName, Strings.Length(FileName)-3, 4);
    Result         := ConvertParam.Open("OberonMod.prm", FileName);
    
    Run();
    
    Input.Close();
    Output.Close();
    ConvertParam.Close();
  END;
 
END WinMain;
 

(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;
  
  StdOut           := WinBase.GetStdHandle(WinBase.STD_OUTPUT_HANDLE);
  ResultWinBool    := WinCon.GetConsoleMode(StdOut, mode);
  ResultWinBool    := WinCon.SetConsoleMode(StdOut, mode);

  StatusQuo        :=  0;

  CrLf[0]          := CHR(0DH);
  CrLf[1]          := CHR(0AH);
  CrLf[2]          := CHR(0);

  FOR Result:=0 TO LEN(EmptyString)-1 DO
    EmptyString[Result] :=  0X;
  END;
  
END CppH2O.

