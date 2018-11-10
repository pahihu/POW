(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Convert Cpp Header Into Oberon-2 Definition Module            *)
(*                                                                           *)
(* MODULE:     Input                                       V 1.40.04         *)
(*                                                         2001NOV05         *)
(*  PURPOSE:   Reads the input file, a Cpp Header file                       *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   ReadChar                                                                *)
(*   ReadLn                                                                  *)
(*   CompareStrings                                                          *)
(*   ReadToken                                                               *) 
(*   Open                                                                    *)
(*   Close                                                                   *)
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
(*   2001JAN04 KlS     positioning implemented                               *)
(*   2001JAN05 KlS     reading comments "on the fly" in ReadToken            *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Input;

IMPORT  
  Strings, SYSTEM, 
  WinDef, WinCon, WinBase, WinNT,
  ConvertParam, Output;
  

CONST
  FirstLine            =                1;
  FirstColumn          =                1;
  IndentationOfComment =                3;

  LowerCaseLetter      =                1;
  UpperCaseLetter      =                2;
  TrimStrings          =                4;
  
  TokenCppEmpty*       =                0;
  TokenCppComment1*    =                1;
  TokenCppComment2*    =                2;
  TokenCppComment3*    =                3;

  TokenCppDefine*      =               11;
  TokenCppIfDef*       =               12;
  TokenCppIfnDef*      =               13;
  TokenCppElse*        =               14;
  TokenCppElsIf*       =               15;
  TokenCppEndIf*       =               16;

  TokenCppTypeDef*     =               20;
  TokenCppStruct*      =               21;
  TokenCppEnum*        =               22;
  TokenCppIn*          =               23;
  TokenCppOut*         =               24;
  TokenCpp_StdCall*    =               31;
  TokenCpp_DeclSpec*   =               32;
  TokenCpp_CALLBACK*   =               33;

  TokenCppBracket1*    =               81;                 (* "(" *)
  TokenCppBracket2*    =               82;                 (* ")" *)
  TokenCppBracket3*    =               83;                 (* "{" *)
  TokenCppBracket4*    =               84;                 (* "}" *)
  TokenCppBracket5*    =               85;                 (* "[" *)
  TokenCppBracket6*    =               86;                 (* "]" *)
  
  TokenCppComma*       =               91;                 (* "," *)
  TokenCppSemicolon*   =               92;                 (* ";" *)
  TokenCppPoint*       =               93;                 (* "." *)
  TokenCppStar*        =               94;                 (* "*" *)
  
  TokenCppAString*     =              101;
  TokenCppANumber*     =              102;
  TokenCppInList*      =              103;
  

TYPE
  TPosition =          RECORD
     Line-,
     Column-:                          LONGINT;
  END (* TPosition *);
  

VAR
  CharacterRead*:                      CHAR;
  m, mode:                             LONGINT;
  Position-:                           TPosition;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  StatusQuo:                           LONGINT;
  StdIn:                               WinDef.HANDLE;
  InputFile:                           WinDef.HANDLE;
  InputFileName:                       ARRAY 132 OF CHAR;
  File_Security:                       WinBase.SECURITY_ATTRIBUTES;
  TokenCppList*:                       ARRAY 100 OF ConvertParam.TokenString;


(*****************************************************************************)
(*                                                                           *)
(* ReadChar                                                                  *)
(* Reads a character from the input file, a C++ Header file                  *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadChar*                   ():                  LONGINT;
                                      
VAR 
  BytesRead:                           LONGINT;
  ResultBool:                          WinDef.BOOL;
  
BEGIN
  
  ResultBool := WinBase.ReadFile(InputFile,
                                 SYSTEM.ADR(CharacterRead),
                                 1,
                                 BytesRead,
                                 0);
                                 
  IF CharacterRead=0AX THEN
    INC(Position.Line);
    Position.Column := FirstColumn;
  ELSE
    INC(Position.Column);
  END;
                                 
  IF ((ResultBool=WinDef.True) & (BytesRead>0)) THEN
    RETURN 0;
  ELSE
    RETURN 1
  END (* IF ((ResultBool=WinDef.True) & (BytesRead>0)) *)
  
END ReadChar;


(*****************************************************************************)
(*                                                                           *)
(* ReadLn                                                                    *)
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

PROCEDURE ReadLn*                     (VAR AString:        ARRAY OF CHAR)
                                      :                    LONGINT;
                                      
VAR
  Index:                               INTEGER;
  Result:                              LONGINT;
  
BEGIN
  
  Result   := ReadChar();
  Index    :=  0;
  
  WHILE ((Result=0) & (CharacterRead#0DX)) DO
    AString[Index] := CharacterRead;
    INC(Index);
    Result         := ReadChar();
  END (* WHILE ((Result=0) & ...) DO *);
  
  AString[Index]   := CHR(0);
  
  IF Result=0 THEN
    Result         := ReadChar();
  END (* IF ... *);
  
  RETURN Result;
  
END ReadLn;


(*****************************************************************************)
(*                                                                           *)
(* CompareStrings                                                            *)
(* compares two strings                                                      *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  String1    first string to compare                                       *)
(*  String2    second string to compare                                      *)
(*  Mode       Sum of          not implemented!!                             *)
(*             LowerCaseLetter                                               *)
(*             UpperCaseLetter                                               *)
(*             TrimStrings                                                   *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  BOOL       TRUE    the strings are equal                                 *)
(*             FALSE                                                         *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE CompareStrings*              (VAR String1:       ARRAY OF CHAR;
                                        VAR String2:       ARRAY OF CHAR;
                                            Mode:          LONGINT)
                                      :                    BOOLEAN;

VAR
  Index:                               LONGINT;
  MyString1,
  MyString2:                           ARRAY 128 OF CHAR;

  (***************************************************************************)
  PROCEDURE LowerCase(VAR MyString:    ARRAY OF CHAR);
  VAR
    i:                                 INTEGER;
  BEGIN
    FOR i:=0 TO Strings.Length(MyString)-2 DO
      CASE MyString[i] OF
        "A".."Z":
          MyString[i]  := CHR(ORD(MyString[i]) - ORD("A") - ORD("a"));
        |
        "Ö": 
           MyString[i] := "ö";
        |
        "Ä": 
           MyString[i] := "ä";
        |
        "ü":
           MyString[i] := "ü";
        |
        "Á": 
           MyString[i]  := "á";
        |
        "É": 
          MyString[i]  := "é";
        |
        "Í": 
          MyString[i]  := "í";
        |
        "Ó": 
           MyString[i] := "ó";
        |
        "Ú": 
           MyString[i] := "ú";
        |
        "À": 
           MyString[i] := "à";
        |
        "È": 
           MyString[i] := "è";
        |
        "Ì": 
           MyString[i] := "ì";
        |
        "Ò": 
           MyString[i] := "ò";
        |
        "Ù": 
           MyString[i] := "ù";
        |
        "Â": 
           MyString[i] := "â";
        |
        "Ê": 
           MyString[i] := "ê";
        |
        "Î": 
           MyString[i] := "î";
        |
        "Ô": 
           MyString[i] := "ô";
        |
        "Û":
           MyString[i] := "û";
      ELSE
        ;
      END;
    END;
  END LowerCase;

BEGIN;

  COPY(String1, MyString1);
  COPY(String2, MyString2);
  
  IF SYSTEM.BIT(Mode, 0) THEN (* Lower Case *)
    LowerCase(MyString1);
    LowerCase(MyString2);
  END;
    
  IF SYSTEM.BIT(Mode, 1) THEN (* Upper Case *)
    Strings.UpCase(MyString1);
    Strings.UpCase(MyString2);
  END;
    
  IF SYSTEM.BIT(Mode, 2) THEN (* Trim Strings *)
    Strings.RemoveLeadingSpaces(MyString1);
    Strings.RemoveTrailingSpaces(MyString1);
    Strings.RemoveLeadingSpaces(MyString2);
    Strings.RemoveTrailingSpaces(MyString2);
  END;
    
  IF Strings.Length(MyString1)#Strings.Length(MyString2) THEN
    RETURN FALSE;
  END;
  
  FOR Index:=0 TO (Strings.Length(String1)-1) DO
    IF String1[Index]#String2[Index] THEN
      RETURN FALSE
    END;
  END;
  
  RETURN TRUE
  
  
END CompareStrings;


(*****************************************************************************)
(*                                                                           *)
(* ReadToken                                                                 *)
(* Reads a token from the input file, a C++ Header file                      *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  AToken     the token (a LONGINT) that was read from the file             *)
(*  ATokenString                                                             *)
(*             the string that was read as token                             *)
(*  StopChar   the character that was read as last one                       *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    Resultcode                                                    *)
(*              0  successful                                                *)
(*             -1  no Token found (probably not a text file)                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*             comments will be directly written into the output file        *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadToken*                  (VAR AToken:         LONGINT;
                                       VAR ATokenString:   ARRAY OF CHAR;
                                       VAR StopChar:       CHAR)
                                      :                    LONGINT;
                                      
VAR
  Done,
  CharIsValid:                         BOOLEAN;
  Index:                               INTEGER;
  MyString:                            ARRAY 80 OF CHAR;
  Result:                              LONGINT;

  (***************************************************************************)
  PROCEDURE ReadSingleLineComment();

  VAR
    Result:                            LONGINT;
    
  BEGIN
(*    IF Position.Column<=IndentationOfComment THEN                          *)
(*      Result     := Output.WriteIndent(0);                                 *)
(*    ELSE                                                                   *)
(*      Result     := Output.WriteIndent(Output.TokenO2Comment1);            *)
(*    END (* IF Position.Column<= ... *);                                    *)
    
(*    Result     := Output.WriteToken(Output.TokenO2Comment1);               *)
    REPEAT
      Result := ReadChar();
(*      IF CharacterRead=0DX THEN                                            *)
(*        Result     := Output.WriteToken(Output.TokenO2Comment2);           *)
(*      END;                                                                 *)
(*      Result := Output.WriteChar(CharacterRead);                           *)
    UNTIL CharacterRead=0AX;
  END ReadSingleLineComment;
  
  (***************************************************************************)
  PROCEDURE ReadMultiLineComment();

  VAR
    MyChar:                            CHAR;
    Result:                            LONGINT;
    
  BEGIN
(*    Result     := Output.WriteIndent(0);                                   *)
(*    Result     := Output.WriteToken(Output.TokenO2Comment1);               *)
    REPEAT
      Result := ReadChar();
(*      IF CharacterRead=0DX THEN                                            *)
(*        Result     := Output.WriteToken(Output.TokenO2Comment2);           *)
(*      END;                                                                 *)
(*      Result := Output.WriteChar(CharacterRead);                           *)
(*      IF CharacterRead=0AX THEN                                            *)
(*        Result     := Output.WriteIndent(0);                               *)
(*        Result     := Output.WriteToken(Output.TokenO2Comment1);           *)
(*      END;                                                                 *)
    UNTIL CharacterRead="*";
    Result     := ReadChar();                              (* must be "/" *)
(*    Result     := Output.WriteToken(Output.TokenO2Comment2);               *)
(*    Result     := Output.WriteLn();                                        *)
    Result     := ReadLn(MyString);
  END ReadMultiLineComment;
  
BEGIN

  Done         := FALSE;
  
  WHILE ~Done DO
    AToken               :=  0;
    ATokenString[0]      :=  0X;
    Index                :=  0;
    CharIsValid          := TRUE;
  
    (* trim leading blanks *)
    REPEAT
      Result   := ReadChar();
      StopChar := CharacterRead;
    UNTIL ((Result>0) OR (StopChar#20X));
    
    IF Result>0 THEN                                         (* an error occurred while reading *)
      RETURN Result
    END;
    
    IF StopChar="/" THEN
      ATokenString[0]  := StopChar;
      Result           := ReadChar();
      StopChar         := CharacterRead;
      IF StopChar="/" THEN
        ReadSingleLineComment();
      ELSIF StopChar="*" THEN
        ReadMultiLineComment()
      ELSE
        Done := TRUE;
      END;
    ELSE
      Done := TRUE;
    END (* IF StopChar="/" *);
  END (* WHILE NOT Done DO *);
  
  WHILE ((Result=0) & CharIsValid) DO
    CASE StopChar OF
      " ":
        CharIsValid := FALSE;
      |
      0DX:
        Result      := ReadChar();
        CharIsValid := FALSE;
      |
      0AX:
        CharIsValid := FALSE;
      |
      "(", ")", "{", "}", "[", "]", "+", "-", ",", ".", ";":
        CharIsValid := FALSE;
      ELSE
        ATokenString[Index]  := StopChar;
        Result               := ReadChar();
        StopChar             := CharacterRead;
        INC(Index);
        IF Index>=LEN(ATokenString) THEN
          RETURN -1
        END;
    END (* CASE AChar OF *);
    
  END (* WHILE ... DO *);

  ATokenString[Index]  := 0X;
  
  IF Result>0 THEN
    RETURN Result;
  END (* IF ... *);
  
  IF Index=0 THEN
    RETURN 0
  END;
  
  (* Find Token *)
  IF ConvertParam.GetParam(ATokenString, MyString) THEN
    COPY(MyString, ATokenString);
    AToken := TokenCppInList;
  END;
  FOR Index:=1 TO LEN(TokenCppList)-1 DO
    IF CompareStrings(ATokenString, TokenCppList[Index], 0) THEN
      AToken := Index;
      RETURN 0;
    END (* IF CompareStrings(... *);
  END;
  
  IF ((ATokenString[0]>="A") & (ATokenString[0]<="Z")) OR
     ((ATokenString[0]>="a") & (ATokenString[0]<="z")) OR
      (ATokenString[0]="_")                            THEN
    AToken :=   TokenCppAString;
    RETURN 0;
  END;

  IF ((ATokenString[0]>="0") & (ATokenString[0]<="9")) THEN
    AToken :=   TokenCppANumber;
    RETURN 0;
  END;

  RETURN 0;
  
END ReadToken;


(*****************************************************************************)
(*                                                                           *)
(* Open                                                                      *)
(* open the input file                                                       *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  FileName                                                                 *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    0               operation was successful                      *)
(*             otherwise       WINDOWS error code                            *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Open*                       (FileName:           ARRAY OF CHAR)
                                      :                    LONGINT;

  
BEGIN;

  InputFile := WinBase.CreateFileA    (SYSTEM.ADR(FileName),
                                       WinNT.GENERIC_READ,
                                       0,
                                       File_Security,
                                       WinBase.OPEN_EXISTING,
                                       WinNT.FILE_ATTRIBUTE_NORMAL,
                                       0);
  IF InputFile=-1 THEN                                     (* error during file open *)
    RETURN WinBase.GetLastError();
  ELSE
    COPY (FileName, InputFileName); 
    RETURN 0
  END;
  
END Open;


(*****************************************************************************)
(*                                                                           *)
(* Close                                                                     *)
(* closes input file                                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Close*                      ();

  
BEGIN;

  ResultBool := WinBase.CloseHandle(InputFile);
  
END Close;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN

  StatusQuo                          :=  0;
  
  Position.Line                      := FirstLine;
  Position.Column                    := FirstColumn;

  FOR m:=0 TO LEN(TokenCppList)-1 DO;
    TokenCppList[m, 0] := 0X;
  END;
  
  TokenCppList[TokenCppComment1]     := "/*";
  TokenCppList[TokenCppComment2]     := "*/";
  TokenCppList[TokenCppComment3]     := "//";

  TokenCppList[TokenCppDefine]       := "#define";
  TokenCppList[TokenCppIfDef]        := "#ifdef";
  TokenCppList[TokenCppIfnDef]       := "#ifndef";
  TokenCppList[TokenCppElse]         := "#else";
  TokenCppList[TokenCppElsIf]        := "#elsif";
  TokenCppList[TokenCppEndIf]        := "#endif";

  TokenCppList[TokenCppTypeDef]      := "typedef";
  TokenCppList[TokenCppStruct]       := "struct";
  TokenCppList[TokenCppEnum]         := "enum";
  TokenCppList[TokenCppIn]           := "IN";
  TokenCppList[TokenCppOut]          := "OUT";
  TokenCppList[TokenCpp_StdCall]     := "__stdcall";
  TokenCppList[TokenCpp_DeclSpec]    := "DECLSPEC_IMPORT";
  TokenCppList[TokenCpp_CALLBACK]    := "CALLBACK";

END Input.

