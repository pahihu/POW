(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     Syntax                                      V 2.00.73         *)
(*                                                         2005JUN14         *)
(*  PURPOSE:   This module provides hard coded Oberon-2 specific syntax      *)
(*             support to automatically complete frequently used constructs  *)
(*             as they are typed.                                            *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(*                                                                           *)
(* Author(s):                                                                *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   2000SEP11                                                               *)
(*                                                                           *)
(*  update                                                                   *)
(*   2002JUL16 KlS     comment added after "END" Statement                   *)
(*   2002SEP13 Luca    Module Name added after last END statement            *)
(*   2002OCT10 KlS     comments after "END" Statement corrected              *)
(*   2003OCT24 KlS     keyword colouring added                               *)
(*   2004APR25 KlS     simple java support implemented                       *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE Syntax;


IMPORT
  ListSt, Strings, Options, GlobWin;


CONST
  MAXIDENT             =               40;
  MaxPathLength*       =               256;
  FLNOTHING            =               0;
  FLEND                =               1;
  FLBEGIN              =               2;
  FLWHILE              =               3;
  FLREPEAT             =               4;
  FLUNTIL              =               5;
  FLFOR                =               6;
  FLIF                 =               7;
  FLELSIF              =               8;
  FLELSE               =               9;
  FLPROCEDURE          =               10;
  FLCONST              =               11;
  FLVAR                =               12;
  FLTYPE               =               13;
  FLLOOP               =               14;
  FLWITH               =               15;
  FLCASE               =               16;
  FLALTERNATIVE*       =               17;
  FLMODULE             =               20;
  FLJAVABLOCK          =               31;                 (* KlS, 2004APR25 *)


TYPE
  LineAnalyzerT        = RECORD
    txt:                               ARRAY ListSt.MAXLENGTH+1 OF CHAR;
    procName:                          ARRAY MAXIDENT+1 OF CHAR;
    indent,
    len,
    paramIndent,
    indentChange:                      LONGINT;
    startFlag,
    endFlag:                           INTEGER;
  END (* LineAnalyzerT *) ;


(*****************************************************************************)
(*                                                                           *)
(* PosInFileName                                                             *)
(* This function returns the position of string Pattern in string FileName.  *)
(* If Pattern does not occur in FileName zero is returned. If Pattern occurs *)
(* several times the position of the first occurrence is returned.           *)
(* Both strings are converted to upper cases before searching starts.        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Pattern    string to search for                                          *)
(*  FileName   (qualified) file name                                         *)
(*  Start      position to start from                                        *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    0       no occurrence found                                   *)
(*             >0      start position of Pattern in FileName (relative to 1 )*)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE PosInFileName*              (VAR Pattern-:       ARRAY OF CHAR;
                                       VAR FileName-:      ARRAY OF CHAR;
                                       Start:              LONGINT)
                                      :LONGINT;

VAR
  MyFileName,
  MyPattern:                           ARRAY MaxPathLength OF CHAR;
  Result:                              LONGINT;

BEGIN
 ;

  COPY(Pattern, MyPattern);
  Strings.UpCase(MyPattern);
  COPY(FileName, MyFileName);
  Strings.UpCase(MyFileName);
  RETURN Strings.Pos(MyPattern, MyFileName, Start);

END PosInFileName;


(*****************************************************************************)
(* prüft, ob das übergebene Zeichen ein Buchstabe, eine Zahl oder ein Underscore ist *)
PROCEDURE IsIdentChar*                (Char:               CHAR)
                                      :BOOLEAN;
BEGIN
  RETURN (Char="_") OR 
  ((Char>="a") & (Char<="z")) OR 
  ((Char>="A") & (Char<="Z")) OR 
  ((Char>="0") & (Char<="9"));
END IsIdentChar;


(*****************************************************************************)
(* liefert einen Textbereich zurück *)
PROCEDURE GetIdent*                   (VAR Text:           ARRAY OF CHAR;
                                       VAR Token:          ARRAY OF CHAR;
                                       VAR Index:          LONGINT);
VAR
  i:                                   LONGINT;

BEGIN
  i            := 0;
  WHILE IsIdentChar(Text[Index]) & (i<LEN(Token) - 1) DO
    Token[i]   := Text[Index];
    INC(i);
    INC(Index);
  END (* WHILE IsIdentChar(Text[Index])  *);
  Token[i]     := 0X;
END GetIdent;


(*****************************************************************************)
(* überspringt Leerzeichen *)
PROCEDURE SkipBlanks                  (VAR Text:           ARRAY OF CHAR;
                                       VAR Index:          LONGINT);
BEGIN
  WHILE Text[Index]=" " DO
    INC(Index)
  END (* WHILE Text[Index]=" *) 
END SkipBlanks;


(*****************************************************************************)
(* Initialisierung eines Indent *)
PROCEDURE InitIndent                  (indent:             LONGINT;
                                       VAR Text:           ARRAY OF CHAR);
BEGIN
  IF indent>LEN(Text) - 1 THEN
    indent     := LEN(Text) - 1
  END (* IF indent>LEN(Text) - 1 *);
  IF indent<0 THEN
    indent     := 0
  END (* IF indent<0 *);
  Text[indent] := 0X;
  WHILE indent>0 DO
    DEC(indent);
    Text[indent] := " ";
  END (* WHILE indent>0 *) 
END InitIndent;


(*****************************************************************************)
(* Initialisierung *)
PROCEDURE (VAR line: LineAnalyzerT) Init
                                      (Text:               ListSt.Text;
                                       Row:                LONGINT);
VAR
  inx,
  i,
  h:                                   LONGINT;
  ident:                               ARRAY MAXIDENT+1 OF CHAR;

  (*--------------------------------------------------------- KlS, 2002OCT10 *)
  PROCEDURE GetExpression               (VAR Statement:      ARRAY OF CHAR;
                                         VAR Expression:     ARRAY OF CHAR;
                                         WhereToStart:       CHAR;
                                         Token:              ARRAY OF CHAR);
  VAR
    i,
    j:                                   LONGINT;
  
  BEGIN
    i            := Strings.PosChar(WhereToStart, Statement, 0);
    j            := 0;
    LOOP
      IF i>=Strings.Length(Statement) THEN
        Expression[j] := 0X;
        EXIT
      END (* IF i>=Strings.Length(Statement) *) ;
      IF j>=(LEN(Expression) - 1) THEN
        Expression[j] := 0X;
        EXIT
      END (* IF j>=(LEN(Expression) - 1) *) ;
      IF (j>0) & (Strings.Pos(Token, Statement, i)=i) THEN
        Expression[j - 1] := 0X;
        EXIT
      END (* IF (j>0) & (Strings.Pos(Token,  *) ;
      Expression[j] := Statement[i];
      INC(i);
      INC(j);
    END (* LOOP *) ;
  END GetExpression;

(*---------------------------------------------------------------------------*)
BEGIN
  line.txt     := "";
  line.indent  := 0;
  line.paramIndent := 0;
  line.indentChange := 0;
  line.len     := 0;
  line.startFlag := FLNOTHING;
  line.endFlag := FLNOTHING;

  IF ~Text.GetLine(Row, line.txt, line.len) THEN
    RETURN 
  END (* IF ~Text.GetLine(Row, line.txt, *);

  WHILE line.txt[line.indent]=" " DO
    INC(line.indent)
  END (* WHILE line.txt[line.indent]=" *);

  Strings.RemoveLeadingSpaces(line.txt);
  Strings.RemoveTrailingSpaces(line.txt);
  line.len     := Strings.Length(line.txt);
  inx          := 0;
  GetIdent(line.txt, ident, inx);

  IF (ident="MODULE") OR (ident="DEFINITION") THEN
    (* Luca, 2002SEP13 *)
    line.indentChange := 0;
    line.startFlag := FLMODULE;
    SkipBlanks(line.txt, inx);
    GetIdent(line.txt, line.procName, inx);
    (* Luca, 2002SEP13 *)

  ELSIF ident="PROCEDURE" THEN
    line.startFlag := FLPROCEDURE;
    line.indentChange := 0;
    SkipBlanks(line.txt, inx);
    IF line.txt[inx]="^" THEN
      line.startFlag := FLNOTHING;
      RETURN ;
    END (* IF line.txt[inx]=" *);
    IF line.txt[inx]="[" THEN
      WHILE (line.txt[inx]#"]") & (line.txt[inx]#0X) DO
       INC(inx)
      END (* WHILE (line.txt[inx]#") & (line *);
      IF line.txt[inx]="]" THEN
        INC(inx)
      END (* IF line.txt[inx]=" *);
    END (* IF line.txt[inx]=" *);
    IF line.txt[inx]="(" THEN
      WHILE (line.txt[inx]#")") & (line.txt[inx]#0X) DO
       INC(inx)
      END (* WHILE (line.txt[inx]#") & (line *);
      IF line.txt[inx]=")" THEN
        INC(inx)
      END (* IF line.txt[inx]=" *);
    END (* IF line.txt[inx]=" *);
    SkipBlanks(line.txt, inx);
    GetIdent(line.txt, line.procName, inx);
    WHILE (line.txt[inx]=" ") OR (line.txt[inx]="*") DO
      INC(inx)
    END (* WHILE (line.txt[inx]=") OR (lin *);
    IF (line.len>inx + 2) & (line.txt[inx]="(") THEN
      i        := 0;
      h        := inx + 1;
      WHILE line.txt[inx]#0X DO
        IF line.txt[inx]="(" THEN
          INC(i)
        ELSIF line.txt[inx]=")" THEN
          DEC(i)
        END (* IF line.txt[inx]=" *);
        INC(inx);
      END (* WHILE line.txt[inx]#0X *);
      IF i>0 THEN
        line.paramIndent := h
      ELSE
        line.paramIndent := 0
      END (* IF i>0 *);
    END (* IF (line.len>inx + 2) & (line.t *) ;

  ELSIF ident="WHILE" THEN
    line.indentChange := 1;
    line.startFlag := FLWHILE;
    GetExpression(line.txt, line.procName, "E", "DO");
    (* KlS, 2002OCT10 *)

  ELSIF ident="REPEAT" THEN
    line.indentChange := 1;
    line.startFlag := FLREPEAT;

  ELSIF ident="FOR" THEN
    line.indentChange := 1;
    line.startFlag := FLFOR;
    GetExpression(line.txt, line.procName, "R", "DO");
    (* KlS, 2002OCT10 *)

  ELSIF ident="LOOP" THEN
    line.indentChange := 1;
    line.startFlag := FLLOOP;

  ELSIF ident="IF" THEN
    line.indentChange := 1;
    line.startFlag := FLIF;
    GetExpression(line.txt, line.procName, "F", "THEN");
    (* KlS, 2002OCT10 *)

  ELSIF ident="VAR" THEN
    line.indentChange := 1;
    line.startFlag := FLVAR;

  ELSIF ident="CONST" THEN
    line.indentChange := 1;
    line.startFlag := FLCONST;

  ELSIF ident="TYPE" THEN
    line.indentChange := 1;
    line.startFlag := FLTYPE;

  ELSIF ident="BEGIN" THEN
    line.indentChange := 1;
    line.startFlag := FLBEGIN;

  ELSIF ident="END" THEN
    line.indentChange :=  - 1;
    line.startFlag := FLEND;

  ELSIF ident="UNTIL" THEN
    line.indentChange :=  - 1;
    line.startFlag := FLUNTIL;

  ELSIF ident="ELSIF" THEN
    line.indentChange := 1;
    line.startFlag := FLELSIF;

  ELSIF ident="ELSE" THEN
    line.indentChange := 1;
    line.startFlag := FLELSE;

  ELSIF ident="WITH" THEN
    line.indentChange := 1;
    line.startFlag := FLWITH;
    GetExpression(line.txt, line.procName, "H", "DO");
    (* KlS, 2002OCT10 *)

  ELSIF ident="CASE" THEN
    line.indentChange := 1;
    line.startFlag := FLCASE;
    GetExpression(line.txt, line.procName, "E", "OF");
    (* KlS, 2002OCT10 *)

  ELSIF (ident="") & (line.txt[inx]="|") THEN
    line.indentChange := 1;
    line.startFlag := FLALTERNATIVE;
    INC(inx);

  ELSIF (line.len>0)(* KlS, 2004APR25 *)
   & (Strings.PosChar("{", line.txt, 1)=line.len) THEN
    line.indentChange := 1;
    line.startFlag := FLJAVABLOCK;
    COPY(line.txt, line.procName);
    Strings.Delete(line.procName, Strings.Length(line.procName), 1);
    INC(inx);

  ELSE
    ;
  END (* IF (ident=") OR (ident=") *) ;

  IF line.startFlag#FLNOTHING THEN
    inx        := line.len - 1;
    WHILE (inx>=0) & ((line.txt[inx]=" ")
     OR (line.txt[inx]=";")
     OR ((inx>0) & (line.txt[inx]=")") & (line.txt[inx - 1]="*"))) DO
      IF (inx>0) & (line.txt[inx]=")") & (line.txt[inx - 1]="*") THEN
        DEC(inx, 2);
        WHILE (inx>0) & ((line.txt[inx]#"(")
         OR (line.txt[inx + 1]#"*")) DO
          DEC(inx)
        END (* WHILE (inx>0) & ((line.txt[inx] *) ;
      END (* IF (inx>0) & (line.txt[inx]=")  *) ;
      DEC(inx);
    END (* WHILE (inx>=0) & ((line.txt[inx *) ;
    IF (inx>2) & (line.txt[inx - 2]="E") & (line.txt[inx - 1]="N") & (line.txt[inx]="D") THEN
      line.endFlag := FLEND;
      DEC(line.indentChange);
    END (* IF (inx>2) & (line.txt[inx - 2] *) ;
  END (* IF line.startFlag#FLNOTHING *) ;
END Init;


(*****************************************************************************)
(* Analysieren eines Textes *)
PROCEDURE Analyze*                    (Row:                LONGINT;
                                       Text:               ListSt.Text;
                                       VAR noNewLine:      BOOLEAN);
VAR
  line,
  nextLine,
  prevLine:                            LineAnalyzerT;
  txt:                                 ARRAY ListSt.MAXLENGTH+1 OF CHAR;
  done:                                BOOLEAN;

BEGIN
  noNewLine    := FALSE;
  line.Init(Text, Row);
  nextLine.Init(Text, Row + 1);
  prevLine.Init(Text, Row - 1);                            (* Luca, 2002SEP13 *)
  IF (line.startFlag=FLMODULE) & (nextLine.startFlag#FLBEGIN)
   & (nextLine.startFlag#FLVAR) & (nextLine.startFlag#FLTYPE)
   & (nextLine.startFlag#FLCONST) & (nextLine.startFlag#FLEND) THEN
    InitIndent(line.indent, txt);
    Strings.Append(txt, "END ");
    Strings.Append(txt, line.procName);
    Strings.Append(txt, ".");
    done       := Text.InsertLine(txt, Row + 1);           (* Luca, 2002SEP13 *)

  ELSIF (line.startFlag=FLPROCEDURE) & (nextLine.startFlag#FLBEGIN)
   & (nextLine.startFlag#FLVAR) & (nextLine.startFlag#FLTYPE)
   & (nextLine.startFlag#FLCONST) THEN
    InitIndent(line.indent, txt);
    Strings.Append(txt, "END ");
    Strings.Append(txt, line.procName);
    Strings.Append(txt, ";");
    done       := Text.InsertLine(txt, Row + 1);
    InitIndent(line.indent, txt);
    Strings.Append(txt, "BEGIN");
    done       := Text.InsertLine(txt, Row + 1);
    InitIndent(line.indent, txt);
    Strings.Append(txt, "VAR");
    done       := Text.InsertLine(txt, Row + 1);
    IF line.paramIndent>0 THEN
      InitIndent(line.indent + line.paramIndent, txt);
      done     := Text.InsertLine(txt, Row + 1);
    END (* IF line.paramIndent>0 *);
    noNewLine  := TRUE;

  ELSIF (line.startFlag=FLVAR)
   OR (line.startFlag=FLCONST)
   OR (line.startFlag=FLTYPE) THEN
    InitIndent(line.indent + Options.indentWidth, txt);
    done       := Text.InsertLine(txt, Row + 1);
    noNewLine  := TRUE;

  ELSIF (line.startFlag=FLELSIF) OR (line.startFlag=FLELSE) THEN
    IF ((prevLine.indentChange - 1=0) & (prevLine.indent<line.indent))
     OR ((prevLine.indentChange - 1<0) & (prevLine.indent=line.indent))
     OR ((prevLine.startFlag=FLELSIF) & (prevLine.indent<line.indent)) THEN
      line.indent := line.indent - Options.indentWidth;
      InitIndent(line.indent, txt);
      Strings.Append(txt, line.txt);
      done     := Text.SetLine(Row, txt);

      IF ~done THEN
        GlobWin.Beep
      END (* IF ~done *);

    END (* IF ((prevLine.indentChange - 1= *) ;
    IF line.endFlag#FLEND THEN
      line.indent := line.indent + Options.indentWidth
    END (* IF line.endFlag#FLEND *);
    InitIndent(line.indent, txt);
    done       := Text.InsertLine(txt, Row + 1);

    IF ~done THEN
      GlobWin.Beep
    END (* IF ~done *);

    noNewLine  := TRUE;

  ELSIF (line.indentChange<0) & 
  (((prevLine.indentChange + line.indentChange=0) & (prevLine.indent<line.indent)) OR 
  ((prevLine.indentChange + line.indentChange<0) & (prevLine.indent=line.indent))) THEN
    InitIndent(line.indent - Options.indentWidth, txt);
    Strings.Append(txt, line.txt);
    done       := Text.SetLine(Row, txt);

    IF ~done THEN
      GlobWin.Beep
    END (* IF ~done *);

    InitIndent(line.indent - Options.indentWidth, txt);
    done       := Text.InsertLine(txt, Row + 1);

    IF ~done THEN
      GlobWin.Beep
    END (* IF ~done *);

    noNewLine  := TRUE;

  ELSIF line.indentChange>0 THEN
    IF ((line.startFlag=FLWHILE) OR (line.startFlag=FLFOR)
     OR (line.startFlag=FLIF) OR (line.startFlag=FLLOOP)
     OR (line.startFlag=FLWITH) OR (line.startFlag=FLCASE))
     & (nextLine.indent#line.indent + Options.indentWidth)
     & ~((nextLine.startFlag=FLEND) & (nextLine.indent=line.indent)) THEN
      InitIndent(line.indent, txt);
      Strings.Append(txt, "END");
      CASE line.startFlag OF                               (* KlS, 2002JUL16 *)
        FLWHILE:                                           (* KlS, 2002OCT10 *)
          Strings.Append(txt, " (* WHILE");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        | (* FLWHILE *)
        FLFOR:
          Strings.Append(txt, " (* FOR");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        | (* FLFOR *)
        FLIF:
          Strings.Append(txt, " (* IF");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        | (* FLIF *)
        FLLOOP:
          Strings.Append(txt, " (* LOOP *)");
        | (* FLLOOP *)
        FLWITH:
          Strings.Append(txt, " (* WHITH");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        | (* FLWITH *)
        FLCASE:
          Strings.Append(txt, " (* CASE");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        ELSE
          ;
      END (* CASE line.startFlag *) ;

      Strings.AppendChar(txt, ";");
      done     := Text.InsertLine(txt, Row + 1);
    ELSIF (line.startFlag=FLREPEAT) & ~(nextLine.startFlag=FLUNTIL)
     & (nextLine.indent<=line.indent) THEN
      InitIndent(line.indent, txt);
      Strings.Append(txt, "UNTIL ;");
      done     := Text.InsertLine(txt, Row + 1);
    ELSIF (line.startFlag=FLJAVABLOCK)                     (* KlS, 2004APR25 *)
     & (nextLine.indent#(line.indent + Options.indentWidth)) THEN
      InitIndent(line.indent, txt);
      IF Strings.Length(line.procName)=0 THEN
        Strings.Append(txt, "}");
      ELSE
        Strings.Append(txt, "} // ");
        Strings.Append(txt, line.procName);
      END (* IF Strings.Length(line.procName *) ;
      done     := Text.InsertLine(txt, Row + 1);
    END (* IF ((line.startFlag=FLWHILE) OR *) ;
    InitIndent(line.indent + Options.indentWidth, txt);
    done       := Text.InsertLine(txt, Row + 1);
    noNewLine  := TRUE;
  END (* IF (line.startFlag=FLMODULE) &  *) ;

END Analyze;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
 ;
  ;
END Syntax.

