(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     Syntax                                      V 1.00.18         *)
(*                                                         2003OCT29         *)
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
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE Syntax;


IMPORT 
  ListSt, Strings, Options, GlobWin;


CONST
  MAXIDENT     =                       40;
  FLNOTHING    =                        0;
  FLEND        =                        1;
  FLBEGIN      =                        2;
  FLWHILE      =                        3;
  FLREPEAT     =                        4;
  FLUNTIL      =                        5;
  FLFOR        =                        6;
  FLIF         =                        7;
  FLELSIF      =                        8;
  FLELSE       =                        9;
  FLPROCEDURE  =                       10;
  FLCONST      =                       11;
  FLVAR        =                       12;
  FLTYPE       =                       13;
  FLLOOP       =                       14;
  FLWITH       =                       15;
  FLCASE       =                       16;
  FLALTERNATIVE*=                      17;
  FLMODULE     =                       20;
  
  MaxKeyWords* =                      128;                 (* KlS, 2003OCT24 *)
   

TYPE
  LineAnalyzerT  = RECORD
    txt:                               ARRAY ListSt.MAXLENGTH+1 OF CHAR;
    procName:                          ARRAY MAXIDENT+1 OF CHAR;
    indent,
    len,
    paramIndent,
    indentChange:                      LONGINT;
    startFlag,
    endFlag:                           INTEGER;
  END (* LineAnalyzerT *);
  

VAR                                                        (* KlS, 2003OCT24 *)
  KeyWords*:                           ARRAY MaxKeyWords OF ARRAY 32 OF CHAR;
  KeyWordLength*:                      ARRAY MaxKeyWords OF LONGINT;
  i,
  NoOfKeyWords*:                       LONGINT;
  
  
(*********************************************************************************************)
PROCEDURE IsIdentChar*                (Char:               CHAR)
                                      :BOOLEAN;
(* prüft, ob das übergebene Zeichen ein Buchstabe, eine Zahl oder ein Underscore ist *)
BEGIN
  RETURN (Char="_") OR 
         ((Char>="a") & (Char<="z")) OR 
         ((Char>="A") & (Char<="Z")) OR 
         ((Char>="0") & (Char<="9"));
END IsIdentChar;


(*********************************************************************************************)
PROCEDURE GetIdent*                   (VAR Text:           ARRAY OF CHAR; 
                                       VAR Token:          ARRAY OF CHAR;
                                       VAR Index:          LONGINT);
(* liefert einen Textbereich zurück *)
VAR
  i:                                   LONGINT;

BEGIN
  i        :=  0;
  WHILE IsIdentChar(Text[Index]) & (i<LEN(Token)-1) DO
    Token[i]   := Text[Index];
    INC(i);
    INC(Index);
  END;
  Token[i] := 0X;
END GetIdent;


(*********************************************************************************************)
PROCEDURE SkipBlanks                  (VAR txt:            ARRAY OF CHAR; 
                                       VAR inx:            LONGINT);
(* überspringt Leerzeichen *)
BEGIN
  WHILE txt[inx]=" " DO 
    INC(inx) 
  END;
END SkipBlanks;


(*********************************************************************************************)
PROCEDURE InitIndent                  (indent:             LONGINT; 
                                       VAR txt:            ARRAY OF CHAR);
(* Initialisierung eines Indent *)
BEGIN
  IF indent>LEN(txt)-1 THEN 
    indent   := LEN(txt)-1 
  END;
  IF indent<0 THEN 
    indent   :=  0 
  END;
  txt[indent]  := 0X;
  WHILE indent>0 DO
    DEC(indent);
    txt[indent]  := " ";
  END;
END InitIndent;


(*********************************************************************************************)
PROCEDURE (VAR line:LineAnalyzerT) Init
                                      (text:               ListSt.Text; 
                                       row:                LONGINT);
(* Initialisierung *)
VAR
  inx, 
  i, 
  h:                                   LONGINT;
  ident:                               ARRAY MAXIDENT+1 OF CHAR;

  (*------------------------------------------------------- KlS, 2002OCT10 *)
  PROCEDURE GetExpression             (VAR Statement:      ARRAY OF CHAR;
                                       VAR Expression:     ARRAY OF CHAR;
                                       WhereToStart:       CHAR;
                                       Token:              ARRAY OF CHAR);
  VAR
    i,
    j:                                 LONGINT;
    
  BEGIN
    i      := Strings.PosChar(WhereToStart, Statement, 0);
    j      :=  0;
    LOOP
      IF i>=Strings.Length(Statement) THEN
        Expression[j]  := 0X;
        EXIT
      END (* IF i>=Strings.Length(Statement) *);
      IF j>=(LEN(Expression)-1) THEN
        Expression[j]  := 0X;
        EXIT
      END (* IF j>=(LEN(Expression)-1) *);
      IF (j>0) & (Strings.Pos(Token, Statement, i)=i) THEN
        Expression[j-1]:= 0X;
        EXIT
      END (* IF (j>0) & (Strings.Pos(Token, Statement, i)=i) *);
      Expression[j]  := Statement[i];
      INC(i);
      INC(j);
    END (* LOOP *);
  END GetExpression;
  
BEGIN
  line.txt         := "";
  line.indent      :=  0;
  line.paramIndent :=  0;
  line.indentChange:=  0;
  line.len         :=  0;
  line.startFlag   := FLNOTHING;
  line.endFlag     := FLNOTHING;

  IF ~text.GetLine(row,line.txt,line.len) THEN RETURN END;

  WHILE line.txt[line.indent]=" " DO INC(line.indent) END;

  Strings.RemoveLeadingSpaces(line.txt);
  Strings.RemoveTrailingSpaces(line.txt);
  line.len       := Strings.Length(line.txt);
  inx            :=  0;
  GetIdent(line.txt, ident, inx);
  line.startFlag := FLNOTHING;

  IF (ident="MODULE") OR (ident="DEFINITION") THEN         (* Luca, 2002SEP13 *)
    line.indentChange  :=  0;
    line.startFlag     := FLMODULE;
    SkipBlanks(line.txt, inx);
    GetIdent(line.txt, line.procName, inx);                (* Luca, 2002SEP13 *)

  ELSIF ident="PROCEDURE" THEN
    line.startFlag     := FLPROCEDURE;
    line.indentChange  :=  0;
    SkipBlanks(line.txt,inx);
    IF line.txt[inx]="^" THEN 
      line.startFlag   := FLNOTHING;
      RETURN;
    END;
    IF line.txt[inx]="[" THEN
      WHILE (line.txt[inx]#"]") & (line.txt[inx]#0X) DO INC(inx) END;
      IF line.txt[inx]="]" THEN INC(inx) END;
    END;
    IF line.txt[inx]="(" THEN
      WHILE (line.txt[inx]#")") & (line.txt[inx]#0X) DO INC(inx) END;
      IF line.txt[inx]=")" THEN INC(inx) END;
    END;
    SkipBlanks(line.txt, inx);
    GetIdent(line.txt, line.procName, inx);
    WHILE (line.txt[inx]=" ") OR (line.txt[inx]="*") DO 
      INC(inx) 
    END;
    IF (line.len>inx+2) & (line.txt[inx]="(") THEN
      i    :=  0;
      h    := inx + 1;
      WHILE line.txt[inx]#0X DO
        IF line.txt[inx]="(" THEN 
          INC(i)
        ELSIF line.txt[inx]=")" THEN 
          DEC(i)
        END;
        INC(inx);
      END;
      IF i>0 THEN 
        line.paramIndent := h 
      ELSE 
        line.paramIndent := 0 
      END;
    END;

  ELSIF ident="WHILE" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLWHILE;
    GetExpression(line.txt, line.procName, "E", "DO");     (* KlS, 2002OCT10*)

  ELSIF ident="REPEAT" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLREPEAT;

  ELSIF ident="FOR" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLFOR;
    GetExpression(line.txt, line.procName, "R", "DO");     (* KlS, 2002OCT10*)

  ELSIF ident="LOOP" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLLOOP;

  ELSIF ident="IF" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLIF;
    GetExpression(line.txt, line.procName, "F", "THEN");   (* KlS, 2002OCT10*)

  ELSIF ident="VAR" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLVAR;

  ELSIF ident="CONST" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLCONST;

  ELSIF ident="TYPE" THEN
    line.indentChange  :=  1;
    line.startFlag     :=FLTYPE;

  ELSIF ident="BEGIN" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLBEGIN;

  ELSIF ident="END" THEN
    line.indentChange  := -1;
    line.startFlag     :=FLEND;

  ELSIF ident="UNTIL" THEN
    line.indentChange  := -1;
    line.startFlag     := FLUNTIL;

  ELSIF ident="ELSIF" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLELSIF;

  ELSIF ident="ELSE" THEN
    line.indentChange  :=  1;
    line.startFlag:=FLELSE;

  ELSIF ident="WITH" THEN
    line.indentChange  :=  1;
    line.startFlag:=FLWITH;
    GetExpression(line.txt, line.procName, "H", "DO");     (* KlS, 2002OCT10*)

  ELSIF ident="CASE" THEN
    line.indentChange  :=  1;
    line.startFlag     := FLCASE;
    GetExpression(line.txt, line.procName, "E", "OF");     (* KlS, 2002OCT10*)

  ELSIF (ident="") & (line.txt[inx]="|") THEN
    line.indentChange  :=  1;
    line.startFlag     := FLALTERNATIVE;
    INC(inx);
    
  ELSE
    ;
  END (* IF (ident="MODULE") OR (ident="DEFINITION") *);

  line.endFlag := FLNOTHING;
  
  IF line.startFlag#FLNOTHING THEN
    inx    := line.len - 1;
    WHILE (inx>=0) & ((line.txt[inx]=" ") OR (line.txt[inx]=";") OR 
          ((inx>0) & (line.txt[inx]=")") & (line.txt[inx-1]="*"))) DO
      IF (inx>0) & (line.txt[inx]=")") & (line.txt[inx-1]="*") THEN
        DEC(inx,2);
        WHILE (inx>0) & ((line.txt[inx]#"(") OR (line.txt[inx+1]#"*")) DO DEC(inx) END;
      END (*IF (inx>0) & (line.txt[inx]=")") & (line.txt[inx-1]="*") *);
      DEC(inx);
    END (* WHILE (inx>=0) & ((line.txt[inx]=" ") OR (line.txt[inx]=";") OR ... *);
    IF (inx>2) & (line.txt[inx-2]="E") & (line.txt[inx-1]="N") & (line.txt[inx]="D") THEN
      line.endFlag:=FLEND;
      DEC(line.indentChange);
    END (* IF (inx>2) & (line.txt[inx-2]="E") & (line.txt[inx-1]="N") & (line.txt[inx]="D") *);
  END (* IF line.startFlag#FLNOTHING *);
END Init;


(*********************************************************************************************)
PROCEDURE Analyze*                    (row:                LONGINT; 
                                       text:               ListSt.Text; 
                                       VAR noNewLine:      BOOLEAN);
(* Analysieren eines Textes *)
VAR
  line,
  nextLine,
  prevLine:                            LineAnalyzerT;
  txt:                                 ARRAY ListSt.MAXLENGTH+1 OF CHAR;
  done:                                BOOLEAN;

BEGIN
  noNewLine    := FALSE;
  line.Init(text, row);
  nextLine.Init(text, row+1);
  prevLine.Init(text, row-1);
                                                           (* Luca, 2002SEP13 *)
  IF (line.startFlag=FLMODULE) & (nextLine.startFlag#FLBEGIN) &
     (nextLine.startFlag#FLVAR) & (nextLine.startFlag#FLTYPE) &
     (nextLine.startFlag#FLCONST) & (nextLine.startFlag#FLEND) THEN
    InitIndent(line.indent,txt);
    Strings.Append(txt, "END ");
    Strings.Append(txt, line.procName);
    Strings.Append(txt, ".");
    done       := text.InsertLine(txt, row+1);             (* Luca, 2002SEP13 *)

  ELSIF (line.startFlag=FLPROCEDURE) & (nextLine.startFlag#FLBEGIN) & 
        (nextLine.startFlag#FLVAR) & (nextLine.startFlag#FLTYPE) &  
        (nextLine.startFlag#FLCONST) THEN
    InitIndent(line.indent,txt);
    Strings.Append(txt,"END ");
    Strings.Append(txt, line.procName);
    Strings.Append(txt,";");
    done   := text.InsertLine(txt, row+1);
    InitIndent(line.indent,txt);
    Strings.Append(txt, "BEGIN");
    done   := text.InsertLine(txt, row+1);
    InitIndent(line.indent, txt);
    Strings.Append(txt, "VAR");
    done   := text.InsertLine(txt, row+1);
    IF line.paramIndent>0 THEN
      InitIndent(line.indent+line.paramIndent, txt);
      done := text.InsertLine(txt, row+1);
    END;
    noNewLine  := TRUE;
    
  ELSIF (line.startFlag=FLVAR) OR
        (line.startFlag=FLCONST) OR
        (line.startFlag=FLTYPE) THEN
    InitIndent(line.indent+Options.indentWidth,txt);
    done       := text.InsertLine(txt, row+1);
    noNewLine  := TRUE;
    
  ELSIF (line.startFlag=FLELSIF) OR (line.startFlag=FLELSE) THEN
    IF ((prevLine.indentChange-1=0) & (prevLine.indent<line.indent)) OR
       ((prevLine.indentChange-1<0) & (prevLine.indent=line.indent)) OR
       ((prevLine.startFlag=FLELSIF) & (prevLine.indent<line.indent)) THEN
      line.indent:=line.indent-Options.indentWidth;
      InitIndent(line.indent, txt);
      Strings.Append(txt, line.txt);
      done     := text.SetLine(row, txt);

      IF ~done THEN GlobWin.Beep END;

    END;
    IF line.endFlag#FLEND THEN line.indent:=line.indent+Options.indentWidth END;
    InitIndent(line.indent,txt);
    done       := text.InsertLine(txt, row+1);

    IF ~done THEN GlobWin.Beep END;

    noNewLine  := TRUE;
  
  ELSIF (line.indentChange<0) &
        (((prevLine.indentChange+line.indentChange=0) & (prevLine.indent<line.indent)) OR
        ((prevLine.indentChange+line.indentChange<0) & (prevLine.indent=line.indent))) THEN
    InitIndent(line.indent-Options.indentWidth, txt);
    Strings.Append(txt, line.txt);
    done   := text.SetLine(row, txt);

    IF ~done THEN GlobWin.Beep END;

    InitIndent(line.indent-Options.indentWidth,txt);
    done   := text.InsertLine(txt,row+1);

    IF ~done THEN GlobWin.Beep END;

    noNewLine:=TRUE;
    
  ELSIF line.indentChange>0 THEN
    IF ((line.startFlag=FLWHILE) OR (line.startFlag=FLFOR) OR 
       (line.startFlag=FLIF) OR (line.startFlag=FLLOOP) OR 
       (line.startFlag=FLWITH) OR (line.startFlag=FLCASE)) & 
       (nextLine.indent#line.indent+Options.indentWidth) &
       ~((nextLine.startFlag=FLEND) & (nextLine.indent=line.indent)) THEN
      InitIndent(line.indent,txt);
      Strings.Append(txt, "END");
      CASE line.startFlag OF                               (* KlS, 2002JUL16 *)
        FLWHILE:                                           (* KlS, 2002OCT10*)
          Strings.Append(txt, " (* WHILE");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        |
        FLFOR:
          Strings.Append(txt, " (* FOR");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        |
        FLIF:
          Strings.Append(txt, " (* IF");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        |
        FLLOOP:
          Strings.Append(txt, " (* LOOP *)");
        |
        FLWITH:
          Strings.Append(txt, " (* WHITH");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
        |
        FLCASE:
          Strings.Append(txt, " (* CASE");
          Strings.Append(txt, line.procName);
          Strings.Append(txt, " *)");
      ELSE
        ;
      END;                                                 (* KlS, 2002JUL16 *)
      Strings.AppendChar(txt, ";");
      done := text.InsertLine(txt, row+1);
    ELSIF (line.startFlag=FLREPEAT) & ~(nextLine.startFlag=FLUNTIL) 
          & (nextLine.indent<=line.indent) THEN
      InitIndent(line.indent, txt);
      Strings.Append(txt, "UNTIL ;");
      done := text.InsertLine(txt, row+1);
    END;
    InitIndent(line.indent+Options.indentWidth, txt);
    done       := text.InsertLine(txt, row+1);
    noNewLine  := TRUE;
  END;
  
END Analyze;


(*********************************************************************************************)
(*********************************************************************************************)
BEGIN;
  KeyWords[01] := "ABS";                                   (* KlS, 2003OCT24 *)
  KeyWords[02] := "ASH";
  KeyWords[03] := "ASSERT";
  KeyWords[04] := "ARRAY";
  KeyWords[05] := "BEGIN";
  KeyWords[06] := "BOOLEAN";
  KeyWords[07] := "BY";
  KeyWords[08] := "CAP";
  KeyWords[09] := "CASE";
  KeyWords[10] := "CHAR";
  KeyWords[11] := "CHR";
  KeyWords[12] := "CONST";
  KeyWords[13] := "COPY";
  KeyWords[14] := "DEC";
  KeyWords[15] := "DEFINITION";
  KeyWords[16] := "DISPOSE";
  KeyWords[17] := "DIV";
  KeyWords[18] := "DO";
  KeyWords[19] := "ELSE";
  KeyWords[20] := "ELSIF";
  KeyWords[21] := "END";
  KeyWords[22] := "ENTIER";
  KeyWords[23] := "EXCL";
  KeyWords[24] := "EXIT";
  KeyWords[25] := "FALSE";
  KeyWords[26] := "FOR";
  KeyWords[27] := "HALT";
  KeyWords[28] := "IF";
  KeyWords[29] := "IMPORT";
  KeyWords[30] := "IN";
  KeyWords[31] := "INC";
  KeyWords[32] := "INCL";
  KeyWords[33] := "INTEGER";
  KeyWords[34] := "IS";
  KeyWords[35] := "LEN";
  KeyWords[36] := "LONG";
  KeyWords[37] := "LONGINT";
  KeyWords[38] := "LONGREAL";
  KeyWords[39] := "LOOP";
  KeyWords[40] := "MAX";
  KeyWords[41] := "MIN";
  KeyWords[42] := "MOD";
  KeyWords[43] := "MODULE";
  KeyWords[44] := "NEW";
  KeyWords[45] := "NIL";
  KeyWords[46] := "ODD";
  KeyWords[47] := "OF";
  KeyWords[48] := "OR";
  KeyWords[49] := "ORD";
  KeyWords[50] := "POINTER";
  KeyWords[51] := "PROCEDURE";
  KeyWords[52] := "REAL";
  KeyWords[53] := "RECORD";
  KeyWords[54] := "REPEAT";
  KeyWords[55] := "RETURN";
  KeyWords[56] := "SHORT";
  KeyWords[57] := "SET";
  KeyWords[58] := "SHORT";
  KeyWords[59] := "SHORTINT";
  KeyWords[60] := "SIZE";
  KeyWords[61] := "THEN";
  KeyWords[62] := "TO";
  KeyWords[63] := "TRUE";
  KeyWords[64] := "TYPE";
  KeyWords[65] := "UNTIL";
  KeyWords[66] := "VAR";
  KeyWords[67] := "WHILE";
  KeyWords[68] := "WITH";
  NoOfKeyWords := 68;                                      (* KlS, 2003OCT24 *)
  FOR i:=1 TO NoOfKeyWords DO                              (* KlS, 2003OCT24 *)
    KeyWordLength[i]   := Strings.Length(KeyWords[i]);
  END (* FOR i:=1 TO NoOfKeyWords *);
END Syntax.
