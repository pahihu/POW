(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     Syntax                                      V 1.00.09         *)
(*                                                         2002OCT10         *)
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
PROCEDURE GetIdent                    (VAR txt:            ARRAY OF CHAR; 
                                       VAR ident:          ARRAY OF CHAR;
                                       VAR inx:            LONGINT);
(* liefert einen Textbereich zurück *)
VAR
  i:                                   LONGINT;

BEGIN
  i        :=  0;
  WHILE IsIdentChar(txt[inx]) & (i<LEN(ident)-1) DO
    ident[i]   := txt[inx];
    INC(i);
    INC(inx);
  END;
  ident[i] := 0X;
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
    ELSIF (line.startFlag=FLREPEAT) & (nextLine.indent#line.indent+Options.indentWidth) THEN
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
  ;
END Syntax.
