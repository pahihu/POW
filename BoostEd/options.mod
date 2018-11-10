(*****************************************************************************)
(*  Module Options                                         V 1.00.14         *)
(*                                                                           *)
(*  This module contains all configurable options of the editor.             *)
(*  Option settings can be temporarily saved to support the implementation   *)
(*  of a cancel when editing the options in a dialog box.                    *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2003OCT23 KlS color for keywords added                                  *)
(*   2003NOV03 KlS color for strings  added                                  *)
(*   2004APR18 KlS Syntax Colouring in *.ini implemented                     *)
(*                                                                           *)
(*****************************************************************************)

MODULE Options;

IMPORT 
  GlobWin,
  Strings;


CONST
  FONTNAMELEN*         =               100;
  FONT_FIXEDSYS*       =              "Fixedsys";
  FONT_COURIER*        =              "Courier";
  FONT_COURIERNEW*     =              "Courier New";
  MAXDELIMLEN*         =                32;                (* maximum length for comment delimiters *)
  MAXSTRINGDELIMS*     =                 3;                (* maximum number of different string literal delimiters *)

  COMMENT_RED*         =                 0;                (* color default for comments *)
  COMMENT_GREEN*       =               180;                (* color default for comments *)
  COMMENT_BLUE*        =                 0;                (* color default for comments *)

  KEYWORD_RED*         =                 0;                (* color default for keywords; KlS, 2003OCT23 *)
  KEYWORD_GREEN*       =                 0;                (* color default for keywords *)
  KEYWORD_BLUE*        =               255;                (* color default for keywords *)

  STRINGS_RED*         =                 0;                (* color default for strings;  KlS, 2003NOV03 *)
  STRINGS_GREEN*       =                 0;                (* color default for strings *)
  STRINGS_BLUE*        =               128;                (* color default for strings *)
  
  MaxKeyWords* =                      128;                 (* KlS, 2003OCT24 *)


TYPE
  SyntaxColouringP* =                   POINTER TO SyntaxColouringT;
  SyntaxColouringT* = RECORD
    Next*:                             SyntaxColouringP;
    Extension*:                        ARRAY 256 OF CHAR;
    CommentStart*,
    CommentEnd*,
    CommentLine*:                      ARRAY MAXDELIMLEN OF CHAR; (* currently not used *)
    CommentsNested*:                   BOOLEAN;
    StringDelims*:                     ARRAY MAXSTRINGDELIMS+1 OF CHAR;
    NoOfKeyWords*:                     LONGINT;
    KeyWords*:                         ARRAY MaxKeyWords OF ARRAY 32 OF CHAR;
    KeyWordLength*:                    ARRAY MaxKeyWords OF LONGINT;
  END (* SyntaxColouringT*);


VAR
  syntax*:                             BOOLEAN;            (* Oberon-2 Syntax Unterstützung ? *)
  smartDel*:                           BOOLEAN;            (* Führende Leerzeichen löschen *)
  indentWidth*:                        LONGINT;            (* Anzahl Leerzeichen bei indent Level *)
  insert*:                             BOOLEAN;            (* Einfüge/Überschreibemodus *)
  autoIndent*:                         BOOLEAN;
  tabsize*:                            LONGINT;
  useTabs*:                            BOOLEAN;
  mouse*:                              BOOLEAN;            (* rechte Maustaste für Themensuche verwenden *)
  colorComments*:                      BOOLEAN;
  ActSyntaxColouring*,
  AnchorSyntaxColouring*:              SyntaxColouringP;
  fontSize*:                           LONGINT;
  fontName*:                           ARRAY FONTNAMELEN OF CHAR;
  printerFontSize*:                    LONGINT;            (* Schriftgröße *)
  printerFontName*:                    ARRAY FONTNAMELEN OF CHAR;

  printMarginLeft*,
  printMarginRight*,
  printMarginTop*,
  printMarginBottom*:                  LONGINT;            (* margin in 1/100 inches from edges of sheet *)
  printLineNumbers*:                   BOOLEAN;
  printDate*:                          BOOLEAN;
  
  CommentColor*,                                           (* RGB value to define the color used for comments *)
  KeyWordColor*,                                           (* RGB value to define the color used for keywords; KlS, 2003OCT23 *)
  StringsColor*:                       LONGINT;            (* RGB value to define the color used for Strings;  KlS, 2003NOV03 *)

  hsyntax:                             BOOLEAN;            (* Oberon-2 Syntax Unterstützung ? *)
  hsmartDel:                           BOOLEAN;            (* Führende Leerzeichen löschen *)
  hindentWidth:                        LONGINT;            (* Anzahl Leerzeichen bei indent Level *)
  hinsert:                             BOOLEAN;            (* Einfüge/Überschreibemodus *)
  hautoIndent:                         BOOLEAN;
  htabsize:                            LONGINT;
  huseTabs:                            BOOLEAN;
  hmouse:                              BOOLEAN;
  hfontSize:                           LONGINT;
  hfontName:                           ARRAY FONTNAMELEN OF CHAR;
  hPrinterFontSize:                    LONGINT;
  hPrinterFontName:                    ARRAY FONTNAMELEN OF CHAR;
  hColorComments:                      BOOLEAN;
  i:                                   LONGINT;

  
(******************************************************************************************)
PROCEDURE TmpSave*                    ();
(* Tempdaten speichern *)
BEGIN
  hsyntax          := syntax;
  hsmartDel        := smartDel;
  hindentWidth     := indentWidth;
  hinsert          := insert;
  hautoIndent      := autoIndent;
  htabsize         := tabsize;
  huseTabs         := useTabs; 
  hmouse           := mouse;
  hfontSize        := fontSize;
  COPY(fontName, hfontName);
  hPrinterFontSize := printerFontSize;
  COPY(printerFontName, hPrinterFontName);
  hColorComments   := colorComments;
END TmpSave;


(******************************************************************************************)
PROCEDURE Restore*                    ();
(* Restore Data *)

BEGIN
  syntax           := hsyntax;
  smartDel         := hsmartDel;
  indentWidth      := hindentWidth;
  insert           := hinsert;
  autoIndent       := hautoIndent;
  tabsize          := htabsize;
  useTabs          := huseTabs; 
  mouse            := hmouse;
  fontSize         := hfontSize;
  COPY(hfontName, fontName);
  printerFontSize  := hPrinterFontSize;
  COPY(hPrinterFontName, printerFontName);
  colorComments    := hColorComments;
END Restore;


(*****************************************************************************)
(*****************************************************************************)
(* Initialisierung der Einstellungen *)

BEGIN
  autoIndent           := TRUE;
  insert               := TRUE;
  useTabs              := TRUE;
  tabsize              :=   3;
  mouse                := TRUE;
  syntax               := TRUE;
  smartDel             := TRUE;
  indentWidth          :=   2;
  insert               := TRUE;
  colorComments        := TRUE;
  fontSize             :=  12;
  fontName             := FONT_FIXEDSYS;
  printerFontSize      :=  10;
  printerFontName      := FONT_COURIERNEW;

  printMarginLeft      :=  40;
  printMarginRight     :=  40;
  printMarginTop       :=  60;
  printMarginBottom    := 100;
  printLineNumbers     := TRUE;
  printDate            := TRUE;
  
  CommentColor         := GlobWin.RGB(COMMENT_RED, COMMENT_GREEN, COMMENT_BLUE);
                                                           (* KlS, 2003OCT23 *)
  KeyWordColor         := GlobWin.RGB(KEYWORD_RED, KEYWORD_GREEN, KEYWORD_BLUE);
                                                           (* KlS, 2003NOV03 *)
  StringsColor         := GlobWin.RGB(STRINGS_RED, STRINGS_GREEN, STRINGS_BLUE);

  (* Data for Syntax Colouring;                               KlS, 2004APR19 *)
  NEW(ActSyntaxColouring);
  AnchorSyntaxColouring                := ActSyntaxColouring;
  ActSyntaxColouring.Next              := NIL;
  ActSyntaxColouring.Extension         := ".mod";          (* Oberon-2 files *)
  ActSyntaxColouring.CommentStart      := "(*";
  ActSyntaxColouring.CommentEnd        := "*)";
  ActSyntaxColouring.CommentLine       := "";
  ActSyntaxColouring.CommentsNested    := TRUE;
  ActSyntaxColouring.StringDelims[0]   := '"';
  ActSyntaxColouring.StringDelims[1]   := "'";
  ActSyntaxColouring.StringDelims[2]   :=   0X;
  ActSyntaxColouring.KeyWords[01]      := "ABS";           (* KlS, 2004APR19 *)
  ActSyntaxColouring.KeyWords[02]      := "ASH";
  ActSyntaxColouring.KeyWords[03]      := "ASSERT";
  ActSyntaxColouring.KeyWords[04]      := "ARRAY";
  ActSyntaxColouring.KeyWords[05]      := "BEGIN";
  ActSyntaxColouring.KeyWords[06]      := "BOOLEAN";
  ActSyntaxColouring.KeyWords[07]      := "BY";
  ActSyntaxColouring.KeyWords[08]      := "CAP";
  ActSyntaxColouring.KeyWords[09]      := "CASE";
  ActSyntaxColouring.KeyWords[10]      := "CHAR";
  ActSyntaxColouring.KeyWords[11]      := "CHR";
  ActSyntaxColouring.KeyWords[12]      := "CONST";
  ActSyntaxColouring.KeyWords[13]      := "COPY";
  ActSyntaxColouring.KeyWords[14]      := "DEC";
  ActSyntaxColouring.KeyWords[15]      := "DEFINITION";
  ActSyntaxColouring.KeyWords[16]      := "DISPOSE";
  ActSyntaxColouring.KeyWords[17]      := "DIV";
  ActSyntaxColouring.KeyWords[18]      := "DO";
  ActSyntaxColouring.KeyWords[19]      := "ELSE";
  ActSyntaxColouring.KeyWords[20]      := "ELSIF";
  ActSyntaxColouring.KeyWords[21]      := "END";
  ActSyntaxColouring.KeyWords[22]      := "ENTIER";
  ActSyntaxColouring.KeyWords[23]      := "EXCL";
  ActSyntaxColouring.KeyWords[24]      := "EXIT";
  ActSyntaxColouring.KeyWords[25]      := "FALSE";
  ActSyntaxColouring.KeyWords[26]      := "FOR";
  ActSyntaxColouring.KeyWords[27]      := "HALT";
  ActSyntaxColouring.KeyWords[28]      := "IF";
  ActSyntaxColouring.KeyWords[29]      := "IMPORT";
  ActSyntaxColouring.KeyWords[30]      := "IN";
  ActSyntaxColouring.KeyWords[31]      := "INC";
  ActSyntaxColouring.KeyWords[32]      := "INCL";
  ActSyntaxColouring.KeyWords[33]      := "INTEGER";
  ActSyntaxColouring.KeyWords[34]      := "IS";
  ActSyntaxColouring.KeyWords[35]      := "LEN";
  ActSyntaxColouring.KeyWords[36]      := "LONG";
  ActSyntaxColouring.KeyWords[37]      := "LONGINT";
  ActSyntaxColouring.KeyWords[38]      := "LONGREAL";
  ActSyntaxColouring.KeyWords[39]      := "LOOP";
  ActSyntaxColouring.KeyWords[40]      := "MAX";
  ActSyntaxColouring.KeyWords[41]      := "MIN";
  ActSyntaxColouring.KeyWords[42]      := "MOD";
  ActSyntaxColouring.KeyWords[43]      := "MODULE";
  ActSyntaxColouring.KeyWords[44]      := "NEW";
  ActSyntaxColouring.KeyWords[45]      := "NIL";
  ActSyntaxColouring.KeyWords[46]      := "ODD";
  ActSyntaxColouring.KeyWords[47]      := "OF";
  ActSyntaxColouring.KeyWords[48]      := "OR";
  ActSyntaxColouring.KeyWords[49]      := "ORD";
  ActSyntaxColouring.KeyWords[50]      := "POINTER";
  ActSyntaxColouring.KeyWords[51]      := "PROCEDURE";
  ActSyntaxColouring.KeyWords[52]      := "REAL";
  ActSyntaxColouring.KeyWords[53]      := "RECORD";
  ActSyntaxColouring.KeyWords[54]      := "REPEAT";
  ActSyntaxColouring.KeyWords[55]      := "RETURN";
  ActSyntaxColouring.KeyWords[56]      := "SHORT";
  ActSyntaxColouring.KeyWords[57]      := "SET";
  ActSyntaxColouring.KeyWords[58]      := "SHORT";
  ActSyntaxColouring.KeyWords[59]      := "SHORTINT";
  ActSyntaxColouring.KeyWords[60]      := "SIZE";
  ActSyntaxColouring.KeyWords[61]      := "THEN";
  ActSyntaxColouring.KeyWords[62]      := "TO";
  ActSyntaxColouring.KeyWords[63]      := "TRUE";
  ActSyntaxColouring.KeyWords[64]      := "TYPE";
  ActSyntaxColouring.KeyWords[65]      := "UNTIL";
  ActSyntaxColouring.KeyWords[66]      := "VAR";
  ActSyntaxColouring.KeyWords[67]      := "WHILE";
  ActSyntaxColouring.KeyWords[68]      := "WITH";
  ActSyntaxColouring.NoOfKeyWords      := 68;
  FOR i:=1 TO ActSyntaxColouring.NoOfKeyWords DO
    ActSyntaxColouring.KeyWordLength[i]   := Strings.Length(ActSyntaxColouring.KeyWords[i]);
  END (* FOR i:=1 TO ActSyntaxColouring.NoOfKeyWords *);

END Options.


