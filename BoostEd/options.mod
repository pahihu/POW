(*****************************************************************************)
(*  Module Options                                         V 1.00.16         *)
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
  MAXDELIMLEN*         =               32;                 (* maximum length for comment delimiters *)
  MAXSTRINGDELIMS*     =               3;                  (* maximum number of different string literal delimiters *)

  COMMENT_RED*         =               0;                  (* color default for comments *)
  COMMENT_GREEN*       =               180;                (* color default for comments *)
  COMMENT_BLUE*        =               0;                  (* color default for comments *)

  KEYWORD_RED*         =               0;                  (* color default for keywords; KlS, 2003OCT23 *)
  KEYWORD_GREEN*       =               0;                  (* color default for keywords *)
  KEYWORD_BLUE*        =               255;                (* color default for keywords *)

  STRINGS_RED*         =               0;                  (* color default for strings;  KlS, 2003NOV03 *)
  STRINGS_GREEN*       =               0;                  (* color default for strings *)
  STRINGS_BLUE*        =               128;                (* color default for strings *)

  MaxKeyWords*         =               128;                (* KlS, 2003OCT24 *)


TYPE
  SyntaxColouringP*    =               POINTER TO SyntaxColouringT;
  SyntaxColouringT*    = RECORD
    Next*:                             SyntaxColouringP;
    Extension*:                        ARRAY 256 OF CHAR;
    CommentStart*,
    CommentEnd*,
    CommentLine*:                      ARRAY MAXDELIMLEN OF CHAR;
    (* currently not used *)
    CommentsNested*:                   BOOLEAN;
    StringDelims*:                     ARRAY MAXSTRINGDELIMS+1 OF CHAR;
    NoOfKeyWords*:                     LONGINT;
    KeyWords*:                         ARRAY MaxKeyWords OF ARRAY 32 OF CHAR;
    KeyWordLength*:                    ARRAY MaxKeyWords OF LONGINT;
  END (* SyntaxColouringT* *) ;


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


(*****************************************************************************)
PROCEDURE TmpSave*                    ();
(* Tempdaten speichern *)
BEGIN
  hsyntax      := syntax;
  hsmartDel    := smartDel;
  hindentWidth := indentWidth;
  hinsert      := insert;
  hautoIndent  := autoIndent;
  htabsize     := tabsize;
  huseTabs     := useTabs;
  hmouse       := mouse;
  hfontSize    := fontSize;
  COPY(fontName, hfontName);
  hPrinterFontSize := printerFontSize;
  COPY(printerFontName, hPrinterFontName);
  hColorComments := colorComments;
END TmpSave;


(*****************************************************************************)
PROCEDURE Restore*                    ();
(* Restore Data *)

BEGIN
  syntax       := hsyntax;
  smartDel     := hsmartDel;
  indentWidth  := hindentWidth;
  insert       := hinsert;
  autoIndent   := hautoIndent;
  tabsize      := htabsize;
  useTabs      := huseTabs;
  mouse        := hmouse;
  fontSize     := hfontSize;
  COPY(hfontName, fontName);
  printerFontSize := hPrinterFontSize;
  COPY(hPrinterFontName, printerFontName);
  colorComments := hColorComments;
END Restore;


(*****************************************************************************)
(*****************************************************************************)
(* Initialisierung der Einstellungen *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN
  autoIndent   := TRUE;
  insert       := TRUE;
  useTabs      := TRUE;
  tabsize      := 3;
  mouse        := TRUE;
  syntax       := TRUE;
  smartDel     := TRUE;
  indentWidth  := 2;
  insert       := TRUE;
  colorComments := TRUE;
  fontSize     := 12;
  fontName     := FONT_FIXEDSYS;
  printerFontSize := 10;
  printerFontName := FONT_COURIERNEW;

  printMarginLeft := 40;
  printMarginRight := 40;
  printMarginTop := 60;
  printMarginBottom := 100;
  printLineNumbers := TRUE;
  printDate    := TRUE;

  CommentColor := GlobWin.RGB(COMMENT_RED, COMMENT_GREEN, COMMENT_BLUE);
  (* KlS, 2003OCT23 *)
  KeyWordColor := GlobWin.RGB(KEYWORD_RED, KEYWORD_GREEN, KEYWORD_BLUE);
  (* KlS, 2003NOV03 *)
  StringsColor := GlobWin.RGB(STRINGS_RED, STRINGS_GREEN, STRINGS_BLUE);

  (* Data for Syntax Colouring;                               KlS, 2004APR19 *)
  NEW(AnchorSyntaxColouring);
  AnchorSyntaxColouring.Next := NIL;
  AnchorSyntaxColouring.Extension := ".mod";               (* Oberon-2 files *)
  AnchorSyntaxColouring.CommentStart := "(*";
  AnchorSyntaxColouring.CommentEnd := "*)";
  AnchorSyntaxColouring.CommentLine := "";
  AnchorSyntaxColouring.CommentsNested := TRUE;
  AnchorSyntaxColouring.StringDelims[0] := '"';
  AnchorSyntaxColouring.StringDelims[1] := "'";
  AnchorSyntaxColouring.StringDelims[2] := 0X;
  AnchorSyntaxColouring.KeyWords[01] := "ABS";             (* KlS, 2004APR19 *)
  AnchorSyntaxColouring.KeyWords[02] := "ASH";
  AnchorSyntaxColouring.KeyWords[03] := "ASSERT";
  AnchorSyntaxColouring.KeyWords[04] := "ARRAY";
  AnchorSyntaxColouring.KeyWords[05] := "BEGIN";
  AnchorSyntaxColouring.KeyWords[06] := "BOOLEAN";
  AnchorSyntaxColouring.KeyWords[07] := "BY";
  AnchorSyntaxColouring.KeyWords[08] := "CAP";
  AnchorSyntaxColouring.KeyWords[09] := "CASE";
  AnchorSyntaxColouring.KeyWords[10] := "CHAR";
  AnchorSyntaxColouring.KeyWords[11] := "CHR";
  AnchorSyntaxColouring.KeyWords[12] := "CONST";
  AnchorSyntaxColouring.KeyWords[13] := "COPY";
  AnchorSyntaxColouring.KeyWords[14] := "DEC";
  AnchorSyntaxColouring.KeyWords[15] := "DEFINITION";
  AnchorSyntaxColouring.KeyWords[16] := "DISPOSE";
  AnchorSyntaxColouring.KeyWords[17] := "DIV";
  AnchorSyntaxColouring.KeyWords[18] := "DO";
  AnchorSyntaxColouring.KeyWords[19] := "ELSE";
  AnchorSyntaxColouring.KeyWords[20] := "ELSIF";
  AnchorSyntaxColouring.KeyWords[21] := "END";
  AnchorSyntaxColouring.KeyWords[22] := "ENTIER";
  AnchorSyntaxColouring.KeyWords[23] := "EXCL";
  AnchorSyntaxColouring.KeyWords[24] := "EXIT";
  AnchorSyntaxColouring.KeyWords[25] := "FALSE";
  AnchorSyntaxColouring.KeyWords[26] := "FOR";
  AnchorSyntaxColouring.KeyWords[27] := "HALT";
  AnchorSyntaxColouring.KeyWords[28] := "IF";
  AnchorSyntaxColouring.KeyWords[29] := "IMPORT";
  AnchorSyntaxColouring.KeyWords[30] := "IN";
  AnchorSyntaxColouring.KeyWords[31] := "INC";
  AnchorSyntaxColouring.KeyWords[32] := "INCL";
  AnchorSyntaxColouring.KeyWords[33] := "INTEGER";
  AnchorSyntaxColouring.KeyWords[34] := "IS";
  AnchorSyntaxColouring.KeyWords[35] := "LEN";
  AnchorSyntaxColouring.KeyWords[36] := "LONG";
  AnchorSyntaxColouring.KeyWords[37] := "LONGINT";
  AnchorSyntaxColouring.KeyWords[38] := "LONGREAL";
  AnchorSyntaxColouring.KeyWords[39] := "LOOP";
  AnchorSyntaxColouring.KeyWords[40] := "MAX";
  AnchorSyntaxColouring.KeyWords[41] := "MIN";
  AnchorSyntaxColouring.KeyWords[42] := "MOD";
  AnchorSyntaxColouring.KeyWords[43] := "MODULE";
  AnchorSyntaxColouring.KeyWords[44] := "NEW";
  AnchorSyntaxColouring.KeyWords[45] := "NIL";
  AnchorSyntaxColouring.KeyWords[46] := "ODD";
  AnchorSyntaxColouring.KeyWords[47] := "OF";
  AnchorSyntaxColouring.KeyWords[48] := "OR";
  AnchorSyntaxColouring.KeyWords[49] := "ORD";
  AnchorSyntaxColouring.KeyWords[50] := "POINTER";
  AnchorSyntaxColouring.KeyWords[51] := "PROCEDURE";
  AnchorSyntaxColouring.KeyWords[52] := "REAL";
  AnchorSyntaxColouring.KeyWords[53] := "RECORD";
  AnchorSyntaxColouring.KeyWords[54] := "REPEAT";
  AnchorSyntaxColouring.KeyWords[55] := "RETURN";
  AnchorSyntaxColouring.KeyWords[56] := "SHORT";
  AnchorSyntaxColouring.KeyWords[57] := "SET";
  AnchorSyntaxColouring.KeyWords[58] := "SHORT";
  AnchorSyntaxColouring.KeyWords[59] := "SHORTINT";
  AnchorSyntaxColouring.KeyWords[60] := "SIZE";
  AnchorSyntaxColouring.KeyWords[61] := "THEN";
  AnchorSyntaxColouring.KeyWords[62] := "TO";
  AnchorSyntaxColouring.KeyWords[63] := "TRUE";
  AnchorSyntaxColouring.KeyWords[64] := "TYPE";
  AnchorSyntaxColouring.KeyWords[65] := "UNTIL";
  AnchorSyntaxColouring.KeyWords[66] := "VAR";
  AnchorSyntaxColouring.KeyWords[67] := "WHILE";
  AnchorSyntaxColouring.KeyWords[68] := "WITH";
  AnchorSyntaxColouring.NoOfKeyWords := 68;
  FOR i:=1 TO AnchorSyntaxColouring.NoOfKeyWords DO
    AnchorSyntaxColouring.KeyWordLength[i] := Strings.Length(AnchorSyntaxColouring.KeyWords[i]);
  END (* FOR i:=1 TO AnchorSyntaxColouri *) ;
  ActSyntaxColouring := AnchorSyntaxColouring;

END Options.



