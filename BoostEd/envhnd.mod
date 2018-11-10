(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     EnvHnd                                      V 2.00.14         *)
(*                                                         2005JUN14         *)
(*  PURPOSE:   This module contains the access procedures to the .INI file   *)
(*             to load and store the settings of the editor options.         *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   GetIniFileName                                                          *)
(*   ReadIniFile                                                             *)
(*   WriteIniFile                                                            *)
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
(*   2004MAY01 KlS     keyword colouring added to *.ini file                 *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE EnvHnd;


IMPORT
  GlobWin, Options, Syntax, 
  Strings, 
  WinBase, WinDef, 
  SYSTEM;


CONST
  SECTION              =              "BoostEd";
  SECTION2             =              "SyntaxColouring";
  INIFILENAME          =              "BoostEd.INI";


TYPE
  ARCHAR               =               ARRAY OF CHAR;


VAR
  MyString:                            ARRAY 4096 OF CHAR;
  MySection:                           ARRAY 256 OF CHAR;
  ResultBool:                          WinDef.BOOL;
  MySyntaxColouring:                   Options.SyntaxColouringP;


(*****************************************************************************)
(* INI Dateiname liefern                                                     *)
PROCEDURE GetIniFileName              (VAR Name:           ARRAY OF CHAR)
                                      :BOOLEAN;
VAR
  Buffer:                              ARRAY 256 OF CHAR;
  Length,
  i:                                   LONGINT;
BEGIN
  (* Verzeichnis von Windows ermitteln *)
  Length       := WinBase.GetWindowsDirectoryA(SYSTEM.ADR(Buffer), LEN(Buffer));
  IF Length=0 THEN
    RETURN FALSE
  END (* IF Length=0 *) ;
  IF Buffer[Length - 1]#"\" THEN
    Buffer[Length] := "\";
    Buffer[Length + 1] := 0X;
  END (* IF Buffer[Length - 1]#" *);
  COPY(Buffer, Name);
  Strings.Append(Name, INIFILENAME);
  RETURN TRUE;
END GetIniFileName;


(*****************************************************************************)
(* INI Datei lesen                                                           *)
PROCEDURE ReadIniFile*                ();
VAR
  Done:                                BOOLEAN;
  IniFileName:                         ARRAY 256 OF CHAR;
  i,
  j,
  Result:                              LONGINT;
  AllSections,
  default,
  Buffer:                              ARRAY 256 OF CHAR;

BEGIN
  IF ~GetIniFileName(IniFileName) THEN
    RETURN 
  END (* IF ~GetIniFileName(IniFileName) *);
  (* get list of ini file's sections *)
  Result       := WinBase.GetPrivateProfileSectionNamesA(SYSTEM.ADR(AllSections), 
  LEN(AllSections), 
  SYSTEM.ADR(IniFileName));
  IF Result=0 THEN
    GlobWin.DisplayError("ERROR", "ReadIniFile: Error in accessing INI-file");
    RETURN 
  ELSE
    FOR i:=0 TO Result DO
      IF AllSections[i]=0X THEN
        AllSections[i] := "§";
      END (* IF AllSections[i]=0X *) ;
    END (* FOR i:=0 TO Result *) ;
  END (* IF Result=0 *);

  Result                   := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                               SYSTEM.ADR("FontName"), 
                                                               SYSTEM.ADR(Options.FONT_FIXEDSYS), 
                                                               SYSTEM.ADR(Options.fontName), 
                                                               Options.FONTNAMELEN, 
                                                               SYSTEM.ADR(IniFileName));
  Result                   := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                               SYSTEM.ADR("PrinterFontName"), 
                                                               SYSTEM.ADR(Options.FONT_COURIERNEW), 
                                                               SYSTEM.ADR(Options.printerFontName), 
                                                               Options.FONTNAMELEN, 
                                                               SYSTEM.ADR(IniFileName));
  Options.autoIndent       := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("AutoIndent"), 1, SYSTEM.ADR(IniFileName)));
  Options.tabsize          := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("Tabsize"), 
                                                            4, 
                                                            SYSTEM.ADR(IniFileName));
  Options.useTabs          := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("UseTabs"), 1, SYSTEM.ADR(IniFileName)));
  Options.mouse            := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("Mouse"), 0, SYSTEM.ADR(IniFileName)));
  Options.fontSize         := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("FontSize"), 
                                                            12, 
                                                            SYSTEM.ADR(IniFileName));
  Options.printerFontSize  := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("PrinterFontSize"), 10, SYSTEM.ADR(IniFileName));
  Options.syntax           := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("Oberon2Syntax"), 1, SYSTEM.ADR(IniFileName)));
  Options.smartDel         := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("SmartLineMerge"), 1, SYSTEM.ADR(IniFileName)));
  Options.indentWidth      := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("IndentWidth"), 
                                                            2, 
                                                            SYSTEM.ADR(IniFileName));
  Options.colorComments    := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("ColorComments"), 1, SYSTEM.ADR(IniFileName)));
  Options.printMarginLeft  := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("PrintMarginLeft"), 
                                                            40, 
                                                            SYSTEM.ADR(IniFileName));
  Options.printMarginRight := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("PrintMarginRight"), 
                                                            40, 
                                                            SYSTEM.ADR(IniFileName));
  Options.printMarginTop   := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("PrintMarginTop"), 
                                                            60, 
                                                            SYSTEM.ADR(IniFileName));
  Options.printMarginBottom := WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                                                            SYSTEM.ADR("PrintMarginBottom"), 
                                                            100, 
                                                            SYSTEM.ADR(IniFileName));
  Options.printDate        := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("PrintDate"), 1, SYSTEM.ADR(IniFileName)));
  Options.printLineNumbers := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), SYSTEM.ADR("PrintLineNumbers"), 1, SYSTEM.ADR(IniFileName)));

  Strings.HexStr(GlobWin.RGB(Options.COMMENT_RED, 
                 Options.COMMENT_GREEN, 
                 Options.COMMENT_BLUE), 
                 default);
  Result                   := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                              SYSTEM.ADR("CommentColor"), 
                                                              SYSTEM.ADR(default), 
                                                              SYSTEM.ADR(Buffer), 
                                                              LEN(Buffer), 
                                                              SYSTEM.ADR(IniFileName));
  Options.CommentColor     := Strings.Val(Buffer);
  Strings.HexStr(GlobWin.RGB(Options.KEYWORD_RED, 
                 Options.KEYWORD_GREEN, 
                 Options.KEYWORD_BLUE), 
                 default);
  Result                   := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                               SYSTEM.ADR("KeyWordColor"), 
                                                               SYSTEM.ADR(default), 
                                                               SYSTEM.ADR(Buffer), 
                                                               LEN(Buffer), 
                                                               SYSTEM.ADR(IniFileName));
  Options.KeyWordColor     := Strings.Val(Buffer);
  Strings.HexStr(GlobWin.RGB(Options.STRINGS_RED, 
                 Options.STRINGS_GREEN, 
                 Options.STRINGS_BLUE), 
                 default);
  Result                   := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                               SYSTEM.ADR("StringsColor"), 
                                                               SYSTEM.ADR(default), 
                                                               SYSTEM.ADR(Buffer), 
                                                               LEN(Buffer), 
                                                               SYSTEM.ADR(IniFileName));
  Options.StringsColor     := Strings.Val(Buffer);

  (* read syntax colouring data                          *)
  IF Strings.Pos(SECTION2, AllSections, 1)=0 THEN
    RETURN 
  END (* IF Strings.Pos(SECTION2, AllSec *) ;
  (* clear all SyntaxColouring to 0                      *)
  Options.ActSyntaxColouring := Options.AnchorSyntaxColouring;
  MySyntaxColouring := Options.AnchorSyntaxColouring;
  WHILE Options.ActSyntaxColouring#NIL DO
    MySyntaxColouring := Options.ActSyntaxColouring.Next;
    DISPOSE(Options.ActSyntaxColouring);
    Options.ActSyntaxColouring := MySyntaxColouring;
  END (* WHILE Options.ActSyntaxColourin *) ;

  NEW(Options.AnchorSyntaxColouring);
  Options.ActSyntaxColouring := Options.AnchorSyntaxColouring;

  default[0]   := 0X;
  Done         := FALSE;
  i            := 0;
  MySection    := SECTION2;
  Strings.Append(MySection, "00");

  REPEAT
    Options.ActSyntaxColouring.Next := NIL;
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("Extension"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(Options.ActSyntaxColouring^.Extension), 
                                                   LEN(Options.ActSyntaxColouring^.Extension), 
                                                   SYSTEM.ADR(IniFileName));
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("CommentStart"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(Options.ActSyntaxColouring^.CommentStart), 
                                                   LEN(Options.ActSyntaxColouring^.CommentStart), 
                                                   SYSTEM.ADR(IniFileName));
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("CommentEnd"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(Options.ActSyntaxColouring^.CommentEnd), 
                                                   LEN(Options.ActSyntaxColouring^.CommentEnd), 
                                                   SYSTEM.ADR(IniFileName));
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("CommentLine"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(Options.ActSyntaxColouring^.CommentLine), 
                                                   LEN(Options.ActSyntaxColouring^.CommentLine), 
                                                   SYSTEM.ADR(IniFileName));
    Options.ActSyntaxColouring^.CommentsNested
               := SYSTEM.VAL(BOOLEAN, WinBase.GetPrivateProfileIntA(SYSTEM.ADR(SECTION), 
                             SYSTEM.ADR("CommentsNested"), 
                             1, 
                             SYSTEM.ADR(IniFileName)));
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("StringDelims"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(Options.ActSyntaxColouring^.StringDelims), 
                                                   LEN(Options.ActSyntaxColouring^.StringDelims), 
                                                   SYSTEM.ADR(IniFileName));
    Result     := WinBase.GetPrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                   SYSTEM.ADR("KeyWords"), 
                                                   SYSTEM.ADR(default), 
                                                   SYSTEM.ADR(MyString), 
                                                   LEN(MyString), 
                                                   SYSTEM.ADR(IniFileName));
    Options.ActSyntaxColouring^.NoOfKeyWords
               := 0;
    WHILE Strings.PosChar(";", MyString, 1)>0 DO
      INC(Options.ActSyntaxColouring^.NoOfKeyWords);
      Strings.Copy(MyString, Options.ActSyntaxColouring^.KeyWords[Options.ActSyntaxColouring^.NoOfKeyWords], 1, Strings.PosChar(";", MyString, 1)-1);
      Strings.Delete(MyString, 1, Strings.PosChar(";", MyString, 1));
      Options.ActSyntaxColouring^.KeyWordLength[Options.ActSyntaxColouring^.NoOfKeyWords]
               := Strings.Length(Options.ActSyntaxColouring^.KeyWords[Options.ActSyntaxColouring^.NoOfKeyWords]);
    END (* WHILE Strings.PosChar(", MyStri *) ;

    (* try reading next section                          *)
    INC(i);
    Strings.Str(i, MySection);
    IF i<10 THEN
      Strings.InsertChar("0", MySection, 1);
    END (* IF i<10 *) ;
    Strings.Insert(SECTION2, MySection, 1);
    IF Strings.Pos(MySection, AllSections, 1)=0 THEN
      Done     := TRUE;
    ELSE
      NEW(Options.ActSyntaxColouring.Next);
      Options.ActSyntaxColouring := Options.ActSyntaxColouring.Next;
    END (* IF Strings.Pos(MySection, AllSe *) ;
  UNTIL Done;
END ReadIniFile;


(*****************************************************************************)
(*****************************************************************************)
(* Integer Wert in INI Datei schreiben                                       *)
PROCEDURE InsertIniInt                (entry:              ARRAY OF CHAR;
                                       val:                LONGINT;
                                       IniFileName:        ARRAY OF CHAR);
BEGIN
  Strings.Str(val, MyString);
  ResultBool   := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
  SYSTEM.ADR(entry), 
  SYSTEM.ADR(MyString), 
  SYSTEM.ADR(IniFileName));
END InsertIniInt;


(*****************************************************************************)
(* Integer Wert in INI Datei schreiben                                       *)
PROCEDURE InsertIniHexInt             (entry:              ARRAY OF CHAR;
                                       val:                LONGINT;
                                       IniFileName:        ARRAY OF CHAR);
BEGIN
  Strings.HexStr(val, MyString);
  ResultBool   := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
  SYSTEM.ADR(entry), 
  SYSTEM.ADR(MyString), 
  SYSTEM.ADR(IniFileName));
END InsertIniHexInt;


(*****************************************************************************)
(* BOOL Wert in INI Datei schreiben                                          *)
PROCEDURE InsertIniBool               (entry:              ARRAY OF CHAR;
                                       val:                BOOLEAN;
                                       IniFileName:        ARRAY OF CHAR);
BEGIN
  IF val THEN
    MyString   := "1"
  ELSE
    MyString   := "0"
  END (* IF val *);
  ResultBool   := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
  SYSTEM.ADR(entry), 
  SYSTEM.ADR(MyString), 
  SYSTEM.ADR(IniFileName));
END InsertIniBool;


(*****************************************************************************)
(* INI Datei schreiben                                                       *)
PROCEDURE WriteIniFile*               ();
VAR
  i,
  j:                                   LONGINT;
  IniFileName:                         ARRAY 256 OF CHAR;
  Ready:                               BOOLEAN;
  ResultBool:                          WinDef.BOOL;

BEGIN
  IF ~GetIniFileName(IniFileName) THEN
    GlobWin.DisplayError("ERROR", "WriteIniFile: Error in accessing INI-file");
    RETURN 
  END (* IF ~GetIniFileName(IniFileName) *) ;
  COPY(SECTION, MySection);
  ResultBool   := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                     SYSTEM.ADR("FontName"), 
                                                     SYSTEM.ADR(Options.fontName), 
                                                     SYSTEM.ADR(IniFileName));
  IF ResultBool=0 THEN
    GlobWin.DisplayError("ERROR", "No access to Editor-INI-file");
    RETURN 
  END (* IF ResultBool=0 *) ;
  ResultBool   := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(SECTION), 
                                                     SYSTEM.ADR("PrinterFontName"), 
                                                     SYSTEM.ADR(Options.printerFontName), 
                                                     SYSTEM.ADR(IniFileName));
  InsertIniBool("AutoIndent", Options.autoIndent, IniFileName);
  InsertIniInt("Tabsize", Options.tabsize, IniFileName);
  InsertIniBool("UseTabs", Options.useTabs, IniFileName);
  InsertIniBool("Mouse", Options.mouse, IniFileName);
  InsertIniInt("FontSize", Options.fontSize, IniFileName);
  InsertIniInt("PrinterFontSize", Options.printerFontSize, IniFileName);
  InsertIniBool("Oberon2Syntax", Options.syntax, IniFileName);
  InsertIniBool("SmartLineMerge", Options.smartDel, IniFileName);
  InsertIniInt("IndentWidth", Options.indentWidth, IniFileName);
  InsertIniBool("ColorComments", Options.colorComments, IniFileName);
  InsertIniInt("PrintMarginLeft", Options.printMarginLeft, IniFileName);
  InsertIniInt("PrintMarginRight", Options.printMarginRight, IniFileName);
  InsertIniInt("PrintMarginTop", Options.printMarginTop, IniFileName);
  InsertIniInt("PrintMarginBottom", Options.printMarginBottom, IniFileName);
  InsertIniBool("PrintDate", Options.printDate, IniFileName);
  InsertIniBool("PrintLineNumbers", Options.printLineNumbers, IniFileName);
  InsertIniHexInt("CommentColor", Options.CommentColor, IniFileName);
  InsertIniHexInt("KeyWordColor", Options.KeyWordColor, IniFileName);
  InsertIniHexInt("StringsColor", Options.StringsColor, IniFileName);

  Options.ActSyntaxColouring := Options.AnchorSyntaxColouring;
  i            := 0;
  WHILE Options.ActSyntaxColouring#NIL DO
    Strings.Str(i, MySection);
    IF i<10 THEN
      Strings.InsertChar("0", MySection, 1);
    END (* IF i<10 *) ;
    Strings.Insert(SECTION2, MySection, 1);
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("Extension"), 
                                                     SYSTEM.ADR(Options.ActSyntaxColouring^.Extension), 
                                                     SYSTEM.ADR(IniFileName));
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("CommentStart"), 
                                                     SYSTEM.ADR(Options.ActSyntaxColouring^.CommentStart), 
                                                     SYSTEM.ADR(IniFileName));
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("CommentEnd"), 
                                                     SYSTEM.ADR(Options.ActSyntaxColouring^.CommentEnd), 
                                                     SYSTEM.ADR(IniFileName));
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("CommentLine"), 
                                                     SYSTEM.ADR(Options.ActSyntaxColouring^.CommentLine), 
                                                     SYSTEM.ADR(IniFileName));
    InsertIniBool("CommentsNested", Options.ActSyntaxColouring.CommentsNested, IniFileName);
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("StringDelims"), 
                                                     SYSTEM.ADR(Options.ActSyntaxColouring^.StringDelims), 
                                                     SYSTEM.ADR(IniFileName));
    MyString[0] := 0X;
    FOR j      := 1 TO Options.ActSyntaxColouring^.NoOfKeyWords DO
      Strings.Append(MyString, Options.ActSyntaxColouring^.KeyWords[j]);
      Strings.AppendChar(MyString, ";");
    END (* FOR j:=1 TO Options.ActSyntaxCo *) ;
    ResultBool := WinBase.WritePrivateProfileStringA(SYSTEM.ADR(MySection), 
                                                     SYSTEM.ADR("KeyWords"), 
                                                     SYSTEM.ADR(MyString), 
                                                     SYSTEM.ADR(IniFileName));
    INC(i);
    Options.ActSyntaxColouring := Options.ActSyntaxColouring^.Next;
  END (* WHILE Options.ActSyntaxColourin *) ;
END WriteIniFile;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
  ;
END EnvHnd.



