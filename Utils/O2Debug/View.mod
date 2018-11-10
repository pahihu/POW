(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     View                                        V 1.42.13         *)
(*                                                         2002APR05         *)
(*  PURPOSE:   functions of menu point "View"                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Update    rewrites the scrren                                           *)
(*   AppendLine                                                              *)
(*             appends a line to the screen contents, the line will be       *)
(*             displayed at the end of the screen                            *)
(*   Switch2NewView                                                          *)
(*             changes the content of the screen when a new view is called   *)
(*   UpAndDown vertical scroll bar                                           *)
(*   LeftAndRight                                                            *)
(*             horizontal scroll bar                                         *)
(*   UseNewSize                                                              *)
(*             sets the screen metrics whenever the screen dimension changes *)
(*   SetMenu   enables & disables menu points                                *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000APR10                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE View;

IMPORT
  WinBase, WinDef, WinGDI, WinUser, WinNT,
  Strings, SYSTEM,
  Dump, Dump_DD, Dump_DD_debug, Dump_Sections, HexData, StatusLine,
  Global;

CONST
  Version*             =              "V 1.42.13";
  Module*              =              "View";
  
  LineEmpty    =                      "                                                                 ";
  
  
TYPE
  LineFormat =  RECORD
    Font:                              WinDef.HFONT;
    Height:                            REAL;               (* Line Spacing *)
  END (* LineFormat *);
  

VAR
  hDC:                                 WinDef.HDC;
  MyFonts:                             ARRAY 15 OF LineFormat;
  hInst:                               WinDef.HANDLE;
  
  ReturnCode:                          LONGINT;
  i:                                   INTEGER;
  NumberOfColumns,
  NumberOfLines:                       LONGINT;
  ActLine,
  MyLine:                              Global.ScreenLineP;
  HeadLines:                           ARRAY 4 OF Global.ScreenLineT;
  
  WriteLines:                          Global.WriteLinesT;


(*****************************************************************************)
(*                                                                           *)
(* EZCreateFont                                                              *)
(* returns the handle of a (logical) font                                    *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  hFont      handle to the desired font                                    *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE EZCreateFont                (hDC:                WinDef.HDC;         (* Kontext handle         *)
                                       FaceName:           ARRAY OF CHAR;
                                       DeciPtHeight,
                                       DeciPtWidth,
                                       Attributes:         INTEGER;
                                       LogRes:             BOOLEAN)
                                      :WinDef.HFONT;
                                      
VAR
  cxDpI,
  cyDpI:                               REAL;
  hFont:                               WinDef.HFONT;
  LogFont:                             WinGDI.LOGFONTA;
  Point:                               WinDef.POINT;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  XForm:                               WinGDI.XFORM;

BEGIN

  Result       := WinGDI.SaveDC (hDC);
  hFont        :=  0;
  
  Result       := WinGDI.SetGraphicsMode (hDC, WinGDI.GM_ADVANCED);
  ResultBool   := WinGDI.ModifyWorldTransform (hDC, NIL, WinGDI.MWT_IDENTITY);
  Result       := WinGDI.SetViewportOrgEx (hDC, 0, 0, WinDef.NULL);
  Result       := WinGDI.SetWindowOrgEx (hDC, 0, 0, WinDef.NULL);
  
(*  IF LogRes THEN
    cxDpI := WinGDI.GetDeviceCaps(hDC, WinGDI.LOGPIXELSX);
    cyDpI := WinGDI.GetDeviceCaps(hDC, WinGDI.LOGPIXELSY);
  ELSE
    cxDpI := 25.4 * WinGDI.GetDeviceCaps(hDC, WinGDI.HORZRES)/WinGDI.GetDeviceCaps(hDC, WinGDI.HORZSIZE);
    cyDpI := 25.4 * WinGDI.GetDeviceCaps(hDC, WinGDI.VERTRES)/WinGDI.GetDeviceCaps(hDC, WinGDI.VERTSIZE);
  END;
  cxDpI        := cxDpI/72;
  cyDpI        := cyDpI/72;
  Point.x      := DeciPtWidth  * SYSTEM.VAL(LONGINT, cxDpI);
  Point.y      := DeciPtHeight * SYSTEM.VAL(LONGINT, cyDpI);
  
  Result       := WinGDI.DPtoLP (hDC, SYSTEM.ADR(Point), 1);
*)
  LogFont.lfHeight             := -DeciPtHeight;
  LogFont.lfWidth              :=  0;
  LogFont.lfEscapement         :=  0;
  LogFont.lfOrientation        :=  0;
  IF SYSTEM.BITAND(Attributes, Global.EZ_ATTR_BOLD)=Global.EZ_ATTR_BOLD THEN
    LogFont.lfWeight           := 700;
  ELSE
    LogFont.lfWeight           :=   0;
  END;
  IF SYSTEM.BITAND(Attributes, Global.EZ_ATTR_ITALIC)=Global.EZ_ATTR_ITALIC THEN
    LogFont.lfItalic           :=  1X;
  ELSE
    LogFont.lfItalic           :=  0X;
  END;
  IF SYSTEM.BITAND(Attributes, Global.EZ_ATTR_UNDERLINE)=Global.EZ_ATTR_UNDERLINE THEN
    LogFont.lfUnderline        :=  1X;
  ELSE
    LogFont.lfUnderline        :=  0X;
  END;
  IF SYSTEM.BITAND(Attributes, Global.EZ_ATTR_STRIKEOUT)=Global.EZ_ATTR_STRIKEOUT THEN
    LogFont.lfStrikeOut        :=  1X;
  ELSE
    LogFont.lfStrikeOut        :=  0X;
  END;
  LogFont.lfCharSet            :=  0X;
  LogFont.lfOutPrecision       :=  0X;
  LogFont.lfClipPrecision      :=  0X;
  LogFont.lfQuality            :=  0X;
  LogFont.lfPitchAndFamily     :=  SYSTEM.VAL(CHAR, WinGDI.FIXED_PITCH);
  COPY(FaceName, LogFont.lfFaceName);
  
  hFont        := WinGDI.CreateFontIndirectA (LogFont);
  
  IF hFont=0 THEN
    HALT(0);
  END;
  
  Result       := WinGDI.RestoreDC (hDC, -1);
  RETURN hFont
END EZCreateFont;


(*****************************************************************************)
(*                                                                           *)
(* Update                                                                    *)
(* rewrites the screen                                                       *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Update*                     (hWnd:               WinDef.HWND)        (* Window handle          *)
                                      : WinDef.LRESULT;

VAR
  ActLine,
  MyLine:                              Global.ScreenLineP;
  Done:                                BOOLEAN;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  lpScreenMetric:                      Global.ScreenMetricP;
  ActLineLength,
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  hDC:                                 WinDef.HDC;
  TextMetric:                          WinGDI.TEXTMETRICA;
  ResulthGDIobj:                       WinDef.HGDIOBJ;
  i, j:                                LONGINT;
  HeightOfLine,
  xPos,
  yPos:                                LONGINT;

BEGIN;
  
  lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
  ActLine        := lpScreenMetric^.FirstLineOnScreen;

  hDC            := WinUser.BeginPaint  (hWnd, PaintStructure);
  
  xPos           := 10;
  yPos           := 10;
  
  IF lpScreenMetric^.Formatted THEN
    FOR i:=1 TO 3 DO
      ResulthGDIobj  := WinGDI.SelectObject (hDC, MyFonts[HeadLines[i].Format].Font);
      ResulthGDIobj  := WinGDI.GetTextMetricsA (hDC, TextMetric);
      HeightOfLine   := SYSTEM.VAL(LONGINT, MyFonts[HeadLines[i].Format].Height * (TextMetric.tmHeight + TextMetric.tmExternalLeading));
      ActLineLength  := Strings.Length(HeadLines[i].Text)-lpScreenMetric^.NumberOfFirstColumn;
      Result         := WinGDI.TextOutA (hDC, 
                                     xPos, 
                                     yPos, 
                                     SYSTEM.ADR(HeadLines[i].Text[lpScreenMetric^.NumberOfFirstColumn]), 
                                     ActLineLength);
      INC(yPos, HeightOfLine);
    END (* FOR i:=1 TO 3 *);
    DEC(i);
  ELSE
    i := 0;
  END (* IF FORMATTED *);
  
  IF (lpScreenMetric^.NumberOfLines-lpScreenMetric^.NumberOfFirstLine)>lpScreenMetric^.LinesOnScreen THEN
    j      := lpScreenMetric^.LinesOnScreen;
  ELSE
    j      := lpScreenMetric^.NumberOfLines-lpScreenMetric^.NumberOfFirstLine;
  END(* IF (lpScreenMetric^.NumberOfLines-lpScreenMetric^.NumberOfFirstLine)>lpScreenMetric^.LinesOnScreen *);
  Done         := FALSE;
  
  REPEAT
    ResulthGDIobj  := WinGDI.SelectObject (hDC, MyFonts[ActLine^.Format].Font);
    ResulthGDIobj  := WinGDI.GetTextMetricsA (hDC, TextMetric);
    HeightOfLine   := SYSTEM.VAL(LONGINT, MyFonts[ActLine^.Format].Height * (TextMetric.tmHeight + TextMetric.tmExternalLeading));
    ActLineLength  := Strings.Length(ActLine^.Text)-lpScreenMetric^.NumberOfFirstColumn;
    IF ActLineLength>0 THEN
      Result     := WinGDI.TextOutA (hDC, 
                                     xPos, 
                                     yPos, 
                                     SYSTEM.ADR(ActLine^.Text[lpScreenMetric^.NumberOfFirstColumn]), 
                                     ActLineLength);
                                     
    (* set line parameter for mouse actions *)
    ActLine^.Area.top      := yPos + TextMetric.tmHeight;
    ActLine^.Area.bottom   := yPos;
    ActLine^.Area.left     := xPos;
    ActLine^.Area.right    := xPos + ActLineLength*TextMetric.tmAveCharWidth;
    
    END (* IF ... *);
    IF ActLine^.Next=NIL THEN
      Done     := TRUE
    ELSE
      ActLine  := ActLine^.Next;
    END;
    INC(i);
    INC(yPos, HeightOfLine);
    IF i>j THEN
      Done     := TRUE;
    END;
    IF i>lpScreenMetric^.LinesOnScreen THEN
      Done     := TRUE;
    END;
  UNTIL Done;
  
  Result       := WinUser.ReleaseDC(hWnd, hDC);
  Result       := WinUser.EndPaint (hWnd, PaintStructure);
  
  RETURN Result;

END Update;


(*****************************************************************************)
(*                                                                           *)
(* AppendLine                                                                *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  -                                                                        *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE AppendLine*                 (hWnd:               WinDef.HWND;
                                       Text:               ARRAY OF CHAR);

VAR
  ActLine,
  MyLine:                              Global.ScreenLineP;
  hDC:                                 WinDef.HDC;
  lpScreenMetric:                      Global.ScreenMetricP;
  Maximum:                             LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  ResulthGDIobj:                       WinDef.HGDIOBJ;
  HeightOfLine:                        LONGINT;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  TextMetric:                          WinGDI.TEXTMETRICA;

BEGIN;

  lpScreenMetric       := SYSTEM.VAL(Global.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
  ASSERT(lpScreenMetric#NIL);
  
  IF Strings.Length(Text)>lpScreenMetric^.NumberOfColumns THEN
    lpScreenMetric^.NumberOfColumns := Strings.Length(Text);
  END (* IF ... *);
  
  hDC                  := WinUser.BeginPaint  (hWnd, PaintStructure);
  
  ResulthGDIobj        := WinGDI.SelectObject (hDC, MyFonts[Global.Text01].Font);
  ResulthGDIobj        := WinGDI.GetTextMetricsA (hDC, TextMetric);
  HeightOfLine         := SYSTEM.VAL(LONGINT, MyFonts[Global.Text01].Height * (TextMetric.tmHeight + TextMetric.tmExternalLeading));

  ActLine              := lpScreenMetric^.FirstLine;
  WHILE ActLine^.Next#NIL DO
    ActLine := ActLine^.Next
  END;
  NEW(ActLine^.Next);
  
  MyLine               := ActLine;
  ActLine              := ActLine^.Next;
  COPY(Text, ActLine^.Text);
  ActLine^.Format      := Global.Text01;
  ActLine^.Next        := NIL;
  ActLine^.Previous    := MyLine;
  
  INC(lpScreenMetric^.NumberOfLines);
  IF lpScreenMetric^.NumberOfLines>=lpScreenMetric^.LinesOnScreen THEN
    INC(lpScreenMetric^.NumberOfFirstLine);
    lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen^.Next;
  END;

  Maximum              := lpScreenMetric^.NumberOfLines - lpScreenMetric^.LinesOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  Result               := WinUser.SetScrollRange (hWnd, 
                                                  WinUser.SB_VERT, 
                                                  0, 
                                                  SHORT(Maximum), 
                                                  WinDef.True);
  Result               := WinUser.SetScrollPos   (hWnd, 
                                                  WinUser.SB_VERT, 
                                                  SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                                  WinDef.True);

  Maximum              := lpScreenMetric^.NumberOfColumns - lpScreenMetric^.ColumnsOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  Result               := WinUser.SetScrollRange (hWnd, 
                                                  WinUser.SB_HORZ, 
                                                  0, 
                                                  SHORT(Maximum), 
                                                  WinDef.True);
  Result               := WinUser.SetScrollPos   (hWnd, 
                                                  WinUser.SB_HORZ, 
                                                  SHORT(lpScreenMetric^.NumberOfFirstColumn), 
                                                  WinDef.True);
  
  ResultBool           := WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
  Result               := Update(hWnd);
  
  Result               := WinUser.ReleaseDC(hWnd, hDC);
  Result               := WinUser.EndPaint (hWnd, PaintStructure);

END AppendLine;


(*****************************************************************************)
(*                                                                           *)
(* Switch2NewView                                                            *)
(* schaltet auf eine andere Darstellung um                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Mode       Global.Display Mode                                           *)
(*             which mode should be used, which data should be displayed     *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  Switch2NewView*            (hWnd:               WinDef.HWND;
                                       Mode:               INTEGER)
                                      : WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  hMenu:                               WinDef.HMENU;
  lpScreenMetric:                      Global.ScreenMetricP;
  i,
  Maximum,
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  hMenu        := WinUser.GetMenu(Global.hWndMain);
  
  FOR i:=Global.IDM_View_HexData TO Global.IDM_View_DD_TLS DO
    ResultBool   := WinUser.CheckMenuItem(hMenu, i, WinUser.MF_UNCHECKED);
  END (* FOR i:=Global.IDM_View_HexData *);

  CASE Mode OF
    Global.HexDataMode:
      WriteLines   := HexData.WriteLines;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_HexData, WinUser.MF_CHECKED);
    |
    Global.DebugDataMode:
      CASE Global.MyFileDescription.FileType OF
        Global.FileTypeLIB:
          WriteLines   := Dump.WriteLinesArchiveLIB;
        ELSE
          WriteLines   := Dump.WriteLines;
      END (* CASE *);
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_Dump, WinUser.MF_CHECKED);
    |
    Global.SectionHeadersMode:
      WriteLines   := Dump_Sections.WriteLines;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_SectionHeaders, WinUser.MF_CHECKED);
    |
    Global.SymbolTableMode:
      WriteLines   := Dump_DD_debug.WriteLinesST;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_SymbolTable, WinUser.MF_CHECKED);
    |
    Global.SymbolTableGlobalMode:
      WriteLines   := Dump_DD_debug.WriteLinesST2;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_SymbolTableGlobal, WinUser.MF_CHECKED);
    |
    Global.LineNumbersMode:
      WriteLines   := Dump_DD_debug.WriteLinesLN;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_LineNumbers, WinUser.MF_CHECKED);
    |
    Global.DD_ExportMode:
      WriteLines   := Dump_DD.WriteLines00;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Export, WinUser.MF_CHECKED);
    |
    Global.DD_ImportMode:
      WriteLines   := Dump_DD.WriteLines01;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Import, WinUser.MF_CHECKED);
    |
    Global.DD_ResourceMode:
      WriteLines   := Dump_DD.WriteLines02;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Resource, WinUser.MF_CHECKED);
    |
    Global.DD_ExceptionMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Export, WinUser.MF_CHECKED);
    |
    Global.DD_SecurityMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Security, WinUser.MF_CHECKED);
    |
    Global.DD_BaseRelocationMode:
      WriteLines   := Dump_DD.WriteLines05;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_BaseRelocation, WinUser.MF_CHECKED);
    |
    Global.DD_DebugMode:
      WriteLines   := Dump_DD_debug.WriteLines06;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Debug, WinUser.MF_CHECKED);
    |
    Global.DD_CopyrightMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Copyright, WinUser.MF_CHECKED);
    |
    Global.DD_GlobalPtrMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_GlobalPtr, WinUser.MF_CHECKED);
    |
    Global.DD_TLSMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_Debug, WinUser.MF_CHECKED);
    |
    Global.DD_LoadConfigMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_LoadConfig, WinUser.MF_CHECKED);
    |
    Global.DD_BoundImportMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_BoundImport, WinUser.MF_CHECKED);
    |
    Global.DD_IATMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_DD_IAT, WinUser.MF_CHECKED);
    ELSE
      HALT(0);
      RETURN 1
  END (* CASE Mode *);
  
  Global.DisplayMode   := Mode;

  (* set data for screen metric *)
  lpScreenMetric                       := SYSTEM.VAL(Global.ScreenMetricP, 
                                                     WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
  ASSERT(lpScreenMetric#NIL);
  lpScreenMetric^.NumberOfLines        := WriteLines(lpScreenMetric^.FirstLine, 
                                                     lpScreenMetric^.NumberOfColumns);

  IF lpScreenMetric^.Formatted THEN                        (* read headers if there are any *)
    ActLine    := lpScreenMetric^.FirstLine;
    FOR i:=1 TO 3 DO                                       (* reset header lines *)
      HeadLines[i].Text[0] := 0X;
      HeadLines[i].Format  := Global.Text01;
    END;
    Done       := FALSE;
    REPEAT
      CASE ActLine^.Format OF
        Global.Header1:
          IF HeadLines[1].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[1].Text);
            HeadLines[1].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        | (* Global.Header1 *)
        Global.Header2:
          IF HeadLines[2].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[2].Text);
            HeadLines[2].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        | (* Global.Header2 *)
        Global.Header3:
          IF HeadLines[3].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[3].Text);
            HeadLines[3].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        (* Global.Header3 *)
        ELSE
          ;
      END (* CASE ActLine^.Format *);
      ActLine := ActLine^.Next;
      IF ((HeadLines[1].Text[0]#0X) & (HeadLines[2].Text[0]#0X) & (HeadLines[3].Text[0]=0X)) THEN
        Done := TRUE;
      END;
      IF ActLine=NIL THEN
        Done := TRUE;
      END;
    UNTIL Done;
  END;
  lpScreenMetric^.NumberOfFirstLine    :=  0;
  lpScreenMetric^.NumberOfFirstColumn  :=  0;
  lpScreenMetric^.FirstLineOnScreen    := lpScreenMetric^.FirstLine;

  Maximum                              := lpScreenMetric^.NumberOfLines - lpScreenMetric^.LinesOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  
  Result                               := WinUser.SetScrollRange (hWnd, 
                                                                  WinUser.SB_VERT, 
                                                                  0, 
                                                                  SHORT(Maximum), 
                                                                  WinDef.False);
  Result                               := WinUser.SetScrollPos   (hWnd, 
                                                                  WinUser.SB_VERT, 
                                                                  lpScreenMetric^.NumberOfFirstLine, 
                                                                  WinDef.True);

  Maximum                              := lpScreenMetric^.NumberOfColumns - lpScreenMetric^.ColumnsOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  Result                               := WinUser.SetScrollRange (hWnd, 
                                                                  WinUser.SB_HORZ, 
                                                                  0, 
                                                                  SHORT(Maximum), 
                                                                  WinDef.False);
  Result                               := WinUser.SetScrollPos   (hWnd, 
                                                                  WinUser.SB_HORZ,
                                                                  lpScreenMetric^.NumberOfFirstColumn, 
                                                                  WinDef.True);

  ResultBool                           := WinUser.InvalidateRect(Global.hWndDump, NIL, WinDef.True);
  ReturnCode                           := Update(Global.hWndDump);
  
  RETURN WinUser.SetFocus(Global.hWndDump);
    
END Switch2NewView;


(*****************************************************************************)
(*                                                                           *)
(* UpAndDown                                                                 *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Mode       Global.Move Screen Content                                    *)
(*             moves the screen's content                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  UpAndDown*                (hWnd:                WinDef.HWND;
                                      Mode:                INTEGER)
                                      : WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  lpScreenMetric:                      Global.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));

  CASE Mode OF
    Global.LineUp:
      IF lpScreenMetric^.FirstLineOnScreen.Previous=NIL THEN
        RETURN 0;
      END (* IF FirstLineOnScreen.Next=NIL *);
      lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Previous;
      DEC(lpScreenMetric^.NumberOfFirstLine);
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_VERT, 
                                     SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                     WinDef.True);
      Result := Update(hWnd);
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.LineDown:
      IF (lpScreenMetric^.NumberOfFirstLine+lpScreenMetric^.LinesOnScreen)>lpScreenMetric^.NumberOfLines THEN
        RETURN 0;
      END;
      IF lpScreenMetric^.FirstLineOnScreen.Next=NIL THEN
        ;
      ELSE
        lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Next;
        IF lpScreenMetric^.Formatted THEN
          CASE lpScreenMetric^.FirstLineOnScreen^.Format OF
            Global.Header1:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[1].Text);
              HeadLines[1].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            | (* Global.Header1 *)
            Global.Header2:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[2].Text);
              HeadLines[2].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            | (* Global.Header2 *)
            Global.Header3:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[3].Text);
              HeadLines[3].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            (* Global.Header3 *)
            ELSE
              ;
          END(* CASE lpScreenMetric^.FirstLineOnScreen^.Format *);
        END;
        INC(lpScreenMetric^.NumberOfFirstLine)
      END (* IF FirstLineOnScreen.Next=NIL *);
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_VERT, 
                                     SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                     WinDef.True);
      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.PageUp:
      IF lpScreenMetric^.FirstLineOnScreen.Previous=NIL THEN
        RETURN 0;
      END (* IF FirstLineOnScreen.Next=NIL *);
      IF (lpScreenMetric^.NumberOfFirstLine-NumberOfLines)>0 THEN
        j := NumberOfLines-2;
        FOR i:=1 TO j DO
          lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Previous;
          DEC(lpScreenMetric^.NumberOfFirstLine)
        END;
      ELSE
        lpScreenMetric^.NumberOfFirstLine := 0;
        lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLine;
      END;
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_VERT, 
                                     SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                     WinDef.True);
      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.PageDown:
      IF (lpScreenMetric^.NumberOfFirstLine+lpScreenMetric^.LinesOnScreen)>lpScreenMetric^.NumberOfLines THEN
        RETURN 0;
      END;
      IF (lpScreenMetric^.NumberOfLines-lpScreenMetric^.NumberOfFirstLine)>=(lpScreenMetric^.LinesOnScreen-2) THEN
        j := lpScreenMetric^.LinesOnScreen-2
      ELSE
        j := lpScreenMetric^.NumberOfLines - lpScreenMetric^.NumberOfFirstLine
      END;
      FOR i:=1 TO j DO
        lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Next;
        INC(lpScreenMetric^.NumberOfFirstLine)
      END;
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_VERT, 
                                     SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                     WinDef.True);
      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.Home:
      lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLine;
      lpScreenMetric^.NumberOfFirstLine :=  0;
      Result            := WinUser.SetScrollPos(hWnd, WinUser.SB_VERT, 
                                                      SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                                      WinDef.True);
      Result            := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.End:
      IF NumberOfLines>lpScreenMetric^.NumberOfLines THEN
        RETURN 0;
      END;
      lpScreenMetric^.NumberOfFirstLine := lpScreenMetric^.NumberOfLines - NumberOfLines;
      i                                 :=  lpScreenMetric^.NumberOfFirstLine;
      lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLine;
      WHILE i>0 DO
        lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Next;
        DEC(i);
      END;
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_VERT, 
                                     SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                     WinDef.True);
      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    ELSE
      RETURN 1
    END (* CASE Mode *);

    RETURN 0
    
END UpAndDown;


(*****************************************************************************)
(*                                                                           *)
(* ThumbPosition                                                             *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Mode       Global.Move Screen Content                                    *)
(*             moves the screen's content                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  ThumbPosition*            (hWnd:                WinDef.HWND;
                                      Position:            INTEGER)
                                      : WinDef.LRESULT;
                                      
VAR
  lpScreenMetric:                      Global.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));

  IF Position>(lpScreenMetric^.NumberOfFirstLine-lpScreenMetric^.LinesOnScreen) THEN
    lpScreenMetric^.NumberOfFirstLine := lpScreenMetric^.NumberOfFirstLine-lpScreenMetric^.LinesOnScreen;
  ELSE
    lpScreenMetric^.NumberOfFirstLine := Position;
  END (* IF ... *);

  Result := WinUser.SetScrollPos(hWnd, 
                                 WinUser.SB_VERT, 
                                 SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                 WinDef.True);
  Result := Update(hWnd);

  (* den neu zu zeichnenden Bereich markieren ! *)
  RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);

END ThumbPosition;


(*****************************************************************************)
(*                                                                           *)
(* ThumbTrack                                                                *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Mode       Global.Move Screen Content                                    *)
(*             moves the screen's content                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  ThumbTrack*               (hWnd:                WinDef.HWND;
                                      Position:            INTEGER)
                                      : WinDef.LRESULT;
                                      
VAR
  lpScreenMetric:                      Global.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));

  i := Position - lpScreenMetric^.NumberOfFirstLine;
  
  IF i=0 THEN
    RETURN 0;
  END;
  
  IF i<0 THEN
    Result := UpAndDown(hWnd, Global.LineUp);
    RETURN 0;
  END;
  
  IF i>0 THEN
    Result := UpAndDown(hWnd, Global.LineDown);
    RETURN 0;
  END;


END ThumbTrack;


(*****************************************************************************)
(*                                                                           *)
(* LeftAndRight                                                              *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  Mode       Global.Move Screen Content                                    *)
(*             moves the screen's content                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  LeftAndRight*             (hWnd:                WinDef.HWND;
                                      Mode:                INTEGER)
                                      : WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  lpScreenMetric:                      Global.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  lpScreenMetric       := SYSTEM.VAL(Global.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));

  CASE Mode OF
    Global.Left:
      IF lpScreenMetric^.NumberOfFirstColumn=0 THEN
        RETURN 0;
      END (* IF lpScreenMetric^.NumberOfFirstColumn=0 *);
      
      IF lpScreenMetric^.NumberOfFirstColumn>5 THEN
        lpScreenMetric^.NumberOfFirstColumn  := lpScreenMetric^.NumberOfFirstColumn - 5;
      ELSE
        lpScreenMetric^.NumberOfFirstColumn  := 0;
      END (* IF lpScreenMetric^.NumberOfFirstColumn>5 *);

      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_HORZ, 
                                     SHORT(lpScreenMetric^.NumberOfFirstColumn), 
                                     WinDef.True);

      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Global.Right:
      IF (lpScreenMetric^.NumberOfFirstColumn+lpScreenMetric^.ColumnsOnScreen)>=lpScreenMetric^.NumberOfColumns THEN
        RETURN 0;
      ELSE
        lpScreenMetric^.NumberOfFirstColumn  := lpScreenMetric^.NumberOfFirstColumn+5;
      END;
      Result := WinUser.SetScrollPos(hWnd, 
                                     WinUser.SB_HORZ, 
                                     SHORT(lpScreenMetric^.NumberOfFirstColumn), 
                                     WinDef.True);

      Result := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    ELSE
      RETURN 1
    END (* CASE Mode *);

    RETURN 0
    
END LeftAndRight;


(*****************************************************************************)
(*                                                                           *)
(* UseNewSize                                                                *)
(* computes the number of lines that are visible on the screen               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*  lParam     the window's new dimension                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    return status code                                            *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  UseNewSize*                (hWnd:               WinDef.HWND; (* Window handle *)
                                       lParam:             WinDef.LPARAM)
                                      : WinDef.LRESULT;

VAR
  cx,
  cy:                                  LONGINT;
  cxChar,
  cyChar,
  Maximum:                             LONGINT;
  lpScreenMetric:                      Global.ScreenMetricP;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  TextMetric:                          WinGDI.TEXTMETRICA;
  hDC:                                 WinDef.HDC;

BEGIN;

  lpScreenMetric                       := SYSTEM.VAL(Global.ScreenMetricP, 
                                                     WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
                                                     
  IF hWnd=Global.hWndDebug THEN
    StatusLine.SetText("Debug Window changes size!", 1);
  END;

  cx                                   := SYSTEM.LOWORD(lParam);
  cy                                   := SYSTEM.HIWORD(lParam);

  hDC                                  := WinUser.GetDC(hWnd);
  ResultBool                           := WinGDI.GetTextMetricsA(hDC, TextMetric);
  
  cyChar                               := TextMetric.tmHeight + TextMetric.tmExternalLeading;
  cxChar                               := TextMetric.tmMaxCharWidth;
  
  lpScreenMetric^.LinesOnScreen        := cy DIV cyChar;
  Maximum                              := lpScreenMetric^.NumberOfLines - lpScreenMetric^.LinesOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  
  Result                               := WinUser.SetScrollRange (hWnd, 
                                                                  WinUser.SB_VERT, 
                                                                  0, 
                                                                  SHORT(Maximum), 
                                                                  WinDef.False);
  Result                               := WinUser.SetScrollPos   (hWnd, 
                                                                  WinUser.SB_VERT, 
                                                                  lpScreenMetric^.NumberOfFirstLine, 
                                                                  WinDef.True);

  lpScreenMetric^.ColumnsOnScreen      := cx DIV cxChar;
  Maximum                              := lpScreenMetric^.NumberOfColumns - lpScreenMetric^.ColumnsOnScreen;
  IF Maximum<0 THEN
    Maximum := 0;
  END;
  Result                               := WinUser.SetScrollRange (hWnd, 
                                                                  WinUser.SB_HORZ, 
                                                                  0, 
                                                                  SHORT(Maximum), 
                                                                  WinDef.False);
  Result                               := WinUser.SetScrollPos   (hWnd, 
                                                                  WinUser.SB_HORZ,
                                                                  lpScreenMetric^.NumberOfFirstColumn, 
                                                                  WinDef.True);

  Result                               := WinUser.ReleaseDC(hWnd, hDC);
  RETURN Result;

END UseNewSize;


(*****************************************************************************)
(*                                                                           *)
(* SetMenu                                                                   *)
(* sets the menus                                                            *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  Mode       refer to Global Display Modus                                 *)
(*             0       first time                                            *)
(*             1       Mode HexData                                          *)
(*             2       Mode DebugData                                        *)
(*             3       Mode SymbolTable                                      *)
(*             4       Mode LineNumbers                                      *)
(*             5       Mode FileLayout                                       *)
(*             6       Mode DataDirectories                                  *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  SetMenu*                   (hWnd:               WinDef.HWND;    (* Window handle          *)
                                       Mode:               INTEGER)        
                                      : WinDef.LRESULT;

VAR
  Done:                                BOOLEAN;
  hMenu:                               WinDef.HMENU;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  ResulthGDIobj:                       WinDef.HGDIOBJ;
  i, j:                                LONGINT;

BEGIN;

  hMenu        := WinUser.GetMenu(hWnd);
  
  CASE Mode OF
    0:
      (* Gray out all items that are not implemented *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_DISABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Stop,   WinUser.MF_GRAYED);
    
      (* Gray out all items that are not useful without having a file opened *)
      FOR i:=301 TO 399 DO
        ResultBool   := WinUser.EnableMenuItem(hMenu, i, WinUser.MF_GRAYED);
      END (* FOR i:=300 TO 399 *);
    | (* 0 *)
    Global.IDM_View_HexData, Global.IDM_View_Dump, Global.IDM_View_SymbolTable, Global.IDM_View_SymbolTableGlobal, Global.IDM_View_LineNumbers:
      IF Global.PMyNT_Header#NIL THEN
        ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_Dump, WinUser.MF_ENABLED);
        ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_SectionHeaders, WinUser.MF_ENABLED);
        IF Global.FirstDebugDirectory#NIL THEN
          ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_SymbolTable,       WinUser.MF_ENABLED);
          ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_SymbolTableGlobal, WinUser.MF_ENABLED);
          ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_LineNumbers,       WinUser.MF_ENABLED);
        END (* IF Global.FirstDebugDirectory#NIL *);
        FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 DO
          IF Global.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress>0 THEN
            ResultBool   := WinUser.EnableMenuItem(hMenu, (Global.IDM_View_DD_Export+i), WinUser.MF_ENABLED);
          END (* IF Global.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress>0 *);
        END (* FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 *);
      END (* IF Global.PMyNT_Header#NIL *);
      IF Global.MyFileDescription.FileType=Global.FileTypeLIB THEN
        ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_Dump, WinUser.MF_ENABLED);
      END;
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_View_HexData, WinUser.MF_ENABLED);
      CASE Mode OF
        Global.IDM_View_HexData:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_HexData, WinUser.MF_CHECKED);
        |
        Global.IDM_View_Dump:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_Dump, WinUser.MF_CHECKED);
        |
        Global.IDM_View_SymbolTable:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_SymbolTable, WinUser.MF_CHECKED);
        |
        Global.IDM_View_SymbolTableGlobal:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_SymbolTableGlobal, WinUser.MF_CHECKED);
        |
        Global.IDM_View_LineNumbers:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Global.IDM_View_LineNumbers, WinUser.MF_CHECKED);
        ELSE
          ;
      END (* CASE Mode *);
    | (* Global.ModeHexData, Global.DebugDataMode, Global.SymbolTableMode, Global.LineNumbersMode *)
    Global.IDM_File_Open:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Open,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Close,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Start,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Stop,   WinUser.MF_GRAYED);
    | (* Global.IDM_File_Open *)
    Global.IDM_File_Close:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Open,   WinUser.MF_ENABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Start,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Stop,   WinUser.MF_GRAYED);
    | (* Global.IDM_File_Close *)
    Global.IDM_File_Start:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Open,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Start,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Stop,   WinUser.MF_ENABLED);
    | (* Global.IDM_File_Start *)
    Global.IDM_File_Attach:
      ;
    | (* Global.IDM_File_Attach *)
    Global.IDM_File_Stop:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Open,   WinUser.MF_ENABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Start,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Global.IDM_File_Stop,   WinUser.MF_GRAYED);
    (* Global.IDM_File_Stop *)
    ELSE
      ;
  END (* CASE Mode *);
  RETURN 0;

END SetMenu;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  NumberOfLines                :=  32;
  NumberOfColumns              :=  50;
  
  HeadLines[1].Text            := "Überschrift 1";
  HeadLines[1].Format          := Global.EZ_ATTR_BOLD;
  HeadLines[1].Next            := NIL;
  HeadLines[1].Previous        := NIL;
  HeadLines[2].Text            := "Überschrift 2";
  HeadLines[2].Format          := Global.Header2;
  HeadLines[2].Next            := NIL;
  HeadLines[2].Previous        := NIL;
  HeadLines[3].Text            := "Überschrift 3";
  HeadLines[3].Format          := Global.Header3;
  HeadLines[3].Next            := NIL;
  HeadLines[3].Previous        := NIL;
  
  MyFonts[Global.Text01].Font  := EZCreateFont (hDC, 
                                                   "Lucida Console", 
                                                    12, 
                                                     0, 
                                                     0, 
                                                    TRUE);
  MyFonts[Global.Text01].Height  := 1.25;
  
  MyFonts[Global.Text02].Font  := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                0, 
                                                TRUE);
  MyFonts[Global.Text02].Height  := 1.5;

  MyFonts[Global.Text03].Font  := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12,
                                                0, 
                                                Global.EZ_ATTR_ITALIC, 
                                                TRUE);
  MyFonts[Global.Text03].Height  := 1.5;

  MyFonts[Global.Header1].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                14, 
                                                0, 
                                                Global.EZ_ATTR_BOLD + Global.EZ_ATTR_UNDERLINE, 
                                                TRUE);
  MyFonts[Global.Header1].Height := 1.5;

  MyFonts[Global.Header2].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                0, 
                                                TRUE);
  MyFonts[Global.Header2].Height := 1.5;

  MyFonts[Global.Header3].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                Global.EZ_ATTR_BOLD, 
                                                TRUE);
  MyFonts[Global.Header3].Height := 2;

END View.

