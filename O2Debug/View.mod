(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     View                                        V 2.00.17         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   functions of menu point "View"                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Update    rewrites the screen                                           *)
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
  Common, Dump, DumpDataDir, DumpDataDirDebug, DumpSections, DumpHexData, 
  Resource, UIStatusLine,
  Strings, 
  WinBase, WinDef, WinGDI, WinUser, WinNT,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.17";
  Module*      =                      "View";
  ErrorNoOffset=                      Resource.IDM_View * 100;
  
  LineEmpty    =                      "                                                                 ";
  
  
TYPE
  LineFormat   = RECORD
    Font:                              WinDef.HFONT;
    Height:                            REAL;               (* Line Spacing *)
  END (* LineFormat *);
  

VAR
  hDC:                                 WinDef.HDC;
  hInst:                               WinDef.HANDLE;
  MyFonts:                             ARRAY 15 OF LineFormat;
  
  ReturnCode:                          LONGINT;
  i:                                   INTEGER;
  NumberOfColumns,
  NumberOfLines:                       LONGINT;
  ActLine,
  MyLine:                              Common.ScreenLineP;
  HeadLines:                           ARRAY 4 OF Common.ScreenLineT;
  
  WriteLines:                          Common.WriteLinesT;


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
  IF SYSTEM.BITAND(Attributes, Common.EZ_ATTR_BOLD)=Common.EZ_ATTR_BOLD THEN
    LogFont.lfWeight           := 700;
  ELSE
    LogFont.lfWeight           :=   0;
  END;
  IF SYSTEM.BITAND(Attributes, Common.EZ_ATTR_ITALIC)=Common.EZ_ATTR_ITALIC THEN
    LogFont.lfItalic           :=  1X;
  ELSE
    LogFont.lfItalic           :=  0X;
  END;
  IF SYSTEM.BITAND(Attributes, Common.EZ_ATTR_UNDERLINE)=Common.EZ_ATTR_UNDERLINE THEN
    LogFont.lfUnderline        :=  1X;
  ELSE
    LogFont.lfUnderline        :=  0X;
  END;
  IF SYSTEM.BITAND(Attributes, Common.EZ_ATTR_STRIKEOUT)=Common.EZ_ATTR_STRIKEOUT THEN
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
  MyLine:                              Common.ScreenLineP;
  Done:                                BOOLEAN;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  lpScreenMetric:                      Common.ScreenMetricP;
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
  
  lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
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

(*      
      IF hWnd=Common.hWndDump THEN
        (* set area for mouse actions *)
        ActLine^.Area.top      := yPos + TextMetric.tmHeight;
        ActLine^.Area.bottom   := yPos;
        ActLine^.Area.left     := xPos;
        ActLine^.Area.right    := xPos + ActLineLength*TextMetric.tmAveCharWidth;
      END  IF  hWnd=Common.hWndDump *);
      
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
(* append a line to the actual content of the screen                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*  Text       line to be added                                              *)
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
  MyLine:                              Common.ScreenLineP;
  hDC:                                 WinDef.HDC;
  lpScreenMetric:                      Common.ScreenMetricP;
  Maximum:                             LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  ResulthGDIobj:                       WinDef.HGDIOBJ;
  HeightOfLine:                        LONGINT;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  TextMetric:                          WinGDI.TEXTMETRICA;

BEGIN;

  lpScreenMetric       := SYSTEM.VAL(Common.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
  ASSERT(lpScreenMetric#NIL);
  
  IF Strings.Length(Text)>lpScreenMetric^.NumberOfColumns THEN
    lpScreenMetric^.NumberOfColumns := Strings.Length(Text);
  END (* IF ... *);
  
  hDC                  := WinUser.BeginPaint  (hWnd, PaintStructure);
  
  ResulthGDIobj        := WinGDI.SelectObject (hDC, MyFonts[Common.Text01].Font);
  ResulthGDIobj        := WinGDI.GetTextMetricsA (hDC, TextMetric);
  HeightOfLine         := SYSTEM.VAL(LONGINT, MyFonts[Common.Text01].Height * (TextMetric.tmHeight + TextMetric.tmExternalLeading));

  ActLine              := lpScreenMetric^.FirstLine;
  WHILE ActLine^.Next#NIL DO
    ActLine := ActLine^.Next
  END;
  NEW(ActLine^.Next);
  
  MyLine               := ActLine;
  ActLine              := ActLine^.Next;
  COPY(Text, ActLine^.Text);
  ActLine^.Format      := Common.Text01;
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
(*  hWnd       the window's handle                                           *)
(*  Mode       Common.Display Mode                                           *)
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
                                      :WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  hMenu:                               WinDef.HMENU;
  lpScreenMetric:                      Common.ScreenMetricP;
  i,
  Maximum,
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  hMenu        := WinUser.GetMenu(Common.hWndMain);
  
  FOR i:=Resource.IDM_View_HexData TO Resource.IDM_View_DD_TLS DO
    ResultBool   := WinUser.CheckMenuItem(hMenu, i, WinUser.MF_UNCHECKED);
  END (* FOR i:=Resource.IDM_View_HexData *);

  CASE Mode OF
    Common.HexDataMode:
      WriteLines   := DumpHexData.WriteLines;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_HexData, WinUser.MF_CHECKED);
    |
    Common.DebugDataMode:
      CASE Common.MyFileDescription.FileType OF
        Common.FileTypeLIB:
          WriteLines   := Dump.WriteLinesArchiveLIB;
        ELSE
          WriteLines   := Dump.WriteLines;
      END (* CASE *);
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_Dump, WinUser.MF_CHECKED);
    |
    Common.SectionHeadersMode:
      WriteLines   := DumpSections.WriteLines;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_SectionHeaders, WinUser.MF_CHECKED);
    |
    Common.SymbolTableMode:
      WriteLines   := DumpDataDirDebug.WriteLinesST;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_SymbolTable, WinUser.MF_CHECKED);
    |
    Common.SymbolTableGlobalMode:
      WriteLines   := DumpDataDirDebug.WriteLinesST2;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_SymbolTableGlobal, WinUser.MF_CHECKED);
    |
    Common.LineNumbersMode:
      WriteLines   := DumpDataDirDebug.WriteLinesLN;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_LineNumbers, WinUser.MF_CHECKED);
    |
    Common.DD_ExportMode:
      WriteLines   := DumpDataDir.WriteLines00;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Export, WinUser.MF_CHECKED);
    |
    Common.DD_ImportMode:
      WriteLines   := DumpDataDir.WriteLines01;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Import, WinUser.MF_CHECKED);
    |
    Common.DD_ResourceMode:
      WriteLines   := DumpDataDir.WriteLines02;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Resource, WinUser.MF_CHECKED);
    |
    Common.DD_ExceptionMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Export, WinUser.MF_CHECKED);
    |
    Common.DD_SecurityMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Security, WinUser.MF_CHECKED);
    |
    Common.DD_BaseRelocationMode:
      WriteLines   := DumpDataDir.WriteLines05;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_BaseRelocation, WinUser.MF_CHECKED);
    |
    Common.DD_DebugMode:
      WriteLines   := DumpDataDirDebug.WriteLines06;
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Debug, WinUser.MF_CHECKED);
    |
    Common.DD_CopyrightMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Copyright, WinUser.MF_CHECKED);
    |
    Common.DD_GlobalPtrMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_GlobalPtr, WinUser.MF_CHECKED);
    |
    Common.DD_TLSMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_Debug, WinUser.MF_CHECKED);
    |
    Common.DD_LoadConfigMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_LoadConfig, WinUser.MF_CHECKED);
    |
    Common.DD_BoundImportMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_BoundImport, WinUser.MF_CHECKED);
    |
    Common.DD_IATMode:
      ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_DD_IAT, WinUser.MF_CHECKED);
    ELSE
      RETURN 1
  END (* CASE Mode *);
  
  Common.DisplayMode   := Mode;

  (* set data for screen metric *)
  lpScreenMetric                       := SYSTEM.VAL(Common.ScreenMetricP, 
                                                     WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
  ASSERT(lpScreenMetric#NIL);
  lpScreenMetric^.NumberOfLines        := WriteLines(lpScreenMetric^.FirstLine, 
                                                     lpScreenMetric^.NumberOfColumns);

  IF lpScreenMetric^.Formatted THEN                        (* read headers if there are any *)
    ActLine    := lpScreenMetric^.FirstLine;
    FOR i:=1 TO 3 DO                                       (* reset header lines *)
      HeadLines[i].Text[0] := 0X;
      HeadLines[i].Format  := Common.Text01;
    END;
    Done       := FALSE;
    REPEAT
      CASE ActLine^.Format OF
        Common.Header1:
          IF HeadLines[1].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[1].Text);
            HeadLines[1].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        | (* Common.Header1 *)
        Common.Header2:
          IF HeadLines[2].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[2].Text);
            HeadLines[2].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        | (* Common.Header2 *)
        Common.Header3:
          IF HeadLines[3].Text[0]=0X THEN
            COPY(ActLine^.Text, HeadLines[3].Text);
            HeadLines[3].Format := ActLine^.Format;
          ELSE                                             (* we need only the first bunch of headers *)
            Done := TRUE;
          END;
        (* Common.Header3 *)
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

  ResultBool                           := WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
  ReturnCode                           := Update(hWnd);
  
  RETURN WinUser.SetFocus(hWnd);
    
END Switch2NewView;


(*****************************************************************************)
(*                                                                           *)
(* UpAndDown                                                                 *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*  Mode       Common.Move Screen Content                                    *)
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

PROCEDURE  UpAndDown*                 (hWnd:               WinDef.HWND;
                                       Mode:               INTEGER)
                                      :WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  lpScreenMetric:                      Common.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));

  CASE Mode OF
    Common.LineUp:
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
    Common.LineDown:
      IF (lpScreenMetric^.NumberOfFirstLine+lpScreenMetric^.LinesOnScreen)>lpScreenMetric^.NumberOfLines THEN
        RETURN 0;
      END;
      IF lpScreenMetric^.FirstLineOnScreen.Next=NIL THEN
        ;
      ELSE
        lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLineOnScreen.Next;
        IF lpScreenMetric^.Formatted THEN
          CASE lpScreenMetric^.FirstLineOnScreen^.Format OF
            Common.Header1:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[1].Text);
              HeadLines[1].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            | (* Common.Header1 *)
            Common.Header2:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[2].Text);
              HeadLines[2].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            | (* Common.Header2 *)
            Common.Header3:
              COPY(lpScreenMetric^.FirstLineOnScreen^.Text, HeadLines[3].Text);
              HeadLines[3].Format := lpScreenMetric^.FirstLineOnScreen^.Format;
            (* Common.Header3 *)
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
    Common.PageUp:
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
    Common.PageDown:
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
    Common.Home:
      lpScreenMetric^.FirstLineOnScreen := lpScreenMetric^.FirstLine;
      lpScreenMetric^.NumberOfFirstLine :=  0;
      Result            := WinUser.SetScrollPos(hWnd, WinUser.SB_VERT, 
                                                      SHORT(lpScreenMetric^.NumberOfFirstLine), 
                                                      WinDef.True);
      Result            := Update(hWnd);
      (* den neu zu zeichnenden Bereich markieren ! *)
      RETURN WinUser.InvalidateRect(hWnd, NIL, WinDef.True);
    |
    Common.End:
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
(*  hWnd       the window's handle                                           *)
(*  Position                                                                 *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ThumbPosition*              (hWnd:               WinDef.HWND;
                                       Position:           INTEGER)
                                      :WinDef.LRESULT;
                                      
VAR
  lpScreenMetric:                      Common.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));

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
(*  hWnd       the window's handle                                           *)
(*  Position                                                                 *)
(*                                                                           *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ThumbTrack*                 (hWnd:               WinDef.HWND;
                                       Position:           INTEGER)
                                      :WinDef.LRESULT;
                                      
VAR
  lpScreenMetric:                      Common.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;

BEGIN;

  lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                               WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));

  i := Position - lpScreenMetric^.NumberOfFirstLine;
  
  IF i=0 THEN
    RETURN 0;
  END;
  
  IF i<0 THEN
    Result := UpAndDown(hWnd, Common.LineUp);
    RETURN 0;
  END;
  
  IF i>0 THEN
    Result := UpAndDown(hWnd, Common.LineDown);
    RETURN 0;
  END;


END ThumbTrack;


(*****************************************************************************)
(*                                                                           *)
(* LeftAndRight                                                              *)
(* bewegt den Bildschirminhalt                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*  Mode       Common.Move Screen Content                                    *)
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

PROCEDURE LeftAndRight*               (hWnd:               WinDef.HWND;
                                       Mode:               INTEGER)
                                      :WinDef.LRESULT;
                                      
VAR
  Done:                                BOOLEAN;
  lpScreenMetric:                      Common.ScreenMetricP;
  i, j:                                LONGINT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  Zeile:                               ARRAY 128 OF CHAR;

BEGIN;

  lpScreenMetric       := SYSTEM.VAL(Common.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));

  CASE Mode OF
    Common.Left:
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
    Common.Right:
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

PROCEDURE UseNewSize*                 (hWnd:               WinDef.HWND;
                                       lParam:             WinDef.LPARAM)
                                      :WinDef.LRESULT;

VAR
  cx,
  cy:                                  LONGINT;
  cxChar,
  cyChar,
  Maximum:                             LONGINT;
  lpScreenMetric:                      Common.ScreenMetricP;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  TextMetric:                          WinGDI.TEXTMETRICA;
  hDC:                                 WinDef.HDC;

BEGIN;

  lpScreenMetric                       := SYSTEM.VAL(Common.ScreenMetricP, 
                                                     WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
                                                     
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
(*****************************************************************************)
BEGIN;

  NumberOfLines                :=  32;
  NumberOfColumns              :=  50;
  
  HeadLines[1].Text            := "Überschrift 1";
  HeadLines[1].Format          := Common.EZ_ATTR_BOLD;
  HeadLines[1].Next            := NIL;
  HeadLines[1].Previous        := NIL;
  HeadLines[2].Text            := "Überschrift 2";
  HeadLines[2].Format          := Common.Header2;
  HeadLines[2].Next            := NIL;
  HeadLines[2].Previous        := NIL;
  HeadLines[3].Text            := "Überschrift 3";
  HeadLines[3].Format          := Common.Header3;
  HeadLines[3].Next            := NIL;
  HeadLines[3].Previous        := NIL;
  
  MyFonts[Common.Text01].Font  := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                 0, 
                                                 0, 
                                                TRUE);
  MyFonts[Common.Text01].Height  := 1.2;
  
  MyFonts[Common.Text02].Font  := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                0, 
                                                TRUE);
  MyFonts[Common.Text02].Height  := 1.5;

  MyFonts[Common.Text03].Font  := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12,
                                                0, 
                                                Common.EZ_ATTR_ITALIC, 
                                                TRUE);
  MyFonts[Common.Text03].Height  := 1.5;

  MyFonts[Common.Header1].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                14, 
                                                0, 
                                                Common.EZ_ATTR_BOLD + Common.EZ_ATTR_UNDERLINE, 
                                                TRUE);
  MyFonts[Common.Header1].Height := 1.5;

  MyFonts[Common.Header2].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                0, 
                                                TRUE);
  MyFonts[Common.Header2].Height := 1.5;

  MyFonts[Common.Header3].Font := EZCreateFont (hDC, 
                                               "Lucida Console", 
                                                12, 
                                                0, 
                                                Common.EZ_ATTR_BOLD, 
                                                TRUE);
  MyFonts[Common.Header3].Height := 2;

END View.

