(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     O2Debug                                     V 1.42.15         *)
(*                                                         2002APR14         *)
(*  PURPOSE:   supports the debugging of Oberon-2 programs                   *)
(*             linenumbers and address mapping                               *)
(*             symbol table and address mapping                              *)
(*             event logging (not yet implemnted)                            *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   MainWndProc()                                                           *)  
(*             processes messages                                            *)
(*   DebugWndProg()                                                          *)
(*             processes messages concerning the debug client window         *)
(*   DumpWndProc()                                                           *)  
(*             processes messages concerning the dump client window          *)
(*   WinMain() calls initialization function, processes message loop         *)
(*                                                                           *)
(*  INPUT:                                                                   *)
(*                                                                           *)
(*  OUTPUT:                                                                  *)
(*                                                                           *)
(* COMMENTS:   informations concerning the project                           *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000SEP15                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2001AUG01 First Changes                                                 *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE O2Debug;


IMPORT 
  CommCTRL, WinBase, WinDef, WinGDI, WinUser,
  Param, Strings, SYSTEM,
  StatusLine,
  Administration, Debug, Dump_CodeView, Dump, File, Help, View, Global;

CONST
  Version*             =              "V 1.42.15";
  Module*              =              "O2Debug";

VAR    
  hInstance:                           WinDef.HANDLE;
  CreateStruct:                        WinUser.CREATESTRUCTA;
  ClientCreateStruct:                  WinUser.CLIENTCREATESTRUCT;
  
  i:                                   INTEGER;
  MyRect:                              WinDef.RECT;
  Number:                              ARRAY  32 OF CHAR;
  Title,
  Parameter:                           ARRAY 256 OF CHAR;
  ResultBool:                          WinDef.BOOL;
  StatusBarRegion:                     ARRAY 4 OF LONGINT;
  Width:                               LONGINT;


(*****************************************************************************)
(*                                                                           *)
(* MainWndProc                                                               *)
(* Processes messages                                                        *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  W.WM_COMMAND                                                             *)
(*             application menu (About dialog box)                           *)
(*  W.WM_DESTROY                                                             *)
(*             Window is to be destroyed                                     *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  To process the IDM_ABOUT message, call MakeProcInstance() to get the     *)
(*  current instance address of the About() function.  Then call Dialog      *)
(*  box which will create the box according to the information in your       *)
(*  generic.rc file and turn control over to the About() function.  When     *)
(*  it returns, free the instance address.                                   *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] MainWndProc*     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                     : WinDef.LRESULT;

VAR
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  hdc:                                 WinDef.HDC;
  hChild:                              WinDef.HWND;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  WinRect:                             WinDef.RECT;
  Breite:                              LONGINT;
  i:                                   INTEGER;
  cx, cy, cyStatus:                    INTEGER;

BEGIN
  
  CASE message OF
    
    WinUser.WM_CREATE:
      (* Initialize MDI Client Window; first: fill mdi client info structure *)
      ClientCreateStruct.idFirstChild  := Global.IDM_Window_First_Child;
      ClientCreateStruct.hWindowMenu   := WinUser.GetSubMenu (WinUser.GetMenu (hWnd),
                                                      WinUser.GetMenuItemCount (WinUser.GetMenu (hWnd)) - 3);
      Global.hWndMDIClient := WinUser.CreateWindowExA(WinUser.WS_EX_CLIENTEDGE,
                                   SYSTEM.ADR("MDICLIENT"),
                                   WinDef.NULL,            (* Text for Window title bar.         *)
                                   WinUser.WS_CHILD +      (* Window style.                      *)
                                   WinUser.WS_CLIPCHILDREN + WinUser.WS_VISIBLE,
                                   0,                      (* Default horizontal position.       *)
                                   0,                      (* Default vertical position.         *)
                                   0,                      (* Default width.                     *)
                                   0,                      (* Default height.                    *)
                                   hWnd,                   (* parent Window                      *)
                                   1,                      (* Use the Window class menu.         *)
                                   Global.hInstance,       (* This instance owns this Window.    *)
                                   SYSTEM.ADR(ClientCreateStruct));     (* MDI Client Structure. *)

      ASSERT(Global.hWndMDIClient#0);

      Result         := View.SetMenu(hWnd, 0);
  
      (* Initialize Statusbar *)
      StatusLine.Create(hWnd, Global.hInstance);
      RETURN 0;

    | (* WinUser.WM_CREATE *)
    WinUser.WM_SIZE:
      cx       := SYSTEM.LOWORD(lParam);
      cy       := SYSTEM.HIWORD(lParam);
      cyStatus := 0;
      (* Calculate the right edge coordinate for each part of the statusbar *)
      (* and copy the coordinates to the array.                             *)
      Width                := cx DIV 10;
      StatusBarRegion[0]   := 5*Width;
      StatusBarRegion[1]   := 7*Width;
      StatusBarRegion[2]   := 9*Width;
      StatusBarRegion[3]   := cx;
      (* Tell the status bar to create the window parts. CommCTRL.SB_SETPARTS *)
      Result   := WinUser.SendMessageA(StatusLine.hWndBar, CommCTRL.SB_SETPARTS, 4, SYSTEM.ADR(StatusBarRegion));
      Result   := WinUser.SendMessageA(StatusLine.hWndBar, WinUser.WM_SIZE, 0, 0);
      IF WinUser.IsWindowVisible(StatusLine.hWndBar)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(StatusLine.hWndBar, WinRect);
        cyStatus     := SHORT(WinRect.bottom - WinRect.top);
      ELSE
        cyStatus := 0;
      END (* IF WinUser.IsWindowVisible(Global.hWndStatusBar)=WinDef.True *);
      cy           := cy - cyStatus;
      ResultBool   := WinUser.MoveWindow(Global.hWndMDIClient, 0, 0, cx, cy, WinDef.True);
      RETURN 0
    | (* WinUser.WM_SIZE *)
    WinUser.WM_COMMAND:                                    (* command from application menu      *)
      CASE SYSTEM.LOWORD(wParam) OF
        Global.IDM_File_Open:                              (* Menu "File", Open a file to dump it *)
          IF File.Open (hWnd, message, wParam, lParam)=1 THEN
            ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Global.MainTitle));
            RETURN 0
          END;
          Result := File.MapFile();
          Result := Dump.DumpTheFile();
          Result := View.Switch2NewView(Global.hWndDump, Global.HexDataMode);
          Title  := Global.MainTitle;
          Strings.Append(Title, " - [");
          Strings.Append(Title, Global.MyFileDescription.Path);
          Strings.AppendChar(Title, "]");
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Title));
          CASE Global.MyFileDescription.FileType OF
            Global.FileTypeUnknown:
              Result :=  View.SetMenu(hWnd, 0);
            ELSE
              Result := View.SetMenu(hWnd, Global.IDM_View_HexData);
          END;
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Close:                             (* Close a file that is dumped *)
          Result     := File.Close (hWnd, message, wParam, lParam);
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Global.MainTitle));
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Start:                             (* Start a file for debugging *)
          IF File.Start (hWnd, message, wParam, lParam)=1 THEN
            ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Global.MainTitle));
            RETURN 0
          END;
          Result := File.MapFile();
          Result := Dump.DumpTheFile();
          Result := View.Switch2NewView(Global.hWndDump, Global.DebugDataMode);
          Title  := Global.MainTitle;
          Strings.Append(Title, " - [");
          Strings.Append(Title, Global.MyFileDescription.Path);
          Strings.AppendChar(Title, "]");
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Title));
          CASE Global.MyFileDescription.FileType OF
            Global.FileTypeEXE:
              Result :=  View.SetMenu(hWnd, Global.IDM_View_Dump);
            ELSE
              Result := View.SetMenu(hWnd, 0);
          END;
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Attach:
          StatusLine.SetText ("File.Attach not yet implemented.", StatusLine.NoticeField);
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Suspend:
          Result     := Debug.Suspend();
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Resume:
          Result     := Debug.Resume(Global.DebugProcess);
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Stop:
          Result     := File.Stop (hWnd, message, wParam, lParam);
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Global.MainTitle));
          Result     := View.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Global.IDM_File_Print:
          StatusLine.SetText ("File.Print not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_File_Printer:
          StatusLine.SetText ("File.Printer not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_File_Exit:
          WinUser.PostQuitMessage(0);
          RETURN 0
        |
        Global.IDM_Edit_Search:                                (* Menu Edit *)
          StatusLine.SetText ("Edit.Search not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_Edit_Replace:
          StatusLine.SetText ("Edit.Replace not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_Edit_Copy:
          StatusLine.SetText ("Edit.Copy not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_Edit_Cut:
          StatusLine.SetText ("Edit.Cut not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_Edit_Paste:
          StatusLine.SetText ("Edit.Paste not yet implemented.", StatusLine.NoticeField);
          RETURN 0
        |
        Global.IDM_View_HexData:                               (* Menu View *)
          Result := View.Switch2NewView (Global.hWndDump, Global.HexDataMode);
          RETURN 0
        |
        Global.IDM_View_Dump:
          Result := View.Switch2NewView (Global.hWndDump, Global.DebugDataMode);
          RETURN 0
        |
        Global.IDM_View_SectionHeaders:
          Result := View.Switch2NewView (Global.hWndDump, Global.SectionHeadersMode);
          RETURN 0
        |
        Global.IDM_View_SymbolTable:
          Result := View.Switch2NewView (Global.hWndDump, Global.SymbolTableMode);
          RETURN 0
        |
        Global.IDM_View_LineNumbers:
          Result := View.Switch2NewView (Global.hWndDump, Global.LineNumbersMode);
          RETURN 0
        |
        Global.IDM_View_SymbolTableGlobal:
          Result := View.Switch2NewView (Global.hWndDump, Global.SymbolTableGlobalMode);
          RETURN 0
        |
        Global.IDM_View_DD_Export:                             (* Submenu Data Directories *)
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_ExportMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Import:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_ImportMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Resource:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_ResourceMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Exception:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_ExceptionMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Security:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_SecurityMode);
          RETURN 0;
        |
        Global.IDM_View_DD_BaseRelocation:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_BaseRelocationMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Debug:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_DebugMode);
          RETURN 0;
        |
        Global.IDM_View_DD_Copyright:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_CopyrightMode);
          RETURN 0;
        |
        Global.IDM_View_DD_GlobalPtr:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_GlobalPtrMode);
          RETURN 0;
        |
        Global.IDM_View_DD_TLS:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_TLSMode);
          RETURN 0;
        |
        Global.IDM_View_DD_LoadConfig:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_LoadConfigMode);
          RETURN 0;
        |
        Global.IDM_View_DD_BoundImport:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_BoundImportMode);
          RETURN 0;
        |
        Global.IDM_View_DD_IAT:
          Result := View.Switch2NewView (Global.hWndDump, Global.DD_IATMode);
          RETURN 0;
        |
        Global.IDM_Window_Tile_hor:                        (* Menu Window *)
          Result  := WinUser.SendMessageA(Global.hWndMDIClient, WinUser.WM_MDITILE, WinUser.MDITILE_HORIZONTAL, 0);
          RETURN 0;
        |
        Global.IDM_Window_Tile_ver:
          Result  := WinUser.SendMessageA(Global.hWndMDIClient, WinUser.WM_MDITILE, WinUser.MDITILE_VERTICAL, 0);
          RETURN 0;
        |
        Global.IDM_Window_Cascade:
          Result  := WinUser.SendMessageA(Global.hWndMDIClient, WinUser.WM_MDICASCADE, 0, 0);
          RETURN 0;
        |
        Global.IDM_Window_Arrange:
          Result  := WinUser.SendMessageA(Global.hWndMDIClient, WinUser.WM_MDIICONARRANGE, 0, 0);
          RETURN 0;
        |
        Global.IDM_Admin_Font:                             (* Menu Administration *)
          Result := Administration.AdminFont (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Admin_Zero:
          Result := Administration.Zero (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Admin_First:
          Result := Administration.First (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Admin_Second:
          Result := Administration.Second (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Admin_Third:
          Result := Administration.Third (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Admin_Fourth:
          Result := Administration.Fourth (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Global.IDM_Help_About:                                 (* Menü Hilfe *)
          Result := WinUser.DialogBoxParamA   (hInstance,         (* current instance         *)
                           SYSTEM.ADR(Global.IDD_HelpAbout),   (* resource to use          *)
                           hWnd,                               (* parent handle            *)
                           Help.About,0);                      (* About() instance address *)
                           
          RETURN 0;
        ELSE                                                   (* Lets Windows process it    *)
          RETURN WinUser.DefFrameProcA(hWnd, Global.hWndMDIClient, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *)
    | (* WinUser.WM_COMMAND *)
    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      WinUser.PostQuitMessage(0);
      RETURN 1;
    (* WinUser.WM_DESTROY *)
    ELSE                                                   (* Passes it on if unproccessed       *)
      RETURN WinUser.DefFrameProcA(hWnd, Global.hWndMDIClient, message, wParam, lParam)
  END (* CASE message OF *);

  RETURN WinDef.NULL

END MainWndProc;


(*****************************************************************************)
(*                                                                           *)
(* DebugWndProc                                                              *)
(* Processes messages                                                        *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  W.WM_COMMAND                                                             *)
(*             application menu (About dialog box)                           *)
(*  W.WM_DESTROY                                                             *)
(*             Window is to be destroyed                                     *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] DebugWndProc*    (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                     : WinDef.LRESULT;

VAR
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  lpScreenMetric:                      Global.ScreenMetricP;
  hdc:                                 WinDef.HDC;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  WinRect:                             WinDef.RECT;
  i:                                   LONGINT;
  cx, cy, cyStatus:                    INTEGER;
  VertPos:                             LONGINT;
  TextMetric:                          WinGDI.TEXTMETRICA;
  hDC:                                 WinDef.HDC;

BEGIN

  CASE message OF
    
    WinUser.WM_CREATE:                                     (* message: Window will be created  *)
      NEW(lpScreenMetric);
      lpScreenMetric^.Formatted            := FALSE;
      lpScreenMetric^.LinesOnScreen        :=  0;
      lpScreenMetric^.NumberOfLines        :=  0;
      lpScreenMetric^.NumberOfFirstLine    :=  0;
      lpScreenMetric^.ColumnsOnScreen      :=  0;
      lpScreenMetric^.NumberOfColumns      :=  0;
      lpScreenMetric^.NumberOfFirstColumn  :=  0;
      NEW(lpScreenMetric^.FirstLine);
      lpScreenMetric^.FirstLine^.Previous  := NIL;
      lpScreenMetric^.FirstLine^.Next      := NIL;
      lpScreenMetric^.FirstLineOnScreen    := lpScreenMetric^.FirstLine;
      lpScreenMetric^.HeadLines            := NIL;
      Result                               := WinUser.SetWindowLongA(hWnd, 
                                                                     Global.WXBScreenMetricP, 
                                                                     SYSTEM.VAL(LONGINT, lpScreenMetric));
      Global.DebugProcess^.hWnd            := hWnd;
      Result                               := Debug.Start(Global.DebugProcess);
      IF Result=0 THEN
        Result := WinUser.SendMessageA(Global.hWndMain, WinUser.WM_COMMAND, Global.IDM_File_Stop, 0);
        StatusLine.SetText("Debug Process ID = 0 !!", StatusLine.NoticeField);
      END;
      RETURN 0;
    | (* WinUser.WM_CREATE *)
  
    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                                   WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
      DISPOSE(lpScreenMetric);
      RETURN 1;
    | (* WinUser.WM_DESTROY *)
    WinUser.WM_PAINT:
      Result := View.Update(hWnd);
      RETURN 0;
    | (* WinUser.WM_PAINT *)
    WinUser.WM_SIZE:
      ResultBool   := WinUser.GetWindowRect(Global.hWndMain, WinRect);
      Result       := View.UseNewSize(hWnd, lParam);
      cx           := SHORT(WinRect.right  - WinRect.left) - 10;
      cy           := SHORT(WinRect.bottom - WinRect.top)  - 50;
      IF WinUser.IsWindowVisible(StatusLine.hWndBar)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(StatusLine.hWndBar, WinRect);
        cy           := cy - SHORT(WinRect.bottom - WinRect.top);
      END (* IF WinUser.IsWindowVisible(Global.hWndStatusBar)=WinDef.True *);
      ResultBool   := WinUser.MoveWindow(Global.hWndMDIClient, 0, 0, cx, cy, WinDef.True);
      RETURN 0
    | (* WinUser.WM_SIZE *)
    WinUser.WM_KEYDOWN:                                    (* Anwender hat Taste gedrückt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.VK_UP:
          Result := View.UpAndDown(hWnd, Global.LineUp);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_DOWN:
          Result := View.UpAndDown(hWnd, Global.LineDown);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_HOME:
          Result := View.UpAndDown(hWnd, Global.Home);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_END:
          Result := View.UpAndDown(hWnd, Global.End);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_LEFT:
          Result := View.LeftAndRight(hWnd, Global.Left);
          RETURN 0
        | (* WinUser.VK_LEFT *)
        WinUser.VK_RIGHT:
          Result := View.LeftAndRight(hWnd, Global.Right);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_KEYDOWN *)
    WinUser.WM_VSCROLL:                                    (* Anwender hat vert. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_TOP:
          Result := View.UpAndDown(hWnd, Global.Home);
          RETURN 0;
        | (* WinUser.SB_TOP *)
        WinUser.SB_BOTTOM:
          Result := View.UpAndDown(hWnd, Global.End);
          RETURN 0;
        | (* WinUser.SB_BOTTOM *)
        WinUser.SB_PAGEUP:
          Result := View.UpAndDown(hWnd, Global.PageUp);
          RETURN 0;
        | (* WinUser.SB_PAGEUP *)
        WinUser.SB_PAGEDOWN:
          Result := View.UpAndDown(hWnd, Global.PageDown);
          RETURN 0;
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_LINEUP:
          Result := View.UpAndDown(hWnd, Global.LineUp);
          RETURN 0;
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.UpAndDown(hWnd, Global.LineDown);
          RETURN 0;
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          StatusLine.SetText("Debug WND; Vert. Scrollbar THUMBPOSITION.", StatusLine.NoticeField);
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          StatusLine.SetText("Debug WND; Vert. Scrollbar THUMBTRACK.", StatusLine.NoticeField);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_VSCROLL *)
    WinUser.WM_HSCROLL:                                    (* Anwender hat horiz. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_LINEUP:
          Result := View.LeftAndRight(hWnd, Global.Left);
          RETURN 0
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.LeftAndRight(hWnd, Global.Right);
          RETURN 0
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          StatusLine.SetText("Debug WND; Horiz. Scrollbar THUMBPOSITION.", StatusLine.NoticeField);
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          StatusLine.SetText("Debug WND; Horz. Scrollbar THUMBTRACK.", StatusLine.NoticeField);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    ELSE                                                   (* Passes it on if unproccessed       *)
      RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam)
  END (* CASE message OF *);

  RETURN WinDef.NULL

END DebugWndProc;


(*****************************************************************************)
(*                                                                           *)
(* DumpWndProc                                                               *)
(* Processes messages                                                        *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  W.WM_COMMAND                                                             *)
(*             application menu (About dialog box)                           *)
(*  W.WM_DESTROY                                                             *)
(*             Window is to be destroyed                                     *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] DumpWndProc*     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                     : WinDef.LRESULT;

VAR
  ActLine:                             Global.ScreenLineP;
  Done:                                BOOLEAN;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  hBrush:                              WinDef.HBRUSH;
  hDC:                                 WinDef.HDC;
  hPenDot:                             WinDef.HPEN;
  lpScreenMetric:                      Global.ScreenMetricP;
  MyCursor:                            WinDef.HCURSOR;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  Position:                            WinDef.POINT;
  WinRect:                             WinDef.RECT;
  i:                                   LONGINT;
  cx, cy, cyStatus:                    INTEGER;

BEGIN

  CASE message OF
    
    WinUser.WM_CREATE:                                     (* message: Window to be created    *)
      NEW(lpScreenMetric);
      lpScreenMetric^.Formatted            := TRUE;
      lpScreenMetric^.LinesOnScreen        :=  0;
      lpScreenMetric^.NumberOfLines        :=  0;
      lpScreenMetric^.NumberOfFirstLine    :=  0;
      lpScreenMetric^.ColumnsOnScreen      :=  0;
      lpScreenMetric^.NumberOfColumns      :=  0;
      lpScreenMetric^.NumberOfFirstColumn  :=  0;
      NEW(lpScreenMetric^.FirstLine);
      lpScreenMetric^.FirstLine.Previous   := NIL;
      lpScreenMetric^.FirstLine.Next       := NIL;
      lpScreenMetric^.FirstLineOnScreen    := lpScreenMetric^.FirstLine;
      NEW(lpScreenMetric^.HeadLines);
      lpScreenMetric^.HeadLines^.Previous  := NIL;
      lpScreenMetric^.HeadLines^.Next      := NIL;
      Result                               := WinUser.SetWindowLongA(hWnd, Global.WXBScreenMetricP, 
                                                                           SYSTEM.VAL(LONGINT, lpScreenMetric));
      Global.DisplayMode                   := Global.HexDataMode;
      RETURN 0;
    | (* WinUser.WM_CREATE *)
    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                                   WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
      DISPOSE(lpScreenMetric);
      Global.DisplayMode                   := Global.Invalid;
      RETURN 1;
    | (* WinUser.WM_DESTROY *)
    WinUser.WM_PAINT:
      Result := View.Update(hWnd);
      RETURN 0;
    | (* WinUser.WM_PAINT *)
    WinUser.WM_SIZE:
      ResultBool   := WinUser.GetWindowRect(Global.hWndMain, WinRect);
      Result       := View.UseNewSize(hWnd, lParam);
      cx           := SHORT(WinRect.right  - WinRect.left) - 10;
      cy           := SHORT(WinRect.bottom - WinRect.top)  - 50;
      IF WinUser.IsWindowVisible(StatusLine.hWndBar)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(StatusLine.hWndBar, WinRect);
        cy           := cy - SHORT(WinRect.bottom - WinRect.top);
      END (* IF WinUser.IsWindowVisible(Global.hWndStatusBar)=WinDef.True *);
      ResultBool   := WinUser.MoveWindow(Global.hWndMDIClient, 0, 0, cx, cy, WinDef.True);
      RETURN 0
    | (* WinUser.WM_SIZE *)
    WinUser.WM_KEYDOWN:                                    (* Anwender hat Taste gedrückt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.VK_UP:
          Result := View.UpAndDown(hWnd, Global.LineUp);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_DOWN:
          Result := View.UpAndDown(hWnd, Global.LineDown);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_HOME:
          Result := View.UpAndDown(hWnd, Global.Home);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_END:
          Result := View.UpAndDown(hWnd, Global.End);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_LEFT:
          Result := View.LeftAndRight(hWnd, Global.Left);
          RETURN 0
        | (* WinUser.VK_LEFT *)
        WinUser.VK_RIGHT:
          Result := View.LeftAndRight(hWnd, Global.Right);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_KEYDOWN *)
    WinUser.WM_VSCROLL:                                    (* Anwender hat vert. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_TOP:
          Result := View.UpAndDown(hWnd, Global.Home);
          RETURN 0;
        | (* WinUser.SB_TOP *)
        WinUser.SB_BOTTOM:
          Result := View.UpAndDown(hWnd, Global.End);
          RETURN 0;
        | (* WinUser.SB_BOTTOM *)
        WinUser.SB_PAGEUP:
          Result := View.UpAndDown(hWnd, Global.PageUp);
          RETURN 0;
        | (* WinUser.SB_PAGEUP *)
        WinUser.SB_PAGEDOWN:
          Result := View.UpAndDown(hWnd, Global.PageDown);
          RETURN 0;
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_LINEUP:
          Result := View.UpAndDown(hWnd, Global.LineUp);
          RETURN 0;
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.UpAndDown(hWnd, Global.LineDown);
          RETURN 0;
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          Result := View.ThumbPosition(hWnd, SYSTEM.HIWORD(wParam));
          StatusLine.SetText("Dump WND; Vert. Scrollbar THUMBPOSITION.", StatusLine.NoticeField);
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          Result := View.ThumbTrack(hWnd, SYSTEM.HIWORD(wParam));
          StatusLine.SetText("Dump WND; Vert. Scrollbar THUMBTRACK.", StatusLine.NoticeField);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_VSCROLL *)
    WinUser.WM_HSCROLL:                                    (* Anwender hat horiz. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_LINEUP:
          Result := View.LeftAndRight(hWnd, Global.Left);
          RETURN 0
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.LeftAndRight(hWnd, Global.Right);
          RETURN 0
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          StatusLine.SetText("Dump WND; Horiz. Scrollbar THUMBPOSITION.", StatusLine.NoticeField);
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          StatusLine.SetText("Dump WND; Horz. Scrollbar THUMBTRACK.", StatusLine.NoticeField);
          RETURN 0
        ELSE                                               (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_HSCROLL *)
    WinUser.WM_MOUSEMOVE:                                  (* mouse has ben moved *)
      Position.x   := SYSTEM.LOWORD(lParam);
      Position.y   := SYSTEM.HIWORD(lParam); 
      IF Global.DisplayMode=Global.DD_DebugMode THEN
        lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
        ActLine        := lpScreenMetric^.FirstLineOnScreen;
        Done           := FALSE;
        i              :=  0;
        MyCursor       := WinUser.SetCursor(WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW));
        REPEAT
          IF ((Position.y<ActLine^.Area.top) & (Position.y>ActLine^.Area.bottom)) THEN
            IF ((MyRect.left=ActLine^.Area.left)
               &(MyRect.right=ActLine^.Area.right) 
               &(MyRect.top=ActLine^.Area.top) 
               &(MyRect.bottom=ActLine^.Area.bottom)) THEN
              ;
            ELSE
              hDC        := WinUser.GetDC  (hWnd);
              hBrush     := WinGDI.CreateSolidBrush(123);
              ResultBool := WinUser.InvertRect(hDC, 
                                             MyRect);
              ResultBool := WinUser.InvertRect(hDC, 
                                             ActLine^.Area);
              Result     := WinUser.ReleaseDC(hWnd, hDC);
              ResultBool := WinUser.CopyRect(MyRect, ActLine^.Area);
            END (* IF WinUser.IsRectEmpty(WinRect)>0 *);
            MyCursor   := WinUser.SetCursor(WinUser.LoadCursorA(Global.hInstance, SYSTEM.ADR(Global.Cursor02)));
            Done       := TRUE;
          END (* IF ((Position.y<ActLine^.Area.top) & (Position.y>ActLine^.Area.bottom)) *);
          IF ActLine^.Next=NIL THEN
            Done     := TRUE;
          ELSE
            ActLine := ActLine^.Next;
            INC(i);
            IF i>lpScreenMetric^.LinesOnScreen THEN
              Done     := TRUE;
            END;
          END (* IF ... *);
        UNTIL Done;
      END (* IF Global.DisplayMode=Global.DD_DebugMode *);
    | (* WinUser.WM_MOUSEMOVE *)
    WinUser.WM_LBUTTONDOWN:                                (* mouse left button clicked *)
      Position.x   := SYSTEM.LOWORD(lParam);
      Position.y   := SYSTEM.HIWORD(lParam); 
      IF Global.DisplayMode=Global.DD_DebugMode THEN
        StatusLine.SetText2("Pos.x: #; Pos.y: #", Position.x, Position.y, 1);
        lpScreenMetric := SYSTEM.VAL(Global.ScreenMetricP, WinUser.GetWindowLongA(hWnd, Global.WXBScreenMetricP));
        ActLine        := lpScreenMetric^.FirstLineOnScreen;
        Done           := FALSE;
        i              :=  0;
        MyCursor       := WinUser.SetCursor(WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW));
        REPEAT
          IF ((Position.y<ActLine^.Area.top) & (Position.y>ActLine^.Area.bottom)) THEN
            MyCursor   := WinUser.SetCursor(WinUser.LoadCursorA(Global.hInstance, SYSTEM.ADR(Global.Cursor02)));
            StatusLine.SetText1("Dir Entry: #", ActLine^.Type, 2);
            hDC        := WinUser.GetDC  (hWnd);
            hBrush     := WinGDI.CreateSolidBrush(123);
            ResultBool := WinUser.FillRect (hDC, ActLine^.Area, hBrush);
            Result     := WinUser.ReleaseDC(hWnd, hDC);
(*            Result     := Dump_CodeView.CVsstModule (hWnd, message, wParam, lParam, ActLine^.Type);*)
            Done       := TRUE;
          END;
          IF ActLine^.Next=NIL THEN
            Done     := TRUE;
          ELSE
            ActLine := ActLine^.Next;
            INC(i);
            IF i>lpScreenMetric^.NumberOfLines THEN
              Done     := TRUE;
            END;
          END (* IF ... *);
        UNTIL Done;
      END (* IF Global.DisplayMode=Global.DD_DebugMode *);
     (* WinUser.MK_LBUTTON *)
    ELSE                                                   (* Passes it on if unproccessed       *)
      RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam)
  END (* CASE message OF *);

  RETURN WinDef.NULL

END DumpWndProc;


(*****************************************************************************)
(*                                                                           *)
(* WinMain                                                                   *)
(* initialization, processes message loop, close                             *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  Windows recognizes this function by name as the initial entry point      *)
(*  for the program.  This function calls the application initialization     *)
(*  routine, if no other instance of the program is running, and always      *)
(*  calls the instance initialization routine.  It then executes a message   *)
(*  retrieval and dispatch loop that is the top-level control structure      *)
(*  for the remainder of execution.  The loop is terminated when a WM_QUIT   *)
(*  message is received, at which time this function exits the application   *)
(*  instance by returning the value passed by PostQuitMessage().             *)
(*                                                                           *)
(*  If this function must abort before entering the message loop, it         *)
(*  returns the conventional value W.NULL.                                   *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] WinMain*         (hInstance:          WinDef.HANDLE;      (* current instance        *)
                                       lpCmdLine:          WinDef.LPSTR;       (* command line            *)
                                       nCmdShow:           LONGINT)            (* show-Window type: open/icon *)
                                      : WinDef.LRESULT;

VAR
  msg:                                 WinUser.MSG;        (* message *)
  WndClassEx:                          WinUser.WNDCLASSEX;
  Result:                              LONGINT;            
  ResultBool:                          WinDef.BOOL; 
  StatLineInstance:                    WinDef.HANDLE;

BEGIN
  
  Global.hInstance         := hInstance;
  Global.nCmdShow          := nCmdShow;
  (* Initialize Frame Window Class *)
  (* Fill in Window class structure with parameters describing the main Window. *)
  WndClassEx.cbSize        := SIZE(WinUser.WNDCLASSEX);
  WndClassEx.style         := WinDef.NULL;                 (* Class style(s).                    *)
  WndClassEx.lpfnWndProc   := MainWndProc;                 (* Function to retrieve messages for Windows of this class.                   *)
  WndClassEx.cbClsExtra    := 0;                           (* No per-class extra data.           *)
  WndClassEx.cbWndExtra    := 0;                           (* No per-Window extra data.          *)
  WndClassEx.hInstance     := hInstance;                   (* Application that owns the class.   *)
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, SYSTEM.ADR(Global.RES_Icon));
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := SYSTEM.ADR(Global.IDM_Main); (* Name of menu resource in .RC file*)
  WndClassEx.lpszClassName := SYSTEM.ADR(Global.MainClass);       (* Name used in call to CreateWindow*)
  WndClassEx.hIconSm       := WinUser.LoadIconA(hInstance, SYSTEM.ADR(Global.RES_Icon));

  (* Register the Window class *)
  Result := WinUser.RegisterClassExA(WndClassEx);
  ASSERT(Result#0);                                        (* If Window could not be created, stop program   *)

  (* Initialize MDI Debug Window *)
  (* Fill in window class structure with parameters describing the dump client window. *)
  WndClassEx.style         :=  0;                          (* Class style(s).                    *)
  WndClassEx.lpfnWndProc   := DebugWndProc;                (* Function to retrieve messages for Windows of this class.                   *)
  WndClassEx.cbClsExtra    :=  0;                          (* No per-class extra data.           *)
  WndClassEx.cbWndExtra    := 12;                          (* per-Window extra data, used to store pointers *)
  WndClassEx.hInstance     := hInstance;                   (* Application that owns the class.   *)
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, SYSTEM.ADR(Global.RES_Icon));
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := WinDef.NULL;                 (* Name of menu resource in .RC file  *)
  WndClassEx.lpszClassName := SYSTEM.ADR(Global.DebugClass);      (* Name used in call to CreateWindow  *)
  WndClassEx.hIconSm       := WinDef.NULL;

  (* Register the Window class *)
  Result := WinUser.RegisterClassExA(WndClassEx);           
  ASSERT(Result#0);


  (* Initialize MDI PECOFF Window *)
  (* Fill in window class structure with parameters describing the dump client window. *)
  WndClassEx.style         :=  0;                          (* Class style(s).                    *)
  WndClassEx.lpfnWndProc   := DumpWndProc;                 (* Function to retrieve messages for Windows of this class. *)
  WndClassEx.cbClsExtra    :=  0;                          (* No per-class extra data.           *)
  WndClassEx.cbWndExtra    := 12;                          (* per-Window extra data, used to store pointers *)
  WndClassEx.hInstance     := hInstance;                   (* Application that owns the class.   *)
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, SYSTEM.ADR(Global.RES_Icon));
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := WinDef.NULL;                 (* Name of menu resource in .RC file  *)
  WndClassEx.lpszClassName := SYSTEM.ADR(Global.DumpClass);       (* Name used in call to CreateWindow  *)
  WndClassEx.hIconSm       := WinDef.NULL;

  (* Register the Window class *)
  Result := WinUser.RegisterClassExA(WndClassEx);           
  ASSERT(Result#0);

  Global.hWndMain := WinUser.CreateWindowExA(WinUser.WS_EX_OVERLAPPEDWINDOW,
                           SYSTEM.ADR(Global.MainClass),   (* See RegisterClass() call.          *)
                           SYSTEM.ADR(Global.MainTitle),   (* Text for Window title bar.         *)
                                                           (* Window style.                      *)
                           WinUser.WS_OVERLAPPEDWINDOW + WinUser.WS_THICKFRAME + WinUser.WS_MAXIMIZEBOX + 
                           WinUser.WS_MINIMIZEBOX      + WinUser.WS_CLIPCHILDREN,
                           WinUser.CW_USEDEFAULT,          (* Default horizontal position.       *)
                           WinUser.CW_USEDEFAULT,          (* Default vertical position.         *)
                           WinUser.CW_USEDEFAULT,          (* Default width.                     *)
                           WinUser.CW_USEDEFAULT,          (* Default height.                    *)
                           WinDef.NULL,                    (* Overlapped Windows have no parent  *)
                           WinDef.NULL,                    (* Use the Window class menu.         *)
                           hInstance,                      (* This instance owns this Window.    *)
                           WinDef.NULL);                   (* Pointer not needed.                *)
                           
  Global.hWndMDIClient := WinUser.GetWindow(Global.hWndMain, WinUser.GW_CHILD);

  (* Make the Main Window visible and update its client area *)
  ResultBool     := WinUser.ShowWindow(Global.hWndMain, nCmdShow);(* Show the Window                      *)
  ResultBool     := WinUser.UpdateWindow(Global.hWndMain); (* Sends WM_PAINT message             *)
  
  (* Let's have a look at the command line to see whether there is a file name to work on    *)
  FOR i:=2 TO Param.Count() DO
    Param.Str(i, Parameter);
    IF ((Parameter[0]="-") OR (Parameter[0]="/")) THEN
      CASE Parameter[1] OF
        "D", "d":
          Global.DisplayMode := Global.DebugDataMode;
        |
        "G", "g":
          Global.DisplayMode := Global.SymbolTableGlobalMode;
        |
        "L", "l":
          Global.DisplayMode := Global.LineNumbersMode;
        |
        "S", "s":
          Global.DisplayMode := Global.SymbolTableMode;
        ELSE
          Global.DisplayMode := Global.HexDataMode;
      END (* CASE Parameter[1] *);
    ELSE
      COPY (Parameter, Global.MyFileDescription.Path);
      Result := File.MapFile();
      Result := Dump.DumpTheFile();
      Result := View.Switch2NewView(Global.hWndDump, Global.DisplayMode);
      Result := View.SetMenu(Global.hWndMain, Global.IDM_View_HexData);
      Title  := Global.MainTitle;
      Strings.Append(Title, " - [");
      Strings.Append(Title, Global.MyFileDescription.Path);
      Strings.AppendChar(Title, "]");
      ResultBool := WinUser.SetWindowTextA(Global.hWndMain, SYSTEM.ADR(Title));
      CASE Global.MyFileDescription.FileType OF
        Global.FileTypeUnknown:
          Result :=  View.SetMenu(Global.hWndMain, 0);
        ELSE
          Result := View.SetMenu(Global.hWndMain, Global.IDM_View_HexData);
      END;
      Result := View.SetMenu(Global.hWndMain, Global.IDM_File_Open);
    END;
  END;

  (* Acquire and dispatch messages until a WM_QUIT message is received.                          *)
  WHILE WinUser.GetMessageA(msg,                           (* message structure                  *)
                     WinDef.NULL,                          (* handle of Window receiving the ms  *)
                     WinDef.NULL,                          (* lowest message to examine          *)
                     WinDef.NULL)#0 DO                     (* highest message to examine         *)
  
    IF WinUser.TranslateMDISysAccel(Global.hWndMDIClient, msg)#WinDef.True THEN
      Result := WinUser.TranslateMessage(msg);             (* Translates virtual key codes *)
      Result := WinUser.DispatchMessageA(msg)              (* Dispatches message to Window *)
    END (* IF TranslateMDISysAccel(Global.hWndMDIClient, msg)#WinDef.True *);
  END (* WHILE WinUser.GetMessageA(msg, ...)#0 *)  ;
  
  (* Functions that should be used on closing... *)
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Global.MainClass),  hInstance);
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Global.DumpClass),  hInstance);
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Global.DebugClass), hInstance);
  
  RETURN WinDef.True

END WinMain;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                             Module Body                                   *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN;

  ResultBool := WinUser.SetRectEmpty(MyRect);
  
END O2Debug.

