(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     O2Debug                                     V 2.00.54         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   supports the debugging of Oberon-2 programs                   *)
(*             linenumbers and address mapping                               *)
(*             symbol table and address mapping                              *)
(*             event logging                                                 *)
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
  Administration, Common, Debug, Dump, DumpCodeView, DumpDataDirDebug, 
  Edit, Files, Help, View, Options, Resource, UIStatusLine, UITabControl, UIToolBar, UserInterface,
  Param, Strings, 
  CommCTRL, WinBase, WinDef, WinGDI, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.54";
  Module*      =                      "O2Debug";
  

VAR    
  hInstance:                           WinDef.HANDLE;
  CreateStruct:                        WinUser.CREATESTRUCTA;
  ClientCreateStruct:                  WinUser.CLIENTCREATESTRUCT;
  
  i:                                   INTEGER;
  MyNMHeaderP:                         WinUser.LPNMHDR;
  MyRect:                              WinDef.RECT;
  Number,
  Number2:                             ARRAY  32 OF CHAR;
  Title,
  Parameter:                           ARRAY 256 OF CHAR;
  ResultBool:                          WinDef.BOOL;
  
  HitCounter:                          LONGINT;


(*****************************************************************************)
(*                                                                           *)
(* MainWndProc                                                               *)
(* Processes messages                                                        *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  WM_CREATE                                                                *)
(*  WM_SIZE                                                                  *)
(*  WM_COMMAND                                                               *)
(*             application menu (About dialog box)                           *)
(*  WM_DESTROY                                                               *)
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
  MyMDIClient:                         Common.MDIClientP;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  hdc:                                 WinDef.HDC;
  hChild:                              WinDef.HWND;
  msg:                                 WinUser.MSG; 
  PaintStructure:                      WinUser.PAINTSTRUCT;
  WinRect:                             WinDef.RECT;
  Breite:                              LONGINT;
  i:                                   INTEGER;
  cx, cy, cyTop:                       INTEGER;

BEGIN
  
  CASE message OF
    
    WinUser.WM_CREATE:
      (* Initialize MDI Client Window; first: fill mdi client info structure *)
      ClientCreateStruct.idFirstChild  := Resource.IDM_Window_First_Child;
      ClientCreateStruct.hWindowMenu   := WinUser.GetSubMenu (WinUser.GetMenu (hWnd),
                                                      WinUser.GetMenuItemCount (WinUser.GetMenu (hWnd)) - 4);
      Common.hWndMDIClient := WinUser.CreateWindowExA(WinUser.WS_EX_CLIENTEDGE,
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
                                   Common.hInstance,       (* This instance owns this Window.    *)
                                   SYSTEM.ADR(ClientCreateStruct));     (* MDI Client Structure. *)

      ASSERT(Common.hWndMDIClient#0);

      Result         := UserInterface.SetMenu(hWnd, 0);
  
      (* Initialize Statusbar *)
      UIStatusLine.Create(hWnd, Common.hInstance);
      (* Initialize Coolbar *)
      UIToolBar.Create(hWnd, Common.hInstance);
      UITabControl.Create(hWnd, Common.hInstance);
      RETURN 0;

    | (* WinUser.WM_CREATE *)
    WinUser.WM_SIZE:
      cx       := SYSTEM.LOWORD(lParam);
      cy       := SYSTEM.HIWORD(lParam);
      cyTop    :=  0;
      IF WinUser.IsWindowVisible(UIStatusLine.hWndStatusBar)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(UIStatusLine.hWndStatusBar, WinRect);
        cy           := cy - SHORT(WinRect.bottom - WinRect.top);
        Result       := WinUser.SendMessageA(UIStatusLine.hWndStatusBar, WinUser.WM_SIZE, 0, 0);
      END (* IF WinUser.IsWindowVisible(StatusLine.hWndBar)=WinDef.True *);
      IF WinUser.IsWindowVisible(UIToolBar.hWndToolBar)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(UIToolBar.hWndToolBar, WinRect);
        cyTop        := cyTop + SHORT(WinRect.bottom - WinRect.top);
        cy           := cy    - SHORT(WinRect.bottom - WinRect.top);
        Result       := WinUser.SendMessageA(UIToolBar.hWndToolBar, WinUser.WM_SIZE, 0, 0);
      END (* IF WinUser.IsWindowVisible(Common.hWndToolBar)=WinDef.True *);
      IF WinUser.IsWindowVisible(UITabControl.hWndTabControl)=WinDef.True THEN 
        ResultBool   := WinUser.GetWindowRect(UITabControl.hWndTabControl, WinRect);
        cyTop        := cyTop + SHORT(WinRect.bottom - WinRect.top);
        cy           := cy    - SHORT(WinRect.bottom - WinRect.top);
        Result       := WinUser.SendMessageA(UITabControl.hWndTabControl, WinUser.WM_SIZE, 0, 0); 
      END (* IF WinUser.IsWindowVisible(Common.hWndTabControl)=WinDef.True *);
      ResultBool   := WinUser.MoveWindow(Common.hWndMDIClient, 0, cyTop, cx, cy, WinDef.True);
      RETURN 0
    | (* WinUser.WM_SIZE *)

    WinUser.WM_COMMAND:                                    (* command from application menu      *)
      CASE SYSTEM.LOWORD(wParam) OF
        Resource.IDM_File_Open:                              (* Menu "File", Open a file to dump it *)
          IF Files.Open (hWnd, message, wParam, lParam)=1 THEN
            ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Common.MainTitle));
            RETURN 0
          END;
          Title  := Common.MainTitle;
          Strings.Append(Title, " - [");
          Strings.Append(Title, Common.MyFileDescription.Path);
          Strings.Append(Title, Common.MyFileDescription.Name);
          Strings.AppendChar(Title, "]");
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Title));
          RETURN 0
        |
        Resource.IDM_File_Close:                             (* Close a file that is dumped *)
          Result     := Files.Close (hWnd, message, wParam, lParam);
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Common.MainTitle));
          RETURN 0
        |
        Resource.IDM_File_Start:                             (* Start a file for debugging *)
          IF Files.Start (hWnd, message, wParam, lParam)=1 THEN
            ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Common.MainTitle));
            RETURN 0
          END;
          Title  := Common.MainTitle;
          Strings.Append(Title, " - [");
          Strings.Append(Title, Common.MyFileDescription.Path);
          Strings.Append(Title, Common.MyFileDescription.Name);
          Strings.AppendChar(Title, "]");
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Title));
          RETURN 0
        |
        Resource.IDM_File_Attach:
          UIStatusLine.ShowMessage ("File.Attach not yet implemented.");
          Result     := UserInterface.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Resource.IDM_File_Suspend:
          Result     := Debug.Suspend();
          Result     := UserInterface.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Resource.IDM_File_Resume:
          Result     := Debug.Resume(Common.DebugProcess);
          Result     := UserInterface.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
          RETURN 0
        |
        Resource.IDM_File_Stop:
          Result     := Files.Stop (hWnd, message, wParam, lParam);
          ResultBool := WinUser.SetWindowTextA(hWnd, SYSTEM.ADR(Common.MainTitle));
          RETURN 0
        |
        Resource.IDM_File_Print:
          UIStatusLine.ShowMessage ("File.Print not yet implemented.");
          RETURN 0
        |
        Resource.IDM_File_Printer:
          UIStatusLine.ShowMessage ("File.Printer not yet implemented.");
          RETURN 0
        |
        Resource.IDM_File_Exit:
          Result := Options.WriteINI();
          WinUser.PostQuitMessage(0);
          RETURN 0
        |
        Resource.IDM_Edit_Search:                                (* Menu Edit *)
          Result := Edit.Search (hWnd, message, wParam, lParam);
          RETURN 0
        |
        Resource.IDM_Edit_Replace:
          Result := Edit.Replace (hWnd, message, wParam, lParam);
          RETURN 0
        |
        Resource.IDM_Edit_Goto:
          Result := Edit.Goto (hWnd, message, wParam, lParam);
          RETURN 0
        |
        Resource.IDM_Edit_Cut:
          Result := Edit.Cut (hWnd, message, wParam, lParam);
          RETURN 0
        |
        Resource.IDM_Edit_Paste:
          Result := Edit.Paste (hWnd, message, wParam, lParam);
          RETURN 0
        |
        Resource.IDM_View_HexData:                               (* Menu View *)
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.HexDataMode);
          RETURN 0
        |
        Resource.IDM_View_Dump:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DebugDataMode);
          RETURN 0
        |
        Resource.IDM_View_SectionHeaders:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.SectionHeadersMode);
          RETURN 0
        |
        Resource.IDM_View_SymbolTable:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.SymbolTableMode);
          RETURN 0
        |
        Resource.IDM_View_LineNumbers:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.LineNumbersMode);
          RETURN 0
        |
        Resource.IDM_View_SymbolTableGlobal:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.SymbolTableGlobalMode);
          RETURN 0
        |
        Resource.IDM_View_DD_Export:                             (* Submenu Data Directories *)
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_ExportMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Import:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_ImportMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Resource:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_ResourceMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Exception:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_ExceptionMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Security:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_SecurityMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_BaseRelocation:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_BaseRelocationMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Debug:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_DebugMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_Copyright:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_CopyrightMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_GlobalPtr:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_GlobalPtrMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_TLS:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_TLSMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_LoadConfig:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_LoadConfigMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_BoundImport:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_BoundImportMode);
          RETURN 0;
        |
        Resource.IDM_View_DD_IAT:
          MyMDIClient  := Common.GetMDIClient(2, 1);
          Result       := View.Switch2NewView (MyMDIClient^.hWnd, Common.DD_IATMode);
          RETURN 0;
        |
        Resource.IDM_Window_Tile_hor:                        (* Menu Window *)
          Result  := WinUser.SendMessageA(Common.hWndMDIClient, WinUser.WM_MDITILE, WinUser.MDITILE_HORIZONTAL, 0);
          RETURN 0;
        |
        Resource.IDM_Window_Tile_ver:
          Result  := WinUser.SendMessageA(Common.hWndMDIClient, WinUser.WM_MDITILE, WinUser.MDITILE_VERTICAL, 0);
          RETURN 0;
        |
        Resource.IDM_Window_Cascade:
          Result  := WinUser.SendMessageA(Common.hWndMDIClient, WinUser.WM_MDICASCADE, 0, 0);
          RETURN 0;
        |
        Resource.IDM_Window_Arrange:
          Result  := WinUser.SendMessageA(Common.hWndMDIClient, WinUser.WM_MDIICONARRANGE, 0, 0);
          RETURN 0;
        |
        Resource.IDM_Admin_Font:                             (* Menu Administration *)
          Result := Administration.AdminFont (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Admin_Zero:
          Result := Administration.Zero (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Admin_First:
          Result := Administration.First (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Admin_Second:
          Result := Administration.Second (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Admin_Third:
          Result := Administration.Third (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Admin_Fourth:
          Result := Administration.Fourth (hWnd, message, wParam, lParam);
          RETURN 0;
        |
        Resource.IDM_Options_Write:
          Result := Options.WriteINI ();
          RETURN 0;
        |

        Resource.IDM_Options_Read:
          Result := Options.ReadINI ();
          RETURN 0;
        |

        Resource.IDM_Help_About:                                 (* Menü Hilfe *)
          Result := WinUser.DialogBoxParamA   (hInstance,         (* current instance         *)
                           SYSTEM.ADR(Resource.IDD_HelpAbout),   (* resource to use          *)
                           hWnd,                               (* parent handle            *)
                           Help.About,0);                      (* About() instance address *)
                           
          RETURN 0;
        ELSE                                                   (* Lets Windows process it    *)
          RETURN WinUser.DefFrameProcA(hWnd, Common.hWndMDIClient, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *)
    | (* WinUser.WM_COMMAND *)

    WinUser.WM_MOUSEMOVE:                                  (* mouse has ben moved *)
(*      IF UIToolBar.hWndToolTip#WinDef.NULL THEN
          msg.lParam   := lParam; 
          msg.wParam   := wParam; 
          msg.message  := message; 
          msg.hwnd     := UIToolBar.hWndToolTip; 
          Result       := WinUser.SendMessageA(UIToolBar.hWndToolTip, 
                                         CommCTRL.TTM_RELAYEVENT, 
                                         0, 
                                         SYSTEM.ADR(msg)); 
      END  IF CoolBar.hWndToolTip#WinDef.NULL *);
    | (* WinUser.WM_MOUSEMOVE *)

    WinUser.WM_NOTIFY:                                    (* a control needs help *)
      MyNMHeaderP := SYSTEM.VAL(WinUser.LPNMHDR, lParam);
      CASE MyNMHeaderP^.code OF
        CommCTRL.TCN_SELCHANGING:
          ;
        | (* CommCTRL.TCN_SELCHANGING *)
        CommCTRL.TCN_SELCHANGE:
          Result   := WinUser.SendMessageA(UITabControl.hWndTabControl, 
                                         CommCTRL.TCM_GETCURSEL, 
                                         0, 
                                         0);
          MyMDIClient  := Common.GetMDIClient(1, Result);
          ASSERT(MyMDIClient#NIL);
          Result       := WinUser.SendMessageA(Common.hWndMDIClient, WinUser.WM_MDIACTIVATE, MyMDIClient.hWnd, 0);
        (* CommCTRL.TCN_SELCHANGE *)
        ELSE
          ;
      END (* CASE SYSTEM.HIWORD(wParam) *);
    | (* WinUser.WM_NOTIFY *)

    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      WinUser.PostQuitMessage(0);
      RETURN 1;
    (* WinUser.WM_DESTROY *)

    ELSE                                                   (* Passes it on if unproccessed       *)
      RETURN WinUser.DefFrameProcA(hWnd, Common.hWndMDIClient, message, wParam, lParam)
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
  MyMDIClient:                         Common.MDIClientP;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  lpScreenMetric:                      Common.ScreenMetricP;
  hdc:                                 WinDef.HDC;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  WinRect:                             WinDef.RECT;
  i:                                   LONGINT;
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
                                                                     Common.WXBScreenMetricP, 
                                                                     SYSTEM.VAL(LONGINT, lpScreenMetric));
      Common.DebugProcess^.hWnd            := hWnd;
      Result                               := Debug.Start(Common.DebugProcess);
      IF Result=0 THEN
        Result := WinUser.SendMessageA(Common.hWndMain, WinUser.WM_COMMAND, Resource.IDM_File_Stop, 0);
        UIStatusLine.ShowMessage("Debug Process ID = 0 !!");
        RETURN 0;
      END;
      UIStatusLine.ShowMode(2);

      RETURN 0;
    | (* WinUser.WM_CREATE *)
  
    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                                   WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
      DISPOSE(lpScreenMetric);
    | (* WinUser.WM_DESTROY *)

    WinUser.WM_PAINT:
      Result := View.Update(hWnd);
      RETURN 0;
    | (* WinUser.WM_PAINT *)

    WinUser.WM_SETFOCUS:
      MyMDIClient  := Common.GetMDIClient(0, hWnd);
      IF MyMDIClient#NIL THEN
        RETURN WinUser.SendMessageA(UITabControl.hWndTabControl, CommCTRL.TCM_SETCURFOCUS, MyMDIClient.TabControl, 0);
      END (* IF MyMDIClient#NIL  *);
      RETURN 0  
    | (* WinUser.WM_SETFOCUS *)

    WinUser.WM_SIZE:
      Result       := View.UseNewSize(hWnd, lParam);
      RETURN 0
    | (* WinUser.WM_SIZE *)

    WinUser.WM_KEYDOWN:                                    (* Anwender hat Taste gedrückt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.VK_UP:
          Result := View.UpAndDown(hWnd, Common.LineUp);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_DOWN:
          Result := View.UpAndDown(hWnd, Common.LineDown);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_HOME:
          Result := View.UpAndDown(hWnd, Common.Home);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_END:
          Result := View.UpAndDown(hWnd, Common.End);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_LEFT:
          Result := View.LeftAndRight(hWnd, Common.Left);
          RETURN 0
        | (* WinUser.VK_LEFT *)
        WinUser.VK_RIGHT:
          Result := View.LeftAndRight(hWnd, Common.Right);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_KEYDOWN *)

    WinUser.WM_VSCROLL:                                    (* Anwender hat vert. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_TOP:
          Result := View.UpAndDown(hWnd, Common.Home);
          RETURN 0;
        | (* WinUser.SB_TOP *)
        WinUser.SB_BOTTOM:
          Result := View.UpAndDown(hWnd, Common.End);
          RETURN 0;
        | (* WinUser.SB_BOTTOM *)
        WinUser.SB_PAGEUP:
          Result := View.UpAndDown(hWnd, Common.PageUp);
          RETURN 0;
        | (* WinUser.SB_PAGEUP *)
        WinUser.SB_PAGEDOWN:
          Result := View.UpAndDown(hWnd, Common.PageDown);
          RETURN 0;
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_LINEUP:
          Result := View.UpAndDown(hWnd, Common.LineUp);
          RETURN 0;
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.UpAndDown(hWnd, Common.LineDown);
          RETURN 0;
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          UIStatusLine.ShowMessage("Debug WND; Vert. Scrollbar THUMBPOSITION.");
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          UIStatusLine.ShowMessage("Debug WND; Vert. Scrollbar THUMBTRACK.");
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_VSCROLL *)

    WinUser.WM_HSCROLL:                                    (* Anwender hat horiz. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_LINEUP:
          Result := View.LeftAndRight(hWnd, Common.Left);
          RETURN 0
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.LeftAndRight(hWnd, Common.Right);
          RETURN 0
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          UIStatusLine.ShowMessage("Debug WND; Horiz. Scrollbar THUMBPOSITION.");
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          UIStatusLine.ShowMessage("Debug WND; Horz. Scrollbar THUMBTRACK.");
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    (* WinUser.WM_HSCROLL*)

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
  ActLine:                             Common.ScreenLineP;
  Done:                                BOOLEAN;
  MyMDIClient:                         Common.MDIClientP;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  hBrush:                              WinDef.HBRUSH;
  hDC:                                 WinDef.HDC;
  hPenDot:                             WinDef.HPEN;
  lpScreenMetric:                      Common.ScreenMetricP;
  MyCursor:                            WinDef.HCURSOR;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  Position:                            WinDef.POINT;
  WinRect:                             WinDef.RECT;
  i:                                   LONGINT;

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
      Result                               := WinUser.SetWindowLongA(hWnd, Common.WXBScreenMetricP, 
                                                                           SYSTEM.VAL(LONGINT, lpScreenMetric));
      Common.DisplayMode                   := Common.HexDataMode;
      Result                               := View.Switch2NewView(hWnd, Common.HexDataMode);
      RETURN 0;
    | (* WinUser.WM_CREATE *)

    WinUser.WM_DESTROY:                                    (* message: Window being destroyed  *)
      lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                                   WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
      DISPOSE(lpScreenMetric);
      Common.DisplayMode                   := Common.Invalid;
      RETURN 1;
    | (* WinUser.WM_DESTROY *)

    WinUser.WM_PAINT:
      Result := View.Update(hWnd);
      RETURN 0;
    | (* WinUser.WM_PAINT *)

    WinUser.WM_SETFOCUS:
      MyMDIClient  := Common.GetMDIClient(0, hWnd);
      IF MyMDIClient#NIL THEN
        RETURN WinUser.SendMessageA(UITabControl.hWndTabControl, CommCTRL.TCM_SETCURFOCUS, MyMDIClient.TabControl, 0);
      END (* IF MyMDIClient#NIL  *);
      RETURN 0;
    | (* WinUser.WM_SETFOCUS *)

    WinUser.WM_SIZE:
      Result       := View.UseNewSize(hWnd, lParam);
      RETURN 0
    | (* WinUser.WM_SIZE *)

    WinUser.WM_KEYDOWN:                                    (* Anwender hat Taste gedrückt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.VK_UP:
          Result := View.UpAndDown(hWnd, Common.LineUp);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_DOWN:
          Result := View.UpAndDown(hWnd, Common.LineDown);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_HOME:
          Result := View.UpAndDown(hWnd, Common.Home);
          RETURN 0;
        | (* WinUser.VK_UP *)
        WinUser.VK_END:
          Result := View.UpAndDown(hWnd, Common.End);
          RETURN 0;
        | (* WinUser.VK_DOWN *)
        WinUser.VK_LEFT:
          Result := View.LeftAndRight(hWnd, Common.Left);
          RETURN 0
        | (* WinUser.VK_LEFT *)
        WinUser.VK_RIGHT:
          Result := View.LeftAndRight(hWnd, Common.Right);
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_KEYDOWN *)

    WinUser.WM_VSCROLL:                                    (* Anwender hat vert. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_TOP:
          Result := View.UpAndDown(hWnd, Common.Home);
          RETURN 0;
        | (* WinUser.SB_TOP *)
        WinUser.SB_BOTTOM:
          Result := View.UpAndDown(hWnd, Common.End);
          RETURN 0;
        | (* WinUser.SB_BOTTOM *)
        WinUser.SB_PAGEUP:
          Result := View.UpAndDown(hWnd, Common.PageUp);
          RETURN 0;
        | (* WinUser.SB_PAGEUP *)
        WinUser.SB_PAGEDOWN:
          Result := View.UpAndDown(hWnd, Common.PageDown);
          RETURN 0;
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_LINEUP:
          Result := View.UpAndDown(hWnd, Common.LineUp);
          RETURN 0;
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.UpAndDown(hWnd, Common.LineDown);
          RETURN 0;
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          Result := View.ThumbPosition(hWnd, SYSTEM.HIWORD(wParam));
          UIStatusLine.ShowMessage("Dump WND; Vert. Scrollbar THUMBPOSITION.");
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          Result := View.ThumbTrack(hWnd, SYSTEM.HIWORD(wParam));
          UIStatusLine.ShowMessage("Dump WND; Vert. Scrollbar THUMBTRACK.");
          RETURN 0
        ELSE                                                 (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_VSCROLL *)

    WinUser.WM_HSCROLL:                                    (* Anwender hat horiz. Scrollbar betätigt *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_LINEUP:
          Result := View.LeftAndRight(hWnd, Common.Left);
          RETURN 0
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_LINEDOWN:
          Result := View.LeftAndRight(hWnd, Common.Right);
          RETURN 0
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_THUMBPOSITION:
          UIStatusLine.ShowMessage("Dump WND; Horiz. Scrollbar THUMBPOSITION.");
          RETURN 0
        | (* WinUser.SB_THUMBPOSITION *)
        WinUser.SB_THUMBTRACK:
          UIStatusLine.ShowMessage("Dump WND; Horz. Scrollbar THUMBTRACK.");
          RETURN 0
        ELSE                                               (* Lets Windows process it    *)
          RETURN WinUser.DefMDIChildProcA(hWnd, message, wParam, lParam);
      END (* CASE SYSTEM.LOWORD(wParam) OF *);
    | (* WinUser.WM_HSCROLL *)

    WinUser.WM_MOUSEMOVE:                                  (* mouse has ben moved *)
      Position.x   := SYSTEM.LOWORD(lParam);
      Position.y   := SYSTEM.HIWORD(lParam); 
      IF Common.DisplayMode=Common.DD_DebugMode THEN
        UIStatusLine.ShowPosition(Position.x, Position.y);
        lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, 
                                     WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
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
            MyCursor   := WinUser.SetCursor(WinUser.LoadCursorA(Common.hInstance, Resource.Cursor02));
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
      END (* IF Common.DisplayMode=Common.DD_DebugMode *);
    | (* WinUser.WM_MOUSEMOVE *)

    WinUser.WM_LBUTTONDOWN:                                (* mouse left button clicked *)
      Position.x   := SYSTEM.LOWORD(lParam);
      Position.y   := SYSTEM.HIWORD(lParam); 
      IF Common.DisplayMode=Common.DD_DebugMode THEN
        lpScreenMetric := SYSTEM.VAL(Common.ScreenMetricP, WinUser.GetWindowLongA(hWnd, Common.WXBScreenMetricP));
        ActLine        := lpScreenMetric^.FirstLineOnScreen;
        Done           := FALSE;
        i              :=  0;
        MyCursor       := WinUser.SetCursor(WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW));
        REPEAT
          IF ((Position.y<ActLine^.Area.top) & (Position.y>ActLine^.Area.bottom)) THEN
            MyCursor   := WinUser.SetCursor(WinUser.LoadCursorA(Common.hInstance, Resource.Cursor02));
            hDC        := WinUser.GetDC  (hWnd);
            hBrush     := WinGDI.CreateSolidBrush(123);
            ResultBool := WinUser.FillRect (hDC, ActLine^.Area, hBrush);
            Result     := WinUser.ReleaseDC(hWnd, hDC);
            Result     := DumpCodeView.WriteLines(ActLine^.Type);
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
      END (* IF Common.DisplayMode=Common.DD_DebugMode *);
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

PROCEDURE [_APICALL] WinMain*         (hInstance:          WinDef.HINSTANCE;   (* current instance        *)
                                       lpCmdLine:          WinDef.LPSTR;       (* command line            *)
                                       nCmdShow:           LONGINT)            (* show-Window type: open/icon *)
                                      :WinDef.LRESULT;

VAR
  msg:                                 WinUser.MSG;        (* message *)
  WndClassEx:                          WinUser.WNDCLASSEX;
  Result:                              LONGINT;            
  ResultBool:                          WinDef.BOOL; 
  StatLineInstance:                    WinDef.HANDLE;

BEGIN
  
  Common.hInstance         := hInstance;
  IF hInstance=0 THEN
    HALT(0)
  END (* IF hInstance=0  *);
  Common.nCmdShow          := nCmdShow;

  (* Initialize Frame Window Class *)
  (* Fill in Window class structure with parameters describing the main Window. *)
  WndClassEx.cbSize        := SIZE(WinUser.WNDCLASSEX);
  WndClassEx.style         := WinDef.NULL;                 (* Class style(s).                    *)
  WndClassEx.lpfnWndProc   := MainWndProc;                 (* Function to retrieve messages for Windows of this class.                   *)
  WndClassEx.cbClsExtra    := 0;                           (* No per-class extra data.           *)
  WndClassEx.cbWndExtra    := 0;                           (* No per-Window extra data.          *)
  WndClassEx.hInstance     := hInstance;                   (* Application that owns the class.   *)
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, Resource.MyIcon);
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := SYSTEM.ADR(Resource.IDM_Main); (* Name of menu resource in .RC file*)
  WndClassEx.lpszClassName := SYSTEM.ADR(Common.MainClass);       (* Name used in call to CreateWindow*)
  WndClassEx.hIconSm       := WinUser.LoadIconA(hInstance, Resource.MyIcon);

  (* Register the Window class *)
  Result := WinUser.RegisterClassExA(WndClassEx);
  ASSERT(Result#0);                                        (* If Window could not be created, stop program   *)

  (* Initialize MDI Debug Window *)
  (* Fill in window class structure with parameters describing the dump client window. *)
  WndClassEx.style         := WinUser.CS_NOCLOSE;          (* Class style(s).                    *)
  WndClassEx.lpfnWndProc   := DebugWndProc;                (* Function to retrieve messages for Windows of this class.                   *)
  WndClassEx.cbClsExtra    :=  0;                          (* No per-class extra data.           *)
  WndClassEx.cbWndExtra    := 12;                          (* per-Window extra data, used to store pointers *)
  WndClassEx.hInstance     := hInstance;                   (* Application that owns the class.   *)
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, Resource.MyIcon);
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := WinDef.NULL;                 (* Name of menu resource in .RC file  *)
  WndClassEx.lpszClassName := SYSTEM.ADR(Common.DebugClass);      (* Name used in call to CreateWindow  *)
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
  WndClassEx.hIcon         := WinUser.LoadIconA(hInstance, Resource.MyIcon);
  WndClassEx.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  WndClassEx.hbrBackground := WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  WndClassEx.lpszMenuName  := WinDef.NULL;                 (* Name of menu resource in .RC file  *)
  WndClassEx.lpszClassName := SYSTEM.ADR(Common.DumpClass);       (* Name used in call to CreateWindow  *)
  WndClassEx.hIconSm       := WinDef.NULL;

  (* Register the Window class *)
  Result := WinUser.RegisterClassExA(WndClassEx);           
  ASSERT(Result#0);

  Common.hWndMain := WinUser.CreateWindowExA(WinUser.WS_EX_OVERLAPPEDWINDOW + WinUser.WS_EX_CLIENTEDGE,
                           SYSTEM.ADR(Common.MainClass),   (* See RegisterClass() call.          *)
                           SYSTEM.ADR(Common.MainTitle),   (* Text for Window title bar.         *)
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
                           
  Common.hWndMDIClient := WinUser.GetWindow(Common.hWndMain, WinUser.GW_CHILD);

  (* Make the Main Window visible and update its client area *)
  ResultBool     := WinUser.ShowWindow(Common.hWndMain, nCmdShow);(* Show the Window                      *)
  ResultBool     := WinUser.UpdateWindow(Common.hWndMain); (* Sends WM_PAINT message             *)

  Result         := WinBase.GetCurrentDirectoryA(256, SYSTEM.ADR(Common.CurrentDirectory));
  Strings.AppendChar (Common.CurrentDirectory, "\");
  Result         := Options.ReadINI();
  
  (* Let's have a look at the command line to see whether there is a file name to work on    *)
  FOR i:=2 TO Param.Count() DO
    Param.Str(i, Parameter);
    IF ((Parameter[0]="-") OR (Parameter[0]="/")) THEN
      CASE Parameter[1] OF
        "D", "d":
          Common.DisplayMode := Common.DebugDataMode;
        |
        "G", "g":
          Common.DisplayMode := Common.SymbolTableGlobalMode;
        |
        "L", "l":
          Common.DisplayMode := Common.LineNumbersMode;
        |
        "S", "s":
          Common.DisplayMode := Common.SymbolTableMode;
        ELSE
          Common.DisplayMode := Common.HexDataMode;
      END (* CASE Parameter[1] *);
    ELSE
      COPY (Parameter, Common.MyFileDescription.Path);
      Result := Files.MapFile();
      Result := Dump.DumpTheFile();
      Result := View.Switch2NewView(Common.ActiveMDIClient^.hWnd, Common.DisplayMode);
      Result := UserInterface.SetMenu(Common.hWndMain, Resource.IDM_View_HexData);
      Title  := Common.MainTitle;
      Strings.Append(Title, " - [");
      Strings.Append(Title, Common.MyFileDescription.Path);
      Strings.AppendChar(Title, "]");
      ResultBool := WinUser.SetWindowTextA(Common.hWndMain, SYSTEM.ADR(Title));
      CASE Common.MyFileDescription.FileType OF
        Common.FileTypeUnknown:
          Result :=  UserInterface.SetMenu(Common.hWndMain, 0);
        ELSE
          Result := UserInterface.SetMenu(Common.hWndMain, Resource.IDM_View_HexData);
      END;
      Result := UserInterface.SetMenu(Common.hWndMain, Resource.IDM_File_Open);
      UIStatusLine.ShowMode(1);
    END;
  END;

  (* Acquire and dispatch messages until a WM_QUIT message is received.                          *)
  WHILE WinUser.GetMessageA(msg,                           (* message structure                  *)
                     WinDef.NULL,                          (* handle of Window receiving the ms  *)
                     WinDef.NULL,                          (* lowest message to examine          *)
                     WinDef.NULL)#0 DO                     (* highest message to examine         *)
  
    IF WinUser.TranslateMDISysAccel(Common.hWndMDIClient, msg)#WinDef.True THEN
      Result := WinUser.TranslateMessage(msg);             (* Translates virtual key codes *)
      Result := WinUser.DispatchMessageA(msg)              (* Dispatches message to Window *)
    END (* IF TranslateMDISysAccel(Common.hWndMDIClient, msg)#WinDef.True *);
  END (* WHILE WinUser.GetMessageA(msg, ...)#0 *)  ;
  
  (* Functions that should be used on closing... *)
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Common.MainClass),  hInstance);
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Common.DumpClass),  hInstance);
  Result       := WinUser.UnregisterClassA(SYSTEM.ADR(Common.DebugClass), hInstance);
  
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

  ResultBool   := WinUser.SetRectEmpty(MyRect);
  
  HitCounter   :=  0;
  
END O2Debug.

