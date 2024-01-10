(*****************************************************************************)
(*                                                                           *)
(* Project:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* Module:     UITabControl                                V 2.00.42         *)
(*                                                         2003APR19         *)
(*  PURPOSE:   manages the TabControl                                        *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Create    generates a tabcontrol                                        *)
(*                                                                           *)
(*                                                                           *)
(* Copyright:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)
(*             e-Mail: schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*                                                                           *)
(*   update                                                                  *)
(*                                                                           *)
(*   release                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE UITabControl;


IMPORT
  Common, UIStatusLine, UIToolBar,
  Strings,
  CommCTRL, WinBase, WinDef, WinGDI, WinNT, WinUser,
  SYSTEM;
  
  
CONST
  Version*     =                      "V 2.00.42";
  Module*      =                      "UITabControl";
  
  
TYPE
  TTextStringP =                       POINTER TO ARRAY 128 OF CHAR;


VAR
  hWndTabControl*:                     WinDef.HWND;
  Index*:                              LONGINT;

  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  TextString:                          ARRAY 1024 OF CHAR;
  
  TCitem:                              CommCTRL.TCITEM;
  

(*****************************************************************************)
(*                                                                           *)
(* Append                                                                    *)
(*   appends a new tab control                                               *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text                                                                    *)
(*                                                                           *)
(* Output:                                                                   *)
(*             the index of the tab control                                  *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Append*                     (hWnd:               WinDef.HWND;
                                       Text:               ARRAY OF CHAR);
                                      
VAR
  MyMDIClient:                         Common.MDIClientP;
                                       
BEGIN

  MyMDIClient              := Common.GetMDIClient(0, hWnd);
  ASSERT(MyMDIClient#NIL);
  TCitem.mask              := CommCTRL.TCIF_TEXT + CommCTRL.TCIF_IMAGE + CommCTRL.TCIF_PARAM;
  TCitem.dwState           :=  0;
  TCitem.dwStateMask       :=  0;
  TCitem.pszText           := SYSTEM.ADR(Text);
  TCitem.cchTextMax        := 12;
  TCitem.iImage            := -1;
  TCitem.lParam            :=  0;
  MyMDIClient.TabControl   := WinUser.SendMessageA(hWndTabControl, CommCTRL.TCM_INSERTITEM, Index, SYSTEM.ADR(TCitem));
  INC(Index);
  
END Append;


(*****************************************************************************)
(*                                                                           *)
(* Remove                                                                    *)
(*   appends a new tab control                                               *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text                                                                    *)
(*                                                                           *)
(* Output:                                                                   *)
(*             the index of the tab control                                  *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Remove*                     (Index:              LONGINT);
                                      
VAR
  Result:                              LONGINT;
                                       
BEGIN

  Result       := WinUser.SendMessageA(hWndTabControl, CommCTRL.TCM_DELETEITEM, Index, 0);
 
END Remove;


(*****************************************************************************)
(*                                                                           *)
(* Create                                                                    *)
(*                                                                           *)
(* Input:                                                                    *)
(*   hWndParent                                                              *)
(*             window handle of parent window                                *)
(*   hInstanceParent                                                         *)
(*             instance handle of the module that wants to create a          *)
(*             status bar.                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Create*                     (hWndParent:         WinDef.HWND;
                                       hInstanceParent:    WinDef.HWND);
                                      
VAR
  Error:                               WinDef.DWORD;
  cyTop,
  Result,
  Width:                               LONGINT;
  ParentRect,
  WinRect:                             WinDef.RECT;
  Number:                              ARRAY 80 OF CHAR;
                                       
BEGIN

  Result         := WinUser.GetClientRect(hWndParent, ParentRect);
  
  cyTop          := 30;
  
  IF WinUser.IsWindowVisible(UIToolBar.hWndToolBar)=WinDef.True THEN 
    ResultBool   := WinUser.GetWindowRect(UIToolBar.hWndToolBar, WinRect);
    cyTop        := SHORT(WinRect.bottom - WinRect.top);
  END (* IF WinUser.IsWindowVisible(hWndToolBar)=WinDef.True *);

  hWndTabControl := WinUser.CreateWindowExA(WinUser.WS_EX_WINDOWEDGE,
                               SYSTEM.ADR("SysTabControl32"),    (* See RegisterClass() call.          *)
                               WinDef.NULL,                (* no Text for Window title bar.      *)
                                                           (* Window style.                      *)
                               WinUser.WS_CHILD + WinUser.WS_VISIBLE + WinUser.WS_CLIPSIBLINGS + CommCTRL.TCS_TABS,
                               0,
                               cyTop,
                               ParentRect.right,
                               20,
                               hWndParent,
                               WinDef.NULL,
                               hInstanceParent,
                               0);
  IF hWndTabControl=0 THEN
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
    RETURN
  END;

  Result       := WinUser.SendMessageA(hWndTabControl, WinUser.WM_SETFONT, WinGDI.GetStockObject(WinGDI.DEFAULT_GUI_FONT), 0);

END Create;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  Index                        :=  1;
  
END UITabControl.

