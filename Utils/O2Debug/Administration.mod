(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Administration                              V 1.42.00         *)
(*                                                         2002MAR16         *)
(*  PURPOSE:   functions of menu point "Administration"                      *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   First     first command                                                 *)
(*   Second    second command                                                *)
(*   Third     third command                                                 *)
(*   Fourth    fourth command                                                *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000SEP15                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Administration;


IMPORT 
  CommCTRL, WinBase, WinDef, WinGDI, WinUser,
  Strings, SYSTEM,
  Global, Help, StatusLine;


CONST
  Version*             =              "V 1.42.00";
  Module*              =              "Administration";
 

VAR
  hInst:                               WinDef.HANDLE;
  Result:                              LONGINT;


(*****************************************************************************)
(*                                                                           *)
(* AdminFont                                               2001JUL08         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE AdminFont*                  (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  StatusLine.SetText("Font Modification, yet not implemented.", StatusLine.NoticeField);
  RETURN 0;
  
END AdminFont;


(*****************************************************************************)
(*                                                                           *)
(* Zero                                                    2000MAY02         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Zero*                       (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  Result := WinUser.SendMessageA(StatusLine.hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 0, 
                                 SYSTEM.ADR("Will always be displayed (in statusline)."));
  RETURN 0;
  
END Zero;


(*****************************************************************************)
(*                                                                           *)
(* First                                                   2000MAY02         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE First*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  Result := WinUser.SendMessageA(StatusLine.hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 0, 
                                 SYSTEM.ADR("This is the First Try."));
  RETURN 0;
  
END First;


(*****************************************************************************)
(*                                                                           *)
(* Second                                                  2000MAY02         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Second*                     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  Result := WinUser.SendMessageA(StatusLine.hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 1, 
                                 SYSTEM.ADR("Oops, Second Try."));
  RETURN 0
  
END Second;


(*****************************************************************************)
(*                                                                           *)
(* Third                                                   2000MAY02         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Third*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  Result := WinUser.SendMessageA(StatusLine.hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 2, 
                                 SYSTEM.ADR("Oh oh, Third Try."));
  RETURN 0
  
END Third;


(*****************************************************************************)
(*                                                                           *)
(* Third                                                   2000MAY02         *)
(* Dummy Meldung für einen Funktionsaufruf                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Fourth*                     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      : WinDef.LRESULT;

BEGIN;
  
  Result := WinUser.SendMessageA(StatusLine.hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 3, 
                                 SYSTEM.ADR("But now, fourth and last Try."));
  RETURN 0
  
END Fourth;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  ;

END Administration.

