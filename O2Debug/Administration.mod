(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Administration                              V 2.00.13         *)
(*                                                         2003APR22         *)
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
  Common, Help, Options, Resource, UIStatusLine,
  Strings,
  CommCTRL, WinBase, WinDef, WinGDI, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.13";
  Module*      =                      "Administration";
  ErrorNoOffset=                      Resource.IDM_Admin * 100;
 

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
                                      :WinDef.LRESULT;

BEGIN;
  
  UIStatusLine.ShowMessage("Font Modification, yet not implemented.");
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
  
  Result       := Options.WriteINI();
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
  
  Result       := Options.ReadINI();
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
  
  UIStatusLine.ShowMessage1Hex("Oops, Second Try: ", 255);
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
  
  UIStatusLine.ShowMessage2("Oh oh, Third Try:", 12, 345);
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
  
  UIStatusLine.ShowMessage("But now, fourth and last Try.");
  RETURN 0
  
END Fourth;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  ;

END Administration.

