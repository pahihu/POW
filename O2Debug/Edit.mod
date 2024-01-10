(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Edit                                        V 2.00.06         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   functions of menu point "Edit"                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Search                                                                  *)
(*   Replace                                                                 *)
(*   Goto                                                                    *)
(*   Cut                                                                     *)
(*   Paste                                                                   *)
(*                                                                           *)
(*  COMMENTS:                                                                *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000APR10                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000SEP29 überarbeitet                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Edit;

IMPORT
  Common, Resource, UIStatusLine,
  WinDef,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.06";
  Module*      =                      "Edit";
  ErrorNoOffset=                      Resource.IDM_Edit * 100;


(*****************************************************************************)
(*                                                                           *)
(* Search                                                                    *)
(*                                                                           *)
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

PROCEDURE Search*                     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;

  UIStatusLine.ShowMessage ("Edit.Search not yet implemented.");

  RETURN 0
  
END Search;


(*****************************************************************************)
(*                                                                           *)
(* Replace                                                                   *)
(*                                                                           *)
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

PROCEDURE Replace*                    (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;

  UIStatusLine.ShowMessage ("Edit.Replace not yet implemented.");

  RETURN 0
  
END Replace;


(*****************************************************************************)
(*                                                                           *)
(* Goto                                                                      *)
(*                                                                           *)
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

PROCEDURE Goto*                       (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;

  UIStatusLine.ShowMessage ("Edit.Goto not yet implemented.");

  RETURN 0
  
END Goto;


(*****************************************************************************)
(*                                                                           *)
(* Cut                                                                       *)
(*                                                                           *)
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

PROCEDURE Cut*                        (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;

  UIStatusLine.ShowMessage ("Edit.Cut not yet implemented.");

  RETURN 0
  
END Cut;


(*****************************************************************************)
(*                                                                           *)
(* Paste                                                                     *)
(*                                                                           *)
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

PROCEDURE Paste*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;

  UIStatusLine.ShowMessage ("Edit.Paste not yet implemented.");

  RETURN 0
  
END Paste;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  ;

END Edit.

