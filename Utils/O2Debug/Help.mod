(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Help                                        V 1.42.00         *)
(*                                                         2002MAR16         *)
(*  PURPOSE:   functions of menu point "Help"                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   About     Show "About Box"                                              *)
(*             Display Help text                                             *)
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

MODULE Help;

IMPORT
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM,
  Global;


CONST
  Version*             =              "V 1.42.00";
  Module*              =              "Help";


VAR
  hInstance:                           WinDef.HANDLE;


(*****************************************************************************)
(*                                                                           *)
(* About                                                                     *)
(* Processes messages for "About" dialog box                                 *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  W.WM_INITDIALOG - initialize dialog box                                  *)
(*  W.WM_COMMAND    - Input received                                         *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  No initialization is needed for this particular dialog box, but TRUE     *)
(*  must be returned to Windows.                                             *)
(*                                                                           *)
(*  Wait for user to click on "Ok" button, then close the dialog box.        *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  [_APICALL] About*          (hDlg:               WinDef.HWND;        (* Window handle of the dialog box *)
                                       message:            WinDef.UINT;        (* type of message                   *)
                                       wParam:             WinDef.WPARAM;      (* message-specific information      *)
                                       lParam:             WinDef.LPARAM)
                                     : WinDef.BOOL;
                              
VAR    
  ResultBool:                          WinDef.BOOL;

BEGIN

  CASE message OF
    
    WinUser.WM_INITDIALOG:                                 (* message: initialize dialog box   *)
      RETURN WinDef.True
    | (* WinUser.WM_INITDIALOG *)
      
    WinUser.WM_COMMAND:                                    (* message: received a command        *)
      CASE (SYSTEM.LOWORD(wParam)) OF
        Global.IDP_HelpAbout_OK:                                  (* Exits the dialog box               *)
          ResultBool := WinUser.EndDialog(hDlg, WinDef.True);
          ResultBool := WinUser.SetForegroundWindow(Global.hWndMain);
          RETURN WinDef.False
        ELSE                                               (* Lets Windows process it            *)
          RETURN WinUser.DefWindowProcA(hDlg, message, wParam, lParam)
      END (* CASE (SYSTEM.LOWORD(wParam)) OF*);
      
    ELSE
      RETURN WinDef.False
  END (* CASE message OF *);

  RETURN WinDef.False                                      (* Didn't process a message           *)

END About;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;
 ;
END Help.

