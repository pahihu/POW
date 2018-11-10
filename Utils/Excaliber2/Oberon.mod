(*****************************************************************************)
(*                                                                           *)
(* Project:    WindowsProgram                                                *)
(*                                                                           *)
(* Module:     Oberon                                      V 1.00.00         *)
(*                                                         2000SEP17         *)
(*  PURPOSE:   Generic template for Windows applications                     *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   WinMain() calls initialization function, processes message loop         *)
(*   InitApplication()                                                       *) 
(*             initializes window data, registers window and creates         *)
(*             main window                                                   *)
(*   MainWndProc()                                                           *)
(*             processes messages                                            *)
(*   About()   processes messages for "About" dialog box                     *)
(*                                                                           *)
(*  INPUT:     for a project main module only                                *)
(*                                                                           *)
(*  OUTPUT:    for a project main module only                                *)
(*                                                                           *)
(*                                                                           *)
(* Authors:    KlS    schultze-schoenberg@t-online.de                        *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created    2000SEP11                                                     *)
(*                                                                           *)
(*  update                                                                   *)
(*   2000SEP16 KlS    change request 01.001 fixed                            *)
(*                    short description                                      *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE Oberon;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM,
  StatusLine,
  MyProg00, MyProg01;
  
CONST
  Version =                           "V 1.00.00";

VAR  
  hInst:                               WinDef.HANDLE;


(*****************************************************************************)
(*                                                                           *)
(* FUNCTION:   Procedure (HWND, UINT, WPARAM, LPARAM)                        *)
(*                                                                           *)
(* PURPOSE:    Processes messages                                            *)
(*                                                                           *)
(* Input:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* Output:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*  W.WM_COMMAND                                                             *)
(*             application menu (About dialog box)                           *)
(*  W.WM_DESTROY                                                             *)
(*             destroy Window                                                *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  To process the IDM_ABOUT message, call MakeProcInstance() to get the    *)
(*  current instance address of the About() function.  Then call Dialog     *)
(*  box which will create the box according to the information in your      *)
(*  generic.rc file and turn control over to the About() function.  When    *)
(*  it returns, free the instance address.                                  *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] Procedure*       (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                     : WinDef.LRESULT;

VAR    
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;

BEGIN

  CASE Result OF
    01:
      do something;
    | (* 01 *)
    02:
      do something other;
    | (* 02 *)
    03:
      ;
    ELSE
      do the rest;
  END;
   
END Procedure;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  ;

END Oberon.

