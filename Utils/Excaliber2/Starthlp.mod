(*----------------------------------------------------------------------------*)
(* Copyright (c) 1997 by the POW! team                                        *)
(*                    e-Mail: pow@fim.uni-linz.ac.at                          *)
(*----------------------------------------------------------------------------*)
(*  09-01-1997 rel. 32/1.0 LEI                                                *)
(**---------------------------------------------------------------------------  
  This is an internal module of the Win32 OPAL implementation.

  This module must not be linked into a DLL. It has to be part of the library
  linked to the application program.
  ----------------------------------------------------------------------------*)

MODULE StartHlp;

IMPORT SYSTEM,WinDef,StartHlpInt,globhandles,Process;


PROCEDURE [_APICALL] WinMain*(hInstance: WinDef.HANDLE; (* current instance      *)
                              lpCmdLine: WinDef.LPSTR;  (* command line          *)
                              nCmdShow: LONGINT     (* show-window type      *)
                             ): WinDef.LRESULT;              
TYPE
  P=POINTER TO ARRAY 200 OF CHAR;
VAR
  p:P;
BEGIN

    globhandles.SetAppInstanceHandle(hInstance);
    (* set command line and nCmdShow  here *)
    StartHlpInt.ProgMain;
    Process.TerminateMsgLoops;
    Process.Yield;
    RETURN 0;
  (*END; *)
END WinMain;

END StartHlp.
