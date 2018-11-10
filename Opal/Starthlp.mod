(*----------------------------------------------------------------------------*)
(* Copyright (c) 1997 by the POW! team                                        *)
(*                    e-Mail: pow@fim.uni-linz.ac.at                          *)
(*----------------------------------------------------------------------------*)
(*  09-01-1997 rel. 32/1.0 LEI                                                *)
(* 2001JAN10   KlS     CALL to OPAL.Param_Parse eliminated                    *)
(**---------------------------------------------------------------------------  
  This is an internal module of the Win32 OPAL implementation.

  This module must not be linked into a DLL. It has to be part of the library
  linked to the application program.
  ----------------------------------------------------------------------------*)

MODULE StartHlp;

IMPORT
  SYSTEM,
  IOManage, StartHlpInt,
  WinDef;


PROCEDURE [_APICALL] WinMain*         (hInstance:          WinDef.HANDLE;      (* current instance      *)
                                       lpCmdLine:          WinDef.LPSTR;       (* command line          *)
                                       nCmdShow:           LONGINT)            (* show-window type      *)
                                      :                    WinDef.LRESULT;
                                      
BEGIN
  IOManage.CreateApp;
  IF IOManage.RunApp(hInstance,lpCmdLine,nCmdShow) THEN
    StartHlpInt.ProgMain;
    RETURN IOManage.EndApp();
  ELSE
    RETURN 0;
  END;
END WinMain;

END StartHlp.
