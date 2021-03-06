(*----------------------------------------------------------------------------*)
(* Copyright (c) 1997 by the POW! team                                        *)
(*                    e-Mail: pow@fim.uni-linz.ac.at                          *)
(*----------------------------------------------------------------------------*)
(*  09-01-1997 rel. 32/1.0 LEI                                                *)
(**---------------------------------------------------------------------------  
  This is an internal module of the Win32 OPAL implementation.
  It provides access to the Win32 handles of the application and the main
  application window.
  ----------------------------------------------------------------------------*)

MODULE GlobHandles;

IMPORT WD:=WinDef;

VAR
  appInstance:WD.HINSTANCE;



PROCEDURE SetAppInstanceHandle*(hinst:WD.HINSTANCE);
BEGIN
  appInstance:=hinst;
END SetAppInstanceHandle;

PROCEDURE GetAppInstanceHandle*():WD.HINSTANCE;
BEGIN
  RETURN appInstance;
END GetAppInstanceHandle;

BEGIN
  appInstance:=WD.NULL;
END GlobHandles.
