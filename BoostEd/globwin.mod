(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     GlobWin                                     V 2.00.04         *)
(*                                                         2004OCT18         *)
(*  PURPOSE:   This module provides global access to some commonly used      *)
(*             functions which are closely tied to the Windows API.          *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(* Author(s):                                                                *)
(*                     Michael Bogner, Max Mayrbäurl                         *)
(*             BL      Bernhard Leisch                                       *)
(*                     Alexander Bergsmann                                   *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   1998                                                                    *)
(*                                                                           *)
(*  update                                                                   *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(* Comments                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE GlobWin;

IMPORT
  Strings, 
  WinBase, WinDef, WinNT, WinUser, 
  SYSTEM;

CONST
  HELPFILENAME         =              "BoostEd.hlp";

VAR
  hInstance*:                          WinDef.HINSTANCE;


  (*****************************************************************************)
PROCEDURE DisplayError*               (Title:              ARRAY OF CHAR;
                                       Message:            ARRAY OF CHAR);
  (* Show a pop-up window with an error message.                               *)
  (* The procedure returns after the user has acknowledged the error message.  *)
VAR
  ResultBool:                          WinDef.BOOL;
BEGIN
  ResultBool   := WinUser.MessageBoxA(WinDef.NULL, 
  SYSTEM.ADR(Message), 
  SYSTEM.ADR(Title), 
  WinUser.MB_OK);
END DisplayError;


(*****************************************************************************)
PROCEDURE ShowHelp*                   (hEdit:              WinDef.HWND);
VAR
  ret,
  i:                                   LONGINT;
  tmp,
  help:                                ARRAY 128 OF CHAR;
  dmyi:                                LONGINT;
BEGIN
  ret          := WinBase.GetModuleFileNameA(hInstance, SYSTEM.ADR(help), 128);
  IF ret=0 THEN
    DisplayError("Error", "Problems getting helpfile");
  ELSE
    i          := ret - 1;
    WHILE (i>=0) & (help[i]#"\") DO
      help[i]  := 0X;
      DEC(i);
    END (* WHILE (i>=0) & (help[i]#") *);
    Strings.Append(help, HELPFILENAME);

    dmyi       := WinUser.WinHelpA(hEdit, SYSTEM.ADR(help), WinUser.HELP_CONTENTS, 0);
    IF dmyi=0 THEN
      tmp      := "The helpfile must reside at ";
      Strings.Append(tmp, help);
      DisplayError("HELP", tmp);
    END (* IF dmyi=0 *);
  END (* IF ret=0 *);
END ShowHelp;


(*****************************************************************************)
PROCEDURE Beep*                       ();
VAR
  ResultBool:                          WinDef.BOOL;
BEGIN
  ResultBool   := WinUser.MessageBeep(WinUser.MB_ICONEXCLAMATION);
END Beep;


(*****************************************************************************)
PROCEDURE RGB*                        (r,
                                       g,
                                       b:                  INTEGER)
                                      :LONGINT;
BEGIN
  RETURN (b*256 + g)*256+r;
END RGB;

(*****************************************************************************)
(*****************************************************************************)
BEGIN
 ;
  ;
END GlobWin.


