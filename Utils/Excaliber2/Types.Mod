(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     Types                                        V 1.00.00        *)
(*                                                         2000SEP17         *)
(*  PURPOSE:  pre-defined types                                              *)
(*                                                                           *)
(*  INPUT:     for a project main module only                                *)
(*                                                                           *)
(*  OUTPUT:    for a project main module only                                *)
(*                                                                           *)
(*                                                                           *)
(* Authors:     steven Watson                                                *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created    2001 Nov 17                                                   *)
(*                                                                           *)
(*  update                                                                   *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)
MODULE Types;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser;
CONST
   cBlack*=0;
   cDarkBlue*=1;
   cDarkGreen*=2;
   cDarkCyan *=3;
   cDarkRed *=4;
   cDarkMagenta*=5;
   cDarkYellow *=6;
   cSkyBlue    *=7;
   cMoneyGreen *=8;
   cLightGrey *= 9;
   cMediumGrey *=10;
   cDarkGrey *=11;
   cCream    *=12;
   cBlue *= 13;
   cGreen* =14;
   cCyan *= 15;
   cRed *= 16;
   cMagenta *= 17;
   cYellow *= 18;
   cWhite *=19;

  cLeftButton      *=  0;
  cMiddleButton    *=  1;
  cRightButton     *=  2;
  cShift           *=  3;
  cCtrl            *=  4;

 (*button consts*)
    Left*=10;
   VertButton*=0;
   PushButton*=1;
   BitmapButton*=2;
   IconButton *=3;
   CheckBox*=4;
   RadioButton*=6;
   ColourButton*=7;
   
BEGIN  
END Types.
