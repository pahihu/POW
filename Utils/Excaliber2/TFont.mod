(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     Font                                         V 1.00.00        *)
(*                                                         2000SEP17         *)
(*  PURPOSE:  Font Class                                                     *)
(*                                                                           *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
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



MODULE TFont;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,T:=Types,
  SYSTEM;
  


     
TYPE
    PCHAR =                       POINTER TO ARRAY OF CHAR;
    TFont* =                      POINTER TO FontDesc;  
    FontDesc* =                   RECORD
        TextColour*:              LONGINT;
        Name:                     PCHAR;
        Size:                     LONGINT;
        Face:                     LONGINT;
        Style*:                   LONGINT;
        Height:                   LONGINT;
        Pel   :                   LONGINT;
        Handle-:                  LONGINT;
    END;  

PROCEDURE (p:TFont)Init*;
VAR
BEGIN
  p^.TextColour:=WinUser.COLOR_BTNTEXT;
END Init;

PROCEDURE (p:TFont)Destroy*;
VAR
  dummy:LONGINT;
BEGIN
  IF p^.Name # NIL THEN
    DISPOSE(p^.Name);
  END;
  IF p^.Handle # 0 THEN
    dummy := WinGDI.DeleteObject(p^.Handle);
  END;
END Destroy;


PROCEDURE (p:TFont)SetDefaultFont*;
VAR
  dummy:LONGINT;
BEGIN
    p^.Handle := WinGDI.GetStockObject(WinGDI.DEFAULT_GUI_FONT);
END SetDefaultFont;


PROCEDURE (p:TFont) SetTextColour*(colour:SET);
VAR
BEGIN
   IF T.cDarkBlue IN colour THEN p^.TextColour:= 00800000H END;
    IF T.cDarkGreen IN colour THEN p^.TextColour:= 0008000H END;
    IF T.cDarkCyan IN colour THEN p^.TextColour:= 00800080H END;
    IF T.cDarkRed IN colour THEN p^.TextColour:= 00808000H END;
    IF T.cDarkMagenta IN colour THEN p^.TextColour:= 00800080H END;
    IF T.cDarkYellow IN colour THEN p^.TextColour:= 00008080H END;
    IF T.cLightGrey IN colour THEN p^.TextColour:= 00C0C0C0H END;
    IF T.cDarkGrey IN colour THEN p^.TextColour:= 00808080H END;
    IF T.cBlue IN colour THEN p^.TextColour:= 00FF0000H END;
    IF T.cGreen IN colour THEN p^.TextColour:= 0000FF00H END;
    IF T.cCyan IN colour THEN p^.TextColour:= 00FFFF00H END;
    IF T.cRed IN colour THEN p^.TextColour:= 00800080H END;
    IF T.cMagenta IN colour THEN p^.TextColour:= 000000FFH END;
    IF T.cYellow IN colour THEN p^.TextColour:= 0000FFFFH END;
    IF T.cWhite IN colour THEN p^.TextColour:= 00FFFFFFH END;
    IF T.cMoneyGreen IN colour THEN p^.TextColour:= 00C0DCC0H END;
    IF T.cSkyBlue IN colour THEN p^.TextColour:= 00F0CAA6H END;
    IF T.cCream IN colour THEN p^.TextColour:= 00F0FBFFH END;
    IF T.cMediumGrey IN colour THEN p^.TextColour:= 00A4A0A0H END;
    IF T.cBlack IN colour THEN p^.TextColour:= 00000000H END;                
END SetTextColour;

END TFont.
