(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     Canvas                                       V 1.00.00        *)
(*                                                         2000SEP17         *)
(*  PURPOSE:  Drawing Class for visible  GUI controls                        *)
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



MODULE TCanvas;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM,Control,TFont;
CONST

    
TYPE
    PCHAR =                      POINTER TO ARRAY OF CHAR;
    TCanvas* =                     POINTER TO CanvasDesc;
    CanvasDesc* =                 RECORD (TFont.FontDesc)
        hDC-  :                    WinDef.HDC;
        Brush-:                    LONGINT;
    END;

PROCEDURE (p:TCanvas)Init*;
VAR
BEGIN
  p^.Init^;
  p^.Brush :=0;
END Init;


PROCEDURE (p : TCanvas) Destroy*;
VAR
  dummy : LONGINT;
BEGIN
  p^.Destroy^;
   IF p^.Brush # 0 THEN
    dummy := WinGDI.DeleteObject(p^.Brush);
  END;
END Destroy;

PROCEDURE (p:TCanvas)SetDC*(hdc:WinDef.HDC);
(*VALIDATES HDC FOR ONPAINT INTERNAL METHOD HIDE *)
BEGIN
  p^.hDC:=hdc;
END SetDC;


PROCEDURE (p:TCanvas)GetDC*(hWnd:WinDef.HWND);
BEGIN
          p.hDC:=WinUser.GetDC(hWnd);
END GetDC;

PROCEDURE (p:TCanvas)ReleaseDC*(hWnd:WinDef.HWND);
VAR
  dummy:LONGINT;
BEGIN
          dummy := WinUser.ReleaseDC(hWnd,p^.hDC);
END ReleaseDC;

PROCEDURE (p:TCanvas)DrawEdge*(rect:WinDef.RECT;edge,flags:LONGINT);
VAR
  dummy:LONGINT;
BEGIN
   dummy := WinUser.DrawEdge(p^.hDC,rect,edge,flags);  
END DrawEdge;
                              
PROCEDURE (p:TCanvas)DrawFocus*(rect:WinDef.RECT);
VAR
  dummy:LONGINT;
BEGIN
          dummy := WinUser.DrawFocusRect(p^.hDC,rect);
END DrawFocus;

PROCEDURE (p:TCanvas)DrawText*(txt:ARRAY OF CHAR;rect:WinDef.RECT;flags:LONGINT);
VAR
  OldMode,OldTxtColour,OldBkColour,dummy :LONGINT;
BEGIN

    OldTxtColour := WinGDI.SetTextColor(p^.hDC,p^.TextColour);
    OldMode := WinGDI.SetBkMode(p^.hDC,WinGDI.TRANSPARENT); 
    dummy := WinUser.DrawTextA(p^.hDC,SYSTEM.ADR(txt),-1,rect,flags);      
    dummy := WinGDI.SetTextColor(p^.hDC,OldTxtColour);
    dummy := WinGDI.SetBkMode(p^.hDC,OldMode);
END DrawText;

PROCEDURE (p : TCanvas)CreateBrush*;
VAR
BEGIN
    p^.Brush:=WinGDI.CreateSolidBrush(00F0CAA6H);
END CreateBrush;
END TCanvas.
