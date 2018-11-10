(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     SingleEdit                                      V 1.00.00     *)
(*                                                         2001 NOV 17       *)
(*  PURPOSE:  Class for Edit Controls                                        *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*  Init();                                                                  *)
(*  handleMessage(hWnd,Message,wParam,lParam):longint;                       *)
(*  Destroy();                                                               *)
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



MODULE SingleEdit;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM,Control,globhandles,TCanvas;

TYPE
  PCHAR=                        POINTER TO ARRAY OF CHAR;
  MESSAGEPROC =                 PROCEDURE ();
  SingleEdit* =                 POINTER TO SingleEditDesc;
  SingleEditDesc* =             RECORD(Control.ControlDesc);
      hWnd-:                    LONGINT;
      Style:                    SET;
      Canvas:                   TCanvas.TCanvas;
      Rect :                    WinDef.RECT;
      Visible :                 BOOLEAN;
      TextLength*:              LONGINT;
      Buffer*:                  PCHAR;
      Owner*:                   LONGINT;
      OnChange*:                MESSAGEPROC;
   END;

PROCEDURE (p : SingleEdit) Init*;
VAR
  style,dummy:LONGINT;
BEGIN
      p^.hWnd       := 0;
      p^.Visible    := FALSE;
      p^.TextLength := 0;
      p^.Buffer     := NIL;
      p^.Owner      := 0;      
      p^.OnChange   := NIL;
   
END Init;

PROCEDURE (p:SingleEdit)GetRect*;
VAR
  dummy:LONGINT;
BEGIN
          dummy := WinUser.GetClientRect(p^.hWnd,p^.Rect);
END GetRect;

PROCEDURE (p : SingleEdit)CtlColourEdit*(wParam,lParam:LONGINT);
VAR
  dummy : LONGINT;
BEGIN
   dummy := WinGDI.SetTextColor(wParam,p^.Canvas.TextColour);
   dummy := WinGDI.SetBkMode(wParam,WinGDI.TRANSPARENT);

   p^.Return( p^.Canvas^.Brush);
END CtlColourEdit;

PROCEDURE (p:SingleEdit) Create*(x,y,Width,Height,Owner:LONGINT;Visible:BOOLEAN;Style:SET):BOOLEAN;
VAR
  dummy : LONGINT;
BEGIN
   p^.Visible:=Visible;p^.Owner:=Owner;p^.Style:=Style;
   p^.hWnd := WinUser.CreateWindowExA(WinUser.WS_EX_CLIENTEDGE,
                             SYSTEM.ADR("EDIT"),
                             0,
                             WinUser.WS_CHILD,
                             x,         
                             y,         
                             Width,Height,
                             p^.Owner,                  
                             p^.GetID(),                  
                             globhandles.GetAppInstanceHandle(),             
                             WinDef.NULL);
   IF p^.hWnd # 0 THEN
     p^.GetRect;
     IF p^.Visible THEN dummy:=WinUser.ShowWindow(p^.hWnd,WinUser.SW_SHOW) END;
     IF p^.TextLength > 0 THEN
       NEW(p.Buffer,p^.TextLength+1);
       dummy:=WinUser.SendMessageA(p^.hWnd,WinUser.EM_LIMITTEXT,p.TextLength,0);
     END;
     NEW(p^.Canvas);
     p^.Canvas^.Init;
     WinBase.SetLastError(0);
    p^.NewProc :=Control.HandleSubclassEvent;
    p^.OldProc := SYSTEM.VAL(WinUser.WNDPROC,WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_WNDPROC,SYSTEM.VAL(LONGINT,p^.NewProc)));
     dummy:=WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_USERDATA,SYSTEM.VAL(LONGINT,p));
   RETURN TRUE;
   END;
   RETURN FALSE; 
END Create;


PROCEDURE (p : SingleEdit)MouseMove*(Keys:SET;x,y:LONGINT);
VAR
  dummy:LONGINT;
BEGIN
  dummy:=WinUser.MessageBeep(-1);
END MouseMove;

PROCEDURE (p : SingleEdit) Command*(wParam,lParam :LONGINT);
VAR
  dummy :LONGINT;
BEGIN
  IF SYSTEM.HIWORD(wParam) = WinUser.EN_CHANGE THEN
    IF p^.OnChange # NIL THEN
      p^.OnChange();
      p^.Return(0);
    END;
  END;
END Command;

PROCEDURE (p : SingleEdit) Destroy*;
VAR
  dummy:LONGINT;
BEGIN
        dummy := WinUser.MessageBoxA(0,SYSTEM.ADR("SingleEdit"),SYSTEM.ADR("Destroy"),0);
  IF (p^.OldProc # NIL) & (p^.NewProc # NIL) THEN (* component is subclassed then restore original values*)
    p^.NewProc := SYSTEM.VAL(WinUser.WNDPROC,WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_WNDPROC,SYSTEM.VAL(LONGINT,p^.OldProc)));    
  END;
  p^.Canvas^.Destroy;
  DISPOSE(p^.Buffer);
END Destroy;

PROCEDURE (p : SingleEdit) SetText* (txt:ARRAY OF CHAR);
VAR
  dummy:LONGINT;
BEGIN
    dummy:=WinUser.SetWindowTextA(p^.hWnd,SYSTEM.ADR(txt));
END SetText;

PROCEDURE (p : SingleEdit) GetText* ();
VAR
  dummy:LONGINT;
BEGIN
    dummy:=WinUser.GetWindowTextLengthA(p.hWnd);
    DISPOSE(p^.Buffer);
    NEW(p^.Buffer,dummy+1);
    dummy:=WinUser.GetWindowTextA(p^.hWnd,SYSTEM.ADR(p^.Buffer^),dummy+1);
END GetText;

PROCEDURE (p:SingleEdit) SetDefaultFont*;
VAR
  dummy : LONGINT;
BEGIN
  p^.Canvas^.SetDefaultFont;
  dummy:=WinUser.SendMessageA(p^.hWnd,WinUser.WM_SETFONT,p^.Canvas.Handle,1);
END SetDefaultFont;

PROCEDURE (p:SingleEdit) SetTextColour*(colour:SET);
VAR
BEGIN
  p^.Canvas.SetTextColour(colour);
END SetTextColour;

PROCEDURE (p : SingleEdit)CreateBrush*;
VAR
BEGIN
    p^.Canvas^.CreateBrush;
END CreateBrush;

END SingleEdit.
