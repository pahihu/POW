(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     PushButton                                   V 1.00.00        *)
(*                                                         2001 NOV 17       *)
(*  PURPOSE:  Class for Panel                                                *)
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



MODULE Panel;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,WinUtils,
  SYSTEM,Control,globhandles,Strings,TCanvas;

TYPE
  PCHAR=                        POINTER TO ARRAY OF CHAR;
  MESSAGEPROC=                  PROCEDURE ();
  Panel* =                      POINTER TO PanelDesc;
  PanelDesc* =                  RECORD (Control.ControlDesc);
      Canvas*:                  TCanvas.TCanvas;
      hWnd*:                    LONGINT;
      Rect*:                    WinDef.RECT;
      Visible:                  BOOLEAN;
      Caption-:                 PCHAR;
      Owner*:                   LONGINT;
   END;

CONST
  Class="PowPanel";

PROCEDURE (p:Panel)GetRect*;
VAR
  dummy:LONGINT;
BEGIN
          dummy := WinUser.GetClientRect(p^.hWnd,p^.Rect);
END GetRect;

PROCEDURE (p:Panel) RegisterClass*():BOOLEAN;
VAR
  wc:                                 WinUser.WNDCLASS;

BEGIN
  IF WinUtils.IsClassRegistered(globhandles.GetAppInstanceHandle(),Class) THEN
    RETURN TRUE;
  END;
  wc.style := 0;    
  wc.lpfnWndProc := Control.HandleEvent;
  wc.cbClsExtra := 0;                   
  wc.cbWndExtra := 0;
  wc.hInstance := globhandles.GetAppInstanceHandle();            
  wc.hIcon := 0; 
  wc.hCursor := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  wc.hbrBackground :=WinUser.COLOR_BTNFACE+1;
  wc.lpszMenuName := 0;
  wc.lpszClassName := SYSTEM.ADR(Class); 
  RETURN WinUser.RegisterClassA(wc)#0;

END RegisterClass;


PROCEDURE (p : Panel) Init*;
VAR
  dummy:LONGINT;
BEGIN
    NEW(p^.Caption,1);
    NEW(p^.Canvas);
    p^.hWnd:=0;
    p^.Owner:=0;
    p^.Visible :=FALSE; 
    p^.Canvas^.Init;
    p^.Canvas^.SetDefaultFont;
END Init;

PROCEDURE (p:Panel) Create*(x,y,Width,Height,Owner:LONGINT;Visible:BOOLEAN):BOOLEAN;
VAR
  dummy:LONGINT;
BEGIN
      p^.Visible:=Visible;p^.Owner:=Owner;

  IF p.RegisterClass() THEN
      p^.hWnd := WinUser.CreateWindowExA(0,
                             SYSTEM.ADR(Class),
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
         IF p^.Visible THEN dummy:=WinUser.ShowWindow(p^.hWnd,WinUser.SW_SHOW) END;
         p^.GetRect;
         WinBase.SetLastError(0);
         dummy:=WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_USERDATA,SYSTEM.VAL(LONGINT,p));
         RETURN TRUE;
       END;
   END;
   RETURN FALSE;
END Create;

PROCEDURE (p : Panel)Paint*;
VAR
  ps:                                 WinUser.PAINTSTRUCT;
  hDC,dummy:                              LONGINT;  
BEGIN
   hDC:=WinUser.BeginPaint(p^.hWnd,ps);
    p^.Canvas^.SetDC(hDC);
    p^.GetRect;
    IF p^.Canvas^.Brush # 0 THEN
        dummy:=WinUser.FillRect(p^.Canvas.hDC,p^.Rect,p^.Canvas.Brush);
    END;
    p^.Canvas^.DrawEdge(p^.Rect,WinUser.EDGE_RAISED,WinUser.BF_RECT);
    IF Strings.Length(p^.Caption^) >0 THEN
       dummy:=WinGDI.SelectObject(p^.Canvas^.hDC,p^.Canvas^.Handle);
       p^.Canvas^.DrawText(p^.Caption^,p^.Rect,WinUser.DT_VCENTER+WinUser.DT_SINGLELINE+WinUser.DT_CENTER);    
       dummy := WinGDI.SelectObject(p.Canvas.hDC,dummy);
    END;
    
    dummy := WinUser.EndPaint(p^.hWnd,ps);
    p^.Return(0);    
END Paint;


PROCEDURE (p : Panel) Destroy*;
VAR
  dummy:LONGINT;
BEGIN
   p^.Canvas^.Destroy;
   DISPOSE(p^.Caption);
   DISPOSE(p^.Canvas); 
END Destroy;

PROCEDURE (p: Panel) SetCaption* (txt:ARRAY OF CHAR);
VAR
  dummy:LONGINT;
BEGIN
    DISPOSE(p^.Caption);
    NEW(p^.Caption,Strings.Length(txt)+1);  
    COPY(txt,p^.Caption^);
    dummy:=WinUser.InvalidateRect(p^.hWnd,NIL,1);
END SetCaption;

PROCEDURE (p:Panel) SetDefaultFont*;
VAR
  dummy : LONGINT;
BEGIN
  p^.Canvas^.SetDefaultFont;
  dummy:=WinUser.SendMessageA(p^.hWnd,WinUser.WM_SETFONT,p^.Canvas.Handle,1);
END SetDefaultFont;

PROCEDURE (p : Panel)CreateBrush*;
VAR
BEGIN
    p^.Canvas^.CreateBrush;
END CreateBrush;
END Panel.
