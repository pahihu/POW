(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     PushButton                                   V 1.00.00        *)
(*                                                         2000SEP17         *)
(*  PURPOSE:  Class for PushButtons                                     *)
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



MODULE Button;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM,Control,globhandles,Strings,TCanvas,T:=Types;



   
TYPE
  PCHAR=                        POINTER TO ARRAY OF CHAR;
  MESSAGEPROC=                  PROCEDURE ();
  Button* =                     POINTER TO ButtonDesc;
      ButtonDesc* =             RECORD(Control.ControlDesc);
      Canvas :                 TCanvas.TCanvas;
      hWnd-:                    LONGINT;
      Style-:                   SET;
      Rect* :                   WinDef.RECT;
      Visible-:                 BOOLEAN;
      Caption-:                 PCHAR;
      Owner-:                   LONGINT;
      OnClick*:                 MESSAGEPROC;
  END;

PROCEDURE (p:Button)GetRect*;
VAR
  dummy:LONGINT;
BEGIN
          dummy := WinUser.GetClientRect(p^.hWnd,p^.Rect);
END GetRect;

PROCEDURE (p : Button) SetFocus*;
VAR
  dummy : LONGINT;
BEGIN
    dummy:= WinUser.MessageBeep(-1);
END SetFocus;
(* Colour button *)
PROCEDURE (p : Button) DrawItem*(lpdis:WinUser.LPDRAWITEMSTRUCT);
VAR
  rc:WinDef.RECT;
  dummy,cx,cy : LONGINT;
BEGIN 
      p^.Canvas^.SetDC(lpdis.hDC);
      IF T.ColourButton IN p^.Style THEN
          dummy:=WinUser.FillRect(p^.Canvas.hDC,lpdis.rcItem,p^.Canvas.Brush);
          p^.Canvas^.DrawEdge(lpdis.rcItem,WinUser.EDGE_RAISED,WinUser.BF_RECT); 
          p^.Canvas^.DrawText(p^.Caption^,lpdis.rcItem,WinUser.DT_VCENTER+WinUser.DT_SINGLELINE+WinUser.DT_CENTER);      

          IF SYSTEM.BITAND(lpdis.itemState, WinUser.ODS_SELECTED) # 0 THEN
              p^.Canvas^.DrawEdge(lpdis.rcItem,WinUser.EDGE_SUNKEN,WinUser.BF_RECT); 
          END;
      
          IF (SYSTEM.BITAND(lpdis.itemState, WinUser.ODS_FOCUS)#0) THEN
              cx:=lpdis.rcItem.right - lpdis.rcItem.left;
              cy:=lpdis.rcItem.bottom - lpdis.rcItem.top;
              SYSTEM.MOVE(SYSTEM.ADR(lpdis.rcItem),SYSTEM.ADR(rc),16);
              INC(rc.top,cy DIV 8);
              INC(rc.left,cx DIV 16);
              DEC(rc.right,cx DIV 16);
              DEC(rc.bottom,cy DIV 8);
              p^.Canvas^.DrawFocus(rc);      
          END;
      END;
        
      p^.Return(1);
END DrawItem; 

PROCEDURE (p : Button) Command*(wParam,lParam :LONGINT);
VAR
    dummy : LONGINT;  
BEGIN
  IF SYSTEM.HIWORD(wParam)= WinUser.BN_CLICKED THEN
      IF p^.OnClick # NIL THEN
        p^.OnClick;
        p^.Return(0);
      END;
  END;
END Command;

(* ALL BUTTONS *)
PROCEDURE (p : Button)KeyDown*(wParam,lParam :LONGINT);
VAR
  dummy : LONGINT;
BEGIN
     IF ~ SYSTEM.BIT(lParam,30) & (wParam = WinUser.VK_RETURN ) THEN
         dummy := WinUser.SendMessageA(p^.hWnd,WinUser.WM_LBUTTONDOWN,0,0);
     ELSIF (wParam = WinUser.VK_TAB) & (~SYSTEM.BIT(lParam,30)) THEN
         dummy := WinUser.SendMessageA(p^.Owner,Control.WM_NEXTFOCUS,0,0);          
     END;
END KeyDown;

PROCEDURE (p : Button)KeyUp*(wParam,lParam :LONGINT);
VAR
  dummy : LONGINT;
BEGIN
    IF SYSTEM.BIT(lParam,30) & (wParam = WinUser.VK_RETURN ) THEN
        dummy := WinUser.SendMessageA(p^.hWnd,WinUser.WM_LBUTTONUP,0,0);
    END;
END KeyUp; 

PROCEDURE (p : Button) Init*;
VAR
  dummy:LONGINT;
BEGIN
    p^.hWnd    := 0;
    p^.Visible := FALSE;
    p^.Caption := NIL;
    p^.Owner   := 0;
    p^.OnClick := NIL;
END Init;

PROCEDURE (p:Button) Create*(x,y,Width,Height,Owner:LONGINT;Visible:BOOLEAN;Style:SET):BOOLEAN;
VAR
  style,dummy:LONGINT; 
BEGIN
  p^.Visible:=Visible;p^.Owner:=Owner;p^.Style:=Style;
  
  IF T.PushButton IN Style THEN style:= WinUser.BS_PUSHBUTTON END;
  IF T.CheckBox IN Style THEN style:=WinUser.BS_CHECKBOX END;
  IF T.RadioButton IN Style THEN style:=WinUser.BS_RADIOBUTTON END;
  IF T.BitmapButton IN Style THEN style := WinUser.BS_PUSHBUTTON + WinUser.BS_BITMAP END;
  IF T.IconButton IN Style THEN style := WinUser.BS_PUSHBUTTON + WinUser.BS_ICON END;
  IF T.ColourButton IN Style THEN style := WinUser.BS_OWNERDRAW END;
  IF T.Left IN Style THEN style:=style+WinUser.BS_LEFTTEXT END;
  
  p^.hWnd := WinUser.CreateWindowExA(0,
                             SYSTEM.ADR("BUTTON"),
                             0,
                             WinUser.WS_CHILD  + style,
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
    NEW(p^.Canvas);
    ASSERT(p^.Canvas # NIL);
    p^.Canvas^.Init;
    p^.Canvas^.TextColour:=0; 
    WinBase.SetLastError(0);
    p^.NewProc :=Control.HandleSubclassEvent;
    p^.OldProc := SYSTEM.VAL(WinUser.WNDPROC,WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_WNDPROC,SYSTEM.VAL(LONGINT,p^.NewProc)));     
     dummy:=WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_USERDATA,SYSTEM.VAL(LONGINT,p));
      RETURN TRUE;
   END;
   RETURN FALSE;
END Create;

PROCEDURE (p : Button) Destroy*;
VAR
  dummy:LONGINT;
BEGIN
      dummy := WinUser.MessageBoxA(0,SYSTEM.ADR("Button"),SYSTEM.ADR("destroy"),0);  
  IF (p^.OldProc # NIL) & (p^.NewProc # NIL) THEN (* component is subclassed then restore original values*)
    p^.NewProc := SYSTEM.VAL(WinUser.WNDPROC,WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_WNDPROC,SYSTEM.VAL(LONGINT,p^.OldProc)));    
  END;
  p^.Canvas^.Destroy;
  DISPOSE(p^.Canvas);
  DISPOSE(p^.Caption);

END Destroy;


PROCEDURE (p: Button) SetCaption* (txt:ARRAY OF CHAR);
VAR
  dummy:LONGINT;
BEGIN
    DISPOSE(p^.Caption);
    NEW(p^.Caption,Strings.Length(txt)+1);
    COPY(txt,p^.Caption^);
    dummy:=WinUser.SetWindowTextA(p^.hWnd,SYSTEM.ADR(txt));
END SetCaption;

PROCEDURE (p:Button) SetDefaultFont*;
VAR
  dummy : LONGINT;
BEGIN
  p^.Canvas^.SetDefaultFont;
  dummy:=WinUser.SendMessageA(p^.hWnd,WinUser.WM_SETFONT,p^.Canvas.Handle,1);
END SetDefaultFont;


PROCEDURE (p:Button) SetTextColour*(colour:SET);
VAR
BEGIN
  p^.Canvas.SetTextColour(colour);
END SetTextColour;

PROCEDURE (p : Button)CreateBrush*;
VAR
BEGIN
    p^.Canvas^.CreateBrush;
END CreateBrush;
END Button.
