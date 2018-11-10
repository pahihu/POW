(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     main                                         V 1.00.01        *)
(*                                                         2001 Nov 17       *)
(*  PURPOSE:  Class for Main Window                                          *)
(*  The functions below must be overloaded by the Created Window Class       *)
(*  Init();                                                                  *)
(*  HandleMessage(hWnd,Message,wParam,lParam):longint;                       *)
(*  Destroy();                                                               *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*  Init();                                                                  *)
(*  HandleMessage(hWnd,Message,wParam,lParam):longint;                       *)
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



MODULE MainWindow;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,WinUtils,globhandles,
  SYSTEM,Control,Strings,TList,Process,User;

  
CONST
  Version =                           "V 1.00.01";

TYPE
  PCHAR =                            POINTER TO ARRAY OF CHAR;
  WindowTab =                      POINTER TO WindowTabDesc;
  WindowTabDesc =                  RECORD (User.DATA)
  END;

  MainWindow* =                      POINTER TO MainWindowDesc;
  MainWindowDesc* =                  RECORD(Control.ControlDesc)
    ChildFocus *:                    WindowTab;
    Destroyed*:                      BOOLEAN;
    Focus    :                       WinDef.HWND;
    hWnd-    :                       WinDef.HWND;
    x-,y-    :                       LONGINT;
    Width-   :                       LONGINT;
    Height-  :                       LONGINT;
    Style-   :                       LONGINT; (*SET ?*)
    Brush-   :                       LONGINT;
    Icon-    :                       LONGINT;
    Caption- :                       PCHAR;(*DEFINE AN OBERON2 STRING*)
    Class -  :                       PCHAR;
    Menu -   :                       LONGINT;
    Visible* :                       BOOLEAN;
  END;

PROCEDURE (p:MainWindow) RegisterClass*():BOOLEAN;
VAR
  wc:                                 WinUser.WNDCLASS;
  done:                               LONGINT;

BEGIN
  IF WinUtils.IsClassRegistered(globhandles.GetAppInstanceHandle(),p^.Class^) THEN
    RETURN TRUE;
  END;
  wc.style := 0;    
  wc.lpfnWndProc := Control.HandleEvent;
  wc.cbClsExtra := 0;                   
  wc.cbWndExtra := 0;
  wc.hInstance := globhandles.GetAppInstanceHandle();            
  wc.hIcon :=0; 
  wc.hCursor := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  wc.hbrBackground :=WinGDI.GetStockObject(WinGDI.WHITE_BRUSH);
  wc.lpszMenuName := 0;
  wc.lpszClassName := SYSTEM.ADR(p^.Class^);
  RETURN  WinUser.RegisterClassA(wc) #0;
END RegisterClass;


PROCEDURE (p:MainWindow) Create*(x,y,Width,Height,Style,Brush:LONGINT;Visible:BOOLEAN):BOOLEAN;
VAR
  dummy:LONGINT;
BEGIN
  p^.x:=x;p^.y:=y;p^.Width:=Width;p^.Height:=Height;p^.Style:=Style;p^.Visible:=Visible;
  ASSERT(LEN(p^.Class^)>1);
  IF p.RegisterClass() THEN

      p^.hWnd := WinUser.CreateWindowExA(0,
                             SYSTEM.ADR(p^.Class^),
                             SYSTEM.ADR(p^.Caption),
                             p^.Style,
                             p^.x,         
                             p^.y,         
                             p^.Width,p^.Height,
                             WinDef.NULL,                  
                             WinDef.NULL,                  
                             globhandles.GetAppInstanceHandle(),             
                             WinDef.NULL);
       IF p^.hWnd # 0 THEN
           IF Brush # 0 THEN
                dummy := WinUser.SetClassLongA(p^.hWnd,WinUser.GCL_HBRBACKGROUND,Brush);
                p^.Brush:=Brush;
           END;

         WinBase.SetLastError(0);
         dummy:=WinUser.SetWindowLongA(p^.hWnd,WinUser.GWL_USERDATA,SYSTEM.VAL(LONGINT,p));
             IF (dummy=0) & ((WinBase.GetLastError())=0) &(p^.Visible)THEN
                 dummy:=WinUser.ShowWindow(p^.hWnd,WinUser.SW_SHOW);
                 (* validate the new background brush *)
                 dummy:=WinUser.InvalidateRect(p^.hWnd,NIL,1);
                 dummy:=WinUser.UpdateWindow(p^.hWnd);
             END;
         RETURN TRUE;
       END;
  END;
   RETURN FALSE;
END Create;
                             
PROCEDURE (p:MainWindow) Init*();
VAR
BEGIN

    NEW(p^.Caption,1);
    NEW(p^.Class,1);
    p^.Brush:=0;
    p^.x :=0;
    p^.y :=0;
    p^.Width:=0;
    p^.Height :=0;
    p^.Style :=0;
    p^.Brush :=0;
    p^.Icon :=0;
    p^.Menu :=0;
    p^.Visible :=FALSE;
    p^.Destroyed := FALSE;
    NEW(p^.ChildFocus); 
END Init;

PROCEDURE (p : MainWindow) SetClass*(txt:ARRAY OF CHAR);
BEGIN
    DISPOSE(p^.Class);
    NEW(p^.Class,Strings.Length(txt)+1);
    COPY(txt,p^.Class^);
END SetClass;
PROCEDURE (p : MainWindow) Run*;
VAR
BEGIN
          Process.Yield;
END Run;
(*PROCEDURE (p : MainWindow) Return*(type:LONGINT);
VAR
BEGIN
  Control.ReturnType:=type;
END Return;*)

PROCEDURE (p : MainWindow) Destroy*;
VAR
  dummy:LONGINT;
BEGIN
    p^.Destroyed :=TRUE;
    DISPOSE(p^.ChildFocus); 
    DISPOSE(p^.Caption);
    DISPOSE(p^.Class);
END Destroy;



PROCEDURE (p: MainWindow) SetCaption* (txt:ARRAY OF CHAR);
VAR
  dummy:LONGINT;
BEGIN
    DISPOSE(p^.Caption);
    NEW(p^.Caption,Strings.Length(txt)+1);
    COPY(txt,p^.Caption^);
    dummy:=WinUser.SetWindowTextA(p^.hWnd,SYSTEM.ADR(txt));
END SetCaption;

PROCEDURE (p: MainWindow) SetIcon(icon:LONGINT);
VAR
BEGIN
END SetIcon;

(*PROCEDURE (p: MainWindow) SetCursor*(cursor:LONGINT);
VAR
BEGIN
END SetCursor;*)


END MainWindow.
