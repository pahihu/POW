(*****************************************************************************)
(*                                                                           *)
(* Project:   Excaliber2                                                     *)
(*                                                                           *)
(* Module:     control                                      V 1.00.00        *)
(*                                                         2000SEP17         *)
(*  PURPOSE:  Base Class for visible  GUI controls                           *)
(*  The functions below must be overloaded by each GUI object                *)
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



MODULE Control;

IMPORT (* no shortcuts *)
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM;

  
CONST

  WM_FOCUSCREATE   *=                  WinUser.WM_USER+1;
  WM_NEXTFOCUS     *=                  WinUser.WM_USER+2;
  WM_PREVIOUSFOCUS *=                  WinUser.WM_USER+3;



TYPE

  Control* =                            POINTER TO ControlDesc;
  ControlDesc* =                        RECORD
    OldProc*,NewProc*:                    WinUser.WNDPROC;
  END;
VAR  
ReturnType,CtrlID :LONGINT;

  

PROCEDURE (p : Control) Init*;
BEGIN
  HALT(0);
END Init;

PROCEDURE (p : Control) Activate*;
VAR
BEGIN
END Activate;

PROCEDURE (p : Control) CaptureChanged*;
VAR
BEGIN
END CaptureChanged;

PROCEDURE (p : Control) CharToItem*;
VAR
BEGIN
END CharToItem;

PROCEDURE (p : Control) ChildActivate*;
VAR
BEGIN
END ChildActivate;

PROCEDURE (p : Control) Close*;
VAR
BEGIN
END Close;

PROCEDURE (p : Control) ContextMenu*;
VAR
BEGIN
END ContextMenu;

PROCEDURE (p : Control) CopyData *;
VAR
BEGIN
END CopyData;

PROCEDURE (p : Control)CtlColourBtn*(hDC,hWnd:LONGINT);
VAR
BEGIN
END CtlColourBtn;

PROCEDURE (p : Control) CtlColourDlg*;
VAR
BEGIN
END CtlColourDlg;

PROCEDURE (p : Control)CtlColourEdit*(hDC,hWnd:LONGINT);
VAR
BEGIN
END CtlColourEdit;

PROCEDURE (p : Control) CtlColourListbox*;
VAR
BEGIN
END CtlColourListbox;

PROCEDURE (p : Control) CtlColourMsgbox*;
VAR
BEGIN
END CtlColourMsgbox;

PROCEDURE (p : Control) CtlColourScrollbar*;
VAR
BEGIN
END CtlColourScrollbar;

PROCEDURE (p : Control) CtlColourStatic*;
VAR
BEGIN
END CtlColourStatic;

PROCEDURE (p : Control) DeleteItem*;
VAR
BEGIN
END DeleteItem;


PROCEDURE (p : Control) CompareItem*;
VAR
BEGIN
END CompareItem;

PROCEDURE (p : Control) Destroy*;
BEGIN
END Destroy;
 
PROCEDURE (p : Control) DisplayChanged*;
VAR
BEGIN
END DisplayChanged;

PROCEDURE (p : Control) DrawItem*(lpdis:WinUser.LPDRAWITEMSTRUCT);
VAR
BEGIN
END DrawItem;

PROCEDURE (p : Control) DropFiles*;
VAR
BEGIN
END DropFiles;

PROCEDURE (p : Control) EndSession*;
VAR
BEGIN
END EndSession;

PROCEDURE (p : Control) GetIcon*;
VAR
BEGIN
END GetIcon;

PROCEDURE (p : Control) GetMinMaxInfo;
VAR
BEGIN
END GetMinMaxInfo;

PROCEDURE (p : Control) Help*;
VAR
BEGIN
END Help;

PROCEDURE (p : Control) HotKey*;
VAR
BEGIN
END HotKey;

PROCEDURE (p : Control) hScroll*;
VAR
BEGIN
END hScroll;

PROCEDURE (p : Control) InitDialog*;
VAR
BEGIN
END InitDialog;

PROCEDURE (p : Control) InitMenu*;
VAR
BEGIN
END InitMenu;

PROCEDURE (p : Control) InitMenuPopup*;
VAR
BEGIN
END InitMenuPopup;

PROCEDURE (p : Control) KeyDown*(Key,Data :LONGINT);
VAR
BEGIN
END KeyDown;

PROCEDURE (p : Control) KeyUp*(Key,Data :LONGINT);
VAR
BEGIN
END KeyUp;

PROCEDURE (p : Control) KillFocus*;
VAR
BEGIN
END KillFocus;

PROCEDURE (p : Control) MdiAcitvate*;
VAR
BEGIN
END MdiAcitvate;

PROCEDURE (p : Control) MdiCascade*;
VAR
BEGIN
END MdiCascade;

PROCEDURE (p : Control) MdiCreate*;
VAR
BEGIN
END MdiCreate;

PROCEDURE (p : Control) MdiDestroy*;
VAR
BEGIN
END MdiDestroy;

PROCEDURE (p : Control) MeasureItem*;
VAR
BEGIN
END MeasureItem;

PROCEDURE (p : Control) MenuChar*;
VAR
BEGIN
END MenuChar;

PROCEDURE (p : Control) MenuSelect*;
VAR
BEGIN
END MenuSelect;

PROCEDURE (p : Control) MouseActivate*;
VAR
BEGIN
END MouseActivate;

PROCEDURE (p : Control) MouseWheel*;
VAR
BEGIN
END MouseWheel;

PROCEDURE (p : Control) Move*;
VAR
BEGIN
END Move;

PROCEDURE (p : Control) Moving*;
VAR
BEGIN
END Moving;

PROCEDURE (p : Control) Notify*;
VAR
BEGIN
END Notify;

PROCEDURE (p : Control) NotifyFormat*;
VAR
BEGIN
END NotifyFormat;

PROCEDURE (p : Control) PaletteChanged*;
VAR
BEGIN
END PaletteChanged;

PROCEDURE (p : Control) PaletteIsChanging*;
VAR
BEGIN
END PaletteIsChanging;

PROCEDURE (p : Control) Paint*;
VAR
BEGIN

END Paint;

PROCEDURE (p : Control) ParentNotify*;
VAR
BEGIN
END ParentNotify;

PROCEDURE (p : Control) QueryEndSession*;
VAR
BEGIN
END QueryEndSession;

PROCEDURE (p : Control) QueryNewPalette*;
VAR
BEGIN
END QueryNewPalette;

PROCEDURE (p : Control) Quit*;
VAR
BEGIN
END Quit;

PROCEDURE (p : Control) SetCursor*;
VAR
BEGIN
END SetCursor;

PROCEDURE (p : Control) SetFont*;
VAR
BEGIN
END SetFont;

PROCEDURE (p : Control) SetFocus*;
VAR
BEGIN
END SetFocus;

PROCEDURE (p : Control) Size*;
VAR
BEGIN
END Size;

PROCEDURE (p : Control) SettingChanged*;
VAR
BEGIN
END SettingChanged;

PROCEDURE (p : Control) SysColourChange*;
VAR
BEGIN
END SysColourChange;

PROCEDURE (p : Control) SysCommand*;
VAR
BEGIN
END SysCommand;

PROCEDURE (p : Control) TimeChange*;
VAR
BEGIN
END TimeChange;

PROCEDURE (p : Control) UserChanged*;
VAR
BEGIN
END UserChanged;

PROCEDURE (p : Control) VkKeyToItem*;
VAR
BEGIN
END VkKeyToItem;

PROCEDURE (p : Control) vScroll*;
VAR
BEGIN
END vScroll;

PROCEDURE (p : Control) Timer*;
VAR
BEGIN
END Timer;

PROCEDURE (p : Control) MouseDown*(keys:SET;x,y :LONGINT);
VAR
BEGIN
END MouseDown;

PROCEDURE (p : Control) MouseUp*(keys:SET;x,y :LONGINT);
VAR
BEGIN
END MouseUp;



PROCEDURE (p : Control) MouseMove*(keys:SET;x,y :LONGINT);
VAR
BEGIN
END MouseMove;



PROCEDURE (p : Control) Char*(ch :CHAR;Data:LONGINT);
VAR
BEGIN
END Char;

PROCEDURE (p : Control) Command*(wParam,lParam :LONGINT);
VAR
BEGIN
END Command;

PROCEDURE (p : Control) WindowPosChanged*;
VAR
BEGIN
END WindowPosChanged;

PROCEDURE (p : Control) WindowPosChanging*;
VAR
BEGIN
END WindowPosChanging;
(* BUTTON COMMANDS *)

PROCEDURE (p : Control) BnClicked*;
VAR
BEGIN
END BnClicked;

(*COMBOBOX COMMANDS*)
PROCEDURE (p : Control) CbCloseUp*;
VAR
BEGIN
END CbCloseUp;
PROCEDURE (p : Control) CbDblClick*;
VAR
BEGIN
END CbDblClick;

PROCEDURE (p : Control) CbDropDown*;
VAR
BEGIN
END CbDropDown;

PROCEDURE (p : Control) CbEditChange*;
VAR
BEGIN
END CbEditChange;

PROCEDURE (p : Control) CbEditUpdate*;
VAR
BEGIN
END CbEditUpdate;

PROCEDURE (p : Control) CbErrSpace*;
VAR
BEGIN
END CbErrSpace;

PROCEDURE (p : Control) CbSelChange*;
VAR
BEGIN
END CbSelChange;

PROCEDURE (p : Control) CbSelEndCancel*;
VAR
BEGIN
END CbSelEndCancel;

PROCEDURE (p : Control) CbSelEndOK*;
VAR
BEGIN
END CbSelEndOK;

(* EDIT CONTROLS *)

PROCEDURE (p : Control) EnChange*;
VAR
BEGIN
END EnChange;

PROCEDURE (p : Control) EnErrSpace*;
VAR
BEGIN
END EnErrSpace;

PROCEDURE (p : Control) EnhScroll*;
VAR
BEGIN
END EnhScroll;

PROCEDURE (p : Control) EnMaxText*;
VAR
BEGIN
END EnMaxText;

PROCEDURE (p : Control) EnUpdate*;
VAR
BEGIN
END EnUpdate;

PROCEDURE (p : Control) EnvScroll*;
VAR
BEGIN
END EnvScroll;

(*LISTBOX CONTOLS *)
PROCEDURE (p : Control) LbDblClick*;
VAR
BEGIN
END LbDblClick;

PROCEDURE (p : Control) LbErrSpace*;
VAR
BEGIN
END LbErrSpace;

PROCEDURE (p : Control) LbSelCancel*;
VAR
BEGIN
END LbSelCancel;

PROCEDURE (p : Control) LbSelChange*;
VAR
BEGIN
END LbSelChange;


PROCEDURE (p : Control) HandleMessage*(VAR hWnd,Message,wParam,lParam :LONGINT);
VAR

  rc:                                 WinDef.RECT;
  h,hDC,dummy:                              LONGINT;
  SubControl :                        Control;
  buttons,key    :                        SET;  
BEGIN
  ReturnType:=2;
  buttons:={};
  key :={};(* to do here process extended keys (alt shift ctrl keys)*)
  
  
    IF Message = WinUser.WM_PAINT THEN
        p^.Paint;
        
    ELSIF  (Message = WinUser.WM_LBUTTONDOWN)
           OR (Message=WinUser.WM_MBUTTONDOWN) OR (Message=WinUser.WM_RBUTTONDOWN)  THEN
      IF SYSTEM.BITAND(wParam,WinUser.MK_LBUTTON)#0 THEN buttons:={0} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_MBUTTON)#0 THEN buttons:=buttons+{1} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_RBUTTON)#0 THEN buttons:=buttons+{2} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_SHIFT)#0 THEN buttons:=buttons+{3} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_CONTROL)#0 THEN buttons:=buttons+{4} END;            
        p^.MouseDown(buttons,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam)); 
        

    ELSIF  (Message = WinUser.WM_LBUTTONUP)
           OR (Message=WinUser.WM_MBUTTONUP) OR (Message=WinUser.WM_RBUTTONUP)  THEN
      IF SYSTEM.BITAND(wParam,WinUser.MK_LBUTTON)#0 THEN buttons:={0} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_MBUTTON)#0 THEN buttons:=buttons+{1} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_RBUTTON)#0 THEN buttons:=buttons+{2} END;
        p^.MouseUp(buttons,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));    


    ELSIF Message = WinUser.WM_MOUSEMOVE THEN
      IF SYSTEM.BITAND(wParam,WinUser.MK_LBUTTON)#0 THEN buttons:={0} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_MBUTTON)#0 THEN buttons:=buttons+{1} END;
      IF SYSTEM.BITAND(wParam,WinUser.MK_RBUTTON)#0 THEN buttons:=buttons+{2} END;      
        p^.MouseMove(buttons,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));  

    ELSIF Message = WinUser.WM_SETFOCUS THEN
     p^.SetFocus;
      
    ELSIF Message = WinUser.WM_KILLFOCUS THEN
        p^.KillFocus;
        
    ELSIF Message = WinUser.WM_KEYDOWN THEN
        p^.KeyDown(wParam,lParam);
        
    ELSIF Message = WinUser.WM_CHAR THEN
        p^.Char(CHR(wParam),lParam);
        
    ELSIF Message = WinUser.WM_KEYUP THEN                    
        p^.KeyUp(wParam,lParam);
    
    ELSIF Message = WinUser.WM_DRAWITEM THEN
        h := WinUser.GetWindowLongA(WinUser.GetDlgItem(hWnd,wParam),WinUser.GWL_USERDATA);
        IF h # 0 THEN
            SubControl := SYSTEM.VAL(Control,h);
            ASSERT(SubControl IS Control);
            SubControl^.DrawItem(SYSTEM.VAL(WinUser.LPDRAWITEMSTRUCT,lParam));
        END;    
    ELSIF Message = WinUser.WM_CTLCOLORBTN THEN (*owner drawn only*)
        h := WinUser.GetWindowLongA(lParam,WinUser.GWL_USERDATA);
        IF h # 0 THEN
            SubControl := SYSTEM.VAL(Control,h);
            ASSERT(SubControl IS Control);
            SubControl^.CtlColourBtn(wParam,lParam);
        END;
    ELSIF Message = WinUser.WM_CTLCOLOREDIT THEN (*owner drawn only*)
        h := WinUser.GetWindowLongA(lParam,WinUser.GWL_USERDATA);
        IF h # 0 THEN
            SubControl := SYSTEM.VAL(Control,h);
            ASSERT(SubControl IS Control);
            SubControl^.CtlColourEdit(wParam,lParam);
        END;                
    ELSIF Message = WinUser.WM_COMMAND THEN
        h := WinUser.GetWindowLongA(lParam,WinUser.GWL_USERDATA);
        IF h # 0 THEN
            SubControl := SYSTEM.VAL(Control,h);
            ASSERT(SubControl IS Control);
            SubControl^.Command(wParam,lParam);
        END;
        
    ELSIF Message = WinUser.WM_DESTROY THEN
        p^.Destroy; 
(*    ELSIF Message = Control.WM_NEXTFOCUS THEN
        dummy := WinUser.SendMessageA(p^.Owner,Control.WM_NEXTFOCUS,0,0); *)
    END;
END HandleMessage;

PROCEDURE [_APICALL] HandleSubclassEvent*(hWnd: WinDef.HWND;
                                      Message: WinDef.UINT;
                                      wParam: WinDef.WPARAM;
                                      lParam: WinDef.LPARAM): LONGINT;
VAR
  p:                                  Control;
  h:                                  LONGINT;
BEGIN

  h := WinUser.GetWindowLongA(hWnd,WinUser.GWL_USERDATA);
  IF h = 0 THEN RETURN WinUser.DefWindowProcA(hWnd, Message, wParam, lParam) END;                                   
  p := SYSTEM.VAL(Control,h);
  ASSERT(p IS Control);

      p^.HandleMessage(hWnd,Message,wParam,lParam);
      RETURN WinUser.CallWindowProcA(p^.OldProc,hWnd,Message,wParam,lParam);
  
END HandleSubclassEvent;


PROCEDURE [_APICALL] HandleEvent*(hWnd: WinDef.HWND;
                                      Message: WinDef.UINT;
                                      wParam: WinDef.WPARAM;
                                      lParam: WinDef.LPARAM): LONGINT;
VAR
  p:                                  Control;
  h:                                  LONGINT;
BEGIN

  h := WinUser.GetWindowLongA(hWnd,WinUser.GWL_USERDATA);
  IF h = 0 THEN RETURN WinUser.DefWindowProcA(hWnd, Message, wParam, lParam) END;                                   
  p := SYSTEM.VAL(Control,h);
  ASSERT(p IS Control);
    p^.HandleMessage(hWnd,Message,wParam,lParam);
    CASE ReturnType OF
      0:RETURN 0;
      |1:RETURN 1;
      |2:RETURN  WinUser.DefWindowProcA(hWnd, Message, wParam, lParam);
    ELSE
    RETURN   ReturnType;
  END;
  
END HandleEvent;

PROCEDURE (p : Control) GetID*():LONGINT;
BEGIN
   INC(CtrlID);
   RETURN CtrlID;
END GetID;
PROCEDURE (p : Control) Return*(type:LONGINT);
VAR
BEGIN
      ReturnType:=type;
END Return;

BEGIN
  CtrlID:=1000;
END Control.
