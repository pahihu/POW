(*****************************************************************************)
(*                                                                           *)
(* Project:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* Module:     UIToolBar                                   V 2.00.42         *)
(*                                                         2003APR23         *)
(*  PURPOSE:   implements a toolbar                                          *)
(*             uses CommCTRL.StatusWindow                                    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Create    generates a ToolBar                                           *)
(*                                                                           *)
(*  COMMENTS:                                                                *)
(*   to enable a button                                                      *)
(*             "SendMessageA (hWndToolBar,                                   *)
(*                            CommCTRL.TB_ENABLEBUTTON,                      *)
(*                            command,           = TBButton.idCommand        *)
(*                            WinDef.True);"                                 *)
(*   to disable a button                                                     *)
(*             "SendMessageA (hWndToolBar,                                   *)
(*                            CommCTRL.TB_ENABLEBUTTON,                      *)
(*                            command,           = TBButton.idCommand        *)
(*                            WinDef.False);"                                *)
(*                                                                           *)
(*                                                                           *)
(* Copyright:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)
(*             e-Mail: schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*                                                                           *)
(*   update                                                                  *)
(*                                                                           *)
(*   release                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE UIToolBar;


IMPORT
  Common, Resource, UIStatusLine,
  Strings,
  CommCTRL, WinBase, WinDef, WinNT, WinUser,
  SYSTEM;
  
  
CONST
  Version*     =                      "V 2.00.42";
  Module*      =                      "UIToolBar";
  
  NumButtons           =              13;
  NumBitmaps           =              12;
  BmpCX                =              16;
  BmpCY                =              16;
  MaxLength            =              64;
  NumLinks             =               7;
  NumMyButton         =               8;
  NumTBBitmaps         =               8;
  MinTBCX              =             200;
  MinComboCX           =             113;
  MinComboCY           =             100;
  MinCY                =              50;

  
TYPE
  TBADDBITMAP  = RECORD [_NOTALIGNED]
    hInst:                             WinDef.HINSTANCE;
    nID:                               LONGINT;
  END (* ADDBITMAP *);
    
  TStringP =                           POINTER TO ARRAY 128 OF CHAR;


VAR
  hWndCheckBox*,
  hWndCheckBox02*,
  hWndComboBox*,
  hWndChild*,
  hWndToolBar*,
  hWndToolTip*:                        WinDef.HWND;

  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  TextString:                          ARRAY 1024 OF CHAR;
  TextStringP:                         POINTER TO ARRAY 128 OF CHAR;
  NumberString:                        ARRAY   32 OF CHAR;
  i, j:                                LONGINT;
  
  (* Image list for combo box *)
  idxFirstImage:                       LONGINT;
  MyButton:                            ARRAY NumButtons OF CommCTRL.TBBUTTON;
  cxButton,
  cyButton:                            LONGINT;
  tbBitmaps:                           TBADDBITMAP;
  MyImageList:                         CommCTRL.HIMAGELIST;
  tbab:                                TBADDBITMAP; 
  HBitmap:                             WinDef.HBITMAP;


PROCEDURE GetButtonInfo               (Command:            LONGINT;
                                       VAR TheIndex:       LONGINT)
                                      :BOOLEAN;
VAR
  Index:                               LONGINT;

BEGIN
  FOR Index:=0 TO NumButtons-1 DO
    IF MyButton[Index].idCommand=Command THEN
      TheIndex := Index;
      RETURN TRUE
    END (* IF MyButton[Index].idCommand=Command  *);
  END (* FOR Index:=0 TO NumButtons-1  *);
  
  RETURN FALSE
END GetButtonInfo;


(*****************************************************************************)
(*                                                                           *)
(* CreateComboBox                                                            *)
(* creates a combobox as child of the window hWndParent                      *)
(*                                                                           *)
(* Input:                                                                    *)
(*   hWndParent                                                              *)
(*             window handle of parent window                                *)
(*   hInstanceParent                                                         *)
(*             instance handle of the module that wants to create a          *)
(*             status bar.                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE CreateComboBox              (hWndParent:         WinDef.HWND;
                                       hInstanceParent:    WinDef.HWND);
                                      
VAR
  Error:                               WinDef.DWORD;
  Width:                               LONGINT;
  Number:                              ARRAY 80 OF CHAR;
                                       
BEGIN
  
  hWndComboBox     := WinUser.CreateWindowExA(0,
                               SYSTEM.ADR("ComboBoxEx32"),    (* See RegisterClass() call.          *)
                               WinDef.NULL,                (* no Text for Window title bar.      *)
                                                           (* Window style.                      *)
                               WinUser.WS_CHILD + WinUser.WS_VISIBLE + WinUser.CBS_DROPDOWN,
                               0,
                               0,
                               0,
                               MinComboCY,
                               hWndParent,
                               WinDef.NULL,                (* Use the Window class menu.         *)
                               hInstanceParent,
                               0);

  IF hWndComboBox=0 THEN                                    (* an error occurred *)
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
    RETURN
  END;

  Result       := WinUser.SetWindowPos(hWndComboBox, WinDef.NULL, 300, 2, 150, 30, WinUser.SWP_NOACTIVATE);
      
END CreateComboBox;


(*****************************************************************************)
(*                                                                           *)
(* CreateToolTip                                                             *)
(* creates a combobox as child of the window hWndParent                      *)
(*                                                                           *)
(* Input:                                                                    *)
(*   hWndParent                                                              *)
(*             window handle of parent window                                *)
(*   hInstanceParent                                                         *)
(*             instance handle of the module that wants to create a          *)
(*             status bar.                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE CreateToolTip               (hWndParent:         WinDef.HWND;
                                       hInstanceParent:    WinDef.HWND);
                                      
VAR
  Column:                              LONGINT;
  Error:                               WinDef.DWORD;
  Left,
  Width:                               LONGINT;
  Number:                              ARRAY 80 OF CHAR;
  ToolInfo:                            CommCTRL.TOOLINFOA;
                                       
BEGIN
  
  hWndToolTip      := WinUser.CreateWindowExA(WinUser.WS_EX_TOPMOST,
                               SYSTEM.ADR("tooltips_class32"),    (* See RegisterClass() call.          *)
                               WinDef.NULL,                (* no Text for Window title bar.      *)
                                                           (* Window style.                      *)
                               WinUser.WS_POPUP + CommCTRL.TTS_ALWAYSTIP,
                               0,
                               0,
                               0,
                               0,
                               WinDef.NULL,
                               WinDef.NULL,                (* Use the Window class menu.         *)
                               hInstanceParent,
                               0);

  IF hWndToolTip=0 THEN                                    (* an error occurred *)
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
    RETURN
  END;

  (* Divide the client area into a grid of rectangles, and add each *)
  (* rectangle to the tooltip.                                      *)
  ToolInfo.cbSize      := SIZE(CommCTRL.TOOLINFOA); 
  ToolInfo.uFlags      := CommCTRL.TTF_SUBCLASS; 
  ToolInfo.hwnd        := hWndParent; 
  ToolInfo.hinst       := hInstanceParent;
  Left                 :=  0;
  FOR Column:=0 TO NumButtons-1 DO;
    IF MyButton[Column].fsStyle=SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_SEP) THEN
      Width := cxButton DIV 4 + 2;
    ELSE
      Width := cxButton;
    END (* IF MyButton[Column].fsStyle=SYSTEM.VAL(CHAR, CommCTRL...) *);
    ToolInfo.uId         := Column;
    ToolInfo.lpszText    := MyButton[Column].iString; 
    ToolInfo.rect.left   := Left + 2;
    Left                 := Left + Width;
    ToolInfo.rect.top    :=  0; 
    ToolInfo.rect.right  := ToolInfo.rect.left + Width - 2;
    ToolInfo.rect.bottom := ToolInfo.rect.top  + cyButton;
 
    Result               := WinUser.SendMessageA(hWndToolTip, 
                                                 CommCTRL.TTM_ADDTOOL, 
                                                 0, 
                                                 SYSTEM.ADR(ToolInfo));
  END (* FOR Col:=0 TO NumButtons-1 *);

  Result               := WinUser.SendMessageA(hWndToolTip, 
                                         CommCTRL.TTM_ACTIVATE, 
                                         WinDef.True, 
                                         0);

END CreateToolTip;


(*****************************************************************************)
(*                                                                           *)
(* Create                                                                    *)
(* creates a toolbox as child of the window hWndParent                       *)
(*                                                                           *)
(* Input:                                                                    *)
(*   hWndParent                                                              *)
(*             window handle of parent window                                *)
(*   hInstanceParent                                                         *)
(*             instance handle of the module that wants to create a          *)
(*             status bar.                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Create*                     (hWndParent:         WinDef.HWND;
                                       hInstanceParent:    WinDef.HWND);
                                      
VAR
  Error:                               WinDef.DWORD;
  Width:                               LONGINT;
  Number:                              ARRAY 80 OF CHAR;
                                       
BEGIN
  
  hWndToolBar      := WinUser.CreateWindowExA(0,
                               SYSTEM.ADR("ToolbarWindow32"),    (* See RegisterClass() call.          *)
                               WinDef.NULL,                (* no Text for Window title bar.      *)
                                                           (* Window style.                      *)
                               WinUser.WS_CHILD + WinUser.WS_VISIBLE + CommCTRL.TBSTYLE_TOOLTIPS,
                               WinUser.CW_USEDEFAULT,
                               WinUser.CW_USEDEFAULT,
                               WinUser.CW_USEDEFAULT,
                               WinUser.CW_USEDEFAULT,
                               hWndParent,
                               WinDef.NULL,                (* Use the Window class menu.         *)
                               hInstanceParent,
                               0);

  IF hWndToolBar=0 THEN                                    (* an error occurred *)
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
    RETURN
  END;

  (* Send the TB_BUTTONSTRUCTSIZE message, which is required for *)
  (* backward compatibility.                                     *)
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_BUTTONSTRUCTSIZE, SIZE(CommCTRL.TBBUTTON), 0); 

  (* Generate ImageList *)
  MyImageList  := CommCTRL.ImageList_Create (BmpCX,
                                             BmpCY,
                                             CommCTRL.ILC_COLOR32 + CommCTRL.ILC_MASK,
                                             NumBitmaps,
                                             12);
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_SETIMAGELIST, 0, SYSTEM.ADR(MyImageList^)); 
  
  (* Add the bitmaps containing standard button images to the toolbar. *)
  tbab.hInst   := -1;                                      (* INDEX  0.. 14 *)
  tbab.nID     :=  2;
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_ADDBITMAP, 15, SYSTEM.ADR(tbab)); 
  
  (* Add the bitmap containing view button images to the toolbar. *)
  tbab.hInst   := -1;                                      (* INDEX 15.. 26 *)
  tbab.nID     :=  4;
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_ADDBITMAP, 13, SYSTEM.ADR(tbab)); 

  (* Add the bitmap containing history button images to the toolbar. *)
  tbab.hInst   := -1;                                      (* INDEX 27.. 31 *)
  tbab.nID     :=  8;
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_ADDBITMAP,  5, SYSTEM.ADR(tbab));

  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_ADDBUTTONS, NumButtons, SYSTEM.ADR(MyButton)); 
  Result       := WinUser.SendMessageA(hWndToolBar, CommCTRL.TB_GETBUTTONSIZE, 0, 0);
  cyButton     := SYSTEM.HIWORD(Result);
  cxButton     := SYSTEM.LOWORD(Result);

  CreateToolTip(hWndToolBar, hInstanceParent);

END Create;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  cxButton                     :=  0;
  cyButton                     :=  0;
  hWndToolTip                  := WinDef.NULL;
  
  MyButton[00].iBitmap         :=  0;
  MyButton[00].idCommand       :=  0;
  MyButton[00].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[00].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_SEP);
  MyButton[00].dwData          :=  0;
  MyButton[00].iString         :=  0;
 
  MyButton[01].iBitmap         :=  7;
  MyButton[01].idCommand       := Resource.IDM_File_Open;
  MyButton[01].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[01].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[01].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[01].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Open,
                                 MyButton[01].iString,
                                 128);
 
  MyButton[02].iBitmap         :=  5;
  MyButton[02].idCommand       := Resource.IDM_File_Close;
  MyButton[02].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[02].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[02].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[02].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Close,
                                 MyButton[02].iString,
                                 128);
 
  MyButton[03].iBitmap         :=  0;
  MyButton[03].idCommand       :=  0;
  MyButton[03].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[03].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_SEP);
  MyButton[03].dwData          :=  0;
  MyButton[03].iString         :=  0;

  MyButton[04].iBitmap         :=  6;
  MyButton[04].idCommand       := Resource.IDM_File_Start;
  MyButton[04].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[04].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[04].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[04].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Start,
                                 MyButton[04].iString,
                                 128);

  MyButton[05].iBitmap         := 10;
  MyButton[05].idCommand       := Resource.IDM_File_Suspend;
  MyButton[05].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[05].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[05].dwData          := 0;
  NEW(SYSTEM.VAL(TStringP, MyButton[05].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Suspend,
                                 MyButton[05].iString,
                                 128);
 
  MyButton[06].iBitmap         :=  3;
  MyButton[06].idCommand       := Resource.IDM_File_Resume;
  MyButton[06].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[06].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[06].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[06].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Resume,
                                 MyButton[06].iString,
                                 128);
 
  MyButton[07].iBitmap         :=  5;
  MyButton[07].idCommand       := Resource.IDM_File_Stop;
  MyButton[07].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[07].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[07].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[07].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Stop,
                                 MyButton[07].iString,
                                 128);
 
  MyButton[08].iBitmap         :=  0;
  MyButton[08].idCommand       :=  0;
  MyButton[08].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[08].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_SEP);
  MyButton[08].dwData          :=  0;
  MyButton[08].iString         :=  0;
 
  MyButton[09].iBitmap         := 25;
  MyButton[09].idCommand       := Resource.IDM_File_Print;
  MyButton[09].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[09].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_CHECK);
  MyButton[09].dwData          :=  0;
  NEW(SYSTEM.VAL(TStringP, MyButton[09].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Print,
                                 MyButton[09].iString,
                                 128);
 
  MyButton[10].iBitmap         :=  0;
  MyButton[10].idCommand       :=  0;
  MyButton[10].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[10].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_SEP);
  MyButton[10].dwData          :=  0;
  MyButton[10].iString         :=  0;
 
  MyButton[11].iBitmap         := 5;
  MyButton[11].idCommand       := Resource.IDM_File_Exit;
  MyButton[11].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[11].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_BUTTON);
  MyButton[11].dwData          := 0;
  NEW(SYSTEM.VAL(TStringP, MyButton[11].iString));
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 Resource.IDM_File_Exit,
                                 MyButton[11].iString,
                                 128);
 
  MyButton[12].iBitmap         := 27;
  MyButton[12].idCommand       :=  0;
  MyButton[12].fsState         := SYSTEM.VAL(CHAR, CommCTRL.TBSTATE_ENABLED);
  MyButton[12].fsStyle         := SYSTEM.VAL(CHAR, CommCTRL.TBSTYLE_CHECK);
  MyButton[12].dwData          :=  0;
  MyButton[12].iString         :=  0;
 
END UIToolBar.

