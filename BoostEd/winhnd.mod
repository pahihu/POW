(*****************************************************************************)
(*  Module WinHnd                                                            *)
(*                                                                           *)
(*  This module implements the Windows window class which is used            *)
(*  for editor windows. This includes registration and deletion of           *)
(*  the window class and the call-back procedure to receive Windows          *)
(*  messages.                                                                *)
(*****************************************************************************)

MODULE WinHnd;


IMPORT
  SYSTEM,
  WinBase, WinUser, WinDef, WinGDI,
  Strings, 
  ListSt, EditWin, Syntax, TextWin, GlobWin, Options;


CONST 
  IDM_EDIT     =                       0CACH;
  CLASSNAME    =                      "BoostEdEditor";
  SELECTTIMER  =                          1;
  MAXWIN       =                        100;

VAR
  hFont*:                              WinDef.HFONT; 
  hcrIBeam,
  hcrArrow,
  hcrWait:                             WinDef.HCURSOR;         (* verschiedene Cursortypen *)
  langHelpFile*:                       ARRAY 150 OF CHAR;
  wCounter-:                           INTEGER; (* Anzahl der offenen Fenster *)
  wList-:                              ARRAY MAXWIN OF EditWin.EditWin;

  

(*****************************************************************************)

PROCEDURE CtrlPressed*():BOOLEAN;
BEGIN
  RETURN WinUser.GetKeyState(WinUser.VK_CONTROL)<0;
END CtrlPressed;


(***********************************************************************************************)   
PROCEDURE ShiftPressed*():BOOLEAN;
BEGIN
  RETURN WinUser.GetKeyState(WinUser.VK_SHIFT)<0;
END ShiftPressed;


(**********************************************************************************************)
PROCEDURE SetWindowOldFont*(wndInx:LONGINT; font:WinDef.HFONT);
BEGIN
  wList[wndInx].oldFont:=font;
END SetWindowOldFont;


(**********************************************************************************************)
PROCEDURE NewEditWindow*(parent : WinDef.HWND; 
                         readOnly : BOOLEAN);
(* legt ein neues Fenster zur Texteingabe an *)

VAR
  rect     : WinDef.RECT;
  hEdit    : WinDef.HWND;
  win      : EditWin.EditWin;
  done     : WinDef.BOOL;

BEGIN
  done := WinUser.GetClientRect(parent,rect);
  hEdit:=WinUser.CreateWindowExA(WinDef.NULL,
                            SYSTEM.ADR(CLASSNAME),
                            WinDef.NULL,
                            WinUser.WS_CHILD+WinUser.WS_VISIBLE+WinUser.WS_VSCROLL+WinUser.WS_HSCROLL+WinUser.WS_CLIPCHILDREN,
                            0,0,
                            rect.right,rect.bottom,
                            parent,
                            IDM_EDIT,
                            GlobWin.hInstance,WinDef.NULL);
  IF (hEdit=0) THEN 
    GlobWin.Beep;
    RETURN 
  ELSE
    win:=EditWin.AssocWinObj(hEdit);
    win.readOnly:=readOnly;
  END;
END NewEditWindow;


(*****************************************************************************)
PROCEDURE AddText*(win:TextWin.WinDesc; text:WinDef.LPSTR):INTEGER;
(* hängt einen Text an den bestehenden Text an *)
(* Rückgabewert : 1 (erfolgreich), 0 (Fehler)  *)

VAR 
  saverow, savecol, len : LONGINT;
  done                  : BOOLEAN;

BEGIN
  saverow:=win.row;
  savecol:=win.col;
  done:=win.text.GetLineLength(win.text.lines,len);
  win.row:=win.text.lines;
  win.col:=len+1;
  done:=win.InsertText(text);
  win.row:=saverow;
  win.col:=savecol;
  win.SetCaret;
  IF done THEN RETURN 1 ELSE RETURN 0 END;
END AddText;

(*****************************************************************************)

PROCEDURE Copy*(hEdit:WinDef.HWND):INTEGER;
(* Kopiert den ausgewählten Text in die Zwischenablage              *)
(* Rückgabewert : 1 (erfolgreich), 0 (nichts ausgewählt oder Fehler *)

VAR
  r            : LONGINT;
  hCopyData    : WinDef.HANDLE;
  win          : EditWin.EditWin;

BEGIN
  win:=EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN 
    GlobWin.Beep;
    RETURN 0; 
  END;
  win.SetUndoAction(TextWin.ACT_NONE);
  IF WinUser.OpenClipboard(win.hwnd) = 0 THEN RETURN 0 END;
  IF win.SelectionToGlobMem(hCopyData) THEN
    r:=WinUser.EmptyClipboard();
    r:=WinUser.SetClipboardData(WinUser.CF_TEXT, hCopyData);
    r:=WinUser.CloseClipboard();
    RETURN 1;
  ELSE
    GlobWin.Beep;
    r:=WinUser.CloseClipboard();
    RETURN 0;
  END;
END Copy;

(*****************************************************************************)

PROCEDURE Paste*(hEdit:WinDef.HWND):INTEGER;
(* Fügt den Inhalt der Zwischenablage an der aktuellen Cursorposition ein *)
(* Rückgabewert : 1 (erfolgreich), 0 (kein Text in der Zwischenablage)    *)

VAR
  r                : LONGINT;
  hCopyData        : WinDef.HANDLE;
  lpCopy           : LONGINT;
  len,n            : LONGINT;
  win              : EditWin.EditWin;
  done             : BOOLEAN;
  reslt            : WinDef.LRESULT;
  dmyi             : LONGINT;

BEGIN
  win:=EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN 
    GlobWin.Beep;
    RETURN 0;
  END;
  IF WinUser.OpenClipboard(hEdit) = 0 THEN RETURN 0 END;
  hCopyData := WinUser.GetClipboardData(WinUser.CF_TEXT);    
  IF hCopyData = WinDef.NULL THEN r := WinUser.CloseClipboard();
    GlobWin.Beep;
    RETURN 0;
  END;
  win.SetUndoAction(TextWin.ACT_PASTE);
  IF win.text.isSelected THEN
    win.undoRow:=win.text.markStart.row;
    win.undoCol:=win.text.markStart.col;
    done:=win.SelectionToGlobMem(win.undoData);
    IF ~done THEN GlobWin.Beep END;
    done:=win.CutSelectionFromScreen();
  END;
  done:=win.InsertGlobMem(hCopyData);
  win.undoToRow:=win.row;
  win.undoToCol:=win.col;
  r := WinUser.CloseClipboard();
  IF ~done THEN 
    GlobWin.Beep;
    RETURN 0;
  END;
  win.changed:= TRUE;
  (* Nachricht senden *)
  reslt:=WinUser.SendMessageA(WinUser.GetParent(hEdit),ListSt.PEM_SHOWCHANGED,1,0); 
  win.UpdateVerScrollBar;
  win.ShowTextRange(win.undoRow,win.text.lines);
  RETURN 1; 
END Paste;

(*****************************************************************************)

PROCEDURE Cut*(hEdit:WinDef.HWND):INTEGER;
(* schneidet den ausgewählten Text aus und überträgt ihn in die Zwischenablage *)
(* Rückgabewert : 1 (erfolgreich), 0 (nichts ausgewählt oder Fehler            *)

VAR
  r         : LONGINT;
  win       : EditWin.EditWin;
  hCopyData : WinDef.HANDLE;
  done      : BOOLEAN;

BEGIN
  win:=EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN 
    GlobWin.Beep;
    RETURN 0;
  END;
  win.SetUndoAction(TextWin.ACT_CUT);
  win.undoRow:=win.text.markStart.row;
  win.undoCol:=win.text.markStart.col;
  done:=win.SelectionToGlobMem(win.undoData);
  IF ~done THEN 
    GlobWin.Beep;
    RETURN 0;
  END;
  IF WinUser.OpenClipboard(win.hwnd) = 0 THEN RETURN 0 END;
  IF win.SelectionToGlobMem(hCopyData) THEN
    r:=WinUser.EmptyClipboard();
    r:=WinUser.SetClipboardData(WinUser.CF_TEXT,hCopyData);
  ELSE
    r:=WinUser.CloseClipboard();
    GlobWin.Beep;
    RETURN 0;
  END;
  r:=WinUser.CloseClipboard();
  IF win.CutSelectionFromScreen() THEN RETURN 1 ELSE RETURN 0 END;
END Cut;


PROCEDURE MousePos2RowCol(win:TextWin.WinDesc; mx,my:LONGINT; VAR row,col,col2:LONGINT);
(* col ist eine gültige Zeilenposition im bestehenden Text und col2 ist die *)
(* Zeilenposition für die Mausposition                                      *)

VAR
  done   : BOOLEAN;
  len    : LONGINT;

BEGIN
  IF win.text.lines=0 THEN row:=-1; col:=1; col2:=1; RETURN END;
  col2:=(mx+win.charwidth DIV 2) DIV win.charwidth + win.colPos;
  row:=my DIV win.lineheight + win.textPos;
  IF row<1 THEN row:=1
  ELSIF row>win.text.lines THEN row:=win.text.lines
  END;
  done:=win.text.GetLineLength(row,len);
  ASSERT(done);
  IF col2<1 THEN col2:=1
  ELSIF col2>ListSt.MAXLENGTH THEN col2:=ListSt.MAXLENGTH END;
  col:=col2;
  IF col>len THEN col:=len+1 END;
END MousePos2RowCol;

(***********************************************************************************************)   

PROCEDURE CreateCaret(win:TextWin.WinDesc);
(* Caret erzeugen *)
VAR
  hi  : LONGINT;
  done: WinDef.BOOL;

BEGIN
(*  IF win.readOnly THEN RETURN END; *)
  IF Options.insert THEN hi:=2 ELSE hi:=win.charwidth END;
  done := WinUser.CreateCaret(win.hwnd,WinDef.NULL,hi,win.textHeight); (* Caret erzeugen *)
  IF ~win.text.isSelected THEN 
  done := WinUser.ShowCaret(win.hwnd); (* Caret anzeigen *)
  END;
  win.SetCaret;
END CreateCaret;

(***********************************************************************************************)   

PROCEDURE DestroyCaret(win:TextWin.WinDesc);
(* Caret löschen *)
VAR 
  done : WinDef.BOOL;

BEGIN
(*  IF win.readOnly THEN RETURN END; *)
  IF ~win.text.isSelected THEN 
  done := WinUser.HideCaret(win.hwnd);
  END;
  done := WinUser.DestroyCaret(); 
END DestroyCaret;

(***********************************************************************************************)   

PROCEDURE SelectWord(win:TextWin.WinDesc);
(* Wort selektieren *)
VAR
  len,pos  : LONGINT;
  txt      : ARRAY ListSt.MAXLENGTH+1 OF CHAR;

BEGIN
  IF ~win.text.GetLine(win.row,txt,len) THEN 
    GlobWin.Beep;
    RETURN;
  END;
  IF win.text.isSelected THEN
    win.text.InvalidateMarkArea;
    win.ShowTextRange(win.text.markStart.row,win.text.markEnd.row);
  END;
  pos:=win.col-2;
  IF pos>len THEN pos:=len-1 END;
  WHILE (pos>=0) & ~Syntax.IsIdentChar(txt[pos]) DO DEC(pos) END;
  WHILE (pos>=0) & Syntax.IsIdentChar(txt[pos]) DO DEC(pos) END;
  INC(pos);
  win.text.markStart.row:=win.row;
  win.text.markStart.col:=pos+1;
  win.markDown:=TRUE;
  WHILE (pos<len) & Syntax.IsIdentChar(txt[pos]) DO INC(pos) END;
  win.MarkUpdate(win.row,pos+1);
  win.col:=pos+1;
END SelectWord;

(***********************************************************************************************)   

PROCEDURE SelectByMouse(win:TextWin.WinDesc);
(* Selektieren mit Maus *)

VAR
  len,row,col,col2 : LONGINT;
  done             : BOOLEAN;

BEGIN
  MousePos2RowCol(win,win.mouseX,win.mouseY,row,col,col2);
  IF row<win.textPos THEN win.VerScroll(row-win.textPos)
  ELSIF row>win.textPos+win.lineNo-1 THEN win.VerScroll(row-(win.textPos+win.lineNo-1)) END;
  done:=win.text.GetLineLength(row,len);
  IF ~done THEN RETURN END;
  IF col>len+1 THEN col:=len+1 END;
  win.MarkUpdate(row,col);
  IF win.markDown THEN
    win.row:=win.text.markEnd.row;
    win.col:=win.text.markEnd.col;
  ELSE
    win.row:=win.text.markStart.row;
    win.col:=win.text.markStart.col;
  END;
  win.CheckHorzScrollPos;
END SelectByMouse;

(***********************************************************************************************)   

PROCEDURE MsgRightButtonDown(win:TextWin.WinDesc; x,y:LONGINT);
(* Nachrichtenbehandlung für rechte Maustaste gedrückt *)

VAR
  i1,i2,row,col,col2,len  : LONGINT;
  txt                     : ARRAY ListSt.MAXLENGTH OF CHAR;
  ident                   : ARRAY 40 OF CHAR;
  res                     : WinDef.BOOL;
  beepOk                  : WinDef.BOOL;

BEGIN
  IF ~Options.mouse THEN RETURN END;
  MousePos2RowCol(win,x,y,row,col,col2);
  IF ~win.text.GetLine(row,txt,len) THEN GlobWin.Beep() END;
  i1:=col-1;
  WHILE (i1>0) & Syntax.IsIdentChar(txt[i1-1]) DO DEC(i1) END;
  i2:=col-1;
  WHILE (i2<len-1) & Syntax.IsIdentChar(txt[i2+1]) DO INC(i2) END;
  Strings.Copy(txt,ident,i1+1,i2-i1+1);
  res:=WinUser.WinHelpA(win.hwnd,
                   SYSTEM.ADR(langHelpFile),
                   WinUser.HELP_PARTIALKEY,
                   SYSTEM.ADR(ident));
END MsgRightButtonDown;

(***********************************************************************************************)   

PROCEDURE MsgLeftButtonDown(win:TextWin.WinDesc; x,y:LONGINT);
(* Nachrichtenbehandlung für linke Maustaste gedrückt *)

VAR
  len,row,col,col2  : LONGINT;
  done              : BOOLEAN;
  oldhwnd           : WinDef.HWND;
  res               : LONGINT;
  ok                : WinDef.BOOL;

BEGIN
  oldhwnd:=WinUser.SetFocus(win.hwnd); 
  IF win.text.isSelected THEN 
    win.text.ResetMarkArea;
    ok := WinUser.InvalidateRect(win.hwnd, NIL, 0);
  END;
  win.mouseX:=x;
  win.mouseY:=y;
  MousePos2RowCol(win,x,y,row,col,col2);
  IF row=-1 THEN RETURN END;
  done:=win.text.GetLineLength(row,len);
  IF ~done THEN RETURN END;
  IF col>len+1 THEN col:=len+1 END;
  win.text.SetMarkArea(row,col,row,col);
  win.markDown:=TRUE;
  win.row:=row;
  win.col:=col;
  win.SetCaret;
  oldhwnd:=WinUser.SetCapture(win.hwnd);   (* alle Mausnachrichten erhalten *)
  win.MouseCapture:=TRUE;
  win.MarkProcess:=TRUE;
  (*res:=WinUser.SetTimer(win.hwnd,SELECTTIMER,125,WinDef.NULL);*)
END MsgLeftButtonDown;

(***********************************************************************************************)   

PROCEDURE MsgMouseMove(win:TextWin.WinDesc; x,y:LONGINT);
(* Nachrichtenbehandlung für Mausbewegung *)

VAR
  len,row,col : LONGINT;
  done        : BOOLEAN;

BEGIN
  win.mouseX:=x;
  win.mouseY:=y;
  IF ~win.MouseCapture THEN RETURN END;
  IF win.MarkProcess & (x>=0) & (y>=0) & (x<win.wndwidth) & (y<win.wndheight) THEN
    SelectByMouse(win);
  END;
END MsgMouseMove;

(***********************************************************************************************)   

PROCEDURE MsgLeftDoubleClick(win:TextWin.WinDesc; x,y:LONGINT);
(* Nachrichtenbehandlung für linken Maustastendoppelklick *)

VAR
  row,col,col2 : LONGINT;

BEGIN
  SelectWord(win);
END MsgLeftDoubleClick;

(***********************************************************************************************)   

PROCEDURE MsgLeftButtonUp(win:TextWin.WinDesc; x,y:LONGINT);
(* Nachrichtenbehandlung für linke Maustaste losgelassen *)

VAR
  res,row,col,col2 : LONGINT;
  swap             : BOOLEAN;
  done             : WinDef.BOOL;

BEGIN
  IF ~win.MouseCapture THEN RETURN END;
  res:=WinUser.KillTimer(win.hwnd,SELECTTIMER);
  done := WinUser.ReleaseCapture();
  win.MouseCapture:=FALSE;
  IF win.MarkProcess THEN   (* end of markprocess *)
    IF (win.text.markStart.row=win.text.markEnd.row) & 
       (win.text.markStart.col=win.text.markEnd.col) THEN 
      win.text.ResetMarkArea; 
      MousePos2RowCol(win,win.mouseX,win.mouseY,row,col,col2);
      win.row:=row;
      win.col:=col2;
      win.SetCaret;
    ELSE
      win.text.CheckMarkRange(swap); 
      win.MarkProcess:=FALSE;                        
    END;
  END;
  (* Nachricht senden *)
  res:=WinUser.SendMessageA(WinUser.GetParent(win.hwnd),
                     ListSt.PEM_SHOWLINER,
                     SYSTEM.VAL(WinDef.WPARAM,win.col),
                     SYSTEM.VAL(WinDef.LPARAM,win.row)); 
END MsgLeftButtonUp;

(***********************************************************************************************)   

PROCEDURE MsgSelectTimer(win:TextWin.WinDesc);
(* Nachrichtenbehandlung für Timer *)

BEGIN
  IF (win.mouseX<0) OR (win.mouseY<0) OR
     (win.mouseX>=win.wndwidth) OR
     (win.mouseY>=win.wndheight) THEN SelectByMouse(win) END;
END MsgSelectTimer;

(***********************************************************************************************)   

PROCEDURE MsgCreate(hwnd:WinDef.HWND):INTEGER;
(* Nachrichtenbehandlung für WM_CREATE Nachricht *)

VAR
  res      : LONGINT;
  quality  : SHORTINT;
  win      : EditWin.EditWin;
  lfHeight : LONGINT;
  hdc      : WinDef.HDC;
BEGIN
  IF wCounter<MAXWIN THEN
    INC(wCounter);
  ELSE
    GlobWin.Beep;
    RETURN -1;
  END;
  hcrIBeam:=WinUser.LoadCursorA(WinDef.NULL,WinUser.IDC_IBEAM);
  hcrArrow:=WinUser.LoadCursorA(WinDef.NULL,WinUser.IDC_ARROW);
  hcrWait :=WinUser.LoadCursorA(WinDef.NULL,WinUser.IDC_WAIT);
  NEW(win);                             
  IF win=NIL THEN
    GlobWin.Beep;
    RETURN -1;
  END;   
  NEW(win.text);
  IF win.text=NIL THEN
    GlobWin.Beep;
    DISPOSE(win);
    RETURN -1;
  END;   
  win.Init;
  win.text.Init;  
  wList[wCounter-1]:=win;
  res:=WinUser.SetWindowLongA(hwnd,0,SYSTEM.VAL(LONGINT,win));
  IF wCounter=1 THEN
    hdc:=WinUser.GetDC(hwnd);
    lfHeight:=-WinBase.MulDiv(Options.fontSize,
                         WinGDI.GetDeviceCaps(hdc,WinGDI.LOGPIXELSY),
                         72);
    res:=WinUser.ReleaseDC(hwnd,hdc);
    hFont:=WinGDI.CreateFontA(lfHeight,
                        0,0,0,0,0,0,0,0,0,0,
                        WinGDI.DEFAULT_QUALITY,
                        WinGDI.FIXED_PITCH,
                        SYSTEM.ADR(Options.fontName));
    IF hFont=0 THEN
      GlobWin.Beep;
      GlobWin.DisplayError("Error","could not create font");
      RETURN -1;
    END;
  END;
  win.hwnd:=hwnd;
  win.hdc:=WinUser.GetDC(hwnd);    (* Device Kontext für Fensterlebensdauer ermitteln *)
  win.oldFont:=WinGDI.SelectObject(win.hdc,hFont);   (* Schriftwahl *)
  win.text.Init;             (* Listenstruktur initialisieren *)
  win.ScreenConfig;          (* Text/Schriftparameter initialisieren *)
  win.SelectColor(TextWin.TextColor);
  (* Nachricht senden *)
  res:=WinUser.SendMessageA(WinUser.GetParent(hwnd),
                       ListSt.PEM_SHOWLINER,
                       SYSTEM.VAL(WinDef.WPARAM,win.col),
                       SYSTEM.VAL(WinDef.LPARAM,win.row)); 
  res:=WinUser.SendMessageA(WinUser.GetParent(hwnd),ListSt.PEM_SHOWINSERTMODE,1,0);
  win.ShowTextRange(1,1);
  RETURN 0;
END MsgCreate;

(***********************************************************************************************)   

PROCEDURE MsgDestroy(win:TextWin.WinDesc);
(* Nachrichtenbehandlung von WM_DESTROY Nachricht *)

VAR
  dummy    : LONGINT;
  res      : WinDef.BOOL;
  i        : LONGINT;

BEGIN
  win.oldFont:=WinGDI.SelectObject(win.hdc,win.oldFont); 
  dummy:=WinUser.ReleaseDC(win.hwnd,win.hdc);
  win.text.ResetContents;
  dummy:=WinUser.SetWindowLongA(win.hwnd,0,0);
  DISPOSE(win.text);
  i:=0;
  WHILE (i<wCounter) & (wList[i]#win) DO INC(i) END;
  WHILE i<wCounter-1 DO
    wList[i]:=wList[i+1];
    INC(i);               
  END;
  DEC(wCounter);
  IF wCounter=0 THEN
    res:=WinGDI.DeleteObject(hFont);
    hFont:=WinDef.NULL;
  END;
  DISPOSE(win);     (* delete global record *)
END MsgDestroy;

(****************************************************************)

(****************************************************************)
(*       CallBack-Funktion zur Nachrichtenbehandlung            *)
(****************************************************************)

PROCEDURE [_APICALL] BoostedWndProc*(hWnd:WinDef.HWND;
                                   message: WinDef.UINT;
                                   wParam:WinDef.WPARAM;
                                   lParam:WinDef.LPARAM): WinDef.LRESULT;

VAR
  win         : EditWin.EditWin;
  hdc         : WinDef.HDC;   (* Handle für Device Kontext für Begin/Endpaint *)
  ps          : WinUser.PAINTSTRUCT;
  tmpcur      : WinDef.HCURSOR;
  reslt       : WinDef.LRESULT;
  dmyb,done   : BOOLEAN;
  dmyhwnd     : WinDef.HWND;
  code        : WinDef.WORD; (* Code aus wParam *)
  ok          : WinDef.BOOL;
  rect        : WinDef.RECT;
  exp         : LONGINT;

BEGIN
  win:=EditWin.AssocWinObj(hWnd);

  IF win=NIL THEN (* Fenster noch nicht vorhanden *)

    IF message=WinUser.WM_CREATE THEN (* Neues Fenster anlegen *)
      RETURN MsgCreate(hWnd);
    ELSE
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
    END;

  ELSE (* Fenster bereits vorhanden *)

    ASSERT(win IS EditWin.EditWin);

  
    (****** WM_PAINT *******)
    IF message=WinUser.WM_PAINT THEN (* WM_PAINT *)

      hdc:=WinUser.BeginPaint(hWnd, ps);
      win.ShowTextRange(1,win.text.lines);
      ok := WinUser.EndPaint(hWnd, ps);
      
    (****** WM_ERASEBKGND *******)
    ELSIF message=WinUser.WM_ERASEBKGND THEN 

      RETURN 1;

    (****** WM_DESTROY *******)
    ELSIF message=WinUser.WM_DESTROY THEN

      MsgDestroy(win);
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)

    (****** WM_VSCROLL *******)
    ELSIF message=WinUser.WM_VSCROLL THEN 

  
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_PAGEDOWN:
          win.VerScroll(win.lineNo);
      | WinUser.SB_PAGEUP:
          win.VerScroll(-win.lineNo);
      | WinUser.SB_LINEDOWN:
          win.VerScroll(1);
      | WinUser.SB_LINEUP:
          win.VerScroll(-1);
      | WinUser.SB_THUMBPOSITION,WinUser.SB_THUMBTRACK: 
          win.VerScrollThumb(SYSTEM.HIWORD(wParam)+1);
      ELSE
      END;
   
    (****** WM_HSCROLL *******)
    ELSIF message=WinUser.WM_HSCROLL THEN

      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_PAGEDOWN:
          win.HorScroll(win.colNo);
      | WinUser.SB_PAGEUP:
          win.HorScroll(-win.colNo);
      | WinUser.SB_LINEDOWN:
          win.HorScroll(1);
      | WinUser.SB_LINEUP:
          win.HorScroll(-1);
      | WinUser.SB_THUMBPOSITION,WinUser.SB_THUMBTRACK: 
          win.HorScrollThumb(SYSTEM.HIWORD(wParam)+1);
      ELSE
      END;

    (****** WM_SETCURSOR *******)
    ELSIF message=WinUser.WM_SETCURSOR THEN

      IF wParam = WinUser.HTCLIENT THEN (* Cursor in Clientbereich ändern *)
        exp := WinUser.MessageBoxA(hWnd, SYSTEM.ADR("Cursor"), SYSTEM.ADR("!!!"), WinUser.MB_OK); 
        tmpcur:=WinUser.SetCursor(hcrIBeam)
      ELSE 
        tmpcur:=WinUser.SetCursor(hcrArrow)
      END;
   
    (****** WM_KEYDOWN *******)
    ELSIF message=WinUser.WM_KEYDOWN THEN

      CASE wParam OF (* virtuellen Tastencode auslesen *)
        WinUser.VK_LEFT: 
          IF CtrlPressed() THEN 
            win.CursIdentLeft(ShiftPressed());
          ELSE 
            win.CursLeft(ShiftPressed());
          END;
      | WinUser.VK_RIGHT:
          IF CtrlPressed() THEN 
            win.CursIdentRight(ShiftPressed());
          ELSE 
            win.CursRight(ShiftPressed());
          END;
      | WinUser.VK_UP:
          win.CursUp(ShiftPressed());
      | WinUser.VK_DOWN:
          win.CursDown(ShiftPressed());
      | WinUser.VK_HOME:
          IF CtrlPressed() THEN 
            win.CursTextStart(ShiftPressed());
          ELSE
            win.CursPos1(ShiftPressed());
          END;
      | WinUser.VK_END:
          IF CtrlPressed() THEN 
            win.CursTextEnd(ShiftPressed());
          ELSE
            win.CursEnd(ShiftPressed());
          END;
      | WinUser.VK_DELETE: 
          IF win.readOnly THEN RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam) END;
          reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWCHANGED,1,0);
          dmyb:=win.DeleteChar();
          IF ~dmyb THEN RETURN 0 END;
      | WinUser.VK_INSERT:                
          IF win.readOnly THEN RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam) END;
          Options.insert:=~Options.insert;
          DestroyCaret(win);         
          CreateCaret(win);
          IF Options.insert THEN 
            reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWINSERTMODE,1,0); 
          ELSE 
            reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWINSERTMODE,0,0);
          END;      
      | WinUser.VK_PRIOR : 
          IF CtrlPressed() THEN
          ELSE
            win.CursPgUp(ShiftPressed());
          END;
      | WinUser.VK_NEXT:
          IF CtrlPressed() THEN
          ELSE
            win.CursPgDn(ShiftPressed());
          END;
      | ORD("I"):
          IF win.readOnly THEN RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam) END;
          IF CtrlPressed() THEN win.IndentMarkedBlock END;
      | ORD("U"):
          IF win.readOnly THEN RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam) END;
          IF CtrlPressed() THEN win.UnIndentMarkedBlock END;
      | ORD("Y"):
          IF win.readOnly THEN RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam) END;
          IF CtrlPressed() THEN win.DeleteLine END; (* ctrl-y *)
      ELSE
        RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam);
      END;
                     
    (****** WM_ACTIVATE *******)
    ELSIF message=WinUser.WM_ACTIVATE THEN

      code := SYSTEM.LOWORD(wParam);
      IF (code = WinUser.WA_ACTIVE) OR (code = WinUser.WA_CLICKACTIVE) THEN
        dmyhwnd:=WinUser.SetFocus(win.hwnd);  
        GlobWin.Beep;
      END;
      RETURN WinUser.DefWindowProcA(hWnd,message,wParam,lParam);
    
    (****** WM_SETFOCUS *******)
    ELSIF message=WinUser.WM_SETFOCUS THEN
      CreateCaret(win);
      reslt:=WinUser.SendMessageA(WinUser.GetParent(win.hwnd),ListSt.PEM_SHOWLINER,SYSTEM.VAL(WinDef.WPARAM,win.col),
                          SYSTEM.VAL(WinDef.LPARAM,win.row));
      IF Options.insert THEN 
        reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWINSERTMODE,1,0); 
      ELSE 
        reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWINSERTMODE,0,0);
      END;      
    
    (****** WM_TIMER *******)
    ELSIF message=WinUser.WM_TIMER THEN
      IF wParam = SELECTTIMER THEN MsgSelectTimer(win) END;
                   
    (****** WM_KILLFOCUS *******)
    ELSIF message=WinUser.WM_KILLFOCUS THEN
      DestroyCaret(win);
   
    (****** WM_LBUTTONDOWN *******)
    ELSIF message=WinUser.WM_LBUTTONDOWN THEN
    (*  IF win.readOnly THEN RETURN 0 END; *)
      MsgLeftButtonDown(win,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));
   
    (****** WM_RBUTTONDOWN *******)
    ELSIF message=WinUser.WM_RBUTTONDOWN THEN
      MsgRightButtonDown(win,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));
   
    (****** WM_LBUTTONUP *******)
    ELSIF message=WinUser.WM_LBUTTONUP THEN
   (*   IF win.readOnly THEN RETURN 0 END; *)
      MsgLeftButtonUp(win,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));
   
    (****** WM_MOUSEMOVE *******)
    ELSIF message=WinUser.WM_MOUSEMOVE THEN
  (*  IF win.readOnly THEN RETURN 0 END; *)
      MsgMouseMove(win,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));
                        
    (****** WM_LBUTTONDBLCLK oder WM_MBUTONDOWN *******)
    ELSIF (message=WinUser.WM_LBUTTONDBLCLK) OR (message=WinUser.WM_MBUTTONDOWN) THEN
      IF win.readOnly THEN 
        reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_DOUBLECLICK,0,0); 
      ELSE
        MsgLeftDoubleClick(win,SYSTEM.LOWORD(lParam),SYSTEM.HIWORD(lParam));
      END;
                   
    (****** WM_CHAR *******)
    ELSIF message=WinUser.WM_CHAR THEN
      IF (wParam=32) & CtrlPressed() THEN 
        SelectWord(win);
        RETURN 0;
      END;
      IF win.readOnly THEN RETURN 0 END;
      win.changed:=TRUE;
      reslt:=WinUser.SendMessageA(WinUser.GetParent(hWnd),ListSt.PEM_SHOWCHANGED,1,0);
      IF win.text.isSelected & 
         ~((wParam=WinUser.VK_ESCAPE) OR (wParam=WinUser.VK_BACK) OR CtrlPressed()) THEN   
        win.SetUndoAction(TextWin.ACT_OVERWRITESELECTION);
        win.undoRow:=win.text.markStart.row;
        win.undoCol:=win.text.markStart.col;
        done:=win.SelectionToGlobMem(win.undoData);
        done:=win.CutSelectionFromScreen();
        win.text.ResetMarkArea;
      END;
      CASE wParam OF
        WinUser.VK_BACK:   (* Backspace *)
          dmyb:=win.Key_Back(); IF ~dmyb THEN RETURN 0 END;
          IF ListSt.hs THEN win.CheckHorzScrollPos END;
      | WinUser.VK_TAB:  (* Tabulator *)
          IF CtrlPressed() THEN RETURN 0 END;
          dmyb:=win.Key_Tab(); IF ~dmyb THEN RETURN 0 END;
          IF ListSt.hs THEN win.CheckHorzScrollPos END;
      | WinUser.VK_RETURN: (* SplitLines(); oder NewLine(); *)
          win.Key_Return;
          IF ListSt.hs THEN win.CheckHorzScrollPos END;
      | WinUser.VK_ESCAPE:
          IF win.text.isSelected THEN (* win.IsSelected:=FALSE;*)
            win.text.ResetMarkArea;
            ok := WinUser.InvalidateRect(hWnd, NIL,0);
            RETURN 0;
          END;
      ELSE 
        IF (wParam<32) & CtrlPressed() THEN RETURN 0 END;
        IF win.col+1>ListSt.MAXLENGTH THEN 
          GlobWin.Beep;
          RETURN 0;
        END;
        win.Key_Char(SYSTEM.VAL(CHAR,wParam));  
        IF ListSt.hs THEN win.CheckHorzScrollPos END;
      END  

    ELSE (* Default Window Procedure *)
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
    END;
  END;
  RETURN 0;

END BoostedWndProc;


(***********************************************************************************************)   

PROCEDURE RegisterClass*():BOOLEAN;
VAR
  wc : WinUser.WNDCLASSEX;
BEGIN
  wc.cbSize        := SIZE(WinUser.WNDCLASSEX);
  wc.style:=0; 
  wc.style:=SYSTEM.BITOR(wc.style,WinUser.CS_OWNDC);
  wc.style:=SYSTEM.BITOR(wc.style,WinUser.CS_DBLCLKS); 
  wc.lpfnWndProc   := BoostedWndProc; 
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 4;  
  wc.hInstance     := GlobWin.hInstance;
  wc.hIcon         := WinDef.NULL;
  wc.hCursor       := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  wc.hbrBackground := WinDef.NULL; 
  wc.lpszMenuName  := WinDef.NULL; 
  wc.lpszClassName := SYSTEM.ADR(CLASSNAME);
  wc.hIconSm       := WinDef.NULL;
  RETURN WinUser.RegisterClassExA(wc)#0;
END RegisterClass;


(***********************************************************************************************)   

PROCEDURE UnregisterClass*();
VAR
  res: WinDef.BOOL;
BEGIN
  res:=WinUser.UnregisterClassA(SYSTEM.ADR(CLASSNAME),GlobWin.hInstance);
END UnregisterClass;


(***********************************************************************************************)   

PROCEDURE CloseAllWindows*();
VAR
  res: WinDef.BOOL;
BEGIN
  WHILE wCounter>0 DO
    res:=WinUser.DestroyWindow(wList[0].hwnd);
  END;
END CloseAllWindows;
 

(***********************************************************************************************)   
(***********************************************************************************************)   

BEGIN
  langHelpFile:='';
END WinHnd.
