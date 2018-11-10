(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    BoostEd32                                                     *)
(*             Basic Operative Oberon Source Text EDitor                     *)
(*                                                                           *)
(* MODULE:     WinHnd                                      V 2.00.93         *)
(*                                                         2005JUN14         *)
(*  PURPOSE:   This module implements the Windows window class which is used *)
(*             for editor windows.                                           *)
(*             This includes registration and deletion of                    *)
(*             the window class and the call-back procedure to receive       *)
(*             Windows messages.                                             *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   .         .                                                             *)
(*                                                                           *)
(*  COMMENTS:                                                                *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:                                                                *)
(*                                                                           *)
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    1995                                                          *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2004AUG25 KlS mouse wheel support added                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE WinHnd;


IMPORT
  EditWin, GlobWin, ListSt, Options, Syntax, TextWin, 
  Strings, 
  WinBase, WinDef, WinGDI, WinUser, 
  SYSTEM;


CONST
  IDM_EDIT             =               0CACH;
  CLASSNAME            =              "BoostEdEditor";
  SELECTTIMER          =               1;
  MAXWIN               =               100;

VAR
  hFont*:                              WinDef.HFONT;
  hcrIBeam,
  hcrArrow,
  hcrWait:                             WinDef.HCURSOR;     (* verschiedene Cursortypen *)
  langHelpFile*:                       ARRAY 150 OF CHAR;
  wCounter-:                           INTEGER;            (* number of opened windows *)
  wList-:                              ARRAY MAXWIN OF EditWin.EditWin;


(*****************************************************************************)
PROCEDURE CtrlPressed*                ()
                                      :BOOLEAN;
BEGIN
  RETURN WinUser.GetKeyState(WinUser.VK_CONTROL)<0;
END CtrlPressed;


(*****************************************************************************)
PROCEDURE ShiftPressed*               ()
                                      :BOOLEAN;
BEGIN
  RETURN WinUser.GetKeyState(WinUser.VK_SHIFT)<0;
END ShiftPressed;


(*****************************************************************************)
PROCEDURE SetWindowOldFont*           (wndInx:             LONGINT;
                                       font:               WinDef.HFONT);
BEGIN
  wList[wndInx].oldFont := font;
END SetWindowOldFont;


(*****************************************************************************)
(* legt ein neues Fenster zur Texteingabe an                                 *)
PROCEDURE NewEditWindow*              (parent:             WinDef.HWND;
                                       readOnly:           BOOLEAN);

VAR
  rect:                                WinDef.RECT;
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;
  done:                                WinDef.BOOL;

BEGIN
  done         := WinUser.GetClientRect(parent, rect);
  hEdit        := WinUser.CreateWindowExA(WinDef.NULL, 
  SYSTEM.ADR(CLASSNAME), 
  WinDef.NULL, 
  WinUser.WS_CHILD + WinUser.WS_VISIBLE + WinUser.WS_VSCROLL + WinUser.WS_HSCROLL + WinUser.WS_CLIPCHILDREN, 
  0, 0, 
  rect.right, rect.bottom, 
  parent, 
  IDM_EDIT, 
  GlobWin.hInstance, WinDef.NULL);
  IF (hEdit=0) THEN
    GlobWin.Beep;
    RETURN 
  ELSE
    win        := EditWin.AssocWinObj(hEdit);
    win.readOnly := readOnly;
  END (* IF (hEdit=0) *);
END NewEditWindow;


(*****************************************************************************)
PROCEDURE AddText*                    (win:                TextWin.WinDesc;
                                       text:               WinDef.LPSTR)
                                      :INTEGER;
(* hängt einen Text an den bestehenden Text an *)
(* Rückgabewert : 1 (erfolgreich), 0 (Fehler)  *)

VAR
  saverow,
  savecol,
  len:                                 LONGINT;
  done:                                BOOLEAN;

BEGIN
  saverow      := win.row;
  savecol      := win.col;
  done         := win.text.GetLineLength(win.text.lines, len);
  win.row      := win.text.lines;
  win.col      := len + 1;
  done         := win.InsertText(text);
  win.row      := saverow;
  win.col      := savecol;
  win.SetCaret;
  IF done THEN
    RETURN 1
  ELSE
   RETURN 0
  END (* IF done *);
END AddText;


(*****************************************************************************)
PROCEDURE Copy*                       (hEdit:              WinDef.HWND)
                                      :INTEGER;
(* Kopiert den ausgewählten Text in die Zwischenablage              *)
(* Rückgabewert : 1 (erfolgreich), 0 (nichts ausgewählt oder Fehler *)

VAR
  r:                                   LONGINT;
  hCopyData:                           WinDef.HANDLE;
  win:                                 EditWin.EditWin;

BEGIN
  win          := EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN
    GlobWin.Beep;
    RETURN 0;
  END (* IF win=NIL *);
  win.SetUndoAction(TextWin.ACT_NONE);
  IF WinUser.OpenClipboard(win.hwnd)=0 THEN
    RETURN 0
  END (* IF WinUser.OpenClipboard(win.hw *);
  IF win.SelectionToGlobMem(hCopyData) THEN
    r          := WinUser.EmptyClipboard();
    r          := WinUser.SetClipboardData(WinUser.CF_TEXT, hCopyData);
    r          := WinUser.CloseClipboard();
    RETURN 1;
  ELSE
    GlobWin.Beep;
    r          := WinUser.CloseClipboard();
    RETURN 0;
  END (* IF win.SelectionToGlobMem(hCopy *);
END Copy;


(*****************************************************************************)
PROCEDURE Paste*                      (hEdit:              WinDef.HWND)
                                      :INTEGER;
(* Fügt den Inhalt der Zwischenablage an der aktuellen Cursorposition ein *)
(* Rückgabewert : 1 (erfolgreich), 0 (kein Text in der Zwischenablage)    *)

VAR
  r:                                   LONGINT;
  hCopyData:                           WinDef.HANDLE;
  lpCopy:                              LONGINT;
  len,
  n:                                   LONGINT;
  win:                                 EditWin.EditWin;
  done:                                BOOLEAN;
  reslt:                               WinDef.LRESULT;
  dmyi:                                LONGINT;

BEGIN
  win          := EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN
    GlobWin.Beep;
    RETURN 0;
  END (* IF win=NIL *);
  IF WinUser.OpenClipboard(hEdit)=0 THEN
    RETURN 0
  END (* IF WinUser.OpenClipboard(hEdit) *);
  hCopyData    := WinUser.GetClipboardData(WinUser.CF_TEXT);
  IF hCopyData=WinDef.NULL THEN
    r          := WinUser.CloseClipboard();
    GlobWin.Beep;
    RETURN 0;
  END (* IF hCopyData=WinDef.NULL *);
  win.SetUndoAction(TextWin.ACT_PASTE);
  IF win.text.isSelected THEN
    win.undoRow := win.text.markStart.row;
    win.undoCol := win.text.markStart.col;
    done       := win.SelectionToGlobMem(win.undoData);
    IF ~done THEN
      GlobWin.Beep
    END (* IF ~done *);
    done       := win.CutSelectionFromScreen();
  END (* IF win.text.isSelected *);
  done         := win.InsertGlobMem(hCopyData);
  win.undoToRow := win.row;
  win.undoToCol := win.col;
  r            := WinUser.CloseClipboard();
  IF ~done THEN
    GlobWin.Beep;
    RETURN 0;
  END (* IF ~done *);
  win.changed  := TRUE;
  (* Nachricht senden *)
  reslt        := WinUser.SendMessageA(WinUser.GetParent(hEdit), ListSt.PEM_SHOWCHANGED, 1, 0);
  win.UpdateVerScrollBar;
  win.ShowTextRange(win.undoRow, win.text.lines);
  RETURN 1;
END Paste;


(*****************************************************************************)
PROCEDURE Cut*                        (hEdit:              WinDef.HWND)
                                      :INTEGER;
(* schneidet den ausgewählten Text aus und überträgt ihn in die Zwischenablage *)
(* Rückgabewert : 1 (erfolgreich), 0 (nichts ausgewählt oder Fehler            *)

VAR
  r:                                   LONGINT;
  win:                                 EditWin.EditWin;
  hCopyData:                           WinDef.HANDLE;
  done:                                BOOLEAN;

BEGIN
  win          := EditWin.AssocWinObj(hEdit);
  IF win=NIL THEN
    GlobWin.Beep;
    RETURN 0;
  END (* IF win=NIL *);
  win.SetUndoAction(TextWin.ACT_CUT);
  win.undoRow  := win.text.markStart.row;
  win.undoCol  := win.text.markStart.col;
  done         := win.SelectionToGlobMem(win.undoData);
  IF ~done THEN
    GlobWin.Beep;
    RETURN 0;
  END (* IF ~done *);
  IF WinUser.OpenClipboard(win.hwnd)=0 THEN
    RETURN 0
  END (* IF WinUser.OpenClipboard(win.hw *);
  IF win.SelectionToGlobMem(hCopyData) THEN
    r          := WinUser.EmptyClipboard();
    r          := WinUser.SetClipboardData(WinUser.CF_TEXT, hCopyData);
  ELSE
    r          := WinUser.CloseClipboard();
    GlobWin.Beep;
    RETURN 0;
  END (* IF win.SelectionToGlobMem(hCopy *);
  r            := WinUser.CloseClipboard();
  IF win.CutSelectionFromScreen() THEN
    RETURN 1
  ELSE
   RETURN 0
  END (* IF win.CutSelectionFromScreen() *);
END Cut;


(*****************************************************************************)
PROCEDURE MousePos2RowCol             (win:                TextWin.WinDesc;
                                       mx,
                                       my:                 LONGINT;
                                       VAR row,
                                       col,
                                       col2:               LONGINT);
(* col ist eine gültige Zeilenposition im bestehenden Text und col2 ist die *)
(* Zeilenposition für die Mausposition                                      *)

VAR
  done:                                BOOLEAN;
  len:                                 LONGINT;

BEGIN
  IF win.text.lines=0 THEN
    row        :=  - 1;
    col        := 1;
    col2       := 1;
    RETURN 
  END (* IF win.text.lines=0 *);
  col2         := (mx + win.charwidth DIV 2) DIV win.charwidth + win.colPos;
  row          := my DIV win.lineheight + win.textPos;
  IF row<1 THEN
    row        := 1
  ELSIF row>win.text.lines THEN
    row        := win.text.lines
  END (* IF row<1 *);
  done         := win.text.GetLineLength(row, len);
  ASSERT(done);
  IF col2<1 THEN
    col2       := 1
  ELSIF col2>ListSt.MAXLENGTH THEN
    col2       := ListSt.MAXLENGTH
  END (* IF col2<1 *);
  col          := col2;
  IF col>len THEN
    col        := len + 1
  END (* IF col>len *);
END MousePos2RowCol;


(*****************************************************************************)
PROCEDURE CreateCaret                 (win:                TextWin.WinDesc);
(* Caret erzeugen *)
VAR
  hi:                                  LONGINT;
  done:                                WinDef.BOOL;

BEGIN
  (*  IF win.readOnly THEN RETURN END; *)
  IF Options.insert THEN
    hi         := 2
  ELSE
   hi          := win.charwidth
  END (* IF Options.insert *);
  done         := WinUser.CreateCaret(win.hwnd, WinDef.NULL, hi, win.textHeight);
  (* Caret erzeugen *)
  IF ~win.text.isSelected THEN
    done       := WinUser.ShowCaret(win.hwnd);
    (* Caret anzeigen *)
  END (* IF ~win.text.isSelected *);
  win.SetCaret;
END CreateCaret;


(*****************************************************************************)
PROCEDURE DestroyCaret                (win:                TextWin.WinDesc);
(* Caret löschen *)
VAR
  done:                                WinDef.BOOL;

BEGIN
  (*  IF win.readOnly THEN RETURN END; *)
  IF ~win.text.isSelected THEN
    done       := WinUser.HideCaret(win.hwnd);
  END (* IF ~win.text.isSelected *);
  done         := WinUser.DestroyCaret();
END DestroyCaret;


(*****************************************************************************)
PROCEDURE SelectWord                  (win:                TextWin.WinDesc);
(* Wort selektieren *)
VAR
  len,
  pos:                                 LONGINT;
  txt:                                 ARRAY ListSt.MAXLENGTH+1 OF CHAR;

BEGIN
  IF ~win.text.GetLine(win.row, txt, len) THEN
    GlobWin.Beep;
    RETURN ;
  END (* IF ~win.text.GetLine(win.row, t *);
  IF win.text.isSelected THEN
    win.text.InvalidateMarkArea;
    win.ShowTextRange(win.text.markStart.row, win.text.markEnd.row);
  END (* IF win.text.isSelected *);
  pos          := win.col - 2;
  IF pos>len THEN
    pos        := len - 1
  END (* IF pos>len *);
  WHILE (pos>=0) & ~Syntax.IsIdentChar(txt[pos]) DO
   DEC(pos)
  END (* WHILE (pos>=0) & ~Syntax.IsIden *);
  WHILE (pos>=0) & Syntax.IsIdentChar(txt[pos]) DO
   DEC(pos)
  END (* WHILE (pos>=0) & Syntax.IsIdent *);
  INC(pos);
  win.text.markStart.row := win.row;
  win.text.markStart.col := pos + 1;
  win.markDown := TRUE;
  WHILE (pos<len) & Syntax.IsIdentChar(txt[pos]) DO
   INC(pos)
  END (* WHILE (pos<len) & Syntax.IsIden *);
  win.MarkUpdate(win.row, pos + 1);
  win.col      := pos + 1;
END SelectWord;


(*****************************************************************************)
PROCEDURE SelectByMouse               (win:                TextWin.WinDesc);
(* Selektieren mit Maus *)

VAR
  len,
  row,
  col,
  col2:                                LONGINT;
  done:                                BOOLEAN;

BEGIN
  MousePos2RowCol(win, win.mouseX, win.mouseY, row, col, col2);
  IF row<win.textPos THEN
    win.VerScroll(row - win.textPos)
  ELSIF row>win.textPos + win.lineNo - 1 THEN
    win.VerScroll(row - (win.textPos + win.lineNo - 1))
  END (* IF row<win.textPos *);
  done         := win.text.GetLineLength(row, len);
  IF ~done THEN
    RETURN 
  END (* IF ~done *);
  IF col>len + 1 THEN
    col        := len + 1
  END (* IF col>len + 1 *);
  win.MarkUpdate(row, col);
  IF win.markDown THEN
    win.row    := win.text.markEnd.row;
    win.col    := win.text.markEnd.col;
  ELSE
    win.row    := win.text.markStart.row;
    win.col    := win.text.markStart.col;
  END (* IF win.markDown *);
  win.CheckHorzScrollPos;
END SelectByMouse;


(*****************************************************************************)
PROCEDURE MsgRightButtonDown          (win:                TextWin.WinDesc;
                                       x,
                                       y:                  LONGINT);
(* Nachrichtenbehandlung für rechte Maustaste gedrückt *)

VAR
  i1,
  i2,
  row,
  col,
  col2,
  len:                                 LONGINT;
  txt:                                 ARRAY ListSt.MAXLENGTH OF CHAR;
  ident:                               ARRAY 40 OF CHAR;
  res:                                 WinDef.BOOL;
  beepOk:                              WinDef.BOOL;

BEGIN
  IF ~Options.mouse THEN
    RETURN 
  END (* IF ~Options.mouse *);
  MousePos2RowCol(win, x, y, row, col, col2);
  IF ~win.text.GetLine(row, txt, len) THEN
    GlobWin.Beep()
  END (* IF ~win.text.GetLine(row, txt,  *);
  i1           := col - 1;
  WHILE (i1>0) & Syntax.IsIdentChar(txt[i1 - 1]) DO
   DEC(i1)
  END (* WHILE (i1>0) & Syntax.IsIdentCh *);
  i2           := col - 1;
  WHILE (i2<len - 1) & Syntax.IsIdentChar(txt[i2 + 1]) DO
   INC(i2)
  END (* WHILE (i2<len - 1) & Syntax.IsI *);
  Strings.Copy(txt, ident, i1 + 1, i2 - i1 + 1);
  res          := WinUser.WinHelpA(win.hwnd, 
  SYSTEM.ADR(langHelpFile), 
  WinUser.HELP_PARTIALKEY, 
  SYSTEM.ADR(ident));
END MsgRightButtonDown;


(*****************************************************************************)
PROCEDURE MsgLeftButtonDown           (win:                TextWin.WinDesc;
                                       x,
                                       y:                  LONGINT);
(* Nachrichtenbehandlung für linke Maustaste gedrückt *)

VAR
  len,
  row,
  col,
  col2:                                LONGINT;
  done:                                BOOLEAN;
  oldhwnd:                             WinDef.HWND;
  res:                                 LONGINT;
  ok:                                  WinDef.BOOL;

BEGIN
  oldhwnd      := WinUser.SetFocus(win.hwnd);
  IF win.text.isSelected THEN
    win.text.ResetMarkArea;
    ok         := WinUser.InvalidateRect(win.hwnd, NIL, 0);
  END (* IF win.text.isSelected *);
  win.mouseX   := x;
  win.mouseY   := y;
  MousePos2RowCol(win, x, y, row, col, col2);
  IF row= - 1 THEN
    RETURN 
  END (* IF row= - 1 *);
  done         := win.text.GetLineLength(row, len);
  IF ~done THEN
    RETURN 
  END (* IF ~done *);
  IF col>len + 1 THEN
    col        := len + 1
  END (* IF col>len + 1 *);
  win.text.SetMarkArea(row, col, row, col);
  win.markDown := TRUE;
  win.row      := row;
  win.col      := col;
  win.SetCaret;
  oldhwnd      := WinUser.SetCapture(win.hwnd);
  (* alle Mausnachrichten erhalten *)
  win.MouseCapture := TRUE;
  win.MarkProcess := TRUE;
  (*res:=WinUser.SetTimer(win.hwnd,SELECTTIMER,125,WinDef.NULL);*)
END MsgLeftButtonDown;


(*****************************************************************************)
PROCEDURE MsgMouseMove                (win:                TextWin.WinDesc;
                                       x,
                                       y:                  LONGINT);
(* Nachrichtenbehandlung für Mausbewegung *)

VAR
  len,
  row,
  col:                                 LONGINT;
  done:                                BOOLEAN;

BEGIN
  win.mouseX   := x;
  win.mouseY   := y;
  IF ~win.MouseCapture THEN
    RETURN 
  END (* IF ~win.MouseCapture *);
  IF win.MarkProcess & (x>=0) & (y>=0) & (x<win.wndwidth) & (y<win.wndheight) THEN
    SelectByMouse(win);
  END (* IF win.MarkProcess & (x>=0) & ( *);
END MsgMouseMove;


(*****************************************************************************)
PROCEDURE MsgLeftDoubleClick          (win:                TextWin.WinDesc;
                                       x,
                                       y:                  LONGINT);
(* Nachrichtenbehandlung für linken Maustastendoppelklick *)

VAR
  row,
  col,
  col2:                                LONGINT;

BEGIN
  SelectWord(win);
END MsgLeftDoubleClick;


(*****************************************************************************)
PROCEDURE MsgLeftButtonUp             (win:                TextWin.WinDesc;
                                       x,
                                       y:                  LONGINT);
(* Nachrichtenbehandlung für linke Maustaste losgelassen *)

VAR
  res,
  row,
  col,
  col2:                                LONGINT;
  swap:                                BOOLEAN;
  done:                                WinDef.BOOL;

BEGIN
  IF ~win.MouseCapture THEN
    RETURN 
  END (* IF ~win.MouseCapture *);
  res          := WinUser.KillTimer(win.hwnd, SELECTTIMER);
  done         := WinUser.ReleaseCapture();
  win.MouseCapture := FALSE;
  IF win.MarkProcess THEN
    (* end of markprocess *)
    IF (win.text.markStart.row=win.text.markEnd.row) & 
    (win.text.markStart.col=win.text.markEnd.col) THEN
      win.text.ResetMarkArea;
      MousePos2RowCol(win, win.mouseX, win.mouseY, row, col, col2);
      win.row  := row;
      win.col  := col2;
      win.SetCaret;
    ELSE
      win.text.CheckMarkRange(swap);
      win.MarkProcess := FALSE;
    END (* IF (win.text.markStart.row=win. *);
  END (* IF win.MarkProcess *);
  (* Nachricht senden *)
  res          := WinUser.SendMessageA(WinUser.GetParent(win.hwnd), 
  ListSt.PEM_SHOWLINER, 
  SYSTEM.VAL(WinDef.WPARAM, win.col), 
  SYSTEM.VAL(WinDef.LPARAM, win.row));
END MsgLeftButtonUp;


(*****************************************************************************)
PROCEDURE MsgSelectTimer              (win:                TextWin.WinDesc);
(* Nachrichtenbehandlung für Timer *)

BEGIN
  IF (win.mouseX<0) OR (win.mouseY<0) OR 
  (win.mouseX>=win.wndwidth) OR 
  (win.mouseY>=win.wndheight) THEN
    SelectByMouse(win)
  END (* IF (win.mouseX<0) OR (win.mouse *);
END MsgSelectTimer;


(*****************************************************************************)
PROCEDURE MsgCreate                   (hwnd:               WinDef.HWND)
                                      :INTEGER;
(* Nachrichtenbehandlung für WM_CREATE Nachricht *)

VAR
  res:                                 LONGINT;
  quality:                             SHORTINT;
  win:                                 EditWin.EditWin;
  lfHeight:                            LONGINT;
  hdc:                                 WinDef.HDC;
BEGIN
  IF wCounter<MAXWIN THEN
    INC(wCounter);
  ELSE
    GlobWin.Beep;
    RETURN  - 1;
  END (* IF wCounter<MAXWIN *);
  hcrIBeam     := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_IBEAM);
  hcrArrow     := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  hcrWait      := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_WAIT);
  NEW(win);
  IF win=NIL THEN
    GlobWin.Beep;
    RETURN  - 1;
  END (* IF win=NIL *);
  NEW(win.text);
  IF win.text=NIL THEN
    GlobWin.Beep;
    DISPOSE(win);
    RETURN  - 1;
  END (* IF win.text=NIL *);
  win.Init;
  win.text.Init;
  wList[wCounter - 1] := win;
  res          := WinUser.SetWindowLongA(hwnd, 0, SYSTEM.VAL(LONGINT, win));
  IF wCounter=1 THEN
    hdc        := WinUser.GetDC(hwnd);
    lfHeight   :=  - WinBase.MulDiv(Options.fontSize, 
                                    WinGDI.GetDeviceCaps(hdc, WinGDI.LOGPIXELSY), 
                                    72);
    res        := WinUser.ReleaseDC(hwnd, hdc);
    hFont      := WinGDI.CreateFontA(lfHeight, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     WinGDI.DEFAULT_QUALITY, 
                                     WinGDI.FIXED_PITCH, 
                                     SYSTEM.ADR(Options.fontName));
    IF hFont=0 THEN
      GlobWin.Beep;
      GlobWin.DisplayError("Error", "could not create font");
      RETURN  - 1;
    END (* IF hFont=0 *);
  END (* IF wCounter=1 *);
  win.hwnd     := hwnd;
  win.hdc      := WinUser.GetDC(hwnd);                     (* Device Kontext für Fensterlebensdauer ermitteln *)
  win.oldFont  := WinGDI.SelectObject(win.hdc, hFont);     (* Schriftwahl *)
  win.text.Init;                                           (* Listenstruktur initialisieren *)
  win.ScreenConfig;                                        (* Text/Schriftparameter initialisieren *)
  win.SelectColor(TextWin.TextColor);                      (* Nachricht senden *)
  res          := WinUser.SendMessageA(WinUser.GetParent(hwnd), 
                                       ListSt.PEM_SHOWLINER, 
                                       SYSTEM.VAL(WinDef.WPARAM, win.col), 
                                       SYSTEM.VAL(WinDef.LPARAM, win.row));
                                       res          := WinUser.SendMessageA(WinUser.GetParent(hwnd), ListSt.PEM_SHOWINSERTMODE, 1, 0);
  win.ShowTextRange(1, 1);
  RETURN 0;
END MsgCreate;


(*****************************************************************************)
PROCEDURE MsgDestroy                  (win:                TextWin.WinDesc);
(* Nachrichtenbehandlung von WM_DESTROY Nachricht *)

VAR
  dummy:                               LONGINT;
  res:                                 WinDef.BOOL;
  i:                                   LONGINT;

BEGIN
  win.oldFont  := WinGDI.SelectObject(win.hdc, win.oldFont);
  dummy        := WinUser.ReleaseDC(win.hwnd, win.hdc);
  win.text.ResetContents;
  dummy        := WinUser.SetWindowLongA(win.hwnd, 0, 0);
  DISPOSE(win.text);
  i            := 0;
  WHILE (i<wCounter) & (wList[i]#win) DO
   INC(i)
  END (* WHILE (i<wCounter) & (wList[i]# *);
  WHILE i<wCounter - 1 DO
    wList[i]   := wList[i+1];
    INC(i);
  END (* WHILE i<wCounter - 1 *);
  DEC(wCounter);
  IF wCounter=0 THEN
    res        := WinGDI.DeleteObject(hFont);
    hFont      := WinDef.NULL;
  END (* IF wCounter=0 *);
  DISPOSE(win);
  (* delete global record *)
END MsgDestroy;


(*****************************************************************************)
(*****************************************************************************)
(*             CallBack-Funktion zur Nachrichtenbehandlung                   *)
(*****************************************************************************)
PROCEDURE [_APICALL] BoostedWndProc*  (hWnd:               WinDef.HWND;
                                       message:            WinDef.UINT;
                                       wParam:             WinDef.WPARAM;
                                       lParam:             WinDef.LPARAM)
                                      :WinDef.LRESULT;

VAR
  win:                                 EditWin.EditWin;
  hdc:                                 WinDef.HDC;         (* Handle für Device Kontext für Begin/Endpaint *)
  ps:                                  WinUser.PAINTSTRUCT;
  tmpcur:                              WinDef.HCURSOR;
  Result:                              WinDef.LRESULT;
  dmyb,
  done:                                BOOLEAN;
  dmyhwnd:                             WinDef.HWND;
  code:                                WinDef.WORD;        (* Code aus wParam *)
  ok:                                  WinDef.BOOL;
  rect:                                WinDef.RECT;
  Ready:                               BOOLEAN;
  exp:                                 LONGINT;
  InfoStr2:                            ARRAY 16 OF CHAR;
  i,
  j:                                   LONGINT;
  MySyntaxColouring:                   Options.SyntaxColouringP;

BEGIN
  win          := EditWin.AssocWinObj(hWnd);

  IF win=NIL THEN                                          (* Fenster noch nicht vorhanden *)

    IF message=WinUser.WM_CREATE THEN                      (* generate new window *)
      RETURN MsgCreate(hWnd);
    ELSE
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
    END (* IF message=WinUser.WM_CREATE *);

  ELSE                                                     (* window exists already *)

    ASSERT(win IS EditWin.EditWin);

    (****** WM_PAINT *******)
    IF message=WinUser.WM_PAINT THEN                       (* WM_PAINT *)
      hdc      := WinUser.BeginPaint(hWnd, ps);
      win.ShowTextRange(1, win.text.lines);
      ok       := WinUser.EndPaint(hWnd, ps);

    (****** WM_ERASEBKGND *******)
    ELSIF message=WinUser.WM_ERASEBKGND THEN               (* WM_ERASEBKGND *)
      RETURN 1;

    (****** WM_DESTROY *******)
    ELSIF message=WinUser.WM_DESTROY THEN                  (* WM_DESTROY *)
      MsgDestroy(win);
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)

    (****** WM_VSCROLL *******)
    ELSIF message=WinUser.WM_VSCROLL THEN                  (* WM_VSCROLL *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_PAGEDOWN:
          win.VerScroll(win.lineNo);
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_PAGEUP:
          win.VerScroll(-win.lineNo);
        | (* WinUser.SB_PAGEUP *)
        WinUser.SB_LINEDOWN:
          win.VerScroll(1);
        | (* WinUser.SB_LINEDOWN *)
        WinUser.SB_LINEUP:
          win.VerScroll(-1);
        | (* WinUser.SB_LINEUP *)
        WinUser.SB_THUMBPOSITION, WinUser.SB_THUMBTRACK:
          win.VerScrollThumb(SYSTEM.HIWORD(wParam)+1);
        ELSE
          ;
      END (* CASE SYSTEM.LOWORD(wParam) *) ;

    (****** WM_HSCROLL *******)
    ELSIF message=WinUser.WM_HSCROLL THEN                  (* WM_HSCROLL *)
      CASE SYSTEM.LOWORD(wParam) OF
        WinUser.SB_PAGEDOWN:
          win.HorScroll(win.colNo);
        | (* WinUser.SB_PAGEDOWN *)
        WinUser.SB_PAGEUP:
          win.HorScroll(-win.colNo);
        | (*  *)
        WinUser.SB_LINEDOWN:
          win.HorScroll(1);
        | (*  *)
        WinUser.SB_LINEUP:
          win.HorScroll(-1);
        | (*  *)
        WinUser.SB_THUMBPOSITION, WinUser.SB_THUMBTRACK:
          win.HorScrollThumb(SYSTEM.HIWORD(wParam)+1);
        ELSE
          ;
      END (* CASE SYSTEM.LOWORD(wParam) *) ;

      (****** WM_MOUSEWHEEL ******                              KlS, 2004AUG25 *)
    ELSIF message=020AH THEN                               (* WM_MOUSEWHEEL  *)
      IF SYSTEM.HIWORD(wParam)>0 THEN
        win.VerScroll( - 1);
      ELSE
        win.VerScroll(1);
      END (* IF SYSTEM.HIWORD(wParam)>0 *) ;

      (****** WM_SETCURSOR *******)
    ELSIF message=WinUser.WM_SETCURSOR THEN                (* WM_SETCURSOR *)
      IF wParam=WinUser.HTCLIENT THEN
        (* Cursor in Clientbereich ändern *)
        exp    := WinUser.MessageBoxA(hWnd, SYSTEM.ADR("Cursor"), SYSTEM.ADR("!!!"), WinUser.MB_OK);
        tmpcur := WinUser.SetCursor(hcrIBeam)
      ELSE
        tmpcur := WinUser.SetCursor(hcrArrow)
      END (* IF wParam=WinUser.HTCLIENT *);

      (****** WM_KEYDOWN *******)
    ELSIF message=WinUser.WM_KEYDOWN THEN                  (* WM_KEYDOWN *)
      CASE wParam OF
        (* virtuellen Tastencode auslesen *)
        WinUser.VK_LEFT:
          IF CtrlPressed() THEN
            win.CursIdentLeft(ShiftPressed());
          ELSE
            win.CursLeft(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (* WinUser.VK_LEFT *)
        WinUser.VK_RIGHT:
          IF CtrlPressed() THEN
            win.CursIdentRight(ShiftPressed());
          ELSE
            win.CursRight(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (*  *)
        WinUser.VK_UP:
          win.CursUp(ShiftPressed());
        | (*  *)
        WinUser.VK_DOWN:
          win.CursDown(ShiftPressed());
        | (*  *)
        WinUser.VK_HOME:
          IF CtrlPressed() THEN
            win.CursTextStart(ShiftPressed());
          ELSE
            win.CursPos1(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (*  *)
        WinUser.VK_END:
          IF CtrlPressed() THEN
            win.CursTextEnd(ShiftPressed());
          ELSE
            win.CursEnd(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (*  *)
        WinUser.VK_DELETE:
          IF win.readOnly THEN
            RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
          END (* IF win.readOnly *);
          Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWCHANGED, 1, 0);
          dmyb := win.DeleteChar();
          IF ~dmyb THEN
            RETURN 0
          END (* IF ~dmyb *);
        | (*  *)
        WinUser.VK_INSERT:
          IF win.readOnly THEN
            RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
          END (* IF win.readOnly *);
          Options.insert := ~Options.insert;
          DestroyCaret(win);
          CreateCaret(win);
          IF Options.insert THEN
            Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWINSERTMODE, 1, 0);
          ELSE
            Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWINSERTMODE, 0, 0);
          END (* IF Options.insert *);
        | (*  *)
        WinUser.VK_PRIOR:
          IF CtrlPressed() THEN
            ;
          ELSE
            win.CursPgUp(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (*  *)
        WinUser.VK_NEXT:
          IF CtrlPressed() THEN
            ;
          ELSE
            win.CursPgDn(ShiftPressed());
          END (* IF CtrlPressed() *);
        | (*  *)
        ORD("I"):
          IF win.readOnly THEN
            RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
          END (* IF win.readOnly *);
          IF CtrlPressed() THEN
            win.IndentMarkedBlock
          END (* IF CtrlPressed() *);
        | (*  *)
        ORD("U"):
          IF win.readOnly THEN
            RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
          END (* IF win.readOnly *);
          IF CtrlPressed() THEN
            win.UnIndentMarkedBlock
          END (* IF CtrlPressed() *);
        | (*  *)
        ORD("Y"):
          IF win.readOnly THEN
            RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
          END (* IF win.readOnly *);
          IF CtrlPressed() THEN
            win.DeleteLine
          END (* IF CtrlPressed() *);

        ELSE
          RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam);
      END (* CASE wParam *);

      (****** WM_ACTIVATE *******)
    ELSIF message=WinUser.WM_ACTIVATE THEN                 (* WM_ACTIVATE *)
      code     := SYSTEM.LOWORD(wParam);
      IF (code=WinUser.WA_ACTIVE) OR (code=WinUser.WA_CLICKACTIVE) THEN
        dmyhwnd := WinUser.SetFocus(win.hwnd);
        GlobWin.Beep;
      ELSE
        ;
      END (* IF (code=WinUser.WA_ACTIVE) OR  *);
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam);

      (****** WM_SETFOCUS *******)
    ELSIF message=WinUser.WM_SETFOCUS THEN                 (* WM_SETFOCUS *)
      IF Strings.Length(win.Extension)#0 THEN
        Options.ActSyntaxColouring := win.SyntaxColouring;
      END (* IF Strings.Length(win.Extension *) ;
      CreateCaret(win);
      dmyhwnd  := WinUser.UpdateWindow(win.hwnd);
      Result   := WinUser.SendMessageA(WinUser.GetParent(win.hwnd), 
      ListSt.PEM_SHOWLINER, 
      SYSTEM.VAL(WinDef.WPARAM, win.col), 
      SYSTEM.VAL(WinDef.LPARAM, win.row));
      IF Options.insert THEN
        Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWINSERTMODE, 1, 0);
      ELSE
        Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWINSERTMODE, 0, 0);
      END (* IF Options.insert *);

      (****** WM_TIMER *******)
    ELSIF message=WinUser.WM_TIMER THEN                    (* WM_TIMER *)
      IF wParam=SELECTTIMER THEN
        MsgSelectTimer(win)
      END (* IF wParam=SELECTTIMER *);

      (****** WM_KILLFOCUS *******)
    ELSIF message=WinUser.WM_KILLFOCUS THEN                (* WM_KILLFOCUS *)
      DestroyCaret(win);

      (****** WM_LBUTTONDOWN *******)
    ELSIF message=WinUser.WM_LBUTTONDOWN THEN              (* WM_LBUTTONDOWN *)
      (*  IF win.readOnly THEN RETURN 0 END; *)
      MsgLeftButtonDown(win, SYSTEM.LOWORD(lParam), SYSTEM.HIWORD(lParam));

      (****** WM_RBUTTONDOWN *******)
    ELSIF message=WinUser.WM_RBUTTONDOWN THEN              (* WM_RBUTTONDOWN *)
      MsgRightButtonDown(win, SYSTEM.LOWORD(lParam), SYSTEM.HIWORD(lParam));

      (****** WM_LBUTTONUP *******)
    ELSIF message=WinUser.WM_LBUTTONUP THEN                (* WM_LBUTTONUP *)
      (*   IF win.readOnly THEN RETURN 0 END; *)
      MsgLeftButtonUp(win, SYSTEM.LOWORD(lParam), SYSTEM.HIWORD(lParam));

      (****** WM_MOUSEMOVE *******)
    ELSIF message=WinUser.WM_MOUSEMOVE THEN                (* WM_MOUSEMOVE *)
      (*  IF win.readOnly THEN RETURN 0 END; *)
      MsgMouseMove(win, SYSTEM.LOWORD(lParam), SYSTEM.HIWORD(lParam));

      (****** WM_LBUTTONDBLCLK oder WM_MBUTONDOWN *******)
    ELSIF (message=WinUser.WM_LBUTTONDBLCLK)
     OR (message=WinUser.WM_MBUTTONDOWN) THEN              (* WM_LBUTTONDBLCLK or WM_MBUTONDOWN *)
      IF win.readOnly THEN
        Result := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_DOUBLECLICK, 0, 0);
      ELSE
        MsgLeftDoubleClick(win, SYSTEM.LOWORD(lParam), SYSTEM.HIWORD(lParam));
      END (* IF win.readOnly *);

      (****** WM_CHAR *******)
    ELSIF message=WinUser.WM_CHAR THEN                     (* WM_CHAR *)
      IF (wParam=32) & CtrlPressed() THEN
        SelectWord(win);
        RETURN 0;
      END (* IF (wParam=32) & CtrlPressed() *);
      IF win.readOnly THEN
        RETURN 0
      END (* IF win.readOnly *);
      win.changed := TRUE;
      Result   := WinUser.SendMessageA(WinUser.GetParent(hWnd), ListSt.PEM_SHOWCHANGED, 1, 0);
      IF win.text.isSelected & ~((wParam=WinUser.VK_ESCAPE)
       OR (wParam=WinUser.VK_BACK) OR CtrlPressed()) THEN
        win.SetUndoAction(TextWin.ACT_OVERWRITESELECTION);
        win.undoRow := win.text.markStart.row;
        win.undoCol := win.text.markStart.col;
        done   := win.SelectionToGlobMem(win.undoData);
        done   := win.CutSelectionFromScreen();
        win.text.ResetMarkArea;
      END (* IF win.text.isSelected & ~((wPa *);

      CASE wParam OF
        WinUser.VK_BACK:
          (* Backspace *)
          dmyb := win.Key_Back();
          IF ~dmyb THEN
            RETURN 0
          END (* IF ~dmyb *);
          IF ListSt.hs THEN
            win.CheckHorzScrollPos
          END (* IF ListSt.hs *);
        | (* WinUser.VK_BACK *)
        WinUser.VK_TAB:
          IF CtrlPressed() THEN
            RETURN 0
          END (* IF CtrlPressed() *);
          dmyb := win.Key_Tab();
          IF ~dmyb THEN
            RETURN 0
          END (* IF ~dmyb *);
          IF ListSt.hs THEN
            win.CheckHorzScrollPos
          END (* IF ListSt.hs *);
        | (*  *)
        WinUser.VK_RETURN:
          win.Key_Return;
          IF ListSt.hs THEN
            win.CheckHorzScrollPos
          END (* IF ListSt.hs *);
        | (*  *)
        WinUser.VK_ESCAPE:
          IF win.text.isSelected THEN
            (* win.IsSelected:=FALSE;*)
            win.text.ResetMarkArea;
            ok := WinUser.InvalidateRect(hWnd, NIL, 0);
            RETURN 0;
          END (* IF win.text.isSelected *);
        ELSE
          IF (wParam<32) & CtrlPressed() THEN
            RETURN 0
          END (* IF (wParam<32) & CtrlPressed() *);
          IF win.col + 1>ListSt.MAXLENGTH THEN
            GlobWin.Beep;
            RETURN 0;
          END (* IF win.col + 1>ListSt.MAXLENGTH *);
          win.Key_Char(SYSTEM.VAL(CHAR, wParam));
          IF ListSt.hs THEN
            win.CheckHorzScrollPos
          END (* IF ListSt.hs *);
      END (* CASE wParam *) 

    ELSE                                                   (* Default Window Procedure *)
      RETURN WinUser.DefWindowProcA(hWnd, message, wParam, lParam)
    END (* IF message=WinUser.WM_PAINT *);
  END (* IF win=NIL *);

  RETURN 0;

END BoostedWndProc;


(*****************************************************************************)
PROCEDURE RegisterClass*              ()
                                      :BOOLEAN;
VAR
  wc:                                  WinUser.WNDCLASSEX;
BEGIN
  wc.cbSize    := SIZE(WinUser.WNDCLASSEX);
  wc.style     := 0;
  wc.style     := SYSTEM.BITOR(wc.style, WinUser.CS_OWNDC);
  wc.style     := SYSTEM.BITOR(wc.style, WinUser.CS_DBLCLKS);
  wc.lpfnWndProc := BoostedWndProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 4;
  wc.hInstance := GlobWin.hInstance;
  wc.hIcon     := WinDef.NULL;
  wc.hCursor   := WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_ARROW);
  wc.hbrBackground := WinDef.NULL;
  wc.lpszMenuName := WinDef.NULL;
  wc.lpszClassName := SYSTEM.ADR(CLASSNAME);
  wc.hIconSm   := WinDef.NULL;
  RETURN WinUser.RegisterClassExA(wc)#0;
END RegisterClass;


(*****************************************************************************)
PROCEDURE UnregisterClass*            ();
VAR
  ResultBool:                          WinDef.BOOL;
BEGIN
  ResultBool   := WinUser.UnregisterClassA(SYSTEM.ADR(CLASSNAME), GlobWin.hInstance);
END UnregisterClass;


(*****************************************************************************)
PROCEDURE CloseAllWindows*            ();
VAR
  ResultBool:                          WinDef.BOOL;
BEGIN
  WHILE wCounter>0 DO
    ResultBool := WinUser.DestroyWindow(wList[0].hwnd);
  END (* WHILE wCounter>0 *);
END CloseAllWindows;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
  langHelpFile := '';
END WinHnd.

