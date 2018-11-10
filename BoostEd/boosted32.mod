(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    BoostEd32                                                     *)
(*             Basic Operative Oberon Source Text EDitor                     *)
(*                                                                           *)
(* MODULE:     BoostEd32                                   V 2.00.15         *)
(*                                                         2005JUN14         *)
(*  PURPOSE:   This module contains the procedures which form the interface  *)
(*             that is used by Pow! to integrate the editor into the         *)
(*             environment.                                                  *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   .         .                                                             *)
(*                                                                           *)
(*  COMMENTS:                                                                *)
(*                                                                           *)
(*    If you want to create a new version of the editor yourself,            *)
(*    you have to                                                            *)
(*                                                                           *)
(*    1. Recompile this project                                              *)
(*    2. Copy the Boosted32.DLL file into the Pow! program directory         *)
(*       (the Pow.EXE file is in this directory)                             *)
(*    3. Rename the Boosted32.DLL into something like "NewBoosted.ELL".      *)
(*       It is important to change the extension from ".DLL" to ".ELL".      *)
(*       Of course it is also possible to replace the original               *)
(*       Boosted32.ELL that came with this distribution, but it is           *)
(*       advisable to keep a copy of a working editor just in case.          *)
(*    4. Start Pow!, select "Preferences" in the menu "Options".             *)
(*       Select the editor in the editor combo box.                          *)
(*                                                                           *)
(*    To get rid of a faulty editor which can not even be loaded,            *)
(*    you can delete the corresponding ".ELL" file in the Pow! program       *)
(*    directory. Although Pow! will complain it will start up and allow      *)
(*    you to select another editor.                                          *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:                                                                *)
(*                                                                           *)
(*                                                                           *)
(* Author(s):                                                                *)
(*   Vi4Win    1.0 1995        Michael Bogner, Max Mayrb‰url                 *)
(*   BoostEd   1.0 1996        Bernhard Leisch                               *)
(*   BoostEd32 0.9 1998        Alexander Bergsmann                           *)
(*   BoostEd32 1.0 1998        Bernhard Leisch                               *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   1995                                                                    *)
(*                                                                           *)
(*  update                                                                   *)
(*   Vi4Win    1.0 1995        Michael Bogner, Max Mayrb‰url                 *)
(*   BoostEd   1.0 1996        Bernhard Leisch                               *)
(*   BoostEd32 0.9 1998        Alexander Bergsmann                           *)
(*   BoostEd32 1.0 1998        Bernhard Leisch                               *)
(*   BoostEd32 1.1 1998 21/10  BL +more explicit error messages for printing *)
(*   2004APR19 KlS     keyword colouring added                               *)
(*                                                                           *)
(*  release                                                                  *)
(*   BoostEd 32  Rel. 1.1      21/10/1998                                    *)
(*                                                                           *)
(*****************************************************************************)

MODULE BoostEd32;

IMPORT
  SYSTEM, 
  WinDef, WinBase, WinUser, WinNT, WinGDI, 
  GlobWin, WinHnd, TextWin, FileHnd, ListSt, EditWin, EnvHnd, Print, 
  Options, OptionDialogs, Syntax, 
  Strings;


CONST
  EDITORINTERFACEVERSION =             170;                (* Pow! editor interface version supported 
                                                              by this implementation *)
  TRACE_CALLS          =               FALSE;              (* if set to TRUE, each call of an interface procedure
                                                              also generates a pop-up window showing which procedure has been called. *)


(*****************************************************************************)
(* returns the code for the Pow! editor interface version supported by the editor *)
PROCEDURE [_APICALL] InterfaceVersion*()
                                      :INTEGER;
BEGIN
  RETURN EDITORINTERFACEVERSION;
END InterfaceVersion;


(*****************************************************************************)
(* Creates an empty new editor window. If readOnly is true, keyboard 
   input to the window is disabled. *)
PROCEDURE [_APICALL] NewEditWindow*   (Parent:             WinDef.HWND;
                                       ReadOnly:           INTEGER);
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in NewEditWindow ")
  END (* IF TRACE_CALLS *);
  WinHnd.NewEditWindow(Parent, ReadOnly#0);
END NewEditWindow;


(*****************************************************************************)
(* Closes an edit window. Any changes to the contents are lost. *)
PROCEDURE [_APICALL] CloseEditWindow* (edit:               WinDef.HWND);
VAR
  hEdit:                               WinDef.HWND;
  ResultBool:                          WinDef.BOOL;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in CloseEditWindow ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  ResultBool   := WinUser.DestroyWindow(hEdit);
END CloseEditWindow;


(*****************************************************************************)
(* This function returns TRUE if the text in the edit window has been changed
   since it was loaded from file *)
PROCEDURE [_APICALL] HasChanged*      (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in HasChanged ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  IF win.changed THEN
    RETURN 1
  ELSE
    RETURN 0
  END (* IF win.changed *);
END HasChanged;


(*****************************************************************************)
(* The edit window is loaded with the file specified in <name> *)
PROCEDURE [_APICALL] LoadFile*        (edit:               WinDef.HWND;
                                       name:               WinDef.LPSTR)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
  res:                                 INTEGER;
  hcurSave,
  tmpcur:                              WinDef.HCURSOR;
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in LoadFile ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  hcurSave     := WinUser.SetCursor(WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_WAIT)); (* Cursorumriﬂ ‰ndern *)
  win.text.ResetContents;
  res          := FileHnd.LoadFile(hEdit, name);
  tmpcur       := WinUser.SetCursor(hcurSave);
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "end of LoadFile")
  END (* IF TRACE_CALLS *);
  RETURN res;
END LoadFile;


(*****************************************************************************)
(* The current contents of the edit window are stored in the file specified by
   <name>. The previous contents of the file are overwritten. *)
PROCEDURE [_APICALL] SaveFile*        (edit:               WinDef.HWND;
                                       name:               WinDef.LPSTR)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
  res:                                 INTEGER;
  hcurSave,
  tmpcur:                              WinDef.HCURSOR;
  reslt:                               WinDef.LRESULT;
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in SaveFile ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  hcurSave     := WinUser.SetCursor(WinUser.LoadCursorA(WinDef.NULL, WinUser.IDC_WAIT));
  res          := FileHnd.SaveFile(hEdit, name);
  IF res=1 THEN
    win.changed := FALSE;
    reslt      := WinUser.SendMessageA(WinUser.GetParent(hEdit), ListSt.PEM_SHOWCHANGED, 0, 0);
  ELSE
    GlobWin.Beep;
  END (* IF res=1 *);
  tmpcur       := WinUser.SetCursor(hcurSave);
  RETURN res;
END SaveFile;


(*****************************************************************************)
(* This function returns the current position of the cursor in the edit window
   in <row> and <col>. 
   The return value of the function is 1 for success and 0 for failure. *)
PROCEDURE [_APICALL] GetCursorpos*    (edit:               WinDef.HWND;
                                       VAR row,
                                       col:                LONGINT)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GetCursorPos ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  IF win#NIL THEN
    row        := win.row;
    col        := win.col;
    RETURN 1;
  ELSE
    RETURN 0;
  END (* IF win#NIL *);
END GetCursorpos;


(*****************************************************************************)
(* The currently selected text is copied to the clipboard 
   The return value of the function is 1 for success and 0 if there was
   a fault or no text was selected. *)
PROCEDURE [_APICALL] Copy*            (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Copy ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  RETURN WinHnd.Copy(hEdit);
END Copy;


(*****************************************************************************)
(* The current contents of the clipboard is inserted at the current 
   cursor location 
   The return value of the function is 1 for success and 0 if there was
   a fault or the clipboard was empty. *)
PROCEDURE [_APICALL] Paste*           (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Paste")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  RETURN WinHnd.Paste(hEdit);
END Paste;


(*****************************************************************************)
(* The currently selected text is copied into the clipboard and removed 
   from the edit window. 
   The return value of the function is 1 for success and 0 if there was
   a fault or no text was selected. *)
PROCEDURE [_APICALL] Cut*             (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Cut ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  RETURN WinHnd.Cut(hEdit);
END Cut;


(*****************************************************************************)
(* The currently selected text is deleted
   The return value of the function is 1 for success and 0 if there was
   a fault or no text was selected. *)
PROCEDURE [_APICALL] Clear*           (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Clear")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  IF win.CutSelectionFromScreen() THEN
    RETURN 1
  ELSE
   RETURN 0
  END (* IF win.CutSelectionFromScreen() *);
END Clear;


(*****************************************************************************)
(* The return value of the function is
   0: if the editor does not support Undo
   1: if the editor supports Undo but does not support Redo
   2: if the editor supports Undo and Redo
   This editor implementation supports a single Undo/Redo (no undo buffer) *)
PROCEDURE [_APICALL] CanUndo*         ()
                                      :INTEGER;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in CanUndo ")
  END (* IF TRACE_CALLS *);
  RETURN 2;
END CanUndo;


(*****************************************************************************)
(* The last change to the text is undone *)
PROCEDURE [_APICALL] Undo*            (edit:               WinDef.HWND);
VAR
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Undo ")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  win.Undo;
END Undo;


(*****************************************************************************)
(* The last undone change to the text is done again *)
PROCEDURE [_APICALL] Redo*            (edit:               WinDef.HWND);
VAR
  hEdit:                               WinDef.HWND;
  win:                                 EditWin.EditWin;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Redo")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  win          := EditWin.AssocWinObj(hEdit);
  win.Redo;
END Redo;


(*****************************************************************************)
(* The cursor position is set to the position defined by <row> and <col>.
   Column and row count starts with 1.
   The cursor can be set after the end of the text by setting <row> or 
   <col> to -1. *)
PROCEDURE [_APICALL] GotoPos*         (edit:               WinDef.HWND;
                                       row,
                                       col:                LONGINT)
                                      :INTEGER;
VAR
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GotoPos ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win#NIL THEN
    win.CursGoto(row, col);
    RETURN 1;
  ELSE
    RETURN 0;
  END (* IF win#NIL *);
END GotoPos;


(*****************************************************************************)
(* The string defined in <text> is searched for in the designated edit
   window. The search is 
   case sensitive if <matchcase> is TRUE, 
   downwards from the current cursor location if <down> is TRUE,
   searches for whole words only if <words> is TRUE. 
   The return value is TRUE if the text has been found and FALSE if
   it could not be found. *)
PROCEDURE [_APICALL] Search*          (edit:               WinDef.HWND;
                                       text:               WinDef.LPSTR;
                                       matchcase,
                                       down,
                                       words:              BOOLEAN)
                                      :BOOLEAN;
VAR
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Search")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win=NIL THEN
    RETURN FALSE
  END (* IF win=NIL *);
  RETURN win.SearchText(text, matchcase, down, words);
END Search;


(*****************************************************************************)
(* The string defined in <text> is replcaded with the string defined in
   <new> in the designated edit window. The search for the text which should
   be replaced is 
   case sensitive if <matchcase> is TRUE, 
   downwards from the current cursor location if <down> is TRUE,
   searches for whole words only if <words> is TRUE. 
   All occurrences of the text are replaced if <all> is true.
   If <ask> is TRUE a pop-up window is displayed to individually confirm the 
   replacement of each occurrence of <text>.
   The return value of the function is the number of text replacements which
   took place. *)
PROCEDURE [_APICALL] Replace*         (edit:               WinDef.HWND;
                                       text,
                                       new:                WinDef.LPSTR;
                                       matchcase,
                                       down,
                                       words,
                                       all,
                                       ask:                BOOLEAN)
                                      :INTEGER;
VAR
  res:                                 BOOLEAN;
  win:                                 EditWin.EditWin;
  once:                                BOOLEAN;

(*---------------------------------------------------------------------------*)
PROCEDURE ReplaceOk                   ()
                                      :BOOLEAN;
VAR
  res:                                 LONGINT;
BEGIN
  res          := WinUser.MessageBoxA(win.hwnd, 
  SYSTEM.ADR("Replace ?"), 
  SYSTEM.ADR("Search and Replace"), 
  WinUser.MB_YESNOCANCEL);
  IF res=WinUser.IDCANCEL THEN
    all        := FALSE
  END (* IF res=WinUser.IDCANCEL *);
  RETURN res=WinUser.IDYES;
END ReplaceOk;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Replace")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win=NIL THEN
    RETURN 0
  END (* IF win=NIL *);
  win.SetUndoAction(TextWin.ACT_NONE);
  once         := FALSE;
  REPEAT
    res        := win.SearchText(text, matchcase, down, words);
    IF (res=TRUE) & ((ask=FALSE) OR ReplaceOk()) THEN
      IF win.CutSelectionFromScreen() THEN
        IF ~win.InsertText(new) THEN
          GlobWin.Beep;
          res  := FALSE;
        END (* IF ~win.InsertText(new) *);
      END (* IF win.CutSelectionFromScreen() *);
    END (* IF (res=TRUE) & ((ask=FALSE) OR *);
    IF res=TRUE THEN
      once     := TRUE
    END (* IF res=TRUE *);
  UNTIL (res=FALSE) OR (all=FALSE);
  IF once THEN
    RETURN 1
  ELSE
   RETURN 0
  END (* IF once *);
END Replace;


(*****************************************************************************)
(* This function supplies the editor with a list of key words to allow       *)
(* syntax highlighting.                                                      *)
PROCEDURE [_APICALL] Keywords*        (CaseSensitive:      BOOLEAN;
                                       KeyWords:           WinDef.LPSTR);

VAR
  i:                                   LONGINT;
  MyKeyWord:                           ARRAY 32 OF CHAR;
  MyString:                            ARRAY 2048 OF CHAR;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in KeyWords");
  END (* IF TRACE_CALLS *) ;
END Keywords;


(*****************************************************************************)
(* This function is not supported.                                           *)
PROCEDURE [_APICALL] SetCommandProcedure*
                                      (command:            WinDef.FARPROC);
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in SetCommandProcedure ")
  END (* IF TRACE_CALLS *);
END SetCommandProcedure;


(*****************************************************************************)
(* This function defines the strings which are used to open and close a comment
   in the source file depending on the compiler which is currently selected. *)
PROCEDURE [_APICALL] Comments*        (nested:             WinDef.WORD;
                                       on,
                                       off,
                                       stringstart:        WinDef.LPSTR);

VAR
  dmy:                                 WinDef.LPSTR;
  buf:                                 ARRAY 1024 OF CHAR;

  (*---------------------------------------------------------------------------*)
  PROCEDURE CutBufAtBlank               ();
  VAR
    i:                                   INTEGER;
  BEGIN
    i            := 0;
    WHILE (buf[i]#0X) & (buf[i]#' ') DO
     INC(i)
    END (* WHILE (buf[i]#0X) & (buf[i]#') *);
    buf[i]       := 0X;
  END CutBufAtBlank;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in Comments")
  END (* IF TRACE_CALLS *);
END Comments;


(*****************************************************************************)
(* This function defines the help file which should be used for context
   sensitive help. This is usually a help file for a programming language. *)
PROCEDURE [_APICALL] SetHelpFile*     (name:               WinDef.LPSTR);
VAR
  dmy:                                 WinDef.LPSTR;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in SetHelpFile ")
  END (* IF TRACE_CALLS *);
  dmy          := WinBase.lstrcpyA(SYSTEM.ADR(WinHnd.langHelpFile), name);
END SetHelpFile;


(*****************************************************************************)
(* This function is used in combination with GetNextBuffer to retrieve the contents of the 
   designated edit window via a small transfer buffer. This function has to be called once
   to start the transfer before GetNextBuffer can be used to retrieve the remainder of the
   text. 
   This function fills the transfer buffer with the first piece of text. The size of the 
   transfer buffer is defined in <size>.
   The return value of the function is the number of bytes which have been copied
   to the buffer. The end of the text has been reached when the return value is
   smaller than <size>. *)
PROCEDURE [_APICALL] GetFirstBuffer*  (edit:               WinDef.HWND;
                                       buf:                WinDef.LPSTR;
                                       size:               LONGINT)
                                      :LONGINT;
VAR
  hEdit:                               WinDef.HWND;
  ptr:                                 POINTER TO ARRAY OF CHAR;
  r:                                   LONGINT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GetFirstBuffer")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  NEW(ptr, size);
  ASSERT(ptr#NIL);
  r            := FileHnd.GetFirstBuffer(hEdit, ptr^, size);
  SYSTEM.MOVE(SYSTEM.ADR(ptr^), buf, r);
  DISPOSE(ptr);
  RETURN r;
END GetFirstBuffer;


(*****************************************************************************)
(* This function is used to retrieve the contents of the designated edit window
   via a small transfer buffer. Each call to this function fills the transfer
   buffer with the next piece of text. The size of the transfer buffer is defined
   in <size>.
   The return value of the function is the number of bytes which have been copied
   to the buffer. The end of the text has been reached when the return value is
   smaller than <size>. *)
PROCEDURE [_APICALL] GetNextBuffer*   (edit:               WinDef.HWND;
                                       buf:                WinDef.LPSTR;
                                       size:               LONGINT)
                                      :LONGINT;
VAR
  hEdit:                               WinDef.HWND;
  ptr:                                 POINTER TO ARRAY OF CHAR;
  r:                                   LONGINT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GetNextBuffer")
  END (* IF TRACE_CALLS *);
  hEdit        := WinUser.GetWindow(edit, WinUser.GW_CHILD);
  NEW(ptr, size);
  ASSERT(ptr#NIL);
  r            := FileHnd.GetNextBuffer(hEdit, ptr^, size);
  SYSTEM.MOVE(SYSTEM.ADR(ptr^), buf, r);
  DISPOSE(ptr);
  RETURN r;
END GetNextBuffer;


(*****************************************************************************)
(* The return value of this function is 1 if the editor generates
   files in plain text ASCII format and 0 if the editor uses a 
   proprietary format.
   This implementation uses plain text ASCII format. *)
PROCEDURE [_APICALL] GeneratesAscii*  ()
                                      :INTEGER;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GeneratesAscii")
  END (* IF TRACE_CALLS *);
  RETURN 1;
END GeneratesAscii;


(*****************************************************************************)
(* This function is not supported. *)
PROCEDURE [_APICALL] LoadOpen*        (File:               WinDef.LPSTR)
                                      :LONGINT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in LoadOpen");
  END (* IF TRACE_CALLS *) ;
  RETURN 0;
END LoadOpen;


(*****************************************************************************)
(* This function is not supported. *)
PROCEDURE [_APICALL] LoadRead*        (hWnd:               WinDef.HWND;
                                       Buffer:             WinDef.LPSTR;
                                       Size:               LONGINT)
                                      :LONGINT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in LoadRead");
  END (* IF TRACE_CALLS *) ;
  RETURN 0;
END LoadRead;


(*****************************************************************************)
(* This function is not supported. *)
PROCEDURE [_APICALL] LoadClose*       (hWnd:               WinDef.HWND);
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in LoadClose");
  END (* IF TRACE_CALLS *) ;
END LoadClose;


(*****************************************************************************)
(* The contents of the designated edit window is printed.
   This function displays a dialog box to determine print 
   parameters before the text is printed. *)
PROCEDURE [_APICALL] PrintWindow*     (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  win:                                 EditWin.EditWin;
  title:                               ARRAY 100 OF CHAR;
  len:                                 LONGINT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in PrintWindow ")
  END (* IF TRACE_CALLS *);
  len          := WinUser.GetWindowTextA(edit, SYSTEM.ADR(title), LEN(title) - 1);
  title[len]   := 0X;
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  RETURN Print.PrintFile(edit, win, title);
END PrintWindow;


(*****************************************************************************)
(* The zero terminated string given in <text> is inserted at the
   current cursor position. *)
PROCEDURE [_APICALL] InsertText*      (edit:               WinDef.HWND;
                                       text:               WinDef.LPSTR)
                                      :INTEGER;
VAR
  win:                                 EditWin.EditWin;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in InsertText ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  win.SetUndoAction(TextWin.ACT_NONE);
  IF win.InsertText(text) THEN
    RETURN 1
  ELSE
   RETURN 0
  END (* IF win.InsertText(text) *);
END InsertText;


(*****************************************************************************)
(* The zero terminated string given in <text> is appended to the current
   contents of the edit window. *)
PROCEDURE [_APICALL] AddText*         (edit:               WinDef.HWND;
                                       text:               WinDef.LPSTR)
                                      :INTEGER;
VAR
  win:                                 TextWin.WinDesc;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in AddText")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win#NIL THEN
    win.SetUndoAction(TextWin.ACT_NONE);
    RETURN WinHnd.AddText(win, text);
  ELSE
    RETURN 0;
  END (* IF win#NIL *);
END AddText;


(*****************************************************************************)
(* The size of the designated edit window is changed to a width of <width> pixels 
   and a height of <height> pixels. The size defines the total size of the edit 
   window and not the size of the client area. *)
PROCEDURE [_APICALL] ResizeWindow*    (edit:               WinDef.HWND;
                                       width,
                                       height:             INTEGER);
VAR
  hEdit:                               WinDef.HWND;
  win:                                 TextWin.WinDesc;
  res:                                 WinDef.BOOL;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in ResizeWindow ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  res          := WinUser.MoveWindow(win.hwnd, 0, 0, width, height, WinDef.True);
  win.ScreenConfig;
END ResizeWindow;


(*****************************************************************************)
(* The return value of the function is 1 if text is selected in 
   the edit window and 0 if no text is selected. *)
PROCEDURE [_APICALL] HasSelection*    (edit:               WinDef.HWND)
                                      :INTEGER;
VAR
  win:                                 TextWin.WinDesc;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in HasSelection ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win.text.isSelected THEN
    RETURN 1
  ELSE
    RETURN 0
  END (* IF win.text.isSelected *);
END HasSelection;


(*****************************************************************************)
(* The designated edit window is cleared. *)
PROCEDURE [_APICALL] ResetContent*    (edit:               WinDef.HWND);
VAR
  win:                                 TextWin.WinDesc;
  dummy:                               LONGINT;
  done:                                WinDef.BOOL;
  rect:                                WinDef.RECT;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in ResetContent ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win#NIL THEN
    win.text.ResetContents;
    win.ScreenConfig;
    win.row    := 1;
    win.col    := 1;
    win.textPos := 1;
    win.SetCaret;
    win.SetUndoAction(TextWin.ACT_NONE);
    done       := WinUser.InvalidateRect(win.hwnd, rect, 0);
    done       := WinUser.UpdateWindow(win.hwnd);
  ELSE
    GlobWin.Beep;
  END (* IF win#NIL *);
END ResetContent;


(*****************************************************************************)
(* A global memory object handle to the text in the current editor window
   is returned.
   This function is not supported in this editor version and therefore
   always fails. *)
PROCEDURE [_APICALL] GetText*         (edit:               WinDef.HWND)
                                      :WinDef.HGLOBAL;
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GetText ")
  END (* IF TRACE_CALLS *);
  RETURN 0;
END GetText;


(*****************************************************************************)
(* The contents of the line number <row> is copied into the buffer <buf>.
   If the line contains more than <max> characters the line is truncated to
   avoid a buffer overflow. *)
PROCEDURE [_APICALL] GetLine*         (edit:               WinDef.HWND;
                                       row,
                                       max:                INTEGER;
                                       buf:                WinDef.LPSTR)
                                      :INTEGER;

VAR
  dmy:                                 WinDef.LPSTR;
  line:                                ARRAY ListSt.MAXLENGTH OF CHAR;
  len:                                 LONGINT;
  win:                                 TextWin.WinDesc;
  copied:                              INTEGER;

BEGIN
  INC(row);
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in GetLine ")
  END (* IF TRACE_CALLS *);
  win          := EditWin.AssocWinObj(WinUser.GetWindow(edit, WinUser.GW_CHILD));
  IF win.text.GetLine(row, line, len) THEN
    IF len>max THEN
      SYSTEM.MOVE(SYSTEM.ADR(line), buf, max);
      copied   := max;
    ELSE
      SYSTEM.MOVE(SYSTEM.ADR(line), buf, len);
      copied   := SHORT(len);
    END (* IF len>max *);
  ELSE
    copied     := 0;
  END (* IF win.text.GetLine(row, line,  *);
  RETURN copied;
END GetLine;


(*****************************************************************************)
(* The editor help file is shown *)
PROCEDURE [_APICALL] ShowHelp*        (hEdit:              WinDef.HWND);
BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in ShowHelp")
  END (* IF TRACE_CALLS *);
  GlobWin.ShowHelp(hEdit);
END ShowHelp;


(*****************************************************************************)
(* A dialog window is opened which allows the user to change editor option
   settings.
   This editor implementation stores the options selected by the user in the
   file "boosted.ini". *)
PROCEDURE [_APICALL] EditOptions*     ();
VAR
  hdc:                                 WinDef.HDC;
  hObj:                                WinDef.HGDIOBJ;
  res:                                 LONGINT;
  i:                                   INTEGER;
  oldFocus,
  dhwnd:                               WinDef.HWND;
  done:                                WinDef.BOOL;
  lfHeight:                            LONGINT;

BEGIN
  IF TRACE_CALLS THEN
    GlobWin.DisplayError("", "in EditOptions")
  END (* IF TRACE_CALLS *);
  OptionDialogs.EditOptions;

  FOR i:=0 TO WinHnd.wCounter-1 DO
    hObj       := WinGDI.SelectObject(WinHnd.wList[i].hdc, WinHnd.wList[i].oldFont);
  END (* FOR i:=0 TO WinHnd.wCounter-1 *);
  IF WinHnd.wCounter>0 THEN
    IF WinGDI.DeleteObject(WinHnd.hFont)=0 THEN
      GlobWin.DisplayError("ERROR", "Can not delete current font")
    END (* IF WinGDI.DeleteObject(WinHnd.h *);
    hdc        := WinUser.GetDC(WinDef.NULL);
    lfHeight   :=  - WinBase.MulDiv(Options.fontSize, 
    WinGDI.GetDeviceCaps(hdc, WinGDI.LOGPIXELSY), 
    72);
    res        := WinUser.ReleaseDC(WinDef.NULL, hdc);
    WinHnd.hFont := WinGDI.CreateFontA(lfHeight, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    WinGDI.DEFAULT_QUALITY, 
    WinGDI.FIXED_PITCH, 
    SYSTEM.ADR(Options.fontName));
    oldFocus   := WinDef.NULL;
    FOR i:=0 TO WinHnd.wCounter-1 DO
      IF oldFocus=WinDef.NULL THEN
        oldFocus := WinUser.SetFocus(WinHnd.wList[i].hwnd);
      ELSE
        dhwnd  := WinUser.SetFocus(WinHnd.wList[i].hwnd);
      END (* IF oldFocus=WinDef.NULL *);
      WinHnd.SetWindowOldFont(i, WinGDI.SelectObject(WinHnd.wList[i].hdc, WinHnd.hFont));
      WinHnd.wList[i].ScreenConfig;
      done     := WinUser.InvalidateRect(WinHnd.wList[i].hwnd, NIL, 0);
      WinHnd.wList[i].CursGoto(WinHnd.wList[i].row, WinHnd.wList[i].col);
    END (* FOR i:=0 TO WinHnd.wCounter-1 *);
    dhwnd      := WinUser.SetFocus(oldFocus);
  END (* IF WinHnd.wCounter>0 *);
END EditOptions;


(*****************************************************************************)
(* This function is obsolete in the Win32 API based version of BoostEd.      *)
(* Cleaning up can be done in the DllEntryPoint procedure.                   *)
PROCEDURE [_APICALL] UnloadEditor*    ();
BEGIN
  ;
END UnloadEditor;


(*****************************************************************************)
(* This function is called by the NT program loader when the editor DLL is   *)
(* loaded or unloaded                                                        *)
PROCEDURE [_APICALL] DllEntryPoint*   (hInst:              WinDef.HINSTANCE;
                                       Reason:             WinDef.DWORD;
                                       Reserved:           WinDef.LPVOID)
                                      :WinDef.BOOL;
BEGIN
  CASE Reason OF
    WinNT.DLL_PROCESS_ATTACH:
      GlobWin.hInstance := hInst;                          (* remember DLL instance handle for later use *)
      IF ~WinHnd.RegisterClass() THEN                      (* register window class for editor windows *)
        GlobWin.Beep();
        RETURN WinDef.False;
      END (* IF ~WinHnd.RegisterClass() *);
      EnvHnd.ReadIniFile();
    | (* WinNT.DLL_PROCESS_ATTACH *)
    WinNT.DLL_PROCESS_DETACH:
      WinHnd.CloseAllWindows();
      WinHnd.UnregisterClass();                            (* unregister window class for editor windows *)
    ELSE
      ;
  END (* CASE Reason *) ;
  RETURN WinDef.True;
END DllEntryPoint;


(*****************************************************************************)
BEGIN
  ;
END BoostEd32.



