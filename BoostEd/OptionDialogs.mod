(******************************************************************************
 *  Module OptionDialogs
 *  
 *  
 ******************************************************************************)


MODULE OptionDialogs;

IMPORT
  SYSTEM, 
  WinDef, WinUser, 
  Utils, Strings, 
  GlobWin, Options, Env:=EnvHnd, ListSt;


CONST
  ID_DISP_COURIER      =               122;
  ID_DISP_COURIERNEW   =               123;
  ID_DISP_FIXEDSYS     =               124;
  ID_DISP_FONTSIZE     =               125;
  ID_PRNT_COURIER      =               132;
  ID_PRNT_COURIERNEW   =               133;
  ID_PRNT_FIXEDSYS     =               134;
  ID_PRNT_FONTSIZE     =               135;
  ID_OKFNT             =               140;
  ID_CANCELFNT         =               141;

  ID_INDENT            =               102;
  ID_USETABS           =               103;
  ID_TABSIZE           =               104;
  ID_RBNOTHING         =               105;
  ID_RBSEARCH          =               106;
  ID_FONT              =               108;
  ID_OK                =               1;
  ID_CANCEL            =               2;
  ID_HELP              =               998;
  ID_SYNTAX            =               121;
  ID_SMARTMERGE        =               120;
  ID_INDENTWIDTH       =               122;
  ID_COLORCOMMENTS     =               140;


(*****************************************************************************)
(* CallBack-Funktion für Font-Dialog                                         *)
(*****************************************************************************)


(*****************************************************************************)
(*                                                                           *)
(* <ProcedureName>                                                           *)
(*  .                                                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  -          .                                                             *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  -          .                                                             *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  .                                                                        *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE [_APICALL] FontOptionsDlgProc*
                                      (hDlg:               WinDef.HWND;
                                       message:            WinDef.UINT;
                                       wParam:             WinDef.WPARAM;
                                       lParam:             WinDef.LPARAM)
                                      :WinDef.BOOL;
CONST
  MINFONTSIZE          =               6;
  MAXFONTSIZE          =               72;

VAR
  tmp:                                 ARRAY 12 OF CHAR;
  res:                                 WinDef.LRESULT;
  dmyi,
  val:                                 LONGINT;
  Done:                                WinDef.BOOL;
  Code:                                WinDef.WORD;

BEGIN
  CASE message OF

    WinUser.WM_INITDIALOG:
      Done     := WinUser.CheckDlgButton(hDlg, ID_DISP_FIXEDSYS, 0);
      Done     := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIER, 0);
      Done     := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIERNEW, 0);
      Done     := WinUser.CheckDlgButton(hDlg, ID_PRNT_FIXEDSYS, 0);
      Done     := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIER, 0);
      Done     := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIERNEW, 0);

      (* eingestellte Schriftart auswählen *)
      IF Options.fontName=Options.FONT_FIXEDSYS THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_FIXEDSYS, 1);
      ELSIF Options.fontName=Options.FONT_COURIER THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIER, 1);
      ELSIF Options.fontName=Options.FONT_COURIERNEW THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIERNEW, 1);
      END (* IF Options.fontName=Options.FON *);

      IF Options.printerFontName=Options.FONT_FIXEDSYS THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_FIXEDSYS, 1);
      ELSIF Options.printerFontName=Options.FONT_COURIER THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIER, 1);
      ELSIF Options.printerFontName=Options.FONT_COURIERNEW THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIERNEW, 1);
      END (* IF Options.printerFontName=Opti *);

      (* eingestellte Schriftgröße auswählen *)
      Strings.Str(Options.fontSize, tmp);
      Done     := WinUser.SetWindowTextA(WinUser.GetDlgItem(hDlg, ID_DISP_FONTSIZE), SYSTEM.ADR(tmp[0]));

      Strings.Str(Options.printerFontSize, tmp);
      Done     := WinUser.SetWindowTextA(WinUser.GetDlgItem(hDlg, ID_PRNT_FONTSIZE), SYSTEM.ADR(tmp[0]));
    | (* WinUser.WM_INITDIALOG *)

    WinUser.WM_COMMAND:
      Code     := Utils.LoWord(wParam);
      (* Code auslesen *)
      IF Code=ID_OKFNT THEN
        IF WinUser.IsDlgButtonChecked(hDlg, ID_DISP_FIXEDSYS)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_FIXEDSYS, Options.fontName);
        ELSIF WinUser.IsDlgButtonChecked(hDlg, ID_DISP_COURIER)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_COURIER, Options.fontName);
        ELSIF WinUser.IsDlgButtonChecked(hDlg, ID_DISP_COURIERNEW)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_COURIERNEW, Options.fontName);
        END (* IF WinUser.IsDlgButtonChecked(h *);
        IF WinUser.IsDlgButtonChecked(hDlg, ID_PRNT_FIXEDSYS)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_FIXEDSYS, Options.printerFontName);
        ELSIF WinUser.IsDlgButtonChecked(hDlg, ID_PRNT_COURIER)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_COURIER, Options.printerFontName);
        ELSIF WinUser.IsDlgButtonChecked(hDlg, ID_PRNT_COURIERNEW)=1 THEN
          (* ist markiert *)
          COPY(Options.FONT_COURIERNEW, Options.printerFontName);
        END (* IF WinUser.IsDlgButtonChecked(h *);
        dmyi   := WinUser.GetWindowTextA(WinUser.GetDlgItem(hDlg, ID_DISP_FONTSIZE), SYSTEM.ADR(tmp), 10);
        Options.fontSize := Strings.Val(tmp);
        dmyi   := WinUser.GetWindowTextA(WinUser.GetDlgItem(hDlg, ID_PRNT_FONTSIZE), SYSTEM.ADR(tmp), 10);
        Options.printerFontSize := Strings.Val(tmp);
        IF Options.fontSize<MINFONTSIZE THEN
          Options.fontSize := MINFONTSIZE
        END (* IF Options.fontSize<MINFONTSIZE *);
        IF Options.fontSize>MAXFONTSIZE THEN
          Options.fontSize := MAXFONTSIZE
        END (* IF Options.fontSize>MAXFONTSIZE *);
        IF Options.printerFontSize<MINFONTSIZE THEN
          Options.printerFontSize := MINFONTSIZE
        END (* IF Options.printerFontSize<MINF *);
        IF Options.printerFontSize>MAXFONTSIZE THEN
          Options.printerFontSize := MAXFONTSIZE
        END (* IF Options.printerFontSize>MAXF *);
        Done   := WinUser.EndDialog(hDlg, wParam);

      ELSIF Code=ID_CANCELFNT THEN
        Done   := WinUser.EndDialog(hDlg, wParam);

      ELSIF Code=ID_DISP_FIXEDSYS THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_FIXEDSYS, 1);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIER, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIERNEW, 0);

      ELSIF Code=ID_DISP_COURIER THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_FIXEDSYS, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIER, 1);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIERNEW, 0);

      ELSIF Code=ID_DISP_COURIERNEW THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_FIXEDSYS, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIER, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_DISP_COURIERNEW, 1);

      ELSIF Code=ID_PRNT_FIXEDSYS THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_FIXEDSYS, 1);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIER, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIERNEW, 0);

      ELSIF Code=ID_PRNT_COURIER THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_FIXEDSYS, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIER, 1);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIERNEW, 0);

      ELSIF Code=ID_PRNT_COURIERNEW THEN
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_FIXEDSYS, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIER, 0);
        Done   := WinUser.CheckDlgButton(hDlg, ID_PRNT_COURIERNEW, 1);

      ELSE
      END (* IF Code=ID_OKFNT *) ;
      (* WM_COMMAND *)

    ELSE
      ;
  END (* CASE message *);

  RETURN 0;
END FontOptionsDlgProc;


(*****************************************************************************)
PROCEDURE SelectFont                  ();
(* Schriftdialogbox erzeugen *)
VAR
  proc:                                WinDef.FARPROC;
  nres:                                LONGINT;
BEGIN
  nres         := WinUser.DialogBoxParamA(GlobWin.hInstance, SYSTEM.ADR("DIALOG_2"), WinDef.NULL, FontOptionsDlgProc, WinDef.NULL);
  IF nres= - 1 THEN
    GlobWin.DisplayError("error", "Could not display dialogbox");
  END (* IF nres= - 1 *);
END SelectFont;


(*****************************************************************************)
(*       CallBack-Funktion für Edit-Dialog                                   *)
(*****************************************************************************)


(*****************************************************************************)
(*                                                                           *)
(* <ProcedureName>                                                           *)
(*  .                                                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  -          .                                                             *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  -          .                                                             *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*  .                                                                        *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE [_APICALL] EditOptionsDlgProc*
                                      (hDlg:               WinDef.HWND;
                                       message:            WinDef.UINT;
                                       wParam:             WinDef.WPARAM;
                                       lParam:             WinDef.LPARAM)
                                      :WinDef.BOOL;

VAR
  tabstr,
  str:                                 ARRAY 12 OF CHAR;
  res:                                 WinDef.LRESULT;
  dmyi,
  val:                                 LONGINT;
  high:                                WinDef.BYTE;
  hObj:                                WinDef.HGDIOBJ;
  fontsel:                             ARRAY 32 OF CHAR;
  hChild,
  hChildfirst:                         WinDef.HWND;
  Done:                                WinDef.BOOL;
  code:                                WinDef.WORD;

BEGIN
  CASE message OF

    WinUser.WM_INITDIALOG:                                 (* WM_INITDIALOG *)
      Options.TmpSave;
      IF Options.autoIndent THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.autoIndent *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_INDENT, WinUser.BM_SETCHECK, val, 0);
      IF Options.useTabs THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.useTabs *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_USETABS, WinUser.BM_SETCHECK, val, 0);
      IF ~Options.mouse THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF ~Options.mouse *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_RBNOTHING, WinUser.BM_SETCHECK, val, 0);
      IF Options.mouse THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.mouse *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_RBSEARCH, WinUser.BM_SETCHECK, val, 0);
      IF Options.syntax THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.syntax *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_SYNTAX, WinUser.BM_SETCHECK, val, 0);
      IF Options.smartDel THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.smartDel *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_SMARTMERGE, WinUser.BM_SETCHECK, val, 0);
      Strings.Str(Options.indentWidth, str);
      Done     := WinUser.SetWindowTextA(WinUser.GetDlgItem(hDlg, ID_INDENTWIDTH), SYSTEM.ADR(str));
      Strings.Str(Options.tabsize, tabstr);
      Done     := WinUser.SetWindowTextA(WinUser.GetDlgItem(hDlg, ID_TABSIZE), SYSTEM.ADR(tabstr));
      IF Options.colorComments THEN
        val    := 1
      ELSE
       val     := 0;
      END (* IF Options.colorComments *);
      res      := WinUser.SendDlgItemMessageA(hDlg, ID_COLORCOMMENTS, WinUser.BM_SETCHECK, val, 0);
    | (* WinUser.WM_INITDIALOG *)
    WinUser.WM_COMMAND:                                    (* WM_COMMAND *)
      code     := Utils.LoWord(wParam);
      (* Code auslesen *)
      IF code=ID_OK THEN
        Options.autoIndent := SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_INDENT, WinUser.BM_GETCHECK, 0, 0));
        Options.useTabs := SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_USETABS, WinUser.BM_GETCHECK, 0, 0));
        Options.mouse := ~SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_RBNOTHING, WinUser.BM_GETCHECK, 0, 0));
        Options.colorComments := SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_COLORCOMMENTS, WinUser.BM_GETCHECK, 0, 0));
        IF Options.useTabs THEN
          dmyi := WinUser.GetWindowTextA(WinUser.GetDlgItem(hDlg, ID_TABSIZE), SYSTEM.ADR(tabstr), 10);
          Options.tabsize := Strings.Val(tabstr);
          IF (Options.tabsize<0) OR (Options.tabsize>40) THEN
            Options.tabsize := 0;
          END (* IF (Options.tabsize<0) OR (Opti *);
        ELSE
          Options.tabsize := 0;
        END (* IF Options.useTabs *);
        Options.syntax := SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_SYNTAX, WinUser.BM_GETCHECK, 0, 0));
        Options.smartDel := SYSTEM.VAL(BOOLEAN, WinUser.SendDlgItemMessageA(hDlg, ID_SMARTMERGE, WinUser.BM_GETCHECK, 0, 0));
        IF Options.syntax THEN
          dmyi := WinUser.GetWindowTextA(WinUser.GetDlgItem(hDlg, ID_INDENTWIDTH), SYSTEM.ADR(str), 10);
          Options.indentWidth := Strings.Val(str);
          IF (Options.indentWidth<0) OR (Options.indentWidth>10) THEN
            Options.indentWidth := 0;
          END (* IF (Options.indentWidth<0) OR ( *);
        ELSE
          Options.indentWidth := 0;
        END (* IF Options.syntax *);
        Env.WriteIniFile();
        Done   := WinUser.EndDialog(hDlg, wParam);

      ELSIF code=ID_CANCEL THEN
        Options.Restore;
        Done   := WinUser.EndDialog(hDlg, wParam);

      ELSIF code=ID_RBNOTHING THEN
        IF Options.mouse THEN
          res  := WinUser.SendDlgItemMessageA(hDlg, ID_RBNOTHING, WinUser.BM_SETCHECK, WinDef.True, 0);
          res  := WinUser.SendDlgItemMessageA(hDlg, ID_RBSEARCH, WinUser.BM_SETCHECK, WinDef.False, 0);
        END (* IF Options.mouse *);

      ELSIF code=ID_RBSEARCH THEN
        IF ~Options.mouse THEN
          res  := WinUser.SendDlgItemMessageA(hDlg, ID_RBSEARCH, WinUser.BM_SETCHECK, WinDef.True, 0);
          res  := WinUser.SendDlgItemMessageA(hDlg, ID_RBNOTHING, WinUser.BM_SETCHECK, WinDef.False, 0);
        END (* IF ~Options.mouse *);

      ELSIF code=ID_HELP THEN
        GlobWin.ShowHelp(hDlg);

      ELSIF code=ID_FONT THEN
        SelectFont();
      ELSE
      END (* IF code=ID_OK *);

    ELSE
  END (* CASE message *);
  RETURN 0;
END EditOptionsDlgProc;


(*****************************************************************************)
PROCEDURE EditOptions*                ();
VAR
  res:                                 LONGINT;
BEGIN
  Options.TmpSave;
  res          := WinUser.DialogBoxParamA(GlobWin.hInstance, SYSTEM.ADR("DIALOG_1"), WinDef.NULL, EditOptionsDlgProc, WinDef.NULL);
  IF res=ID_CANCEL THEN
    Options.Restore;
  ELSIF res= - 1 THEN
    GlobWin.DisplayError("error", "Could not display dialogbox");
  END (* IF res=ID_CANCEL *);
END EditOptions;

(*****************************************************************************)
(*****************************************************************************)
BEGIN
  ;
END OptionDialogs.

