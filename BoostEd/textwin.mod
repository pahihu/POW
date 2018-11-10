(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    BoostEd32                                                     *)
(*             Basic Operative Oberon Source Text EDitor                     *)
(*                                                                           *)
(* MODULE:     TextWin                                     V 2.00.80         *)
(*                                                         2005JAN16         *)
(*  PURPOSE:   This module implements the class WinDescT, which is the basic *)
(*             editor window class. The functionality supported by this      *)
(*             class includes                                                *)
(*                     text output to screen                                 *)
(*                     adding and deleting text                              *)
(*                     handling to support scroll bars                       *)
(*                     block indent and unindent                             *)
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
(*   2003OCT24 KlS Select*Color collected in 1 procedure                     *)
(*                 SelectColor(Index)                                        *)
(*                 keyword colouring added                                   *)
(*   2003NOV03 KlS strings colouring added                                   *)
(*   2004APR22 KlS local syntax colouring implemented                        *)
(*                                                                           *)
(*****************************************************************************)

MODULE TextWin;


IMPORT 
  SYSTEM,
  WinDef, WinUser, WinGDI, WinBase,
  ListSt, Utils, Strings, Syntax, WinUtils, Options, GlobMem, GlobWin;


CONST
  BRAKE        =                       40;                 (* Unterbrechungswert um horizontales Markieren zu verlangsamen *)
  HORZCOLSTEP  =                       10;                 (* Anzahl der Spalten bei horizontalem Shift *)
  ACT_PASTE*   =                        8;
  ACT_CUT*     =                        9;
  ACT_DELCHAR* =                        2;
  ACT_NONE*    =                        3;
  ACT_MERGELINE*       =                4;
  ACT_DELLINE* =                        5;
  ACT_INSERTCHAR*      =                6;                 (* undoLen enthält Anzahl der eingefügten Zeichen *)
  ACT_OVERWRITECHAR*   =                7;                 (* undoLen enthält Anzahl der überschriebenen Zeichen *)
  ACT_OVERWRITESELECTION*=             11;
  ACT_SPLITLINE*       =               12;                 (* undoLen enthält Anzahl der eingefügten Ident Leerzeichen *)
  MarkColor*           =                1;                 (* KlS, 2003OCT24 *)
  CommentColor*        =                2;
  TextColor*           =                3;
  KeywordColor*        =                4;                 (* KlS, 2003OCT24 *)
  StringsColor*        =                5;                 (* KlS, 2003NOV03 *)


TYPE
  WinDesc*     =                       POINTER TO WinDescT;

  WinDescT*    = RECORD
    Name*:                             ARRAY 256 OF CHAR;  (* name of file                KlS, 2004APR20 *)
    Extension*:                        ARRAY   8 OF CHAR;  (*                             KlS, 2004APR20 *)
    text*:                             ListSt.Text;        (* Text                                       *)
    hwnd*:                             WinDef.HWND;        (* Fensterhandle                              *)
    changed*:                          BOOLEAN;            (* Änderungen am Text ?                       *)
    row*, col*:                        LONGINT;            (* aktuelle Cursorposition                    *)
    delrow*, delcol*:                  LONGINT;            (* Cursorpos. des zuletzt gelöschten Elements *)
    lineNo*,                                               (* Anzahl Zeilen im Fenster                   *)
    lineheight*,                                           (* Höhe (abhängig von Schrift                 *)
    charwidth*,                                            (* Breite eines Zeichens                      *)
    initrowpos*,                                           (* Y - Offset Fenster - 1.Zeile               *)
    initcolpos*,                                           (* X - Offset Fenster - 1.Zeile               *)
    colPos*:                           LONGINT;            (* erste sichtbare Spalte am Bildschirm       *)
    colNo*:                            LONGINT;            (* Anzahl sichtbarer Spalten am Bildschirm    *)
    textPos*:                          LONGINT;            (* Reihennummer der 1.Zeile des Fensters      *)
    sumLen*,                                               (* Handling GetFirst/NextBuffer               *) 
    position*,                                             (* ----------- " --------------               *)
    lineNbr*:                          LONGINT;            (* Zeilenzähler in filehnd                    *)
    hdc*:                              WinDef.HDC;         (* Privater device context                    *)
    wndheight*, wndwidth*:             LONGINT;            (* Geometrie des Fensters                     *)
    textHeight*:                       LONGINT;            (* Höhe einer Textzeile                       *)
    MouseCapture*:                     BOOLEAN;            (* für Textmarkierung                         *)
    MarkProcess*:                      BOOLEAN;            (* Markierung in Arbeit ?                     *)
    backBrush*:                        WinDef.HBRUSH;      (* Pinsel für Hintergrundlöschen              *)
    undoData*:                         WinDef.HGLOBAL;     (* Handle für globale Daten, für UnDo         *)
    undoRow*, undoCol*:                LONGINT;
    undoToRow*, undoToCol*:            LONGINT;            (* Undo - Bereich                             *)
    undoAction*:                       LONGINT;
    undoLen*:                          LONGINT;
    undo*:                             BOOLEAN;            (* TRUE: undo; FALSE: redo                    *)
    oldFont*:                          WinDef.HFONT;       (* Originalschrift von Windows                *)
    markDown*:                         BOOLEAN;
    mouseX*, mouseY*:                  LONGINT;            (* Mausposition während Markierung            *)
    readOnly*:                         BOOLEAN;            (* Fenster kann nur gelesen werden            *)
  END (* WinDescT *);

  LargeStringT*=                       POINTER TO ARRAY 0FFFFFFFH OF CHAR;             (* KlS, 2003OCT24 *)


VAR
  colWnd,
  colWndText:                          WinDef.COLORREF;
  colHighlight,
  colHighlightText:                    WinDef.COLORREF;
  localTxt:                            ARRAY ListSt.MAXLENGTH+1 OF CHAR; 
                                                           (* This variable is global to reduce                               *)
                                                           (* stack usage; it should be local to each procedure that uses it. *)
                                                           (* It is used in : ShowTextLine, ShowTextRange, UnIndentMarkedBlock, DeleteLine, InsertText *)
  localStrLine:                        ARRAY ListSt.MAXLENGTH OF CHAR;  
                                                           (* this variable is global to reduce *)
                                                           (* stack usage; it should be local to each procedure that uses it *)
                                                           (* It is used in : InsertText *)


(*****************************************************************************)
PROCEDURE (VAR win: WinDescT) SetUndoAction*
                                      (action:             LONGINT);
(* abstract method *)
BEGIN
  HALT(0);
END SetUndoAction;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) SetCaret*();
VAR
  done:                                WinDef.BOOL;
BEGIN
  done := WinUser.HideCaret(win.hwnd);                     (* Caret vom Bildschirm entfernen *)
  (* Caret an angegebene Koordinaten verschieben *)  
  done := WinUser.SetCaretPos(win.initcolpos+(win.col-win.colPos)*win.charwidth,
                              (win.row-win.textPos)*win.lineheight+win.initrowpos);
  done := WinUser.ShowCaret(win.hwnd);                     (* Caret anzeigen *)
END SetCaret;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) RowVisible*(VAR row:LONGINT):BOOLEAN;
(* liefert TRUE, wenn sich die übergebene Zeile im Fensterbereich befindet, *)
(* ansonsten FALSE zurück.                                                  *)

BEGIN
  RETURN (row>=win.textPos) & (row<win.textPos+win.lineNo);
END RowVisible;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) ColVisible*(VAR col:LONGINT):BOOLEAN;
(* liefert TRUE, wenn sich die übergebene Spalte im Fensterbereich befindet, *)
(* ansonsten FALSE zurück.                                                   *)
BEGIN
  RETURN (col>=win.colPos) & (col<win.colPos+win.colNo);
END ColVisible;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) UpdateHorScrollBar*;
(* aktualisiert die Position der horizontalen Scrollbar *)
VAR
  dummy : LONGINT;
  done  : WinDef.BOOL;
BEGIN
  (* minimale und maximale Position für horizontale Bildlaufleiste setzen *)
  done := WinUser.SetScrollRange(win.hwnd,WinUser.SB_HORZ,0,ListSt.MAXLENGTH-win.colNo-1,WinDef.False);
  (* Position des Schiebefeldes setzen *)
  dummy:=WinUser.SetScrollPos(win.hwnd,
                        WinUser.SB_HORZ,
                        win.colPos-1,
                        WinDef.True);
END UpdateHorScrollBar;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) UpdateVerScrollBar*;
(* aktualisiert die Position der vertikalen Scrollbar *)
VAR
  dummy : LONGINT;
  range : LONGINT;
  done  : WinDef.BOOL;
BEGIN
  range:=win.text.lines-win.lineNo;
  IF range<0 THEN range:=0 END;
  (* minimale und maximale Position für vertikale Bildlaufleiste setzen *)
  done := WinUser.SetScrollRange(win.hwnd, WinUser.SB_VERT, 0, range, WinDef.False);
  (* Position des Schiebefeldes setzen *)
  dummy:=WinUser.SetScrollPos(win.hwnd,
                         WinUser.SB_VERT,
                         win.textPos-1,
                         WinDef.True);
END UpdateVerScrollBar;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) Init*    ();
(* Initialisierung *)
VAR
  i:                                   LONGINT;
  
BEGIN
  win.text.Init();
  win.Name[0]          :=  0X;
  win.Extension[0]     :=  0X;
  win.changed          := FALSE;
  win.row              :=  1;
  win.col              :=  1;
  win.lineNo           :=  0;             
  win.lineheight       :=  0;
  win.initrowpos       :=  0;
  win.initcolpos       :=  0;        
  win.textPos          :=  1;
  win.colPos           :=  1;
  win.colNo            :=  0;
  win.sumLen           :=  0;
  win.position         :=  0;
  win.lineNbr          :=  0;
  win.wndheight        :=  0;
  win.wndwidth         :=  0;
  win.textHeight       :=  0;
  win.MouseCapture     := FALSE;
  win.MarkProcess      := FALSE;               
  win.backBrush        := WinUser.GetSysColorBrush(WinUser.COLOR_WINDOW);
  win.mouseX           :=  1;
  win.mouseY           :=  1;
  win.readOnly         := FALSE;
  win.undoData         := WinDef.NULL;
  win.undoAction       := ACT_NONE;
  win.undoRow          :=  1;
  win.undoCol          :=  1;
  win.undo             := TRUE;
  win.undoLen          :=  0;
END Init;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) InitTextMetrics;
(* Text/Schrift Parameter konfigurieren für Bildschirm-Management *)
VAR 
  rect   : WinDef.RECT;
  res    : WinDef.BOOL;
  tm     : WinGDI.TEXTMETRIC;
  done   : WinDef.BOOL;
BEGIN
  done := WinUser.GetClientRect(win.hwnd,rect); (* Größe des Clientbereichs ermitteln *)
  win.wndwidth:=rect.right;
  win.wndheight:=rect.bottom;
  res := WinGDI.GetTextMetricsA(win.hdc,tm); (* Daten über aktuelle Schrift auslesen *)
  win.lineheight:=tm.tmHeight;
  win.charwidth :=tm.tmAveCharWidth;
  win.textHeight:=tm.tmHeight;
  win.lineNo:=win.wndheight DIV win.lineheight;
  win.colNo:=win.wndwidth DIV win.charwidth;
  win.initrowpos:=0; (* win.lineheight; *)
  win.initcolpos:=0; (* win.lineheight DIV 2; *)
END InitTextMetrics;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) ScreenConfig*;
(* Parameter für Bildschirm - Management konfigurieren *)
VAR 
  done : WinDef.BOOL;
BEGIN
  done := WinUser.ShowScrollBar(win.hwnd,WinUser.SB_VERT,WinDef.True); (* vertikale Scrollbar anzeigen *)
  win.UpdateVerScrollBar;
  IF ListSt.hs THEN 
    done := WinUser.ShowScrollBar(win.hwnd,WinUser.SB_HORZ,WinDef.True);
    done := WinUser.SetScrollRange(win.hwnd,WinUser.SB_HORZ,0,ListSt.MAXLENGTH-win.colNo-1,WinDef.True);  
    win.UpdateHorScrollBar;
  END;  
  win.InitTextMetrics;
END ScreenConfig;


(*****************************************************************************)
(* SelectColor                                                               *)
(*  update                                                                   *)
(*   2003OCT24 KlS     Select*Color collected in 1 procedure                 *)
(*                     SelectColor(Index)                                    *)
(*   2003NOV03 KlS     strings colour added                                  *)
PROCEDURE (VAR win:WinDescT) SelectColor*
                                      (Index:              LONGINT);
VAR
  ColorRef:                            WinDef.COLORREF;
  
BEGIN
  CASE Index OF
    MarkColor:
      ColorRef := WinGDI.SetTextColor(win.hdc, colHighlightText);
      ColorRef := WinGDI.SetBkColor  (win.hdc, colHighlight);
    | (* MarkColor *)
    CommentColor:
      ColorRef := WinGDI.SetTextColor(win.hdc, Options.CommentColor);
      ColorRef := WinGDI.SetBkColor  (win.hdc, colWnd); 
    | (* CommentColor *)
    KeywordColor:
      ColorRef := WinGDI.SetTextColor(win.hdc, Options.KeyWordColor);
      ColorRef := WinGDI.SetBkColor  (win.hdc, colWnd);
    | (* KeywordColor *)
    StringsColor:
      ColorRef := WinGDI.SetTextColor(win.hdc, Options.StringsColor);
      ColorRef := WinGDI.SetBkColor  (win.hdc, colWnd);
    (* StringsColor *)
    ELSE                                                   (* TextColor *)
      ColorRef := WinGDI.SetTextColor(win.hdc, colWndText);
      ColorRef := WinGDI.SetBkColor  (win.hdc, colWnd); 
  END (* CASE Index  *);
END SelectColor;


(*****************************************************************************)
(* TextOut                                                                   *)
(*  created    2003OCT24 KlS                                                 *)
PROCEDURE (VAR win:WinDescT) TextOut* (Color:              LONGINT;
                                       x,
                                       y:                  LONGINT;
                                       TextPtr:            WinDef.LPSTR;
                                       Length:             LONGINT)
                                      :WinDef.LRESULT;

VAR
  i,
  j,
  k,
  Pos1,
  Pos2:                                LONGINT;
  Result:                              WinDef.LRESULT;
  Token:                               ARRAY 64 OF CHAR;
  MyText:                              LargeStringT;

  (*-------------------------------------------------------------------------*)
  PROCEDURE AreEqual                  (Token,
                                       KeyWord:            ARRAY OF CHAR;
                                       Length:             LONGINT)
                                      :BOOLEAN;
  VAR
    i:                                 LONGINT;
  BEGIN
    FOR i:=0 TO Length-1 DO
      IF Token[i]#KeyWord[i] THEN
        RETURN FALSE
      END (* IF Token[i]#KeyWord[i]  *);
    END (* FOR i:=0 TO Length-1  *);
    RETURN TRUE
  END AreEqual;

(*---------------------------------------------------------------------------*)
BEGIN
  
  CASE Color OF
    
    TextColor:
      MyText       := SYSTEM.VAL(LargeStringT, TextPtr);
      Pos1         :=  0;
      i            :=  0;
      WHILE i<Length DO
        IF Syntax.IsIdentChar(MyText[i]) THEN 
          j  :=  1;
          WHILE ((Options.ActSyntaxColouring.KeyWords[j, 0]<MyText^[i]) & (j<=Options.ActSyntaxColouring.NoOfKeyWords)) DO
            INC(j);
          END (* WHILE ((Options.ActSyntaxColouring.KeyWords[j, 0]<MyText^[i]) & (... *);
          WHILE ((Options.ActSyntaxColouring.KeyWords[j, 0]=MyText^[i]) & (j<=Options.ActSyntaxColouring.NoOfKeyWords)) DO
            Pos2     := i;
            Syntax.GetIdent(MyText^, Token, i);
            IF (((i-Pos2)=Options.ActSyntaxColouring.KeyWordLength[j])
            & AreEqual(Token, Options.ActSyntaxColouring.KeyWords[j], Options.ActSyntaxColouring.KeyWordLength[j])
            & ((Pos2=0) OR ~Syntax.IsIdentChar(MyText^[Pos2-1]))) THEN
              win.SelectColor(TextColor);
              Result       := WinGDI.TextOutA(win.hdc, x+Pos1*win.charwidth, y, TextPtr+Pos1, Pos2-Pos1);
              win.SelectColor(KeywordColor);
              Result       := WinGDI.TextOutA(win.hdc, x+Pos2*win.charwidth, y, TextPtr+Pos2, Options.ActSyntaxColouring.KeyWordLength[j]);
              Pos1         := i;
            ELSE
              i            := Pos2;
            END (* IF (((i-Pos2)=Options.ActSyntaxColouring.KeyWordLength[j])...  *);
            INC(j);
          END (* WHILE ((Options.ActSyntaxColouring.KeyWords[j, 0]=MyText^[i]) & (... *);
        ELSIF MyText[i]=Options.ActSyntaxColouring.StringDelims[0] THEN        (* KlS, 2003NOV03 *)
          k  := Strings.PosChar(Options.ActSyntaxColouring.StringDelims[0], MyText^, i+2);
          IF k>0 THEN
            win.SelectColor(TextColor);
            Result       := WinGDI.TextOutA(win.hdc, x+Pos1*win.charwidth, y, TextPtr+Pos1, i-Pos1);
            win.SelectColor(StringsColor);
            Result       := WinGDI.TextOutA(win.hdc, x+i*win.charwidth,    y, TextPtr+i,    k-i);
            i            := k;
            Pos1         := k;
          END (* IF k>0  *);                               (* KlS, 2003NOV03 *)
        ELSIF MyText[i]=Options.ActSyntaxColouring.StringDelims[1] THEN        (* KlS, 2003NOV03 *)
          k  := Strings.PosChar(Options.ActSyntaxColouring.StringDelims[1], MyText^, i+2);
          IF k>0 THEN
            win.SelectColor(TextColor);
            Result       := WinGDI.TextOutA(win.hdc, x+Pos1*win.charwidth, y, TextPtr+Pos1, i-Pos1);
            win.SelectColor(StringsColor);
            Result       := WinGDI.TextOutA(win.hdc, x+i*win.charwidth,    y, TextPtr+i,    k-i);
            i            := k;
            Pos1         := k;
          END (* IF k>0  *);                               (* KlS, 2003NOV03 *)
        END (* IF Syntax.IsIdent(MyText[i]) *);
        INC(i);
      END (* WHILE i<Length  *);
      win.SelectColor(TextColor);
      Result       := WinGDI.TextOutA(win.hdc, x+Pos1*win.charwidth, y, TextPtr+Pos1, Length-Pos1);
    | (* TextColor *)
      
    ELSE
      win.SelectColor(Color);
      Result       := WinGDI.TextOutA(win.hdc, x, y, TextPtr, Length);
  END (* CASE Color  *);
  
  RETURN Result
  
END TextOut;


(*****************************************************************************)
PROCEDURE (VAR win: WinDescT) BackRect*
                                      (x1,
                                       y1,
                                       x2,
                                       y2:                 LONGINT);
VAR
  rect  : WinDef.RECT;
  dummy : LONGINT;
BEGIN
  IF y2-y1<=0 THEN RETURN END;
  rect.left:=x1;
  rect.top:=y1;
  rect.right:=x2+1;
  rect.bottom:=y2+1;
  dummy := WinUser.FillRect(win.hdc,rect,win.backBrush); (* Rechteck füllen *)
END BackRect;


(*****************************************************************************)
PROCEDURE IsStrChar                   (x:                  CHAR)
                                      :BOOLEAN;
VAR
  i:                                   INTEGER;
BEGIN
  i            := 0;
  WHILE (Options.ActSyntaxColouring.StringDelims[i]#0X) & (Options.ActSyntaxColouring.StringDelims[i]#x) DO
    INC(i);
  END;
  RETURN Options.ActSyntaxColouring.StringDelims[i]=x;
END IsStrChar;


(*****************************************************************************)
PROCEDURE IsCommentStartAt            (VAR txt-:           ARRAY OF CHAR;
                                       CommentString:      ARRAY OF CHAR;
                                       inx:                LONGINT)
                                      :BOOLEAN;
VAR
  sInx:                                LONGINT;
BEGIN
  sInx         := 0;
  WHILE (CommentString[sInx]#0X) & (CommentString[sInx]=txt[sInx+inx]) DO
    INC(sInx);
  END;
  RETURN CommentString[sInx]=0X;
END IsCommentStartAt;


(*****************************************************************************)
(* Textzeile ausgeben                                                        *)
(*  updated    KlS 2003OCT24   procedure TextOut implemented & integrated    *)
(*                             instead of WinGDI.TextOutA(win.hdc, ...)      *)
PROCEDURE (VAR win:WinDescT) WriteTextLine
                                      (VAR txt-:           ARRAY OF CHAR;
                                       len:                LONGINT;
                                       isCommented:        BOOLEAN;
                                       nesting:            INTEGER;
                                       row:                LONGINT);

VAR
  dummy:                               WinDef.BOOL;
  len1,
  len2,
  len3:                                LONGINT;
  x,
  y:                                   LONGINT;
  l,
  startInx,
  i:                                   LONGINT;
  colFlag,
  oldColFlag:                          INTEGER;
  strChar:                             CHAR;
  comEndInx:                           LONGINT;
  earliestComEnd:                      LONGINT;

BEGIN
  x            := win.initcolpos + win.charwidth*(1-win.colPos);
  y            := win.initrowpos + win.lineheight*(row-win.textPos);

  IF txt="" THEN
    win.BackRect(0, y, win.wndwidth-1, y+win.lineheight-1);
    RETURN;
  END;

  win.BackRect(x+win.charwidth*len, y, win.wndwidth-1, y+win.lineheight-1);

  IF Options.colorComments & isCommented THEN
    colFlag        := -1;
    startInx       :=  0;
    i              :=  0;
    strChar        :=  0X;
    comEndInx      :=  0;
    earliestComEnd :=  0;
    
    WHILE txt[i]#0X DO
      IF strChar#0X THEN
        IF txt[i]=strChar THEN
          strChar  := 0X;
        END (* IF txt[i]=strChar *);
        comEndInx  :=  0;
      ELSIF (nesting<=0) & IsStrChar(txt[i]) THEN
        strChar    := txt[i];
        comEndInx  :=  0;
      ELSIF (txt[i]=Options.ActSyntaxColouring.CommentStart[0]) & IsCommentStartAt(txt, Options.ActSyntaxColouring.CommentStart, i) THEN
        IF Options.ActSyntaxColouring.CommentsNested OR (nesting<=0) THEN
          INC(nesting);
        END;
        comEndInx      := 0;
        earliestComEnd := i + Strings.Length(Options.ActSyntaxColouring.CommentStart);
      ELSIF Options.ActSyntaxColouring.CommentEnd[comEndInx]=0X THEN
        IF Options.ActSyntaxColouring.CommentsNested OR (nesting>0) THEN
          DEC(nesting);
        END;
        comEndInx      := 0;
      ELSIF (txt[i]=Options.ActSyntaxColouring.CommentEnd[0]) & (i>=earliestComEnd) THEN
        comEndInx  :=  1;
      ELSIF (txt[i]=Options.ActSyntaxColouring.CommentEnd[comEndInx]) & (i>=earliestComEnd) THEN
        INC(comEndInx);
      ELSE
        comEndInx  :=  0;
      END (* IF strChar#0X *);
      oldColFlag := colFlag;
      IF win.text.isSelected & (((row>win.text.markStart.row) & (row<win.text.markEnd.row)) 
      OR ((row=win.text.markStart.row) & (row<win.text.markEnd.row) & ((i+1)>=win.text.markStart.col)) 
      OR ((row>win.text.markStart.row) & (row=win.text.markEnd.row) & ((i+1)<win.text.markEnd.col)) 
      OR ((row=win.text.markStart.row) & (row=win.text.markEnd.row) & ((i+1)>=win.text.markStart.col) & ((i+1)<win.text.markEnd.col))) THEN
        colFlag  :=  1;
      ELSE    
        IF nesting>0 THEN 
          colFlag  :=  2 
        ELSE 
          colFlag  :=  3 
        END (* IF nesting>0 *);
      END (* IF win.text.isSelected & (... *);
      IF ((oldColFlag#colFlag) & (oldColFlag#-1)) OR (txt[i+1]=0X) THEN
        dummy    := win.TextOut(oldColFlag, x, y, SYSTEM.ADR(txt[startInx]), i-startInx);
        x        := x + win.charwidth*(i-startInx);
        startInx := i;
      END (*  IF ((oldColFlag#colFlag) & (... *);
      INC(i);
    END (* WHILE txt[i]#0X *);
    dummy := win.TextOut(colFlag, x, y, SYSTEM.ADR(txt[i-1]), 1);

  ELSIF win.text.isSelected                                (* IF Options.colorComments & isCommented *)
  & (row>=win.text.markStart.row)& (row<=win.text.markEnd.row) THEN
    IF win.text.markStart.row=win.text.markEnd.row THEN
      len1     := win.text.markStart.col-1;
      len2     := win.text.markEnd.col-1-len1;
      len3     := len-len1-len2;
      IF len1>0 THEN
        IF Options.colorComments & (nesting>0) THEN
          dummy := win.TextOut(CommentColor, x, y, SYSTEM.ADR(txt), len1);
        ELSE
          dummy := win.TextOut(TextColor, x, y, SYSTEM.ADR(txt), len1);
        END;
      END (* IF len1>0 *);
      IF len2>0 THEN
        dummy := win.TextOut(MarkColor, x+len1*win.charwidth, y, SYSTEM.ADR(txt)+len1, len2);
      END;
      IF len3>0 THEN
        IF Options.colorComments & (nesting>0) THEN
          dummy := win.TextOut(CommentColor, x+(len1+len2)*win.charwidth, y, SYSTEM.ADR(txt)+len1+len2,len3);
        ELSE
          dummy := win.TextOut(TextColor, x+(len1+len2)*win.charwidth, y, SYSTEM.ADR(txt)+len1+len2,len3);
        END;
      END;
    ELSIF row=win.text.markStart.row THEN
      len1     := win.text.markStart.col-1;
      IF len1>0 THEN
        IF Options.colorComments & (nesting>0) THEN
          dummy := win.TextOut(CommentColor, x, y, SYSTEM.ADR(txt), len1);
        ELSE
          dummy := win.TextOut(TextColor, x, y, SYSTEM.ADR(txt), len1);
        END;
      END;
      IF len=0 THEN
        dummy := win.TextOut(MarkColor, x-win.charwidth DIV 2, y, SYSTEM.ADR("  "), 1);
      ELSE
        dummy := win.TextOut(MarkColor, x+len1*win.charwidth, y, SYSTEM.ADR(txt)+len1, len-len1);
      END;
    ELSIF row=win.text.markEnd.row THEN
      len1     := win.text.markEnd.col-1;
      IF len=0 THEN
        (* dummy := win.TextOut(MarkColor,SHORT(x-win.charwidth DIV 2),SHORT(y),SYSTEM.ADR("  "),1); *);
      ELSIF len1>0 THEN
        dummy := win.TextOut(MarkColor, x, y, SYSTEM.ADR(txt), len1);
      END (* IF len=0 *);
      IF Options.colorComments & (nesting>0) THEN
        dummy := win.TextOut(CommentColor, x+len1*win.charwidth, y, SYSTEM.ADR(txt)+len1, len-len1);
      ELSE
        dummy := win.TextOut(TextColor, x+len1*win.charwidth, y, SYSTEM.ADR(txt)+len1, len-len1);
      END;
    ELSE
      IF len>0 THEN
        dummy := win.TextOut(MarkColor, x, y, SYSTEM.ADR(txt), len);
      ELSE
        dummy := win.TextOut(MarkColor, x-win.charwidth DIV 2, y, SYSTEM.ADR("  "), 1);
      END (* IF len>0 *);
    END;

  ELSE
    IF Options.colorComments & (nesting>0) THEN
      dummy      := win.TextOut(CommentColor, x, y, SYSTEM.ADR(txt), len);
    ELSIF (Strings.Length(Options.ActSyntaxColouring.CommentLine)>0) & (Strings.Pos(Options.ActSyntaxColouring.CommentLine, txt, 1)>0) THEN
      i          := Strings.Pos(Options.ActSyntaxColouring.CommentLine, txt, 1);
      dummy      := win.TextOut(TextColor, x, y, SYSTEM.ADR(txt), i+1);
      dummy      := win.TextOut(CommentColor, x+(i-1)*win.charwidth, y, SYSTEM.ADR(txt[i-1]), len-i+1);
    ELSE
      dummy      := win.TextOut(TextColor, x, y, SYSTEM.ADR(txt), len);
    END (* IF Options.colorComments & (nesting>0) *); 
  END (* IF Options.colorComments & isCommented *);
  
END WriteTextLine;


(*****************************************************************************)
(* Ausgabe einer bestimmten Textzeile, wobei row eine absolute Zeilennummer  *)
(* ist und die 1. Zeile des Textes 1 ist                                     *)
PROCEDURE (VAR win:WinDescT) ShowTextLine*
                                      (row:                LONGINT);
VAR 
  len:                                 LONGINT;
  isCommented:                         BOOLEAN;
  h,
  nesting:                             INTEGER;
  Done:                                WinDef.BOOL;

BEGIN
  IF (row<win.textPos) OR (row>win.textPos+win.lineNo) THEN
    RETURN 
  END (* IF (row<win.textPos) OR (row>win.textPos+win.lineNo) *);

  Done := WinUser.HideCaret(win.hwnd);                     (* Caret vom Bildschirm verbergen *)
  IF ~win.text.GetLineEx(row, localTxt, len, isCommented, nesting, h) THEN 
    localTxt   := ""
  END;
  win.WriteTextLine(localTxt,len,isCommented,nesting,row);
  Done         := WinUser.ShowCaret(win.hwnd);             (* Caret anzeigen *)
END ShowTextLine;    


(*****************************************************************************)
(* Textbereich anzeigen *)
PROCEDURE (VAR win:WinDescT) ShowTextRange*(startRow,endRow:LONGINT);
VAR 
  i,dummy,x,y,len  : LONGINT;
  done             : BOOLEAN;
  isCommented      : BOOLEAN;
  h,nesting        : INTEGER;
  ok               : WinDef.BOOL;
BEGIN
  ok := WinUser.HideCaret(win.hwnd); (* Caret verbergen *)
  IF win.textPos+win.lineNo>win.text.lines THEN
    win.BackRect(0,(win.text.lines-win.textPos+1)*win.lineheight,
                 win.wndwidth-1,win.wndheight-1);
  ELSE
    win.BackRect(0,(win.lineNo+1)*win.lineheight,
                 win.wndwidth-1,win.wndheight-1);
  END;
  IF (startRow>win.textPos+win.lineNo) OR (endRow<win.textPos) THEN 
    ok := WinUser.ShowCaret(win.hwnd); (* Caret anzeigen *)
    RETURN;
  END;
  IF startRow<win.textPos THEN startRow:=win.textPos END;
  IF endRow>win.textPos+win.lineNo THEN endRow:=win.textPos+win.lineNo END;
  IF win.text.GetLineEx(startRow,localTxt,len,isCommented,nesting,h) THEN
    done:=TRUE;
    WHILE (startRow<=endRow) & done DO
      win.WriteTextLine(localTxt,len,isCommented,nesting,startRow);
      INC(startRow);
      IF startRow<=endRow THEN
        nesting:=nesting+h;
        done:=win.text.GetNextLineEx(localTxt,len,isCommented,h);
      END;
    END;
  ELSE
    win.BackRect(0,win.initrowpos+win.lineheight*(startRow-win.textPos),
                 win.wndwidth-1,win.wndheight-1);
  END;
  ok := WinUser.ShowCaret(win.hwnd); (* Caret anzeigen *)
END ShowTextRange;


(*****************************************************************************)
(* n Zeilen in angegebene Richtung scrollen, positive Werte meinen abwärts scrollen *)
PROCEDURE (VAR win:WinDescT) VerScroll*(n:LONGINT);
BEGIN
  IF (n>0) & (win.textPos+n+win.lineNo-1>win.text.lines) THEN
    win.textPos:=win.text.lines-win.lineNo+1;
  ELSE
    win.textPos:=win.textPos+n; 
  END;
  IF win.textPos<1 THEN win.textPos:=1 END;
  win.ShowTextRange(1,win.text.lines);
  win.UpdateVerScrollBar;
  win.SetCaret;
END VerScroll;


(*****************************************************************************)
(* auf einen bestimmten Wert scrollen *)
PROCEDURE (VAR win:WinDescT) VerScrollThumb*(scrpos:LONGINT);
BEGIN
  win.textPos:=scrpos;
  win.ShowTextRange(win.textPos,win.textPos+win.lineNo);
  win.UpdateVerScrollBar;
  win.SetCaret;
END VerScrollThumb;  


(*****************************************************************************)
(* horizontal scrollen - n Einheiten *)
PROCEDURE (VAR win:WinDescT) HorScroll*(n:LONGINT);
BEGIN
  win.colPos   := win.colPos + n;
  IF win.colPos<1 THEN 
    win.colPos := 1
  ELSIF win.colPos>ListSt.MAXLENGTH-win.colNo THEN 
    win.colPos := ListSt.MAXLENGTH-win.colNo 
  END;
  win.UpdateHorScrollBar;
  win.ShowTextRange(1,win.text.lines);
  win.SetCaret;
END HorScroll;
  

(*****************************************************************************)
(* horizontal auf einen bestimmten Wert scrollen *)
PROCEDURE (VAR win:WinDescT) HorScrollThumb*(scrpos:LONGINT);
BEGIN
  win.colPos:=scrpos;
  win.UpdateHorScrollBar;
  win.ShowTextRange(1,win.text.lines);
  win.SetCaret;
END HorScrollThumb;
    

(*****************************************************************************)
(* horizontale Position der Scrollbar überprüfen *)
PROCEDURE (VAR win:WinDescT) CheckHorzScrollPos*;

BEGIN
  WHILE ((win.col>=(win.colPos+win.colNo-1)) & (win.colPos<(ListSt.MAXLENGTH-win.colNo))) DO
    win.HorScroll(HORZCOLSTEP);
  END;
  WHILE (win.col<=win.colPos) & (win.colPos>1) DO 
    win.HorScroll(-HORZCOLSTEP);
  END;
END CheckHorzScrollPos;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) IndentMarkedBlock*;
VAR
  i,end         : LONGINT;
  t             : ARRAY 2 OF CHAR;
  changed,dummy : BOOLEAN;
  done          : WinDef.BOOL;
BEGIN
  IF ~win.text.isSelected THEN RETURN END;
  t:=" ";
  changed:=FALSE;
  end:=win.text.markEnd.row;
  IF win.text.markEnd.col<=1 THEN DEC(end) END;
  FOR i:=win.text.markStart.row TO end DO
    dummy:=win.text.InsertInLine(t,1,i);
    changed:=TRUE;
  END;
  IF changed THEN 
    done := WinUser.InvalidateRect(win.hwnd,NIL,0); (* Aktualisierungsbereich festlegen *)
    win.changed:=TRUE;
  END;
END IndentMarkedBlock;


(*****************************************************************************)
PROCEDURE (VAR win:WinDescT) UnIndentMarkedBlock*;
VAR
  i,end           : LONGINT;
  len             : LONGINT;
  changed,dummy   : BOOLEAN;
  done            : WinDef.BOOL;
BEGIN
  localTxt:=" ";
  changed:=FALSE;
  end:=win.text.markEnd.row;
  IF win.text.markEnd.col<=1 THEN DEC(end) END;
  FOR i:=win.text.markStart.row TO end DO
    dummy:=win.text.GetLine(i,localTxt,len);
    IF (len>0) & (localTxt[0]=" ") THEN
      dummy:=win.text.DeleteInLine(1,1,i);
      changed:=TRUE;
    END;
  END;
  IF changed THEN 
    done := WinUser.InvalidateRect(win.hwnd,NIL,0); (* Aktualisierungsbereich festlegen *)
    win.changed:=TRUE;
  END;
END UnIndentMarkedBlock;


(*****************************************************************************)
(* Markierung aktualisieren *)
PROCEDURE (VAR win:WinDescT) MarkUpdate*(row,col:LONGINT);
VAR
  swap                   : BOOLEAN;
  oldStartRow,oldEndRow  : LONGINT;
BEGIN
  oldStartRow:=win.text.markStart.row;
  oldEndRow:=win.text.markEnd.row;
  IF win.markDown THEN
    win.text.SetMarkArea(win.text.markStart.row,win.text.markStart.col,row,col);
  ELSE
    win.text.SetMarkArea(row,col,win.text.markEnd.row,win.text.markEnd.col);
  END;
  win.text.CheckMarkRange(swap);
  IF swap THEN win.markDown:=~win.markDown END;
  IF oldStartRow>win.text.markStart.row THEN oldStartRow:=win.text.markStart.row END;
  IF oldEndRow<win.text.markEnd.row THEN oldEndRow:=win.text.markEnd.row END;
  IF (win.text.markStart.row=win.text.markEnd.row) & 
     (win.text.markStart.col=win.text.markEnd.col) THEN win.text.InvalidateMarkArea END;
  win.ShowTextRange(oldStartRow,oldEndRow);
END MarkUpdate;


(*****************************************************************************)
(* Zeile löschen *)
PROCEDURE (VAR win:WinDescT) DeleteLine*;
VAR
  dmyi,len  : LONGINT;
BEGIN
  win.changed:=TRUE;
  IF win.text.isSelected THEN
    win.text.InvalidateMarkArea;
    win.ShowTextRange(win.text.markStart.row,win.text.markEnd.row);
  END;
  IF win.text.GetLine(win.row, localTxt, len) THEN 
    win.SetUndoAction(ACT_DELLINE);
    GlobMem.NewLineBuf(win.undoData);
    GlobMem.CopyString(win.undoData,localTxt);  
  ELSE
    GlobWin.Beep;
    RETURN;
  END;    
  IF win.text.DeleteLine(win.row) THEN
    win.UpdateVerScrollBar;
    win.ShowTextRange(win.row,win.row+win.lineNo);
  ELSE
    GlobWin.Beep;
  END;    
END DeleteLine;


(*****************************************************************************)
(* Text an aktueller Caretposition einfügen  *)
(* Rückgabewert : TRUE (erfolgreich), FALSE (Fehler) *)
PROCEDURE (VAR win:WinDescT) InsertText*(source:WinDef.LPSTR):BOOLEAN;
VAR 
  dmy                     : WinDef.LPSTR;
  i,j                     : LONGINT;
  linelen                 : LONGINT;
  merge, update,dmyb      : BOOLEAN;
  offset,xoffset,strlen,r : LONGINT;
  text                    : LargeStringT;
  done                    : WinDef.BOOL;
  rect                    : WinDef.RECT;
  
  PROCEDURE NextLineFromSource(VAR line:ARRAY OF CHAR):BOOLEAN;
  VAR 
    pos, endpos  : LONGINT;
    moreText     : BOOLEAN;
  BEGIN
    pos:=i;
    endpos:=-1;
    WHILE (text^[i]#0DX) & (text^[i]#0AX) & (text^[i]#0X) DO
      IF i-pos<ListSt.MAXLENGTH-1 THEN
        line[i-pos]:=text^[i];
        endpos:=i-pos;
      END;
      INC(i);
    END;
    moreText:=text^[i]#0X;
    IF text^[i]=0DX THEN INC(i) END;
    IF text^[i]=0AX THEN INC(i) END;
    line[endpos+1]:=0X;
    RETURN moreText;
  END NextLineFromSource;
  
BEGIN
  text:=SYSTEM.VAL(LargeStringT,source);
  i:=0;
  merge:=FALSE;
  offset:=0; xoffset:=0;
  
  IF NextLineFromSource(localTxt) THEN   (* there are more lines *)
    update:=win.text.GetLine(win.row, localStrLine, strlen);
    linelen:=Strings.Length(localTxt);
    IF update THEN
      IF win.col+linelen>=ListSt.MAXLENGTH THEN 
        GlobWin.DisplayError("ERROR","Buffer exceeds maximal linelength");
        RETURN FALSE 
      END;
      IF win.col-1<=strlen THEN     (* at/before 0X *)
        merge:=TRUE;
        dmyb:=win.text.SplitLine(win.row, (win.col)-1, 0);
        ASSERT(dmyb);
      ELSE                                (* after 0X *)
        FOR j:=strlen TO win.col-2 DO
          localStrLine[j]:=" ";
        END;
      END;
      FOR j:=win.col-1 TO win.col-1+linelen DO
        localStrLine[j]:=localTxt[j-win.col+1];
      END;
      dmyb:=win.text.SetLine(win.row, localStrLine);
      ASSERT(dmyb);
      offset:=1;        
    ELSE
      dmyb:=win.text.AddLine(localTxt);
      offset:=win.text.lines-win.row+1;
    END;
    
    WHILE NextLineFromSource(localTxt) DO (* insert whole lines *)
      IF offset=1 THEN
        dmyb:=win.text.InsertLine(localTxt, win.row+offset);
      ELSE
        dmyb:=win.text.InsertNextLine(localTxt);
      END;
      IF ~dmyb THEN GlobWin.Beep END;
      INC(offset);
    END;
    (* insert the last line *)
    dmyb:=win.text.InsertLine(localTxt, win.row+offset);
    IF dmyb & merge THEN
      dmyb:=win.text.MergeLines(win.row+offset);
    END;
    IF ~dmyb THEN GlobWin.Beep END;
    
  ELSE  (* work within a line *)
    update:=win.text.GetLine(win.row, localStrLine, strlen);
    linelen:=Strings.Length(localTxt);
    
    IF update THEN
      IF strlen+linelen>=ListSt.MAXLENGTH THEN 
        GlobWin.DisplayError("ERROR","Buffer exceeds maximal linelength");
        RETURN FALSE; 
      END;
      IF win.col-1<=strlen THEN     (* bei/bevor 0X *)
        FOR j:=strlen TO win.col-1 BY -1 DO (* shift back *)
          localStrLine[j+linelen]:=localStrLine[j];
        END;
        xoffset:=win.col-1;
        FOR j:=win.col-1 TO win.col-1+linelen-1 DO
          localStrLine[j]:=localTxt[j-win.col+1];
        END;
      ELSE                                (* nach 0X *)
        FOR j:=strlen TO win.col-2 DO
          localStrLine[j]:=" ";
        END;                       
        IF win.col+linelen>ListSt.MAXLENGTH THEN 
          GlobWin.DisplayError("ERROR","Buffer exceeds maximal linelength");
          RETURN FALSE; 
        END;
        xoffset:=win.col-1;
        FOR j:=win.col-1 TO win.col-1+linelen DO
          localStrLine[j]:=localTxt[j-win.col+1];
        END;
      END;
      dmyb:=win.text.SetLine( win.row, localStrLine);
      ASSERT(dmyb);
    ELSE
      dmyb:=win.text.AddLine(localTxt);
      ASSERT(dmyb);
    END;
  END;
  win.row:=win.row+offset;
  win.col:=Strings.Length(localTxt)+1+xoffset;
  IF win.textPos+win.lineNo<=win.row THEN 
    win.textPos:=win.row-win.lineNo+1;
    done := WinUser.InvalidateRect(win.hwnd, rect, 0); (* Aktualisierungsbereich festlegen *)
    win.UpdateVerScrollBar;
  END;
  win.ShowTextRange(win.textPos,win.textPos+win.lineNo);
  win.SetCaret;
  RETURN TRUE;
END InsertText;


(*****************************************************************************)
(* Globalen Speicher Buffer an aktueller Position einfügen *)
PROCEDURE (VAR win:WinDescT) InsertGlobMem*(globMem:WinDef.HGLOBAL):BOOLEAN;
VAR
  dummy,lpCopy : LONGINT;
  done         : BOOLEAN;
BEGIN
  lpCopy:=WinBase.GlobalLock(globMem); (* globalen Speicherbereich sperren *)
  IF lpCopy#WinDef.NULL THEN 
    done:=win.InsertText(lpCopy);
    dummy:=WinBase.GlobalUnlock(globMem); (* Sperren aufheben *)
  ELSE 
    done:=FALSE;
  END;
  RETURN done;
END InsertGlobMem;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
  
  colWnd               := WinUser.GetSysColor(WinUser.COLOR_WINDOW);
  colWndText           := WinUser.GetSysColor(WinUser.COLOR_WINDOWTEXT);
  colHighlight         := WinUser.GetSysColor(WinUser.COLOR_HIGHLIGHT);
  colHighlightText     := WinUser.GetSysColor(WinUser.COLOR_HIGHLIGHTTEXT);
  
END TextWin.
