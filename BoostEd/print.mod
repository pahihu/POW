(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     Print                                       V 2.00.02         *)
(*                                                         2004MAY01         *)
(*  PURPOSE:   This module implements printing. This includes the dialog box *)
(*             which allows the user to choose which part of the document    *)
(*             should be printed and which printer should be used.           *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(* Author(s):                                                                *)
(*                     Michael Bogner, Max Mayrbäurl                         *)
(*             BL      Bernhard Leisch                                       *)
(*                     Alexander Bergsmann                                   *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   1998                                                                    *)
(*                                                                           *)
(*  update                                                                   *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(* Comments                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE Print;


IMPORT 
  SYSTEM, 
  CommDlg, WinBase, WinDef, WinGDI, WinUser, 
  Strings, Utils, 
  GlobWin, ListSt, Options, TextWin;


CONST
  ABORTDIALOG    =                    "AbortDialog";
  ID_ABORTBUTTON =                     211;

  MODE_ALL       =                     CommDlg.PD_ALLPAGES;
  MODE_PAGES     =                     CommDlg.PD_PAGENUMS;
  MODE_SELECTION =                     CommDlg.PD_SELECTION;

  
TYPE
  AbortProcT = PROCEDURE [_APICALL]   (hdc:                WinDef.HDC; 
                                       code:               INTEGER) 
                                      :INTEGER;


VAR
  fAbort           : BOOLEAN;           (* TRUE wenn der Benutzer den Druck abgebrochen hat *)
  hwndPDlg         : WinDef.HWND;           (* Handle des Abbruchs Druck Dialog                 *)
  szTitle          : ARRAY 256 OF CHAR; (* Globaler Zeiger auf Druckjobtitel                *)
  fileName         : ARRAY 256 OF CHAR; (* Name der zu druckenden Datei                     *)
  tabWidth         : LONGINT;           (* Breite eines Tabulators                          *)
  printMode        : INTEGER;
  fromPage         : INTEGER;
  toPage           : INTEGER;
  copies           : INTEGER;

  xOffs  : LONGINT; (* leftmost co-ordinate of print area in device units *)
  yOffs  : LONGINT; (* topmost co-ordinate of print area in device units *)
  xMax   : LONGINT; (* rightmost co-ordinate of print area in device units *)
  yMax   : LONGINT; (* maximum vertical co-ordinate of print area in device units *)
  dpiX   : LONGINT; (* number of pixels per inch along the screen width *)
  dpiY   : LONGINT; (* number of pixels per inch along the screen height *)
  textOffs:LONGINT; (* x co-ordinate for source code *)
  halfChar:LONGINT;
  lineHeight : LONGINT; (* height of a of text in device units *)

  hDCPrinter : WinDef.HDC;  (* decive context for printer *)
  hFont,
  hItalicFont: WinDef.HFONT;

  printedOnCurrentPage:BOOLEAN;


(*****************************************************************************)
PROCEDURE [_APICALL] PrintAbortProc*  (hdc:                WinDef.HDC; 
                                       code:               INTEGER) 
                                      :INTEGER;
(* wird durch GDI Druck Code aufgerufen um auf Benutzerabbruch hin zu prüfen *)

VAR
  msg:                                 WinUser.MSG;
  done:                                WinDef.BOOL;
  ok:                                  LONGINT;

BEGIN
  WHILE ~fAbort & (WinUser.PeekMessageA(msg,0,0,0,WinUser.PM_REMOVE)#0) DO
    IF WinUser.IsDialogMessageA(hwndPDlg,msg)=0 THEN
      done :=WinUser.TranslateMessage(msg);
      ok   :=WinUser.DispatchMessageA(msg); (* Nachricht senden *)
    END;
  END;
  IF fAbort THEN RETURN 0 ELSE RETURN 1 END;
END PrintAbortProc;


(*****************************************************************************)
PROCEDURE [_APICALL] AbortDlg*        (hwnd:               WinDef.HWND;
                                       message:            WinDef.UINT;
                                       wParam:             WinDef.WPARAM;
                                       lParam:             WinDef.LPARAM) 
                                      :WinDef.BOOL;
(* Dialogfunktion für Druckabbruch Dialogbox *)

VAR 
  done:                                WinDef.BOOL;

BEGIN
  CASE message OF
    WinUser.WM_INITDIALOG : 
        done := WinUser.ShowWindow(hwnd,WinUser.SW_NORMAL);
        done := WinUser.UpdateWindow(hwnd);
        RETURN 1;
    | WinUser.WM_COMMAND : 
        fAbort:=TRUE;
        done := WinUser.DestroyWindow(hwnd);
        RETURN 1;
  ELSE
    RETURN 0;
  END;
  RETURN 1;
END AbortDlg;


(*****************************************************************************)
PROCEDURE PrintHeader                 (page:               INTEGER; 
                                       VAR currentY:       LONGINT);
(* druckt die Kopfzeile einer Seite *)

VAR
  buf:                                 ARRAY 100 OF CHAR;
  res:                                 WinDef.BOOL;
  date:                                ARRAY 100 OF CHAR;
  size:                                WinDef.SIZE;
  prevFont:                            WinDef.HFONT;

BEGIN    
  printedOnCurrentPage:=TRUE;
  prevFont:=WinGDI.SelectObject(hDCPrinter,hItalicFont);
  COPY(fileName,buf);
  IF Options.printDate THEN
    Strings.Append(buf,"  ");
    Utils.GetDateStr(date);
    Strings.Append(buf,date);
  END;
  res:=WinGDI.TextOutA(hDCPrinter,xOffs,currentY,SYSTEM.ADR(buf),Strings.Length(buf));    

  Strings.Str(page,buf);          
  Strings.Insert("page ",buf,1);
  res := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR(buf),Strings.Length(buf),size);
  lineHeight := size.cy; (* Höhe einer Zeile zuweisen *)
  res:=WinGDI.TextOutA(hDCPrinter,xMax-size.cx,currentY,SYSTEM.ADR(buf),Strings.Length(buf));    
 
  IF Options.printLineNumbers THEN
    res:=WinGDI.MoveToEx(hDCPrinter,textOffs-halfChar,currentY+(lineHeight*3) DIV 2,NIL);
    res:=WinGDI.LineTo(hDCPrinter,xMax,currentY+(lineHeight*3) DIV 2);
    res:=WinGDI.MoveToEx(hDCPrinter,textOffs-halfChar,currentY+(lineHeight*3) DIV 2,NIL);
    res:=WinGDI.LineTo(hDCPrinter,textOffs-halfChar,yMax);
  ELSE
    res:=WinGDI.MoveToEx(hDCPrinter,textOffs,currentY+(lineHeight*3) DIV 2,NIL);
    res:=WinGDI.LineTo(hDCPrinter,xMax,currentY+(lineHeight*3) DIV 2);
  END;
  INC(currentY,lineHeight*2);
  prevFont:=WinGDI.SelectObject(hDCPrinter,prevFont);
END PrintHeader;


(*****************************************************************************)
PROCEDURE ShouldPrintPage             (pageNr:             LONGINT) 
                                      :BOOLEAN;
(* prüft, ob die aktuelle Seite gedruckt werden soll oder nicht *)
(* Rückgabewert : TRUE - Seite wird gedruckt, FALSE - Seite wird nicht gedruckt *)
BEGIN
  IF (printMode=MODE_ALL) OR (printMode=MODE_SELECTION) THEN 
    RETURN TRUE;
  ELSIF printMode=MODE_PAGES THEN 
    RETURN (pageNr >= fromPage) & (pageNr <= toPage); 
  END;
END ShouldPrintPage;


(*****************************************************************************)
PROCEDURE EndOfPage                   (actYPos:            LONGINT) 
                                      :BOOLEAN;
(* prüft, ob das Ende einer Seite erreicht wurde oder nicht  *)
(* Rückgabewert : TRUE - Ende der Seite, FALSE - andernfalls *)

BEGIN
  RETURN (actYPos + lineHeight) > yMax;
END EndOfPage;


(*****************************************************************************)
PROCEDURE StartNewPage                (VAR pageNr:         INTEGER; 
                                       VAR actY:           LONGINT; 
                                       height:             LONGINT) 
                                      :BOOLEAN;
(* beginnt eine neue Seite für den Druck *)
(* Rückgabewert : TRUE - erfolgreich, FALSE - Fehler aufgetreten *)

VAR
  pError:                              LONGINT;
  ok:                                  WinDef.BOOL;
BEGIN
  INC(pageNr);       (* increment page count *)
  IF ShouldPrintPage(pageNr) THEN (* should we print this page? *)
    IF printedOnCurrentPage THEN
      printedOnCurrentPage:=FALSE;
      IF WinGDI.EndPage(hDCPrinter)<=0 THEN (* close the old page *)
        pError := WinGDI.SP_ERROR;
        GlobWin.DisplayError("Internal Error","Could not finish the current page");
        RETURN FALSE;
      END;
      IF WinGDI.StartPage(hDCPrinter)<=0 THEN (* start a new page *)
        pError := WinGDI.SP_ERROR;
        GlobWin.DisplayError("Internal Error","Could not start a new page");
        RETURN FALSE;
      END;
    END;
    actY:=yOffs;
    PrintHeader(pageNr, actY); (* print the header for the current page *)
  END;
  RETURN TRUE;
END StartNewPage;


(*****************************************************************************)
PROCEDURE PrintLine                   (lineNr:             LONGINT;
                                       VAR pageNr:         INTEGER;
                                       VAR text-:          ARRAY OF CHAR; (* Text             *)
                                       VAR yPos:           LONGINT);
(* Zeile drucken *)

VAR
  lineWidth  : LONGINT; (* Breite der Zeile *)
  len        : LONGINT; (* Länge des Textes einer Zeile *)  
  textInx    : LONGINT; (* Position im Text *)  
  lenSubStr  : LONGINT; (* Länge des Teilstrings *)
  res        : LONGINT;
  size       : WinDef.SIZE;
  ok         : WinDef.BOOL;
  prevFont   : WinDef.HFONT;
  buf        : ARRAY 10 OF CHAR;
BEGIN
  IF Options.printLineNumbers & ShouldPrintPage(pageNr) THEN
    prevFont:=WinGDI.SelectObject(hDCPrinter,hItalicFont);
    Strings.Str(lineNr,buf);
    Strings.RightAlign(buf,4);
    res:=WinGDI.TextOutA(hDCPrinter,xOffs,yPos,SYSTEM.ADR(buf),Strings.Length(buf));
    printedOnCurrentPage:=TRUE;
    prevFont:=WinGDI.SelectObject(hDCPrinter,prevFont);
  END;
  
  len:=Strings.Length(text); (* Länge des Textes ermitteln *)
  
  ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR(text),len,size);
  lineWidth := size.cx; (* Breite der Zeile ermitteln *)

  IF textOffs+lineWidth-1 > xMax THEN (* Text länger als maximale Breite *)
    textInx:=0; 
    WHILE (textInx < len) & ShouldPrintPage(pageNr) DO
      lenSubStr := len - textInx;
      ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR(text)+textInx,lenSubStr,size);
      lineWidth := size.cx;
      WHILE textOffs+lineWidth-1 > xMax DO
        DEC(lenSubStr);
        ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR(text)+textInx,lenSubStr,size);
        lineWidth := size.cx;
      END;
      IF ShouldPrintPage(pageNr) THEN
        res:=WinUser.TabbedTextOutA(hDCPrinter,textOffs,yPos,SYSTEM.ADR(text)+textInx,lenSubStr,1,tabWidth,textOffs);
        printedOnCurrentPage:=TRUE;
      END;
      INC(textInx,lenSubStr);
      INC(yPos,lineHeight);
      IF EndOfPage(yPos) THEN
        IF ~StartNewPage(pageNr, yPos, lineHeight) THEN
          GlobWin.Beep;
        END;
      END; 
    END;

  ELSE (* Text passt in Zeile *)

    IF text[0]#0X THEN
      IF ShouldPrintPage(pageNr) THEN
        res:=WinUser.TabbedTextOutA(hDCPrinter,textOffs,yPos,SYSTEM.ADR(text),len,1,tabWidth,textOffs);
        printedOnCurrentPage:=TRUE;
      END;
    END;
    INC(yPos,lineHeight); 

  END;    
(*  Process.Yield;*)
END PrintLine;


(*****************************************************************************)
PROCEDURE PrintFile*                  (hwnd:               WinDef.HWND;
                                       win:                TextWin.WinDesc; 
                                       title:              ARRAY OF CHAR)
                                      :INTEGER;
(* Datei drucken *)

VAR
  hwndPDlg   : WinDef.HWND;    (* Handle für Abbruch - Dialog *)
  dInfo      : WinGDI.DOCINFO; (* Informationen für Druck *)
  abortProc  : AbortProcT; (* Abbruchprozedur *)

  ySizePage   : LONGINT; (* height in raster lines *)
  xSizePage   : LONGINT; (* width in pixels *)
  
  actPage    : INTEGER; (* aktuelle Seite *)
  actLine    : LONGINT; (* current line *)
  firstLine  : LONGINT; (* first line which is to be printed *)
  lastLine   : LONGINT; (* last line which is to be printed *)
  yExtSoFar  : LONGINT; (* Druckfortschritt - aktuelle Position *)

  ok         : WinDef.BOOL;
  size       : WinDef.SIZE;
  pError     : LONGINT; (* Druckerfehler *)
  penWidth   : LONGINT;
  printJobId : LONGINT;

  lineTxt    : ARRAY ListSt.MAXLENGTH+1 OF CHAR;
  res        : LONGINT;
  lineLen    : LONGINT;
  done       : BOOLEAN;
  txt        : ARRAY 10 OF CHAR;
  copyNr     : INTEGER; (* Schleifenvariable *)
  oldFont    : WinDef.HFONT; (* logische Schrift *)
  hPen,
  oldPen     : WinDef.HPEN;
  printDlg   : CommDlg.PDA;
  lfHeight   : LONGINT;
  devNames   : CommDlg.DEVNAMES;
  devNamesAdr: LONGINT;
  deviceName : ARRAY 300 OF CHAR;
  driverName : ARRAY 300 OF CHAR;

  PROCEDURE GetStringFromAdr(adr:LONGINT; VAR str:ARRAY OF CHAR);
  VAR
    i:INTEGER;
  BEGIN
    i:=0;
    SYSTEM.GET(adr+i,str[i]);
    WHILE (i+1<LEN(str)) & (str[i]#0X) DO
      INC(i);
      SYSTEM.GET(adr+i,str[i]);
    END;
    IF str[i]#0X THEN str[i]:=0X END; 
  END GetStringFromAdr;
  
BEGIN
  IF win=NIL THEN 
    GlobWin.DisplayError("Error","no edit window selected");
    RETURN 0;
  END;

  COPY(title,fileName);


  printDlg.lStructSize:=SIZE(CommDlg.PDA);
  printDlg.hwndOwner:=win.hwnd;
  printDlg.hDevMode:=0;
  printDlg.hDevNames:=0;
  printDlg.hDC:=0;
  printDlg.Flags:=printMode;
  printDlg.nFromPage:=fromPage;
  printDlg.nToPage:=toPage;
  printDlg.nMinPage:=1;
  printDlg.nMaxPage:=9999;
  printDlg.nCopies:=copies;
  printDlg.hInstance:=0;
  printDlg.lCustData:=0;
  printDlg.lpfnPrintHook:=NIL;
  printDlg.lpfnSetupHook:=NIL;
  printDlg.lpPrintTemplateName:=0;
  printDlg.lpSetupTemplateName:=0;
  printDlg.hPrintTemplate:=0;
  printDlg.hSetupTemplate:=0;
 
  res:=CommDlg.PrintDlgA(printDlg);
  IF res=0 THEN 
    RETURN 0;
  END;

  IF SYSTEM.BITAND(printDlg.Flags,CommDlg.PD_ALLPAGES)#0 THEN
    printMode:=MODE_ALL;
  ELSIF SYSTEM.BITAND(printDlg.Flags,CommDlg.PD_PAGENUMS)#0 THEN
    printMode:=MODE_PAGES;
  ELSIF SYSTEM.BITAND(printDlg.Flags,CommDlg.PD_SELECTION)#0 THEN
    printMode:=MODE_SELECTION;
  ELSE
    printMode:=MODE_ALL;
  END;
  fromPage:=printDlg.nFromPage;
  toPage:=printDlg.nToPage;
  copies:=printDlg.nCopies;

  devNamesAdr:=WinBase.GlobalLock(printDlg.hDevNames);
  SYSTEM.MOVE(devNamesAdr,SYSTEM.ADR(devNames),SIZE(CommDlg.DEVNAMES));
  GetStringFromAdr(devNamesAdr+devNames.wDriverOffset,driverName);
  GetStringFromAdr(devNamesAdr+devNames.wDeviceOffset,deviceName);
  res:=WinBase.GlobalUnlock(printDlg.hDevNames);
  printDlg.hDevNames:=WinBase.GlobalFree(printDlg.hDevNames);

 (* Gerätekontext für Drucker erzeugen *)
  hDCPrinter := WinGDI.CreateDCA(SYSTEM.ADR(driverName),
                             SYSTEM.ADR(deviceName),
                             WinDef.NULL,
                             NIL);
  IF hDCPrinter = 0 THEN 
    GlobWin.DisplayError("Printer Error","It was not possible to create a device context for the specified printer");
    RETURN 0; 
  END;
  
  dpiX:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.LOGPIXELSX);
  dpiY:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.LOGPIXELSY);
  
  xOffs:=WinBase.MulDiv(Options.printMarginLeft,dpiX,100);
  yOffs:=WinBase.MulDiv(Options.printMarginTop,dpiY,100);
  
  IF xOffs<WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALOFFSETX) THEN
    xOffs:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALOFFSETX);
  END;
  IF yOffs<WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALOFFSETY) THEN
    yOffs:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALOFFSETY);
  END; 
  
  xMax:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALWIDTH)-WinBase.MulDiv(Options.printMarginRight,dpiX,100);
  yMax:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.PHYSICALHEIGHT)-WinBase.MulDiv(Options.printMarginBottom,dpiY,100);
  
  ySizePage:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.VERTRES); 
  xSizePage:=WinGDI.GetDeviceCaps(hDCPrinter,WinGDI.HORZRES);
  
  IF xMax>xSizePage THEN xMax:=xSizePage END;
  IF yMax>ySizePage THEN yMax:=ySizePage END;

  lfHeight:=-WinBase.MulDiv(Options.printerFontSize, dpiY, 72);
  hFont := WinGDI.CreateFontA(lfHeight,
                          0, (* width or 0 for closest match *)
                          0, (* escapement *)
                          0, (* orientation *)
                          WinGDI.FW_DONTCARE, (* weight *)
                          0, (* italics *)
                          0, (* underline *)
                          0, (* strikeout *)
                          WinGDI.DEFAULT_CHARSET, (* character set *)
                          WinGDI.OUT_DEFAULT_PRECIS, (* output precision *)
                          WinGDI.CLIP_DEFAULT_PRECIS,   (* clipping precision *)
                          WinGDI.DEFAULT_QUALITY,
                          WinGDI.FIXED_PITCH,
                          SYSTEM.ADR(Options.printerFontName));
  hItalicFont := WinGDI.CreateFontA(lfHeight,
                          0, (* width or 0 for closest match *)
                          0, (* escapement *)
                          0, (* orientation *)
                          WinGDI.FW_DONTCARE, (* weight *)
                          1, (* italics *)
                          0, (* underline *)
                          0, (* strikeout *)
                          WinGDI.DEFAULT_CHARSET, (* character set *)
                          WinGDI.OUT_DEFAULT_PRECIS, (* output precision *)
                          WinGDI.CLIP_DEFAULT_PRECIS,   (* clipping precision *)
                          WinGDI.DEFAULT_QUALITY,
                          WinGDI.FIXED_PITCH,
                          SYSTEM.ADR(Options.printerFontName));
  IF (hFont=0) OR (hItalicFont=0) THEN
    GlobWin.DisplayError("Internal Error","Create font failed");
  END;
  oldFont := WinGDI.SelectObject(hDCPrinter,hFont);

  penWidth:= 2 (* 2 points *) * dpiX DIV 72;
  hPen := WinGDI.CreatePen(WinGDI.PS_SOLID,0,0);
  oldPen := WinGDI.SelectObject(hDCPrinter,hPen);

  (* Abbruchdialog wird erzeugt *)
  hwndPDlg:=WinUser.CreateDialogParamA(GlobWin.hInstance,SYSTEM.ADR(ABORTDIALOG),win.hwnd,AbortDlg,WinDef.NULL);
  IF hwndPDlg=0 THEN
    GlobWin.DisplayError("Internal Error","Create abort dialog failed");
    GlobWin.Beep;
    ok :=WinGDI.DeleteDC(hDCPrinter);
    RETURN 0;
  END;

  abortProc := PrintAbortProc;

  (* Abbruchprozedur einrichten, die die Nachrichten für den Abbruchdialog verarbeitet *)
  res := WinGDI.SetAbortProc(hDCPrinter, SYSTEM.VAL(WinDef.FARPROC,abortProc)); 

  fAbort:=FALSE;

  (* Initialisierung des Dokuments *)
  dInfo.cbSize:=SIZE(WinGDI.DOCINFO);
  dInfo.lpszDocName:=SYSTEM.ADR(fileName);
  dInfo.lpszOutput:=WinDef.NULL;
  dInfo.lpszDatatype:=WinDef.NULL;
  dInfo.fwType:=0;

  (* height of a line *)
  ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR("Cg"),2,size);
  lineHeight := size.cy; (* Höhe einer Zeile zuweisen *)

  (* width of a tab *)
  ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR("  "),1,size);
  tabWidth  := Options.tabsize * size.cx;
  halfChar:= size.cx DIV 2;

  IF Options.printLineNumbers THEN
    ok := WinGDI.GetTextExtentPoint32A(hDCPrinter,SYSTEM.ADR("9999:"),5,size);
    textOffs:=xOffs+size.cx;
  ELSE
    textOffs:=xOffs;
  END;

  IF printMode=MODE_SELECTION THEN 
    firstLine:=win.text.markStart.row;
    lastLine:=win.text.markEnd.row;
    IF win.text.markEnd.col<=1 THEN DEC(lastLine) END;
  ELSE
    firstLine:=1;
    lastLine:=win.text.lines;
  END;

  pError:=WinGDI.SP_ERROR+1;
  copyNr := 1; (* Initialisierung *)
  printedOnCurrentPage:=FALSE;
  WHILE (copyNr <= copies) & (~fAbort) & (pError#WinGDI.SP_ERROR) DO
  
    (* Initialisierung des zu druckenden Dokumentes *)
    actPage   := 1;
    actLine   := firstLine;
  
    (* Beginn des Drucks *)
    printJobId := WinGDI.StartDocA(hDCPrinter, dInfo);
    IF printJobId <= 0 THEN
      GlobWin.DisplayError("Internal Error","Could not create a print job");
      pError:=WinGDI.SP_ERROR;
      GlobWin.Beep;
    ELSE
      (* Erste Seite beginnen *)
      IF WinGDI.StartPage(hDCPrinter)<=0 THEN 
        pError := WinGDI.SP_ERROR;
        GlobWin.DisplayError("Internal Error","Could not start a new page");
        GlobWin.Beep;
      ELSE
        yExtSoFar:=yOffs;
        IF ShouldPrintPage(actPage) THEN PrintHeader(actPage, yExtSoFar) END;
      END;
    END;

    (* Solange noch Zeilen vorhanden sind, Text ausdrucken *)
    WHILE (actLine <= lastLine) & (~fAbort) & (pError#WinGDI.SP_ERROR) DO

      IF EndOfPage(yExtSoFar) THEN
         (* Ende einer Seite wurde erreicht *)
         IF ~StartNewPage(actPage, yExtSoFar, lineHeight) THEN
           GlobWin.Beep;
         END;
      END; 
      IF win.text.GetLine(actLine,lineTxt,lineLen) THEN
        PrintLine(actLine, actPage, lineTxt, yExtSoFar);
      END;
      INC(actLine);
    END; (* while *)

    IF (~fAbort) THEN
      (* Letzte Seite auswerfen und Dokument fertigstellen *)
      IF printedOnCurrentPage & (WinGDI.EndPage(hDCPrinter)<=0) THEN 
        pError := WinGDI.SP_ERROR;
        GlobWin.Beep;
        GlobWin.DisplayError("Internal Error","Could not finish the current page");
      ELSE
        IF WinGDI.EndDoc(hDCPrinter)<=0 THEN 
          pError := WinGDI.SP_ERROR;
          GlobWin.Beep;
          GlobWin.DisplayError("Internal Error","Could not close the current print job");
        END;
      END;
    ELSE 
      (* Druck abbrechen *)
      pError := WinGDI.AbortDoc(hDCPrinter);
      IF pError = WinGDI.SP_ERROR THEN 
        GlobWin.Beep;
        GlobWin.DisplayError("Internal Error","Could not abort the document");
      END;
    END;
            
    INC(copyNr);
  END; (* Kopien - Schleife *)

  (* Abbruchfenster schließen und Hauptfenster wiederherstellen *)
  ok := WinUser.DestroyWindow(hwndPDlg);

  oldFont := WinGDI.SelectObject(hDCPrinter,oldFont);
  oldPen := WinGDI.SelectObject(hDCPrinter,oldPen);
  ok := WinGDI.DeleteObject(hFont);
  ok := WinGDI.DeleteObject(hItalicFont);
  ok := WinGDI.DeleteObject(hPen);
  ok := WinGDI.DeleteDC(hDCPrinter); (* Gerätekontext freigeben *)
  RETURN 1;
END PrintFile;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
  printMode    := MODE_ALL;
  fromPage     :=   1;
  toPage       := 999;
  copies       :=   1;
END Print.
