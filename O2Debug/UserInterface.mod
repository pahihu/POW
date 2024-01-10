(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     UserInterface                               V 2.00.16         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   base module for the User Interface (UI)                       *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   SetMenu   enables & disables menu points                                *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000APR10                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE UserInterface;

IMPORT
  Common, Dump, DumpDataDir, DumpDataDirDebug, DumpSections, DumpHexData,
  Resource, UIStatusLine, UIToolBar,
  Strings, 
  CommCTRL, WinBase, WinDef, WinGDI, WinUser, WinNT,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.16";
  Module*      =                      "UserInterface";
  ErrorNoOffset=                      Resource.IDM_UserInterface * 100;
  
  LineEmpty    =                      "                                                                 ";
  
  
TYPE
  LineFormat   = RECORD
    Font:                              WinDef.HFONT;
    Height:                            REAL;               (* Line Spacing *)
  END (* LineFormat *);
  

VAR
  hDC:                                 WinDef.HDC;
  hInst:                               WinDef.HANDLE;
  
  ReturnCode:                          LONGINT;
  i:                                   INTEGER;
  NumberOfColumns,
  NumberOfLines:                       LONGINT;
  ActLine,
  MyLine:                              Common.ScreenLineP;
  HeadLines:                           ARRAY 4 OF Common.ScreenLineT;
  
  WriteLines:                          Common.WriteLinesT;


(*****************************************************************************)
(*                                                                           *)
(* SetMenu                                                                   *)
(* sets the menus                                                            *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       the window's handle                                           *)
(*  Mode       refer to Global Display Modus                                 *)
(*             0       first time                                            *)
(*             1       Mode HexData                                          *)
(*             2       Mode DebugData                                        *)
(*             3       Mode SymbolTable                                      *)
(*             4       Mode LineNumbers                                      *)
(*             5       Mode FileLayout                                       *)
(*             6       Mode DataDirectories                                  *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE SetMenu*                    (hWnd:               WinDef.HWND;
                                       Mode:               INTEGER)        
                                      :WinDef.LRESULT;

VAR
  Done:                                BOOLEAN;
  hMenu:                               WinDef.HMENU;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  ResulthGDIobj:                       WinDef.HGDIOBJ;
  i, j:                                LONGINT;

BEGIN;

  hMenu        := WinUser.GetMenu(hWnd);
  
  CASE Mode OF

    0:
      (* Gray out all items that are not implemented *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Close, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_DISABLED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Stop,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Stop, WinDef. False);
    
      (* Gray out all items that are not useful without having a file opened *)
      FOR i:=301 TO 399 DO
        ResultBool   := WinUser.EnableMenuItem(hMenu, i, WinUser.MF_GRAYED);
      END (* FOR i:=300 TO 399 *);
    | (* 0 *)

    Resource.IDM_View_HexData, Resource.IDM_View_Dump, Resource.IDM_View_SymbolTable, Resource.IDM_View_SymbolTableGlobal, Resource.IDM_View_LineNumbers:
      IF Common.PMyNT_Header#NIL THEN
        ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_Dump, WinUser.MF_ENABLED);
        ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_SectionHeaders, WinUser.MF_ENABLED);
        IF Common.FirstDebugDirectory#NIL THEN
          ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_SymbolTable,       WinUser.MF_ENABLED);
          ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_SymbolTableGlobal, WinUser.MF_ENABLED);
          ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_LineNumbers,       WinUser.MF_ENABLED);
        END (* IF Common.FirstDebugDirectory#NIL *);
        FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 DO
          IF Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress>0 THEN
            ResultBool   := WinUser.EnableMenuItem(hMenu, (Resource.IDM_View_DD_Export+i), WinUser.MF_ENABLED);
          END (* IF Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress>0 *);
        END (* FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 *);
      END (* IF Common.PMyNT_Header#NIL *);
      IF Common.MyFileDescription.FileType=Common.FileTypeLIB THEN
        ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_Dump, WinUser.MF_ENABLED);
      END;
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_View_HexData, WinUser.MF_ENABLED);
      CASE Mode OF
        Resource.IDM_View_HexData:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_HexData, WinUser.MF_CHECKED);
        |
        Resource.IDM_View_Dump:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_Dump, WinUser.MF_CHECKED);
        |
        Resource.IDM_View_SymbolTable:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_SymbolTable, WinUser.MF_CHECKED);
        |
        Resource.IDM_View_SymbolTableGlobal:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_SymbolTableGlobal, WinUser.MF_CHECKED);
        |
        Resource.IDM_View_LineNumbers:
          ResultBool   := WinUser.CheckMenuItem(hMenu, Resource.IDM_View_LineNumbers, WinUser.MF_CHECKED);
        ELSE
          ;
      END (* CASE Mode *);
    | (* Common.ModeHexData, Common.DebugDataMode, Common.SymbolTableMode, Common.LineNumbersMode *)

    Resource.IDM_File_Open:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Open,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Open, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Close,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Close, WinDef.True);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Start,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Start, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Attach, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Stop,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Stop, WinDef.False);
    | (* Resource.IDM_File_Open *)

    Resource.IDM_File_Close:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Open,   WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Open, WinDef.True);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Close, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Start,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Start, WinDef.True);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Attach, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Stop,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Stop, WinDef.False);
    | (* Resource.IDM_File_Close *)

    Resource.IDM_File_Start:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Open,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Open, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Close, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Start,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Start, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Attach, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Stop,   WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Stop, WinDef.True);
    | (* Resource.IDM_File_Start *)

    Resource.IDM_File_Attach:
      ;
    | (* Resource.IDM_File_Attach *)

    Resource.IDM_File_Stop:
      (* Gray out all items that are not useful in this state *)
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Open,   WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Open, WinDef.True);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Close,  WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Close, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Start,  WinUser.MF_ENABLED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Start, WinDef.True);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Attach, WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Attach, WinDef.False);
      ResultBool   := WinUser.EnableMenuItem(hMenu, Resource.IDM_File_Stop,   WinUser.MF_GRAYED);
      ResultBool   := WinUser.SendMessageA(UIToolBar.hWndToolBar, CommCTRL.TB_ENABLEBUTTON, Resource.IDM_File_Stop, WinDef.False);
    (* Resource.IDM_File_Stop *)

    ELSE
      ;
  END (* CASE Mode *);

  RETURN 0;

END SetMenu;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;
  ;
END UserInterface.

