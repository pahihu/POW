(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     File                                        V 1.42.02         *)
(*                                                         2002MAR29         *)
(*  PURPOSE:   functions of menu point "File"                                *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Close     a file that is dumped                                         *)
(*   MapFile                                                                 *)
(*   Open      a file for being dumped                                       *)
(*   Start     the debugger                                                  *)
(*   Stop      the debugger                                                  *)
(*   Print     ?                                                             *)
(*                                                                           *)
(*  COMMENTS:                                                                *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000APR10                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000SEP29 überarbeitet                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE File;

IMPORT
  CommDLG, WinBase, WinDef, WinGDI, WinNT, WinUser,
  Strings, SYSTEM,
  Debug, Dump, Global, StatusLine, View;


CONST
  Version =                           "V 1.42.02";
  FileSpecTitle =                     "Open File.";


VAR
  ErrorMessage:                        ARRAY 128 OF CHAR;
  i:                                   INTEGER;
  Rectangle:                           WinDef.RECT;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;


(*****************************************************************************)
(*                                                                           *)
(* Close                                                                     *)
(* the file                                                                  *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  Close*                     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;
  
  ResultBool := WinBase.UnmapViewOfFile(Global.MyFileDescription.lpFileBase);
  ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFileMapping);
  ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFile);
  
  Result     := WinUser.SendMessageA (Global.hWndMDIClient, WinUser.WM_MDIDESTROY, Global.hWndDump, 0);

  RETURN 0
  
END Close;


(*****************************************************************************)
(*                                                                           *)
(* MapFile                                                                   *)
(* Maps a file                                                               *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE MapFile*                    ()                  :LONGINT;

VAR
  BytesRead:                           LONGINT;
  i:                                   LONGINT;

  ActSectionHeader,
  TempSectionHeader:                   Global.PSectionHeader;
  ActDebugDirectory,
  TempDebugDirectory:                  Global.PDebugDirectory;
  MyPointer,
  COFFFileHeaderPos:                   LONGINT;

  
BEGIN
  (* Open the file for read level access *)
  Global.MyFileDescription.hFile := WinBase.CreateFileA (SYSTEM.ADR(Global.MyFileDescription.Path), 
                                                WinNT.GENERIC_READ, 
                                                WinNT.FILE_SHARE_READ, 
                                                Global.FileSecurity, 
                                                WinBase.OPEN_EXISTING, 
                                                WinNT.FILE_ATTRIBUTE_NORMAL, 
                                                0);
  IF Global.MyFileDescription.hFile=WinBase.INVALID_HANDLE_VALUE THEN
    ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFile);
    StatusLine.SetText ("Couldn't open file with CreateFileA().", StatusLine.NoticeField);
    RETURN 1
  END (* IF FileDescription=WinBase.INVALID_HANDLE_VALUE *);
  
  (* Map file into memory and read file structure *)
  Global.MyFileDescription.hFileMapping 
                   := WinBase.CreateFileMappingA (Global.MyFileDescription.hFile, 
                                                Global.FileSecurity, 
                                                WinNT.PAGE_WRITECOPY, 
                                                0, 
                                                0, 
                                                WinDef.NULL);
  IF Global.MyFileDescription.hFileMapping=0 THEN
    ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFileMapping);
    ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFile);
    StatusLine.SetText ("Couldn't create file mapping with CreateFileMapping().", StatusLine.NoticeField);
    RETURN 1
  END (* IF Global.MyFileDescription.hFileMapping=0 *);

  Global.MyFileDescription.lpFileBase
                   := WinBase.MapViewOfFile (Global.MyFileDescription.hFileMapping,
                                             WinNT.SECTION_MAP_READ,
                                             0,
                                             0,
                                             0);
  IF Global.MyFileDescription.lpFileBase=0 THEN
    ResultBool := WinBase.UnmapViewOfFile(Global.MyFileDescription.lpFileBase);
    ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFileMapping);
    ResultBool := WinBase.CloseHandle(Global.MyFileDescription.hFile);
    StatusLine.SetText ("Couldn't map file with MapViewOfFile().", StatusLine.NoticeField);
    RETURN 1
  END (* IF Global.MyFileDescription.lpFileBase=0 *);

  RETURN 0;

END MapFile;


(*****************************************************************************)
(*                                                                           *)
(* Open                                                                      *)
(* Opens a file                                                              *)
(*             reads hexdata                                                 *)
(*             reads PE informations                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  Open*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

VAR
  FileMessage:                         ARRAY 256 OF CHAR;
  ErrorCode:                           LONGINT;
  ErrorMessage:                        ARRAY 128 OF CHAR;

BEGIN;
  
  IF Global.MyFileDescription.Loaded THEN
    Result := Close(hWnd, message, wParam, lParam);
  END;

  Global.FileFilter := "Executables (*.exe)$";
  Strings.Append (Global.FileFilter, "*.exe$");
  Strings.Append (Global.FileFilter, "Dynamic Link Libraries (*.dll)$");
  Strings.Append (Global.FileFilter, "*.dll$");
  Strings.Append (Global.FileFilter, "Objectfiles (*.obj)$");
  Strings.Append (Global.FileFilter, "*.obj$");
  Strings.Append (Global.FileFilter, "Libraries (*.lib)$");
  Strings.Append (Global.FileFilter, "*.lib$");
  Strings.Append (Global.FileFilter, "All Files (*.*)$");
  Strings.Append (Global.FileFilter, "*.*$");
  FOR i:=0 TO Strings.Length(Global.FileFilter) DO
    IF Global.FileFilter[i]="$"
    THEN
      Global.FileFilter[i] := CHR(0);
    END;
  END;
  Global.FileFilter[i]   := CHR(0);
  Global.FileFilter[i+1] := CHR(0);
  
  (* Dialog aufrufen zur Ermittlung des Filenamens *)
  Global.FileSpec.lStructSize      := SIZE(CommDLG.OPENFILENAME);
  Global.FileSpec.hwndOwner        := hWnd;
  Global.FileSpec.lpstrTitle       := SYSTEM.ADR(FileSpecTitle);
  Global.FileSpec.lpstrFilter      := SYSTEM.ADR(Global.FileFilter);
  Global.MyFileDescription.Path[0] := 0X;
  Global.FileSpec.lpstrFile        := SYSTEM.ADR(Global.MyFileDescription.Path);
  Global.FileSpec.nMaxFile         := 255;
  Global.MyFileDescription.Name[0] := 0X;
  Global.FileSpec.lpstrFileTitle   := SYSTEM.ADR(Global.MyFileDescription.Name);
  Global.FileSpec.nMaxFileTitle    := 255;
  Global.FileSpec.Flags            := CommDLG.OFN_PATHMUSTEXIST + CommDLG.OFN_FILEMUSTEXIST;
  Result                           := CommDLG.GetOpenFileNameA(Global.FileSpec);
  IF Result=0 THEN
    ErrorCode := WinBase.GetLastError();
    IF ErrorCode=1400 THEN
      StatusLine.SetText ("No File selected.", StatusLine.NoticeField);
    ELSE
      StatusLine.DisplayError (1);
    END;
    RETURN 1;
  ELSE
    FileMessage := "Opened(Dump): ";
    Strings.Append (FileMessage, Global.MyFileDescription.Path);
    Strings.AppendChar(FileMessage, CHR(0));
    StatusLine.SetText (FileMessage, StatusLine.NoticeField);
  END (* IF Result=0 *);
  
  RETURN 0;
  
END Open;


(*****************************************************************************)
(*                                                                           *)
(* Start                                                                     *)
(* Opens a file, starts it and views the execution                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  Start*                     (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

VAR
  FileMessage:                         ARRAY 256 OF CHAR;
  ErrorCode:                           LONGINT;
  ErrorMessage:                        ARRAY 128 OF CHAR;
  MDICreateStruct:                     WinUser.MDICREATESTRUCTA;
  Title:                               ARRAY 256 OF CHAR;

BEGIN;
  
  IF Global.MyFileDescription.Loaded THEN
    Result := Close(hWnd, message, wParam, lParam);
  END;

  Global.FileFilter := "Executables (*.exe)$";
  Strings.Append (Global.FileFilter, "*.exe$");
  Strings.Append (Global.FileFilter, "All Files (*.*)$");
  Strings.Append (Global.FileFilter, "*.*$");
  FOR i:=0 TO Strings.Length(Global.FileFilter) DO
    IF Global.FileFilter[i]="$"
    THEN
      Global.FileFilter[i] := CHR(0);
    END;
  END;
  Global.FileFilter[i]   := CHR(0);
  Global.FileFilter[i+1] := CHR(0);
  
  (* Dialog aufrufen zur Ermittlung des Filenamens *)
  Global.FileSpec.lStructSize      := SIZE(CommDLG.OPENFILENAME);
  Global.FileSpec.hwndOwner        := hWnd;
  Global.FileSpec.lpstrTitle       := SYSTEM.ADR(FileSpecTitle);
  Global.FileSpec.lpstrFilter      := SYSTEM.ADR(Global.FileFilter);
  Global.MyFileDescription.Path[0] := 0X;
  Global.FileSpec.lpstrFile        := SYSTEM.ADR(Global.MyFileDescription.Path);
  Global.FileSpec.nMaxFile         := 255;
  Global.MyFileDescription.Name[0] := 0X;
  Global.FileSpec.lpstrFileTitle   := SYSTEM.ADR(Global.MyFileDescription.Name);
  Global.FileSpec.nMaxFileTitle    := 255;
  Global.FileSpec.Flags            := CommDLG.OFN_PATHMUSTEXIST + CommDLG.OFN_FILEMUSTEXIST;
  Result                           := CommDLG.GetOpenFileNameA(Global.FileSpec);
  IF Result=0 THEN
    ErrorCode := WinBase.GetLastError();
    IF ErrorCode=1400 THEN
      StatusLine.SetText ("No File selected.", StatusLine.NoticeField);
    ELSE
      StatusLine.DisplayError (1);
    END;
    RETURN 1;
  ELSE
    FileMessage := "Opened(Debug): ";
    Strings.Append (FileMessage, Global.MyFileDescription.Path);
    Strings.AppendChar(FileMessage, CHR(0));
    StatusLine.SetText (FileMessage, StatusLine.NoticeField);
  END (* IF Result=0 *);

  (* Generate MDI Debug Window *)
  MDICreateStruct.szClass      := SYSTEM.ADR(Global.DebugClass);
  MDICreateStruct.szTitle      := SYSTEM.ADR(Global.MDIDebugTitle);
  MDICreateStruct.hOwner       := Global.hInstance;
  MDICreateStruct.x            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.y            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cx           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cy           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.style        := WinUser.WS_CHILD +      (* Window style.                      *)
                                  WinUser.WS_CLIPCHILDREN +
                                  WinUser.WS_VISIBLE + WinUser.WS_VSCROLL + WinUser.WS_HSCROLL;
  Global.hWndDebug             := WinUser.SendMessageA(Global.hWndMDIClient, 
                                                       WinUser.WM_MDICREATE, 
                                                       0, 
                                                       SYSTEM.ADR(MDICreateStruct));

  ASSERT(Global.hWndDebug#0);

  RETURN 0;
  
END Start;


(*****************************************************************************)
(*                                                                           *)
(* Stop                                                                      *)
(* Debugging and close all windows                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  Stop*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;
  
  Result     := Debug.Stop(Global.DebugProcess);
  Result     := Close(hWnd, message, wParam, lParam);

  Result     := WinUser.SendMessageA (Global.hWndMDIClient, WinUser.WM_MDIDESTROY, Global.hWndDebug, 0);

  RETURN 0
  
END Stop;


(*****************************************************************************)
(*                                                                           *)
(* Print                                                                     *)
(* not yet implemented                                                       *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Print*                      (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM)      (* additional information *)
                                      :WinDef.LRESULT;

BEGIN;
  
  StatusLine.SetText ("File.Print not yet implemented.", StatusLine.NoticeField);
  RETURN 1
  
END Print;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  Global.FileSecurity.nLength                 := SIZE(WinBase.SECURITY_ATTRIBUTES);
  Global.FileSecurity.lpSecurityDescriptor    := 0;
  Global.FileSecurity.bInheritHandle          := WinDef.False;

END File.

