(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Files                                       V 2.00.31         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   functions of menu point "Files"                               *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Close     a file that is dumped                                         *)
(*   MapFile                                                                 *)
(*   Open      a file for being debugged                                     *)
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

MODULE Files;

IMPORT
  Common, Debug, Dump, Options, Resource, UIStatusLine, UITabControl, UserInterface, View,
  Strings,
  CommDLG, WinBase, WinDef, WinGDI, WinNT, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.31";
  Module*      =                      "Files";
  ErrorNoOffset=                      Resource.IDM_Files * 100;
  
  FileSpecTitle =                     "Open File.";


VAR
  FileSpec:                            CommDLG.OPENFILENAME;
  FileFilter:                          ARRAY 256 OF CHAR;
  FileSecurity:                        WinBase.SECURITY_ATTRIBUTES;
  ErrorMessage:                        ARRAY 128 OF CHAR;
  i:                                   LONGINT;
  MyFile:                              ARRAY 256 OF CHAR;
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
                                      
VAR
  MyMDIClient:                         Common.MDIClientP;

BEGIN;
  
  ResultBool   := WinBase.UnmapViewOfFile(Common.MyFileDescription.lpFileBase);
  ResultBool   := WinBase.CloseHandle(Common.MyFileDescription.hFileMapping);
  ResultBool   := WinBase.CloseHandle(Common.MyFileDescription.hFile);

  MyMDIClient  := Common.GetMDIClient(2, 1);
  IF MyMDIClient#NIL THEN
    Result       := WinUser.SendMessageA (Common.hWndMDIClient, WinUser.WM_MDIDESTROY, MyMDIClient^.hWnd, 0);
    UITabControl.Remove(MyMDIClient^.TabControl);
    Result       := Common.RemoveMDIClient(MyMDIClient);
  END (* IF MyMDIClient#NIL *);

  Result       := UserInterface.SetMenu(hWnd, SYSTEM.LOWORD(wParam));
  UIStatusLine.ShowMode(0);

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
PROCEDURE MapFile*                    ()                  
                                      :LONGINT;

VAR
  BytesRead:                           LONGINT;
  i:                                   LONGINT;

  ActSectionHeader,
  TempSectionHeader:                   Common.PSectionHeader;
  ActDebugDirectory,
  TempDebugDirectory:                  Common.PDebugDirectory;
  MyPointer,
  COFFFileHeaderPos:                   LONGINT;

  
BEGIN
  (* Open the file for read level access *)
  COPY (Common.MyFileDescription.Path, MyFile);
  Strings.Append (MyFile, Common.MyFileDescription.Name);
  Common.MyFileDescription.hFile := WinBase.CreateFileA (SYSTEM.ADR(MyFile), 
                                                WinNT.GENERIC_READ, 
                                                WinNT.FILE_SHARE_READ, 
                                                FileSecurity, 
                                                WinBase.OPEN_EXISTING, 
                                                WinNT.FILE_ATTRIBUTE_NORMAL, 
                                                0);
  IF Common.MyFileDescription.hFile=WinBase.INVALID_HANDLE_VALUE THEN
    ResultBool := WinBase.CloseHandle(Common.MyFileDescription.hFile);
    UIStatusLine.ShowMessage ("Couldn't open file with CreateFileA().");
    RETURN 1
  END (* IF FileDescription=WinBase.INVALID_HANDLE_VALUE *);
  
  (* Map file into memory and read file structure *)
  Common.MyFileDescription.hFileMapping 
                   := WinBase.CreateFileMappingA (Common.MyFileDescription.hFile, 
                                                FileSecurity, 
                                                WinNT.PAGE_WRITECOPY, 
                                                0, 
                                                0, 
                                                WinDef.NULL);
  IF Common.MyFileDescription.hFileMapping=0 THEN
    ResultBool := WinBase.CloseHandle(Common.MyFileDescription.hFileMapping);
    ResultBool := WinBase.CloseHandle(Common.MyFileDescription.hFile);
    UIStatusLine.ShowMessage ("Couldn't create file mapping with CreateFileMapping().");
    RETURN 1
  END (* IF Common.MyFileDescription.hFileMapping=0 *);

  Common.MyFileDescription.lpFileBase
                   := WinBase.MapViewOfFile (Common.MyFileDescription.hFileMapping,
                                             WinNT.SECTION_MAP_READ,
                                             0,
                                             0,
                                             0);
  IF Common.MyFileDescription.lpFileBase=0 THEN
    ResultBool := WinBase.UnmapViewOfFile(Common.MyFileDescription.lpFileBase);
    ResultBool := WinBase.CloseHandle(Common.MyFileDescription.hFileMapping);
    ResultBool := WinBase.CloseHandle(Common.MyFileDescription.hFile);
    UIStatusLine.ShowMessage ("Couldn't map file with MapViewOfFile().");
    RETURN 1
  END (* IF Common.MyFileDescription.lpFileBase=0 *);

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
  MyMDIClient:                         Common.MDIClientP;

BEGIN;
  
  IF Common.MyFileDescription.Loaded THEN
    Result := Close(hWnd, message, wParam, lParam);
  END;

  FileFilter := "Executables (*.exe)$";
  Strings.Append (FileFilter, "*.exe$");
  Strings.Append (FileFilter, "Dynamic Link Libraries (*.dll)$");
  Strings.Append (FileFilter, "*.dll$");
  Strings.Append (FileFilter, "Objectfiles (*.obj)$");
  Strings.Append (FileFilter, "*.obj$");
  Strings.Append (FileFilter, "Libraries (*.lib)$");
  Strings.Append (FileFilter, "*.lib$");
  Strings.Append (FileFilter, "All Files (*.*)$");
  Strings.Append (FileFilter, "*.*$");
  FOR i:=0 TO Strings.Length(FileFilter) DO
    IF FileFilter[i]="$"
    THEN
      FileFilter[i] := CHR(0);
    END;
  END;
  FileFilter[i]   := CHR(0);
  FileFilter[i+1] := CHR(0);
  
  MyFile[0]       :=  0X;
  
  (* Dialog aufrufen zur Ermittlung des Filenamens *)
  FileSpec.Flags         := CommDLG.OFN_PATHMUSTEXIST + CommDLG.OFN_FILEMUSTEXIST;
  Result                 := CommDLG.GetOpenFileNameA(FileSpec);

  IF Result=0 THEN
    ErrorCode := WinBase.GetLastError();
    IF ErrorCode=1400 THEN
      UIStatusLine.ShowMessage ("No File selected.");
    ELSE
      UIStatusLine.DisplayError (1, ErrorNoOffset+210);
    END;
    RETURN 1;
  END (* IF Result=0 *);

  (* separate path information *)
  COPY (MyFile, Common.MyFileDescription.Path);
  Result       := Options.RememberFile(MyFile);
  i           := Strings.Pos(Common.MyFileDescription.Name, Common.MyFileDescription.Path, 1);
  IF i>0 THEN
    Strings.Delete(Common.MyFileDescription.Path, i, Strings.Length(Common.MyFileDescription.Name));
  ELSE
    Common.MyFileDescription.Path[0] := 0X;
  END (* IF i>0  *);
  FileMessage := "Opened(Dump): ";
  Strings.Append (FileMessage, Common.MyFileDescription.Path);
  Strings.Append (FileMessage, Common.MyFileDescription.Name);
  UIStatusLine.ShowMessage (FileMessage);

  Result := MapFile();
  
  Result := Dump.DumpTheFile();

  CASE Common.MyFileDescription.FileType OF
    Common.FileTypeUnknown:
      Result :=  UserInterface.SetMenu(hWnd, 0);
    ELSE
      Result := UserInterface.SetMenu(hWnd, Resource.IDM_View_HexData);
  END;
  
  UIStatusLine.ShowMode(1);

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
  AhWnd:                               WinDef.HWND;
  FileMessage:                         ARRAY 256 OF CHAR;
  ErrorCode:                           LONGINT;
  ErrorMessage:                        ARRAY 128 OF CHAR;
  MDICreateStruct:                     WinUser.MDICREATESTRUCTA;
  MyMDIClient:                         Common.MDIClientP;
  Title:                               ARRAY 256 OF CHAR;

BEGIN;
  
  IF Common.MyFileDescription.Loaded THEN
    Result := Close(hWnd, message, wParam, lParam);
  END;

  FileFilter := "Executables (*.exe)$";
  Strings.Append (FileFilter, "*.exe$");
  Strings.Append (FileFilter, "All Files (*.*)$");
  Strings.Append (FileFilter, "*.*$");
  FOR i:=0 TO Strings.Length(FileFilter) DO
    IF FileFilter[i]="$"
    THEN
      FileFilter[i] := CHR(0);
    END;
  END;
  FileFilter[i]   := CHR(0);
  FileFilter[i+1] := CHR(0);
  
  MyFile[0]       := CHR(0);
  
  (* Dialog aufrufen zur Ermittlung des Filenamens *)
  FileSpec.Flags         := CommDLG.OFN_PATHMUSTEXIST + CommDLG.OFN_FILEMUSTEXIST;
  Result                 := CommDLG.GetOpenFileNameA(FileSpec);
  IF Result=0 THEN
    ErrorCode := WinBase.GetLastError();
    IF ErrorCode=1400 THEN
      UIStatusLine.ShowMessage ("No File selected.");
    ELSE
      UIStatusLine.DisplayError (ErrorCode, ErrorNoOffset+310);
    END;
    RETURN 1;
  END (* IF Result=0 *);

  (* separate path information *)
  COPY (MyFile, Common.MyFileDescription.Path);
  i           := Strings.Pos(Common.MyFileDescription.Name, Common.MyFileDescription.Path, 1);
  IF i>0 THEN
    Strings.Delete(Common.MyFileDescription.Path, i, Strings.Length(Common.MyFileDescription.Name));
  END (* IF i>0  *);
  FileMessage  := "Opened(Debug): ";
  Strings.Append (FileMessage, Common.MyFileDescription.Path);
  Strings.Append (FileMessage, Common.MyFileDescription.Name);
  UIStatusLine.ShowMessage (FileMessage);

  Result       := MapFile();
  IF Result>0 THEN
    RETURN 1;
  END (* IF Result=0 *);

  Result       := Dump.DumpTheFile();
  IF Result>0 THEN
    RETURN 1;
  END (* IF Result=0 *);

  CASE Common.MyFileDescription.FileType OF
    Common.FileTypeUnknown:
      Result :=  UserInterface.SetMenu(hWnd, 0);
    ELSE
      Result := UserInterface.SetMenu(hWnd, Resource.IDM_View_HexData);
  END;
  
  (* Generate MDI Debug Window *)
  MDICreateStruct.szClass      := SYSTEM.ADR(Common.DebugClass);
  MDICreateStruct.szTitle      := SYSTEM.ADR(Common.MDIDebugTitle);
  MDICreateStruct.hOwner       := Common.hInstance;
  MDICreateStruct.x            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.y            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cx           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cy           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.style        := WinUser.WS_CHILD +      (* Window style.                      *)
                                  WinUser.WS_CLIPCHILDREN + WinUser.WS_MAXIMIZE +
                                  WinUser.WS_VISIBLE + WinUser.WS_VSCROLL + WinUser.WS_HSCROLL;
  AhWnd                        := WinUser.SendMessageA(Common.hWndMDIClient, 
                                                       WinUser.WM_MDICREATE, 
                                                       0, 
                                                       SYSTEM.ADR(MDICreateStruct));

  ASSERT(AhWnd#0);
  
  Common.ActiveMDIClient       := Common.AppendMDIClient(AhWnd, Common.MyFileDescription.Path, Common.MyFileDescription.Name);
  Common.ActiveMDIClient.Mode  :=  2;
  UITabControl.Append(AhWnd, "Debug");
  
  UIStatusLine.ShowMode(2);

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

VAR
  MyMDIClient:                         Common.MDIClientP;

BEGIN;
  
  Result       := Debug.Stop(Common.DebugProcess);

  MyMDIClient  := Common.GetMDIClient(2, 2);
  IF MyMDIClient#NIL THEN
    Result       := WinUser.SendMessageA (Common.hWndMDIClient, WinUser.WM_MDIDESTROY, MyMDIClient^.hWnd, 0);
    UITabControl.Remove(MyMDIClient^.TabControl);
    Result       := Common.RemoveMDIClient(MyMDIClient);
  END (* IF MyMDIClient#NIL *);

  Result       := Close(hWnd, message, wParam, lParam);
  UIStatusLine.ShowMode(0);

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
  
  UIStatusLine.ShowMessage ("File.Print not yet implemented.");
  RETURN 1
  
END Print;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

  FileSpec.lStructSize                 := SIZE(CommDLG.OPENFILENAME);
  FileSpec.hwndOwner                   := Common.hWndMain;
  FileSpec.lpstrTitle                  := SYSTEM.ADR(FileSpecTitle);
  FileSpec.lpstrFilter                 := SYSTEM.ADR(FileFilter);
  FileSpec.lpstrInitialDir             := SYSTEM.ADR(Common.MyFileDescription.Path);
  FileSpec.lpstrFile                   := SYSTEM.ADR(MyFile);
  FileSpec.nMaxFile                    := 255;
  FileSpec.lpstrFileTitle              := SYSTEM.ADR(Common.MyFileDescription.Name);
  FileSpec.nMaxFileTitle               := 255;
  FileSpec.Flags                       := CommDLG.OFN_PATHMUSTEXIST + CommDLG.OFN_FILEMUSTEXIST;

  FileSecurity.nLength                 := SIZE(WinBase.SECURITY_ATTRIBUTES);
  FileSecurity.lpSecurityDescriptor    := 0;
  FileSecurity.bInheritHandle          := WinDef.False;

END Files.

