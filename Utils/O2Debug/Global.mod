(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Global                                      V 1.42.06         *)
(*                                                         2002APR14         *)
(*  PURPOSE:   Definition of global values                                   *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   ReadBytes                                                               *)
(*   ReadInteger                                                             *)
(*   ReadLongInt                                                             *)
(*   ReadFirstBlock                                                          *)
(*   ReadNextBlock                                                           *)
(*   ImageDirectoryOffset                                                    *)
(*   GetSectionHeader                                                        *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000SEP15                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000NOV05 KlS                                                           *)
(*             Procudere DisplayErrors implemented                           *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Global;

IMPORT
  SYSTEM,
  Strings,
  CommDLG, WinBase, WinDef, WinNT, WinUser,
  StatusLine;

CONST
  Version*             =              "V 1.42.06";
  Module*              =              "Global";

  RES_Icon*            =              "MyIcon";
  Cursor02*            =              "Cursor02";
  MainClass*           =              "O2Debug";           (* window class names *)
  DumpClass*           =              "DumpPECOFF";
  DebugClass*          =              "DebugThread";

  MainTitle*           =              "Oberon-2 Debugger";
  MDIDumpTitle*        =              "Dump File";
  MDIDebugTitle*       =              "Debug Program";
  MDIDumpCVTitle*      =              "Codeview Data";
 
  (* Menu IDs *)
  IDM_File_Open*    =                  102;                (* Menu Files *)
  IDM_File_Close*   =                  103;
  IDM_File_Start*   =                  111;
  IDM_File_Attach*  =                  112;
  IDM_File_Suspend* =                  113;
  IDM_File_Resume*  =                  114;
  IDM_File_Stop*    =                  115;
  IDM_File_Print*   =                  121;
  IDM_File_Printer* =                  122;
  IDM_File_Exit*    =                  199;

  IDM_Edit_Search*   =                 201;                (* Menu Edit *)
  IDM_Edit_Replace*  =                 202; 
  IDM_Edit_Copy*     =                 203;
  IDM_Edit_Cut*      =                 204;
  IDM_Edit_Paste*    =                 205;
  
  IDM_View_HexData*  =                 301;                (* Menu View *)
  IDM_View_Dump*     =                 302; 
  IDM_View_SectionHeaders* =           303;
  IDM_View_DD_Export*         =        330;
  IDM_View_DD_Import*         =        331;
  IDM_View_DD_Resource*       =        332;
  IDM_View_DD_Exception*      =        333;
  IDM_View_DD_Security*       =        334;
  IDM_View_DD_BaseRelocation* =        335;
  IDM_View_DD_Debug*   =               336;
  IDM_View_DD_Copyright*      =        337;
  IDM_View_DD_GlobalPtr*      =        338;
  IDM_View_DD_TLS*            =        339;
  IDM_View_DD_LoadConfig*     =        310;
  IDM_View_DD_BoundImport*    =        311;
  IDM_View_DD_IAT*            =        312;
  IDM_View_SymbolTable* =              305;
  IDM_View_LineNumbers* =              306;
  IDM_View_SymbolTableGlobal*  =       307;
  
  IDM_Window_Tile_hor*         =       401;                (* Menu Window *)
  IDM_Window_Tile_ver*         =       402;
  IDM_Window_Cascade*          =       403;
  IDM_Window_Arrange*          =       404;
  IDM_Window_Close*            =       405;
  IDM_Window_First_Child*      =       406;
  
  IDM_Admin_Font*              =       500;                (* Menu Administration *)
  IDM_Admin_Zero*              =       520;
  IDM_Admin_First*             =       521;
  IDM_Admin_Second*            =       522; 
  IDM_Admin_Third*             =       523;
  IDM_Admin_Fourth*            =       524; 
  
  IDM_Help*          =                 601;                (* Menu Help *)
  IDM_Help_About*    =                 602;
  
  (* Resource Names *)
  IDM_Main*          =                "MainMenu";
  IDD_HelpAbout*     =                "AboutBox";

  (* Buttons *)
  IDP_HelpAbout_Dialog*              = 601;
  IDP_HelpAbout_OK*                  = 609;
  
  (* Enumerator "Display Mode" *)
  Invalid*             =                 0;
  HexDataMode*         =                10;
  DebugDataMode*       =                20;
  SectionHeadersMode*  =                30;
  DD_ExportMode*       =                40;
  DD_ImportMode*       =                41;
  DD_ResourceMode*     =                42;
  DD_ExceptionMode*    =                43;
  DD_SecurityMode*     =                44;
  DD_BaseRelocationMode* =              45;
  DD_DebugMode*        =                46;
  DD_CopyrightMode*    =                47;
  DD_GlobalPtrMode*    =                48;
  DD_TLSMode*          =                49;
  DD_LoadConfigMode*   =                50;
  DD_BoundImportMode*  =                51;
  DD_IATMode*          =                52;
  SymbolTableMode*     =                60;
  SymbolTableGlobalMode*       =        61;
  LineNumbersMode*     =                70;
  FileLayoutMode*      =                80;

  (* Enumerator "File Type" *)
  FileTypeUnknown*     =                 0;
  FileTypeEXE*         =                 1;
  FileTypeOBJ*         =                 2;
  FileTypeDLL*         =                 3;
  FileTypeLIB*         =                11;
  FileTypeImpLIB*      =                12;

  (* Enumerator "Type of Paragraph" *)
  Header1*             =                 1;
  Header2*             =                 2;
  Header3*             =                 3;
  Text01*              =                11;
  Text02*              =                12;
  Text03*              =                13;
  
  (* character attributes, used while creating a logical font *)
  EZ_ATTR_BOLD*        =                 1;
  EZ_ATTR_ITALIC*      =                 2;
  EZ_ATTR_UNDERLINE*   =                 4;
  EZ_ATTR_STRIKEOUT*   =                 8;

  (* Enumerator "Move Screen Content" *)
  LineUp*              =                 1;
  LineDown*            =                 2;
  PageUp*              =                 3;
  PageDown*            =                 4;
  Home*                =                 5;
  End*                 =                 6;
  Left*                =                11;
  Right*               =                12;

  (* Data Definition Constants *)
  BlockLength* =                    1024;
  LineLength*  =                      16;
  
  Max_Path_Length*     =             256;
  
  WXBScreenMetricP*    =               0;
  WXBDebugProcessP*    =               4;


TYPE
  Byte4Array*  =                       ARRAY 4 OF SYSTEM.BYTE;
  LongIntP     =                       POINTER TO Byte4Array;
  
  ArchiveFileSignatureP*       =       POINTER TO ARRAY 8 OF CHAR;

  (* global type definitions concerning the file data *)
  FileDescriptionP*=                   POINTER TO FileDescription;
  FileDescription* =   RECORD
                         hFile*,
                         hFileMapping*:WinDef.HFILE;
                         lpFileBase*:  LONGINT;
                         FileType*:    INTEGER;
                         Loaded*:      BOOLEAN;
                         Name*:        ARRAY 256 OF CHAR;
                         Path*:        ARRAY 256 OF CHAR;
                       END (* FileDescription *);

  (* global type definitions concerning HexData *)
  HexBlockP*   = POINTER TO HexBlock;
  HexBlock     = RECORD
                   Number:             LONGINT;
                   Previous, Next:     HexBlockP;
                   Byte:               ARRAY BlockLength OF SYSTEM.BYTE
                 END (* HexBlock *);

  (* global type definitions concerning the PECOFF structure *)
  PDebugDirectory*
               =                       POINTER TO DebugDirectory;
  DebugDirectory*
               = RECORD
                   Next*:              PDebugDirectory;
                   PDebugDirectory*:   WinNT.PIMAGE_DEBUG_DIRECTORY
                 END (* DebugDirectory *);
  PSectionHeader*
               =                       POINTER TO SectionHeader;
  SectionHeader*
               = RECORD
                   Next*:              PSectionHeader;
                   PSectionHeader*:    WinNT.PIMAGE_SECTION_HEADER
                 END (* SectionHeader *);

  (* global type definitions concerning data displaying *)
  LineOfTextT* =                       ARRAY 128 OF CHAR;
  ScreenLineP* =                       POINTER TO ScreenLineT;
  ScreenLineT* =   RECORD
                     Text*:            LineOfTextT;
                     Type*:            INTEGER;            (* holds information describing the line, *)
                                                           (* only used in Dump_DD_debug             *)
                     Format*:          INTEGER;            (* Enumerator "Type Of Paragraph"         *)
                     Next*,
                     Previous*:        ScreenLineP;
                     Usage*:           INTEGER;            (* Bit 0 = 1: generate area for mouse actions *)
                     Area*:            WinDef.RECT;
                   END (* RECORD ScreenLineT *);
  ScreenMetricP*   =                   POINTER TO ScreenMetricT;
  ScreenMetricT* = RECORD
                     NumberOfFirstLine*,                   (* relative to screen *)
                     NumberOfLines*,                       (* number of lines read by the appropriate routine *)
                     LinesOnScreen*,                       (* depends on the actual size of the window *)
                     NumberOfFirstColumn*,                 (* relative to screen *)
                     NumberOfColumns*,                     (* number of lines read by the appropriate routine *)
                     ColumnsOnScreen*: LONGINT;            (* depends on the actual size of the window *)
                     FirstLineOnScreen*,
                     FirstLine*,                           (* anchor *)
                     HeadLines*:       ScreenLineP;        (* anchor *)
                     Formatted*:       BOOLEAN;
                   END (* ScreenMetric *);
  WriteLinesT* =   PROCEDURE          
                    (FirstLineP:       ScreenLineP;
                    VAR NumberOfColumns:
                                       LONGINT)
                    :                  LONGINT;

  (* define structures for debugging processes and threads *)
  DebugThreadP* =                      POINTER TO DebugThreadT;
  DebugThreadT* =  RECORD
                     hThread*:         WinDef.HANDLE;
                     dwThreadID*:      WinDef.DWORD;
                     lpStartAddress*:  WinBase.LPTHREAD_START_ROUTINE;
                     bfActive*:        WinDef.BOOL;
                     nPriority*:       INTEGER;
                     Next*:            DebugThreadP
                   END (* DebugThread *);

  DebugProcessP* =                     POINTER TO DebugProcessT;
  DebugProcessT* = RECORD
                     hDbgHeap*:        WinDef.HANDLE;
                     dwProcessID*,
                     dwThreadID*:      WinDef.DWORD;
                     hProcess*,
                     hFile*:           WinDef.HANDLE;
                     lpImage*:         WinDef.LPVOID;                     
                     dwDbgInfoOffset*,
                     nDbgInfoSize*:    WinDef.DWORD;
                     lpThreads*:       DebugThreadP;
                     hWnd*:            WinDef.HWND;
                     ProcessPriority*: INTEGER;
                     hThread*:         WinDef.HANDLE;
                     ThreadPriority*:  INTEGER;
                     szModule*:        ARRAY Max_Path_Length OF CHAR;
                     FirstBreakpoint*: BOOLEAN;
                   END (* DebugProcess *);


VAR
  hInstance*:                          WinDef.HINSTANCE;
  nCmdShow*:                           LONGINT;
  hWndMain*,
  hWndMDIClient*,
  hWndDebug*,
  hWndDumpCV*,
  hWndDump*:                           WinDef.HWND;

  DisplayMode*:                        INTEGER;
                                       (* holds values of const Display Mode *)
  Level*:                              INTEGER;
                                       (* 0 Display everything *)
                                       (* 1 medium level of information displayed *)
                                       (* 2 low level of information displayed *)

  MyFileDescription*:                  FileDescription;
  FileData*:                           ARRAY BlockLength OF SYSTEM.BYTE;
  FileDataLength*:                     LONGINT;
  FileSpec*:                           CommDLG.OPENFILENAME;
  FileFilter*:                         ARRAY 256 OF CHAR;
  FileSecurity*:                       WinBase.SECURITY_ATTRIBUTES;
  
  MyLongIntP:                          LongIntP;

  (* PECOFF structure *)
  PMyDOS_Header*:                      WinNT.PIMAGE_DOS_HEADER;
  PMyNT_Header*:                       WinNT.PIMAGE_NT_HEADERS;
  ActSectionHeader,
  FirstSectionHeader*:                 (* indefinite number of items, so it is realized as chain *)
                                       PSectionHeader;
  FirstDebugDirectory*:                (* indefinite number of items, so it is realized as chain *)
                                       PDebugDirectory;
                                       
  (* Library Structure *)
  PArchiveFileSignature*:              ArchiveFileSignatureP;
  PMyImageArchiveHeader1*,
  PMyImageArchiveHeader2*:             WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER;
  (* HexData storage variables *)
  FirstBlock,                          (* indefinite number of items, so it is realized as chain *)
  LastBlock,
  ActBlock, ActBlock2:                 HexBlockP;
  FileLength:                          LONGINT;

  i, j:                                INTEGER;
  ReturnCode:                          LONGINT;

  DebugProcess*:                       DebugProcessP;


(*****************************************************************************)
(*                                                                           *)
(* ReadBytes                                                                 *)
(* Reads a given number of Bytes                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LRESULT   wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadBytes*                  (Address:            LONGINT;
                                       Number:             LONGINT;
                                       VAR ByteString:     ARRAY OF SYSTEM.BYTE)
                                      :                    LONGINT;

VAR
  BlockNo,
  AddressInBlock,
  Index,
  NumberOfBytes:                       LONGINT;
  
BEGIN
  BlockNo          := Address DIV BlockLength;
  AddressInBlock   := Address MOD BlockLength;
  NumberOfBytes    := Number;
  
  IF ActBlock.Number<BlockNo THEN
    WHILE ActBlock.Number<BlockNo DO
      IF ActBlock.Next=NIL THEN
        RETURN 0
      END (* IF ActBlock.Next=NIL *);
      ActBlock := ActBlock.Next;
    END (* WHILE ActBlock.Number<BlockNo *);
  END (* IF ActBlock.Number<BlockNo *);
  IF ActBlock.Number>BlockNo THEN
    WHILE ActBlock.Number>BlockNo DO
      ActBlock := ActBlock.Previous;
    END (* WHILE ActBlock.Number<BlockNo *);
  END (* IF ActBlock.Number<BlockNo *);
  
  Index        :=  0;
  WHILE NumberOfBytes>0 DO
    ByteString[Index]  := ActBlock.Byte[AddressInBlock];
    INC(Index);
    INC(AddressInBlock);
    IF AddressInBlock>=BlockLength THEN
      IF ActBlock.Next=NIL THEN
        NumberOfBytes    := 0;
      ELSE
        ActBlock         := ActBlock.Next;
        AddressInBlock   := 0;
      END;
    END(* IF AddressInBlock>=BlockLength *);
    DEC(NumberOfBytes);
  END(* WHILE NumberOfBytes>0 *);
  
(*  ByteString[Index]    := 0X;*)
  RETURN Index;

END ReadBytes;


(*****************************************************************************)
(*                                                                           *)
(* ReadInteger                                                               *)
(* Reads an INTEGER Variable                                                 *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   INTEGER                                                                 *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadInteger*                (Address:            LONGINT)
                                      :                    INTEGER;

VAR
  BytesRead:                           LONGINT;
  MyBytes:                             ARRAY 3 OF SYSTEM.BYTE;
  
BEGIN
  BytesRead        := ReadBytes(Address, 2, MyBytes);
  MyLongIntP^[0]   := MyBytes[0];
  MyLongIntP^[1]   := MyBytes[1];
  
  RETURN SYSTEM.VAL(INTEGER, MyLongIntP^);

END ReadInteger;


(*****************************************************************************)
(*                                                                           *)
(* ReadLongInt                                                               *)
(* Reads a LONGINT Variable                                                  *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LONGINT                                                                 *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadLongInt*                (Address:            LONGINT)
                                      :                    LONGINT;

VAR
  BytesRead:                           LONGINT;
  i:                                   INTEGER;
  MyBytes:                             ARRAY 5 OF SYSTEM.BYTE;
  
BEGIN
  BytesRead        := ReadBytes(Address, 4, MyBytes);
  FOR i:=0 TO 3 DO
    MyLongIntP^[i] := MyBytes[i];
  END;
  
  RETURN SYSTEM.VAL(LONGINT, MyLongIntP^);

END ReadLongInt;


(*****************************************************************************)
(*                                                                           *)
(* ReadFirstBlock                                                            *)
(* Liest den ersten Block einer Datei, alte Daten werden gelöscht!!          *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LRESULT   wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadFirstBlock*             (RawDataBlock:       ARRAY OF SYSTEM.BYTE; 
                                       Number:             LONGINT);

VAR
  i:                                   INTEGER;
  
  
BEGIN
  
  (* Folgeblöcke vernichten 
  ActBlock := FirstBlock.Next;
  REPEAT
    ActBlock2 := ActBlock.Next;
    DISPOSE(ActBlock);
  UNTIL ActBlock2=NIL;
*)
  FOR i:=0 TO Number-1 DO
    FirstBlock.Byte[i] := RawDataBlock[i];
  END;

  FirstBlock.Next      := NIL;                                 (* es gibt keinen nächsten Block *)
  FirstBlock.Number    :=  0;
  ActBlock             := FirstBlock;
  FileLength           := Number;

END ReadFirstBlock;


(*****************************************************************************)
(*                                                                           *)
(* ReadNextBlock                                                             *)
(* Liest Folgeblöcke einer Datei                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LRESULT   wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ReadNextBlock*              (RawDataBlock:       ARRAY OF SYSTEM.BYTE; 
                                       Number:             LONGINT);

VAR
  i:                                   INTEGER;

  
BEGIN
  
  IF ActBlock.Next=NIL THEN
    NEW(ActBlock2);                                        (* neuen Block erzeugen *)
  END;
  ActBlock.Next      := ActBlock2;
  ActBlock2.Number   := ActBlock.Number+1;
  ActBlock2.Previous := ActBlock;
  ActBlock2.Next     := NIL;
  ActBlock           := ActBlock.Next;
  
  FOR i:=0 TO Number-1 DO
    ActBlock.Byte[i] := RawDataBlock[i];
  END;
  FileLength         := FileLength + Number;

END ReadNextBlock;


(*****************************************************************************)
(*                                                                           *)
(* ImageDirectoryOffset                                                      *)
(* Liest Folgeblöcke einer Datei                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LRESULT   wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE ImageDirectoryOffset*       (NoOfImageDirectory: INTEGER;
                                       VAR PSectionHeader: WinNT.PIMAGE_SECTION_HEADER)
                                      :LONGINT;
                                      
VAR
  VAImageDir:                          LONGINT;
  i:                                   INTEGER;
  
BEGIN
  PSectionHeader   := NIL;
  IF NoOfImageDirectory>=PMyNT_Header^.OptionalHeader.NumberOfRvaAndSizes THEN
    RETURN 0
  END (* IF *);
  
  ActSectionHeader := FirstSectionHeader;
  VAImageDir       := PMyNT_Header.OptionalHeader.DataDirectory[NoOfImageDirectory].VirtualAddress;
  IF VAImageDir=0 THEN
    RETURN 0                                               (* no such directory ! *)
  END;
  FOR i:=0 TO PMyNT_Header^.FileHeader.NumberOfSections-1 DO
    IF ActSectionHeader^.PSectionHeader=NIL THEN
      StatusLine.SetText1("PSectionHeader=NIL: #", i, StatusLine.NoticeField);
    ELSIF ((ActSectionHeader^.PSectionHeader^.VirtualAddress<=VAImageDir) & 
       ((ActSectionHeader^.PSectionHeader^.VirtualAddress+ActSectionHeader^.PSectionHeader^.SizeOfRawData)>VAImageDir)) THEN
      ASSERT(MyFileDescription.lpFileBase#0);
      ASSERT(ActSectionHeader^.PSectionHeader^.PointerToRawData#0);
      VAImageDir       := MyFileDescription.lpFileBase + ActSectionHeader^.PSectionHeader^.PointerToRawData;
      PSectionHeader   := ActSectionHeader^.PSectionHeader;
(*      ASSERT(VAImageDir>0);*)
      RETURN VAImageDir
    END;
    ActSectionHeader := ActSectionHeader^.Next;
  END (* FOR i:=... *);
  
  RETURN 0
END ImageDirectoryOffset;


(*****************************************************************************)
(*                                                                           *)
(* GetSectionHeader                                                          *)
(* Liest Folgeblöcke einer Datei                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LRESULT   wie war's                                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE GetSectionHeader*           (NoOfImageDirectory: INTEGER)
                                      :WinNT.PIMAGE_SECTION_HEADER;
                                      
VAR
  ActSectionHeader:                    PSectionHeader;
  VAImageDir:                          LONGINT;
  i:                                   INTEGER;
  
BEGIN
  IF NoOfImageDirectory>=PMyNT_Header^.OptionalHeader.NumberOfRvaAndSizes THEN
    RETURN NIL
  END (* IF *);
  
  ActSectionHeader := FirstSectionHeader;
  VAImageDir       := PMyNT_Header.OptionalHeader.DataDirectory[NoOfImageDirectory].VirtualAddress;
  IF VAImageDir=0 THEN
    RETURN NIL                                             (* no such directory ! *)
  END;
  FOR i:=0 TO PMyNT_Header^.FileHeader.NumberOfSections-1 DO
    IF ((ActSectionHeader^.PSectionHeader^.VirtualAddress<=VAImageDir) & 
       ((ActSectionHeader^.PSectionHeader^.VirtualAddress+ActSectionHeader^.PSectionHeader^.SizeOfRawData)>VAImageDir)) THEN
      VAImageDir := MyFileDescription.lpFileBase + ActSectionHeader^.PSectionHeader^.PointerToRawData;
      RETURN ActSectionHeader^.PSectionHeader
    END;
    ActSectionHeader := ActSectionHeader^.Next;
  END (* FOR i:=... *);
  
  RETURN NIL
END GetSectionHeader;


(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)

BEGIN;

  Level                :=  1;
  DisplayMode          :=  0;

  MyFileDescription.Loaded     := FALSE;
  MyFileDescription.lpFileBase :=  0;
  
  NEW(FirstSectionHeader);
  FirstSectionHeader^.PSectionHeader   := NIL;
  NEW(FirstDebugDirectory);
  FirstDebugDirectory^.PDebugDirectory := NIL;
  NEW(MyLongIntP);

  (* Einlesen der Hexdaten vorbereiten *)
  NEW(FirstBlock);
  FirstBlock.Previous  := NIL;
  FirstBlock.Number    :=  0;
  FOR i:=0 TO 255 DO
    FirstBlock.Byte[i] := SYSTEM.VAL(SYSTEM.BYTE, i);
  END;
  NEW(LastBlock);
  LastBlock.Number     :=  1;
  i := 0;
  FOR j:=255 TO 0 BY -1 DO
    LastBlock.Byte[i] := SYSTEM.VAL(SYSTEM.BYTE, j);
    INC(i);
  END;
  LastBlock.Previous   := FirstBlock;
  FirstBlock.Next      := LastBlock;
  LastBlock.Next       := NIL;
  
  ActBlock             := FirstBlock;
  
  NEW(DebugProcess);
  
END Global.

