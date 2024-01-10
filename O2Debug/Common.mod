(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Common                                      V 2.00.32         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   Definition of global values                                   *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   ReadBytes                                                               *)
(*   ReadInteger                                                             *)
(*   ReadLongInt                                                             *)
(*   ReadFirstBlock                                                          *)
(*             reads HEX data                                                *)
(*   ReadNextBlock                                                           *)
(*             reads HEX data                                                *)
(*   AppendString                                                            *)
(*                                                                           *)
(*   AppendMDIClient                                                         *)
(*             generates a new entry to the MDIClient list                   *)
(*   RemoveMDIClient                                                         *)
(*             removes an entry from the MDIClient list                      *)
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
(*             Procedure DisplayErrors implemented                           *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Common;

IMPORT
  Strings,
  CommDLG, CommCTRL, WinBase, WinDef, WinNT, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.32";
  Module*      =                      "Common";

  MainClass*           =              "O2Debug";           (* window class names *)
  DumpClass*           =              "DumpPECOFF";
  DebugClass*          =              "DebugThread";

  MainTitle*           =              "Oberon-2 Debugger";
  MDIDumpTitle*        =              "Dump File";
  MDIDebugTitle*       =              "Debug Program";
  MDIDumpCVTitle*      =              "Codeview Data";

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
  maxArrayLength*      =               MAX(INTEGER);

  
  MaxPathLength*       =             256;
  
  WXBScreenMetricP*    =               0;
  WXBDebugProcessP*    =               4;


TYPE
  Byte4Array*  =                       ARRAY 4 OF SYSTEM.BYTE;
  LongIntP*    =                       POINTER TO Byte4Array;
  PCharArray*          =               POINTER TO ARRAY maxArrayLength OF CHAR;
  PStringArray*        =               POINTER TO ARRAY maxArrayLength OF PCharArray;
  PDWordArray*         =               POINTER TO ARRAY maxArrayLength OF WinDef.DWORD;
  PWordArray*          =               POINTER TO ARRAY maxArrayLength OF WinDef.WORD;
  PWord*               =               POINTER TO ARRAY 1              OF WinDef.WORD;
  
  ArchiveFileSignatureP*       =       POINTER TO ARRAY 8 OF CHAR;
  
  (* global type definitions concerning the application *)
  MDIClientP*          =               POINTER TO MDIClient;
  MDIClient*   = RECORD
    Previous*,
    Next*:                             MDIClientP;
    hWnd*:                             WinDef.HWND;
    TabControl*:                       LONGINT;
    Mode*:                             LONGINT;
    Path*,
    Name*:                             ARRAY MaxPathLength OF CHAR;
  END (* MDIClient *);

  (* global type definitions concerning the file data *)
  FileDescriptionP*=                   POINTER TO FileDescription;
  FileDescription* = RECORD
    hFile*,
    hFileMapping*:                     WinDef.HFILE;
    lpFileBase*:                       LONGINT;
    FileType*:                         INTEGER;
    Loaded*:                           BOOLEAN;
    Path*,
    Name*:                             ARRAY MaxPathLength OF CHAR;
  END (* FileDescription *);

  (* global type definitions concerning HexData *)
  HexBlockP*   = POINTER TO HexBlock;
  HexBlock     = RECORD
    Number:                            LONGINT;
    Previous, Next:                    HexBlockP;
    Byte:                              ARRAY BlockLength OF SYSTEM.BYTE
  END (* HexBlock *);

  (* global type definitions concerning the PECOFF structure *)
  PDebugDirectory* =                   POINTER TO DebugDirectory;
  DebugDirectory*  = RECORD
    Next*:                             PDebugDirectory;
    PDebugDirectory*:                  WinNT.PIMAGE_DEBUG_DIRECTORY
  END (* DebugDirectory *);
  PSectionHeader*  =                   POINTER TO SectionHeader;
  SectionHeader* = RECORD
    Next*:                             PSectionHeader;
    PSectionHeader*:                   WinNT.PIMAGE_SECTION_HEADER
  END (* SectionHeader *);

  (* global type definitions concerning data displaying *)
  LineOfTextT* =                       ARRAY 128 OF CHAR;
  ScreenLineP* =                       POINTER TO ScreenLineT;
  ScreenLineT* = RECORD
    Text*:                             LineOfTextT;
    Type*:                             INTEGER;            (* holds information describing the line, *)
                                                           (* only used in Dump_DD_debug             *)
    Format*:                           INTEGER;            (* Enumerator "Type Of Paragraph"         *)
    Next*,
    Previous*:                         ScreenLineP;
    Usage*:                            INTEGER;            (* Bit 0 = 1: generate area for mouse actions *)
    Area*:                             WinDef.RECT;
  END (* RECORD ScreenLineT *);
  ScreenMetricP* =                     POINTER TO ScreenMetricT;
  ScreenMetricT* = RECORD
    NumberOfFirstLine*,                                    (* relative to screen *)
    NumberOfLines*,                                        (* number of lines read by the appropriate routine *)
    LinesOnScreen*,                                        (* depends on the actual size of the window *)
    NumberOfFirstColumn*,                                  (* relative to screen *)
    NumberOfColumns*,                                      (* number of lines read by the appropriate routine *)
    ColumnsOnScreen*:                  LONGINT;            (* depends on the actual size of the window *)
    FirstLineOnScreen*,
    FirstLine*,                                            (* anchor *)
    HeadLines*:                        ScreenLineP;        (* anchor *)
    Formatted*:                        BOOLEAN;
  END (* ScreenMetric *);
  WriteLinesT* = PROCEDURE          
                   (FirstLineP:       ScreenLineP;
                   VAR NumberOfColumns:
                                      LONGINT)
                   :                  LONGINT;

  (* define structures for debugging processes and threads *)
  DebugThreadP*  =                     POINTER TO DebugThreadT;
  DebugThreadT*  = RECORD
    hThread*:                          WinDef.HANDLE;
    dwThreadID*:                       WinDef.DWORD;
    lpStartAddress*:                   WinBase.LPTHREAD_START_ROUTINE;
    bfActive*:                         WinDef.BOOL;
    nPriority*:                        INTEGER;
    Next*:                             DebugThreadP
  END (* DebugThreadT *);

  DebugProcessP* =                     POINTER TO DebugProcessT;
  DebugProcessT* = RECORD
    hDbgHeap*:                         WinDef.HANDLE;
    dwProcessID*,
    dwThreadID*:                       WinDef.DWORD;
    hProcess*,
    hFile*:                            WinDef.HANDLE;
    lpImage*:                          WinDef.LPVOID;                     
    dwDbgInfoOffset*,
    nDbgInfoSize*:                     WinDef.DWORD;
    lpThreads*:                        DebugThreadP;
    hWnd*:                             WinDef.HWND;
    ProcessPriority*:                  INTEGER;
    hThread*:                          WinDef.HANDLE;
    ThreadPriority*:                   INTEGER;
    szModule*:                         ARRAY MaxPathLength OF CHAR;
    FirstBreakpoint*:                  BOOLEAN;
  END (* DebugProcess *);


VAR
  hInstance*:                          WinDef.HINSTANCE;
  nCmdShow*:                           LONGINT;
  hWndMain*,
  hWndMDIClient*,
  hWndDumpCV*:                         WinDef.HWND;
  ActiveMDIClient*,
  MDIClientList*:                      MDIClientP;


  MyCommonControlsEx:                  CommCTRL.LPINITCOMMONCONTROLSEX;
  
  CurrentDirectory*:                   ARRAY 256 OF CHAR;

  DisplayMode*:                        INTEGER;
                                       (* holds values of const Display Mode *)
  Level*:                              INTEGER;
                                       (* 0 Display everything *)
                                       (* 1 medium level of information displayed *)
                                       (* 2 low level of information displayed *)

  MyFileDescription*:                  FileDescription;
  FileData*:                           ARRAY BlockLength OF SYSTEM.BYTE;
  FileDataLength*:                     LONGINT;
  
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
  
  ResultBool:                          WinDef.BOOL;


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
(* AppendString                                                              *)
(*                                                                           *)
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

PROCEDURE AppendPtrString*            (VAR dest:           ARRAY OF CHAR; 
                                       src:                ARRAY OF CHAR);

VAR
  Index,
  Index2:                              LONGINT;
  MaxLength:                           LONGINT;

  
BEGIN
  
  Index        := Strings.Length(dest);
  Index2       :=  0;
  MaxLength    := LEN(dest)-4;
  
  LOOP
    Strings.AppendChar(dest, src[Index2]);
    IF src[Index2]=0X THEN
      RETURN
    END (* IF src[Index2]=0X  *);
    IF Index>=MaxLength THEN
      Strings.AppendChar(dest, ".");
      INC(Index);
      Strings.AppendChar(dest, ".");
      INC(Index);
      Strings.AppendChar(dest, ".");
      INC(Index);
      Strings.AppendChar(dest, 0X);
    END (* IF Index>=MaxLength  *);
    INC(Index);
    INC(Index2);
  END (* LOOP *);

END AppendPtrString;


(*****************************************************************************)
(*                                                                           *)
(* RemoveMDIClient                                                           *)
(*                                                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   Field     0   Value is WinDef.HWND                                      *)
(*             1   Value is TabControlIndex                                  *)
(*             2   Mode (ref: UIStatusLine.ShowMode)                         *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   MDIClientP                                                              *)
(*             NIL     an error occurred                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE GetMDIClient*               (Field:              LONGINT;
                                       Value:              LONGINT)
                                      :MDIClientP;

VAR
  Done:                                BOOLEAN;
  MyMDIClient:                         MDIClientP;
  
BEGIN

  Done                 := FALSE;
  MyMDIClient          := MDIClientList;
  
  REPEAT
    CASE Field OF
      0:
        IF MyMDIClient.hWnd=SYSTEM.VAL(WinDef.HWND, Value) THEN
          Done := TRUE;
        END (* IF MyMDIClient.hWnd=SYSTEM.VAL(WinDef.HWND *);
      | (* 0, WinDef.HWND *)
      1:
        IF MyMDIClient.TabControl=Value THEN
          Done := TRUE;
        END (* IF MyMDIClient.TabControl=Value *);
       | (* 1, TabControlIndex *)
      2:
        IF MyMDIClient.Mode=Value THEN
          Done := TRUE;
        END (* IF MyMDIClient.Mode=Value *);
        (* 2, Mode *)
      ELSE
        ;
    END (* CASE Field  *);
    IF ~Done THEN
      MyMDIClient  := MyMDIClient.Next;
    END (* IF ~Done  *);
  UNTIL (Done OR (MyMDIClient=NIL));
  
  RETURN MyMDIClient;
  
END GetMDIClient;


(*****************************************************************************)
(*                                                                           *)
(* AppendMDIClient                                                           *)
(*                                                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   hWnd                                                                    *)
(*   PathName                                                                *)
(*   FileName                                                                *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   MDIClientP                                                              *)
(*             pointer to generated element                                  *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE AppendMDIClient*            (hWnd:               WinDef.HWND;
                                       PathName:           ARRAY OF CHAR;
                                       FileName:           ARRAY OF CHAR)
                                      :MDIClientP;

VAR
  MyMDIClient:                         MDIClientP;
  
BEGIN

  MyMDIClient  := MDIClientList;
  
  WHILE MyMDIClient.Next#NIL DO;
    MyMDIClient := MyMDIClient.Next;
  END (* WHILE MyMDIClient.Next#NIL  *);
  
  NEW(MyMDIClient.Next);
  MyMDIClient.Next.Previous    := MyMDIClient;
  MyMDIClient                  := MyMDIClient.Next;
  MyMDIClient.Next             := NIL;
  
  MyMDIClient.hWnd             := hWnd;
  MyMDIClient.TabControl       :=  0;
  MyMDIClient.Mode             :=  0;
  COPY (PathName, MyMDIClient.Path);
  COPY (FileName, MyMDIClient.Name);
  
  RETURN MyMDIClient;
  
END AppendMDIClient;


(*****************************************************************************)
(*                                                                           *)
(* RemoveMDIClient                                                           *)
(*                                                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   MyMDIClient                                                             *)
(*                                                                           *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LONGINT   0       operation successful                                  *)
(*             1       an error occurred                                     *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE RemoveMDIClient*            (MyMDIClient:        MDIClientP)
                                      :LONGINT;

VAR
  PrevMDIClient,
  NextMDIClient:                       MDIClientP;
  
BEGIN

  PrevMDIClient          := MyMDIClient.Previous;
  NextMDIClient          := MyMDIClient.Next;
  
  PrevMDIClient.Next     := NextMDIClient;
  IF NextMDIClient#NIL THEN
    NextMDIClient.Previous := PrevMDIClient;
  END (* IF NextMDIClient=NIL  *);
  
  DISPOSE(MyMDIClient);
  
  RETURN 0;
  
END RemoveMDIClient;


(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)

BEGIN;

  Level                        :=  1;
  DisplayMode                  :=  0;
  CurrentDirectory[0]          :=  0X;
  
  NEW(MDIClientList);
  MDIClientList.Previous       := NIL;
  MDIClientList.Next           := NIL;
  MDIClientList.TabControl     := -1;
  MDIClientList.Mode           :=  0;
  MDIClientList.Name[0]        :=  0X;
  MDIClientList.Path[0]        :=  0X;

  MyFileDescription.hFile      := WinDef.NULL;
  MyFileDescription.hFileMapping 
                               := WinDef.NULL;
  MyFileDescription.lpFileBase :=  0;
  MyFileDescription.FileType   :=  0;
  MyFileDescription.Loaded     := FALSE;
  MyFileDescription.Path[0]    :=  0X;
  MyFileDescription.Name[0]    :=  0X;
  
  NEW(DebugProcess);
  DebugProcess^.dwProcessID  :=  0;

  NEW(FirstSectionHeader);
  FirstSectionHeader^.PSectionHeader   := NIL;
  NEW(FirstDebugDirectory);
  FirstDebugDirectory^.PDebugDirectory := NIL;
  NEW(MyLongIntP);

  (* Einlesen der Hexdaten vorbereiten *)
  NEW(FirstBlock);
  FirstBlock.Previous          := NIL;
  FirstBlock.Number            :=  0;
  FOR i:=0 TO 255 DO
    FirstBlock.Byte[i] := SYSTEM.VAL(SYSTEM.BYTE, i);
  END;

  NEW(LastBlock);
  LastBlock.Number             :=  1;
  i                            := 0;
  FOR j:=255 TO 0 BY -1 DO
    LastBlock.Byte[i] := SYSTEM.VAL(SYSTEM.BYTE, j);
    INC(i);
  END;
  LastBlock.Previous           := FirstBlock;
  FirstBlock.Next              := LastBlock;
  LastBlock.Next               := NIL;
  
  ActBlock                     := FirstBlock;


(*---------    Common Controls Initialization      ------*)
  CommCTRL.InitCommonControls();
  
  NEW(MyCommonControlsEx);
  MyCommonControlsEx.dwSize  := SIZE(CommCTRL.INITCOMMONCONTROLSEX);

  (* Load the coolbar. *)
  MyCommonControlsEx.dwICC   := CommCTRL.ICC_BAR_CLASSES + CommCTRL.ICC_TAB_CLASSES +
                                CommCTRL.ICC_USEREX_CLASSES + 
                                CommCTRL.ICC_COOL_CLASSES;   
  ResultBool                 := CommCTRL.InitCommonControlsEx(MyCommonControlsEx);
  
END Common.

