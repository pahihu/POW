(*****************************************************************************)
(*                                                                           *)
(* Project:    WindowsProgram                                                *)
(*                                                                           *)
(* Module:     Imagehlp                                    V 1.00.00         *)
(*                                                         2001MAY23         *)
(*  PURPOSE:   interface to WinAPI library                                   *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(* Authors:    KlS    schultze-schoenberg@t-online.de                        *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created    2000SEP11                                                     *)
(*                                                                           *)
(*  update                                                                   *)
(*   2000SEP16 KlS    change request 01.001 fixed                            *)
(*                    short description                                      *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

DEFINITION Imagehlp;

IMPORT
  SYSTEM, WinBase, WinDef, WinNT;

  (* ++ BUILD Version: 0001     Increment this if a change has global effects *)
  (*  *)
  (* Copyright 1993 - 1998 Microsoft Corporation *)
  (*  *)
  (* Module Name: *)
  (*  *)
  (*     imagehlp.h *)
  (*  *)
  (* Abstract: *)
  (*  *)
  (*     This module defines the prototypes and constants required for the image *)
  (*     help routines. *)
  (*  *)
  (* Revision History: *)
  (*  *)
  (* --* *)
  (*  *)
  (*  Define checksum return codes. *)
  (*  *)

CONST
  CHECKSUM_SUCCESS  =                  0;
  CHECKSUM_OPEN_FAILURE  =             1;
  CHECKSUM_MAP_FAILURE  =              2;
  CHECKSUM_MAPVIEW_FAILURE  =          3;
  CHECKSUM_UNICODE_FAILURE  =          4;
  (*  Define Splitsym flags. *)
  SPLITSYM_REMOVE_PRIVATE  =           000000001H;         (* Remove CV types/symbols and Fixup debug *)
                                                           (*   Used for creating .dbg files that ship *)
                                                           (*   as part of the product. *)
  SPLITSYM_EXTRACT_ALL  =              000000002H;         (* Extract all debug info from image. *)
                                                           (*   Normally, FPO is left in the image *)
                                                           (*   to allow stack traces through the code. *)
                                                           (*   Using this switch is similar to linking *)
                                                           (*   with -debug:none except the .dbg file *)
                                                           (*   exists... *)
  SPLITSYM_SYMBOLPATH_IS_SRC  =        000000004H;         (* The SymbolFilePath contains an alternate *)
                                                           (*   path to locate the pdb. *)

CONST

  (* Enumerator IMAGEHLP_STATUS_REASON *)
  BindOutOfMemory  =                   0;
  BindRvaToVaFailed  =                 1;
  BindNoRoomInImage  =                 2;
  BindImportModuleFailed  =            3;
  BindImportProcedureFailed  =         4;
  BindImportModule  =                  5;
  BindImportProcedure  =               6;
  BindForwarder  =                     7;
  BindForwarderNOT  =                  8;
  BindImageModified  =                 9;
  BindExpandFileHeaders  =             10;
  BindImageComplete  =                 11;
  BindMismatchedSymbols  =             12;
  BindSymbolsNotUpdated  =             13;

CONST
  BIND_NO_BOUND_IMPORTS  =             000000001H;
  BIND_NO_UPDATE  =                    000000002H;
  BIND_ALL_IMAGES  =                   000000004H;
  BIND_CACHE_IMPORT_DLLS  =            000000008H;         (* Cache dll's across *)
                                                           (*   calls to BindImageEx *)
                                                           (*   (same as NT 3.1->NT 4.0) *)

CONST
  IMAGE_SEPARATION  =                  64*1024;

CONST
  CERT_PE_IMAGE_DIGEST_DEBUG_INFO  =   001H;
  CERT_PE_IMAGE_DIGEST_RESOURCES  =    002H;
  CERT_PE_IMAGE_DIGEST_ALL_IMPORT_INFO  =   004H;
  CERT_PE_IMAGE_DIGEST_NON_PE_INFO  =   008H;              (* include data outside the PE image *)
  CERT_SECTION_TYPE_ANY  =             0FFH;               (* Any Certificate type *)

CONST
  SYMOPT_CASE_INSENSITIVE  =           000000001H;
  SYMOPT_UNDNAME  =                    000000002H;
  SYMOPT_DEFERRED_LOADS  =             000000004H;
  SYMOPT_NO_CPP  =                     000000008H;
  SYMOPT_LOAD_LINES  =                 000000010H;
  SYMOPT_OMAP_FIND_NEAREST  =          000000020H;


  (*  *)
  (*  data structures used for registered symbol callbacks *)
  (*  *)

CONST
  CBA_DEFERRED_SYMBOL_LOAD_START  =    000000001H;
  CBA_DEFERRED_SYMBOL_LOAD_COMPLETE  =   000000002H;
  CBA_DEFERRED_SYMBOL_LOAD_FAILURE  =   000000003H;
  CBA_SYMBOLS_UNLOADED  =              000000004H;
  CBA_DUPLICATE_SYMBOL  =              000000005H;

(*  *)
(*  UnDecorateSymbolName Flags *)
(*  *)

CONST
  UNDNAME_COMPLETE  =                  00000H;             (* Enable full undecoration *)
  UNDNAME_NO_LEADING_UNDERSCORES  =    00001H;             (* Remove leading underscores from MS extended keywords *)
  UNDNAME_NO_MS_KEYWORDS  =            00002H;             (* Disable expansion of MS extended keywords *)
  UNDNAME_NO_FUNCTION_RETURNS  =       00004H;             (* Disable expansion of return type for primary declaration *)
  UNDNAME_NO_ALLOCATION_MODEL  =       00008H;             (* Disable expansion of the declaration model *)
  UNDNAME_NO_ALLOCATION_LANGUAGE  =    00010H;             (* Disable expansion of the declaration language specifier *)
  UNDNAME_NO_MS_THISTYPE  =            00020H;             (* NYI Disable expansion of MS keywords on the 'this' type for primary declaration *)
  UNDNAME_NO_CV_THISTYPE  =            00040H;             (* NYI Disable expansion of CV modifiers on the 'this' type for primary declaration *)
  UNDNAME_NO_THISTYPE  =               00060H;             (* Disable all modifiers on the 'this' type *)
  UNDNAME_NO_ACCESS_SPECIFIERS  =      00080H;             (* Disable expansion of access specifiers for members *)
  UNDNAME_NO_THROW_SIGNATURES  =       00100H;             (* Disable expansion of 'throw-signatures' for functions and pointers to functions *)
  UNDNAME_NO_MEMBER_TYPE  =            00200H;             (* Disable expansion of 'static' or 'virtual'ness of members *)
  UNDNAME_NO_RETURN_UDT_MODEL  =       00400H;             (* Disable expansion of MS model for UDT returns *)
  UNDNAME_32_BIT_DECODE  =             00800H;             (* Undecorate 32-bit decorated names *)
  UNDNAME_NAME_ONLY  =                 01000H;             (* Crack only the name for primary declaration; *)
                                                           (*   return just [scope::]name.  Does expand template params *)
  UNDNAME_NO_ARGUMENTS  =              02000H;             (* Don't undecorate arguments to function *)
  UNDNAME_NO_SPECIAL_SYMS  =           04000H;             (* Don't undecorate special names (v-table, vcall, vector xxx, metatype, etc) *)

(*  *)
(*  StackWalking API *)
(*  *)

CONST

  (* Enumerator *)
  AddrMode1616  =                      0;
  AddrMode1632  =                      1;
  AddrModeReal  =                      2;
  AddrModeFlat  =                      3;

CONST
  API_VERSION_NUMBER  =                7;

(*  *)
(*  symbol flags *)
(*  *)

CONST
  SYMF_OMAP_GENERATED  =               000000001H;
  SYMF_OMAP_MODIFIED  =                000000002H;
  (*  *)
  (*  symbol type enumeration *)
  (*  *)

  (* Enumerator *)
  SymNone  =                           0;
  SymCoff  =                           1;
  SymCv  =                             2;
  SymPdb  =                            3;
  SymExport  =                         4;
  SymDeferred  =                       5;
  SymSym  =                            6;

  (*  *)
  (*  symbol data structure *)
  (*  *)

TYPE
  IMAGEHLP_SYMBOL = RECORD [_NOTALIGNED]
      SizeOfStruct:                    WinDef.DWORD;
      Address:                         WinDef.DWORD;
      Size:                            WinDef.DWORD;
      Flags:                           WinDef.DWORD;
      MaxNameLength:                   WinDef.DWORD;
      Name:                            ARRAY 1 OF CHAR;
  END (* IMAGEHLP_SYMBOL *) ;
  PIMAGEHLP_SYMBOL =                   POINTER TO IMAGEHLP_SYMBOL;

  (*  *)
  (*  module data structure *)
  (*  *)
  IMAGEHLP_MODULE = RECORD [_NOTALIGNED]
      SizeOfStruct:                    WinDef.DWORD;
      BaseOfImage:                     WinDef.DWORD;
      ImageSize:                       WinDef.DWORD;
      TimeDateStamp:                   WinDef.DWORD;
      CheckSum:                        WinDef.DWORD;
      NumSyms:                         WinDef.DWORD;
      SymType:                         LONGINT;
      ModuleName:                      ARRAY 32 OF CHAR;
      ImageName:                       ARRAY 256 OF CHAR;
      LoadedImageName:                 ARRAY 256 OF CHAR;
  END (* IMAGEHLP_MODULE *) ;
  PIMAGEHLP_MODULE =                   POINTER TO IMAGEHLP_MODULE;

  (*  *)
  (*  source file line data structure *)
  (*  *)
  IMAGEHLP_LINE = RECORD [_NOTALIGNED]
      SizeOfStruct:                    WinDef.DWORD;
      Key:                             WinDef.DWORD;
      LineNumber:                      WinDef.DWORD;
      FileName:                        WinNT.PCHAR;
      Address:                         WinDef.DWORD;
  END (* IMAGEHLP_LINE *) ;
  PIMAGEHLP_LINE =                     POINTER TO IMAGEHLP_LINE;

TYPE
  API_VERSION = RECORD [_NOTALIGNED]
      MajorVersion:                    WinDef.USHORT;
      MinorVersion:                    WinDef.USHORT;
      Revision:                        WinDef.USHORT;
      Reserved:                        WinDef.USHORT;
  END (* API_VERSION *) ;
  LPAPI_VERSION =                      POINTER TO API_VERSION;


TYPE
  tagADDRESS = RECORD [_NOTALIGNED]
      Offset:                          WinDef.DWORD;
      Segment:                         WinDef.WORD;
      Mode:                            LONGINT;
  END (* SYSTEM.PTR *) ;
  LPADDRESS =                          POINTER TO tagADDRESS;

  (*  *)
  (*  This structure is included in the STACKFRAME structure, *)
  (*  and is used to trace through usermode callbacks in a thread's *)
  (*  kernel stack.  The values must be copied by the kernel debugger *)
  (*  from the DBGKD_GET_VERSION and WAIT_STATE_CHANGE packets. *)
  (*  *)
  KDHELP = RECORD [_NOTALIGNED]
                                                           (*  *)
                                                           (*  address of kernel thread object, as provided in the *)
                                                           (*  WAIT_STATE_CHANGE packet. *)
                                                           (*  *)
      Thread:                          WinDef.DWORD;
                                                           (*  *)
                                                           (*  offset in thread object to pointer to the current callback frame *)
                                                           (*  in kernel stack. *)
                                                           (*  *)
      ThCallbackStack:                 WinDef.DWORD;
                                                           (*  *)
                                                           (*  offsets to values in frame: *)
                                                           (*  *)
                                                           (*  address of next callback frame *)
      NextCallback:                    WinDef.DWORD;
                                                           (*  address of saved frame pointer (if applicable) *)
      FramePointer:                    WinDef.DWORD;
                                                           (*  *)
                                                           (*  Address of the kernel function that calls out to user mode *)
                                                           (*  *)
      KiCallUserMode:                  WinDef.DWORD;
                                                           (*  *)
                                                           (*  Address of the user mode dispatcher function *)
                                                           (*  *)
      KeUserCallbackDispatcher:        WinDef.DWORD;
                                                           (*  *)
                                                           (*  Lowest kernel mode address *)
                                                           (*  *)
      SystemRangeStart:                WinDef.DWORD;
  END (* KDHELP *) ;
  PKDHELP =                            POINTER TO KDHELP;

  tagSTACKFRAME = RECORD [_NOTALIGNED]
      AddrPC:                          SYSTEM.PTR;
      AddrReturn:                      SYSTEM.PTR;
      AddrFrame:                       SYSTEM.PTR;
      AddrStack:                       SYSTEM.PTR;
      FuncTableEntry:                  WinDef.LPVOID;
      Params:                          ARRAY 4 OF WinDef.DWORD;
      Far:                             WinDef.BOOL;
      Virtual:                         WinDef.BOOL;
      Reserved:                        ARRAY 3 OF WinDef.DWORD;
      KdHelp:                          KDHELP;
  END (* STACKFRAME *) ;
  LPSTACKFRAME =                       POINTER TO tagSTACKFRAME;

TYPE
  IMAGEHLP_DEFERRED_SYMBOL_LOAD = RECORD [_NOTALIGNED]
      SizeOfStruct:                    WinDef.DWORD;
      BaseOfImage:                     WinDef.DWORD;
      CheckSum:                        WinDef.DWORD;
      TimeDateStamp:                   WinDef.DWORD;
      FileName:                        ARRAY 128 OF CHAR;
      Reparse:                         BOOLEAN;
  END (* IMAGEHLP_DEFERRED_SYMBOL_LOAD *) ;
  PIMAGEHLP_DEFERRED_SYMBOL_LOAD =     POINTER TO IMAGEHLP_DEFERRED_SYMBOL_LOAD;

  IMAGEHLP_DUPLICATE_SYMBOL = RECORD [_NOTALIGNED]
      SizeOfStruct:                    WinDef.DWORD;
      NumberOfDups:                    WinDef.DWORD;
      Symbol:                          PIMAGEHLP_SYMBOL;
      SelectedSymbol:                  WinDef.ULONG;
  END (* IMAGEHLP_DUPLICATE_SYMBOL *) ;
  PIMAGEHLP_DUPLICATE_SYMBOL =         POINTER TO IMAGEHLP_DUPLICATE_SYMBOL;

TYPE
  LOADED_IMAGE = RECORD [_NOTALIGNED]
      ModuleName:                      WinDef.LPSTR;
      hFile:                           WinDef.HANDLE;
      MappedAddress:                   WinDef.PUCHAR;
      FileHeader:                      WinNT.PIMAGE_NT_HEADERS;
      LastRvaSection:                  WinNT.PIMAGE_SECTION_HEADER;
      NumberOfSections:                WinDef.ULONG;
      Sections:                        WinNT.PIMAGE_SECTION_HEADER;
      Characteristics:                 WinDef.ULONG;
      fSystemImage:                    BOOLEAN;
      fDOSImage:                       BOOLEAN;
      Links:                           WinNT.LIST_ENTRY;
      SizeOfImage:                     WinDef.ULONG;
  END (* LOADED_IMAGE *) ;
  PLOADED_IMAGE =                      POINTER TO LOADED_IMAGE;

TYPE
  IMAGE_DEBUG_INFORMATION = RECORD [_NOTALIGNED]
      List:                            WinNT.LIST_ENTRY;
      Size:                            WinDef.DWORD;
      MappedBase:                      SYSTEM.PTR;
      Machine:                         WinDef.USHORT;
      Characteristics:                 WinDef.USHORT;
      CheckSum:                        WinDef.DWORD;
      ImageBase:                       WinDef.DWORD;
      SizeOfImage:                     WinDef.DWORD;
      NumberOfSections:                WinDef.DWORD;
      Sections:                        WinNT.PIMAGE_SECTION_HEADER;
      ExportedNamesSize:               WinDef.DWORD;
      ExportedNames:                   WinDef.LPSTR;
      NumberOfFunctionTableEntries:    WinDef.DWORD;
      FunctionTableEntries:            WinNT.PIMAGE_FUNCTION_ENTRY;
      LowestFunctionStartingAddress:   WinDef.DWORD;
      HighestFunctionEndingAddress:    WinDef.DWORD;
      NumberOfFpoTableEntries:         WinDef.DWORD;
      FpoTableEntries:                 WinNT.PFPO_DATA;
      SizeOfCoffSymbols:               WinDef.DWORD;
      CoffSymbols:                     WinNT.PIMAGE_COFF_SYMBOLS_HEADER;
      SizeOfCodeViewSymbols:           WinDef.DWORD;
      CodeViewSymbols:                 SYSTEM.PTR;
      ImageFilePath:                   WinDef.LPSTR;
      ImageFileName:                   WinDef.LPSTR;
      DebugFilePath:                   WinDef.LPSTR;
      TimeDateStamp:                   WinDef.DWORD;
      RomImage:                        WinDef.BOOL;
      DebugDirectory:                  WinNT.PIMAGE_DEBUG_DIRECTORY;
      NumberOfDebugDirectories:        WinDef.DWORD;
      Reserved:                        ARRAY 3 OF WinDef.DWORD;
  END (* IMAGE_DEBUG_INFORMATION *) ;
  PIMAGE_DEBUG_INFORMATION =           POINTER TO IMAGE_DEBUG_INFORMATION;


TYPE
  PIMAGEHLP_STATUS_ROUTINE = PROCEDURE [_APICALL]  
                                      (Reason:    LONGINT;
                                       ImageName:          WinDef.LPSTR;
                                       DllName:            WinDef.LPSTR;
                                       Va:                 WinDef.ULONG;
                                       Parameter:          WinDef.ULONG)
                                      :                    WinDef.BOOL;



  (* ##### Precompiler Directive:  IMAGEAPI ==> DECLSPEC_IMPORT *)
  (*  *)
  (*  Define checksum function prototypes. *)
  (*  *)

PROCEDURE [_APICALL] CheckSumMappedFile (BaseAddress:      WinDef.LPVOID;
                                       FileLength:         WinDef.DWORD;
                                       HeaderSum:          WinDef.LPDWORD;
                                       CheckSum:           WinDef.LPDWORD)
                                      :                    WinNT.PIMAGE_NT_HEADERS;

PROCEDURE [_APICALL] MapFileAndCheckSumA (Filename:        WinDef.LPSTR;
                                       HeaderSum:          WinDef.LPDWORD;
                                       CheckSum:           WinDef.LPDWORD)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] MapFileAndCheckSumW (Filename:        WinDef.LPWSTR;
                                       HeaderSum:          WinDef.LPDWORD;
                                       CheckSum:           WinDef.LPDWORD)
                                      :                    WinDef.DWORD;
(* ##### Precompiler Directive:  MapFileAndCheckSum ==> MapFileAndCheckSumA *)

PROCEDURE [_APICALL] TouchFileTimes   (FileHandle:         WinDef.HANDLE;
                                       lpSystemTime:       WinBase.LPSYSTEMTIME)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SplitSymbols     (ImageName:          WinDef.LPSTR;
                                       SymbolsPath:        WinDef.LPSTR;
                                       SymbolFilePath:     WinDef.LPSTR;
                                       Flags:              WinDef.DWORD)
                                      :                    WinDef.BOOL;
                                                           (*  Combination of flags above *)

PROCEDURE [_APICALL] FindDebugInfoFile (FileName:          WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       DebugFilePath:      WinDef.LPSTR)
                                      :                    WinDef.HANDLE;

PROCEDURE [_APICALL] FindExecutableImage (FileName:        WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       ImageFilePath:      WinDef.LPSTR)
                                      :                    WinDef.HANDLE;

PROCEDURE [_APICALL] UpdateDebugInfoFile (ImageFileName:   WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       DebugFilePath:      WinDef.LPSTR;
                                       NtHeaders:          WinNT.PIMAGE_NT_HEADERS)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] UpdateDebugInfoFileEx (ImageFileName: WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       DebugFilePath:      WinDef.LPSTR;
                                       NtHeaders:          WinNT.PIMAGE_NT_HEADERS;
                                       OldChecksum:        WinDef.DWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] BindImage        (ImageName:          WinDef.LPSTR;
                                       DllPath:            WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] BindImageEx      (Flags:              WinDef.DWORD;
                                       ImageName:          WinDef.LPSTR;
                                       DllPath:            WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       StatusRoutine:      PIMAGEHLP_STATUS_ROUTINE)
                                      :                    WinDef.BOOL;

(*PROCEDURE [_APICALL] ReBaseImage      (CurrentImageName:   WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       fReBase:            WinDef.BOOL;
                                       fRebaseSysfileOk:   WinDef.BOOL;
                                       fGoingDown:         WinDef.BOOL;
                                       CheckImageSize:     WinDef.ULONG;
                                       VAR STATICTYPED OldImageSize:     WinDef.ULONG;
                                       VAR STATICTYPED OldImageBase:     WinDef.ULONG;
                                       VAR STATICTYPED NewImageSize:     WinDef.ULONG;
                                       VAR STATICTYPED NewImageBase:     WinDef.ULONG;
                                                           (*  (out) Next address (actual if going down) *)
                                       TimeStamp:          WinDef.ULONG)
                                      :                    WinDef.BOOL;
                                                           (*  new timestamp for image, if non-zero *)
*)
PROCEDURE [_APICALL] ImageLoad        (DllName:            WinDef.LPSTR;
                                       DllPath:            WinDef.LPSTR)
                                      :                    PLOADED_IMAGE;

PROCEDURE [_APICALL] ImageUnload      (LoadedImage:        PLOADED_IMAGE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] ImageNtHeader    (Base:               SYSTEM.PTR)
                                      :                    WinNT.PIMAGE_NT_HEADERS;

PROCEDURE [_APICALL] ImageDirectoryEntryToData (Base:      SYSTEM.PTR;
                                       MappedAsImage:      BOOLEAN;
                                       DirectoryEntry:     WinDef.USHORT;
                                       Size:               WinDef.PULONG)
                                      :                    SYSTEM.PTR;

PROCEDURE [_APICALL] ImageRvaToSection (NtHeaders:         WinNT.PIMAGE_NT_HEADERS;
                                       Base:               SYSTEM.PTR;
                                       Rva:                WinDef.ULONG)
                                      :                    WinNT.PIMAGE_SECTION_HEADER;

PROCEDURE [_APICALL] ImageRvaToVa     (NtHeaders:          WinNT.PIMAGE_NT_HEADERS;
                                       Base:               SYSTEM.PTR;
                                       Rva:                WinDef.ULONG;
                                       VAR LastRvaSection: WinNT.PIMAGE_SECTION_HEADER)
                                      :                    SYSTEM.PTR;

PROCEDURE [_APICALL] MapAndLoad       (ImageName:          WinDef.LPSTR;
                                       DllPath:            WinDef.LPSTR;
                                       LoadedImage:        PLOADED_IMAGE;
                                       DotDll:             WinDef.BOOL;
                                       ReadOnly:           WinDef.BOOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] GetImageConfigInformation (LoadedImage:     PLOADED_IMAGE;
                                       ImageConfigInformation:   WinNT.PIMAGE_LOAD_CONFIG_DIRECTORY)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] GetImageUnusedHeaderBytes (LoadedImage:     PLOADED_IMAGE;
                                       SizeUnusedHeaderBytes:    WinDef.LPDWORD)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] SetImageConfigInformation (LoadedImage:     PLOADED_IMAGE;
                                       ImageConfigInformation:   WinNT.PIMAGE_LOAD_CONFIG_DIRECTORY)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] UnMapAndLoad     (LoadedImage:        PLOADED_IMAGE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] MapDebugInformation (FileHandle:      WinDef.HANDLE;
                                       FileName:           WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       ImageBase:          WinDef.DWORD)
                                      :                    PIMAGE_DEBUG_INFORMATION;

PROCEDURE [_APICALL] UnmapDebugInformation (DebugInfo:     PIMAGE_DEBUG_INFORMATION)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] FindExecutableImage (FileName:        WinDef.LPSTR;
                                       SymbolPath:         WinDef.LPSTR;
                                       ImageFilePath:      WinDef.LPSTR)
                                      :                    WinDef.HANDLE;

PROCEDURE [_APICALL] SearchTreeForFile (RootPath:          WinDef.LPSTR;
                                       InputPathName:      WinDef.LPSTR;
                                       OutputPathBuffer:   WinDef.LPSTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] MakeSureDirectoryPathExists (DirPath: WinDef.LPCSTR)
                                      :                    WinDef.BOOL;
(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] PREAD_PROCESS_MEMORY_ROUTINE (hProcess:     WinDef.HANDLE;
                                       lpBaseAddress:      WinDef.LPCVOID;
                                       lpBuffer:           WinDef.LPVOID;
                                       nSize:              WinDef.DWORD;
                                       lpNumberOfBytesRead:     WinDef.LPDWORD);

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] PFUNCTION_TABLE_ACCESS_ROUTINE (hProcess:     WinDef.HANDLE;
                                       AddrBase:           WinDef.DWORD);

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] PGET_MODULE_BASE_ROUTINE (hProcess:  WinDef.HANDLE;
                                       ReturnAddress:      WinDef.DWORD);

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] PTRANSLATE_ADDRESS_ROUTINE (hProcess:     WinDef.HANDLE;
                                       hThread:            WinDef.HANDLE;
                                       lpaddr:             LPADDRESS);

PROCEDURE [_APICALL] StackWalk        (MachineType:        WinDef.DWORD;
                                       hProcess:           WinDef.HANDLE;
                                       hThread:            WinDef.HANDLE;
                                       StackFrame:         LPSTACKFRAME;
                                       ContextRecord:      WinDef.LPVOID;
                                       ReadMemoryRoutine:  PREAD_PROCESS_MEMORY_ROUTINE;
                                       FunctionTableAccessRoutine:     PFUNCTION_TABLE_ACCESS_ROUTINE;
                                       GetModuleBaseRoutine:     PGET_MODULE_BASE_ROUTINE;
                                       TranslateAddress:   PTRANSLATE_ADDRESS_ROUTINE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] ImagehlpApiVersion (:                 VOID)
                                      :                    LPAPI_VERSION;

PROCEDURE [_APICALL] ImagehlpApiVersionEx (AppVersion:     LPAPI_VERSION)
                                      :                    LPAPI_VERSION;

PROCEDURE [_APICALL] GetTimestampForLoadedLibrary (Module: WinDef.HMODULE)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] RemovePrivateCvSymbolic (DebugData:   PCHAR;
                                       VAR STATICTYPED NewDebugData:     PCHAR;
                                       VAR STATICTYPED NewDebugSize:     WinDef.ULONG)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] RemoveRelocations (ImageName:         PCHAR)
                                      :                    VOID;
(*  *)
(*  typedefs for function pointers *)
(*  *)

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] *PSYM_ENUMMODULES_CALLBACK (ModuleName:     WinDef.LPSTR;
                                       BaseOfDll:          WinDef.ULONG;
                                       UserContext:        SYSTEM.PTR)
                                      :                    ;

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] *PSYM_ENUMSYMBOLS_CALLBACK (SymbolName:     WinDef.LPSTR;
                                       SymbolAddress:      WinDef.ULONG;
                                       SymbolSize:         WinDef.ULONG;
                                       UserContext:        SYSTEM.PTR)
                                      :                    ;

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] *PENUMLOADED_MODULES_CALLBACK (ModuleName:     WinDef.LPSTR;
                                       ModuleBase:         WinDef.ULONG;
                                       ModuleSize:         WinDef.ULONG;
                                       UserContext:        SYSTEM.PTR)
                                      :                    ;

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] *PSYMBOL_REGISTERED_CALLBACK (hProcess:     WinDef.HANDLE;
                                       ActionCode:         WinDef.ULONG;
                                       CallbackData:       SYSTEM.PTR;
                                       UserContext:        SYSTEM.PTR)
                                      :                    ;
  (*  *)
  (*  options that are set/returned by SymSetOptions() & SymGetOptions() *)
  (*  these are used as a mask *)
  (*  *)

PROCEDURE [_APICALL] SymSetOptions    (SymOptions:         WinDef.DWORD)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] SymGetOptions    (:                   VOID)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] SymCleanup       (hProcess:           WinDef.HANDLE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymEnumerateModules (hProcess:        WinDef.HANDLE;
                                       EnumModulesCallback:     PSYM_ENUMMODULES_CALLBACK;
                                       UserContext:        SYSTEM.PTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymEnumerateSymbols (hProcess:        WinDef.HANDLE;
                                       BaseOfDll:          WinDef.DWORD;
                                       EnumSymbolsCallback:     PSYM_ENUMSYMBOLS_CALLBACK;
                                       UserContext:        SYSTEM.PTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] EnumerateLoadedModules (hProcess:     WinDef.HANDLE;
                                       EnumLoadedModulesCallback:     PENUMLOADED_MODULES_CALLBACK;
                                       UserContext:        SYSTEM.PTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymFunctionTableAccess (hProcess:     WinDef.HANDLE;
                                       AddrBase:           WinDef.DWORD)
                                      :                    WinDef.LPVOID;

PROCEDURE [_APICALL] SymGetModuleInfo (hProcess:           WinDef.HANDLE;
                                       dwAddr:             WinDef.DWORD;
                                       ModuleInfo:         PIMAGEHLP_MODULE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetModuleBase (hProcess:           WinDef.HANDLE;
                                       dwAddr:             WinDef.DWORD)
                                      :                    WinDef.DWORD;

PROCEDURE [_APICALL] SymGetSymFromAddr (hProcess:          WinDef.HANDLE;
                                       dwAddr:             WinDef.DWORD;
                                       pdwDisplacement:    WinDef.PDWORD;
                                       Symbol:             PIMAGEHLP_SYMBOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetSymFromName (hProcess:          WinDef.HANDLE;
                                       Name:               WinDef.LPSTR;
                                       Symbol:             PIMAGEHLP_SYMBOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetSymNext    (hProcess:           WinDef.HANDLE;
                                       Symbol:             PIMAGEHLP_SYMBOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetSymPrev    (hProcess:           WinDef.HANDLE;
                                       Symbol:             PIMAGEHLP_SYMBOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetLineFromAddr (hProcess:         WinDef.HANDLE;
                                       dwAddr:             WinDef.DWORD;
                                       pdwDisplacement:    WinDef.PDWORD;
                                       Line:               PIMAGEHLP_LINE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetLineFromName (hProcess:         WinDef.HANDLE;
                                       ModuleName:         WinDef.LPSTR;
                                       FileName:           WinDef.LPSTR;
                                       dwLineNumber:       WinDef.DWORD;
                                       plDisplacement:     PLONG;
                                       Line:               PIMAGEHLP_LINE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetLineNext   (hProcess:           WinDef.HANDLE;
                                       Line:               PIMAGEHLP_LINE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetLinePrev   (hProcess:           WinDef.HANDLE;
                                       Line:               PIMAGEHLP_LINE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymMatchFileName (FileName:           WinDef.LPSTR;
                                       Match:              WinDef.LPSTR;
                                       VAR STATICTYPED FileNameStop:     WinDef.LPSTR;
                                       VAR STATICTYPED MatchStop:     WinDef.LPSTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymInitialize    (hProcess:           WinDef.HANDLE;
                                       UserSearchPath:     WinDef.LPSTR;
                                       fInvadeProcess:     WinDef.BOOL)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymGetSearchPath (hProcess:           WinDef.HANDLE;
                                       SearchPath:         WinDef.LPSTR;
                                       SearchPathLength:   WinDef.DWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymSetSearchPath (hProcess:           WinDef.HANDLE;
                                       SearchPath:         WinDef.LPSTR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymLoadModule    (hProcess:           WinDef.HANDLE;
                                       hFile:              WinDef.HANDLE;
                                       ImageName:          WinDef.PSTR;
                                       ModuleName:         WinDef.PSTR;
                                       BaseOfDll:          WinDef.DWORD;
                                       SizeOfDll:          WinDef.DWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymUnloadModule  (hProcess:           WinDef.HANDLE;
                                       BaseOfDll:          WinDef.DWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] SymUnDName       (sym:                PIMAGEHLP_SYMBOL;
                                       UnDecName:          WinDef.LPSTR;
                                       UnDecNameLength:    WinDef.DWORD)
                                      :                    WinDef.BOOL;
                                                           (*  Size of the buffer *)

PROCEDURE [_APICALL] SymRegisterCallback (hProcess:        WinDef.HANDLE;
                                       CallbackFunction:   PSYMBOL_REGISTERED_CALLBACK;
                                       UserContext:        SYSTEM.PTR)
                                      :                    WinDef.BOOL;
(*  Image Integrity API's *)

(* Procedure Type [Call Back Procedure] *)
(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] *DIGEST_FUNCTION (refdata:            DIGEST_HANDLE;
                                       DECLSPEC_IMPORT:    WinDef.BOOL)
                                      :                    WINAPI;

(* Procedure Type [Call Back Procedure] *)
(* Procedure Type [Call Back Procedure] *)
(* Procedure Type [Call Back Procedure] *)
(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] ImageAddCertificate (FileHandle:      WinDef.HANDLE;
                                       Certificate:        LPWIN_CERTIFICATE;
                                       Index:              WinDef.PDWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] ImageRemoveCertificate (FileHandle:   WinDef.HANDLE;
                                       Index:              WinDef.DWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] ImageEnumerateCertificates (FileHandle:     WinDef.HANDLE;
                                       TypeFilter:         WinDef.WORD;
                                       CertificateCount:   WinDef.PDWORD;
                                       Indices:            WinDef.PDWORD)
                                      :                    WinDef.BOOL;

(* Procedure Type [Call Back Procedure] *)
PROCEDURE [_APICALL] ImageGetCertificateData (FileHandle:  WinDef.HANDLE;
                                       CertificateIndex:   WinDef.DWORD;
                                       Certificate:        LPWIN_CERTIFICATE;
                                       RequiredLength:     WinDef.PDWORD)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] ImageGetCertificateHeader (FileHandle:     WinDef.HANDLE;
                                       CertificateIndex:   WinDef.DWORD;
                                       Certificateheader:  LPWIN_CERTIFICATE)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] CopyPdb          (const:              CHAR)
                                      :                    WinDef.BOOL;

PROCEDURE [_APICALL] RemovePrivateCvSymbolicEx (DebugData: PCHAR;
                                       DebugSize:          WinDef.ULONG;
                                       VAR STATICTYPED NewDebugData:     PCHAR;
                                       VAR STATICTYPED NewDebugSize:     WinDef.ULONG)
                                      :                    WinDef.BOOL;
END Imagehlp.
