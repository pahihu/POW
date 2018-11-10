(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Dump_DD_debug                               V 1.42.01         *)
(*                                                         2002APR14         *)
(*  PURPOSE:   Processing Dump Infos                                         *)
(*             analyzing Data Directories                                    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   WriteLines06                                                            *)
(*             Generate lines containing debug informations                  *)
(*   WriteLinesLN                                                            *)
(*             displays the linenumbers                                      *)
(*   WriteLinesST                                                            *)
(*             displays the symbol table                                     *)
(*   GetLineNumber                                                           *)
(*             finds the linenumber for a given program address              *)
(*                                                                           *)
(*                                                                           *)
(* COPYRIGHT:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)  
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(*  CREATED    2000SEP02                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Dump_DD_debug;


IMPORT
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  Strings, SYSTEM,
  Dump, Global, StatusLine;


CONST
  Version*             =              "V 1.42.01";
  Module*              =              "Dump_DD_Debug";
  
  LineEmpty    =                      "   ";
  Line0601     =                      "DATA DIRECTORY 06: Debug Data";
  Line0602     =                      "  Characteristics:              ";
  Line0603     =                      "  Time Date Stamp:              ";
  Line0604     =                      "  Version:                      ";
  Line0605     =                      "  Minor Version:                ";
  Line0606     =                      "  Type:                         ";
  Line0607     =                      "  Size Of Data:                 ";
  Line0608     =                      "  Address Of Raw Data:          ";
  Line0609     =                      "  Pointer To Raw Data:          ";
  Line0620     =                      "CodeView Section:          ";
  Line0621     =                      "Subsection Directory Header  ";
  Line0622     =                      "  Length of header:             ";
  Line0623     =                      "  Length of each dir entry:     ";
  Line0624     =                      "  Number of dir entries:        ";
  Line0625     =                      "  Offset from lfaBase (unused): ";
  Line0626     =                      "  Characteristics (unused):     ";
  Line0630     =                      "Directory Entries";
  Line0631     =                      "  Subsection   Index  ovl iLib cSeg Style";
  Line0632     =                      "  Length of header:             ";
  Line0633     =                      "  Length of each dir entry:     ";
  Line0634     =                      "  Number of dir entries:        ";
  Line0635     =                      "  Offset from lfaBase (unused): ";
  Line0636     =                      "  Characteristics (unused):     ";
  Line0650     =                      "  Unknown:                      ";

  (* Showing Linenumbers *)
  Line1000     =                      "LineNumbers from";
  Line1001     =                      "File:      ";
  Line1002     =                      "SrcModule   NoOfFiles  NoOfSegs  SrcFileOffset     Start       End  Seg";
  Line1003     =                      "SrcFile:               NoOfSegs  SrcLineOffset     Start       End  FileName";
  Line1004     =                      "Seg      NoOfPairs         Offset       LineNo";

  (* Showing Symbol Table *)
  Line1100     =                      "Symbol Table from";
  Line1101     =                      "sstGlobalSym            symhash       addrhash  cbSymbol cbSymHash cbAddrHash";
  Line1102     =                      "   Symbol                                        Segment               Offset";
    

  (* The following structures and constants describe the format of the       *)
  (* CodeView Debug OMF for that will be accepted by CodeView 4.0 and        *)
  (* later.  These are executables with signatures of NB05, NB06 and NB07.   *)
  (* There is some confusion about the signatures NB03 and NB04 so none      *)
  (* of the utilites will accept executables with these signatures.          *)
  (* All of the structures described below must start on a long word         *)
  (* boundary to maintain natural alignment.                                 *)
  (* Pad space can be inserted during the write operation and the            *)
  (* addresses adjusted without affecting the contents of the structures.    *)
  
  (* Type of subsection entry.   *)
  sstModule    =                       0120H;
  sstTypes     =                       0121H;
  sstPublic    =                       0122H;
  sstPublicSym =                       0123H;              (* publics as symbol (waiting for link)   *)
  sstSymbols   =                       0124H;
  sstAlignSym  =                       0125H;
  sstSrcLnSeg  =                       0126H;              (* because link doesn't emit SrcModule   *)
  sstSrcModule =                       0127H;
  sstLibraries =                       0128H;
  sstGlobalSym =                       0129H;
  sstGlobalPub =                       012AH;
  sstGlobalTypes   =                   012BH;
  sstMPC       =                       012CH;
  sstSegMap    =                       012DH;
  sstSegName   =                       012EH;
  sstPreComp   =                       012FH;              (* precompiled types   *)
  sstPreCompMap    =                   0130H;              (* map precompiled types in global types   *)
  sstOffsetMap16   =                   0131H;
  sstOffsetMap32   =                   0132H;
  sstFileIndex =                       0133H;
  sstStaticSym =                       0134H;

  (* Type of SYM_ENUM_e                                                      *)
  S_COMPILE    =  0001H;     (* Compile flags symbol                         *)
  S_REGISTER   =  0002H;     (* Register variable                            *)
  S_CONSTANT   =  0003H;     (* constant symbol                              *)
  S_UDT        =  0004H;     (* User defined type                            *)
  S_SSEARCH    =  0005H;     (* Start Search                                 *)
  S_END        =  0006H;     (* Block, procedure, "with" or thunk end        *)
  S_SKIP       =  0007H;     (* Reserve symbol space in $$Symbols table      *)
  S_CVRESERVE  =  0008H;     (* Reserve symbol for CV internal use           *)
  S_OBJNAME    =  0009H;     (* path to object file name                     *)
  S_ENDARG     =  000AH;     (* end of argument list                         *)
  S_COBOLUDT   =  000BH;     (* special UDT for cobol -- not packed          *)

  S_BPREL16    =  0100H;     (* BP-relative                                  *)
  S_LDATA16    =  0101H;     (* Module-local symbol                          *)
  S_GDATA16    =  0102H;     (* Global data symbol                           *)
  S_PUB16      =  0103H;     (* a public symbol                              *)
  S_LPROC16    =  0104H;     (* Local procedure start                        *)
  S_GPROC16    =  0105H;     (* Global procedure start                       *)
  S_THUNK16    =  0106H;     (* Thunk Start                                  *)
  S_BLOCK16    =  0107H;     (* block start                                  *)
  S_WITH16     =  0108H;     (* with start                                   *)
  S_LABEL16    =  0109H;     (* code label                                   *)
  S_CEXMODEL16 =  010AH;     (* change execution model                       *)
  S_VFTABLE16  =  010BH;     (* address of virtual function table            *)
  S_REGREL16   =  010CH;     (* register relative address                    *)

  S_BPREL32    =  0200H;     (* BP-relative                                  *)     
  S_LDATA32    =  0201H;     (* Module-local symbol                          *)     
  S_GDATA32    =  0202H;     (* Global data symbol                           *)     
  S_PUB32      =  0203H;     (* a public symbol (CV internal reserved)       *)
  S_LPROC32    =  0204H;     (* Local procedure start                        *)     
  S_GPROC32    =  0205H;     (* Global procedure start                       *)     
  S_THUNK32    =  0206H;     (* Thunk Start                                  *)     
  S_BLOCK32    =  0207H;     (* block start                                  *)     
  S_WITH32     =  0208H;     (* with start                                   *)     
  S_LABEL32    =  0209H;     (* code label                                   *)
  S_CEXMODEL32 =  020AH;     (* change execution model                       *)
  S_VFTABLE32  =  020BH;     (* address of virtual function table            *)
  S_REGREL32   =  020CH;     (* register relative address                    *)
  S_LTHREAD32  =  020DH;     
  S_GTHREAD32  =  020EH;

  S_LPROCMIPS  =  0300H;     (* Local procedure start                        *)
  S_GPROCMIPS  =  0301H;     (* Global procedure start                       *)
  
  S_PROCREF    =  0400H;     (* Procedure reference                          *)
  S_DATAREF    =  0401H;     (* Data reference                               *)
  S_ALIGN      =  0402H;     (* Page Alignment                               *)

  (* Enumerator MFHash *)
  OMFHASH_NONE     =                   0;
  OMFHASH_SUMUC16  =                   1;
  OMFHASH_SUMUC32  =                   2;
  OMFHASH_ADDR16   =                   3;
  OMFHASH_ADDR32   =                   4;

  maxArrayLength   =                   MAX(INTEGER);


TYPE

  PCharArray           =               POINTER TO ARRAY maxArrayLength OF CHAR;
  PWordArray           =               POINTER TO ARRAY maxArrayLength OF WinDef.WORD;
  PDWordArray          =               POINTER TO ARRAY maxArrayLength OF WinDef.DWORD;

  (* CodeView Debug OMF signature.                                           *)
  (* The signature at the end of the file is                                 *)
  (* a negative offset from the end of the file to another signature.  At    *)
  (* the negative offset (base address) is another signature whose filepos   *)
  (* field points to the first OMFDirHeader in a chain of directories.       *)
  (* The NB05 signature is used by the link utility to indicated a           *)
  (* completely unpacked file.  The NB06 signature is used by ilink to       *)
  (* indicate that the executable has had CodeView information from an       *)
  (* incremental link appended to the executable.                            *)
  (* The NB07 signature is used by cvpack to indicate that the CodeView      *)
  (* Debug OMF has been packed.  CodeView will only process executables with *)
  (* the NB07 signature.                                                     *)
  POMFSignature =                      POINTER TO OMFSignature;
  OMFSignature = RECORD [_NOTALIGNED]
      Signature:                       ARRAY 4 OF CHAR;    (* "NBxx"         *)
      filepos:                         LONGINT;            (* offset in file *)
  END (* OMFSignature *) ;

  (* DIRECTORY INFORMATION STRUCTURE                                         *)
  (* This structure contains the information describing the directory.       *)
  (* It is pointed to by the signature at the base address or the directory  *)
  (* link field of a preceeding directory.                                   *)
  (* The directory entries immediately follow this structure.                *)
  POMFDirHeader    =                   POINTER TO OMFDirHeader;
  OMFDirHeader = RECORD [_NOTALIGNED]
      cbDirHeader:                     WinDef.WORD;        (* length of this structure *)
      cbDirEntry:                      WinDef.WORD;        (* number of bytes in each directory entry *)
      cDir:                            WinDef.DWORD;       (* number of directory entries *)
      lfoNextDir:                      LONGINT;            (* offset from base of next deirectory *)
      flags:                           WinDef.DWORD;       (* status flags   *)
  END (* OMFDirHeader *) ;

  (* DIRECTORY STRUCTURE                                                     *)
  (* The data in this structure is used to reference the data for each       *)
  (* subsection of the CodeView Debug OMF information.  Tables that are      *)
  (* not associated with a specific module will have a module index of       *)
  (* oxffff.  These tables are the global types table, the global symbol     *)
  (* table, the global public table and the library table.                   *)
  POMFDirEntry =                       POINTER TO OMFDirEntry;
  OMFDirEntry = RECORD [_NOTALIGNED]
      subsection:                      WinDef.WORD;        (* subsection type (sst...) *)
      iMod:                            WinDef.WORD;        (* module index   *)
      lfo:                             LONGINT;            (* large file offset of subsection *)
      cb:                              WinDef.DWORD;       (* number of bytes in subsection *)
  END (* OMFDirEntry *) ;

  (* sstModule (120H)                               PER MODULE INFORMATION   *)
  (* information describing each segment in a module                         *)
  POMFSegDesc  =                       POINTER TO OMFSegDesc;
  OMFSegDesc = RECORD [_NOTALIGNED]
      Seg:                             WinDef.WORD;        (* segment index  *)
      pad:                             WinDef.WORD;        (* pad to maintain alignment *)
      offset:                          WinDef.DWORD;       (* offset of code in segment *)
      cbSeg:                           WinDef.DWORD;       (* number of bytes in segment *)
  END (* OMFSegDesc *) ;
  (* There is one of these subsection entries for each module in the         *)
  (* executable.  The entry is generated by link/ilink.                      *)
  (* This table will probably require padding because of the variable        *)
  (* length module name.                                                     *)
  POMFSegDescArray     =               POINTER TO ARRAY maxArrayLength OF OMFSegDesc;
  POMFModule   =                       POINTER TO OMFModule;
  OMFModule = RECORD [_NOTALIGNED]
      ovlNumber:                       WinDef.WORD;        (* overlay number *)
      iLib:                            WinDef.WORD;        (* library that the module was linked from *)
      cSeg:                            WinDef.WORD;        (* count of number of segments in module *)
      Style:                           ARRAY 2 OF CHAR;    (* debugging style "CV" *)
      SegInfo:                         ARRAY 1 OF OMFSegDesc;
      NameLength:                      SHORTINT;           (* length prefixed module name paded to *)
      Name:                            ARRAY 1 OF CHAR;    (* long word boundary *)
  END (* OMFModule *) ;

  (* sstSrcModule (127H)            source line number to addressing mapping *)
  (* This table is generated by the link/ilink utility from line number      *)
  (* information contained in the object file OMF data.  This table contains *)
  (* only the code contribution for one segment from one source file.        *)
  POMFSourceLine  =                    POINTER TO OMFSourceLine;
  OMFSourceLine   = RECORD [_NOTALIGNED]
      Seg:                             WinDef.WORD;        (* linker segment index *)
      cPair:                           WinDef.WORD;        (* count of line/offset pairs *)
                                                           (* array of offsets in segment *)
      offset:                          ARRAY 1 OF WinDef.DWORD;
                                                           (* array of line number in source *)
      linenumber:                      ARRAY 1 OF WinDef.DWORD;
  END (* OMFSourceLine *);
  
  (* This table is generated by the linker                                   *)
  POMFSourceLineArray     =            POINTER TO ARRAY maxArrayLength OF POMFSourceLine;
  POMFSourceFile  =                    POINTER TO OMFSourceFile;
  OMFSourceFile   = RECORD [_NOTALIGNED]
      cSeg:                            WinDef.WORD;        (* number of segments from source file *)
      reserved:                        WinDef.WORD;
      (* base of OMFSourceLine tables                                        *)
      (* this array is followed by array of segment start/end pairs followed *)
      (* by an array of linker indices for each segment in the file          *)
      baseSrcLn:                       ARRAY 1 OF POMFSourceLine;
      StartEnd:                        ARRAY 2 OF WinDef.DWORD;
      cbName:                          SHORTINT;           (* WinDef.WORD; *)
      Name:                            ARRAY 1 OF CHAR;
  END (* OMFSourceFile *) ;
  (* This structure describes the number and location of the OMFAddrLine     *)
  (* tables for a module.  The offSourceLine entries are relative to the        *)
  (* beginning of this structure.                                            *)
  POMFSourceFileArray     =               POINTER TO ARRAY maxArrayLength OF POMFSourceFile;
  POMFSourceModule  =                     POINTER TO OMFSourceModule;
  OMFSourceModule = RECORD [_NOTALIGNED]
      cFile:                           WinDef.WORD;        (* number of OMFSourceTables *)
      cSeg:                            WinDef.WORD;        (* number of segments in module *)
      (* base of OMFSourceFile table                                         *)
      (* this array is followed by an array of segment start/end pairs       *)
      (* followed by an array of linker indices for each segment in the      *)
      (* module                                                              *)
      baseSrcFile:                     ARRAY 1 OF POMFSourceFile;
      StartEnd:                        ARRAY 2 OF WinDef.DWORD;
      seg:                             ARRAY 1 OF WinDef.WORD
  END (* OMFSourceModule *) ;

  (* sstLibraries (128H)                                                     *)
  OMFLibrary = RECORD [_NOTALIGNED]
      cbLibs:                          CHAR;               (* count of library names *)
      Libs:                            ARRAY 1 OF CHAR;    (* array of length prefixed lib names *)
                                                           (* firts entry zero length            *)
  END (* OMFLibrary *) ;

  (* sstGlobalPub (12AH)                            SYMBOL HASH TABLE FORMAT *)
  PDATASYM32           =               POINTER TO DATASYM32;
  DATASYM32  = RECORD
    reclen:                            WinDef.WORD;        (* Record length *)
    rectyp:                            WinDef.WORD;        (* S_LDATA32, S_GDATA32, S_LTHREAD32, S_GTHREAD32 or S_PUB32 *)
    off:                               WinDef.DWORD;
    seg:                               WinDef.WORD;
    typind:                            WinDef.WORD;        (* Type index *)
    cbName:                            SHORTINT;           (* Length-prefixed ... *)
    name:                              ARRAY 128 OF CHAR;  (*                 ... name *)
  END (* RECORD DATASYM32   *);
  PUDTSYM              =               POINTER TO UDTSYM;
  UDTSYM     = RECORD
    reclen:                            WinDef.WORD;        (* Record length *)
    rectyp:                            WinDef.WORD;        (* S_UDT, S_COBOLUDT *)
    typind:                            WinDef.WORD;        (* Type index *)
    cbName:                            SHORTINT;           (* Length-prefixed ... *)
    name:                              ARRAY 128 OF CHAR;  (*                 ... name *)
  END (* RECORD UDTSYM *);
  PREFSYM              =               POINTER TO REFSYM;
  REFSYM     = RECORD
    reclen:                            WinDef.WORD;        (* Record length *)
    rectyp:                            WinDef.WORD;        (* S_PROCREF, S_DATAREF *)
    sumName:                           WinDef.DWORD;       (* SUC of the name      *)
    ibSym:                             WinDef.DWORD;       (* Offset of actual symbol in $$Symbols *)
    imod:                              WinDef.WORD;        (* Module containing the actual symbol  *)
    usFill:                            WinDef.WORD;        (* align this record *) 
  END (* RECORD REFSYM *);    
  (* This structure immediately preceeds the global publics table and        *)
  (* global symbol tables.                                                   *)
  POMFSymHash          =               POINTER TO OMFSymHash;
  OMFSymHash = RECORD [_NOTALIGNED]
      symhash:                         WinDef.WORD;
      addrhash:                        WinDef.WORD;
      cbSymbol:                        WinDef.DWORD;
      cbSymHash:                       WinDef.DWORD;
      cbAddrHash:                      WinDef.DWORD;
  END (* OMFSymHash *) ;

  (* GLOBAL TYPES SUBSECTION FORMAT                                          *)
  (* This structure immediately preceeds the global types table.             *)
  (* The offsets in the typeOffset array are relative to the address of      *)
  (* ctypes.  Each type entry following the typeOffset array must begin on a *)
  (* long word boundary.                                                     *)
  OMFTypeFlags = RECORD [_NOTALIGNED]
      sig:                             WinDef.DWORD;
      unused:                          WinDef.DWORD;
  END (* OMFTypeFlags *) ;

  (* sstGlobalTypes (12BH)                                                   *)
  OMFGlobalTypes = RECORD [_NOTALIGNED]
      flags:                           OMFTypeFlags;
      cTypes:                          WinDef.DWORD;       (* number of types *)
                                                           (* array of offsets to types *)
      typeOffset:                      ARRAY 1 OF WinDef.DWORD;
  END (* OMFGlobalTypes *) ;

  (* sstPreCompMap (130H)                                                    *)
  (* PRECOMPILED TYPES MAPPING TABLE                                         *)
  (* This table should be ignored by all consumers except the incremental    *)
  (* packer.                                                                 *)
  OMFPreCompMap = RECORD [_NOTALIGNED]
      FirstType:                       WinDef.WORD;        (* first precompiled type index *)
      cTypes:                          WinDef.WORD;        (* number of precompiled types *)
      signature:                       WinDef.DWORD;       (* precompiled types signature *)
      pad:                             WinDef.WORD;
(*      map:                             ARRAY 1 OF CV_typ_t; mapping of precompiled types *)
  END (* OMFPreCompMap *) ;

  (* sstMPC (12CH)                                            Pcode support. *)
  (* This subsection contains debug information generated by the MPC utility *)
  (* used to process Pcode executables.                                      *)
  (* Currently it contains a mapping table from segment index (zero based)   *)
  (* to frame paragraph.  MPC converts segmented exe's to non-segmented      *)
  (* exe's for DOS support.  To avoid backpatching all CV info, this table   *)
  (* is provided for the mapping.  Additional info may be provided in the    *)
  (* future for profiler support.                                            *)
  OMFMpcDebugInfo = RECORD [_NOTALIGNED]
      cSeg:                            WinDef.WORD;
      mpSegFrame:                      ARRAY 1 OF WinDef.WORD;
  END (* OMFMpcDebugInfo *) ;


VAR
  ActLine,
  MyLine:                              Global.ScreenLineP;
  MyChars:                             ARRAY 128 OF CHAR;
  NumberOfLines:                       LONGINT;
  
  lfaBase:                             LONGINT;
  
  MyOMFSignature:                      POMFSignature;
  MyOMFDirHeader:                      POMFDirHeader;
  ActOMFDirEntry:                      POMFDirEntry;

  ActOMFSegDesc:                       POMFSegDesc;
  
  ActOMFModule:                        POMFModule;
  ActOMFSourceModule:                  POMFSourceModule;
  ActOMFSourceFile:                    POMFSourceFile;
  ActOMFSourceLine:                    POMFSourceLine;
  
  ActOMFGlobalPub,
  ActOMFGlobalSym,
  ActOMFStaticSym,
  ActOMFSymHash:                       POMFSymHash;

  OMFSourceModulePOMFSourceFile:       POMFSourceFileArray;
  OMFSourceFilePOMFSourceLine:         POMFSourceLineArray;
  OMFSourceFileName:                   PCharArray;
  OMFSourceLineOffset:                 PDWordArray;
  OMFSourceLineLineNumber:             PWordArray;
  ActDATASYM32:                        PDATASYM32;
  ActUDTSYM:                           PUDTSYM;
  ActREFSYM:                           PREFSYM;
  OMFMpcDebugInfoWORD:                 PWordArray;
  OMFLibraryCHAR:                      PCharArray;
  OMFModuleCHAR:                       PCharArray;
  SymbolString:                        PCharArray;

  PDebugDirectory:                     Global.PDebugDirectory;


(*****************************************************************************)
(*                                                                           *)
(* WriteCodeViewData                                                         *)
(* Generates the lines to be displayed when a Codeview section is found      *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteCodeviewData           (MylfaBase:          LONGINT;
                                       VAR NumberOfCol:    LONGINT);

VAR
  i, j:                                INTEGER;
  BytesRead,
  MyPointer,
  MyPointer2,
  MyPointer3:                          LONGINT;
  
BEGIN
  
  (* read signature and offset *)
  MyPointer            := MylfaBase;
  MyOMFSignature       := SYSTEM.VAL(POMFSignature, MyPointer);
  COPY (Line0620, ActLine^.Text);
  FOR i:=0 TO 3 DO
    Strings.AppendChar(ActLine^.Text, MyOMFSignature^.Signature[i]);
  END;
  ActLine^.Format  := Global.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  (* read directory header *)
  MyPointer            := MylfaBase + MyOMFSignature^.filepos;
  MyOMFDirHeader       := SYSTEM.VAL(POMFDirHeader, MyPointer);
  COPY (Line0621, ActLine^.Text);
  ActLine^.Format  := Global.Header3;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (Line0622, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Strings.UHexStr(MyOMFDirHeader^.cbDirHeader, 4, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (Line0623, ActLine^.Text);
  Strings.UHexStr(MyOMFDirHeader^.cbDirEntry, 4, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (Line0624, ActLine^.Text);
  Strings.Str(MyOMFDirHeader^.cDir, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (Line0625, ActLine^.Text);
  Strings.UHexStr(MyOMFDirHeader^.lfoNextDir, 4, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (Line0626, ActLine^.Text);
  Strings.UHexStr(MyOMFDirHeader^.flags, 4, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  (* Read directory entries *)
  COPY (Line0630, ActLine^.Text);
  ActLine^.Format  := Global.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  INC(MyPointer, SIZE(OMFDirHeader));
  COPY (Line0631, ActLine^.Text);
  ActLine^.Format  := Global.Header3;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  1;
  FOR i:=1 TO MyOMFDirHeader^.cDir DO
    ActOMFDirEntry   := SYSTEM.VAL(POMFDirEntry, MyPointer);
    ActLine^.Text[0] := 0X;
    ActLine^.Type    :=  i;
    CASE ActOMFDirEntry^.subsection OF
      sstModule:
        MyChars  := "ssTModule";
      |
      sstTypes:
        MyChars  := "ssTTypes";
      |
      sstPublic:
        MyChars  := "ssTPublic";
      |
      sstPublicSym:
        MyChars  := "ssTPublicSym";
      |
      sstSymbols:
        MyChars  := "ssTSymbols";
      |
      sstAlignSym:
        MyChars  := "ssTAlignSym";
      |
      sstSrcLnSeg:
        MyChars  := "ssTSrcLnSeg";
      |
      sstSrcModule:
        MyChars  := "ssTSrcModule";
      |
      sstLibraries:
        MyChars  := "ssTLibraries";
      |
      sstGlobalSym:
        MyChars  := "ssTGlobalSym";
      |
      sstGlobalPub:
        MyChars  := "ssTGlobalPub";
      |
      sstGlobalTypes:
        MyChars  := "ssTGlobalTypes";
      |
      sstMPC:
        MyChars  := "ssTMPC";
      |
      sstSegMap:
        MyChars  := "ssTSegMap";
      |
      sstSegName:
        MyChars  := "ssTSegName";
      |
      sstPreComp:
        MyChars  := "ssTPreComp";
      |
      sstPreCompMap:
        MyChars  := "ssTPreCompMap";
      |
      sstOffsetMap16:
        MyChars  := "ssTOffsetMap16";
      |
      sstOffsetMap32:
        MyChars  := "ssTOffsetMap32";
      |
      sstFileIndex:
        MyChars  := "ssTFileIndex";
      |
      sstStaticSym:
        MyChars  := "ssTStaticSym";
      ELSE
        MyChars  := "unknown";
      
    END;
    Strings.RightAlign(MyChars, 15);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Str(ActOMFDirEntry^.iMod, MyChars);
    Strings.RightAlign(MyChars, 5);
    Strings.Append(ActLine^.Text, MyChars);
(*  Strings.HexStr(ActOMFDirEntry^.lfo, MyChars);          we won't display all this garbage
    Strings.RightAlign(MyChars, 10);                       but the real interesting information
    Strings.Append(ActLine^.Text, MyChars);
    Strings.HexStr(ActOMFDirEntry^.cb, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
*)
    CASE ActOMFDirEntry^.subsection OF
      sstModule:
        ActOMFModule     := SYSTEM.VAL(POMFModule, (MylfaBase + ActOMFDirEntry.lfo));
        Strings.Str(ActOMFModule^.ovlNumber, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFModule^.iLib, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFModule^.cSeg, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Append(ActLine^.Text, "   ");
        Strings.Append(ActLine^.Text, ActOMFModule^.Style);
        Strings.Append(ActLine^.Text, "   ");
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstTypes:
        MyChars  := "ssTTypes";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstPublic:
        MyChars  := "ssTPublic";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstPublicSym:
        MyChars  := "ssTPublicSym";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstSymbols:
        MyChars  := "ssTSymbols";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstAlignSym:
        MyChars  := "ssTAlignSym";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstSrcLnSeg:
        MyChars  := "ssTSrcLnSeg";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstSrcModule:
        ActOMFSourceModule  := SYSTEM.VAL(POMFSourceModule, (MylfaBase + ActOMFDirEntry.lfo));
        Strings.Str(ActOMFSourceModule^.cFile, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFSourceModule^.cSeg, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        ActOMFSourceFile    := SYSTEM.VAL(POMFSourceFile, (MylfaBase + ActOMFDirEntry.lfo 
                                                             + SYSTEM.VAL(LONGINT, ActOMFSourceModule^.baseSrcFile)));
        OMFSourceFileName   := SYSTEM.VAL(PCharArray, SYSTEM.ADR(ActOMFSourceFile.Name));
        Strings.Append(ActLine^.Text, "   ");
        FOR j:=0 TO ActOMFSourceFile^.cbName-1 DO
          Strings.AppendChar(ActLine^.Text, OMFSourceFileName[j]);
        END;
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstLibraries:
        MyChars  := "ssTLibraries";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstGlobalSym:
        ActOMFGlobalSym  := SYSTEM.VAL(POMFSymHash, (MylfaBase + ActOMFDirEntry.lfo));
        Strings.Str(ActOMFGlobalSym^.symhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalSym^.addrhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalSym^.cbSymbol, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalSym^.cbSymHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalSym^.cbAddrHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstGlobalPub:
        ActOMFGlobalPub  := SYSTEM.VAL(POMFSymHash, (MylfaBase + ActOMFDirEntry.lfo));
        Strings.Str(ActOMFGlobalPub^.symhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalPub^.addrhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalPub^.cbSymbol, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalPub^.cbSymHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFGlobalPub^.cbAddrHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstGlobalTypes:
        MyChars  := "ssTGlobalTypes";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstMPC:
        MyChars  := "ssTMPC";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstSegMap:
        MyChars  := "ssTSegMap";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstSegName:
        MyChars  := "ssTSegName";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstPreComp:
        MyChars  := "ssTPreComp";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstPreCompMap:
        MyChars  := "ssTPreCompMap";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstOffsetMap16:
        MyChars  := "ssTOffsetMap16";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstOffsetMap32:
        MyChars  := "ssTOffsetMap32";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstFileIndex:
        MyChars  := "ssTFileIndex";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      |
      sstStaticSym:
        ActOMFStaticSym  := SYSTEM.VAL(POMFSymHash, (MylfaBase + ActOMFDirEntry.lfo));
        Strings.Str(ActOMFStaticSym^.symhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFStaticSym^.addrhash, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFStaticSym^.cbSymbol, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFStaticSym^.cbSymHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFStaticSym^.cbAddrHash, MyChars);
        Strings.RightAlign(MyChars, 8);
        Strings.Append(ActLine^.Text, MyChars);
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      ELSE
        MyChars  := "unknown";
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      
    END;

    INC(MyPointer, SIZE(OMFSegDesc));

  END;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

END WriteCodeviewData;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines06                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* DataDirectories 06 mode (Debug Directory)                                 *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  FirstLineP         pointer to the first line of screen memory            *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LONGINT           number of generated lines                             *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLines06*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  Done:                                BOOLEAN;
  MyPointer,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  NumberOfBytes:                       LONGINT;
  MyPDWordArray:                       PDWordArray;
  
BEGIN;

  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  0;
  NumberOfLines    :=  0;
  Done             := FALSE;
  PDebugDirectory  := Global.FirstDebugDirectory;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  IF Global.MyFileDescription.FileType=Global.FileTypeOBJ THEN
    PDebugDirectory  := PDebugDirectory^.Next;
    PDebugDirectory  := PDebugDirectory^.Next;
    MyPDWordArray    := SYSTEM.VAL(PDWordArray, PDebugDirectory^.PDebugDirectory);
    FOR i:=0 TO 20 DO
      COPY (Line0650, ActLine^.Text);
      Strings.UHexStr(MyPDWordArray[i], 4, MyChars);
      Strings.Append(ActLine^.Text, MyChars);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END (* FOR i:=0 TO 20 *);
    RETURN NumberOfLines
  END;

  COPY (Line0601, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  ActLine^.Format  := Global.Text01;
  
  IF PDebugDirectory=NIL THEN
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    COPY ("No Debug Information found.", ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN NumberOfLines;
  END;
  
  REPEAT

    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    COPY (Line0602, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.Characteristics, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0603, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.TimeDateStamp, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0604, ActLine^.Text);
    Strings.Str(PDebugDirectory^.PDebugDirectory^.MajorVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(PDebugDirectory^.PDebugDirectory^.MinorVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0606, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.Type, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    CASE PDebugDirectory^.PDebugDirectory^.Type OF
      WinNT.IMAGE_DEBUG_TYPE_UNKNOWN:                      (* 00 *)
        Strings.Append(ActLine^.Text, "  (Unknown).");
      |
      WinNT.IMAGE_DEBUG_TYPE_COFF:                         (* 01 *)
        Strings.Append(ActLine^.Text, "  (COFF).");
      |
      WinNT.IMAGE_DEBUG_TYPE_CODEVIEW:                     (* 02 *)
        Strings.Append(ActLine^.Text, "  (CodeView).");
      |
      WinNT.IMAGE_DEBUG_TYPE_FPO:                          (* 03 *)
        Strings.Append(ActLine^.Text, "  (FPO).");
      |
      WinNT.IMAGE_DEBUG_TYPE_MISC:                         (* 04 *)
        Strings.Append(ActLine^.Text, "  (Misc.).");
      |
      WinNT.IMAGE_DEBUG_TYPE_EXCEPTION:                    (* 05 *)
        Strings.Append(ActLine^.Text, "  (Exception).");
      |
      WinNT.IMAGE_DEBUG_TYPE_FIXUP:                        (* 06 *)
        Strings.Append(ActLine^.Text, "  (Fixup).");
      |
      WinNT.IMAGE_DEBUG_TYPE_OMAP_TO_SRC:                  (* 07 *)
        Strings.Append(ActLine^.Text, "  (OMAP to Source).");
      |
      WinNT.IMAGE_DEBUG_TYPE_OMAP_FROM_SRC:                (* 08 *)
        Strings.Append(ActLine^.Text, "  (OMAP from Source).");
      ELSE
        ;
    END;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    COPY (Line0607, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.SizeOfData, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0608, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.AddressOfRawData, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0609, ActLine^.Text);
    Strings.UHexStr(PDebugDirectory^.PDebugDirectory^.PointerToRawData, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    CASE PDebugDirectory^.PDebugDirectory^.Type OF
      WinNT.IMAGE_DEBUG_TYPE_CODEVIEW:                     (* 02 *)
        lfaBase    := Global.MyFileDescription.lpFileBase + PDebugDirectory^.PDebugDirectory^.PointerToRawData;
        WriteCodeviewData(lfaBase, NumberOfCol);
      ELSE
        ;
    END;

    IF PDebugDirectory^.Next=NIL THEN
      Done := TRUE;
    ELSE
      PDebugDirectory := PDebugDirectory^.Next;
    END;
    
  UNTIL Done;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  RETURN NumberOfLines
  
END WriteLines06;


(*****************************************************************************)
(*                                                                           *)
(* WriteLinesLN                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* LineNumber mode                                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLinesLN*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActSectionHeader:                    Global.PSectionHeader;
  i, j:                                INTEGER;
  k,
  BytesRead,
  MyPointer,
  MyPointer2,
  MyPointer3,
  Offset:                              LONGINT;
  
BEGIN
  
  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  0;
  NumberOfLines    :=  0;
  PDebugDirectory  := Global.FirstDebugDirectory;
  
  COPY (Line1000, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  LOOP
    CASE PDebugDirectory^.PDebugDirectory^.Type OF
      WinNT.IMAGE_DEBUG_TYPE_CODEVIEW:                     (* 02 *)
        Strings.Append(ActLine^.Text, "  (CodeView).");
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        EXIT;
      ELSE
        ;
    END (* CASE PDebugDirectory^.PDebugDirectory^.Type *);
    IF PDebugDirectory^.Next=NIL THEN
      Strings.Append(ActLine^.Text, "  not found.");
      ActLine^.Format  := Global.Text01;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      RETURN NumberOfLines
    ELSE
      PDebugDirectory := PDebugDirectory^.Next;
    END (* IF PDebugDirectory^.Next=NIL *);
    
  END (* LOOP *);

  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  lfaBase    := Global.MyFileDescription.lpFileBase + PDebugDirectory^.PDebugDirectory^.PointerToRawData;

  (* read signature and offset *)
  MyPointer            := lfaBase;
  MyOMFSignature       := SYSTEM.VAL(POMFSignature, MyPointer);

  (* read directory header *)
  MyPointer            := lfaBase + MyOMFSignature^.filepos;
  MyOMFDirHeader       := SYSTEM.VAL(POMFDirHeader, MyPointer);
  INC(MyPointer, SIZE(OMFDirHeader));
  
  (* Read directory entries *)
  FOR i:=1 TO MyOMFDirHeader^.cDir DO
    ActOMFDirEntry   := SYSTEM.VAL(POMFDirEntry, MyPointer);
    IF ActOMFDirEntry^.subsection=sstSrcModule THEN        (* describes the source line number to address mapping *)
      ActOMFSourceModule  := SYSTEM.VAL(POMFSourceModule, (lfaBase + ActOMFDirEntry.lfo));
      IF Global.Level=0 THEN
        COPY(Line1002, ActLine^.Text);
        ActLine^.Format  := Global.Header3;
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        ActLine^.Text[0] := 0X;
        ActLine^.Format  := Global.Text01;
        Strings.Str(ActOMFSourceModule^.cFile, MyChars);
        Strings.RightAlign(MyChars, 20);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFSourceModule^.cSeg, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(SYSTEM.VAL(LONGINT, ActOMFSourceModule^.baseSrcFile), 4, MyChars);
        Strings.RightAlign(MyChars, 15);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(ActOMFSourceModule^.StartEnd[0], 4, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(ActOMFSourceModule^.StartEnd[1], 4, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(ActOMFSourceModule^.seg[0], MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      END (* IF Global.Level=0 *);
      
      ActOMFSourceFile    := SYSTEM.VAL(POMFSourceFile, (lfaBase + ActOMFDirEntry.lfo 
                                                           + SYSTEM.VAL(LONGINT, ActOMFSourceModule^.baseSrcFile)));
      OMFSourceFileName   := SYSTEM.VAL(PCharArray, SYSTEM.ADR(ActOMFSourceFile.Name));
      IF Global.Level=0 THEN
        COPY(Line1003, ActLine^.Text);
        ActLine^.Format  := Global.Header2;
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        ActLine^.Text[0] := 0X;
        ActLine^.Format  := Global.Text01;
        Strings.Str(ActOMFSourceFile^.cSeg, MyChars);
        Strings.RightAlign(MyChars, 30);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(SYSTEM.VAL(LONGINT, ActOMFSourceFile^.baseSrcLn), 4, MyChars);
        Strings.RightAlign(MyChars, 15);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(ActOMFSourceFile^.StartEnd[0], 4, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(ActOMFSourceFile^.StartEnd[1], 4, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Append(ActLine^.Text, "   ");
      ELSE
        ActLine^.Text[0]    :=0X;
      END (* IF Global.Level=0 *);

      FOR j:=0 TO ActOMFSourceFile^.cbName-1 DO
        Strings.AppendChar(ActLine^.Text, OMFSourceFileName[j]);
      END;
      ActLine^.Format  := Global.Header2;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

      ActOMFSourceLine         := SYSTEM.VAL(POMFSourceLine, (lfaBase + ActOMFDirEntry.lfo 
                                                                + SYSTEM.VAL(LONGINT, ActOMFSourceFile^.baseSrcLn)));

      OMFSourceLineOffset     := SYSTEM.VAL(PDWordArray, SYSTEM.ADR(ActOMFSourceLine.offset));
      OMFSourceLineLineNumber := SYSTEM.VAL(PWordArray, SYSTEM.ADR(OMFSourceLineOffset[ActOMFSourceLine^.cPair]));
      COPY(Line1004, ActLine^.Text);
      ActLine^.Format  := Global.Header3;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      ActLine^.Text[0] := 0X;
      ActLine^.Format  := Global.Text01;
      Strings.Str(ActOMFSourceLine^.Seg, MyChars);
      Strings.RightAlign(MyChars, 2);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(ActOMFSourceLine^.cPair, MyChars);
      Strings.RightAlign(MyChars, 15);
      Strings.Append(ActLine^.Text, MyChars);
      ActSectionHeader := Global.FirstSectionHeader;
      k                := 1;
      WHILE k<ActOMFSourceLine^.Seg DO
        ActSectionHeader := ActSectionHeader^.Next;
        INC(k);
      END;
      Offset           := Global.PMyNT_Header.OptionalHeader.ImageBase + ActSectionHeader^.PSectionHeader.VirtualAddress;
      Strings.UHexStr(OMFSourceLineOffset[0]+Offset, 4, MyChars);
      Strings.InsertChar(":", MyChars, 5);
      Strings.RightAlign(MyChars, 15);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(OMFSourceLineLineNumber[0], MyChars);
      Strings.RightAlign(MyChars, 13);
      Strings.Append(ActLine^.Text, MyChars);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      (* write offset and linenumber pairs *)
      FOR j:=1 TO ActOMFSourceLine^.cPair-1 DO
        ActLine^.Text[0]  := 0X;
        ActSectionHeader := Global.FirstSectionHeader;
        k                := 1;
        WHILE k<ActOMFSourceLine^.Seg DO
          ActSectionHeader := ActSectionHeader^.Next;
          INC(k);
        END;
        Offset           := Global.PMyNT_Header.OptionalHeader.ImageBase + ActSectionHeader^.PSectionHeader.VirtualAddress;
        Strings.UHexStr(OMFSourceLineOffset[j]+Offset, 4, MyChars);
        Strings.InsertChar(":", MyChars, 5);
        Strings.RightAlign(MyChars, 32);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Str(OMFSourceLineLineNumber[j], MyChars);
        Strings.RightAlign(MyChars, 13);
        Strings.Append(ActLine^.Text, MyChars);
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;

      COPY (LineEmpty, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    END (*  IF ActOMFDirEntry^.subsection=sstSourceModule  *);

    INC(MyPointer, SIZE(OMFSegDesc));

  END (* FOR i:=1 TO MyOMFDirHeader^.cDir *);
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines

END WriteLinesLN;


(*****************************************************************************)
(*                                                                           *)
(* WriteLinesST                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* Symbol Table mode (Public Symbols)                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLinesST*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActLength,
  i:                                   INTEGER;
  ActSectionHeader:                    Global.PSectionHeader;
  j, k,
  BytesRead,  
  MyPointer,
  MyPointer2,
  MyPointer3,
  Offset:                              LONGINT;
  
BEGIN
  
  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  0;
  NumberOfLines    :=  0;
  PDebugDirectory  := Global.FirstDebugDirectory;
  
  COPY (Line1100, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  LOOP
    CASE PDebugDirectory^.PDebugDirectory^.Type OF
      WinNT.IMAGE_DEBUG_TYPE_CODEVIEW:                     (* 02 *)
        Strings.Append(ActLine^.Text, "  (CodeView).");
        Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        EXIT;
      ELSE
        ;
    END (* CASE ... *);
    IF PDebugDirectory^.Next=NIL THEN
      Strings.Append(ActLine^.Text, "  not found.");
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      RETURN NumberOfLines
    ELSE
      PDebugDirectory := PDebugDirectory^.Next;
    END (* IF PDebugDirectory^.Next=NIL *);
    
  END (* LOOP *);

  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  lfaBase    := Global.MyFileDescription.lpFileBase + PDebugDirectory^.PDebugDirectory^.PointerToRawData;

  (* read signature and offset *)
  MyPointer            := lfaBase;
  MyOMFSignature       := SYSTEM.VAL(POMFSignature, MyPointer);

  (* read directory header *)
  MyPointer            := lfaBase + MyOMFSignature^.filepos;
  MyOMFDirHeader       := SYSTEM.VAL(POMFDirHeader, MyPointer);
  INC(MyPointer, SIZE(OMFDirHeader));
  
  (* Read directory entries *)
  FOR i:=1 TO MyOMFDirHeader^.cDir DO
    ActOMFDirEntry   := SYSTEM.VAL(POMFDirEntry, MyPointer);
    IF ActOMFDirEntry^.subsection=sstGlobalPub THEN        (* describes the public symbol to address mapping *)
      ActOMFGlobalPub  := SYSTEM.VAL(POMFSymHash, (lfaBase + ActOMFDirEntry.lfo));
      MyPointer        := SYSTEM.VAL(LONGINT, ActOMFGlobalPub);
      Offset           := Global.PMyNT_Header.OptionalHeader.ImageBase;
      COPY (Line1101, ActLine^.Text);
      ActLine^.Format  := Global.Text01;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      COPY (LineEmpty, ActLine^.Text);
      Strings.Str(ActOMFGlobalPub^.symhash, MyChars);
      Strings.RightAlign(MyChars, 28);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(ActOMFGlobalPub^.addrhash, MyChars);
      Strings.RightAlign(MyChars, 15);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(ActOMFGlobalPub^.cbSymbol, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(ActOMFGlobalPub^.cbSymHash, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.Str(ActOMFGlobalPub^.cbAddrHash, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
      COPY (LineEmpty, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      COPY (Line1102, ActLine^.Text);
      ActLine^.Format  := Global.Header3;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      ActLine^.Format  := Global.Text01;
      j              := SIZE(OMFSymHash);
      WHILE j<ActOMFGlobalPub^.cbSymbol-20 DO
        ActDATASYM32     := SYSTEM.VAL(PDATASYM32, (MyPointer + j));
        CASE ActDATASYM32^.rectyp OF
          S_PUB32:
            COPY (LineEmpty, ActLine^.Text);
            ActSectionHeader := Global.FirstSectionHeader;
            k                := 1;
            WHILE k<ActDATASYM32^.seg DO
              ActSectionHeader := ActSectionHeader^.Next;
              INC(k);
            END;
            Offset           := Global.PMyNT_Header.OptionalHeader.ImageBase + ActSectionHeader^.PSectionHeader.VirtualAddress;
            FOR k:=0 TO ActDATASYM32^.cbName-1 DO
              Strings.AppendChar(ActLine^.Text, ActDATASYM32^.name[k]);
            END;
            FOR k:=ActDATASYM32^.cbName+3 TO 40 DO
              Strings.AppendChar(ActLine^.Text, " ");
            END;
            Strings.Str(ActDATASYM32^.seg, MyChars);
            Strings.RightAlign(MyChars, 14);
            Strings.Append(ActLine^.Text, MyChars);
            Strings.UHexStr((ActDATASYM32^.off+Offset), 4, MyChars);
            Strings.InsertChar(":", MyChars, 5);
            Strings.RightAlign(MyChars, 21);
            Strings.Append(ActLine^.Text, MyChars);
            Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
          (* S_PUB32 *)
          ELSE
            COPY ("Record Type: ", ActLine^.Text);
            Strings.UHexStr(ActDATASYM32^.rectyp, 2, MyChars);
            Strings.RightAlign(MyChars, 6);
            Strings.Append(ActLine^.Text, MyChars);
            Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        END (* CASE ActDATASYM32^.rectyp *);
        j := j + ActDATASYM32^.reclen + 2;
      END (* While j<ActOMFGlobalPub^.cbSymbol *);
    END;
    INC(MyPointer, SIZE(OMFSegDesc));

  END (* FOR i:=1 TO MyOMFDirHeader^.cDir *);
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines;

END WriteLinesST;


(*****************************************************************************)
(*                                                                           *)
(* WriteLinesST2                                                             *)
(* Generates the lines to be displayed when the program switches to          *)
(* Symbol Table mode (Global Symbols)                                        *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLinesST2*              (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActLength,
  i:                                   INTEGER;
  ActSectionHeader:                    Global.PSectionHeader;
  j, k,
  BytesRead,  
  MyPointer,
  MyPointer2,
  MyPointer3,
  Offset:                              LONGINT;
  
BEGIN
  
  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  0;
  NumberOfCol      :=  0;
  NumberOfLines    :=  0;
  PDebugDirectory  := Global.FirstDebugDirectory;
  
  COPY (Line1100, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  ActLine^.Format  := Global.Text01;
  
  IF Global.PMyNT_Header^.FileHeader.NumberOfSymbols=0 THEN
    COPY (LineEmpty, ActLine^.Text);
    Strings.Append(ActLine^.Text, "  not found.");
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN NumberOfLines
  ELSE
    Strings.UHexStr(Global.PMyNT_Header^.FileHeader.NumberOfSymbols, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append(ActLine^.Text, " Symbols at  ");
    Strings.UHexStr(Global.PMyNT_Header^.FileHeader.PointerToSymbolTable, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  END (* IF PDebugDirectory^.Next=NIL *);
    
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines;

END WriteLinesST2;


(*****************************************************************************)
(*                                                                           *)
(* GetLineNumber                                                             *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE GetLineNumber*              (MemAddress:         LONGINT;
                                       VAR ModuleName:     ARRAY OF CHAR;
                                       VAR LineNo:         LONGINT)
                                      :                    BOOLEAN;

VAR
  ActSectionHeader:                    Global.PSectionHeader;
  i, j, l:                             INTEGER;
  k,
  MyPointer,
  Offset:                              LONGINT;
  
BEGIN
  
  COPY ("################", ModuleName);
  LineNo               :=  0;
  PDebugDirectory      := Global.FirstDebugDirectory;
  
  LOOP
    CASE PDebugDirectory^.PDebugDirectory^.Type OF
      WinNT.IMAGE_DEBUG_TYPE_CODEVIEW:                     (* 02 *)
        EXIT;
      ELSE
        ;
    END (* CASE PDebugDirectory^.PDebugDirectory^.Type *);
    IF PDebugDirectory^.Next=NIL THEN
      COPY("Memory Address not found.", ModuleName);
      RETURN FALSE
    ELSE
      PDebugDirectory := PDebugDirectory^.Next;
    END (* IF PDebugDirectory^.Next=NIL *);
    
  END (* LOOP *);

  lfaBase              := Global.MyFileDescription.lpFileBase + PDebugDirectory^.PDebugDirectory^.PointerToRawData;

  (* read signature and offset *)
  MyPointer            := lfaBase;
  MyOMFSignature       := SYSTEM.VAL(POMFSignature, MyPointer);

  (* read directory header *)
  MyPointer            := lfaBase + MyOMFSignature^.filepos;
  MyOMFDirHeader       := SYSTEM.VAL(POMFDirHeader, MyPointer);
  INC(MyPointer, SIZE(OMFDirHeader));
  
  (* Read directory entries *)
  FOR i:=1 TO MyOMFDirHeader^.cDir DO
    ActOMFDirEntry   := SYSTEM.VAL(POMFDirEntry, MyPointer);
    IF ActOMFDirEntry^.subsection=sstSrcModule THEN        (* describes the source line number to address mapping *)
      ActOMFSourceModule          := SYSTEM.VAL(POMFSourceModule, (lfaBase + ActOMFDirEntry.lfo));
      ActOMFSourceFile            := SYSTEM.VAL(POMFSourceFile, (lfaBase + ActOMFDirEntry.lfo 
                                                           + SYSTEM.VAL(LONGINT, ActOMFSourceModule^.baseSrcFile)));
      OMFSourceFileName           := SYSTEM.VAL(PCharArray, SYSTEM.ADR(ActOMFSourceFile.Name));
      ActOMFSourceLine            := SYSTEM.VAL(POMFSourceLine, (lfaBase + ActOMFDirEntry.lfo 
                                                                + SYSTEM.VAL(LONGINT, ActOMFSourceFile^.baseSrcLn)));

      OMFSourceLineOffset         := SYSTEM.VAL(PDWordArray, SYSTEM.ADR(ActOMFSourceLine.offset));
      OMFSourceLineLineNumber     := SYSTEM.VAL(PWordArray,  SYSTEM.ADR(OMFSourceLineOffset[ActOMFSourceLine^.cPair]));
      ActSectionHeader         := Global.FirstSectionHeader;
      k                        := 1;
      WHILE k<ActOMFSourceLine^.Seg DO
        ActSectionHeader := ActSectionHeader^.Next;
        INC(k);
      END;
      Offset                   := Global.PMyNT_Header.OptionalHeader.ImageBase + ActSectionHeader^.PSectionHeader.VirtualAddress;
      (* read linenumber pairs *)
      FOR j:=1 TO ActOMFSourceLine^.cPair-1 DO
        ActSectionHeader := Global.FirstSectionHeader;
        k                := 1;
        WHILE k<ActOMFSourceLine^.Seg DO
          ActSectionHeader := ActSectionHeader^.Next;
          INC(k);
        END;
        Offset           := Global.PMyNT_Header.OptionalHeader.ImageBase + ActSectionHeader^.PSectionHeader.VirtualAddress;
        IF ((OMFSourceLineOffset[j]+Offset)>MemAddress) THEN
          RETURN TRUE
        END (* IF ((OMFSourceLineOffset[j]+Offset)>MemAddress) *);
        IF j=1 THEN
          ModuleName[0]            := 0X;
          FOR l:=0 TO ActOMFSourceFile^.cbName-1 DO
            Strings.AppendChar(ModuleName, OMFSourceFileName[l]);
          END;
        END (* IF j=1 *);
        LineNo := OMFSourceLineLineNumber[j];
      END (* FOR j:=1 TO ActOMFSourceLine^.cPair-1 *);

    END (*  IF ActOMFDirEntry^.subsection=sstSourceModule  *);

    INC(MyPointer, SIZE(OMFSegDesc));

  END (* FOR i:=1 TO MyOMFDirHeader^.cDir *);
  
  RETURN FALSE

END GetLineNumber;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  ;

END Dump_DD_debug.

