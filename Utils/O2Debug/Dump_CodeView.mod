(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Dump_CodeView                               V 1.42.00         *)
(*                                                         2002MAR16         *)
(*  PURPOSE:   displays the debug sections                                   *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   CVsstModule                                                             *)
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
(*  CREATED    2002JAN01                                                     *)
(*                                                                           *)
(*  UPDATED                                                                  *)
(*   2000SEP29 überarbeitet                                                  *)
(*                                                                           *)
(*  RELEASED                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE Dump_CodeView;

IMPORT
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  Strings, SYSTEM,
  Debug, Dump, Global, StatusLine, View;


CONST
  Version =                           "V 1.42.00";

  LineEmpty    =                      "   ";
  Line0000     =                      "DATA DIRECTORIES HEADER";

  Line0001     =                      "Entry ";
  Line0002     =                      "  Characteristics:              ";
  Line0003     =                      "  TimeDateStamp:                ";
  Line0004     =                      "  MajorVersion:                 ";
  Line0005     =                      "  MinorVersion:                 ";
  Line0006     =                      "  Name:                      ";
  Line0007     =                      "  Base:                         ";
  Line0008     =                      "  NumberOfFunctions:            ";
  Line0009     =                      "  NumberOfNames:                ";
  Line0010     =                      "  AddressOfFunctions:           ";
  Line0011     =                      "  AddressOfNames:               ";
  Line0012     =                      "  AddressOfNameOrdinals:        ";
  Line0020     =                      "  Ordn    EntryPt                Name";
  Line0620     =                      "CodeView Section:          ";
  Line0631     =                      "  Subsection   Index  ovl iLib cSeg Style";


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
  i:                                   INTEGER;
  Rectangle:                           WinDef.RECT;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  MyEntryNo:                           LONGINT;            (* number of the directory we are working on *)


(*****************************************************************************)
(*                                                                           *)
(* WriteLines                                                                *)
(* Generates the lines to be displayed in the DumpCV (CodeView) Window       *)
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

PROCEDURE WriteLines*                 (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActOMFDirEntry:                      POMFDirEntry;
  ActLine,
  MyLine:                              Global.ScreenLineP;
  MyChars:                             ARRAY 128 OF CHAR;
  BytesRead,
  MyPointer,
  MyPointer2,
  MyPointer3:                          LONGINT;
  Done:                                BOOLEAN;
  i:                                   INTEGER;
  MylfaBase:                           LONGINT;
  MyOMFSignature:                      POMFSignature;
  MyOMFDirHeader:                      POMFDirHeader;
  Number:                              ARRAY   5 OF CHAR;
  NumberOfLines:                       LONGINT;
  PDebugDirectory:                     Global.PDebugDirectory;

BEGIN;

  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Global.Text01;
  ActLine^.Usage   :=  0;
  MylfaBase        :=  0;
  NumberOfLines    :=  0;
  Done             := FALSE;
  PDebugDirectory  := Global.FirstDebugDirectory;

  REPEAT
    IF PDebugDirectory^.PDebugDirectory^.Type=WinNT.IMAGE_DEBUG_TYPE_CODEVIEW THEN
      MylfaBase  := Global.MyFileDescription.lpFileBase + PDebugDirectory^.PDebugDirectory^.PointerToRawData;
      Done       := TRUE
    END;

    IF PDebugDirectory^.Next=NIL THEN
      Done := TRUE;
    ELSE
      PDebugDirectory := PDebugDirectory^.Next;
    END;
  UNTIL Done;
  
  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text02;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  IF MylfaBase=0 THEN
    COPY (Line0001, ActLine^.Text);
    ActLine^.Format  := Global.Text02;
    Strings.Append (ActLine^.Text, " not found.");
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN 0
  ELSE
    COPY (Line0631, ActLine^.Text);
    ActLine^.Format  := Global.Header2;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;

  (* find the appropriate directory entry *)
  MyPointer            := MylfaBase;                       (* read signature and offset *)
  MyOMFSignature       := SYSTEM.VAL(POMFSignature, MyPointer);
                                                           (* read directory header *)
  MyPointer            := MylfaBase + MyOMFSignature^.filepos;
  MyOMFDirHeader       := SYSTEM.VAL(POMFDirHeader, MyPointer);
  INC(MyPointer, SIZE(OMFDirHeader));
  
  IF MyEntryNo>MyOMFDirHeader^.cDir THEN                   (* test the entry number *)
    COPY ("Directory entry(", ActLine^.Text);
    Strings.Str(MyEntryNo, MyChars);
    Strings.RightAlign(MyChars, 4);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append (ActLine^.Text, ") out of range(");
    Strings.Str(MyOMFDirHeader^.cDir, MyChars);
    Strings.RightAlign(MyChars, 4);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append (ActLine^.Text, ").");
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN 0
  END;

  FOR i:=1 TO MyEntryNo DO
    ActOMFDirEntry   := SYSTEM.VAL(POMFDirEntry, MyPointer);
    ActLine^.Text[0] := 0X;
    ActLine^.Type    :=  i;
    ActLine^.Format  := Global.Header3;
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
    END (* CASE ActOMFDirEntry^.subsection *);
    
    INC(MyPointer, SIZE(OMFSegDesc));

  END;

  Strings.RightAlign(MyChars, 15);
  Strings.Append(ActLine^.Text, MyChars);
  Strings.Str(ActOMFDirEntry^.iMod, MyChars);
  Strings.RightAlign(MyChars, 5);
  Strings.Append(ActLine^.Text, MyChars);
  Strings.HexStr(ActOMFDirEntry^.lfo, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);                                                              
      
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines
  
END WriteLines;


(*****************************************************************************)
(*                                                                           *)
(* CVsstModule                                                               *)
(* display the information of this section                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  hWnd       Handle des Fensters                                           *)
(*  message    was soll ich tun                                              *)
(*  wParam     Kommando                                                      *)
(*  lParam     zus. Information                                              *)
(*  EntryNo    directory number                                              *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE  CVsstModule*               (hWnd:               WinDef.HWND;        (* Window handle          *)
                                       message:            WinDef.UINT;        (* type of message        *)
                                       wParam:             WinDef.WPARAM;      (* additional information *)
                                       lParam:             WinDef.LPARAM;      (* additional information *)
                                       EntryNo:            LONGINT)
                                      :WinDef.LRESULT;

VAR
  MDICreateStruct:                     WinUser.MDICREATESTRUCTA;
  Title:                               ARRAY 256 OF CHAR;
  lpScreenMetric:                      Global.ScreenMetricP;
  PaintStructure:                      WinUser.PAINTSTRUCT;
  Result:                              LONGINT;
  ResultBool:                          WinDef.BOOL;
  hDC:                                 WinDef.HDC;
  xPos,
  yPos:                                LONGINT;
  
BEGIN

  (* Generate MDI Debug Window *)
  MDICreateStruct.szClass      := SYSTEM.ADR(Global.DumpClass);
  MDICreateStruct.szTitle      := SYSTEM.ADR(Global.MDIDumpCVTitle);
  MDICreateStruct.hOwner       := Global.hInstance;
  MDICreateStruct.x            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.y            := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cx           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.cy           := WinUser.CW_USEDEFAULT;
  MDICreateStruct.style        := WinUser.WS_CHILD +      (* Window style.                      *)
                                  WinUser.WS_CLIPCHILDREN +
                                  WinUser.WS_VISIBLE + WinUser.WS_VSCROLL + WinUser.WS_HSCROLL;
  Global.hWndDumpCV            := WinUser.SendMessageA(Global.hWndMDIClient, WinUser.WM_MDICREATE, 0, SYSTEM.ADR(MDICreateStruct));

  Title        := "Waiting ..";
  WHILE WinUser.IsWindowVisible(Global.hWndDumpCV)#WinDef.True DO
    StatusLine.SetText(Title, 2);
    Strings.AppendChar(Title, ".");
  END (* WHILE WinUser.IsWindowVisible(Global.hWndDumpCV)#WinDef.True *);
  
  MyEntryNo    := EntryNo;
  
  (* set data for screen metric *)
  lpScreenMetric                       := SYSTEM.VAL(Global.ScreenMetricP, 
                                                     WinUser.GetWindowLongA(Global.hWndDumpCV, Global.WXBScreenMetricP));

  lpScreenMetric^.NumberOfLines        := WriteLines(lpScreenMetric^.FirstLine, 
                                                     lpScreenMetric^.NumberOfColumns);

  RETURN 0;
  
END CVsstModule;


(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
BEGIN;

END Dump_CodeView.

