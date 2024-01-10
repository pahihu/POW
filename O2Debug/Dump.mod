(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Dump                                        V 2.00.19         *)
(*                                                         2003APR22         *)
(*  PURPOSE:   Processing PECOFF structure data of a file                    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   NextLine                                                                *)
(*   WriteLines                                                              *)
(*             Generate lines to be displayed                                *)
(*   WriteLinesArchiveLIB                                                    *)
(*             Generates the lines to be displayed when the program switches *)
(*             to DebugData mode and the file is of type Archive Library     *)
(*   DumpTheFile                                                             *)
(*             Generate MDI Dump Window & Read the PECOFF structure          *)
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

MODULE Dump;


IMPORT
  Common, Resource, UIStatusLine, UITabControl,
  Strings,
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  SYSTEM;

CONST
  Version*     =                      "V 2.00.19";
  Module*      =                      "Dump";
  ErrorNoOffset=                      Resource.IDM_Dump * 100;
  
  Indentation  =                      32;
  LineEmpty    =                      "   ";
  Line050      =                      "DOS HEADER VALUES";
  Line051      =                      "File Type:                      ";
  Line052      =                      "Magic number:                   ";
  Line053      =                      "Bytes on last page of file:     ";
  Line054      =                      "Pages in file:                  ";
  Line055      =                      "Relocations:                    ";
  Line056      =                      "Size of header in paragraphs:   ";
  Line057      =                      "Min extra paragraphs needed:    ";
  Line058      =                      "Max extra paragraphs needed:    ";
  Line059      =                      "Initial (relative) SS value:    ";
  Line060      =                      "Initial SP value:               ";
  Line061      =                      "Checksum:                       ";
  Line062      =                      "Initial IP value:               ";
  Line063      =                      "Initial (relative) CS value:    ";
  Line064      =                      "File address of reloc table:    ";
  Line065      =                      "Overlay number:                 ";
  Line066      =                      "Reserved words:                 ";
  Line067      =                      "OEM identifier (for e_oeminfo): ";
  Line068      =                      "OEM information; e_oemid spec.: ";
  Line069      =                      "Reserved words                  ";
  Line070      =                      "File address of new exe header: ";
  Line080      =                      "NT File Type:                   ";
  Line081      =                      "NT HEADER VALUES";
  Line082      =                      "  machine:                      ";
  Line083      =                      "  number of sections:           ";
  Line084      =                      "  time date stamp:              ";
  Line085      =                      "  file pointer to symbol table: ";
  Line086      =                      "  number of Symbols:            ";
  Line087      =                      "  size of optional header:      ";
  Line088      =                      "  characteristics:              ";
  Line200      =                      "                                 does not contain base relocations.";
  Line201      =                      "                                 image is valid.";
  Line202      =                      "                                 COFF line numbers removed.";
  Line203      =                      "                                 COFF symbol table entries removed.";
  Line204      =                      "                                 Aggressively trim working set.";
  Line206      =                      "                                 App can handle>2GB.";
  Line207      =                      "                                 Little endian: LSB precedes MSB.";
  Line208      =                      "                                 Machine based on 32-bit-word.";
  Line209      =                      "                                 debugging info removed.";
  Line210      =                      "                                 if image is in removable media, copy & run from swap.";
  Line212      =                      "                                 image file is system file, not user prog.";
  Line213      =                      "                                 image file is dynamic-link library.";
  Line214      =                      "                                 file should be run only on a UP machine.";
  Line215      =                      "                                 Big endian: MSB precedes LSB.";
  Line100      =                      "OPTIONAL HEADER";
  Line110      =                      "Standard Fields";
  Line111      =                      "  Magic Number:                 ";
  Line112      =                      "  Linker Version:                       ";
  Line113      =                      "  Minor Linker Version:         ";
  Line114      =                      "  Size Of Code:                 ";
  Line115      =                      "  Size Of Initialized Data:     ";
  Line116      =                      "  Size Of UnInitialized Data:   ";
  Line117      =                      "  Address Of Entry Point:       ";
  Line118      =                      "  Base Of Code:                 ";
  Line120      =                      "Windows Specific Fields";
  Line121      =                      "  Image Base:                   ";
  Line122      =                      "  Section Alignment:            ";
  Line123      =                      "  FileAlignment:                ";
  Line124      =                      "  Operating System Version:             ";
  Line125      =                      "  Minor Operating Syst Version: ";
  Line126      =                      "  Image version:                        ";
  Line127      =                      "  Minor Image Version:          ";
  Line128      =                      "  Subsystem Version:                    ";
  Line129      =                      "  Minor Subsystem Version:      ";
  Line130      =                      "  Win32 Version Value:          ";
  Line131      =                      "  Size Of Image:                ";
  Line132      =                      "  Size Of Headers:              ";
  Line133      =                      "  CheckSum:                     ";
  Line134      =                      "  Subsystem:                    ";
  Line135      =                      "  DLL Characteristics:          ";
  Line300      =                      "                                 Reserved.";
  Line311      =                      "                                 Do not bind image.";
  Line313      =                      "                                 Driver is a WMD driver.";
  Line315      =                      "                                 Image is Terminal Server aware.";
  Line136      =                      "  Size Of Stack Reserve:        ";
  Line137      =                      "  Size Of Stack Commit:         ";
  Line138      =                      "  Size Of Heap Reserve:         ";
  Line139      =                      "  Size Of Heap Commit:          ";
  Line140      =                      "  Loader Flags:                 ";
  Line141      =                      "  Number Of RVAs and Sizes:     ";
  Line150      =                      "DATA DIRECTORIES";
  Line151      =                      "    #  Name                             Address (RVA)       Size";
  Line160      =                      "SECTION HEADERS     (";
  Line161      =                      "    #  Name          Virtual Address     Virtual Size        Characteristics";

  Line500      =                      "ARCHIVE (LIBRARY) FILE FORMAT   ";
  Line501      =                      "Signature:                      ";
  Line509      =                      "1st Linker Member";
  Line510      =                      "2nd Linker Member";
  Line511      =                      "  name:                         ";
  Line512      =                      "  time date stamp:              ";
  Line513      =                      "  user  ID:                     ";
  Line514      =                      "  group ID:                     ";
  Line515      =                      "  mode:                         ";
  Line516      =                      "  size:                         ";
  Line520      =                      "First Linker Member             ";
  Line521      =                      "  number of members:            ";
  Line522      =                      "  number of symbols:            ";
  Line523      =                      "  archive member offset:   ";
  Line581      =                      "  IMPORT HEADER";
  Line582      =                      "  signature (1&2):             ";
  Line583      =                      "  version:                      ";
  Line584      =                      "  machine:                      ";
  Line585      =                      "  time date stamp:              ";
  Line586      =                      "  size of data:                 ";
  Line587      =                      "  ordinal/hint:                 ";
  Line588      =                      "  type:                         ";


TYPE
  String4T     =                       ARRAY   4 OF CHAR;
  String16T    =                       ARRAY  16 OF CHAR;
  StringT      =                       ARRAY 256 OF CHAR;
  String16P    =                       POINTER TO String16T;
  StringArrayP =                       POINTER TO StringT;
  ByteArray    =                       ARRAY MAX(INTEGER) OF SYSTEM.BYTE;
  ByteArrayP   =                       POINTER TO ByteArray;
  IntegerP     =                       POINTER TO ARRAY 1 OF INTEGER;
  LongIntP     =                       POINTER TO ARRAY 1 OF LONGINT;
  PImportHeader  =                     POINTER TO TImportHeader;
  TImportHeader  =  RECORD
    Sig1,
    Sig2:                              WinDef.WORD;
    Version:                           WinDef.WORD;
    Machine:                           WinDef.WORD;
    TimeDateStamp:                     WinDef.DWORD;
    SizeOfData:                        WinDef.DWORD;
    Ordinal_Hint:                      WinDef.WORD;
    Type:                              WinDef.WORD;
  END (* TImportHeader *);

VAR    
  hInst:                               WinDef.HANDLE;
  Result:                              WinDef.LRESULT;
  ReturnCode:                          LONGINT;
  ResultBool:                          WinDef.BOOL;
  ByteAddress:                         LONGINT;            (* Pointer to the actual Byte *)
  HexAddress,
  AsciiData:                           ARRAY  20 OF CHAR;
  HexData:                             ARRAY  90 OF CHAR;
  PSectionHeader:                      Common.PSectionHeader;


(*****************************************************************************)
PROCEDURE BigEndian2LongInt           (ByteArrayPtr:       LONGINT)
                                      :LONGINT;

VAR
  PByteArray:                          ByteArrayP;
  i:                                   SHORTINT;
  j:                                   INTEGER;
  MyLongInt:                           LONGINT;

BEGIN
  PByteArray := SYSTEM.VAL(ByteArrayP, ByteArrayPtr);
  MyLongInt  := SYSTEM.VAL(SHORTINT, PByteArray[0]);
  IF MyLongInt<0 THEN
    MyLongInt := MyLongInt + 0100H;
  END;

  FOR i:=1 TO 3 DO
    j          := SYSTEM.VAL(SHORTINT, PByteArray[i]);
    IF j<0 THEN
      j := j + 0100H;
    END;
    MyLongInt := 100H*MyLongInt + j;
  END;
  RETURN MyLongInt
END BigEndian2LongInt;


(*****************************************************************************)
(*                                                                           *)
(* NextLine                                                                  *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   ActLine           Pointer to the view's line structure                  *)
(*   NumberOfCol                                                             *)
(*   NumberOfLines     number of lines                                       *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE NextLine*                   (VAR ActLine:        Common.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT;
                                       VAR NumberOfLines:  LONGINT);

VAR
  MyLine:                              Common.ScreenLineP;
  MyFormat:                            INTEGER;

BEGIN
  IF Strings.Length(ActLine^.Text)>NumberOfCol THEN
    NumberOfCol := Strings.Length(ActLine^.Text);
  END (* IF ... *);
  IF ActLine^.Next=NIL THEN
    NEW(MyLine);
    MyLine^.Previous   := ActLine;
    MyLine^.Next       := NIL;
    ActLine^.Next      := MyLine;
  ELSE
    MyLine             := ActLine^.Next;
  END;
  (* set fields of new ScreenLine element *)
  MyLine^.Text[0]  := 0X;
  MyLine^.Type     := ActLine^.Type;
  MyLine^.Format   := ActLine^.Format;
  MyLine^.Usage    := ActLine^.Usage;
  ActLine          := MyLine;
  INC(NumberOfLines);
END NextLine;

  
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
  ActSectionHeader:                    Common.PSectionHeader;
  VAImageDir:                          LONGINT;
  i:                                   INTEGER;
  
BEGIN
  PSectionHeader   := NIL;
  IF NoOfImageDirectory>=Common.PMyNT_Header^.OptionalHeader.NumberOfRvaAndSizes THEN
    RETURN 0
  END (* IF *);
  
  ActSectionHeader := Common.FirstSectionHeader;
  VAImageDir       := Common.PMyNT_Header.OptionalHeader.DataDirectory[NoOfImageDirectory].VirtualAddress;
  IF VAImageDir=0 THEN
    RETURN 0                                               (* no such directory ! *)
  END;
  FOR i:=0 TO Common.PMyNT_Header^.FileHeader.NumberOfSections-1 DO
    IF ActSectionHeader^.PSectionHeader=NIL THEN
      UIStatusLine.ShowMessage1("PSectionHeader=NIL: #", i);
    ELSIF ((ActSectionHeader^.PSectionHeader^.VirtualAddress<=VAImageDir) & 
       ((ActSectionHeader^.PSectionHeader^.VirtualAddress+ActSectionHeader^.PSectionHeader^.SizeOfRawData)>VAImageDir)) THEN
      ASSERT(Common.MyFileDescription.lpFileBase#0);
      ASSERT(ActSectionHeader^.PSectionHeader^.PointerToRawData#0);
      VAImageDir       := Common.MyFileDescription.lpFileBase + ActSectionHeader^.PSectionHeader^.PointerToRawData;
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
  ActSectionHeader:                    Common.PSectionHeader;
  VAImageDir:                          LONGINT;
  i:                                   INTEGER;
  
BEGIN
  IF NoOfImageDirectory>=Common.PMyNT_Header^.OptionalHeader.NumberOfRvaAndSizes THEN
    RETURN NIL
  END (* IF *);
  
  ActSectionHeader := Common.FirstSectionHeader;
  VAImageDir       := Common.PMyNT_Header.OptionalHeader.DataDirectory[NoOfImageDirectory].VirtualAddress;
  IF VAImageDir=0 THEN
    RETURN NIL                                             (* no such directory ! *)
  END;
  FOR i:=0 TO Common.PMyNT_Header^.FileHeader.NumberOfSections-1 DO
    IF ((ActSectionHeader^.PSectionHeader^.VirtualAddress<=VAImageDir) & 
       ((ActSectionHeader^.PSectionHeader^.VirtualAddress+ActSectionHeader^.PSectionHeader^.SizeOfRawData)>VAImageDir)) THEN
      VAImageDir := Common.MyFileDescription.lpFileBase + ActSectionHeader^.PSectionHeader^.PointerToRawData;
      RETURN ActSectionHeader^.PSectionHeader
    END;
    ActSectionHeader := ActSectionHeader^.Next;
  END (* FOR i:=... *);
  
  RETURN NIL
END GetSectionHeader;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines                                                                *)
(* Generates the lines to be displayed when the program switches to          *)
(* DebugData mode                                                            *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   FirstLineP        Pointer to the view's first line structure            *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LONGINT           number of lines generated                             *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:           genügt der procedure definition des moduls view       *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLines*                 (FirstLineP:         Common.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActAddress:                          SYSTEM.PTR;
  ActLine,
  MyLine:                              Common.ScreenLineP;
  BytesRead, k:                        LONGINT;
  i, j, IntByte:                       INTEGER;
  MyByte:                              SYSTEM.BYTE;
  MyBytes:                             ARRAY 17 OF SYSTEM.BYTE;
  MyChars:                             ARRAY 32 OF CHAR;
  MyLongInt,
  MyPointer:                           LONGINT;
  NumberOfBytes,
  NumberOfLines:                       LONGINT;
  
BEGIN;

  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Common.Text01;
  ActLine^.Usage   :=  0;
  NumberOfLines    :=  0;

  IF Common.PMyDOS_Header#NIL THEN                         (* *.exe or *.dll file *)
    COPY (Line050, ActLine^.Text);
    ActLine^.Format := Common.Header1;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line052, ActLine^.Text);
    Strings.Append(ActLine^.Text, SYSTEM.VAL(String4T, Common.PMyDOS_Header^.e_magic));
    ActLine^.Text[Strings.Length(ActLine^.Text)-1] := 0X;
    ActLine^.Format := Common.Text01;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line053, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_cblp, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line054, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_cp, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line055, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_crlc, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line056, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_cparhdr, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line057, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_minalloc, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line058, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_maxalloc, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line059, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_ss, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line060, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_sp, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line061, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_csum, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line062, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_ip, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line063, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_cs, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line064, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_lfarlc, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line065, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_ovno, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line066, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_res[0], 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line067, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_oemid, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line068, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_oeminfo, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line069, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_res2[0], 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line070, ActLine^.Text);
    Strings.UHexStr(Common.PMyDOS_Header^.e_lfanew, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (LineEmpty, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    
  END (* IF Common.PMyDOS_Header#NIL *);
      
  COPY (Line080, ActLine^.Text);
  Strings.Append(ActLine^.Text, SYSTEM.VAL(String4T, Common.PMyNT_Header^.Signature));
  ActLine^.Format  := Common.Header1;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line081, ActLine^.Text);
  ActLine^.Format := Common.Header2;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line082, ActLine^.Text);
  ActLine^.Format  := Common.Text01;
  Strings.UHexStr(Common.PMyNT_Header^.FileHeader.Machine, 2, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  CASE Common.PMyNT_Header^.FileHeader.Machine OF
    WinNT.IMAGE_FILE_MACHINE_UNKNOWN:
      Strings.Append(ActLine^.Text, "   (machine unknown)");
    |
    WinNT.IMAGE_FILE_MACHINE_I386:
      Strings.Append(ActLine^.Text, "   (INTEL 386, 486, Pentium, ...)");
    |
    WinNT.IMAGE_FILE_MACHINE_R3000:
      Strings.Append(ActLine^.Text, "   (IBM R3000)");
    |
    WinNT.IMAGE_FILE_MACHINE_R4000:
      Strings.Append(ActLine^.Text, "   (IBM R4000)");
    |
    WinNT.IMAGE_FILE_MACHINE_R10000:
      Strings.Append(ActLine^.Text, "   (IBM R10.000)");
    |
    WinNT.IMAGE_FILE_MACHINE_ALPHA:
      Strings.Append(ActLine^.Text, "   (DEC Alpha)");
    |
    WinNT.IMAGE_FILE_MACHINE_POWERPC:
      Strings.Append(ActLine^.Text, "   (IBM Power PC)");
    ELSE
      Strings.Append(ActLine^.Text, "   (machine unknown)");
  END;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line083, ActLine^.Text);
  Strings.Str(Common.PMyNT_Header^.FileHeader.NumberOfSections, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line084, ActLine^.Text);
  Strings.UHexStr(Common.PMyNT_Header^.FileHeader.TimeDateStamp, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line085, ActLine^.Text);
  Strings.UHexStr(Common.PMyNT_Header^.FileHeader.PointerToSymbolTable, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line086, ActLine^.Text);
  Strings.Str(Common.PMyNT_Header^.FileHeader.NumberOfSymbols, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line087, ActLine^.Text);
  Strings.UHexStr(Common.PMyNT_Header^.FileHeader.SizeOfOptionalHeader, 2, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line088, ActLine^.Text);
  Strings.UHexStr(Common.PMyNT_Header^.FileHeader.Characteristics, 2, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  0) THEN
    COPY (Line200, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  1) THEN
    COPY (Line201, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  2) THEN
    COPY (Line202, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  3) THEN
    COPY (Line203, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  4) THEN
    COPY (Line204, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  6) THEN
    COPY (Line206, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  7) THEN
    COPY (Line207, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  8) THEN
    COPY (Line208, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics),  9) THEN
    COPY (Line209, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics), 10) THEN
    COPY (Line210, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics), 12) THEN
    COPY (Line212, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics), 13) THEN
    COPY (Line213, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics), 14) THEN
    COPY (Line214, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
  IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.FileHeader.Characteristics), 15) THEN
    COPY (Line215, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;
      
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);

  IF Common.PMyNT_Header^.FileHeader.SizeOfOptionalHeader>0 THEN
    COPY (LineEmpty, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line100, ActLine^.Text);
    ActLine^.Format := Common.Header2;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line110, ActLine^.Text);
    ActLine^.Format := Common.Header3;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    ActLine^.Format := Common.Text01;
    COPY (Line111, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.Magic, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line112, ActLine^.Text);
    Strings.Str(SYSTEM.VAL(INTEGER, Common.PMyNT_Header^.OptionalHeader.MajorLinkerVersion), MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(SYSTEM.VAL(INTEGER, Common.PMyNT_Header^.OptionalHeader.MinorLinkerVersion), MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line114, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfCode, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line115, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfInitializedData, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line116, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfUninitializedData, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line117, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.AddressOfEntryPoint, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line118, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.BaseOfCode, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (LineEmpty, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line120, ActLine^.Text);
    ActLine^.Format := Common.Header2;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line121, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.ImageBase, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format := Common.Header3;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line122, ActLine^.Text);
    ActLine^.Format := Common.Text01;
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SectionAlignment, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line123, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.FileAlignment, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line124, ActLine^.Text);
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MajorOperatingSystemVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MinorOperatingSystemVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line126, ActLine^.Text);
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MajorImageVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MinorImageVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line128, ActLine^.Text);
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MajorSubsystemVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(Common.PMyNT_Header^.OptionalHeader.MinorSubsystemVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line130, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.Win32VersionValue, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line131, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfImage, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line132, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfHeaders, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line133, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.CheckSum, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line134, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.Subsystem, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    CASE Common.PMyNT_Header^.OptionalHeader.Subsystem OF
      WinNT.IMAGE_SUBSYSTEM_UNKNOWN:
        Strings.Append(ActLine^.Text, "   (Unknown subsystem)");
      |
      WinNT.IMAGE_SUBSYSTEM_NATIVE:
        Strings.Append(ActLine^.Text, "   (Used for device drivers and native NT Processes)");
      |
      WinNT.IMAGE_SUBSYSTEM_WINDOWS_GUI:
        Strings.Append(ActLine^.Text, "   (Subsystem Windows graphical user interface)");
      |
      WinNT.IMAGE_SUBSYSTEM_WINDOWS_CUI:
        Strings.Append(ActLine^.Text, "   (subsystem Windows character)");
      |
      WinNT.IMAGE_SUBSYSTEM_OS2_CUI:
        Strings.Append(ActLine^.Text, "   (subsystem OS/2 character)");
      |
      WinNT.IMAGE_SUBSYSTEM_POSIX_CUI:
        Strings.Append(ActLine^.Text, "   (subsystem POSIX)");
      ELSE
        Strings.Append(ActLine^.Text, "   (Unknown subsystem)");
    END;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line135, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics, 2, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);

    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics),  0) THEN
      COPY (Line300, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics),  1) THEN
      COPY (Line300, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics),  2) THEN
      COPY (Line300, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics),  3) THEN
      COPY (Line300, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics), 11) THEN
      COPY (Line311, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics), 13) THEN
      COPY (Line313, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(LONG(Common.PMyNT_Header^.OptionalHeader.DllCharacteristics), 15) THEN
      COPY (Line315, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
  
    COPY (Line136, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfStackReserve, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line137, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfStackCommit, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line138, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfHeapReserve, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line139, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.SizeOfHeapCommit, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line140, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.LoaderFlags, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line141, ActLine^.Text);
    Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.NumberOfRvaAndSizes, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (LineEmpty, ActLine^.Text);
    ActLine^.Format := Common.Header2;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line150, ActLine^.Text);
    ActLine^.Format := Common.Header1;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    COPY (Line151, ActLine^.Text);
    ActLine^.Format := Common.Header3;
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    ActLine^.Format := Common.Text01;
    FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 DO
      COPY (LineEmpty, ActLine^.Text);
      Strings.Str(i, MyChars);
      Strings.Append(ActLine^.Text, MyChars);
      IF i<10 THEN
        Strings.InsertChar("0", ActLine^.Text, Strings.Length(ActLine^.Text));
      END;
      CASE i OF
        0:
          Strings.Append(ActLine^.Text, "  Export Table                    ");
        |
        1:
          Strings.Append(ActLine^.Text, "  Import Table                    ");
        |
        2:
          Strings.Append(ActLine^.Text, "  Resource Table                  ");
        |
        3:
          Strings.Append(ActLine^.Text, "  Exception Table                 ");
        |
        4:
          Strings.Append(ActLine^.Text, "  Certificate Table               ");
        |
        5:
          Strings.Append(ActLine^.Text, "  Base Relocation Table           ");
        |
        6:
          Strings.Append(ActLine^.Text, "  Debug Data                      ");
        |
        7:
          Strings.Append(ActLine^.Text, "  Architecture Data               ");
        |
        8:
          Strings.Append(ActLine^.Text, "  Global Ptr                      ");
        |
        9:
          Strings.Append(ActLine^.Text, "  Thread Load Storage Table (TLS) ");
        |
        10:
          Strings.Append(ActLine^.Text, "  Load Config Table               ");
        |
        11:
          Strings.Append(ActLine^.Text, "  Bound Import Table              ");
        |
        12:
          Strings.Append(ActLine^.Text, "  Import Address Table (IAT)      ");
        |
        13:
          Strings.Append(ActLine^.Text, "  Delay Import Descriptor         ");
        |
        14:
          Strings.Append(ActLine^.Text, "  COM+ Runtime Header             ");
        |
        15:
          Strings.Append(ActLine^.Text, "  Reserved                        ");
        ELSE
        ;
      END;
      IF Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress=0 THEN
        Strings.Append(ActLine^.Text, " Not Used.");
      ELSE
        Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress, 4, MyChars);
        Strings.RightAlign(MyChars, 10);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.UHexStr(Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].Size, 4, MyChars);
        Strings.RightAlign(MyChars, 20);
        Strings.Append(ActLine^.Text, MyChars);
      END;
      NextLine(ActLine, NumberOfCol, NumberOfLines);
    END (* FOR i:=0 To ... *);
  END (* IF Common.PMyNT_Header^.FileHeader.SizeOfOptionalHeader>0 *);
        
  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format := Common.Header2;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line160, ActLine^.Text);
  Strings.Str(Common.PMyNT_Header^.FileHeader.NumberOfSections, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Strings.AppendChar(ActLine^.Text, ")");
  ActLine^.Format  := Common.Header1;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line161, ActLine^.Text);
  ActLine^.Format  := Common.Header3;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  ActLine^.Format  := Common.Text01;
  PSectionHeader   := Common.FirstSectionHeader;
  FOR i:=1 TO Common.PMyNT_Header^.FileHeader.NumberOfSections DO
    
    COPY (LineEmpty, ActLine^.Text);
    Strings.Str(i, MyChars);
    Strings.RightAlign(MyChars, 2);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append(ActLine^.Text, "  ");
    COPY(PSectionHeader^.PSectionHeader^.Name, MyChars);
    k := Strings.Length(MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    FOR j:=SHORT(k) TO 8 DO
      Strings.AppendChar(ActLine^.Text, " ");
    END;

    Strings.UHexStr(PSectionHeader^.PSectionHeader^.VirtualAddress, 4, MyChars);
    Strings.RightAlign(MyChars, 15);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.SizeOfRawData, 4, MyChars);
    Strings.RightAlign(MyChars, 20);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.Characteristics, 4, MyChars);
    Strings.RightAlign(MyChars, 20);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    IF PSectionHeader^.Next#NIL THEN
      PSectionHeader := PSectionHeader^.Next;
    END;
    
  END (* FOR i:=1 TO ... *);
      
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines
  
END WriteLines;


(*****************************************************************************)
(*                                                                           *)
(* WriteLinesArchiveLIB                                                      *)
(* Generates the lines to be displayed when the program switches to          *)
(* DebugData mode and the file is of type Archive Library                    *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   FirstLineP        Pointer to the view's first line structure            *)
(*   NumberOfColumns   max number of characters in any line                  *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*   LONGINT           number of lines generated                             *)
(*                                                                           *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:           genügt der procedure definition des moduls view       *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE WriteLinesArchiveLIB*       (FirstLineP:         Common.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActAddress:                          SYSTEM.PTR;
  ActSectionHeader,
  TempSectionHeader:                   Common.PSectionHeader;
  ActLine,
  MyLine:                              Common.ScreenLineP;
  BytesRead, k:                        LONGINT;
  i, j, IntByte:                       INTEGER;
  MyChars:                             ARRAY 64 OF CHAR;
  MyName:                              String16P;
  MyString:                            StringT;
  PByteArray:                          ByteArrayP;
  PInteger,
  PInteger2:                           IntegerP;
  PLongInt:                            LongIntP;
  PStringArray:                        StringArrayP;
  PMyImageArchiveHeader:               WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER;
  PMyFileHeader:                       WinNT.PIMAGE_FILE_HEADER;
  PMyImportHeader:                     PImportHeader;
  MyLongInt,
  MyPointer,
  MyPointer2:                          LONGINT;
  NumberOfBytes,
  NumberOfMembers,
  NumberOfSymbols,
  NumberOfLines:                       LONGINT;
  

  PROCEDURE DisplayArchiveMemberHeader  (PMyImageArchiveHeader:               
                                                           WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER);
  
  BEGIN
    COPY (Line511, ActLine^.Text);
    ActLine^.Format  := Common.Header2;
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.Name);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
    COPY (Line512, ActLine^.Text);
    ActLine^.Format  := Common.Text01;
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.Date);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
    COPY (Line513, ActLine^.Text);
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.UserID);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
    COPY (Line514, ActLine^.Text);
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.GroupID);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
    COPY (Line515, ActLine^.Text);
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.Mode);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
    COPY (Line516, ActLine^.Text);
    Strings.Append(ActLine^.Text, PMyImageArchiveHeader^.Size);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
          
  END DisplayArchiveMemberHeader;
  
BEGIN;

  ActLine          := FirstLineP;
  ActLine^.Type    :=  0;
  ActLine^.Format  := Common.Text01;
  ActLine^.Usage   :=  0;
  NumberOfLines    :=  0;
  MyPointer        := Common.MyFileDescription.lpFileBase;

  COPY (Line500, ActLine^.Text);
  ActLine^.Format  := Common.Header1;
  NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line501, ActLine^.Text);
  ActLine^.Format  := Common.Text01;
  Strings.Append(ActLine^.Text, Common.PArchiveFileSignature^);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  INC(MyPointer, 8);
    
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line509, ActLine^.Text);                            (* First Linker Member *)
  NextLine(ActLine, NumberOfCol, NumberOfLines);
        
  DisplayArchiveMemberHeader(Common.PMyImageArchiveHeader1);

  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  INC(MyPointer, SIZE(WinNT.IMAGE_ARCHIVE_MEMBER_HEADER));

  COPY (Line522, ActLine^.Text);
  NumberOfSymbols  := BigEndian2LongInt(MyPointer);
  Strings.Str(NumberOfSymbols, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  INC(MyPointer, 4);
  MyPointer2       := MyPointer + 4*NumberOfSymbols;
  
  FOR i:=1 TO NumberOfSymbols DO;
    COPY (Line523, ActLine^.Text);
    k      := BigEndian2LongInt(MyPointer);
    Strings.UHexStr(k, 4, MyChars);
    Strings.RightAlign(MyChars, 15);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append(ActLine^.Text, ";   Symbol:  ");
    PStringArray := SYSTEM.VAL(StringArrayP, MyPointer2);
    Strings.Append(ActLine^.Text, PStringArray^);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    INC(MyPointer, 4);
    INC(MyPointer2, (Strings.Length(PStringArray^)+1));
  END;
        
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);

  MyPointer                          := MyPointer2;
  Common.PMyImageArchiveHeader2      := SYSTEM.VAL(WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER, MyPointer);

  COPY (Line510, ActLine^.Text);                            (* Second Linker Member *)
  NextLine(ActLine, NumberOfCol, NumberOfLines);
        
  DisplayArchiveMemberHeader(Common.PMyImageArchiveHeader2);

  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  INC(MyPointer, SIZE(WinNT.IMAGE_ARCHIVE_MEMBER_HEADER));

  COPY (Line521, ActLine^.Text);
  PLongInt         := SYSTEM.VAL(LongIntP, MyPointer);
  NumberOfMembers  := PLongInt^[0];
  Strings.Str(NumberOfMembers, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);

  INC(MyPointer, 4);
  FOR i:=1 TO NumberOfMembers DO;
    COPY (Line523, ActLine^.Text);
    PLongInt               := SYSTEM.VAL(LongIntP, MyPointer);
    Strings.UHexStr(PLongInt^[0], 4, MyChars);
    Strings.RightAlign(MyChars, 15);
    Strings.Append(ActLine^.Text, MyChars);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    MyPointer2             := Common.MyFileDescription.lpFileBase + PLongInt^[0];
    PMyImageArchiveHeader  := SYSTEM.VAL(WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER, MyPointer2);
    DisplayArchiveMemberHeader(PMyImageArchiveHeader);
    INC(MyPointer2, SIZE(WinNT.IMAGE_ARCHIVE_MEMBER_HEADER));
    PMyFileHeader          := SYSTEM.VAL(WinNT.PIMAGE_FILE_HEADER, MyPointer2);
    (* read Section Headers 
    Common.FirstSectionHeader^.PSectionHeader   := NIL;
    ActSectionHeader     := Common.FirstSectionHeader;
    FOR i:=1 TO PMyFileHeader^.NumberOfSections DO
      ActSectionHeader^.PSectionHeader := SYSTEM.VAL(WinNT.PIMAGE_SECTION_HEADER, MyPointer);
      IF ActSectionHeader^.Next=NIL THEN
        NEW(TempSectionHeader);
        ActSectionHeader^.Next := TempSectionHeader;
      END (* IF ActSectionHeader^.Next=NIL *);
      ActSectionHeader       := ActSectionHeader^.Next;
      INC(MyPointer, SIZE(WinNT.IMAGE_SECTION_HEADER));
    END  FOR i:=0 TO ... *);
    IF PMyFileHeader^.Machine=0 THEN                       (* Write Import Header *)
      PMyImportHeader        := SYSTEM.VAL(PImportHeader, MyPointer2);
      COPY (Line581, ActLine^.Text);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
        
      COPY (Line582, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.Sig1, 2, MyChars);
      Strings.RightAlign(MyChars, 2);
      Strings.Append(ActLine^.Text, MyChars);
      Strings.UHexStr(PMyImportHeader^.Sig2, 2, MyChars);
      Strings.RightAlign(MyChars, 6);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);

      COPY (Line583, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.Version, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);

      COPY (Line584, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.Machine, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      CASE PMyImportHeader^.Machine OF
        WinNT.IMAGE_FILE_MACHINE_UNKNOWN:
          Strings.Append(ActLine^.Text, "   (machine unknown)");
        |
        WinNT.IMAGE_FILE_MACHINE_I386:
          Strings.Append(ActLine^.Text, "   (INTEL 386, 486, Pentium, ...)");
        |
        WinNT.IMAGE_FILE_MACHINE_R3000:
          Strings.Append(ActLine^.Text, "   (IBM R3000)");
        |
        WinNT.IMAGE_FILE_MACHINE_R4000:
          Strings.Append(ActLine^.Text, "   (IBM R4000)");
        |
        WinNT.IMAGE_FILE_MACHINE_R10000:
          Strings.Append(ActLine^.Text, "   (IBM R10.000)");
        |
        WinNT.IMAGE_FILE_MACHINE_ALPHA:
          Strings.Append(ActLine^.Text, "   (DEC Alpha)");
        |
        WinNT.IMAGE_FILE_MACHINE_POWERPC:
          Strings.Append(ActLine^.Text, "   (IBM Power PC)");
        ELSE
          Strings.Append(ActLine^.Text, "   (machine unknown)");
      END;
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line585, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.TimeDateStamp, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line586, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.SizeOfData, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line587, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.Ordinal_Hint, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line588, ActLine^.Text);
      Strings.UHexStr(PMyImportHeader^.Type, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
      
    ELSE                                                   (* Write COFF File Header *)
      COPY (Line081, ActLine^.Text);
      ActLine^.Format  := Common.Header2;
      Strings.Insert("  ", ActLine^.Text, 1);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
        
      COPY (Line082, ActLine^.Text);
      ActLine^.Format  := Common.Text01;
      Strings.UHexStr(PMyFileHeader^.Machine, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      CASE PMyFileHeader^.Machine OF
        WinNT.IMAGE_FILE_MACHINE_UNKNOWN:
          Strings.Append(ActLine^.Text, "   (machine unknown)");
        |
        WinNT.IMAGE_FILE_MACHINE_I386:
          Strings.Append(ActLine^.Text, "   (INTEL 386, 486, Pentium, ...)");
        |
        WinNT.IMAGE_FILE_MACHINE_R3000:
          Strings.Append(ActLine^.Text, "   (IBM R3000)");
        |
        WinNT.IMAGE_FILE_MACHINE_R4000:
          Strings.Append(ActLine^.Text, "   (IBM R4000)");
        |
        WinNT.IMAGE_FILE_MACHINE_R10000:
          Strings.Append(ActLine^.Text, "   (IBM R10.000)");
        |
        WinNT.IMAGE_FILE_MACHINE_ALPHA:
          Strings.Append(ActLine^.Text, "   (DEC Alpha)");
        |
        WinNT.IMAGE_FILE_MACHINE_POWERPC:
          Strings.Append(ActLine^.Text, "   (IBM Power PC)");
        ELSE
          Strings.Append(ActLine^.Text, "   (machine unknown)");
      END;
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line083, ActLine^.Text);
      Strings.Str(PMyFileHeader^.NumberOfSections, MyChars);
      Strings.RightAlign(MyChars, 9);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line084, ActLine^.Text);
      Strings.UHexStr(PMyFileHeader^.TimeDateStamp, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line085, ActLine^.Text);
      Strings.UHexStr(PMyFileHeader^.PointerToSymbolTable, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line086, ActLine^.Text);
      Strings.Str(PMyFileHeader^.NumberOfSymbols, MyChars);
      Strings.RightAlign(MyChars, 9);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line087, ActLine^.Text);
      Strings.UHexStr(PMyFileHeader^.SizeOfOptionalHeader, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
          
      COPY (Line088, ActLine^.Text);
      Strings.UHexStr(PMyFileHeader^.Characteristics, 2, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      NextLine(ActLine, NumberOfCol, NumberOfLines);
      
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  0) THEN
        COPY (Line200, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  1) THEN
        COPY (Line201, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  2) THEN
        COPY (Line202, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  3) THEN
        COPY (Line203, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  4) THEN
        COPY (Line204, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  6) THEN
        COPY (Line206, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  7) THEN
        COPY (Line207, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  8) THEN
        COPY (Line208, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics),  9) THEN
        COPY (Line209, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics), 10) THEN
        COPY (Line210, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics), 12) THEN
        COPY (Line212, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics), 13) THEN
        COPY (Line213, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics), 14) THEN
        COPY (Line214, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
      IF SYSTEM.BIT(LONG(PMyFileHeader^.Characteristics), 15) THEN
        COPY (Line215, ActLine^.Text);
        NextLine(ActLine, NumberOfCol, NumberOfLines);
      END;
    END (* IF PMyFileHeader.Machine=0 *);
    COPY (LineEmpty, ActLine^.Text);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    INC(MyPointer, 4);
  END;
        
  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);

  COPY (Line522, ActLine^.Text);
  ActLine^.Format  := Common.Header2;
  PLongInt         := SYSTEM.VAL(LongIntP, MyPointer);
  NumberOfSymbols  := PLongInt^[0];
  Strings.Str(NumberOfSymbols, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  INC(MyPointer, 4);
  MyPointer2       := MyPointer + 2*NumberOfSymbols;

  FOR i:=1 TO NumberOfSymbols DO;
    COPY (Line523, ActLine^.Text);
    ActLine^.Format  := Common.Text01;
    PInteger       := SYSTEM.VAL(IntegerP, MyPointer);
    Strings.UHexStr(PInteger^[0], 4, MyChars);
    Strings.RightAlign(MyChars, 15);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append(ActLine^.Text, ";   Symbol:  ");
    PStringArray := SYSTEM.VAL(StringArrayP, MyPointer2);
    Strings.Append(ActLine^.Text, PStringArray^);
    NextLine(ActLine, NumberOfCol, NumberOfLines);
    INC(MyPointer, 2);
    INC(MyPointer2, (Strings.Length(PStringArray^)+1));
  END;

  COPY (LineEmpty, ActLine^.Text);
  NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  RETURN NumberOfLines
  
END WriteLinesArchiveLIB;


(*****************************************************************************)
(*                                                                           *)
(* DumpTheFile                                                               *)
(* Reads the PECOFF structure of the file                                    *)
(* the file's data are stored in Common.mod MyFileDescription                *)
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
PROCEDURE DumpTheFile*                ()
                                      :LONGINT;

VAR
  BytesRead:                           LONGINT;
  Done:                                BOOLEAN;
  i, j:                                LONGINT;
  MyName:                              ARRAY 10 OF CHAR;
  
  AhWnd:                               WinDef.HWND;

  ActSectionHeader,
  TempSectionHeader:                   Common.PSectionHeader;
  ActDebugDirectory,
  LastDebugDirectory,
  TempDebugDirectory:                  Common.PDebugDirectory;
  MyPointer,
  COFFFileHeaderPos:                   LONGINT;
  MDICreateStruct:                     WinUser.MDICREATESTRUCTA;
  
BEGIN
  
  (* Read Bytes until eof reached, this will be the basis for displaying hexdata *)
  Result := WinBase.ReadFile (Common.MyFileDescription.hFile, SYSTEM.ADR(Common.FileData), Common.BlockLength, Common.FileDataLength, 0);
  Common.ReadFirstBlock (Common.FileData, Common.FileDataLength);
  
  REPEAT
    Result := WinBase.ReadFile (Common.MyFileDescription.hFile, SYSTEM.ADR(Common.FileData), Common.BlockLength, Common.FileDataLength, 0);
    Common.ReadNextBlock (Common.FileData, Common.FileDataLength)
  UNTIL Common.FileDataLength<Common.BlockLength;

  (* Read File's DOS Header *)
  MyPointer            := Common.MyFileDescription.lpFileBase;
  Common.PMyDOS_Header := SYSTEM.VAL(WinNT.PIMAGE_DOS_HEADER, MyPointer);

  IF Common.PMyDOS_Header.e_magic=WinNT.IMAGE_DOS_SIGNATURE THEN
    MyPointer            := Common.MyFileDescription.lpFileBase + Common.PMyDOS_Header^.e_lfanew;
    (* Read File's PE Structure *)
    Common.PMyNT_Header  := SYSTEM.VAL(WinNT.PIMAGE_NT_HEADERS, MyPointer);
    IF Common.PMyNT_Header.Signature=WinNT.IMAGE_NT_SIGNATURE THEN
      Common.MyFileDescription.FileType := Common.FileTypeEXE;
    ELSE
      Common.MyFileDescription.FileType := Common.FileTypeUnknown;
      RETURN 0;
    END;
    INC(MyPointer, (SIZE(WinDef.DWORD)+SIZE(WinNT.IMAGE_FILE_HEADER)+LONG(Common.PMyNT_Header.FileHeader.SizeOfOptionalHeader)));
  ELSIF Common.PMyDOS_Header.e_magic=3C21H THEN
    MyPointer                          := Common.MyFileDescription.lpFileBase;
    Common.PArchiveFileSignature       := SYSTEM.VAL(Common.ArchiveFileSignatureP, MyPointer);
    INC(MyPointer, 8);
    Common.PMyImageArchiveHeader1      := SYSTEM.VAL(WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER, MyPointer);
    INC(MyPointer, SIZE(WinNT.IMAGE_ARCHIVE_MEMBER_HEADER));
    Common.PMyImageArchiveHeader2      := SYSTEM.VAL(WinNT.PIMAGE_ARCHIVE_MEMBER_HEADER, MyPointer);
    Common.MyFileDescription.FileType  := Common.FileTypeLIB;
    Common.PMyDOS_Header               := NIL;
    NEW(Common.PMyNT_Header);
    Common.PMyNT_Header                := NIL;
    Common.MyFileDescription.Loaded    := TRUE;
    RETURN 0;
  ELSE
    Common.PMyDOS_Header               := NIL;
    NEW(Common.PMyNT_Header);
    SYSTEM.MOVE(MyPointer, SYSTEM.ADR(Common.PMyNT_Header.FileHeader), SIZE(WinNT.IMAGE_FILE_HEADER));
    Common.MyFileDescription.FileType  := Common.FileTypeOBJ;
    FOR i:=0 TO WinNT.IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 DO
      Common.PMyNT_Header^.OptionalHeader.DataDirectory[i].VirtualAddress := 0
    END;
    INC(MyPointer, (SIZE(WinNT.IMAGE_FILE_HEADER)+LONG(Common.PMyNT_Header.FileHeader.SizeOfOptionalHeader)));
  END;
  
  (* read Section Headers *)
  ActSectionHeader     := Common.FirstSectionHeader;
  FOR i:=1 TO Common.PMyNT_Header.FileHeader.NumberOfSections DO
    ActSectionHeader^.PSectionHeader := SYSTEM.VAL(WinNT.PIMAGE_SECTION_HEADER, MyPointer);
    IF ActSectionHeader^.Next=NIL THEN
      NEW(TempSectionHeader);
      ActSectionHeader^.Next := TempSectionHeader;
    END (* IF ActSectionHeader^.Next=NIL *);
    ActSectionHeader       := ActSectionHeader^.Next;
    INC(MyPointer, SIZE(WinNT.IMAGE_SECTION_HEADER));
  END (* FOR i:=1 TO Common.PMyNT_Header.FileHeader.NumberOfSections *);  

  (* read Debug Directory *)
  ActDebugDirectory    := Common.FirstDebugDirectory;
  MyPointer            := ImageDirectoryOffset(6, ActSectionHeader^.PSectionHeader);
  IF MyPointer#0 THEN                                      (* COFF *)
    i                    := Common.PMyNT_Header^.OptionalHeader.DataDirectory[6].Size;
    WHILE i>=SIZE(WinNT.IMAGE_DEBUG_DIRECTORY) DO
      IF ActDebugDirectory^.PDebugDirectory=NIL THEN
        NEW(ActDebugDirectory^.PDebugDirectory);
      END (* IF ActDebugDirectory^.PDebugDirectory=NIL *);
      ActDebugDirectory^.PDebugDirectory := SYSTEM.VAL(WinNT.PIMAGE_DEBUG_DIRECTORY, MyPointer);
      INC(MyPointer, SIZE(WinNT.IMAGE_DEBUG_DIRECTORY));
      DEC(i, SIZE(WinNT.IMAGE_DEBUG_DIRECTORY));
      IF ((ActDebugDirectory^.Next=NIL) & (i>0)) THEN
        NEW(TempDebugDirectory);
        ActDebugDirectory^.Next  := TempDebugDirectory;
        ActDebugDirectory        := TempDebugDirectory;
        ActDebugDirectory^.PDebugDirectory := NIL;
      ELSE
        ActDebugDirectory        := ActDebugDirectory^.Next;
      END (* IF ActDebugDirectory^.Next=NIL *);
    END (* WHILE i>=SIZE(WinNT.IMAGE_DEBUG_DIRECTORY) *);
  ELSE                                                     (* may be an object file containing codeview data *)
    ActSectionHeader           := Common.FirstSectionHeader;
    FOR i:=1 TO Common.PMyNT_Header.FileHeader.NumberOfSections DO
      IF Strings.Pos("debug", ActSectionHeader^.PSectionHeader^.Name, 1)>0 THEN
        MyPointer                          := Common.MyFileDescription.lpFileBase;
        MyPointer                          := MyPointer + ActSectionHeader^.PSectionHeader^.PointerToRawData;
        ActDebugDirectory^.PDebugDirectory := SYSTEM.VAL(WinNT.PIMAGE_DEBUG_DIRECTORY, MyPointer);
        LastDebugDirectory                 := ActDebugDirectory;
        IF ActDebugDirectory^.Next=NIL THEN
          NEW(TempDebugDirectory);
          ActDebugDirectory^.Next  := TempDebugDirectory;
          ActDebugDirectory        := TempDebugDirectory;
        ELSE
          ActDebugDirectory  := ActDebugDirectory^.Next;
        END;
        ActDebugDirectory^.PDebugDirectory := NIL;
      END;
      ActSectionHeader       := ActSectionHeader^.Next;
    END (* FOR i:=1 TO Common.PMyNT_Header.FileHeader.NumberOfSections *);
    LastDebugDirectory^.Next := NIL;
  END (* IF MyPointer#0 *);
  
  Common.MyFileDescription.Loaded := TRUE;
  
  (* Generate the MDI Dump Window *)
  MDICreateStruct.szClass      := SYSTEM.ADR(Common.DumpClass);
  MDICreateStruct.szTitle      := SYSTEM.ADR(Common.MDIDumpTitle);
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
  Common.ActiveMDIClient.Mode  :=  1;
  UITabControl.Append(AhWnd, "Dump");

  RETURN 0

END DumpTheFile;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN;
  ;
END Dump.

