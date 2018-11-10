(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Dump_DD                                     V 1.42.00         *)
(*                                                         2002MAR16         *)
(*  PURPOSE:   Processing Debug Infos                                        *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   WriteLines00                                                            *)
(*             generate lines for displaying the EXPORT Data Directory       *)
(*   WriteLines01                                                            *)
(*             generate lines for displaying the IMPORT Data Directory       *)
(*   WriteLines02                                                            *)
(*             generate lines for displaying the RESOURCE Data Directory     *)
(*   WriteLines05                                                            *)
(*             generate lines for displaying the BASE RELOCATION Data        *)
(*             Directory                                                     *)
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

MODULE Dump_DD;


IMPORT
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  Strings, SYSTEM,
  Dump, Global, StatusLine;


CONST
  Version*             =              "V 1.42.00";
  Module*              =              "Dump_DD";
  
  LineEmpty    =                      "   ";
  Line0000     =                      "DATA DIRECTORIES HEADER";

  Line0001     =                      "DATA DIRECTORY 00:   Export Table";
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

  Line0101     =                      "DATA DIRECTORY 01:   Import Table";
  Line0102     =                      "  Hint/Name Table:              ";
  Line0103     =                      "  Time Date Stamp:              ";
  Line0104     =                      "  Forwarder Chain:              ";
  Line0105     =                      "  First Thunk RVA:              ";
  Line0106     =                      "  Ordn  Name";

  Line0201     =                      "DATA DIRECTORY 02:   Resource Table";
  Line0202     =                      "  Resource Directory Table (Level ";
  Line0203     =                      "    Characteristics:            ";
  Line0204     =                      "    TimeDateStamp:              ";
  Line0205     =                      "    MajorVersion:               ";
  Line0206     =                      "    MinorVersion:               ";
  Line0207     =                      "    Number of named entries:    ";
  Line0208     =                      "    Number of ID entries:       ";
  Line0209     =                      "    Resource Directory Entries  ";
  Line0210     =                      "      Named entries   at address ";
  Line0211     =                      "        Name,   RVA:            ";
  Line0220     =                      "      ID entries      at address ";
  Line0221     =                      "        Int ID, RVA:            ";
  
  Line0301     =                      "DATA DIRECTORY 03:   Exception Table";
  Line0401     =                      "DATA DIRECTORY 04:   Certificate Table";
  Line0501     =                      "DATA DIRECTORY 05:   Base Relocation Table";
  Line0601     =                      "DATA DIRECTORY 06:   Debug Data";          (* refer to module DD_Debug *)

  Line0701     =                      "DATA DIRECTORY 07:   Architecture-specific Data";
  Line0801     =                      "DATA DIRECTORY 08:   Global Pointer";
  Line0901     =                      "DATA DIRECTORY 09:   Thread Local Storage Table (TLS)";
  Line1001     =                      "DATA DIRECTORY 10:   Load Config Table";
  Line1101     =                      "DATA DIRECTORY 11:   Bound Import Table";
  Line1201     =                      "DATA DIRECTORY 12:   Import Address Table (IAT)";
  Line1301     =                      "DATA DIRECTORY 13:   Delay Import Descriptor";
  Line1401     =                      "DATA DIRECTORY 14:   COM+ Runtime Header";
  Line1501     =                      "DATA DIRECTORY 15:   Reserved";
  
  maxArrayLength       =               MAX(INTEGER);


TYPE
  PCharArray           =               POINTER TO ARRAY maxArrayLength OF CHAR;
  PStringArray         =               POINTER TO ARRAY maxArrayLength OF PCharArray;
  PDWordArray          =               POINTER TO ARRAY maxArrayLength OF WinDef.DWORD;
  PWordArray           =               POINTER TO ARRAY maxArrayLength OF WinDef.WORD;
  PWord                =               POINTER TO ARRAY 1              OF WinDef.WORD;
  ImportHintName       =
               RECORD
                 Ordinal:              WinDef.WORD;
                 Name:                 ARRAY 1024 OF CHAR;
               END (* ImportHintName *);
  PImportHintName      =               POINTER TO ImportHintName;
  
  
VAR
  ActLine,
  MyLine:                              Global.ScreenLineP;
  MyChars:                             ARRAY 32 OF CHAR;
  NumberOfLines:                       LONGINT;
  
  PMyCharArray:                        PCharArray;
  PMyImportHintName:                   PImportHintName;
  PMyStringArray:                      PStringArray;
  PMyDWordArray:                       PDWordArray;
  PMyWord:                             PWord;
  
  PMyImageSectionHeader:               WinNT.PIMAGE_SECTION_HEADER;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines00                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* DataDirectories EXPORT mode                                               *)
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

PROCEDURE WriteLines00*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  Done:                                BOOLEAN;
  Delta,
  MyPointer,
  MyPointer2,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  NumberOfBytes:                       LONGINT;
  POrdinals:                           PWordArray;
  PFunctions:                          PDWordArray;
  PNames:                              PDWordArray;
  PMyImage_Export_Directory:           WinNT.PIMAGE_EXPORT_DIRECTORY;
  PMyImportHintName:                   PImportHintName;

BEGIN;

  ActLine                      := FirstLineP;
  ActLine^.Type                :=  0;
  ActLine^.Format              := Global.Text01;
  ActLine^.Usage               :=  0;
  NumberOfLines                :=  0;
  Done                         := FALSE;
  MyPointer                    := Global.ImageDirectoryOffset(0, PMyImageSectionHeader);
  PMyImage_Export_Directory    := SYSTEM.VAL(WinNT.PIMAGE_EXPORT_DIRECTORY, MyPointer);
  Delta                        := PMyImageSectionHeader.VirtualAddress - PMyImageSectionHeader.PointerToRawData;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line0001, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  IF MyPointer=0 THEN
    COPY ("   No export directory found.", ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN NumberOfLines
  END (* IF MyPointer=0 *);

  COPY (Line0002, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.Characteristics, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line0003, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.TimeDateStamp, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line0004, ActLine^.Text);
  Strings.Str(PMyImage_Export_Directory^.MajorVersion, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  Strings.AppendChar(ActLine^.Text, ".");
  Strings.Str(PMyImage_Export_Directory^.MinorVersion, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
  COPY (Line0006, ActLine^.Text);
  MyPointer2   := PMyImage_Export_Directory^.Name - Delta + Global.MyFileDescription.lpFileBase;
  PMyCharArray := SYSTEM.VAL(PCharArray, MyPointer2);
  Strings.Append(ActLine^.Text, PMyCharArray^);
  ActLine^.Format  := Global.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line0007, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.Base, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line0008, ActLine^.Text);
  Strings.Str(PMyImage_Export_Directory^.NumberOfFunctions, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  COPY (Line0009, ActLine^.Text);
  Strings.Str(PMyImage_Export_Directory^.NumberOfNames, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  COPY (Line0010, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.AddressOfFunctions, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
  COPY (Line0011, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.AddressOfNames, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (Line0012, ActLine^.Text);
  Strings.UHexStr(PMyImage_Export_Directory^.AddressOfNameOrdinals, 4, MyChars);
  Strings.RightAlign(MyChars, 10);
  Strings.Append(ActLine^.Text, MyChars);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  MyPointer2   := PMyImage_Export_Directory^.AddressOfNameOrdinals - Delta + Global.MyFileDescription.lpFileBase;
  POrdinals    := SYSTEM.VAL(PWordArray, MyPointer2);
  MyPointer2   := PMyImage_Export_Directory^.AddressOfFunctions - Delta + Global.MyFileDescription.lpFileBase;
  PFunctions   := SYSTEM.VAL(PDWordArray, MyPointer2);
  MyPointer2   := PMyImage_Export_Directory^.AddressOfNames - Delta + Global.MyFileDescription.lpFileBase;
  PNames       := SYSTEM.VAL(PDWordArray, MyPointer2);
  
  COPY (Line0020, ActLine^.Text);
  ActLine^.Format  := Global.Header3;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  ActLine^.Format  := Global.Text01;
  FOR i:=0 TO PMyImage_Export_Directory^.NumberOfNames-1 DO
    Strings.Str(POrdinals[i], MyChars);
    Strings.RightAlign(MyChars, 5);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.UHexStr(PFunctions[i], 4, MyChars);
    Strings.RightAlign(MyChars, 12);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.Append(ActLine^.Text, "                ");
    MyPointer2   := PNames[i] - Delta + Global.MyFileDescription.lpFileBase;
    PMyCharArray := SYSTEM.VAL(PCharArray, MyPointer2);
    Strings.Append(ActLine^.Text, PMyCharArray^);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  END;

  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  RETURN NumberOfLines
  
END WriteLines00;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines01                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* DataDirectories IMPORT mode                                               *)
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

PROCEDURE WriteLines01*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  Done:                                BOOLEAN;
  Delta,
  MyPointer,
  MyPointer2,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  NumberOfBytes:                       LONGINT;
  PMyCharArray:                        PCharArray;
  PMyImage_Import_Descriptor:          WinNT.PIMAGE_IMPORT_DESCRIPTOR;
  PMyImportHintName:                   PImportHintName;
  
BEGIN;

  ActLine                      := FirstLineP;
  ActLine^.Type                :=  0;
  ActLine^.Format              := Global.Text01;
  ActLine^.Usage               :=  0;
  NumberOfLines                :=  0;
  Done                         := FALSE;
  MyPointer                    := Global.ImageDirectoryOffset(1, PMyImageSectionHeader);
  PMyImage_Import_Descriptor   := SYSTEM.VAL(WinNT.PIMAGE_IMPORT_DESCRIPTOR, MyPointer);
  Delta                        := PMyImageSectionHeader.VirtualAddress - PMyImageSectionHeader.PointerToRawData;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line0101, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  IF MyPointer=0 THEN
    COPY ("   No import directory found.", ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN NumberOfLines
  END (* IF MyPointer=0 *);

  REPEAT
    
    MyPointer2   := PMyImage_Import_Descriptor^.Name - Delta + Global.MyFileDescription.lpFileBase;
    PMyCharArray := SYSTEM.VAL(PCharArray, MyPointer2);
    Strings.Append(ActLine^.Text, PMyCharArray^);
    ActLine^.Format  := Global.Header2;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line0102, ActLine^.Text);
    Strings.UHexStr(PMyImage_Import_Descriptor^.u.Characteristics, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format  := Global.Text01;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0103, ActLine^.Text);
    Strings.UHexStr(PMyImage_Import_Descriptor^.TimeDateStamp, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0104, ActLine^.Text);
    Strings.UHexStr(PMyImage_Import_Descriptor^.ForwarderChain, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line0105, ActLine^.Text);
    Strings.UHexStr(PMyImage_Import_Descriptor^.Name, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    COPY (Line0106, ActLine^.Text);
    ActLine^.Format  := Global.Header3;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    MyPointer2       := SYSTEM.VAL(LONGINT, PMyImage_Import_Descriptor^.FirstThunk) - Delta + Global.MyFileDescription.lpFileBase;
    PMyDWordArray    := SYSTEM.VAL(PDWordArray, MyPointer2);
    j                :=  0;
    ActLine^.Format  := Global.Text01;
    REPEAT
      IF SYSTEM.BIT(PMyDWordArray[j], 31) THEN
        Strings.Str(SYSTEM.LOWORD(PMyDWordArray[j]), MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
      ELSE
        MyPointer2         := SYSTEM.VAL(LONGINT, PMyDWordArray[j]) - Delta + Global.MyFileDescription.lpFileBase;
        PMyImportHintName  := SYSTEM.VAL(PImportHintName, MyPointer2);
        Strings.Str(PMyImportHintName^.Ordinal, MyChars);
        Strings.RightAlign(MyChars, 5);
        Strings.Append(ActLine^.Text, MyChars);
        Strings.Append(ActLine^.Text, "   ");
        Strings.Append(ActLine^.Text, PMyImportHintName^.Name);
      END (* IF SYSTEM.BIT(PMyDWordArray[j], 31) *);
      INC(j);
      INC(SYSTEM.VAL(LONGINT, PMyCharArray), 4);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    UNTIL PMyDWordArray[j]=0;


    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    INC(SYSTEM.VAL(LONGINT, PMyImage_Import_Descriptor), SIZE(WinNT.IMAGE_IMPORT_DESCRIPTOR));

  UNTIL ((PMyImage_Import_Descriptor^.TimeDateStamp=0) & (PMyImage_Import_Descriptor^.Name=0));

  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  RETURN NumberOfLines
  
END WriteLines01;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines02                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* DataDirectories RESOURCE mode                                             *)
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

PROCEDURE WriteLines02*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  Done:                                BOOLEAN;
  Delta,
  Index,
  Index2,
  Level,
  MyPointer,
  MyPointer2BaseOfResource,
  MyPointer2,
  MyPointer3,
  Value,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  NumberOfBytes:                       LONGINT;
  PMyImage_Resource_Directory:         WinNT.PIMAGE_RESOURCE_DIRECTORY;
  PMyImage_Resource_Directory_Entry:   WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY;
  
  (*****************************************************************************)
  PROCEDURE ResourceType2String         (Type:               LONGINT;
                                         VAR String:         ARRAY OF CHAR);
  BEGIN
    CASE Type OF
      WinUser.RT_CURSOR:
        COPY(" (Cursor);       ", String);
      |
      WinUser.RT_BITMAP:
        COPY(" (Bitmap);       ", String);
      |
      WinUser.RT_ICON:
        COPY(" (Icon);         ", String);
      |
      WinUser.RT_MENU:
        COPY(" (Menu);         ", String);
      |
      WinUser.RT_DIALOG:
        COPY(" (Dialog);       ", String);
      |
      WinUser.RT_STRING:
        COPY(" (String);       ", String);
      |
      WinUser.RT_FONTDIR:
        COPY(" (FontDir);      ", String);
      |
      WinUser.RT_FONT:
        COPY(" (Font);         ", String);
      |
      WinUser.RT_ACCELERATOR:
        COPY(" (Accelerator);  ", String);
      |
      WinUser.RT_RCDATA:
        COPY(" (RC Data);      ", String);
      |
      WinUser.RT_MESSAGETABLE:
        COPY(" (MessageTable); ", String);
      |
      WinUser.RT_GROUP_CURSOR:
        COPY(" (Group Cursor); ", String);
      |
      WinUser.RT_GROUP_ICON:
        COPY(" (Group Icon);   ", String);
      |
      WinUser.RT_VERSION:
        COPY(" (Version);      ", String);
      |
      WinUser.RT_DLGINCLUDE:
        COPY(" (DLG Include);  ", String);
      |
      WinUser.RT_PLUGPLAY:
        COPY(" (Plug & Play);  ", String);
      |
      WinUser.RT_VXD:
        COPY(" (VXD);          ", String);
      |
      WinUser.RT_ANICURSOR:
        COPY(" (Any Cursor);   ", String);
      |
      WinUser.RT_ANIICON:
        COPY(" (Any Icon);     ", String);
      ELSE
        COPY(" (unknown);      ", String);
    END (* CASE Value *);
  END ResourceType2String;

  (*****************************************************************************)
  PROCEDURE WriteResourceDirectory      (AddressOfDirectory: LONGINT;
                                         VAR NumberOfCol:    LONGINT;
                                         VAR NumberOfLines:  LONGINT);
  VAR
    Done:                                BOOLEAN;
    MyChars:                             ARRAY 32 OF CHAR;
    Index,
    MyPointer,
    MyPointer2,
    MyPointer3,
    Offset:                              LONGINT;
    i, j, IntByte:                       INTEGER;
    PMyImage_Resource_Directory:         WinNT.PIMAGE_RESOURCE_DIRECTORY;
    PMyImage_Resource_Directory_Entry:   WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY;
  
  BEGIN
    PMyImage_Resource_Directory  := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY, AddressOfDirectory);
  
    COPY (Line0203, ActLine^.Text);
    Strings.UHexStr(PMyImage_Resource_Directory^.Characteristics, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0204, ActLine^.Text);
    Strings.UHexStr(PMyImage_Resource_Directory^.TimeDateStamp, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line0205, ActLine^.Text);
    Strings.Str(PMyImage_Resource_Directory^.MajorVersion, MyChars);
    Strings.RightAlign(MyChars, 9);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ".");
    Strings.Str(PMyImage_Resource_Directory^.MinorVersion, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      
    COPY (Line0207, ActLine^.Text);
    Strings.Str(PMyImage_Resource_Directory^.NumberOfNamedEntries, MyChars);
    Strings.RightAlign(MyChars, 9);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line0208, ActLine^.Text);
    Strings.Str(PMyImage_Resource_Directory^.NumberOfIdEntries, MyChars);
    Strings.RightAlign(MyChars, 9);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    (* Read Resource Directory Entries *)
    MyPointer                            := AddressOfDirectory + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY);
    PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
    
    COPY (Line0209, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line0210, ActLine^.Text);
    Strings.UHexStr(MyPointer, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format  := Global.Header3;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    (* Read Resource Directory Named Entries *)
    ActLine^.Format  := Global.Text01;
    Index            := PMyImage_Resource_Directory^.NumberOfNamedEntries;
    WHILE Index>0 DO
      COPY (Line0211, ActLine^.Text);
      Offset             := SYSTEM.LOWORD(SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.d.u));
      Offset             := MyPointer2BaseOfResource + Offset;
      PMyImportHintName  := SYSTEM.VAL(PImportHintName, Offset);
      FOR i:=0 TO 2*PMyImportHintName^.Ordinal-1 DO          (* caveat, this is UNICODE!! *)
        Strings.AppendChar(ActLine^.Text, PMyImportHintName^.Name[i]);
      END;
      
      MyPointer2 := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.q.p);
      Strings.UHexStr(MyPointer2, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      IF SYSTEM.BIT(MyPointer2, 31) THEN
        Strings.Append(ActLine^.Text, "  (Subdirectory RVA)");
      ELSE
        Strings.Append(ActLine^.Text, "  (Data Entry   RVA)");
      END;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      MyPointer                            := MyPointer + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY_ENTRY);
      PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
      DEC(Index);
    END;
  
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line0220, ActLine^.Text);
    Strings.UHexStr(MyPointer, 4, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format  := Global.Header3;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    (* Read Resource Directory ID Entries *)
    ActLine^.Format  := Global.Text01;
    Index            := PMyImage_Resource_Directory^.NumberOfIdEntries;
    WHILE Index>0 DO
      COPY (Line0221, ActLine^.Text);
      Value      := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.d.u);
      Strings.UHexStr(Value, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      ResourceType2String(Value, MyChars);
      Strings.Append(ActLine^.Text, MyChars);
      
      MyPointer2 := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.q.p);
      Strings.UHexStr(MyPointer2, 4, MyChars);
      Strings.RightAlign(MyChars, 10);
      Strings.Append(ActLine^.Text, MyChars);
      IF SYSTEM.BIT(MyPointer2, 31) THEN
        Strings.Append(ActLine^.Text, "  (Subdirectory RVA)");
      ELSE
        Strings.Append(ActLine^.Text, "  (Data Entry   RVA)");
      END;
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
      MyPointer                            := MyPointer + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY_ENTRY);
      PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
      DEC(Index);
    END;
  END WriteResourceDirectory;
  
BEGIN;

  ActLine                      := FirstLineP;
  ActLine^.Type                :=  0;
  ActLine^.Format              := Global.Text01;
  ActLine^.Usage               :=  0;
  NumberOfLines                :=  0;
  Done                         := FALSE;
  Level                        :=  1;
  MyPointer                    := Global.ImageDirectoryOffset(2, PMyImageSectionHeader);
  MyPointer2BaseOfResource     := MyPointer;
  Delta                        := PMyImageSectionHeader.VirtualAddress - PMyImageSectionHeader.PointerToRawData;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line0201, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  IF MyPointer=0 THEN
    COPY ("   No RESOURCE directory found.", ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    RETURN NumberOfLines
  END (* IF MyPointer=0 *);

  (* Read Resource Directory Table Level 1 *)
  PMyImage_Resource_Directory  := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY, MyPointer);
  COPY (Line0202, ActLine^.Text);
  Strings.Str(Level, MyChars);
  Strings.Append(ActLine^.Text, MyChars);
  Strings.AppendChar(ActLine^.Text, ")");
  ActLine^.Format  := Global.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  INC(Level);
  
  ActLine^.Format  := Global.Text01;
  WriteResourceDirectory(MyPointer, NumberOfCol, NumberOfLines);
  
  (* Read Resource Directory Table Level 2 *)
  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Header3;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  MyPointer                            := MyPointer + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY);
  PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
  
  Index    := PMyImage_Resource_Directory^.NumberOfNamedEntries;
  Index2   := PMyImage_Resource_Directory^.NumberOfIdEntries;
  WHILE Index>0 DO                                         (* Read Named Entries *)
    MyPointer3                     := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.q.p);
    MyPointer3                     := MyPointer2BaseOfResource + SYSTEM.LOWORD(MyPointer3);
    PMyImage_Resource_Directory    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY, MyPointer3);
    COPY (Line0202, ActLine^.Text);
    Strings.Str(Level, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ")");
    Value      := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.d.u);
    ResourceType2String(Value, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format  := Global.Header2;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    ActLine^.Format  := Global.Text01;
    WriteResourceDirectory(MyPointer3, NumberOfCol, NumberOfLines);
    
    COPY (LineEmpty, ActLine^.Text);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    MyPointer                            := MyPointer + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY_ENTRY);
    PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
    ActLine^.Format  := Global.Text01;
    DEC(Index);
  END (* WHILE Index>0 *);

  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Header3;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
  
  WHILE Index2>0 DO                                         (* Read ID Entries *)
    MyPointer3                     := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.q.p);
    MyPointer3                     := MyPointer2BaseOfResource + SYSTEM.LOWORD(MyPointer3);
    PMyImage_Resource_Directory    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY, MyPointer3);
    COPY (Line0202, ActLine^.Text);
    Strings.Str(Level, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    Strings.AppendChar(ActLine^.Text, ")");
    Value      := SYSTEM.VAL(LONGINT, PMyImage_Resource_Directory_Entry^.d.u);
    ResourceType2String(Value, MyChars);
    Strings.Append(ActLine^.Text, MyChars);
    ActLine^.Format  := Global.Header2;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    ActLine^.Format  := Global.Text01;
    WriteResourceDirectory(MyPointer3, NumberOfCol, NumberOfLines);
    
    COPY (LineEmpty, ActLine^.Text);
    ActLine^.Format  := Global.Header3;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    COPY (LineEmpty, ActLine^.Text);
    ActLine^.Format  := Global.Text01;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    MyPointer                            := MyPointer + SIZE(WinNT.IMAGE_RESOURCE_DIRECTORY_ENTRY);
    PMyImage_Resource_Directory_Entry    := SYSTEM.VAL(WinNT.PIMAGE_RESOURCE_DIRECTORY_ENTRY, MyPointer);
    DEC(Index2);
  END (* WHILE Index>0 *);

  RETURN NumberOfLines
  
END WriteLines02;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines05                                                              *)
(* Generates the lines to be displayed when the program switches to          *)
(* DataDirectories BASE RELOCATION mode                                      *)
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

PROCEDURE WriteLines05*               (FirstLineP:         Global.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  Done:                                BOOLEAN;
  Delta,
  MyPointer,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  NumberOfBytes:                       LONGINT;
  
BEGIN;

  ActLine                      := FirstLineP;
  ActLine^.Type                :=  0;
  ActLine^.Format              := Global.Text01;
  ActLine^.Usage               :=  0;
  NumberOfLines                :=  0;
  Done                         := FALSE;
  
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line0501, ActLine^.Text);
  ActLine^.Format  := Global.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (LineEmpty, ActLine^.Text);
  ActLine^.Format  := Global.Text01;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  RETURN NumberOfLines
  
END WriteLines05;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  ;
  
END Dump_DD.

