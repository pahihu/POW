(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     DumpSections                                V 2.00.04         *)
(*                                                         2003APR18         *)
(*  PURPOSE:   Processing Debug Infos                                        *)
(*             Section Headers                                               *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   WriteLines                                                              *)
(*             Generate lines to be displayed                                *)
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

MODULE DumpSections;


IMPORT
  Common, Dump, UIStatusLine,
  Strings,
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.04";
  Module*      =                      "Dump_Sections";
  
  LineEmpty    =                      "   ";
  Line001      =                      "SECTION HEADERS";
  Line002      =                      "  number of sections:           ";
  Line009      =                      "Section #";
  Line010      =                      "  Name:                         ";
  Line011      =                      "  Virtual Size:                 ";
  Line012      =                      "  Virtual Address:              ";
  Line013      =                      "  Size of Raw Data:             ";
  Line014      =                      "  Pointer to Raw Data:          ";
  Line015      =                      "  Pointer to Relocations:       ";
  Line016      =                      "  Pointer to Linenumbers:       ";
  Line017      =                      "  Number of Relocations:        ";
  Line018      =                      "  Number of Linenumbers:        ";
  Line019      =                      "  Characteristics:              ";
  Line100      =                      "                                ";
  Line105      =                      "                                 contains executable code.";
  Line106      =                      "                                 contains initialized Data.";
  Line107      =                      "                                 contains uninitialized data.";
  Line124      =                      "                                 contains extended relocations.";
  Line125      =                      "                                 can be discarded as needed.";
  Line126      =                      "                                 cannot be reached.";
  Line127      =                      "                                 is not pageable.";
  Line128      =                      "                                 can be shared in memory.";
  Line129      =                      "                                 can be executed as code.";
  Line130      =                      "                                 can be read.";
  Line131      =                      "                                 can be written to.";


TYPE
  String       =                       ARRAY 4 OF CHAR;

VAR    
  hInst:                               WinDef.HANDLE;
  ReturnCode:                          LONGINT;
  ByteAddress:                         LONGINT;            (* Pointer to the actual Byte *)
  HexAddress,
  AsciiData:                           ARRAY  20 OF CHAR;
  HexData:                             ARRAY  90 OF CHAR;
  PSectionHeader:                      Common.PSectionHeader;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines                                                                *)
(* Generates the lines to be displayed when the program switches to          *)
(* SectionData mode                                                          *)
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
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  MyByte:                              SYSTEM.BYTE;
  MyBytes:                             ARRAY 17 OF SYSTEM.BYTE;
  MyChars:                             ARRAY 17 OF CHAR;
  MyLongInt,
  MyPointer:                           LONGINT;
  NumberOfBytes,
  NumberOfLines:                       LONGINT;
  
BEGIN;

  ActLine          := FirstLineP;
  ActLine^.Format  := Common.Text01;
  NumberOfLines    :=  0;
  PSectionHeader   := Common.FirstSectionHeader;
  
  COPY (Line001, ActLine^.Text);
  ActLine^.Format  := Common.Header1;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
  COPY (Line002, ActLine^.Text);
  Strings.Str(Common.PMyNT_Header^.FileHeader.NumberOfSections, MyChars);
  Strings.RightAlign(MyChars, 9);
  Strings.Append(ActLine^.Text, MyChars);
  ActLine^.Format  := Common.Header2;
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  FOR i:=1 TO Common.PMyNT_Header^.FileHeader.NumberOfSections DO

    COPY (LineEmpty, ActLine^.Text);
    ActLine^.Format  := Common.Text01;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

    COPY (Line009, ActLine^.Text);
    ActLine^.Format  := Common.Header2;
    Strings.Str(i, MyChars);
    Strings.RightAlign(MyChars, 9);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (LineEmpty, ActLine^.Text);
    ActLine^.Format  := Common.Text01;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
  
    COPY (Line010, ActLine^.Text);
    Strings.Append(ActLine^.Text, PSectionHeader^.PSectionHeader^.Name);
    ActLine^.Format  := Common.Header3;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line011, ActLine^.Text);
(*    Strings.HexStr(PSectionHeader^.SectionHeader.Misc, MyChars);
    Strings.Append(ActLine^.Text, MyChars);*)
    ActLine^.Format  := Common.Text01;
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line012, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.VirtualAddress, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line013, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.SizeOfRawData, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line014, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.PointerToRawData, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line015, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.PointerToRelocations, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line016, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.PointerToLinenumbers, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line017, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.NumberOfRelocations, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line018, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.NumberOfLinenumbers, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
        
    COPY (Line019, ActLine^.Text);
    Strings.UHexStr(PSectionHeader^.PSectionHeader^.Characteristics, 4, MyChars);
    Strings.RightAlign(MyChars, 10);
    Strings.Append(ActLine^.Text, MyChars);
    Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics,  5) THEN
      COPY (Line105, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics,  6) THEN
      COPY (Line106, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics,  7) THEN
      COPY (Line107, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 24) THEN
      COPY (Line124, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 25) THEN
      COPY (Line125, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 26) THEN
      COPY (Line126, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 27) THEN
      COPY (Line127, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 28) THEN
      COPY (Line128, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 29) THEN
      COPY (Line129, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 30) THEN
      COPY (Line130, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
    IF SYSTEM.BIT(PSectionHeader^.PSectionHeader^.Characteristics, 31) THEN
      COPY (Line131, ActLine^.Text);
      Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);
    END;
        
    IF PSectionHeader^.Next#NIL THEN
      PSectionHeader := PSectionHeader^.Next;
    END;
    
  END (* FOR i:=0 TO ... *);
      
  COPY (LineEmpty, ActLine^.Text);
  Dump.NextLine(ActLine, NumberOfCol, NumberOfLines);

  RETURN NumberOfLines
  
END WriteLines;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN;

END DumpSections.

