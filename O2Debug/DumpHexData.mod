(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     DumpHexData                                 V 2.00.04         *)
(*                                                         2003APR18         *)
(*  PURPOSE:   Processing HexData                                            *)
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

MODULE DumpHexData;


IMPORT
  Common, UIStatusLine,
  Strings,
  WinBase, WinDef, WinGDI, WinUser,
  SYSTEM;


CONST
  Version*     =                      "V 2.00.04";
  Module*      =                      "HexData";
  
  LineEmpty    =                      "   ";
  LineLength   =                       16;


VAR    
  hInst:                               WinDef.HANDLE;
  ReturnCode:                          LONGINT;
  ByteAddress:                         LONGINT;            (* Pointer to the actual Byte *)
  HexAddress,
  AsciiData:                           ARRAY  20 OF CHAR;
  HexData:                             ARRAY  90 OF CHAR;


(*****************************************************************************)
(*                                                                           *)
(* ShowLine                                                                  *)
(* zeigt eine Zeile (= 16 Byte) des aktuellen Blocks an                      *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*   LineNumber        Nummer der Zeile, die ausgegeben werden soll          *)
(*                     diese Nummer kann 0 oder 10H, 20H, 30H, ..., also ein *)
(*                     Vielfaches (16-fach) von 1 sein                       *)
(*   Line              Darstellung der Ascii Werte der Bytes, nicht          *)
(*                     darstellbare Werte werden mit . angezeigt             *)
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

PROCEDURE  ShowLine                   (LineNumber:         LONGINT;
                                       VAR Line:           ARRAY OF CHAR)
                                      :                    LONGINT ;

VAR
  ActAddress,
  BytesRead:                           LONGINT;
  i, j, IntByte:                       INTEGER;
  MyByte:                              SYSTEM.BYTE;
  MyBytes:                             ARRAY 17 OF SYSTEM.BYTE;
  
  
BEGIN;
  ByteAddress  := LineNumber*LineLength;
  BytesRead    := Common.ReadBytes(ByteAddress, LineLength, MyBytes);
  IF BytesRead=0 THEN
    RETURN 0
  END;
  
  AsciiData[0] := ":";
  AsciiData[1] := " ";
  
  HexData[0]   := ":";
  HexData[1]   := " ";
  
  Strings.HexStr(ByteAddress, HexAddress);
  Strings.RightAlign(HexAddress, 9);
  FOR j:=0 TO (LEN(HexAddress)-1) DO;
    IF HexAddress[j]=" "
    THEN
      HexAddress[j] := "0";
    END;
  END (* FOR j:=0 TO (LEN(HexAdresse)-1) DO *);
  FOR i:=0 TO 15 DO
    MyByte     := MyBytes[i];
    MyByte     := SYSTEM.LSH(MyByte, -4);
    MyByte     := SYSTEM.VAL(SYSTEM.BYTE, SYSTEM.BITAND(SYSTEM.VAL(SHORTINT, MyByte), 8+4+2+1));
    CASE SYSTEM.VAL(SHORTINT, MyByte) OF
      0..9:
        HexData[3*i+2] := CHR(48+SYSTEM.VAL(SHORTINT, MyByte));
      |
      10..15:
        HexData[3*i+2] := CHR(55+SYSTEM.VAL(SHORTINT, MyByte));
    ELSE
      HexData[3*i+2] := "X";
    END;

    MyByte     := MyBytes[i];
    MyByte     := SYSTEM.VAL(SYSTEM.BYTE, SYSTEM.BITAND(SYSTEM.VAL(SHORTINT, MyByte), 8+4+2+1));
    CASE SYSTEM.VAL(SHORTINT, MyByte) OF
      0..9:
        HexData[3*i+3] := CHR(48+SYSTEM.VAL(SHORTINT, MyByte));
      |
      10..15:
        HexData[3*i+3] := CHR(55+SYSTEM.VAL(SHORTINT, MyByte));
    ELSE
      HexData[3*i+3] := "X";
    END;
    HexData[3*i+4] := " ";

    MyByte     := MyBytes[i];
    CASE SYSTEM.VAL(SHORTINT, MyByte) OF
      32..126:
        AsciiData[i+2] := CHR(SYSTEM.VAL(SHORTINT, MyByte));
      ELSE
        AsciiData[i+2] := ".";
    END (* CASE MyByte OF *);
  END (*   FOR i:=0 TO 15 DO *);

  HexData[2+3*i+1]   := 0X;
  AsciiData[2+i+1]   := 0X;

  Line[0]            := 0X;
  Strings.Append(Line, HexAddress);
  Strings.Append(Line, HexData);
  Strings.Append(Line, AsciiData);
  
  RETURN BytesRead

END ShowLine;


(*****************************************************************************)
(*                                                                           *)
(* WriteLines                                                                *)
(* Displays data in Hex format                                               *)
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

PROCEDURE WriteLines*                 (FirstLineP:         Common.ScreenLineP;
                                       VAR NumberOfCol:    LONGINT)
                                      :                    LONGINT;

VAR
  ActLine,
  MyLine:                              Common.ScreenLineP;
  NumberOfBytes,
  NumberOfLines:                       LONGINT;
  
BEGIN
  
  NumberOfLines  :=  0;
  ActLine        := FirstLineP;
  ActLine.Format := Common.Text01;
  NumberOfBytes  := ShowLine(NumberOfLines, ActLine.Text);
  NumberOfCol    := Strings.Length(ActLine.Text);
  
  REPEAT
    INC(NumberOfLines);
    IF ActLine.Next=NIL THEN
      NEW(MyLine);
      MyLine.Previous := ActLine;
      ActLine.Next   := MyLine;
      ActLine        := MyLine;
      ActLine.Next   := NIL;
    ELSE
      ActLine        := ActLine.Next;
    END;
    ActLine.Text[0]    := 0X;
    ActLine.Format     := Common.Text01;
    NumberOfBytes      := ShowLine(NumberOfLines, ActLine.Text);
  UNTIL NumberOfBytes=0;
  
  RETURN NumberOfLines

END WriteLines;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN;

  ByteAddress          :=    0;

END DumpHexData.

