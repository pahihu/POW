(*****************************************************************************)
(*                                                                           *)
(* Project:    BoostEd32                                                     *)
(*                                                                           *)
(* Module:     GlobMem                                     V 2.00.05         *)
(*                                                         2005JUN14         *)
(*  PURPOSE:   This module supports handling of Windows Global Memory        *)
(*             objects.                                                      *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*                                                                           *)
(* Author(s):                                                                *)
(*                     Michael Bogner, Max Mayrbäurl                         *)
(*             BL      Bernhard Leisch                                       *)
(*                     Alexander Bergsmann                                   *)
(*             KlS     schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created                                                                  *)
(*   1998                                                                    *)
(*                                                                           *)
(*  update                                                                   *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(* Comments                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE GlobMem;


IMPORT
  SYSTEM, 
  WinBase, WinDef, WinUser, 
  Strings, 
  ListSt, GlobWin;


(*****************************************************************************)
PROCEDURE CopyChar*                   (globMem:            WinDef.HGLOBAL;
                                       Char:               CHAR);
(* Globaler Speicherbuffer, kopiert ein Zeichen in globalen Speicher *)
VAR
  lpGlob,
  inx:                                 LONGINT;
  chx:                                 CHAR;
BEGIN
  IF globMem=WinDef.NULL THEN
    GlobWin.Beep;
    RETURN ;
  END (* IF globMem=WinDef.NULL *);
  lpGlob       := WinBase.GlobalLock(globMem);
  (* globalen Speicherbereich sperren *)
  ASSERT(lpGlob#WinDef.NULL);
  inx          := 0;
  SYSTEM.GET(lpGlob + inx, chx);
  WHILE (inx<ListSt.MAXLENGTH) & (chx#0X) DO
    INC(inx);
    SYSTEM.GET(lpGlob+inx, chx);
  END (* WHILE (inx<ListSt.MAXLENGTH) &  *);
  IF inx<ListSt.MAXLENGTH THEN
    SYSTEM.PUT(lpGlob + inx, Char);
    SYSTEM.PUT(lpGlob + inx + 1, 0X);
  ELSE
    GlobWin.Beep;
  END (* IF inx<ListSt.MAXLENGTH *);
  lpGlob       := WinBase.GlobalUnlock(globMem);
  (* Sperren aufheben *)
END CopyChar;


(*****************************************************************************)
PROCEDURE NewLineBuf*                 (VAR globMem:        WinDef.HGLOBAL);
(* globalen Speicherbuffer allokieren ausreichend für eine Zeile und Initialisieren *)
(* mit einem leeren String.                                                         *)
VAR
  lpGlob:                              LONGINT;
BEGIN
  globMem      := WinBase.GlobalAlloc(WinBase.GMEM_MOVEABLE, ListSt.MAXLENGTH + 1);
  (* Speicher allokieren *)
  IF globMem=WinDef.NULL THEN
    RETURN 
  END (* IF globMem=WinDef.NULL *);

  lpGlob       := WinBase.GlobalLock(globMem);
  (* Speicherbereich sperren *)
  ASSERT(lpGlob#WinDef.NULL);
  SYSTEM.PUT(lpGlob, 0X);
  lpGlob       := WinBase.GlobalUnlock(globMem);
  (* Sperren aufheben *)
END NewLineBuf;


(*****************************************************************************)
PROCEDURE InsertChar*                 (globMem:            WinDef.HGLOBAL;
                                       ch:                 CHAR);
(* ein Zeichen am Beginn des globalen Speicherbuffers einfügen *)
VAR
  lpGlob,
  i,
  inx:                                 LONGINT;
  chx:                                 CHAR;
BEGIN
  IF globMem=WinDef.NULL THEN
    GlobWin.Beep;
    RETURN ;
  END (* IF globMem=WinDef.NULL *);
  lpGlob       := WinBase.GlobalLock(globMem);
  (* globalen Speicherbereich sperren *)
  ASSERT(lpGlob#WinDef.NULL);
  inx          := 0;
  SYSTEM.GET(lpGlob + inx, chx);
  WHILE (inx<ListSt.MAXLENGTH) & (chx#0X) DO
    INC(inx);
    SYSTEM.GET(lpGlob+inx, chx);
  END (* WHILE (inx<ListSt.MAXLENGTH) &  *);
  IF inx<ListSt.MAXLENGTH THEN
    FOR i:=inx TO 0 BY-1 DO
      SYSTEM.GET(lpGlob+i, chx);
      SYSTEM.PUT(lpGlob+i+1, chx);
    END (* FOR i:=inx TO 0 BY-1 *);
    SYSTEM.PUT(lpGlob, ch);
  ELSE
    GlobWin.Beep;
  END (* IF inx<ListSt.MAXLENGTH *);
  lpGlob       := WinBase.GlobalUnlock(globMem);
  (* Sperren aufheben *)
END InsertChar;


(*****************************************************************************)
PROCEDURE CopyString*                 (globMem:            WinDef.HGLOBAL;
                                       txt:                ARRAY OF CHAR);
(* einen String samt Carriage Return in einen globalen Speicherbuffer kopieren *)
VAR
  lpGlob,
  len:                                 LONGINT;
BEGIN
  IF globMem=WinDef.NULL THEN
    GlobWin.Beep;
    RETURN ;
  END (* IF globMem=WinDef.NULL *);
  lpGlob       := WinBase.GlobalLock(globMem);
  (* Speicherbereich sperren *)
  ASSERT(lpGlob#WinDef.NULL);
  len          := Strings.Length(txt);
  IF len>ListSt.MAXLENGTH - 2 THEN
    len        := ListSt.MAXLENGTH - 2;
    GlobWin.Beep;
  END (* IF len>ListSt.MAXLENGTH - 2 *);
  SYSTEM.MOVE(SYSTEM.ADR(txt), lpGlob, len);
  INC(lpGlob, len);
  SYSTEM.PUT(lpGlob, 0DX);
  INC(lpGlob);
  SYSTEM.PUT(lpGlob, 0AX);
  INC(lpGlob);
  SYSTEM.PUT(lpGlob, 0X);
  lpGlob       := WinBase.GlobalUnlock(globMem);
  (* Sperren aufheben *)
END CopyString;


(*****************************************************************************)
(*****************************************************************************)
BEGIN
 ;
  ;
END GlobMem.



