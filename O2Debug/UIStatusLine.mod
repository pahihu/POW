(*****************************************************************************)
(*                                                                           *)
(* Project:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* Module:     UIStatusLine                                V 1.42.20         *)
(*                                                         2003APR04         *)
(*  PURPOSE:   implements the statusline displaying general messages         *)
(*             uses CommCTRL.StatusWindow                                    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   ShowMessage                                                             *)
(*             Displays text in the Status Bar                               *)
(*                         max length is 256 Char                            *)
(*   ShowMessage1                                                            *)
(*             Displays text with a number                                   *)
(*   ShowMessage1Hex                                                         *)
(*             Displays text with a hex number                               *)
(*   ShowMessage2                                                            *)
(*             Displays text with 2 numbers                                  *)
(*   ShowPosition                                                            *)
(*   ShowMode                                                                *)
(*   ShowStatus                                                              *)
(*   DisplayError                                                            *)
(*   Create    generates a Status Bar                                        *)
(*                                                                           *)
(*                                                                           *)
(* Copyright:  Klaus Schultze                                                *)
(*             Kamillenweg 15; 24217 Schönberg             Tel. 04344 1445   *)
(*             e-Mail: schultze-schoenberg@t-online.de                       *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*   created   Peter René Dietmüller, dietmueller@fim.uni-linz.ac.at (PDI)   *)
(*   2000APR20 Programm übernommen von POW! CD                               *)
(*             Portierung auf POW! 32                                        *)
(*                                                                           *)
(*   update                                                                  *)
(*   2000SEP24 Routinen aus CommCTRL benutzt, Funktionen reduziert           *)
(*   2001SEP15 now the statusbar has some fields, thanks to Steven           *)
(*                                                                           *)
(*   release                                                                 *)
(*                                                                           *)
(*****************************************************************************)

MODULE UIStatusLine;


IMPORT
  Strings, 
  CommCTRL, WinBase, WinDef, WinNT, WinUser,
  SYSTEM;
  
  
CONST
  Version*             =              "V 1.42.20";
  Module*              =              "UIStatusLine";
  
  NoticeField*         =               5;
  PositionField*       =               1;
  StatusField*         =               3;
  ModeField*           =               2;
  
  StatusBarDim         =               6;
  

VAR
  hWndStatusBar*:                      WinDef.HWND;

  StatusBarRect:                       WinDef.RECT;
  StatusBarRegion,
  StatusBarAttribute:                  ARRAY StatusBarDim OF LONGINT;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  TextString:                          ARRAY 1024 OF CHAR;
  NumberString:                        ARRAY   32 OF CHAR;
  i, j:                                LONGINT;


(*****************************************************************************)
(*                                                                           *)
(* ShowMessage                                                               *)
(* sets text to be displayed in the status bar's notice field                *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowMessage*                (Text:               ARRAY OF CHAR);

VAR    Result:                         WinDef.LRESULT;
       
BEGIN
  
  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 NoticeField + StatusBarAttribute[NoticeField], 
                                 SYSTEM.ADR(Text));
  
END ShowMessage;


(*****************************************************************************)
(*                                                                           *)
(* ShowMessage1                                                              *)
(* sets text including a number to be displayed in the status bar's Notice   *)
(* Field                                                                     *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number    number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowMessage1*                (Text:              ARRAY OF CHAR;
                                        Number:            LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  j := 0;
  FOR i:=0 TO (Strings.Length(Text)-1) DO;
    IF Text[i]="#" THEN
      TextString[j]  := 0X;
      Strings.Str (Number, NumberString);
      Strings.Append (TextString, NumberString);
      j              := j + Strings.Length(NumberString);
    ELSE
      TextString[j]  := Text[i];
      INC(j);
    END;
  END;
  TextString[j]    := 0X;
  
  Result           := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 NoticeField + StatusBarAttribute[NoticeField], 
                                 SYSTEM.ADR(TextString));
  
END ShowMessage1;


(*****************************************************************************)
(*                                                                           *)
(* ShowMessage1Hex                                                           *)
(* sets text including a number to be displayed in the status bar's Notice   *)
(* Field                                                                     *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number    number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowMessage1Hex*             (Text:              ARRAY OF CHAR;
                                        Number:            LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  j := 0;
  FOR i:=0 TO (Strings.Length(Text)-1) DO;
    IF Text[i]="#" THEN
      TextString[j]  := 0X;
      Strings.HexStr (Number, NumberString);
      Strings.Append (TextString, NumberString);
      j              := j + Strings.Length(NumberString);
    ELSE
      TextString[j]  := Text[i];
      INC(j);
    END;
  END;

  TextString[j]    := 0X;
  
  Result           := WinUser.SendMessageA(hWndStatusBar, 
                                           CommCTRL.SB_SETTEXT, 
                                           NoticeField + StatusBarAttribute[NoticeField], 
                                           SYSTEM.ADR(TextString));
  
END ShowMessage1Hex;


(*****************************************************************************)
(*                                                                           *)
(* ShowMessage2                                                              *)
(* sets text including two numbers to be displayed in the status bar's       *)
(* Notice Field                                                                          *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number1                                                                 *)
(*   Number2   number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowMessage2*                (Text:              ARRAY OF CHAR;
                                        Number1, Number2:  LONGINT);

VAR
  Result:                              WinDef.LRESULT;
  k:                                   INTEGER;
  Number:                              ARRAY 2 OF LONGINT;
       
BEGIN
  j := 0;
  Number[0] := Number1;
  Number[1] := Number2;
  
  k := 0;
  FOR i:=0 TO (Strings.Length(Text)-1) DO;
    IF Text[i]="#" THEN
      TextString[j] := 0X;
      Strings.Str (Number[k], NumberString);
      Strings.Append (TextString, NumberString);
      j := j + Strings.Length(NumberString);
      IF k<(LEN(Number)-1) THEN
        INC(k);
      END;
    ELSE
      TextString[j] := Text[i];
      INC(j);
    END;
  END;
  TextString[j]    := 0X;
  
  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 NoticeField + StatusBarAttribute[NoticeField], 
                                 SYSTEM.ADR(TextString));
  
END ShowMessage2;


(*****************************************************************************)
(*                                                                           *)
(* ShowPosition                                                              *)
(* fills the position part of the statusbar                                  *)
(*                                                                           *)
(* Input:                                                                    *)
(*   cx        actual line number                                            *)
(*   cy        actual column number                                          *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowPosition*               (cx, cy:            LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  
  TextString[0]    := 09X;
  TextString[1]    :=  0X;
  Strings.Str(cx, NumberString);
  Strings.Append(TextString, NumberString);
  Strings.Append(TextString, ":");
  Strings.Str(cy, NumberString);
  Strings.Append(TextString, NumberString);
  
  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 PositionField + StatusBarAttribute[PositionField], 
                                 SYSTEM.ADR(TextString));
  
END ShowPosition;


(*****************************************************************************)
(*                                                                           *)
(* ShowMode                                                                  *)
(* fills the mode part of the statusbar                                      *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Mode      Index of mode table                                           *)
(*             0   ---                                                       *)
(*             1   Dump                                                      *)
(*             2   Debug                                                     *)
(*                 Unknown                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowMode*                   (Mode:               LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  
  TextString[0]    := 09X;
  TextString[1]    :=  0X;
  CASE Mode OF
    0:
      Strings.Append(TextString, "---");
    |
    1:
      Strings.Append(TextString, "Dump");
    |
    2:
      Strings.Append(TextString, "Debug");
    ELSE
      Strings.Append(TextString, "Unknown");
  END (* CASE Mode  *);
  
  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 ModeField + StatusBarAttribute[ModeField], 
                                 SYSTEM.ADR(TextString));
  
END ShowMode;


(*****************************************************************************)
(*                                                                           *)
(* ShowStatus                                                                *)
(* fills the status part of the statusbar                                    *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Mode      Index of mode table                                           *)
(*             0   ---                                                       *)
(*             1                                                             *)
(*             2   Modified                                                  *)
(*                 Unknown                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE ShowStatus*                 (Status:             LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  
  TextString[0]    := 09X;
  TextString[1]    :=  0X;
  CASE Status OF
    0:
      Strings.Append(TextString, "---");
    |
    1:
      Strings.Append(TextString, "");
    |
    2:
      Strings.Append(TextString, "Modified");
    ELSE
      Strings.Append(TextString, "Unknown");
  END (* CASE Mode  *);
  
  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 StatusField + StatusBarAttribute[StatusField], 
                                 SYSTEM.ADR(TextString));
  
END ShowStatus;


(*****************************************************************************)
(*                                                                           *)
(* DisplayError                                                              *)
(* Displays an error message and error text in the status line               *)
(*                                                                           *)
(* Input:                                                                    *)
(*   ErrorCode error code                                                    *)
(*   Location  a number used to identify the positoin (in source code) where *)
(*             the error occurred:                                           *)
(*             digits 1-3      designate the position within the module      *)
(*             digits 4-6      designate the module                          *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*   Values are 32 bit values layed out as follows:                          *)
(*   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1                             *)
(*   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0         *)
(*  +---+-+-+-----------------------+-------------------------------+        *)
(*  |Sev|C|R|     Facility          |               Code            |        *)
(*  +---+-+-+-----------------------+-------------------------------+        *)
(*  where                                                                    *)
(*     Sev - is the severity code                                            *)
(*         00 - Success                                                      *)
(*         01 - Informationa                                                 *)
(*         10 - Warning                                                      *)
(*         11 - Error                                                        *)
(*     C - is the Customer code flag                                         *)
(*     R - is a reserved bit                                                 *)
(*     Facility - is the facility code                                       *)
(*     Code - is the facility's status code                                  *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE DisplayError*               (ErrorCode:          LONGINT;
                                       Location:           LONGINT);

VAR
  ErrorString,
  ErrorMessage:                        ARRAY 1024 OF CHAR;
  MyChars:                             ARRAY   16 OF CHAR;
  Result:                              WinDef.LRESULT;
  
BEGIN
  
  IF ErrorCode=0 THEN
(*    COPY("No Error.", ErrorMessage); *)
    RETURN
  END (* IF ErrorCode=0 *);

  Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               ErrorCode,
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(ErrorString),
                               LEN(ErrorString),
                               0);
  Strings.Str (Location DIV 1000, ErrorMessage);
  Strings.Append(ErrorMessage, ".");
  Strings.Str (Location MOD 1000, MyChars);
  WHILE Strings.Length(MyChars)<3 DO
    Strings.Insert("0", MyChars, 0);
  END (* WHILE Strings.Length(ErrorMessage)<3  *);
  Strings.Append(ErrorMessage, MyChars);
  Strings.Append(ErrorMessage, "; ");
  IF Result=0 THEN 
    Strings.Append(ErrorMessage, "No message for error code ");
    Strings.Str(ErrorCode, MyChars);
    Strings.Append(ErrorMessage, MyChars); 
    Strings.Append(ErrorMessage, " available.");
  ELSE
    Strings.Append(ErrorMessage, "Err ");
    Strings.Str(ErrorCode, MyChars);
    Strings.Append(ErrorMessage, MyChars); 
    Strings.Append(ErrorMessage, ": "); 
    Strings.Append(ErrorMessage, ErrorString);
  END;

  Result := WinUser.SendMessageA(hWndStatusBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 NoticeField + StatusBarAttribute[NoticeField], 
                                 SYSTEM.ADR(ErrorMessage));

END DisplayError;



(*****************************************************************************)
(*                                                                           *)
(* Create                                                                    *)
(* creates a status bar as child window of the window hWndParent             *)
(*                                                                           *)
(* Input:                                                                    *)
(*   hWndParent                                                              *)
(*             window handle of parent window                                *)
(*   hInstanceParent                                                         *)
(*             instance handle of the module that wants to create a          *)
(*             status bar.                                                   *)
(*                                                                           *)
(* Output:                                                                   *)
(*                                                                           *)
(* PRECONDITIONS:                                                            *)
(*             The parent window for the status bar must exist.              *)
(*                                                                           *)
(* POSTCONDITIONS:                                                           *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE [_APICALL] Create*          (hWndParent:         WinDef.HWND;
                                       hInstanceParent:    WinDef.HWND);
                                      
VAR
  Error:                               WinDef.DWORD;
  Width:                               LONGINT;
  Number:                              ARRAY 80 OF CHAR;
                                       
BEGIN
  
  hWndStatusBar      := WinUser.CreateWindowExA(WinUser.WS_EX_WINDOWEDGE,
                               SYSTEM.ADR("msctls_statusbar32"),   (* See RegisterClass() call.          *)
                               WinDef.NULL                 ,   (* no Text for Window title bar.      *)
                                                               (* Window style.                      *)
                               CommCTRL.SBARS_SIZEGRIP + WinUser.WS_CHILD + WinUser.WS_BORDER + WinUser.WS_VISIBLE,
                               WinUser.CW_USEDEFAULT,          (* Default horizontal position.       *)
                               WinUser.CW_USEDEFAULT,          (* Default vertical position.         *)
                               WinUser.CW_USEDEFAULT,          (* Default width.                     *)
                               WinUser.CW_USEDEFAULT,          (* Default height.                    *)
                               hWndParent,                     (* Overlapped Windows have no parent  *)
                               WinDef.NULL,                    (* Use the Window class menu.         *)
                               hWndParent,
                               3);
  IF hWndStatusBar=0 THEN
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
  END;
  
  (* Calculate the right edge coordinate for each part,  *)
  (* and copy the coordinates to the array.              *)
  StatusBarRegion[0]   :=   5;
  StatusBarRegion[1]   :=  80;
  StatusBarRegion[2]   := 155;
  StatusBarRegion[3]   := 230;
  StatusBarRegion[4]   := 235;
  StatusBarRegion[5]   :=  -1;

  (* Tell the status bar to create the window parts. CommCTRL.SB_SETPARTS *)
  Result   := WinUser.SendMessageA(hWndStatusBar, CommCTRL.SB_SETPARTS, StatusBarDim, SYSTEM.ADR(StatusBarRegion)); 
  IF Result=0 THEN
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                               0,
                               WinBase.GetLastError(),
                               SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                               SYSTEM.ADR(TextString),
                               LEN(TextString),
                               0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
  END (* IF Result=0 *);

  FOR i:=0 TO StatusBarDim-1 DO
    Result := WinUser.SendMessageA(hWndStatusBar, 
                               CommCTRL.SB_SETTEXT, 
                               i + StatusBarAttribute[i], 
                               SYSTEM.ADR("   "));
  END (* FOR i:=0 TO StatusBarDim-1  *);
  ShowMessage("Running...");
  ShowPosition(0, 0);
  ShowMode(0);
  ShowStatus(0);
 
END Create;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  StatusBarAttribute[0]        := CommCTRL.SBT_NOBORDERS;
  StatusBarAttribute[1]        := 0;
  StatusBarAttribute[2]        := 0;
  StatusBarAttribute[3]        := 0;
  StatusBarAttribute[4]        := CommCTRL.SBT_NOBORDERS;
  StatusBarAttribute[5]        := CommCTRL.SBT_NOBORDERS;
  
END UIStatusLine.

