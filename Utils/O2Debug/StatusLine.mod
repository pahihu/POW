(*****************************************************************************)
(*                                                                           *)
(* Project:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* Module:     StatusLine.mod                              V 1.42.01         *)
(*                                                         2002APR14         *)
(*  PURPOSE:   Anzeigen von Nachrichten in einer Fensterzeile                *)
(*             uses CommCTRL.StatusWindow                                    *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   Create    generates a Status Bar                                        *)
(*   SetText   displays text in the Status Bar                               *)
(*                         max length is 256 Char                            *)
(*   SetText1  displays text with a number                                   *)
(*   SetText1H displays text with a hex number                               *)
(*   SetText2  displays text with 2 numbers                                  *)
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

MODULE StatusLine;


IMPORT
  SYSTEM,
  Strings, 
  CommCTRL, WinBase, WinDef, WinNT, WinUser;
  
  
CONST
  Version*             =              "V 1.42.01";
  Module*              =              "StatusLine";
  
  NoticeField*         =               0;
  SecondField*         =               1;
  StatusField*         =               2;
  FourthField*         =               3;
  

VAR
  hWndBar*:                            WinDef.HWND;
  StatusBarRect:                       WinDef.RECT;
  StatusBarRegion:                     ARRAY 4 OF LONGINT;
  Result:                              WinDef.LRESULT;
  ResultBool:                          WinDef.BOOL;
  TextString:                          ARRAY 1024 OF CHAR;
  NumberString:                        ARRAY   32 OF CHAR;
  i, j:                                LONGINT;


(*****************************************************************************)
(*                                                                           *)
(* SetText                                                                   *)
(* sets text to be displayed in the status bar                               *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Field     number of segment to hold the text (starts with 0)            *)
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

PROCEDURE [_APICALL] SetText*         (Text:               ARRAY OF CHAR;
                                       Field:              LONGINT);

VAR    Result:                         WinDef.LRESULT;
       
BEGIN
  
  Result := WinUser.SendMessageA(hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 Field, 
                                 SYSTEM.ADR(Text));
  
END SetText;


(*****************************************************************************)
(*                                                                           *)
(* SetText1                                                                  *)
(* sets text including a number to be displayed in the status bar            *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number    number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*   Field     number of segment to hold the text (starts with 0)            *)
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

PROCEDURE [_APICALL] SetText1*         (Text:              ARRAY OF CHAR;
                                        Number:            LONGINT;
                                        Field:             LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  j := 0;
  FOR i:=0 TO (Strings.Length(Text)-1) DO;
    IF Text[i]="#" THEN
      TextString[j] := 0X;
      Strings.Str (Number, NumberString);
      Strings.Append (TextString, NumberString);
      j := j + Strings.Length(NumberString);
    ELSE
      TextString[j] := Text[i];
      INC(j);
    END;
  END;
  TextString[j] := 0X;
  
  Result := WinUser.SendMessageA(hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 Field, 
                                 SYSTEM.ADR(TextString));
  
END SetText1;


(*****************************************************************************)
(*                                                                           *)
(* SetText1H                                                                 *)
(* sets text including a number to be displayed in the status bar            *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number    number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*   Field     number of segment to hold the text (starts with 0)            *)
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

PROCEDURE [_APICALL] SetText1H*        (Text:              ARRAY OF CHAR;
                                        Number:            LONGINT;
                                        Field:             LONGINT);

VAR
  Result:                              WinDef.LRESULT;
       
BEGIN
  j := 0;
  FOR i:=0 TO (Strings.Length(Text)-1) DO;
    IF Text[i]="#" THEN
      TextString[j] := 0X;
      Strings.HexStr (Number, NumberString);
      Strings.Append (TextString, NumberString);
      j := j + Strings.Length(NumberString);
    ELSE
      TextString[j] := Text[i];
      INC(j);
    END;
  END;
  TextString[j] := 0X;
  
  Result := WinUser.SendMessageA(hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 Field, 
                                 SYSTEM.ADR(TextString));
  
END SetText1H;


(*****************************************************************************)
(*                                                                           *)
(* SetText2                                                                  *)
(* sets text including two numbers to be displayed in the status bar         *)
(*                                                                           *)
(* Input:                                                                    *)
(*   Text      text that is to be displayed                                  *)
(*   Number1                                                                 *)
(*   Number2   number to be included into the text                           *)
(*             position marked with "#"                                      *)
(*   Field     number of segment to hold the text (starts with 0)            *)
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

PROCEDURE [_APICALL] SetText2*         (Text:              ARRAY OF CHAR;
                                        Number1, Number2:  LONGINT;
                                        Field:             LONGINT);

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
  
  Result := WinUser.SendMessageA(hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 Field, 
                                 SYSTEM.ADR(TextString));
  
END SetText2;


(*****************************************************************************)
(*                                                                           *)
(* DisplayError                                                              *)
(* Displays an error message and error text in the status line               *)
(*                                                                           *)
(* Input:                                                                    *)
(*   WinError  error code                                                    *)
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

PROCEDURE [_APICALL] DisplayError*    (ErrorCode:          LONGINT);

VAR
  ErrorMessage:                        ARRAY 1024 OF CHAR;
  MyChars:                             ARRAY   16 OF CHAR;
  Result:                              WinDef.LRESULT;
  
BEGIN
  
  IF ErrorCode=0 THEN
    COPY("No Error.", ErrorMessage);
    RETURN
  END (* IF ErrorCode=0 *);

  Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                                         0,
                                         ErrorCode,
                                         SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                                         SYSTEM.ADR(ErrorMessage),
                                         LEN(ErrorMessage),
                                         0);
  IF Result=0 THEN 
    COPY("No message for error code ", ErrorMessage);
    Strings.Str(ErrorCode, MyChars);
    Strings.Append(ErrorMessage, MyChars); 
    Strings.Append(ErrorMessage, " available.");
  ELSE
    Strings.Str(ErrorCode, MyChars);
    Strings.Append(MyChars, ": "); 
    Strings.Insert(MyChars, ErrorMessage, 1);
  END;

  Result := WinUser.SendMessageA(hWndBar, 
                                 CommCTRL.SB_SETTEXT, 
                                 NoticeField, 
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
  
  (* Get the coordinates of the parent window's client area. *)
  Result   := WinUser.GetClientRect(hWndParent, StatusBarRect); 
  IF Result=0 THEN
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
  Width                := StatusBarRect.right DIV 10;
  StatusBarRegion[0]   := 5*Width;
  StatusBarRegion[1]   := 7*Width;
  StatusBarRegion[2]   := 9*Width;
  StatusBarRegion[3]   := StatusBarRect.right;

  hWndBar  := CommCTRL.CreateStatusWindowA(
                       CommCTRL.SBARS_SIZEGRIP + WinUser.WS_CHILD + WinUser.WS_VISIBLE,
                       SYSTEM.ADR("Running..."),
                       hWndParent,
                       3);

  IF hWndBar=0 THEN
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                                         0,
                                         WinBase.GetLastError(),
                                         SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                                         SYSTEM.ADR(TextString),
                                         LEN(TextString),
                                         0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
  END;
 
  (* Tell the status bar to create the window parts. CommCTRL.SB_SETPARTS *)
  Result   := WinUser.SendMessageA(hWndBar, CommCTRL.SB_SETPARTS, 4, SYSTEM.ADR(StatusBarRegion)); 
  IF Result=0 THEN
    Result       := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                                         0,
                                         WinBase.GetLastError(),
                                         SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                                         SYSTEM.ADR(TextString),
                                         LEN(TextString),
                                         0);
    Result := WinUser.MessageBoxA(hWndParent, SYSTEM.ADR(TextString), SYSTEM.ADR("Error Message"), 0);
  END;

END Create;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN;

  CommCTRL.InitCommonControls();
  
END StatusLine.

