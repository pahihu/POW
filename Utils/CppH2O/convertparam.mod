(*****************************************************************************)
(*                                                                           *)
(* PROJECT     Convert Cpp Header Into Oberon-2 Definition Module            *)
(*                                                                           *)
(* MODULE      ConvertParam                                V 1.40.04         *)
(*                                                         2001NOV05         *)
(*  PURPOSE:   parameter translation                                         *)
(*             holds an extendable list which contains names and the         *)
(*             associated qualifiers (a file name)                           *)
(*             this list can be read from an ASCII file                      *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   GetParam  input is a name, the procedure returns the qualified name     *)
(*   PutParam  appends a new qualified item to the list                      *)
(*   Open      reads the names and the corresponding qualified names from    *)
(*             an ASCII file   "OberonMod.prm"                               *)
(*   Close     writes all names and qualified names to an ASCII file         *)
(*             "OberonMod.prm"                                               *)
(*                                                                           *)
(* COMMENTS:   the file "OberonMod.prm" format is as follows:                *)
(*             [<filename0>]                                                 *)
(*             <name01>        gives <filename0>.<name01>                    *)
(*             <name02>        gives <filename0>.<name02>                    *)
(*             ...                                                           *)
(*             [<filenamei>]                                                 *)
(*             <nameii>        gives <filenamei>.<nameii>                    *)
(*             ...                                                           *)
(*                                                                           *)
(*             the parameters in memory are configured as follows:           *)
(*             param01 equivalent01                                          *)
(*             param02 equivalent02                                          *)
(*             param03 equivalent03                                          *)
(*             ...                                                           *)
(*             parami1 SYSTEM.equivalent01                                   *)
(*             parami2 SYSTEM.equivalent02                                   *)
(*             ...                                                           *)
(*             paramj1 <filename01>.<name01>                                 *)
(*             paramj2 <filename01>.<name02>                                 *)
(*             ...                                                           *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    schultze-schoenberg@t-online.de                               *)
(*                                                                           *)
(* Configuration Management                                                  *)
(*                                                                           *)
(*  created    2000DEC31                                                     *)
(*                                                                           *)
(*  update                                                                   *)
(*   2000NOV08                                                               *)
(*   2000SEP16 KlS     change request 01.001 fixed                           *)
(*                     short description                                     *)
(*                                                                           *)
(*  release                                                                  *)
(*                                                                           *)
(*****************************************************************************)

MODULE ConvertParam;

IMPORT  
  Strings, SYSTEM,
  WinDef, WinBase, WinNT;
  

CONST
  TokenLength*         =               128;
  

TYPE
  TokenString*         =               ARRAY TokenLength OF CHAR;
  TokenPair    = RECORD
                   Index,
                   Value:              TokenString
                 END (* TokenPair *);              
  TokenElementPtr      =               POINTER TO TokenElement;
  TokenElement = RECORD
                   Token:              TokenPair;
                   NextToken:          TokenElementPtr
                 END (* TokenElement *);


VAR
  AnchorTokenPtr,
  FirstTokenPtr,
  NextTokenPtr:                        TokenElementPtr;
  ParamFile:                           WinDef.HANDLE;
  ModuleFileName,
  ParamFileName:                       ARRAY 132 OF CHAR;
  File_Security:                       WinBase.SECURITY_ATTRIBUTES;
  OpenBracket, CloseBracket,
  CrLf:                                ARRAY 3 OF CHAR;

  

(*****************************************************************************)
(*                                                                           *)
(* GetParam                                                                  *)
(* searches in TranslationTable whether there exists a conversion            *)
(* for StringIn                                                              *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  StringIn   the string to be looked up                                    *)
(*  StringOut  the converted string, if no string is found,                  *)
(*             StringOut=StringIn                                            *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  BOOLEAN    TRUE                                                          *)
(*             the string was found in the table, StringOut holds the        *)
(*             translation                                                   *)
(*             FALSE                                                         *)
(*             the string was not found, StringOut contains StringIn         *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE GetParam*                   (VAR StringIn-:      ARRAY OF CHAR;
                                       VAR StringOut:      ARRAY OF CHAR)
                                      :                    BOOLEAN;
                                      
VAR 
  BytesRead:                           LONGINT;
  Found:                               BOOLEAN;
  MyToken:                             TokenElementPtr;

  (***************************************************************************)
  PROCEDURE CompareStrings             (VAR String1-:      ARRAY OF CHAR;
                                        VAR String2-:      ARRAY OF CHAR)
                                       :                   BOOLEAN;
  VAR
    Index:                             LONGINT;
  
  BEGIN;
    IF Strings.Length(String1)#Strings.Length(String2) THEN
      RETURN FALSE;
    END;
    
    FOR Index:=0 TO (Strings.Length(String1)-1) DO
      IF String1[Index]#String2[Index] THEN
        RETURN FALSE
      END;
    END;
    
    RETURN TRUE
  END CompareStrings;


BEGIN

  COPY(StringIn, StringOut);
  
  MyToken  := AnchorTokenPtr;

  IF CompareStrings(MyToken^.Token.Index, StringIn) THEN
    COPY(MyToken^.Token.Value, StringOut);
    RETURN TRUE
  END (* IF CompareStrings(MyToken^.Token.Index, StringIn) *);

  WHILE MyToken^.NextToken#NIL DO
    MyToken := MyToken^.NextToken;
    IF CompareStrings(MyToken^.Token.Index, StringIn) THEN
      COPY(MyToken^.Token.Value, StringOut);
      RETURN TRUE
    END (* IF CompareStrings(MyToken^.Token.Index, StringIn) *);
  END;
  
  RETURN FALSE;
  
END GetParam;


(*****************************************************************************)
(*                                                                           *)
(* PutParam                                                                  *)
(* stores the converted string and its associated "index" in the             *)
(* TranslationTable                                                          *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  StringIn   the index                                                     *)
(*  StringOut  the converted string                                          *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  BOOLEAN    TRUE                                                          *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE PutParam*                   (VAR StringIn-:      ARRAY OF CHAR;
                                       VAR StringOut-:     ARRAY OF CHAR)
                                      :                    BOOLEAN;
                                      
VAR 
  MyToken:                             TokenElementPtr;
  
BEGIN
  
  MyToken  := AnchorTokenPtr;

  WHILE MyToken^.NextToken#NIL DO
    MyToken := MyToken^.NextToken;
  END (* WHILE ... DO *);

  NEW(NextTokenPtr);  
  COPY(StringIn,  NextTokenPtr^.Token.Index);
  COPY(StringOut, NextTokenPtr^.Token.Value);
  MyToken^.NextToken       := NextTokenPtr;
  NextTokenPtr^.NextToken  := NIL;
  
  RETURN TRUE
  
END PutParam;


(*****************************************************************************)
(*                                                                           *)
(* SetDefault                                                                *)
(* fills TranslationTable with default values (WinDef.xxxxxx)                *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE SetDefault                  ();
  
BEGIN

  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "WINVER";
  NextTokenPtr^.Token.Value  := "WinDef.WINVER";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "MAX_PATH";
  NextTokenPtr^.Token.Value  := "WinDef.MAX_PATH";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "False";
  NextTokenPtr^.Token.Value  := "WinDef.False";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "True";
  NextTokenPtr^.Token.Value  := "WinDef.True";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "NULL";
  NextTokenPtr^.Token.Value  := "WinDef.NULL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_UPDATE";
  NextTokenPtr^.Token.Value  := "WinDef.DM_UPDATE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_OUT_DEFAULT";
  NextTokenPtr^.Token.Value  := "WinDef.DM_OUT_DEFAULT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_COPY";
  NextTokenPtr^.Token.Value  := "WinDef.DM_COPY";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_OUT_BUFFER";
  NextTokenPtr^.Token.Value  := "WinDef.DM_OUT_BUFFER";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_PROMPT";
  NextTokenPtr^.Token.Value  := "WinDef.DM_PROMPT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_IN_PROMPT"; 
  NextTokenPtr^.Token.Value  := "WinDef.DM_IN_PROMPT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_MODIFY";
  NextTokenPtr^.Token.Value  := "WinDef.DM_MODIFY";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DM_IN_BUFFER";
  NextTokenPtr^.Token.Value  := "WinDef.DM_IN_BUFFER";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_FIELDS";
  NextTokenPtr^.Token.Value  := "WinDef.DC_FIELDS";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_PAPERS"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_PAPERS";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_PAPERSIZE"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_PAPERSIZE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_MINEXTENT"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_MINEXTENT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_MAXEXTENT"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_MAXEXTENT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_BINS"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_BINS";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_DUPLEX"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_DUPLEX";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_SIZE"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_SIZE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_EXTRA"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_EXTRA";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_VERSION"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_VERSION";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_DRIVER"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_DRIVER";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_BINNAMES"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_BINNAMES";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_ENUMRESOLUTIONS"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_ENUMRESOLUTIONS";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_FILEDEPENDENCIES"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_FILEDEPENDENCIES";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_TRUETYPE"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_TRUETYPE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_PAPERNAMES"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_PAPERNAMES";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_ORIENTATION"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_ORIENTATION";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DC_COPIES"; 
  NextTokenPtr^.Token.Value  := "WinDef.DC_COPIES";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HFILE_ERROR"; 
  NextTokenPtr^.Token.Value  := "WinDef.HFILE_ERROR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "ULONG"; 
  NextTokenPtr^.Token.Value  := "WinDef.ULONG";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PULONG"; 
  NextTokenPtr^.Token.Value  := "WinDef.PULONG";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "USHORT"; 
  NextTokenPtr^.Token.Value  := "WinDef.USHORT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PUSHORT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PUSHORT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "UCHAR"; 
  NextTokenPtr^.Token.Value  := "WinDef.UCHAR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PUCHAR"; 
  NextTokenPtr^.Token.Value  := "WinDef.PUCHAR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PSZ"; 
  NextTokenPtr^.Token.Value  := "WinDef.PSZ";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "DWORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.DWORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "BOOL"; 
  NextTokenPtr^.Token.Value  := "WinDef.BOOL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "BYTE"; 
  NextTokenPtr^.Token.Value  := "WinDef.BYTE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "INT"; 
  NextTokenPtr^.Token.Value  := "WinDef.INT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "WORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.WORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PFLOAT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PFLOAT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PBOOL"; 
  NextTokenPtr^.Token.Value  := "WinDef.PBOOL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPBOOL"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPBOOL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PBYTE"; 
  NextTokenPtr^.Token.Value  := "WinDef.PBYTE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPBYTE"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPBYTE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PWORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.PWORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPWORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPWORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPLONG"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPLONG";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PDWORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.PDWORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPDWORD"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPDWORD";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPVOID"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPVOID";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCVOID"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCVOID";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "UINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.UINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PUINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PUINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "WCHAR"; 
  NextTokenPtr^.Token.Value  := "WinDef.WCHAR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LP"; 
  NextTokenPtr^.Token.Value  := "WinDef.LP";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.PSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPWSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPWSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCWSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCWSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "WPARAM"; 
  NextTokenPtr^.Token.Value  := "WinDef.WPARAM";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPARAM"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPARAM";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LRESULT"; 
  NextTokenPtr^.Token.Value  := "WinDef.LRESULT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HANDLE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HANDLE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HWND"; 
  NextTokenPtr^.Token.Value  := "WinDef.HWND";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HHOOK"; 
  NextTokenPtr^.Token.Value  := "WinDef.HHOOK";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "ATOM"; 
  NextTokenPtr^.Token.Value  := "WinDef.ATOM";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "SPHANDLE"; 
  NextTokenPtr^.Token.Value  := "WinDef.SPHANDLE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPHANDLE"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPHANDLE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HGLOBAL"; 
  NextTokenPtr^.Token.Value  := "WinDef.HGLOBAL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HLOCAL"; 
  NextTokenPtr^.Token.Value  := "WinDef.HLOCAL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "GLOBALHANDLE"; 
  NextTokenPtr^.Token.Value  := "WinDef.GLOBALHANDLE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LOCALHANDLE"; 
  NextTokenPtr^.Token.Value  := "WinDef.LOCALHANDLE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "FARPROC"; 
  NextTokenPtr^.Token.Value  := "WinDef.FARPROC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "NEARPROC"; 
  NextTokenPtr^.Token.Value  := "WinDef.NEARPROC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PROC"; 
  NextTokenPtr^.Token.Value  := "WinDef.PROC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HGDIOBJ"; 
  NextTokenPtr^.Token.Value  := "WinDef.HGDIOBJ";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HACCEL"; 
  NextTokenPtr^.Token.Value  := "WinDef.HACCEL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HBITMAP"; 
  NextTokenPtr^.Token.Value  := "WinDef.HBITMAP";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HBRUSH"; 
  NextTokenPtr^.Token.Value  := "WinDef.HBRUSH";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HCOLORSPACE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HCOLORSPACE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HDC"; 
  NextTokenPtr^.Token.Value  := "WinDef.HDC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HGLRC"; 
  NextTokenPtr^.Token.Value  := "WinDef.HGLRC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HDESK"; 
  NextTokenPtr^.Token.Value  := "WinDef.HDESK";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HENHMETAFILE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HENHMETAFILE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HFONT"; 
  NextTokenPtr^.Token.Value  := "WinDef.HFONT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HICON"; 
  NextTokenPtr^.Token.Value  := "WinDef.HICON";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HMENU"; 
  NextTokenPtr^.Token.Value  := "WinDef.HMENU";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HMETAFILE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HMETAFILE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HINSTANCE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HINSTANCE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HMODULE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HMODULE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HPALETTE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HPALETTE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HPEN"; 
  NextTokenPtr^.Token.Value  := "WinDef.HPEN";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HRGN"; 
  NextTokenPtr^.Token.Value  := "WinDef.HRGN";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HRSRC"; 
  NextTokenPtr^.Token.Value  := "WinDef.HRSRC";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HSTR"; 
  NextTokenPtr^.Token.Value  := "WinDef.HSTR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HTASK"; 
  NextTokenPtr^.Token.Value  := "WinDef.HTASK";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HWINSTA"; 
  NextTokenPtr^.Token.Value  := "WinDef.HWINSTA";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HKL"; 
  NextTokenPtr^.Token.Value  := "WinDef.HKL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HFILE"; 
  NextTokenPtr^.Token.Value  := "WinDef.HFILE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "HCURSOR"; 
  NextTokenPtr^.Token.Value  := "WinDef.HCURSOR";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "COLORREF"; 
  NextTokenPtr^.Token.Value  := "WinDef.COLORREF";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCOLORREF"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCOLORREF";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PRECT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PRECT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "NPRECT"; 
  NextTokenPtr^.Token.Value  := "WinDef.NPRECT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPRECT"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPRECT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCRECT"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCRECT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PRECTL"; 
  NextTokenPtr^.Token.Value  := "WinDef.PRECTL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPRECTL"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPRECTL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPCRECTL"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPCRECTL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PPOINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.PPOINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "NPPOINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.NPPOINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPPOINT"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPPOINT";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PPOINTL"; 
  NextTokenPtr^.Token.Value  := "WinDef.PPOINTL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PSIZE"; 
  NextTokenPtr^.Token.Value  := "WinDef.PSIZE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPSIZE"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPSIZE";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PSIZEL"; 
  NextTokenPtr^.Token.Value  := "WinDef.PSIZEL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPSIZEL"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPSIZEL";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "PPOINTS"; 
  NextTokenPtr^.Token.Value  := "WinDef.PPOINTS";
  FirstTokenPtr              := NextTokenPtr;
  
  NEW(NextTokenPtr);
  FirstTokenPtr^.NextToken   := NextTokenPtr;  
  NextTokenPtr^.Token.Index  := "LPPOINTS"; 
  NextTokenPtr^.Token.Value  := "WinDef.LPPOINTS";
  NextTokenPtr^.NextToken    := NIL;
  
END SetDefault;


(*****************************************************************************)
(*                                                                           *)
(* Open                                                                      *)
(* fills TranslationTable from the parameter file, which contains the fully  *)
(* qualified items                                                           *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*  FileName                                                                 *)
(*  ModuleName                                                               *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LONGINT    0               Operation was successfull                     *)
(*             otherwise       WINDOWS Error Code                            *)  
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Open*                       (FileName:           ARRAY OF CHAR;
                                       ModuleName:         ARRAY OF CHAR)
                                      :                    LONGINT;

VAR
  ResultBool:                          WinDef.BOOL;
  Result,
  BytesRead:                           LONGINT;
  Position:                            LONGINT;
  Qualifier,
  QualifierUpperCase,
  TokenQualifier:                      TokenString;
  Done:                                BOOLEAN;
  
BEGIN;
  COPY (FileName, ParamFileName); 
  COPY (ModuleName, ModuleFileName);
  Strings.UpCase(ModuleFileName);
  ParamFile := WinBase.CreateFileA    (SYSTEM.ADR(FileName),
                                       WinNT.GENERIC_READ,
                                       0,
                                       File_Security,
                                       WinBase.OPEN_EXISTING,
                                       WinNT.FILE_ATTRIBUTE_NORMAL,
                                       0);
  IF ParamFile=-1 THEN                                     (* error during file open *)
    SetDefault();
  ELSE                                                     (* read file and load qualifier *)
    Done       := FALSE;
    Qualifier  := "SYSTEM";
    REPEAT
      (* read line *)
      Position := -1;
      REPEAT
        INC(Position);
        ResultBool   := WinBase.ReadFile(ParamFile,
                                      SYSTEM.ADR(TokenQualifier[Position]),
                                      1,
                                      BytesRead,
                                      0);
      UNTIL ((BytesRead=0) OR (TokenQualifier[Position]=0DX));
      ResultBool   := WinBase.ReadFile(ParamFile,
                                    SYSTEM.ADR(TokenQualifier[Position]),
                                    1,
                                    BytesRead,
                                    0);
      TokenQualifier[Position] := 0X;
      IF BytesRead>0 THEN
        IF TokenQualifier[0]="[" THEN
          Strings.Delete(TokenQualifier, 1, 1);
          Strings.Copy(TokenQualifier, Qualifier, 1, Strings.Length(TokenQualifier)-1);
          COPY(Qualifier, QualifierUpperCase);
          Strings.UpCase(QualifierUpperCase);
        ELSE
          IF QualifierUpperCase#ModuleFileName THEN
            NEW(NextTokenPtr^.NextToken);
            NextTokenPtr   := NextTokenPtr^.NextToken;  
            COPY(TokenQualifier, NextTokenPtr^.Token.Index);
            Strings.InsertChar(".", TokenQualifier, 1);
            Strings.Insert(Qualifier, TokenQualifier, 1);
            COPY(TokenQualifier, NextTokenPtr^.Token.Value);
            FirstTokenPtr              := NextTokenPtr;
          END (* IF Qualifier#ModuleFileName *);
        END (* IF TokenQualifier[0]="[" *);
      ELSE
        NextTokenPtr^.NextToken    := NIL;                 (* Done *)
        Done := TRUE;
      END (* IF BytesRead>0 *);
      
    UNTIL Done;
  END (* IF ParamFile=-1  *);
  
  ResultBool := WinBase.CloseHandle(ParamFile);
  COPY (ModuleName, ModuleFileName);
  
  RETURN 0
  
END Open;


(*****************************************************************************)
(*                                                                           *)
(* Close                                                                     *)
(* writes all entries from TranslationTable into patameter file              *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)

PROCEDURE Close*                      ();

VAR
  ResultBool:                          WinDef.BOOL;
  BytesWritten:                        LONGINT;
  Position:                            LONGINT;
  Qualifier,
  TokenQualifier:                      TokenString;
  Done:                                BOOLEAN;
  
BEGIN;

  ParamFile := WinBase.CreateFileA    (SYSTEM.ADR(ParamFileName),
                                       WinNT.GENERIC_WRITE,
                                       0,
                                       File_Security,
                                       WinBase.TRUNCATE_EXISTING,
                                       WinNT.FILE_ATTRIBUTE_NORMAL,
                                       0);
  IF ParamFile=-1 THEN                                     (* error during file open *)
    ParamFile := WinBase.CreateFileA    (SYSTEM.ADR(ParamFileName),
                                         WinNT.GENERIC_WRITE,
                                         0,
                                         File_Security,
                                         WinBase.CREATE_NEW,
                                         WinNT.FILE_ATTRIBUTE_NORMAL,
                                         0);
  END;
  (* generate file containing the qualified items *)
  FirstTokenPtr  := AnchorTokenPtr;
  Done           := FALSE;
  REPEAT                             (* read over parameter without qualifier *)
    Position := Strings.PosChar(".", FirstTokenPtr^.Token.Value, 1);
    IF Position>0 THEN
      Done := TRUE;
    END;
    FirstTokenPtr := FirstTokenPtr^.NextToken;
    IF FirstTokenPtr=NIL THEN
      Done := TRUE;
    END;
  UNTIL Done;
  
  Done           := FALSE;
  REPEAT                             (* read over SYSTEM.NAME *)
    Position := Strings.PosChar(".", FirstTokenPtr^.Token.Value, 1);
    Strings.Copy(FirstTokenPtr^.Token.Value, TokenQualifier, 1, Position-1);
    IF TokenQualifier#"SYSTEM" THEN
      Done := TRUE
    END;
    FirstTokenPtr := FirstTokenPtr^.NextToken;
    IF FirstTokenPtr=NIL THEN
      Done := TRUE;
    END;
  UNTIL Done;

  Done       := FALSE;
  Qualifier  := "SYSTEM";
  
  REPEAT                             (* read and write <qualifier>.NAME *)
    Position := Strings.PosChar(".", FirstTokenPtr^.Token.Value, 1);
    IF Position=0 THEN
      Done := TRUE;
    ELSE
      Strings.Copy(FirstTokenPtr^.Token.Value, TokenQualifier, 1, Position-1);
      IF TokenQualifier#Qualifier THEN
        COPY(TokenQualifier, Qualifier);
        ResultBool   := WinBase.WriteFile(ParamFile,
                                      SYSTEM.ADR(OpenBracket),
                                      1,
                                      BytesWritten,
                                      0);
        ResultBool   := WinBase.WriteFile(ParamFile,
                                      SYSTEM.ADR(Qualifier),
                                      Strings.Length(Qualifier),
                                      BytesWritten,
                                      0);
        ResultBool   := WinBase.WriteFile(ParamFile,
                                      SYSTEM.ADR(CloseBracket),
                                      1,
                                      BytesWritten,
                                      0);
        ResultBool   := WinBase.WriteFile(ParamFile,
                                      SYSTEM.ADR(CrLf),
                                      2,
                                      BytesWritten,
                                      0);
      END (* IF TokenQualifier#Qualifier *);
      ResultBool   := WinBase.WriteFile(ParamFile,
                                    SYSTEM.ADR(FirstTokenPtr^.Token.Index),
                                    Strings.Length(FirstTokenPtr^.Token.Index),
                                    BytesWritten,
                                    0);
      ResultBool   := WinBase.WriteFile(ParamFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);

      FirstTokenPtr := FirstTokenPtr^.NextToken;
      IF FirstTokenPtr=NIL THEN
        Done := TRUE;
      END (* IF FirstTokenPtr=NIL *);
    END (* IF Position=0  *);
  UNTIL Done;
                                     (* Write actual qualified items *)
  ResultBool   := WinBase.WriteFile(ParamFile,
                                SYSTEM.ADR(OpenBracket),
                                1,
                                BytesWritten,
                                0);
  ResultBool   := WinBase.WriteFile(ParamFile,
                                SYSTEM.ADR(ModuleFileName),
                                Strings.Length(ModuleFileName),
                                BytesWritten,
                                0);
  ResultBool   := WinBase.WriteFile(ParamFile,
                                SYSTEM.ADR(CloseBracket),
                                1,
                                BytesWritten,
                                0);
  ResultBool   := WinBase.WriteFile(ParamFile,
                                SYSTEM.ADR(CrLf),
                                2,
                                BytesWritten,
                                0);
  Done         := FALSE;
  REPEAT                             (* read and write NAME *)
    IF FirstTokenPtr=NIL THEN
      Done := TRUE;
    ELSE
      Strings.Copy(FirstTokenPtr^.Token.Value, TokenQualifier, 1, Position-1);
      ResultBool   := WinBase.WriteFile(ParamFile,
                                    SYSTEM.ADR(FirstTokenPtr^.Token.Index),
                                    Strings.Length(FirstTokenPtr^.Token.Index),
                                    BytesWritten,
                                    0);
      ResultBool   := WinBase.WriteFile(ParamFile,
                                    SYSTEM.ADR(CrLf),
                                    2,
                                    BytesWritten,
                                    0);

      FirstTokenPtr := FirstTokenPtr^.NextToken;
    END (* IF FirstTokenPtr=NIL *);
  UNTIL Done;

  ResultBool := WinBase.CloseHandle(ParamFile);
  
END Close;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

BEGIN

  CrLf[0]                      := CHR(0DH);
  CrLf[1]                      := CHR(0AH);
  CrLf[2]                      := CHR(0);
  CloseBracket[0]              := "]";
  CloseBracket[1]              := CHR(0);
  OpenBracket[0]               := "[";
  OpenBracket[1]               := CHR(0);

  NEW(FirstTokenPtr);                                      (* Generate TranslationTable *)
  AnchorTokenPtr               := FirstTokenPtr;
  
  FirstTokenPtr^.Token.Index   := "WINAPI";                (* caveat:  define parameter without qualifier first *)
  FirstTokenPtr^.Token.Value   := "__stdcall";
  NextTokenPtr                 := FirstTokenPtr;
  
  NEW(NextTokenPtr^.NextToken);
  NextTokenPtr                 := NextTokenPtr^.NextToken;  
  NextTokenPtr^.Token.Index    := "APIENTRY";
  NextTokenPtr^.Token.Value    := "__stdcall";

  NEW(NextTokenPtr^.NextToken);
  NextTokenPtr                 := NextTokenPtr^.NextToken;  
  NextTokenPtr^.Token.Index    := "CALLBACK";
  NextTokenPtr^.Token.Value    := "__stdcall";

  NEW(NextTokenPtr^.NextToken);
  NextTokenPtr                 := NextTokenPtr^.NextToken;  
  NextTokenPtr^.Token.Index    := "PASCAL";
  NextTokenPtr^.Token.Value    := "__stdcall";

  NEW(NextTokenPtr^.NextToken);                            (* here starts the definition of qualified SYSTEM parameter *)
  NextTokenPtr                 := NextTokenPtr^.NextToken;  
  NextTokenPtr^.Token.Index    := "PVOID";
  NextTokenPtr^.Token.Value    := "SYSTEM.PTR";

  NEW(NextTokenPtr^.NextToken);
  NextTokenPtr                 := NextTokenPtr^.NextToken;  
  NextTokenPtr^.Token.Index    := "ADDRESS";
  NextTokenPtr^.Token.Value    := "SYSTEM.PTR";

  NextTokenPtr^.NextToken      := NIL;
  
END ConvertParam.

