
DEFINITION SBWin32;
  
(********************************************************************************
 * Autor   : Koen Desaeger                                                      *
 * Project : Symbol file browser ( viewer ) for POW-Oberon-2 symbol-files       *
 ********************************************************************************
 *   This module defines a subset of the Windows API.                           *
 ********************************************************************************)

  TYPE
    INT32 = LONGINT; INT16 = INTEGER;
    DWORD = INT32;
    UINT = INT32;
    HINSTANCE = INT32;
    LPSTR = INT32;
    
  
(*********************************************************************
 **                    DDE Api : CONST & TYPE                       **
 *********************************************************************)
   
  TYPE
    (* TYPE safe DDE handles: *)
    HCONV = POINTER TO RECORD END;
    HSZ = POINTER TO RECORD END;
    HDDEDATA = POINTER TO RECORD END;


    PFNCALLBACK = PROCEDURE [_APICALL] ( wType: INT32;
                                         wFmt: INT32;
                                         hConv: HCONV;
                                         hsz1: HSZ;
                                         hsz2: HSZ;
                                         hData: HDDEDATA;
                                         dwData1: INT32;
                                         dwData2: INT32
                                       )
                                       : HDDEDATA;
  CONST 
    XTYP_EXECUTE = 16464;   (*4050H(0x0050 | XCLASS_FLAGS         )*)
    APPCMD_CLIENTONLY = 10H;
    DMLERR_NO_ERROR = 0;
  
  
(*********************************************************************
 **             CommDlg, WinBase, WinUser : CONST & TYPE            **
 *********************************************************************)

  CONST
    CF_TEXT = 1;
    MB_OK = 0H;
    IDOK = 1;
    
  TYPE  
    LPOFNHOOKPROC = PROCEDURE [_APICALL] ( hwnd: INT32;
                                           uint: INT32;
                                           wParam: INT32; 
                                           lParam: INT32
                                         )
                                         : INT32;

    OPENFILENAMEA = RECORD [_NOTALIGNED]
      lStructSize      : INT32;
      hwndOwner        : INT32;
      hInstance        : INT32;
      lpstrFilter      : LPSTR;
      lpstrCustomFilter: LPSTR;
      nMaxCustFilter   : INT32;
      nFilterIndex     : INT32;
      lpstrFile        : LPSTR;
      nMaxFile         : INT32;
      lpstrFileTitle   : LPSTR;
      nMaxFileTitle    : INT32;
      lpstrInitialDir  : LPSTR;
      lpstrTitle       : LPSTR;
      Flags            : INT32;
      nFileOffset      : INT16;
      nFileExtension   : INT16;
      lpstrDefExt      : LPSTR;
      lCustData        : INT32;
      lpfnHook         : LPOFNHOOKPROC;
      lpTemplateName   : LPSTR;
    END;
    OPENFILENAME = OPENFILENAMEA;
    
  CONST
    OFN_FILEMUSTEXIST = 1000H;
    
  
  
(*********************************************************************
 **                 DDE Api : PROCEDURES                            **
 *********************************************************************)
 
  PROCEDURE [_APICALL] DdeClientTransaction ( data: INT32(*WD.LPBYTE*);
                                              cbData: INT32;
                                              hConv: HCONV;
                                              hszItem: HSZ;
                                              wFmt: INT32;
                                              wType: INT32;
                                              dwTimeout: INT32;
                                              VAR pdwResult: INT32
                                            )
                                            : HDDEDATA;
  
  PROCEDURE [_APICALL] DdeInitializeA ( VAR idInst: INT32;
                                        pfnCallback: PFNCALLBACK;
                                        afCmd: INT32;
                                        ulRes: INT32 ): INT32;

  PROCEDURE [_APICALL] DdeConnect ( idInst: INT32;
                                    hszService: HSZ;
                                    hszTopic: HSZ;
                                  (*VAR STATICTYPED CC: CONVCONTEXT*)
                                    CC: INT32 (* set to 0! *)
                                  )
                                  : HCONV;

  PROCEDURE [_APICALL] DdeCreateStringHandleA ( idInst: INT32;
                                                psz: INT32(*WD.LPCSTR*);
                                                iCodePage: INT32
                                              )
                                              : HSZ;

  PROCEDURE [_APICALL] DdeDisconnect ( hConv: HCONV ): BOOLEAN;

  PROCEDURE [_APICALL] DdeUninitialize ( idInst: INT32 ): BOOLEAN;



(*********************************************************************
 **           CommDlg, WinBase, WinUser : PROCEDURES                **
 *********************************************************************)

  PROCEDURE [_APICALL] GetOpenFileNameA ( VAR STATICTYPED ofn: OPENFILENAMEA ): BOOLEAN;


  PROCEDURE [_APICALL] MessageBoxA ( hWnd: INT32;
                                     lpText: LPSTR;
                                     lpCaption: LPSTR;
                                     uType: INT32
                                   )
                                   : INT32;
      
  PROCEDURE [_APICALL] lstrcpynA (lpStr1, lpStr2 : LPSTR ; iMaxLength : INT32 ): LPSTR;
  
  PROCEDURE [_APICALL] CreateFileA ( lpFileName: INT32;
                                     dwDesiredAccess: INT32;
                                     dwShareMode: INT32; 
                                   (*VAR STATICTYPED SecurityAttributes: SECURITY_ATTRIBUTES;*)
                                     SecurityAttributes: INT32;
                                     dwCreationDisposition: INT32;
                                     dwFlagsAndAttributes: INT32;
                                     hTemplateFile: INT32 ): INT32;

  PROCEDURE [_APICALL] CloseHandle ( handle : INT32 );
  
  PROCEDURE [_APICALL] ReadFile ( hFile: INT32;
                                  lpBuffer: INT32;
                                  nNumberOfBytesToRead: INT32;
                                  VAR NumberOfBytesRead: INT32;
                                  Overlapped: INT32
                                )
                                : INT32;

  
  
  END SBWin32.
