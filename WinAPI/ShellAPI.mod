(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 1992-1999, Microsoft Corp.  All rights reserved             *)
(*                                                                           *)
(*****************************************************************************)

DEFINITION ShellAPI;

IMPORT
  WinDef;


CONST
  (* AppBar stuff*)
  ABM_NEW              =               00000000H;
  ABM_REMOVE           =               00000001H;
  ABM_QUERYPOS         =               00000002H;
  ABM_SETPOS           =               00000003H;
  ABM_GETSTATE         =               00000004H;
  ABM_GETTASKBARPOS    =               00000005H;
  ABM_ACTIVATE         =               00000006H;          (* lParam == TRUE/FALSE means activate/deactivate *)
  ABM_GETAUTOHIDEBAR   =               00000007H;
  ABM_SETAUTOHIDEBAR   =               00000008H;          (* this can fail at any time.  MUST check the result *)
                                                           (* lParam = TRUE/FALSE  Set/Unset *)
                                                           (* uEdge = what edge *)
  ABM_WINDOWPOSCHANGED =               0000009H;
  
  (* these are put in the wparam of callback messages*)
  ABN_STATECHANGE      =               0000000H;
  ABN_POSCHANGED       =               0000001H;
  ABN_FULLSCREENAPP    =               0000002H;
  ABN_WINDOWARRANGE    =               0000003H;           (* lParam == TRUE means hide*)
  
  (* flags for get state*)
  ABS_AUTOHIDE         =               0000001H;
  ABS_ALWAYSONTOP      =               0000002H;
  
  ABE_LEFT             =                      0;
  ABE_TOP              =                      1;
  ABE_RIGHT            =                      2;
  ABE_BOTTOM           =                      3;
  
  (* Shell File Operations *)
  (*these need to be kept in sync with the ones in shlobj.h*)
  
  FO_MOVE           =0001H;
  FO_COPY           =0002H;
  FO_DELETE         =0003H;
  FO_RENAME         =0004H;
  
  FOF_MULTIDESTFILES         =0001H;
  FOF_CONFIRMMOUSE           =0002H;
  FOF_SILENT                 =0004H;  (* don't create progress/report*)
  FOF_RENAMEONCOLLISION      =0008H;
  FOF_NOCONFIRMATION         =0010H;  (* Don't prompt the user.*)
  FOF_WANTMAPPINGHANDLE      =0020H;  (* Fill in SHFILEOPSTRUCT.hNameMappings*)
                                        (* Must be freed using SHFreeNameMappings *)
  FOF_ALLOWUNDO              =0040H;
  FOF_FILESONLY              =0080H;  (* on *.*, do only files *)
  FOF_SIMPLEPROGRESS         =0100H;  (* means don't show names of files*)
  FOF_NOCONFIRMMKDIR         =0200H;  (* don't confirm making any needed dirs*)
  FOF_NOERRORUI              =0400H;  (* don't put up error UI*)
  FOF_NOCOPYSECURITYATTRIBS  =0800H;  (* dont copy NT file Security Attributes*)
  FOF_NORECURSION            =1000H;  (* don't recurse into directories.*)
  
  FOF_NO_CONNECTED_ELEMENTS  =2000H;  (* don't operate on connected elements.*)
  FOF_WANTNUKEWARNING        =4000H;  (* during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)*)
  
  
  
  PO_DELETE       =0013H;  (* printer is being deleted *)
  PO_RENAME       =0014H;  (* printer is being renamed   *)
  PO_PORTCHANGE   =0020H;  (* port this printer connected to is being changed*)
                                  (* if this id is set, the strings received by      *)
                                  (* the copyhook are a doubly-null terminated       *)
                                  (* list of strings.  The first is the printer       *)
                                  (* name and the second is the printer port.         *)
  PO_REN_PORT     =0034H;  (* PO_RENAME and PO_PORTCHANGE at same time.         *)
                                                                                      
  (* no POF_ flags currently defined*)
  
  (* End Shell File Operations *)
  
  (*  Begin ShellExecuteEx and family*)
  (* ShellExecute() and ShellExecuteEx() error codes *)
  
  (* regular WinExec() codes *)
   SE_ERR_FNF            =  2;       (* file not found*)
   SE_ERR_PNF             = 3;       (* path not found*)
   SE_ERR_ACCESSDENIED   = 5;       (* access denied*)
   SE_ERR_OOM             = 8;       (* out of memory*)
   SE_ERR_DLLNOTFOUND             = 32;
  
  
  
  (* error values for ShellExecute() beyond the regular WinExec() codes *)
   SE_ERR_SHARE                   = 26;
   SE_ERR_ASSOCINCOMPLETE         = 27;
   SE_ERR_DDETIMEOUT              = 28;
   SE_ERR_DDEFAIL                 = 29;
   SE_ERR_DDEBUSY                =  30;
   SE_ERR_NOASSOC                 = 31;
  
  
  
  (* Note CLASSKEY overrides CLASSNAME *)
   SEE_MASK_CLASSNAME        =00000001H;
   SEE_MASK_CLASSKEY         =00000003H;
  (* INVOKEIDLIST overrides IDLIST *)
   SEE_MASK_IDLIST           =00000004H;
   SEE_MASK_INVOKEIDLIST     =0000000CH;
   SEE_MASK_ICON             =00000010H;
   SEE_MASK_HOTKEY           =00000020H;
   SEE_MASK_NOCLOSEPROCESS   =00000040H;
   SEE_MASK_CONNECTNETDRV    =00000080H;
   SEE_MASK_FLAG_DDEWAIT     =00000100H;
   SEE_MASK_DOENVSUBST       =00000200H;
   SEE_MASK_FLAG_NO_UI       =00000400H;
   SEE_MASK_UNICODE          =00004000H;
   SEE_MASK_NO_CONSOLE       =00008000H;
   SEE_MASK_ASYNCOK          =00100000H;
   SEE_MASK_HMONITOR         =00200000H;
  
   SEE_MASK_NOQUERYCLASSSTORE =01000000H;
   SEE_MASK_WAITFORINPUTIDLE  =02000000H;
  
  
  
  (* For compilers that don't support nameless unions *)
  
  (*#ifndef DUMMYUNIONNAME
  #ifdef NONAMELESSUNION
  #define DUMMYUNIONNAME   u
  #define DUMMYUNIONNAME2  u2
  #define DUMMYUNIONNAME3  u3
  #define DUMMYUNIONNAME4  u4
  #define DUMMYUNIONNAME5  u5
  #else
  #define DUMMYUNIONNAME
  #define DUMMYUNIONNAME2
  #define DUMMYUNIONNAME3
  #define DUMMYUNIONNAME4
  #define DUMMYUNIONNAME5
  #endif
  #endif  DUMMYUNIONNAME*)
  
  SHERB_NOCONFIRMATION    =00000001H;
  SHERB_NOPROGRESSUI      =00000002H;
  SHERB_NOSOUND           =00000004H;


TYPE
  HDROP        =                       LONGINT;
  HKEY         =                       LONGINT;
  HRESULT      =                       LONGINT;
  PRINTEROP_FLAGSH =                   WinDef.WORD; 
  FILEOP_FLAGS =                       WinDef.WORD;

  LPDRAGINFOA = POINTER TO DRAGINFOA;
  DRAGINFOA = RECORD [_NOTALIGNED]
    uSize        : WinDef.UINT;                 (* init with sizeof(DRAGINFO) *)
    pt           : WinDef.POINT;
    fNC          : WinDef.BOOL;
    lpFileList   : WinDef.LPSTR;
    grfKeyState  : WinDef.DWORD;
  END;
  DRAGINFOW   = DRAGINFOA;
  LPDRAGINFOW = LPDRAGINFOA;

  DRAGINFO   = DRAGINFOA;
  LPDRAGINFO = LPDRAGINFOA;
    
  LPAPPBARDATA = POINTER TO APPBARDATA;
  
  APPBARDATA = RECORD [_NOTALIGNED]
    cbSize            : WinDef.DWORD ;
    hWnd              : WinDef.HWND ;
    uCallbackMessage  : WinDef.UINT ;
    uEdge             : WinDef.UINT ;
    rc                : WinDef.RECT ;
    lParam            : WinDef.LPARAM ; (* message specific*)
  END;

  LPSHFILEOPSTRUCTA = POINTER TO SHFILEOPSTRUCTA;
  SHFILEOPSTRUCTA= RECORD [_NOTALIGNED]
    hWnd                    : WinDef.HWND;
    wFunc                   : WinDef.UINT;
    pFrom                   : WinDef.LPCSTR;
    pTo                     : WinDef.LPCSTR;
    fFlags                  : FILEOP_FLAGS;
    fAnyOperationsAborted   : WinDef.BOOL;
    hNameMappings           : WinDef.LPVOID;
    lpszProgressTitle       : WinDef.LPCSTR           ; (* only used if FOF_SIMPLEPROGRESS*)
  END;
  
  SHFILEOPSTRUCT = SHFILEOPSTRUCTA;
  LPSHFILEOPSTRUCT= LPSHFILEOPSTRUCTA;
  SHFILEOPSTRUCTW = SHFILEOPSTRUCT;
  LPSHFILEOPSTRUCTW = LPSHFILEOPSTRUCT;
  
  LPSHNAMEMAPPINGA = POINTER TO SHNAMEMAPPINGA;
  SHNAMEMAPPINGA = RECORD [_NOTALIGNED]
    pszOldPath  : WinDef.LPSTR;
    pszNewPath  : WinDef.LPSTR;
    cchOldPath  : WinDef.INT;
    cchNewPath  : WinDef.INT;
  END;
  SHNAMEMAPPING = SHNAMEMAPPINGA;
  LPSHNAMEMAPPING = LPSHNAMEMAPPINGA;
  SHNAMEMAPPINGW = SHNAMEMAPPING;
  LPSHNAMEMAPPINGW = LPSHNAMEMAPPING;

  LPSHELLEXECUTEINFOA = POINTER TO SHELLEXECUTEINFOA;
  SHELLEXECUTEINFOA = RECORD [_NOTALIGNED]
    cbSize:WinDef.DWORD ;
    fMask:WinDef.ULONG ;
    hWnd:WinDef.HWND ;
    lpVerb:WinDef.LPCSTR   ;
    lpFile:WinDef.LPCSTR   ;
    lpParameters:WinDef.LPCSTR   ;
    lpDirectory:WinDef.LPCSTR   ;
    nShow:WinDef.INT ;
    hInstApp:WinDef.HINSTANCE ;
    (* Optional fields*)
    lpIDList:WinDef.LPVOID ;
    lpClass:WinDef.LPCSTR   ;
    hkeyClass:HKEY ;
    dwHotKey:WinDef.DWORD ;
    (*union {*)
    hIcon:WinDef.HANDLE ;
    (*HANDLE hMonitor;
    } DUMMYUNIONNAME;*)
    hProcess: WinDef.HANDLE ;
  END;
  SHELLEXECUTEINFO = SHELLEXECUTEINFOA;
  LPSHELLEXECUTEINFO = LPSHELLEXECUTEINFOA;

  SHELLEXECUTEINFOW = SHELLEXECUTEINFO;
  LPSHELLEXECUTEINFOW = LPSHELLEXECUTEINFO;
 
  LPSHQUERYRBINFO = POINTER TO SHQUERYRBINFO;
  SHQUERYRBINFO = RECORD [_NOTALIGNED]
    iSizeA:LONGINT;
    iSizeB:LONGINT;
    NumItemsA:LONGINT;
    NumItemsB:LONGINT;
  END;

  
PROCEDURE [_APICALL] DragQueryFileA   (hdrop:              HDROP;
                                       iFile:              WinDef.UINT;
                                       lpszFile:           WinDef.LPSTR;
                                       cch:                WinDef.UINT)
                                      :WinDef.UINT;

(* PROCEDURE [_APICALL] DragQueryFileW(hdrop:HDROP;iFile:WinDef.UINT,lpszFile:WinDef.LPWSTR,WinDef.UINT):WinDef.UINT;*)

PROCEDURE [_APICALL] DragQueryPoint   (hdrop:              HDROP;
                                       lpPoint:            WinDef.LPPOINT)
                                      :WinDef.BOOL;

PROCEDURE [_APICALL] DragFinish       (hdrop:              HDROP);
PROCEDURE [_APICALL] DragAcceptFiles  (hWnd:               WinDef.HWND;
                                       bool:               WinDef.BOOL);

PROCEDURE [_APICALL] ShellExecuteA    (hWnd:               WinDef.HWND; 
                                       lpOperation:        WinDef.LPCSTR ;
                                       lpFile:             WinDef.LPCSTR ;
                                       lpParameters:       WinDef.LPCSTR ;
                                       lpDirectory:        WinDef.LPCSTR ;
                                       nShowCmd:           WinDef.INT )
                                      :WinDef.HINSTANCE;

(* PROCEDURE [_APICALL] ShellExecuteW(HWND hWnd, LPCWSTR lpOperation, LPCWSTR lpFile, LPCWSTR lpParameters, LPCWSTR lpDirectory, INT nShowCmd):WinDef.HINSTANCE;*)

PROCEDURE [_APICALL] FindExecutableA  (lpFile:             WinDef.LPCSTR ; 
                                       lpDirectory:        WinDef.LPCSTR ;
                                       lpResult:           WinDef.LPSTR )
                                      :WinDef.HINSTANCE;

(* PROCEDURE [_APICALL] FindExecutableW(LPCWSTR lpFile, LPCWSTR lpDirectory, LPWSTR lpResult)WinDef.HINSTANCE;*)

(* PROCEDURE [_APICALL] CommandLineToArgvW(LPCWSTR lpCmdLine, int*pNumArgs)WinDef.LPWSTR;*)

PROCEDURE [_APICALL] ShellAboutA      (hWnd:               WinDef.HWND;
                                       szApp:              WinDef.LPCSTR; 
                                       szOtherStuff:       WinDef.LPCSTR; 
                                       hIcon:              WinDef.HICON )
                                      :WinDef.INT;

(* PROCEDURE [_APICALL] ShellAboutW(HWND hWnd, LPCWSTR szApp, LPCWSTR szOtherStuff, HICON hIcon)WinDef.INT;*)

PROCEDURE [_APICALL] DuplicateIcon    (hInstance:          WinDef.HINSTANCE; 
                                       hIcon:              WinDef.HICON )
                                      :WinDef.HICON;

PROCEDURE [_APICALL] ExtractAssociatedIconA
                                      (hInst:              WinDef.HINSTANCE; 
                                       lpIconPath:         WinDef.LPSTR;
                                       lpiIcon:            WinDef.LPWORD )
                                      :WinDef.HICON;

(* PROCEDURE [_APICALL] ExtractAssociatedIconW(HINSTANCE hInst, LPWSTR lpIconPath, LPWORD lpiIcon):WinDef.HICON;*)

PROCEDURE [_APICALL] ExtractIconA     (hInst:              WinDef.HINSTANCE;
                                       lpszExeFileName:    WinDef.LPCSTR;
                                       nIconIndex:         WinDef.UINT )
                                      :WinDef.HICON;

(* PROCEDURE [_APICALL] ExtractIconW(HINSTANCE hInst, LPCWSTR lpszExeFileName, UINT nIconIndex)WinDef.HICON;*)

PROCEDURE [_APICALL] SHAppBarMessage  (dwMessage:          WinDef.DWORD;
                                       pData:              LPAPPBARDATA)
                                      :WinDef.UINT;
(*  EndAppBar *)

PROCEDURE [_APICALL] DoEnvironmentSubstA
                                      (szString:           WinDef.LPSTR; 
                                       cchString:          WinDef.UINT)
                                      :WinDef.DWORD;

(* SHSTDAPI_(DWORD)   DoEnvironmentSubstW(LPWSTR szString, UINT cchString); *)

PROCEDURE [_APICALL] ExtractIconExA   (lpszFile:           WinDef.LPCSTR; 
                                       nIconIndex:         LONGINT ;
                                       phiconLarge:        LONGINT;
                                       phiconSmall:        LONGINT;
                                       nIcons:             WinDef.UINT)
                                      :WinDef.UINT;

(* SHSTDAPI_(UINT) ExtractIconExW(LPCWSTR lpszFile, int nIconIndex, HICON *phiconLarge, HICON *phiconSmall, UINT nIcons);*)



(* implicit parameters are: *)
(*      if pFrom or pTo are unqualified names the current directories are *)
(*      taken from the global current drive/directory settings managed*)
(*      by Get/SetCurrentDrive/Directory*)
(*                             *)
(*      the global confirmation settings *)



PROCEDURE [_APICALL] SHFileOperationA (lpFileOp:           LPSHFILEOPSTRUCTA )
                                      :WinDef.INT;
                                      
(* SHSTDAPI_(int) SHFileOperationW(LPSHFILEOPSTRUCTW lpFileOp);*)

PROCEDURE [_APICALL] SHFreeNameMappings
                                      (hNameMappings:      WinDef.HANDLE );

PROCEDURE [_APICALL] ShellExecuteExA  (lpExecInfo:         LPSHELLEXECUTEINFOA)
                                      :WinDef.BOOL;

(* SHSTDAPI_(BOOL) ShellExecuteExW(LPSHELLEXECUTEINFOW lpExecInfo); *)

PROCEDURE [_APICALL] WinExecErrorA    (hWnd:               WinDef.HWND;
                                       error:              WinDef.INT;
                                       lpstrFileName:      WinDef.LPCSTR;
                                       lpstrTitle:         WinDef.LPCSTR );

(* SHSTDAPI_(void) WinExecErrorW(HWND hWnd, int error, LPCWSTR lpstrFileName, LPCWSTR lpstrTitle);*)

(*  End ShellExecuteEx and family *)

(* RecycleBin*)

(* flags for SHEmptyRecycleBin *)

PROCEDURE [_APICALL] SHQueryRecycleBinA
                                      (pszRootPath:        WinDef.LPCSTR;
                                       pSHQueryRBInfo:     LPSHQUERYRBINFO )
                                      :HRESULT;

(* SHSTDAPI SHQueryRecycleBinW(LPCWSTR pszRootPath, LPSHQUERYRBINFO pSHQueryRBInfo);*)

PROCEDURE [_APICALL] SHEmptyRecycleBinA
                                      (hWnd:               WinDef.HWND;
                                      pszRootPath:         WinDef.LPCSTR;
                                      dwFlags:             WinDef.DWORD )
                                     :HRESULT;

(* SHSTDAPI SHEmptyRecycleBinW(HWND hWnd, LPCWSTR pszRootPath, DWORD dwFlags);*)



(* end of RecycleBin*)



(* Tray notification definitions*)

(*
typedef struct _NOTIFYICONDATAA {
        DWORD cbSize;
        HWND hWnd;
        UINT uID;
        UINT uFlags;
        UINT uCallbackMessage;
        HICON hIcon;
#if (_WIN32_IE < =0500)
        CHAR   szTip[64];
#else
        CHAR   szTip[128];
#endif
#if (_WIN32_IE >= =0500)
        DWORD dwState;
        DWORD dwStateMask;
        CHAR   szInfo[256];
        union {
            UINT  uTimeout;
            UINT  uVersion;
        } DUMMYUNIONNAME;
        CHAR   szInfoTitle[64];
        DWORD dwInfoFlags;
#endif
} NOTIFYICONDATAA, *PNOTIFYICONDATAA;
typedef struct _NOTIFYICONDATAW {
        DWORD cbSize;
        HWND hWnd;
        UINT uID;
        UINT uFlags;
        UINT uCallbackMessage;
        HICON hIcon;
#if (_WIN32_IE < =0500)
        WCHAR  szTip[64];
#else
        WCHAR  szTip[128];
#endif
#if (_WIN32_IE >= =0500)
        DWORD dwState;
        DWORD dwStateMask;
        WCHAR  szInfo[256];
        union {
            UINT  uTimeout;
            UINT  uVersion;
        } DUMMYUNIONNAME;
        WCHAR  szInfoTitle[64];
        DWORD dwInfoFlags;
#endif
} NOTIFYICONDATAW, *PNOTIFYICONDATAW;
#ifdef UNICODE
typedef NOTIFYICONDATAW NOTIFYICONDATA;
typedef PNOTIFYICONDATAW PNOTIFYICONDATA;
#else
typedef NOTIFYICONDATAA NOTIFYICONDATA;
typedef PNOTIFYICONDATAA PNOTIFYICONDATA;
#endif (* UNICODE *)


#define NOTIFYICONDATAA_V1_SIZE     FIELD_OFFSET(NOTIFYICONDATAA, szTip[64])
#define NOTIFYICONDATAW_V1_SIZE     FIELD_OFFSET(NOTIFYICONDATAW, szTip[64])
#ifdef UNICODE
#define NOTIFYICONDATA_V1_SIZE      NOTIFYICONDATAW_V1_SIZE
#else
#define NOTIFYICONDATA_V1_SIZE      NOTIFYICONDATAA_V1_SIZE
#endif


#if (_WIN32_IE >= =0500)
#define NIN_SELECT          (WM_USER + 0)
#define NINF_KEY            =1
#define NIN_KEYSELECT       (NIN_SELECT | NINF_KEY)
#endif


#define NIM_ADD         =00000000
#define NIM_MODIFY      =00000001
#define NIM_DELETE      =00000002
#if (_WIN32_IE >= =0500)
#define NIM_SETFOCUS    =00000003
#define NIM_SETVERSION  =00000004
#define     NOTIFYICON_VERSION 3
#endif

#define NIF_MESSAGE     =00000001
#define NIF_ICON        =00000002
#define NIF_TIP         =00000004
#if (_WIN32_IE >= =0500)
#define NIF_STATE       =00000008
#define NIF_INFO        =00000010
#endif

#if (_WIN32_IE >= =0500)
#define NIS_HIDDEN      =00000001
#define NIS_SHAREDICON  =00000002

(* Notify Icon Infotip flags *)
#define NIIF_NONE       =00000000
(* icon flags are mutualy exclusive *)
(* and take only the lowest 2 bits *)
#define NIIF_INFO       =00000001
#define NIIF_WARNING    =00000002
#define NIIF_ERROR      =00000003
#endif

SHSTDAPI_(BOOL) Shell_NotifyIconA(DWORD dwMessage, PNOTIFYICONDATAA lpData);
SHSTDAPI_(BOOL) Shell_NotifyIconW(DWORD dwMessage, PNOTIFYICONDATAW lpData);
#ifdef UNICODE
#define Shell_NotifyIcon  Shell_NotifyIconW
#else
#define Shell_NotifyIcon  Shell_NotifyIconA
#endif (* !UNICODE  *)


(*(* End Tray Notification Icons  *)



#ifndef SHFILEINFO_DEFINED
#define SHFILEINFO_DEFINED

(* Begin SHGetFileInfo*)

(*                                                               *)
(* The SHGetFileInfo API provides an easy way to get attributes  *)
(* for a file given a pathname.                                    *)
(*                                                                   *)
(*   PARAMETERS                                                        *)
(*                                                                       *)
(*     pszPath              file name to get info about                    *)
(*     dwFileAttributes     file attribs, only used with SHGFI_USEFILEATTRIBUTES*)
(*     psfi                 place to return file info                          *)
(*     cbFileInfo           size of structure                                  *)
(*     uFlags               flags                                                *)
(*                                                                                 *)
(*   RETURN                                                                          *)
(*     TRUE if things worked                                                           *)
(*                                                                                      *)
                                                                                           *)
typedef struct _SHFILEINFOA
{
        HICON       hIcon;                      (* out: icon*)
        int         iIcon;                      (* out: icon index *)
        DWORD       dwAttributes;               (* out: SFGAO_ flags *)
        CHAR        szDisplayName[MAX_PATH];    (* out: display name (or path)*)
        CHAR        szTypeName[80];             (* out: type name *)
} SHFILEINFOA;
typedef struct _SHFILEINFOW
{
        HICON       hIcon;                      (* out: icon *)
        int         iIcon;                      (* out: icon index *)
        DWORD       dwAttributes;               (* out: SFGAO_ flags *)
        WCHAR       szDisplayName[MAX_PATH];    (* out: display name (or path)*)
        WCHAR       szTypeName[80];             (* out: type name*)
} SHFILEINFOW;
#ifdef UNICODE
typedef SHFILEINFOW SHFILEINFO;
#else
typedef SHFILEINFOA SHFILEINFO;
#endif (* UNICODE *)


(* NOTE: This is also in shlwapi.h.  Please keep in synch. *)
#endif (* !SHFILEINFO_DEFINED *)

#define SHGFI_ICON              =000000100     (* get icon*)
#define SHGFI_DISPLAYNAME       =000000200     (* get display name*)
#define SHGFI_TYPENAME          =000000400     (* get type name     *)
#define SHGFI_ATTRIBUTES        =000000800     (* get attributes      *)
#define SHGFI_ICONLOCATION      =000001000     (* get icon location     *)
#define SHGFI_EXETYPE           =000002000     (* return exe type         *)
#define SHGFI_SYSICONINDEX      =000004000     (* get system icon index     *)
#define SHGFI_LINKOVERLAY       =000008000     (* put a link overlay on icon  *)
#define SHGFI_SELECTED          =000010000     (* show icon in selected state   *)
#define SHGFI_ATTR_SPECIFIED    =000020000     (* get only specified attributes   *)
#define SHGFI_LARGEICON         =000000000     (* get large icon                  *)
#define SHGFI_SMALLICON         =000000001     (* get small icon                  *)
#define SHGFI_OPENICON          =000000002     (* get open icon                    *)
#define SHGFI_SHELLICONSIZE     =000000004     (* get shell size icon              *)
#define SHGFI_PIDL              =000000008     (* pszPath is a pidl                *)
#define SHGFI_USEFILEATTRIBUTES =000000010     (* use passed dwFileAttribute      *)

#if (_WIN32_IE >= =0500)
#define SHGFI_ADDOVERLAYS       =000000020     (* apply the appropriate overlays    *)
#define SHGFI_OVERLAYINDEX      =000000040     (* Get the index of the overlay      *)
                                                (* in the upper 8 bits of the iIcon *)
#endif

SHSTDAPI_(DWORD_PTR) SHGetFileInfoA(LPCSTR pszPath, DWORD dwFileAttributes, SHFILEINFOA *psfi, UINT cbFileInfo, UINT uFlags);
SHSTDAPI_(DWORD_PTR) SHGetFileInfoW(LPCWSTR pszPath, DWORD dwFileAttributes, SHFILEINFOW *psfi, UINT cbFileInfo, UINT uFlags);
#ifdef UNICODE
#define SHGetFileInfo  SHGetFileInfoW
#else
#define SHGetFileInfo  SHGetFileInfoA
#endif (* !UNICODE *)


#define SHGetDiskFreeSpace SHGetDiskFreeSpaceEx

SHSTDAPI_(BOOL) SHGetDiskFreeSpaceExA(LPCSTR pszDirectoryName, ULARGE_INTEGER* pulFreeBytesAvailableToCaller, ULARGE_INTEGER* pulTotalNumberOfBytes, ULARGE_INTEGER* pulTotalNumberOfFreeBytes);
SHSTDAPI_(BOOL) SHGetDiskFreeSpaceExW(LPCWSTR pszDirectoryName, ULARGE_INTEGER* pulFreeBytesAvailableToCaller, ULARGE_INTEGER* pulTotalNumberOfBytes, ULARGE_INTEGER* pulTotalNumberOfFreeBytes);
#ifdef UNICODE
#define SHGetDiskFreeSpaceEx  SHGetDiskFreeSpaceExW
#else
#define SHGetDiskFreeSpaceEx  SHGetDiskFreeSpaceExA
#endif (* !UNICODE *)
SHSTDAPI_(BOOL) SHGetNewLinkInfoA(LPCSTR pszLinkTo, LPCSTR pszDir, LPSTR pszName, BOOL *pfMustCopy, UINT uFlags);
SHSTDAPI_(BOOL) SHGetNewLinkInfoW(LPCWSTR pszLinkTo, LPCWSTR pszDir, LPWSTR pszName, BOOL *pfMustCopy, UINT uFlags);
#ifdef UNICODE
#define SHGetNewLinkInfo  SHGetNewLinkInfoW
#else
#define SHGetNewLinkInfo  SHGetNewLinkInfoA
#endif (* !UNICODE *)

#define SHGNLI_PIDL             =000000001     (* pszLinkTo is a pidl *)
#define SHGNLI_PREFIXNAME       =000000002     (* Make name "Shortcut to xxx" *)
#define SHGNLI_NOUNIQUE         =000000004     (* don't do the unique name generation *)



(* End SHGetFileInfo *)

(* Printer stuff *)
#define PRINTACTION_OPEN           0
#define PRINTACTION_PROPERTIES     1
#define PRINTACTION_NETINSTALL     2
#define PRINTACTION_NETINSTALLLINK 3
#define PRINTACTION_TESTPAGE       4
#define PRINTACTION_OPENNETPRN     5
#ifdef WINNT
#define PRINTACTION_DOCUMENTDEFAULTS 6
#define PRINTACTION_SERVERPROPERTIES 7
#endif

SHSTDAPI_(BOOL) SHInvokePrinterCommandA(HWND hWnd, UINT uAction, LPCSTR lpBuf1, LPCSTR lpBuf2, BOOL fModal);
SHSTDAPI_(BOOL) SHInvokePrinterCommandW(HWND hWnd, UINT uAction, LPCWSTR lpBuf1, LPCWSTR lpBuf2, BOOL fModal);
#ifdef UNICODE
#define SHInvokePrinterCommand  SHInvokePrinterCommandW
#else
#define SHInvokePrinterCommand  SHInvokePrinterCommandA
#endif (* !UNICODE *)


#endif /* WINVER >= =0400 */

#if (_WIN32_WINNT >= =0500) || (_WIN32_WINDOWS >= =0500)  

(*                                                                  *)
(* The SHLoadNonloadedIconOverlayIdentifiers API causes the shell's   *)
(* icon overlay manager to load any registered icon overlay             *)
(* identifers that are not currently loaded.  This is useful if an     *)
(* overlay identifier did not load at shell startup but is needed      *)
(* and can be loaded at a later time.  Identifiers already loaded      *)
(* are not affected.  Overlay identifiers implement the                *)
(* IShellIconOverlayIdentifier interface.                              *)
(*                                                                      *)
(* Returns:                                                             *)
(*      S_OK                                                            *)
(*                                                                       *)
SHSTDAPI SHLoadNonloadedIconOverlayIdentifiers(void);

(*
(* The SHIsFileAvailableOffline API determines whether a file *)
(* or folder is available for offline use.                      *)
(*                                                                *)
(* Parameters:                                                      *)
(*     pwszPath             file name to get info about               *)
(*     pdwStatus            (optional) OFFLINE_STATUS_* flags returned here *)
(*                                                                            *)
(* Returns:                                                                  *)
(*     S_OK                 File/directory is available offline, unless      *)
(*                            OFFLINE_STATUS_INCOMPLETE is returned.         *)
(*     E_INVALIDARG         Path is invalid, or not a net path               *)
(*     E_FAIL               File/directory is not available offline          *)
(*                                                                           *)
(* Notes:                                                                    *)
(*     OFFLINE_STATUS_INCOMPLETE is never returned for directories.          *)
(*     Both OFFLINE_STATUS_LOCAL and OFFLINE_STATUS_REMOTE may be returned,  *)
(*     indicating "open in both places." This is common when the server is online.*)
(*                                                                                  *)
SHSTDAPI SHIsFileAvailableOffline(LPCWSTR pwszPath, LPDWORD pdwStatus);           *)

#define OFFLINE_STATUS_LOCAL        =0001  (* If open, it's open locally         *)
#define OFFLINE_STATUS_REMOTE       =0002  (* If open, it's open remotely          *)
#define OFFLINE_STATUS_INCOMPLETE   =0004  (* The local copy is currently imcomplete.*)
                                            (* The file will not be available offline  *)
                                            (* until it has been synchronized.           *)

#endif



#ifdef __cplusplus
}
#endif  /* __cplusplus */

#include <poppack.h>

#endif  /* _INC_SHELLAPI *)
END ShellAPI.

