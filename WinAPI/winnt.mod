(******************************************************************************)
(*                                                                            *)
(**)                        DEFINITION WinNT;                               (**)
(*                                                                            *)
(******************************************************************************)
(* Copyright (c) 1993, Robinson Associates                                    *)
(*                     Red Lion House                                         *)
(*                     St Mary's Street                                       *)
(*                     PAINSWICK                                              *)
(*                     Glos                                                   *)
(*                     GL6  6QR                                               *)
(*                     Tel:    (+44) (0)1452 813 699                          *)
(*                     Fax:    (+44) (0)1452 812 912                          *)
(*                     e-Mail: Oberon@robinsons.co.uk                         *)
(******************************************************************************)
(*  05-30-1997 rel. 1.0 by Christian Wohlfahrtstaetter                        *)
(******************************************************************************)
(*     winnt.h                                                                *)
(*                                                                            *)
(* Abstract:                                                                  *)
(*                                                                            *)
(*     This module defines the 32-Bit Windows types and constants that are    *)
(*     defined by NT, but exposed through the Win32 API.                      *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

IMPORT WD := WinDef;

CONST 
  ANYSIZE_ARRAY = 1;

  APPLICATION_ERROR_MASK = 20000000H;
  ERROR_SEVERITY_SUCCESS = 0H;
  ERROR_SEVERITY_INFORMATIONAL = 40000000H;
  ERROR_SEVERITY_WARNING = MIN(LONGINT);
  ERROR_SEVERITY_ERROR = -40000000H;
 
  MAXLONGLONG = 0H;                    (* (0x7fffffffffffffff) *)

  UNICODE_NULL = 0;
 
  MINCHAR = 80H;
  MAXCHAR = 7FH;
  MAXIMUM_SUSPEND_COUNT = MAXCHAR;     (*  Maximum times thread can be suspended *)
  MINSHORT = 8000H;
  MAXSHORT = 7FFFH;
  MINLONG = MIN(LONGINT);
  MAXLONG = 7FFFFFFFH;
  MAXBYTE = 0FFH;
  MAXWORD = 0FFFFH;
  MAXDWORD = -1H;

(*  *)
(*   Language IDs. *)
(*  *)
(*   The following two combinations of primary language ID and *)
(*   sublanguage ID have special semantics: *)
(*  *)
(*     Primary Language ID   Sublanguage ID      Result *)
(*     -------------------   ---------------     ------------------------ *)
(*     LANG_NEUTRAL          SUBLANG_NEUTRAL     Language neutral *)
(*     LANG_NEUTRAL          SUBLANG_DEFAULT     User default language *)
(*     LANG_NEUTRAL          SUBLANG_SYS_DEFAULT System default language *)
(*  *)
(*  *)
(*   Primary language IDs. *)
(*  *)
 
  LANG_NEUTRAL = 0H;
  LANG_AFRIKAANS = 36H;
  LANG_ALBANIAN = 1CH;
  LANG_BASQUE = 2DH;
  LANG_BELARUSIAN = 23H;
  LANG_BULGARIAN = 2H;
  LANG_CATALAN = 3H;
  LANG_CHINESE = 4H;
  LANG_CROATIAN = 1AH;
  LANG_CZECH = 5H;
  LANG_DANISH = 6H;
  LANG_DUTCH = 13H;
  LANG_ENGLISH = 9H;
  LANG_ESTONIAN = 25H;
  LANG_FAEROESE = 38H;
  LANG_FINNISH = 0BH;
  LANG_FRENCH = 0CH;
  LANG_GERMAN = 7H;
  LANG_GREEK = 8H;
  LANG_HUNGARIAN = 0EH;
  LANG_ICELANDIC = 0FH;
  LANG_INDONESIAN = 21H;
  LANG_ITALIAN = 10H;
  LANG_JAPANESE = 11H;
  LANG_KOREAN = 12H;
  LANG_LATVIAN = 26H;
  LANG_LITHUANIAN = 27H;
  LANG_NORWEGIAN = 14H;
  LANG_POLISH = 15H;
  LANG_PORTUGUESE = 16H;
  LANG_ROMANIAN = 18H;
  LANG_RUSSIAN = 19H;
  LANG_SERBIAN = 1AH;
  LANG_SLOVAK = 1BH;
  LANG_SLOVENIAN = 24H;
  LANG_SPANISH = 0AH;
  LANG_SWEDISH = 1DH;
  LANG_THAI = 1EH;
  LANG_TURKISH = 1FH;
  LANG_UKRAINIAN = 22H;
  LANG_VIETNAMESE = 2AH;

(*  *)
(*   Sublanguage IDs. *)
(*  *)
(*   The name immediately following SUBLANG_ dictates which primary *)
(*   language ID that sublanguage ID can be combined with to form a *)
(*   valid language ID. *)
(*  *)
  SUBLANG_NEUTRAL = 0H;                (*  language neutral *)
  SUBLANG_DEFAULT = 1H;                (*  user default *)
  SUBLANG_SYS_DEFAULT = 2H;            (*  system default *)
  SUBLANG_CHINESE_TRADITIONAL = 1H;    (*  Chinese (Taiwan) *)
  SUBLANG_CHINESE_SIMPLIFIED = 2H;     (*  Chinese (PR China) *)
  SUBLANG_CHINESE_HONGKONG = 3H;       (*  Chinese (Hong Kong) *)
  SUBLANG_CHINESE_SINGAPORE = 4H;      (*  Chinese (Singapore) *)
  SUBLANG_DUTCH = 1H;                  (*  Dutch *)
  SUBLANG_DUTCH_BELGIAN = 2H;          (*  Dutch (Belgian) *)
  SUBLANG_ENGLISH_US = 1H;             (*  English (USA) *)
  SUBLANG_ENGLISH_UK = 2H;             (*  English (UK) *)
  SUBLANG_ENGLISH_AUS = 3H;            (*  English (Australian) *)
  SUBLANG_ENGLISH_CAN = 4H;            (*  English (Canadian) *)
  SUBLANG_ENGLISH_NZ = 5H;             (*  English (New Zealand) *)
  SUBLANG_ENGLISH_EIRE = 6H;           (*  English (Irish) *)
  SUBLANG_ENGLISH_SOUTH_AFRICA = 7H;   (*  English (South Africa) *)
  SUBLANG_ENGLISH_JAMAICA = 8H;        (*  English (Jamaica) *)
  SUBLANG_ENGLISH_CARIBBEAN = 9H;      (*  English (Caribbean) *)
  SUBLANG_ENGLISH_BELIZE = 0AH;        (*  English (Belize) *)
  SUBLANG_ENGLISH_TRINIDAD = 0BH;      (*  English (Trinidad) *)
  SUBLANG_FRENCH = 1H;                 (*  French *)
  SUBLANG_FRENCH_BELGIAN = 2H;         (*  French (Belgian) *)
  SUBLANG_FRENCH_CANADIAN = 3H;        (*  French (Canadian) *)
  SUBLANG_FRENCH_SWISS = 4H;           (*  French (Swiss) *)
  SUBLANG_FRENCH_LUXEMBOURG = 5H;      (*  French (Luxembourg) *)
  SUBLANG_GERMAN = 1H;                 (*  German *)
  SUBLANG_GERMAN_SWISS = 2H;           (*  German (Swiss) *)
  SUBLANG_GERMAN_AUSTRIAN = 3H;        (*  German (Austrian) *)
  SUBLANG_GERMAN_LUXEMBOURG = 4H;      (*  German (Luxembourg) *)
  SUBLANG_GERMAN_LIECHTENSTEIN = 5H;   (*  German (Liechtenstein) *)
  SUBLANG_ITALIAN = 1H;                (*  Italian *)
  SUBLANG_ITALIAN_SWISS = 2H;          (*  Italian (Swiss) *)
  SUBLANG_NORWEGIAN_BOKMAL = 1H;       (*  Norwegian (Bokmal) *)
  SUBLANG_NORWEGIAN_NYNORSK = 2H;      (*  Norwegian (Nynorsk) *)
  SUBLANG_PORTUGUESE = 2H;             (*  Portuguese *)
  SUBLANG_PORTUGUESE_BRAZILIAN = 1H;   (*  Portuguese (Brazilian) *)
  SUBLANG_SERBIAN_LATIN = 2H;          (*  Serbian (Latin) *)
  SUBLANG_SERBIAN_CYRILLIC = 3H;       (*  Serbian (Cyrillic) *)
  SUBLANG_SPANISH = 1H;                (*  Spanish (Castilian) *)
  SUBLANG_SPANISH_MEXICAN = 2H;        (*  Spanish (Mexican) *)
  SUBLANG_SPANISH_MODERN = 3H;         (*  Spanish (Modern) *)
  SUBLANG_SPANISH_GUATEMALA = 4H;      (*  Spanish (Guatemala) *)
  SUBLANG_SPANISH_COSTA_RICA = 5H;     (*  Spanish (Costa Rica) *)
  SUBLANG_SPANISH_PANAMA = 6H;         (*  Spanish (Panama) *)
  SUBLANG_SPANISH_DOMINICAN_REPUBLIC = 7H;   (*  Spanish (Dominican Republic) *)
  SUBLANG_SPANISH_VENEZUELA = 8H;      (*  Spanish (Venezuela) *)
  SUBLANG_SPANISH_COLOMBIA = 9H;       (*  Spanish (Colombia) *)
  SUBLANG_SPANISH_PERU = 0AH;          (*  Spanish (Peru) *)
  SUBLANG_SPANISH_ARGENTINA = 0BH;     (*  Spanish (Argentina) *)
  SUBLANG_SPANISH_ECUADOR = 0CH;       (*  Spanish (Ecuador) *)
  SUBLANG_SPANISH_CHILE = 0DH;         (*  Spanish (Chile) *)
  SUBLANG_SPANISH_URUGUAY = 0EH;       (*  Spanish (Uruguay) *)
  SUBLANG_SPANISH_PARAGUAY = 0FH;      (*  Spanish (Paraguay) *)
  SUBLANG_SPANISH_BOLIVIA = 10H;       (*  Spanish (Bolivia) *)
  SUBLANG_SPANISH_EL_SALVADOR = 11H;   (*  Spanish (El Salvador) *)
  SUBLANG_SPANISH_HONDURAS = 12H;      (*  Spanish (Honduras) *)
  SUBLANG_SPANISH_NICARAGUA = 13H;     (*  Spanish (Nicaragua) *)
  SUBLANG_SPANISH_PUERTO_RICO = 14H;   (*  Spanish (Puerto Rico) *)
  SUBLANG_SWEDISH = 1H;                (*  Swedish *)
  SUBLANG_SWEDISH_FINLAND = 2H;        (*  Swedish (Finland) *)

(*  *)
(*   Sorting IDs. *)
(*  *)
  SORT_DEFAULT = 0H;                   (*  sorting default *)
  SORT_JAPANESE_XJIS = 0H;             (*  Japanese XJIS order *)
  SORT_JAPANESE_UNICODE = 1H;          (*  Japanese Unicode order *)
  SORT_CHINESE_BIG5 = 0H;              (*  Chinese BIG5 order *)
  SORT_CHINESE_PRCP = 0H;              (*  PRC Chinese Phonetic order *)
  SORT_CHINESE_UNICODE = 1H;           (*  Chinese Unicode order *)
  SORT_CHINESE_PRC = 2H;               (*  PRC Chinese Stroke Count order *)
  SORT_KOREAN_KSC = 0H;                (*  Korean KSC order *)
  SORT_KOREAN_UNICODE = 1H;            (*  Korean Unicode order *)
  SORT_GERMAN_PHONE_BOOK = 1H;         (*  German Phone Book order *)

(*  *)
(*   A language ID is a 16 bit value which is the combination of a *)
(*   primary language ID and a secondary language ID.  The bits are *)
(*   allocated as follows: *)
(*  *)
(*        +-----------------------+-------------------------+ *)
(*        |     Sublanguage ID    |   Primary Language ID   | *)
(*        +-----------------------+-------------------------+ *)
(*         15                   10 9                       0   bit *)

  NLS_VALID_LOCALE_MASK = 0FFFFFH;


(* lint -save -e767  *)

  STATUS_WAIT_0 = 0;
  STATUS_ABANDONED_WAIT_0 = 128;
  STATUS_USER_APC = 192;
  STATUS_TIMEOUT = 258;
  STATUS_PENDING = 259;
  STATUS_SEGMENT_NOTIFICATION = 1073741829;
  STATUS_GUARD_PAGE_VIOLATION = -2147483647;
  STATUS_DATATYPE_MISALIGNMENT = -2147483646;
  STATUS_BREAKPOINT = -2147483645;
  STATUS_SINGLE_STEP = -2147483644;
  STATUS_ACCESS_VIOLATION = -1073741819;
  STATUS_IN_PAGE_ERROR = -1073741818;
  STATUS_INVALID_HANDLE = -1073741816;
  STATUS_NO_MEMORY = -1073741801;
  STATUS_ILLEGAL_INSTRUCTION = -1073741795;
  STATUS_NONCONTINUABLE_EXCEPTION = -1073741787;
  STATUS_INVALID_DISPOSITION = -1073741786;
  STATUS_ARRAY_BOUNDS_EXCEEDED = -1073741684;
  STATUS_FLOAT_DENORMAL_OPERAND = -1073741683;
  STATUS_FLOAT_DIVIDE_BY_ZERO = -1073741682;
  STATUS_FLOAT_INEXACT_RESULT = -1073741681;
  STATUS_FLOAT_INVALID_OPERATION = -1073741680;
  STATUS_FLOAT_OVERFLOW = -1073741679;
  STATUS_FLOAT_STACK_CHECK = -1073741678;
  STATUS_FLOAT_UNDERFLOW = -1073741677;
  STATUS_INTEGER_DIVIDE_BY_ZERO = -1073741676;
  STATUS_INTEGER_OVERFLOW = -1073741675;
  STATUS_PRIVILEGED_INSTRUCTION = -1073741674;
  STATUS_STACK_OVERFLOW = -1073741571;
  STATUS_CONTROL_C_EXIT = -1073741510;

(* lint -restore  *)
  MAXIMUM_WAIT_OBJECTS = 64;           (*  Maximum number of wait objects *)

  EXCEPTION_NONCONTINUABLE = 1H;       (*  Noncontinuable exception *)
  EXCEPTION_MAXIMUM_PARAMETERS = 15;   (*  maximum number of exception parameters *)

  PROCESS_TERMINATE = 1H;
  PROCESS_CREATE_THREAD = 2H;
  PROCESS_VM_OPERATION = 8H;
  PROCESS_VM_READ = 10H;
  PROCESS_VM_WRITE = 20H;
  PROCESS_DUP_HANDLE = 40H;
  PROCESS_CREATE_PROCESS = 80H;
  PROCESS_SET_QUOTA = 100H;
  PROCESS_SET_INFORMATION = 200H;
  PROCESS_QUERY_INFORMATION = 400H;
  MAXIMUM_PROCESSORS = 32;
  THREAD_TERMINATE = 1H;
  THREAD_SUSPEND_RESUME = 2H;
  THREAD_GET_CONTEXT = 8H;
  THREAD_SET_CONTEXT = 10H;
  THREAD_SET_INFORMATION = 20H;
  THREAD_QUERY_INFORMATION = 40H;
  THREAD_SET_THREAD_TOKEN = 80H;
  THREAD_IMPERSONATE = 100H;
  THREAD_DIRECT_IMPERSONATION = 200H;

(*  begin_ntddk *)
(*  end_ntddk *)
  TLS_MINIMUM_AVAILABLE = 64;

  THREAD_BASE_PRIORITY_LOWRT = 15;     (*  value that gets a thread to LowRealtime-1 *)
  THREAD_BASE_PRIORITY_MAX = 2;        (*  maximum thread base priority boost *)
  THREAD_BASE_PRIORITY_MIN = -2;       (*  minimum thread base priority boost *)
  THREAD_BASE_PRIORITY_IDLE = -15;     (*  value that gets a thread to idle *)

  EVENT_MODIFY_STATE = 2H;
  MUTANT_QUERY_STATE = 1H;
  SEMAPHORE_MODIFY_STATE = 2H;
  TIME_ZONE_ID_UNKNOWN = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
  PROCESSOR_INTEL_386 = 386;
  PROCESSOR_INTEL_486 = 486;
  PROCESSOR_INTEL_PENTIUM = 586;
  PROCESSOR_MIPS_R4000 = 4000;
  PROCESSOR_ALPHA_21064 = 21064;
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_MIPS = 1;
  PROCESSOR_ARCHITECTURE_ALPHA = 2;
  PROCESSOR_ARCHITECTURE_PPC = 3;
  PROCESSOR_ARCHITECTURE_UNKNOWN = 0FFFFH;

  SECTION_QUERY = 1H;
  SECTION_MAP_WRITE = 2H;
  SECTION_MAP_READ = 4H;
  SECTION_MAP_EXECUTE = 8H;
  SECTION_EXTEND_SIZE = 10H;
  PAGE_NOACCESS = 1H;
  PAGE_READONLY = 2H;
  PAGE_READWRITE = 4H;
  PAGE_WRITECOPY = 8H;
  PAGE_EXECUTE = 10H;
  PAGE_EXECUTE_READ = 20H;
  PAGE_EXECUTE_READWRITE = 40H;
  PAGE_EXECUTE_WRITECOPY = 80H;
  PAGE_GUARD = 100H;
  PAGE_NOCACHE = 200H;
  MEM_COMMIT = 1000H;
  MEM_RESERVE = 2000H;
  MEM_DECOMMIT = 4000H;
  MEM_RELEASE = 8000H;
  MEM_FREE = 10000H;
  MEM_PRIVATE = 20000H;
  MEM_MAPPED = 40000H;
  MEM_RESET = 80000H;
  MEM_TOP_DOWN = 100000H;
  SEC_FILE = 800000H;
  SEC_IMAGE = 1000000H;
  MEM_IMAGE = SEC_IMAGE;
  SEC_RESERVE = 4000000H;
  SEC_COMMIT = 8000000H;
  SEC_NOCACHE = 10000000H;

(*  *)
(*  Define access rights to files and directories *)
(*  *)
(*  *)
(*  The FILE_READ_DATA and FILE_WRITE_DATA constants are also defined in *)
(*  devioctl.h as FILE_READ_ACCESS and FILE_WRITE_ACCESS. The values for these *)
(*  constants *MUST* always be in sync. *)
(*  The values are redefined in devioctl.h because they must be available to *)
(*  both DOS and NT. *)
(*  *)
  FILE_READ_DATA = 1H;                 (*  file & pipe *)
  FILE_LIST_DIRECTORY = 1H;            (*  directory *)
  FILE_WRITE_DATA = 2H;                (*  file & pipe *)
  FILE_ADD_FILE = 2H;                  (*  directory *)
  FILE_APPEND_DATA = 4H;               (*  file *)
  FILE_ADD_SUBDIRECTORY = 4H;          (*  directory *)
  FILE_CREATE_PIPE_INSTANCE = 4H;      (*  named pipe *)
  FILE_READ_EA = 8H;                   (*  file & directory *)
  FILE_WRITE_EA = 10H;                 (*  file & directory *)
  FILE_EXECUTE = 20H;                  (*  file *)
  FILE_TRAVERSE = 20H;                 (*  directory *)
  FILE_DELETE_CHILD = 40H;             (*  directory *)
  FILE_READ_ATTRIBUTES = 80H;          (*  all *)
  FILE_WRITE_ATTRIBUTES = 100H;        (*  all *)
  FILE_SHARE_READ = 1H;
  FILE_SHARE_WRITE = 2H;
  FILE_SHARE_DELETE = 4H;
  FILE_ATTRIBUTE_READONLY = 1H;
  FILE_ATTRIBUTE_HIDDEN = 2H;
  FILE_ATTRIBUTE_SYSTEM = 4H;
  FILE_ATTRIBUTE_DIRECTORY = 10H;
  FILE_ATTRIBUTE_ARCHIVE = 20H;
  FILE_ATTRIBUTE_NORMAL = 80H;
  FILE_ATTRIBUTE_TEMPORARY = 100H;
  FILE_ATTRIBUTE_COMPRESSED = 800H;
  FILE_ATTRIBUTE_OFFLINE = 1000H;
  FILE_NOTIFY_CHANGE_FILE_NAME = 1H;
  FILE_NOTIFY_CHANGE_DIR_NAME = 2H;
  FILE_NOTIFY_CHANGE_ATTRIBUTES = 4H;
  FILE_NOTIFY_CHANGE_SIZE = 8H;
  FILE_NOTIFY_CHANGE_LAST_WRITE = 10H;
  FILE_NOTIFY_CHANGE_LAST_ACCESS = 20H;
  FILE_NOTIFY_CHANGE_CREATION = 40H;
  FILE_NOTIFY_CHANGE_SECURITY = 100H;
  FILE_ACTION_ADDED = 1H;
  FILE_ACTION_REMOVED = 2H;
  FILE_ACTION_MODIFIED = 3H;
  FILE_ACTION_RENAMED_OLD_NAME = 4H;
  FILE_ACTION_RENAMED_NEW_NAME = 5H;
  MAILSLOT_NO_MESSAGE = -1;
  MAILSLOT_WAIT_FOREVER = -1;
  FILE_CASE_SENSITIVE_SEARCH = 1H;
  FILE_CASE_PRESERVED_NAMES = 2H;
  FILE_UNICODE_ON_DISK = 4H;
  FILE_PERSISTENT_ACLS = 8H;
  FILE_FILE_COMPRESSION = 10H;
  FILE_VOLUME_IS_COMPRESSED = 8000H;

  IO_COMPLETION_MODIFY_STATE = 2H;
  DUPLICATE_CLOSE_SOURCE = 1H;
  DUPLICATE_SAME_ACCESS = 2H;
 
  DELETE = 10000H;
  READ_CONTROL = 20000H;
  WRITE_DAC = 40000H;
  WRITE_OWNER = 80000H;
  SYNCHRONIZE = 100000H;
  STANDARD_RIGHTS_REQUIRED = 0F0000H;
  STANDARD_RIGHTS_READ = READ_CONTROL;
  STANDARD_RIGHTS_WRITE = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;
  STANDARD_RIGHTS_ALL = 1F0000H;
  SPECIFIC_RIGHTS_ALL = 0FFFFH;

(*  *)
(*  AccessSystemAcl access type *)
(*  *)
  ACCESS_SYSTEM_SECURITY = 1000000H;

(*  *)
(*  MaximumAllowed access type *)
(*  *)
  MAXIMUM_ALLOWED = 2000000H;

(*  *)
(*   These are the generic rights. *)
(*  *)
  GENERIC_READ = MIN(LONGINT);
  GENERIC_WRITE = 40000000H;
  GENERIC_EXECUTE = 20000000H;
  GENERIC_ALL = 10000000H;

  SID_REVISION = 1;                    (*  Current revision level *)
  SID_MAX_SUB_AUTHORITIES = 15;
  SID_RECOMMENDED_SUB_AUTHORITIES = 1; (*  Will change to around 6 *)

(*  in a future release. *)
(* H2D: Enumeration: _SID_NAME_USE *)
  SidTypeUser = 1;
  SidTypeGroup = 2;
  SidTypeDomain = 3;
  SidTypeAlias = 4;
  SidTypeWellKnownGroup = 5;
  SidTypeDeletedAccount = 6;
  SidTypeInvalid = 7;
  SidTypeUnknown = 8;

  SECURITY_NULL_RID = 0H;
  SECURITY_WORLD_RID = 0H;
  SECURITY_LOCAL_RID = 0H;
  SECURITY_CREATOR_OWNER_RID = 0H;
  SECURITY_CREATOR_GROUP_RID = 1H;
  SECURITY_CREATOR_OWNER_SERVER_RID = 2H;
  SECURITY_CREATOR_GROUP_SERVER_RID = 3H;

(* /////////////////////////////////////////////////////////////////////////// *)
(*                                                                          // *)
(*  NT well-known SIDs                                                      // *)
(*                                                                          // *)
(*      NT Authority          S-1-5                                         // *)
(*      Dialup                S-1-5-1                                       // *)
(*                                                                          // *)
(*      Network               S-1-5-2                                       // *)
(*      Batch                 S-1-5-3                                       // *)
(*      Interactive           S-1-5-4                                       // *)
(*      Service               S-1-5-6                                       // *)
(*      AnonymousLogon        S-1-5-7       (aka null logon session)        // *)
(*      Proxy                 S-1-5-8                                       // *)
(*      ServerLogon           S-1-5-8       (aka domain controller account) // *)
(*                                                                          // *)
(*      (Logon IDs)           S-1-5-5-X-Y                                   // *)
(*                                                                          // *)
(*      (NT non-unique IDs)   S-1-5-0x15-...                                // *)
(*                                                                          // *)
(*      (Built-in domain)     s-1-5-0x20                                    // *)
(*                                                                          // *)
(* /////////////////////////////////////////////////////////////////////////// *)
  SECURITY_DIALUP_RID = 1H;
  SECURITY_NETWORK_RID = 2H;
  SECURITY_BATCH_RID = 3H;
  SECURITY_INTERACTIVE_RID = 4H;
  SECURITY_SERVICE_RID = 6H;
  SECURITY_ANONYMOUS_LOGON_RID = 7H;
  SECURITY_PROXY_RID = 8H;
  SECURITY_SERVER_LOGON_RID = 9H;
  SECURITY_LOGON_IDS_RID = 5H;
  SECURITY_LOGON_IDS_RID_COUNT = 3;
  SECURITY_LOCAL_SYSTEM_RID = 12H;
  SECURITY_NT_NON_UNIQUE = 15H;
  SECURITY_BUILTIN_DOMAIN_RID = 20H;

(* /////////////////////////////////////////////////////////////////////////// *)
(*                                                                          // *)
(*  well-known domain relative sub-authority values (RIDs)...               // *)
(*                                                                          // *)
(* /////////////////////////////////////////////////////////////////////////// *)
(*  Well-known users ... *)
  DOMAIN_USER_RID_ADMIN = 1F4H;
  DOMAIN_USER_RID_GUEST = 1F5H;

(*  well-known groups ... *)
  DOMAIN_GROUP_RID_ADMINS = 200H;
  DOMAIN_GROUP_RID_USERS = 201H;
  DOMAIN_GROUP_RID_GUESTS = 202H;

(*  well-known aliases ... *)
  DOMAIN_ALIAS_RID_ADMINS = 220H;
  DOMAIN_ALIAS_RID_USERS = 221H;
  DOMAIN_ALIAS_RID_GUESTS = 222H;
  DOMAIN_ALIAS_RID_POWER_USERS = 223H;
  DOMAIN_ALIAS_RID_ACCOUNT_OPS = 224H;
  DOMAIN_ALIAS_RID_SYSTEM_OPS = 225H;
  DOMAIN_ALIAS_RID_PRINT_OPS = 226H;
  DOMAIN_ALIAS_RID_BACKUP_OPS = 227H;
  DOMAIN_ALIAS_RID_REPLICATOR = 228H;

(*  *)
(*  Allocate the System Luid.  The first 1000 LUIDs are reserved. *)
(*  Use #999 here (0x3E7 = 999) *)
(*  *)
(*  end_ntifs *)
(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                           User and Group related SID attributes     // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  *)
(*  Group attributes *)
(*  *)
  SE_GROUP_MANDATORY = 1H;
  SE_GROUP_ENABLED_BY_DEFAULT = 2H;
  SE_GROUP_ENABLED = 4H;
  SE_GROUP_OWNER = 8H;
  SE_GROUP_LOGON_ID = -40000000H;

(*  *)
(*  User attributes *)
(*  *)
(*  (None yet defined.) *)
(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                          ACL  and  ACE                              // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  *)
(*   Define an ACL and the ACE format.  The structure of an ACL header *)
(*   followed by one or more ACEs.  Pictorally the structure of an ACL header *)
(*   is as follows: *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +-------------------------------+---------------+---------------+ *)
(*       |            AclSize            |      Sbz1     |  AclRevision  | *)
(*       +-------------------------------+---------------+---------------+ *)
(*       |              Sbz2             |           AceCount            | *)
(*       +-------------------------------+-------------------------------+ *)
(*  *)
(*   The current AclRevision is defined to be ACL_REVISION. *)
(*  *)
(*   AclSize is the size, in bytes, allocated for the ACL.  This includes *)
(*   the ACL header, ACES, and remaining free space in the buffer. *)
(*  *)
(*   AceCount is the number of ACES in the ACL. *)
(*  *)
(*  begin_ntddk begin_ntifs *)
(*  This is the *current* ACL revision *)
  ACL_REVISION = 2;

(*  This is the history of ACL revisions.  Add a new one whenever *)
(*  ACL_REVISION is updated *)
  ACL_REVISION1 = 1;
  ACL_REVISION2 = 2;
  ACL_REVISION3 = 3;
(*  *)
(*   The following are the predefined ace types that go into the AceType *)
(*   field of an Ace header. *)
(*  *)


  ACCESS_ALLOWED_ACE_TYPE = 0H;
  ACCESS_DENIED_ACE_TYPE = 1H;
  SYSTEM_AUDIT_ACE_TYPE = 2H;
  SYSTEM_ALARM_ACE_TYPE = 3H;
  ACCESS_ALLOWED_COMPOUND_ACE_TYPE = 4H;

(*  *)
(*   The following are the inherit flags that go into the AceFlags field *)
(*   of an Ace header. *)
(*  *)
  OBJECT_INHERIT_ACE = 1H;
  CONTAINER_INHERIT_ACE = 2H;
  NO_PROPAGATE_INHERIT_ACE = 4H;
  INHERIT_ONLY_ACE = 8H;
  VALID_INHERIT_FLAGS = 0FH;

(*   The following are the currently defined ACE flags that go into the *)
(*   AceFlags field of an ACE header.  Each ACE type has its own set of *)
(*   AceFlags. *)
(*  *)
(*   SUCCESSFUL_ACCESS_ACE_FLAG - used only with system audit and alarm ACE *)
(*   types to indicate that a message is generated for successful accesses. *)
(*  *)
(*   FAILED_ACCESS_ACE_FLAG - used only with system audit and alarm ACE types *)
(*   to indicate that a message is generated for failed accesses. *)
(*  *)
(*  *)
(*   SYSTEM_AUDIT and SYSTEM_ALARM AceFlags *)
(*  *)
(*   These control the signaling of audit and alarms for success or failure. *)
(*  *)
  SUCCESSFUL_ACCESS_ACE_FLAG = 40H;
  FAILED_ACCESS_ACE_FLAG = 80H;

(*  *)
(*   We'll define the structure of the predefined ACE types.  Pictorally *)
(*   the structure of the predefined ACE's is as follows: *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*       |    AceFlags   | Resd  |Inherit|    AceSize    |     AceType   | *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*       |                              Mask                             | *)
(*       +---------------------------------------------------------------+ *)
(*       |                                                               | *)
(*       +                                                               + *)
(*       |                                                               | *)
(*       +                              Sid                              + *)
(*       |                                                               | *)
(*       +                                                               + *)
(*       |                                                               | *)
(*       +---------------------------------------------------------------+ *)
(*   Mask is the access mask associated with the ACE.  This is either the *)
(*   access allowed, access denied, audit, or alarm mask. *)
(*   Sid is the Sid associated with the ACE. *)

(*  Currently defined Compound ACE types *)

  COMPOUND_ACE_IMPERSONATION = 1;

(*  *)
(*   The following declarations are used for setting and querying information *)
(*   about and ACL.  First are the various information classes available to *)
(*   the user. *)
(*  *)
(* H2D: Enumeration: _ACL_INFORMATION_CLASS *)
  AclRevisionInformation = 1;
  AclSizeInformation = 2;

(*  Current security descriptor revision value *)

  SECURITY_DESCRIPTOR_REVISION = 1;
  SECURITY_DESCRIPTOR_REVISION1 = 1;

(*  end_ntddk *)
(*  *)
(*  Minimum length, in bytes, needed to build a security descriptor *)
(*  (NOTE: This must manually be kept consistent with the) *)
(*  (sizeof(SECURITY_DESCRIPTOR)                         ) *)
(*  *)
  SECURITY_DESCRIPTOR_MIN_LENGTH = 20;
 
  SE_OWNER_DEFAULTED = 1H;
  SE_GROUP_DEFAULTED = 2H;
  SE_DACL_PRESENT = 4H;
  SE_DACL_DEFAULTED = 8H;
  SE_SACL_PRESENT = 10H;
  SE_SACL_DEFAULTED = 20H;
  SE_DACL_UNTRUSTED = 40H;
  SE_SERVER_SECURITY = 80H;
  SE_SELF_RELATIVE = 8000H;

(*   Where: *)
(*       SE_OWNER_DEFAULTED - This boolean flag, when set, indicates that the *)
(*           SID pointed to by the Owner field was provided by a *)
(*           defaulting mechanism rather than explicitly provided by the *)
(*           original provider of the security descriptor.  This may *)
(*           affect the treatment of the SID with respect to inheritence *)
(*           of an owner. *)
(*  *)
(*       SE_GROUP_DEFAULTED - This boolean flag, when set, indicates that the *)
(*           SID in the Group field was provided by a defaulting mechanism *)
(*           rather than explicitly provided by the original provider of *)
(*           the security descriptor.  This may affect the treatment of *)
(*           the SID with respect to inheritence of a primary group. *)
(*  *)
(*       SE_DACL_PRESENT - This boolean flag, when set, indicates that the *)
(*           security descriptor contains a discretionary ACL.  If this *)
(*           flag is set and the Dacl field of the SECURITY_DESCRIPTOR is *)
(*           null, then a null ACL is explicitly being specified. *)
(*  *)
(*       SE_DACL_DEFAULTED - This boolean flag, when set, indicates that the *)
(*           ACL pointed to by the Dacl field was provided by a defaulting *)
(*           mechanism rather than explicitly provided by the original *)
(*           provider of the security descriptor.  This may affect the *)
(*           treatment of the ACL with respect to inheritence of an ACL. *)
(*           This flag is ignored if the DaclPresent flag is not set. *)
(*  *)
(*       SE_SACL_PRESENT - This boolean flag, when set,  indicates that the *)
(*           security descriptor contains a system ACL pointed to by the *)
(*           Sacl field.  If this flag is set and the Sacl field of the *)
(*           SECURITY_DESCRIPTOR is null, then an empty (but present) *)
(*           ACL is being specified. *)
(*  *)
(*       SE_SACL_DEFAULTED - This boolean flag, when set, indicates that the *)
(*           ACL pointed to by the Sacl field was provided by a defaulting *)
(*           mechanism rather than explicitly provided by the original *)
(*           provider of the security descriptor.  This may affect the *)
(*           treatment of the ACL with respect to inheritence of an ACL. *)
(*           This flag is ignored if the SaclPresent flag is not set. *)
(*  *)
(*       SE_DACL_TRUSTED - This boolean flag, when set, indicates that the *)
(*           ACL pointed to by the Dacl field was provided by a trusted source *)
(*           and does not require any editing of compound ACEs.  If this flag *)
(*           is not set and a compound ACE is encountered, the system will *)
(*           substitute known valid SIDs for the server SIDs in the ACEs. *)
(*  *)
(*       SE_SERVER_SECURITY - This boolean flag, when set, indicates that the *)
(*          caller wishes the system to create a Server ACL based on the *)
(*          input ACL, regardess of its source (explicit or defaulting. *)
(*          This is done by replacing all of the GRANT ACEs with compound *)
(*          ACEs granting the current server.  This flag is only *)
(*          meaningful if the subject is impersonating. *)
(*  *)
(*       SE_SELF_RELATIVE - This boolean flag, when set, indicates that the *)
(*           security descriptor is in self-relative form.  In this form, *)
(*           all fields of the security descriptor are contiguous in memory *)
(*           and all pointer fields are expressed as offsets from the *)
(*           beginning of the security descriptor.  This form is useful *)
(*           for treating security descriptors as opaque data structures *)
(*           for transmission in communication protocol or for storage on *)
(*           secondary media. *)

(*  Pictorially the structure of a security descriptor is as follows: *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +---------------------------------------------------------------+ *)
(*       |            Control            |Reserved1 (SBZ)|   Revision    | *)
(*       +---------------------------------------------------------------+ *)
(*       |                            Owner                              | *)
(*       +---------------------------------------------------------------+ *)
(*       |                            Group                              | *)
(*       +---------------------------------------------------------------+ *)
(*       |                            Sacl                               | *)
(*       +---------------------------------------------------------------+ *)
(*       |                            Dacl                               | *)
(*       +---------------------------------------------------------------+ *)
(*  *)

(*  *)
(*  Privilege attributes *)
(*  *)

 
  SE_PRIVILEGE_ENABLED_BY_DEFAULT = 1H;
  SE_PRIVILEGE_ENABLED = 2H;
  SE_PRIVILEGE_USED_FOR_ACCESS = MIN(LONGINT);

(*  *)
(*  Privilege Set Control flags *)
(*  *)
  PRIVILEGE_SET_ALL_NECESSARY = 1;

(* H2D: Enumeration: _SECURITY_IMPERSONATION_LEVEL *)

  SecurityAnonymous = 0;
  SecurityIdentification = 1;
  SecurityImpersonation = 2;
  SecurityDelegation = 3;


  SECURITY_MAX_IMPERSONATION_LEVEL = SecurityDelegation;
  DEFAULT_IMPERSONATION_LEVEL = SecurityImpersonation;

(* ////////////////////////////////////////////////////////////////// *)
(*                                                                 // *)
(*            Token Object Definitions                             // *)
(*                                                                 // *)
(*                                                                 // *)
(* ////////////////////////////////////////////////////////////////// *)
(*  *)
(*  Token Specific Access Rights. *)
(*  *)

 
  TOKEN_ASSIGN_PRIMARY = 1H;
  TOKEN_DUPLICATE = 2H;
  TOKEN_IMPERSONATE = 4H;
  TOKEN_QUERY = 8H;
  TOKEN_QUERY_SOURCE = 10H;
  TOKEN_ADJUST_PRIVILEGES = 20H;
  TOKEN_ADJUST_GROUPS = 40H;
  TOKEN_ADJUST_DEFAULT = 80H;
  TOKEN_ALL_ACCESS = 983295;
  TOKEN_READ = 131080;
  TOKEN_WRITE = 131296;
  TOKEN_EXECUTE = STANDARD_RIGHTS_EXECUTE;

(*  begin_ntifs *)
(*  *)
(*  Token Types *)
(*  *)
(* H2D: Enumeration: _TOKEN_TYPE *)
  TokenPrimary = 1;
  TokenImpersonation = 2;
(*  *)
(*  Token Information Classes. *)
(*  *)
(* H2D: Enumeration: _TOKEN_INFORMATION_CLASS *)
 
  TokenUser = 1;
  TokenGroups = 2;
  TokenPrivileges = 3;
  TokenOwner = 4;
  TokenPrimaryGroup = 5;
  TokenDefaultDacl = 6;
  TokenSource = 7;
  TokenType = 8;
  TokenImpersonationLevel = 9;
  TokenStatistics = 10;
 
  TOKEN_SOURCE_LENGTH = 8;

  SECURITY_DYNAMIC_TRACKING = WD.True;
  SECURITY_STATIC_TRACKING = WD.False;

  OWNER_SECURITY_INFORMATION = 1H;
  GROUP_SECURITY_INFORMATION = 2H;
  DACL_SECURITY_INFORMATION = 4H;
  SACL_SECURITY_INFORMATION = 8H;

(*  *)
(*  Image Format *)
(*  *)
  IMAGE_DOS_SIGNATURE = 5A4DH;         (*  MZ *)
  IMAGE_OS2_SIGNATURE = 454EH;         (*  NE *)
  IMAGE_OS2_SIGNATURE_LE = 454CH;      (*  LE *)
  IMAGE_VXD_SIGNATURE = 454CH;         (*  LE *)
  IMAGE_NT_SIGNATURE = 4550H;          (*  PE00 *)

  IMAGE_SIZEOF_FILE_HEADER = 20;
  IMAGE_FILE_RELOCS_STRIPPED = 1H;     (*  Relocation info stripped from file. *)
  IMAGE_FILE_EXECUTABLE_IMAGE = 2H;    (*  File is executable  (i.e. no unresolved externel references). *)
  IMAGE_FILE_LINE_NUMS_STRIPPED = 4H;  (*  Line nunbers stripped from file. *)
  IMAGE_FILE_LOCAL_SYMS_STRIPPED = 8H; (*  Local symbols stripped from file. *)
  IMAGE_FILE_BYTES_REVERSED_LO = 80H;  (*  Bytes of machine word are reversed. *)
  IMAGE_FILE_32BIT_MACHINE = 100H;     (*  32 bit word machine. *)
  IMAGE_FILE_DEBUG_STRIPPED = 200H;    (*  Debugging info stripped from file in .DBG file *)
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = 400H;   (*  If Image is on removable media, copy and run from the swap file. *)
  IMAGE_FILE_NET_RUN_FROM_SWAP = 800H; (*  If Image is on Net, copy and run from the swap file. *)
  IMAGE_FILE_SYSTEM = 1000H;           (*  System File. *)
  IMAGE_FILE_DLL = 2000H;              (*  File is a DLL. *)
  IMAGE_FILE_UP_SYSTEM_ONLY = 4000H;   (*  File should only be run on a UP machine *)
  IMAGE_FILE_BYTES_REVERSED_HI = 8000H;(*  Bytes of machine word are reversed. *)
  IMAGE_FILE_MACHINE_UNKNOWN = 0;
  IMAGE_FILE_MACHINE_I386 = 14CH;      (*  Intel 386. *)
  IMAGE_FILE_MACHINE_R3000 = 162H;     (*  MIPS little-endian, 0x160 big-endian *)
  IMAGE_FILE_MACHINE_R4000 = 166H;     (*  MIPS little-endian *)
  IMAGE_FILE_MACHINE_R10000 = 168H;    (*  MIPS little-endian *)
  IMAGE_FILE_MACHINE_ALPHA = 184H;     (*  Alpha_AXP *)
  IMAGE_FILE_MACHINE_POWERPC = 1F0H;   (*  IBM PowerPC Little-Endian *)
 
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  IMAGE_SIZEOF_ROM_OPTIONAL_HEADER = 56;
  IMAGE_SIZEOF_STD_OPTIONAL_HEADER = 28;
  IMAGE_SIZEOF_NT_OPTIONAL_HEADER = 224;
  IMAGE_NT_OPTIONAL_HDR_MAGIC = 10BH;
  IMAGE_ROM_OPTIONAL_HDR_MAGIC = 107H;

(*  Subsystem Values *)
  IMAGE_SUBSYSTEM_UNKNOWN = 0;         (*  Unknown subsystem. *)
  IMAGE_SUBSYSTEM_NATIVE = 1;          (*  Image doesn't require a subsystem. *)
  IMAGE_SUBSYSTEM_WINDOWS_GUI = 2;     (*  Image runs in the Windows GUI subsystem. *)
  IMAGE_SUBSYSTEM_WINDOWS_CUI = 3;     (*  Image runs in the Windows character subsystem. *)
  IMAGE_SUBSYSTEM_OS2_CUI = 5;         (*  image runs in the OS/2 character subsystem. *)
  IMAGE_SUBSYSTEM_POSIX_CUI = 7;       (*  image run  in the Posix character subsystem. *)
  IMAGE_SUBSYSTEM_RESERVED8 = 8;       (*  image run  in the 8 subsystem. *)

(*  Directory Entries *)
  IMAGE_DIRECTORY_ENTRY_EXPORT = 0;    (*  Export Directory *)
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1;    (*  Import Directory *)
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2;  (*  Resource Directory *)
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; (*  Exception Directory *)
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4;  (*  Security Directory *)
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; (*  Base Relocation Table *)
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6;     (*  Debug Directory *)
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7; (*  Description String *)
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8; (*  Machine Value (MIPS GP) *)
  IMAGE_DIRECTORY_ENTRY_TLS = 9;       (*  TLS Directory *)
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10;   (*  Load Configuration Directory *)
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11;   (*  Bound Import Directory in headers *)
  IMAGE_DIRECTORY_ENTRY_IAT = 12;      (*  Import Address Table *)

(*  *)
(*  Section header format. *)
(*  *)
  IMAGE_SIZEOF_SHORT_NAME = 8;

 
  IMAGE_SIZEOF_SECTION_HEADER = 40;

(*  *)
(*  Section characteristics. *)
(*  *)
(*       IMAGE_SCN_TYPE_REG                   0x00000000  // Reserved. *)
(*       IMAGE_SCN_TYPE_DSECT                 0x00000001  // Reserved. *)
(*       IMAGE_SCN_TYPE_NOLOAD                0x00000002  // Reserved. *)
(*       IMAGE_SCN_TYPE_GROUP                 0x00000004  // Reserved. *)
  IMAGE_SCN_TYPE_NO_PAD = 8H;          (*  Reserved. *)

(*       IMAGE_SCN_TYPE_COPY                  0x00000010  // Reserved. *)
  IMAGE_SCN_CNT_CODE = 20H;            (*  Section contains code. *)
  IMAGE_SCN_CNT_INITIALIZED_DATA = 40H;(*  Section contains initialized data. *)
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = 80H;   (*  Section contains uninitialized data. *)
  IMAGE_SCN_LNK_OTHER = 100H;          (*  Reserved. *)
  IMAGE_SCN_LNK_INFO = 200H;           (*  Section contains comments or some other type of information. *)

(*       IMAGE_SCN_TYPE_OVER                  0x00000400  // Reserved. *)
  IMAGE_SCN_LNK_REMOVE = 800H;         (*  Section contents will not become part of image. *)
  IMAGE_SCN_LNK_COMDAT = 1000H;        (*  Section contents comdat. *)

(*                                            0x00002000  // Reserved. *)
(*       IMAGE_SCN_MEM_PROTECTED - Obsolete   0x00004000 *)
  IMAGE_SCN_MEM_FARDATA = 8000H;

(*       IMAGE_SCN_MEM_SYSHEAP  - Obsolete    0x00010000 *)
  IMAGE_SCN_MEM_PURGEABLE = 20000H;
  IMAGE_SCN_MEM_16BIT = 20000H;
  IMAGE_SCN_MEM_LOCKED = 40000H;
  IMAGE_SCN_MEM_PRELOAD = 80000H;
  IMAGE_SCN_ALIGN_1BYTES = 100000H;    (*  *)
  IMAGE_SCN_ALIGN_2BYTES = 200000H;    (*  *)
  IMAGE_SCN_ALIGN_4BYTES = 300000H;    (*  *)
  IMAGE_SCN_ALIGN_8BYTES = 400000H;    (*  *)
  IMAGE_SCN_ALIGN_16BYTES = 500000H;   (*  Default alignment if no others are specified. *)
  IMAGE_SCN_ALIGN_32BYTES = 600000H;   (*  *)
  IMAGE_SCN_ALIGN_64BYTES = 700000H;   (*  *)

(*  Unused                                    0x00800000 *)
  IMAGE_SCN_LNK_NRELOC_OVFL = 1000000H;(*  Section contains extended relocations. *)
  IMAGE_SCN_MEM_DISCARDABLE = 2000000H;(*  Section can be discarded. *)
  IMAGE_SCN_MEM_NOT_CACHED = 4000000H; (*  Section is not cachable. *)
  IMAGE_SCN_MEM_NOT_PAGED = 8000000H;  (*  Section is not pageable. *)
  IMAGE_SCN_MEM_SHARED = 10000000H;    (*  Section is shareable. *)
  IMAGE_SCN_MEM_EXECUTE = 20000000H;   (*  Section is executable. *)
  IMAGE_SCN_MEM_READ = 40000000H;      (*  Section is readable. *)
  IMAGE_SCN_MEM_WRITE = MIN(LONGINT);     (*  Section is writeable. *)

 
  IMAGE_SIZEOF_SYMBOL = 18;

(*  *)
(*  Section values. *)
(*  *)
(*  Symbols have a section number of the section in which they are *)
(*  defined. Otherwise, section numbers have the following meanings: *)
(*  *)
  IMAGE_SYM_UNDEFINED = 0;             (*  Symbol is undefined or is common. *)
  IMAGE_SYM_ABSOLUTE = -1;             (*  Symbol is an absolute value. *)
  IMAGE_SYM_DEBUG = -2;                (*  Symbol is a special debug item. *)

(*  *)
(*  Type (fundamental) values. *)
(*  *)
  IMAGE_SYM_TYPE_NULL = 0H;            (*  no type. *)
  IMAGE_SYM_TYPE_VOID = 1H;            (*  *)
  IMAGE_SYM_TYPE_CHAR = 2H;            (*  type character. *)
  IMAGE_SYM_TYPE_SHORT = 3H;           (*  type short integer. *)
  IMAGE_SYM_TYPE_INT = 4H;             (*  *)
  IMAGE_SYM_TYPE_LONG = 5H;            (*  *)
  IMAGE_SYM_TYPE_FLOAT = 6H;           (*  *)
  IMAGE_SYM_TYPE_DOUBLE = 7H;          (*  *)
  IMAGE_SYM_TYPE_STRUCT = 8H;          (*  *)
  IMAGE_SYM_TYPE_UNION = 9H;           (*  *)
  IMAGE_SYM_TYPE_ENUM = 0AH;           (*  enumeration. *)
  IMAGE_SYM_TYPE_MOE = 0BH;            (*  member of enumeration. *)
  IMAGE_SYM_TYPE_BYTE = 0CH;           (*  *)
  IMAGE_SYM_TYPE_WORD = 0DH;           (*  *)
  IMAGE_SYM_TYPE_UINT = 0EH;           (*  *)
  IMAGE_SYM_TYPE_DWORD = 0FH;          (*  *)
  IMAGE_SYM_TYPE_PCODE = 8000H;        (*  *)

(*  *)
(*  Type (derived) values. *)
(*  *)
  IMAGE_SYM_DTYPE_NULL = 0;            (*  no derived type. *)
  IMAGE_SYM_DTYPE_POINTER = 1;         (*  pointer. *)
  IMAGE_SYM_DTYPE_FUNCTION = 2;        (*  function. *)
  IMAGE_SYM_DTYPE_ARRAY = 3;           (*  array. *)

(*  *)
(*  Storage classes. *)
(*  *)
  IMAGE_SYM_CLASS_END_OF_FUNCTION = -1;
  IMAGE_SYM_CLASS_NULL = 0H;
  IMAGE_SYM_CLASS_AUTOMATIC = 1H;
  IMAGE_SYM_CLASS_EXTERNAL = 2H;
  IMAGE_SYM_CLASS_STATIC = 3H;
  IMAGE_SYM_CLASS_REGISTER = 4H;
  IMAGE_SYM_CLASS_EXTERNAL_DEF = 5H;
  IMAGE_SYM_CLASS_LABEL = 6H;
  IMAGE_SYM_CLASS_UNDEFINED_LABEL = 7H;
  IMAGE_SYM_CLASS_MEMBER_OF_STRUCT = 8H;
  IMAGE_SYM_CLASS_ARGUMENT = 9H;
  IMAGE_SYM_CLASS_STRUCT_TAG = 0AH;
  IMAGE_SYM_CLASS_MEMBER_OF_UNION = 0BH;
  IMAGE_SYM_CLASS_UNION_TAG = 0CH;
  IMAGE_SYM_CLASS_TYPE_DEFINITION = 0DH;
  IMAGE_SYM_CLASS_UNDEFINED_STATIC = 0EH;
  IMAGE_SYM_CLASS_ENUM_TAG = 0FH;
  IMAGE_SYM_CLASS_MEMBER_OF_ENUM = 10H;
  IMAGE_SYM_CLASS_REGISTER_PARAM = 11H;
  IMAGE_SYM_CLASS_BIT_FIELD = 12H;
  IMAGE_SYM_CLASS_FAR_EXTERNAL = 44H;  (*  *)
  IMAGE_SYM_CLASS_BLOCK = 64H;
  IMAGE_SYM_CLASS_FUNCTION = 65H;
  IMAGE_SYM_CLASS_END_OF_STRUCT = 66H;
  IMAGE_SYM_CLASS_FILE = 67H;

(*  new *)
  IMAGE_SYM_CLASS_SECTION = 68H;
  IMAGE_SYM_CLASS_WEAK_EXTERNAL = 69H;

(*  type packing constants *)
  N_BTMASK = 0FH;
  N_TMASK = 30H;
  N_TMASK1 = 0C0H;
  N_TMASK2 = 0F0H;
  N_BTSHFT = 4;
  N_TSHIFT = 2;

 
  IMAGE_SIZEOF_AUX_SYMBOL = 18;

(*  *)
(*  Communal selection types. *)
(*  *)
  IMAGE_COMDAT_SELECT_NODUPLICATES = 1;
  IMAGE_COMDAT_SELECT_ANY = 2;
  IMAGE_COMDAT_SELECT_SAME_SIZE = 3;
  IMAGE_COMDAT_SELECT_EXACT_MATCH = 4;
  IMAGE_COMDAT_SELECT_ASSOCIATIVE = 5;
  IMAGE_COMDAT_SELECT_LARGEST = 6;
  IMAGE_COMDAT_SELECT_NEWEST = 7;
  IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY = 1;
  IMAGE_WEAK_EXTERN_SEARCH_LIBRARY = 2;
  IMAGE_WEAK_EXTERN_SEARCH_ALIAS = 3;
 
  IMAGE_SIZEOF_RELOCATION = 10;

(*  *)
(*  I386 relocation types. *)
(*  *)
  IMAGE_REL_I386_ABSOLUTE = 0H;        (*  Reference is absolute, no relocation is necessary *)
  IMAGE_REL_I386_DIR16 = 1H;           (*  Direct 16-bit reference to the symbols virtual address *)
  IMAGE_REL_I386_REL16 = 2H;           (*  PC-relative 16-bit reference to the symbols virtual address *)
  IMAGE_REL_I386_DIR32 = 6H;           (*  Direct 32-bit reference to the symbols virtual address *)
  IMAGE_REL_I386_DIR32NB = 7H;         (*  Direct 32-bit reference to the symbols virtual address, base not included *)
  IMAGE_REL_I386_SEG12 = 9H;           (*  Direct 16-bit reference to the segment-selector bits of a 32-bit virtual address *)
  IMAGE_REL_I386_SECTION = 0AH;
  IMAGE_REL_I386_SECREL = 0BH;
  IMAGE_REL_I386_REL32 = 14H;          (*  PC-relative 32-bit reference to the symbols virtual address *)

(*  *)
(*  MIPS relocation types. *)
(*  *)
  IMAGE_REL_MIPS_ABSOLUTE = 0H;        (*  Reference is absolute, no relocation is necessary *)
  IMAGE_REL_MIPS_REFHALF = 1H;
  IMAGE_REL_MIPS_REFWORD = 2H;
  IMAGE_REL_MIPS_JMPADDR = 3H;
  IMAGE_REL_MIPS_REFHI = 4H;
  IMAGE_REL_MIPS_REFLO = 5H;
  IMAGE_REL_MIPS_GPREL = 6H;
  IMAGE_REL_MIPS_LITERAL = 7H;
  IMAGE_REL_MIPS_SECTION = 0AH;
  IMAGE_REL_MIPS_SECREL = 0BH;
  IMAGE_REL_MIPS_SECRELLO = 0CH;       (*  Low 16-bit section relative referemce (used for >32k TLS) *)
  IMAGE_REL_MIPS_SECRELHI = 0DH;       (*  High 16-bit section relative reference (used for >32k TLS) *)
  IMAGE_REL_MIPS_REFWORDNB = 22H;
  IMAGE_REL_MIPS_PAIR = 25H;

(*  *)
(*  Alpha Relocation types. *)
(*  *)
  IMAGE_REL_ALPHA_ABSOLUTE = 0H;
  IMAGE_REL_ALPHA_REFLONG = 1H;
  IMAGE_REL_ALPHA_REFQUAD = 2H;
  IMAGE_REL_ALPHA_GPREL32 = 3H;
  IMAGE_REL_ALPHA_LITERAL = 4H;
  IMAGE_REL_ALPHA_LITUSE = 5H;
  IMAGE_REL_ALPHA_GPDISP = 6H;
  IMAGE_REL_ALPHA_BRADDR = 7H;
  IMAGE_REL_ALPHA_HINT = 8H;
  IMAGE_REL_ALPHA_INLINE_REFLONG = 9H;
  IMAGE_REL_ALPHA_REFHI = 0AH;
  IMAGE_REL_ALPHA_REFLO = 0BH;
  IMAGE_REL_ALPHA_PAIR = 0CH;
  IMAGE_REL_ALPHA_MATCH = 0DH;
  IMAGE_REL_ALPHA_SECTION = 0EH;
  IMAGE_REL_ALPHA_SECREL = 0FH;
  IMAGE_REL_ALPHA_REFLONGNB = 10H;
  IMAGE_REL_ALPHA_SECRELLO = 11H;      (*  Low 16-bit section relative reference *)
  IMAGE_REL_ALPHA_SECRELHI = 12H;      (*  High 16-bit section relative reference *)

(*  *)
(*  IBM PowerPC relocation types. *)
(*  *)
  IMAGE_REL_PPC_ABSOLUTE = 0H;         (*  NOP *)
  IMAGE_REL_PPC_ADDR64 = 1H;           (*  64-bit address *)
  IMAGE_REL_PPC_ADDR32 = 2H;           (*  32-bit address *)
  IMAGE_REL_PPC_ADDR24 = 3H;           (*  26-bit address, shifted left 2 (branch absolute) *)
  IMAGE_REL_PPC_ADDR16 = 4H;           (*  16-bit address *)
  IMAGE_REL_PPC_ADDR14 = 5H;           (*  16-bit address, shifted left 2 (load doubleword) *)
  IMAGE_REL_PPC_REL24 = 6H;            (*  26-bit PC-relative offset, shifted left 2 (branch relative) *)
  IMAGE_REL_PPC_REL14 = 7H;            (*  16-bit PC-relative offset, shifted left 2 (br cond relative) *)
  IMAGE_REL_PPC_TOCREL16 = 8H;         (*  16-bit offset from TOC base *)
  IMAGE_REL_PPC_TOCREL14 = 9H;         (*  16-bit offset from TOC base, shifted left 2 (load doubleword) *)
  IMAGE_REL_PPC_ADDR32NB = 0AH;        (*  32-bit addr w/o image base *)
  IMAGE_REL_PPC_SECREL = 0BH;          (*  va of containing section (as in an image sectionhdr) *)
  IMAGE_REL_PPC_SECTION = 0CH;         (*  sectionheader number *)
  IMAGE_REL_PPC_IFGLUE = 0DH;          (*  substitute TOC restore instruction iff symbol is glue code *)
  IMAGE_REL_PPC_IMGLUE = 0EH;          (*  symbol is glue code; virtual address is TOC restore instruction *)
  IMAGE_REL_PPC_SECREL16 = 0FH;        (*  va of containing section (limited to 16 bits) *)
  IMAGE_REL_PPC_REFHI = 10H;
  IMAGE_REL_PPC_REFLO = 11H;
  IMAGE_REL_PPC_PAIR = 12H;
  IMAGE_REL_PPC_SECRELLO = 13H;        (*  Low 16-bit section relative reference (used for >32k TLS) *)
  IMAGE_REL_PPC_SECRELHI = 14H;        (*  High 16-bit section relative reference (used for >32k TLS) *)
  IMAGE_REL_PPC_TYPEMASK = 0FFH;       (*  mask to isolate above values in IMAGE_RELOCATION.Type *)

(*  Flag bits in IMAGE_RELOCATION.TYPE *)
  IMAGE_REL_PPC_NEG = 100H;            (*  subtract reloc value rather than adding it *)
  IMAGE_REL_PPC_BRTAKEN = 200H;        (*  fix branch prediction bit to predict branch taken *)
  IMAGE_REL_PPC_BRNTAKEN = 400H;       (*  fix branch prediction bit to predict branch not taken *)
  IMAGE_REL_PPC_TOCDEFN = 800H;        (*  toc slot defined in file (or, data in toc) *)

  IMAGE_SIZEOF_LINENUMBER = 6;

  IMAGE_SIZEOF_BASE_RELOCATION = 8;

(*  *)
(*  Based relocation types. *)
(*  *)
  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGH = 1;
  IMAGE_REL_BASED_LOW = 2;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_HIGHADJ = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;

(*  *)
(*  Archive format. *)
(*  *)
  IMAGE_ARCHIVE_START_SIZE = 8;
(*  IMAGE_ARCHIVE_START = '!<arch>' + 12C;
  IMAGE_ARCHIVE_END = '`' + 12C;
  IMAGE_ARCHIVE_PAD = 12C;
  IMAGE_ARCHIVE_LINKER_MEMBER = '/               ';
  IMAGE_ARCHIVE_LONGNAMES_MEMBER = '//              ';
 *)
  IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR = 60;

  IMAGE_ORDINAL_FLAG = MIN(LONGINT);
 
  IMAGE_RESOURCE_NAME_IS_STRING = MIN(LONGINT);
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = MIN(LONGINT);

  IMAGE_DEBUG_TYPE_UNKNOWN = 0;
  IMAGE_DEBUG_TYPE_COFF = 1;
  IMAGE_DEBUG_TYPE_CODEVIEW = 2;
  IMAGE_DEBUG_TYPE_FPO = 3;
  IMAGE_DEBUG_TYPE_MISC = 4;
  IMAGE_DEBUG_TYPE_EXCEPTION = 5;
  IMAGE_DEBUG_TYPE_FIXUP = 6;
  IMAGE_DEBUG_TYPE_OMAP_TO_SRC = 7;
  IMAGE_DEBUG_TYPE_OMAP_FROM_SRC = 8;

  FRAME_FPO = 0;
  FRAME_TRAP = 1;
  FRAME_TSS = 2;
  FRAME_NONFPO = 3;
 
  SIZEOF_RFPO_DATA = 16;
  IMAGE_DEBUG_MISC_EXENAME = 1;

  IMAGE_SEPARATE_DEBUG_SIGNATURE = 4944H;
  IMAGE_SEPARATE_DEBUG_FLAGS_MASK = 8000H;
  IMAGE_SEPARATE_DEBUG_MISMATCH = 8000H;   (*  when DBG was updated, the *)

(*  old checksum didn't match. *)
(* #include "poppack.h"                        // Return to the default *)
(*  *)
(*  End Image Format *)
(*  *)
(*  *)
(*  for move macros *)

  HEAP_NO_SERIALIZE = 1H;
  HEAP_GROWABLE = 2H;
  HEAP_GENERATE_EXCEPTIONS = 4H;
  HEAP_ZERO_MEMORY = 8H;
  HEAP_REALLOC_IN_PLACE_ONLY = 10H;
  HEAP_TAIL_CHECKING_ENABLED = 20H;
  HEAP_FREE_CHECKING_ENABLED = 40H;
  HEAP_DISABLE_COALESCE_ON_FREE = 80H;
  HEAP_CREATE_ALIGN_16 = 10000H;
  HEAP_CREATE_ENABLE_TRACING = 20000H;
  HEAP_MAXIMUM_TAG = 0FFFH;
  HEAP_PSEUDO_TAG_FLAG = 8000H;
  HEAP_TAG_SHIFT = 16;

  IS_TEXT_UNICODE_ASCII16 = 1H;
  IS_TEXT_UNICODE_REVERSE_ASCII16 = 10H;
  IS_TEXT_UNICODE_STATISTICS = 2H;
  IS_TEXT_UNICODE_REVERSE_STATISTICS = 20H;
  IS_TEXT_UNICODE_CONTROLS = 4H;
  IS_TEXT_UNICODE_REVERSE_CONTROLS = 40H;
  IS_TEXT_UNICODE_SIGNATURE = 8H;
  IS_TEXT_UNICODE_REVERSE_SIGNATURE = 80H;
  IS_TEXT_UNICODE_ILLEGAL_CHARS = 100H;
  IS_TEXT_UNICODE_ODD_LENGTH = 200H;
  IS_TEXT_UNICODE_DBCS_LEADBYTE = 400H;
  IS_TEXT_UNICODE_NULL_BYTES = 1000H;
  IS_TEXT_UNICODE_UNICODE_MASK = 0FH;
  IS_TEXT_UNICODE_REVERSE_MASK = 0F0H;
  IS_TEXT_UNICODE_NOT_UNICODE_MASK = 0F00H;
  IS_TEXT_UNICODE_NOT_ASCII_MASK = 0F000H;
  COMPRESSION_FORMAT_NONE = 0H;
  COMPRESSION_FORMAT_DEFAULT = 1H;
  COMPRESSION_FORMAT_LZNT1 = 2H;
  COMPRESSION_ENGINE_STANDARD = 0H;
  COMPRESSION_ENGINE_MAXIMUM = 100H;

  MESSAGE_RESOURCE_UNICODE = 1H;

  RTL_CRITSECT_TYPE = 0;
  RTL_RESOURCE_TYPE = 1;
 
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH = 2;
  DLL_THREAD_DETACH = 3;
  DLL_PROCESS_DETACH = 0;

(*  *)
(*  Defines for the READ flags for Eventlogging *)
(*  *)
  EVENTLOG_SEQUENTIAL_READ = 1H;
  EVENTLOG_SEEK_READ = 2H;
  EVENTLOG_FORWARDS_READ = 4H;
  EVENTLOG_BACKWARDS_READ = 8H;

(*  *)
(*  The types of events that can be logged. *)
(*  *)
  EVENTLOG_SUCCESS = 0H;
  EVENTLOG_ERROR_TYPE = 1H;
  EVENTLOG_WARNING_TYPE = 2H;
  EVENTLOG_INFORMATION_TYPE = 4H;
  EVENTLOG_AUDIT_SUCCESS = 8H;
  EVENTLOG_AUDIT_FAILURE = 10H;

(*  *)
(*  Defines for the WRITE flags used by Auditing for paired events *)
(*  These are not implemented in Product 1 *)
(*  *)
  EVENTLOG_START_PAIRED_EVENT = 1H;
  EVENTLOG_END_PAIRED_EVENT = 2H;
  EVENTLOG_END_ALL_PAIRED_EVENTS = 4H;
  EVENTLOG_PAIRED_EVENT_ACTIVE = 8H;
  EVENTLOG_PAIRED_EVENT_INACTIVE = 10H;
 
  DBG_CONTINUE = 65538;
  DBG_TERMINATE_THREAD = 1073807363;
  DBG_TERMINATE_PROCESS = 1073807364;
  DBG_CONTROL_C = 1073807365;
  DBG_CONTROL_BREAK = 1073807368;
  DBG_EXCEPTION_NOT_HANDLED = -2147418111;

(*  *)
(*  begin_ntddk begin_nthal *)
(*  *)
(*  Registry Specific Access Rights. *)
(*  *)
  KEY_QUERY_VALUE = 1H;
  KEY_SET_VALUE = 2H;
  KEY_CREATE_SUB_KEY = 4H;
  KEY_ENUMERATE_SUB_KEYS = 8H;
  KEY_NOTIFY = 10H;
  KEY_CREATE_LINK = 20H;
  KEY_READ = 131097;
  KEY_WRITE = 131078;
  KEY_EXECUTE = 131097;
  KEY_ALL_ACCESS = 983103;

(*  *)
(*  Open/Create Options *)
(*  *)
  REG_OPTION_RESERVED = 0H;            (*  Parameter is reserved *)
  REG_OPTION_NON_VOLATILE = 0H;        (*  Key is preserved *)

(*  when system is rebooted *)
  REG_OPTION_VOLATILE = 1H;            (*  Key is not preserved *)

(*  when system is rebooted *)
  REG_OPTION_CREATE_LINK = 2H;         (*  Created key is a *)

(*  symbolic link *)
  REG_OPTION_BACKUP_RESTORE = 4H;      (*  open for backup or restore *)

(*  special access rules *)
(*  privilege required *)
  REG_OPTION_OPEN_LINK = 8H;           (*  Open symbolic link *)
  REG_LEGAL_OPTION = 15;

(*  *)
(*  Key creation/open disposition *)
(*  *)
  REG_CREATED_NEW_KEY = 1H;            (*  New Registry Key created *)
  REG_OPENED_EXISTING_KEY = 2H;        (*  Existing Key opened *)

(*  *)
(*  Key restore flags *)
(*  *)
  REG_WHOLE_HIVE_VOLATILE = 1H;        (*  Restore whole hive volatile *)
  REG_REFRESH_HIVE = 2H;               (*  Unwind changes to last flush *)
  REG_NO_LAZY_FLUSH = 4H;              (*  Never lazy flush this hive *)

(*  end_ntddk end_nthal *)
(*  *)
(*  Notify filter values *)
(*  *)
  REG_NOTIFY_CHANGE_NAME = 1H;         (*  Create or delete (child) *)
  REG_NOTIFY_CHANGE_ATTRIBUTES = 2H;
  REG_NOTIFY_CHANGE_LAST_SET = 4H;     (*  time stamp *)
  REG_NOTIFY_CHANGE_SECURITY = 8H;
  REG_LEGAL_CHANGE_FILTER = 15;

(*  *)
(*  *)
(*  Predefined Value Types. *)
(*  *)
  REG_NONE = 0;                        (*  No value type *)
  REG_SZ = 1;                          (*  Unicode nul terminated string *)
  REG_EXPAND_SZ = 2;                   (*  Unicode nul terminated string *)

(*  (with environment variable references) *)
  REG_BINARY = 3;                      (*  Free form binary *)
  REG_DWORD = 4;                       (*  32-bit number *)
  REG_DWORD_LITTLE_ENDIAN = 4;         (*  32-bit number (same as REG_DWORD) *)
  REG_DWORD_BIG_ENDIAN = 5;            (*  32-bit number *)
  REG_LINK = 6;                        (*  Symbolic Link (unicode) *)
  REG_MULTI_SZ = 7;                    (*  Multiple Unicode strings *)
  REG_RESOURCE_LIST = 8;               (*  Resource list in the resource map *)
  REG_FULL_RESOURCE_DESCRIPTOR = 9;    (*  Resource list in the hardware description *)
  REG_RESOURCE_REQUIREMENTS_LIST = 10;

(*  end_ntddk end_nthal *)
(*  begin_ntddk begin_nthal *)
(*  *)
(*  Service Types (Bit Mask) *)
(*  *)
  SERVICE_KERNEL_DRIVER = 1H;
  SERVICE_FILE_SYSTEM_DRIVER = 2H;
  SERVICE_ADAPTER = 4H;
  SERVICE_RECOGNIZER_DRIVER = 8H;
  SERVICE_DRIVER = 11;
  SERVICE_WIN32_OWN_PROCESS = 10H;
  SERVICE_WIN32_SHARE_PROCESS = 20H;
  SERVICE_WIN32 = 48;
  SERVICE_INTERACTIVE_PROCESS = 100H;
  SERVICE_TYPE_ALL = 319;

(*  *)
(*  Start Type *)
(*  *)
  SERVICE_BOOT_START = 0H;
  SERVICE_SYSTEM_START = 1H;
  SERVICE_AUTO_START = 2H;
  SERVICE_DEMAND_START = 3H;
  SERVICE_DISABLED = 4H;

(*  *)
(*  Error control type *)
(*  *)
  SERVICE_ERROR_IGNORE = 0H;
  SERVICE_ERROR_NORMAL = 1H;
  SERVICE_ERROR_SEVERE = 2H;
  SERVICE_ERROR_CRITICAL = 3H;

(*  *)
(*  *)
(*  Define the registry driver node enumerations *)
(*  *)
(* H2D: Enumeration: _CM_SERVICE_NODE_TYPE *)
  DriverType = SERVICE_KERNEL_DRIVER;
  FileSystemType = SERVICE_FILE_SYSTEM_DRIVER;
  Win32ServiceOwnProcess = SERVICE_WIN32_OWN_PROCESS;
  Win32ServiceShareProcess = SERVICE_WIN32_SHARE_PROCESS;
  AdapterType = SERVICE_ADAPTER;
  RecognizerType = SERVICE_RECOGNIZER_DRIVER;

(* H2D: Enumeration: _CM_ERROR_CONTROL_TYPE *)
 
  BootLoad = SERVICE_BOOT_START;
  SystemLoad = SERVICE_SYSTEM_START;
  AutoLoad = SERVICE_AUTO_START;
  DemandLoad = SERVICE_DEMAND_START;
  DisableLoad = SERVICE_DISABLED;

  IgnoreError = SERVICE_ERROR_IGNORE;
  NormalError = SERVICE_ERROR_NORMAL;
  SevereError = SERVICE_ERROR_SEVERE;
  CriticalError = SERVICE_ERROR_CRITICAL;

(*  *)
(*  IOCTL_TAPE_ERASE definitions *)
(*  *)

  TAPE_ERASE_SHORT = 0;
  TAPE_ERASE_LONG = 1;

(*  *)
(*  IOCTL_TAPE_PREPARE definitions *)
(*  *)
 
  TAPE_LOAD = 0;
  TAPE_UNLOAD = 1;
  TAPE_TENSION = 2;
  TAPE_LOCK = 3;
  TAPE_UNLOCK = 4;
  TAPE_FORMAT = 5;
(*  *)
(*  IOCTL_TAPE_WRITE_MARKS definitions *)
(*  *)

  TAPE_SETMARKS = 0;
  TAPE_FILEMARKS = 1;
  TAPE_SHORT_FILEMARKS = 2;
  TAPE_LONG_FILEMARKS = 3;

(*  *)
(*  IOCTL_TAPE_GET_POSITION definitions *)
(*  *)

  TAPE_ABSOLUTE_POSITION = 0;
  TAPE_LOGICAL_POSITION = 1;
  TAPE_PSEUDO_LOGICAL_POSITION = 2;

(*  *)
(*  IOCTL_TAPE_SET_POSITION definitions *)
(*  *)

  TAPE_REWIND = 0;
  TAPE_ABSOLUTE_BLOCK = 1;
  TAPE_LOGICAL_BLOCK = 2;
  TAPE_PSEUDO_LOGICAL_BLOCK = 3;
  TAPE_SPACE_END_OF_DATA = 4;
  TAPE_SPACE_RELATIVE_BLOCKS = 5;
  TAPE_SPACE_FILEMARKS = 6;
  TAPE_SPACE_SEQUENTIAL_FMKS = 7;
  TAPE_SPACE_SETMARKS = 8;
  TAPE_SPACE_SEQUENTIAL_SMKS = 9;
(*  *)
(*  IOCTL_TAPE_GET_DRIVE_PARAMS definitions *)
(*  *)
(*  *)
(*  Definitions for FeaturesLow parameter *)
(*  *)

  TAPE_DRIVE_FIXED = 1H;
  TAPE_DRIVE_SELECT = 2H;
  TAPE_DRIVE_INITIATOR = 4H;
  TAPE_DRIVE_ERASE_SHORT = 10H;
  TAPE_DRIVE_ERASE_LONG = 20H;
  TAPE_DRIVE_ERASE_BOP_ONLY = 40H;
  TAPE_DRIVE_ERASE_IMMEDIATE = 80H;
  TAPE_DRIVE_TAPE_CAPACITY = 100H;
  TAPE_DRIVE_TAPE_REMAINING = 200H;
  TAPE_DRIVE_FIXED_BLOCK = 400H;
  TAPE_DRIVE_VARIABLE_BLOCK = 800H;
  TAPE_DRIVE_WRITE_PROTECT = 1000H;
  TAPE_DRIVE_EOT_WZ_SIZE = 2000H;
  TAPE_DRIVE_ECC = 10000H;
  TAPE_DRIVE_COMPRESSION = 20000H;
  TAPE_DRIVE_PADDING = 40000H;
  TAPE_DRIVE_REPORT_SMKS = 80000H;
  TAPE_DRIVE_GET_ABSOLUTE_BLK = 100000H;
  TAPE_DRIVE_GET_LOGICAL_BLK = 200000H;
  TAPE_DRIVE_SET_EOT_WZ_SIZE = 400000H;
  TAPE_DRIVE_RESERVED_BIT = MIN(LONGINT); (* don't use this bit! *)

(*                                               //can't be a low features bit! *)
(*                                               //reserved; high features only *)
(*  *)
(*  Definitions for FeaturesHigh parameter *)
(*  *)
  TAPE_DRIVE_LOAD_UNLOAD = -7FFFFFFFH;
  TAPE_DRIVE_TENSION = -7FFFFFFEH;
  TAPE_DRIVE_LOCK_UNLOCK = -7FFFFFFCH;
  TAPE_DRIVE_REWIND_IMMEDIATE = -7FFFFFF8H;
  TAPE_DRIVE_SET_BLOCK_SIZE = -7FFFFFF0H;
  TAPE_DRIVE_LOAD_UNLD_IMMED = -7FFFFFE0H;
  TAPE_DRIVE_TENSION_IMMED = -7FFFFFC0H;
  TAPE_DRIVE_LOCK_UNLK_IMMED = -7FFFFF80H;
  TAPE_DRIVE_SET_ECC = -7FFFFF00H;
  TAPE_DRIVE_SET_COMPRESSION = -7FFFFE00H;
  TAPE_DRIVE_SET_PADDING = -7FFFFC00H;
  TAPE_DRIVE_SET_REPORT_SMKS = -7FFFF800H;
  TAPE_DRIVE_ABSOLUTE_BLK = -7FFFF000H;
  TAPE_DRIVE_ABS_BLK_IMMED = -7FFFE000H;
  TAPE_DRIVE_LOGICAL_BLK = -7FFFC000H;
  TAPE_DRIVE_LOG_BLK_IMMED = -7FFF8000H;
  TAPE_DRIVE_END_OF_DATA = -7FFF0000H;
  TAPE_DRIVE_RELATIVE_BLKS = -7FFE0000H;
  TAPE_DRIVE_FILEMARKS = -7FFC0000H;
  TAPE_DRIVE_SEQUENTIAL_FMKS = -7FF80000H;
  TAPE_DRIVE_SETMARKS = -7FF00000H;
  TAPE_DRIVE_SEQUENTIAL_SMKS = -7FE00000H;
  TAPE_DRIVE_REVERSE_POSITION = -7FC00000H;
  TAPE_DRIVE_SPACE_IMMEDIATE = -7F800000H;
  TAPE_DRIVE_WRITE_SETMARKS = -7F000000H;
  TAPE_DRIVE_WRITE_FILEMARKS = -7E000000H;
  TAPE_DRIVE_WRITE_SHORT_FMKS = -7C000000H;
  TAPE_DRIVE_WRITE_LONG_FMKS = -78000000H;
  TAPE_DRIVE_WRITE_MARK_IMMED = -70000000H;
  TAPE_DRIVE_FORMAT = -60000000H;
  TAPE_DRIVE_FORMAT_IMMEDIATE = -40000000H;
  TAPE_DRIVE_HIGH_FEATURES = MIN(LONGINT);(* mask for high features flag *)

  TAPE_FIXED_PARTITIONS = 0;
  TAPE_SELECT_PARTITIONS = 1;
  TAPE_INITIATOR_PARTITIONS = 2;

TYPE 
(*  Define API decoration for direct importing system DLL references. *)
(*  Basics *)
(*  UNICODE (Wide Character) types *)
  PWCHAR = WD.PBOOL;
  LPWCH = PWCHAR;
  PWCH = PWCHAR;
  LPCWCH = PWCHAR;
  PCWCH = PWCHAR;
  NWPSTR = PWCHAR;
(* defined in WinBase *)
(* typedef WCHAR *LPWSTR, *PWSTR; *)
(* typedef CONST WCHAR *LPCWSTR, *PCWSTR; *)

(*  ANSI (Multi-byte Character) types *)
  PCHAR = WD.PSZ;
  LPCH = PCHAR;
  PCH = PCHAR;
  LPCCH = PCHAR;
  PCCH = PCHAR;
  NPSTR = PCHAR;
(* defined in WinBase *)
(* typedef CHAR *LPSTR, *PSTR; *)
(* typedef CONST CHAR *LPCSTR, *PCSTR; *)

(*  Neutral ANSI/UNICODE types and macros *)
  TCHAR = CHAR;
  PTCHAR = PCHAR;
  TBYTE = CHAR;
  PTBYTE = WD.PUCHAR;
  LPTCH = LONGINT;
  PTCH = LONGINT;
  PTSTR = LONGINT;
  LPTSTR = LONGINT;
  LPCTSTR = LONGINT;
  PSHORT = WD.LP;  (*POINTER TO INTEGER;*)
  PLONG = WD.LPLONG;

(* typedef WD.LPVOID HANDLE; *)
  PHANDLE = PLONG;

(*  Flag (bit) fields *)
  FCHAR = CHAR;
  FSHORT = INTEGER;
  FLONG = LONGINT;
  CCHAR = CHAR;
  LCID = LONGINT;
  PLCID = WD.PULONG;
  LANGID = INTEGER;

(* lint -e624  *)
(* lint +e624  *)


(*  *)
(*  __int64 is only supported by 2.0 and later midl. *)
(*  __midl is set by the 2.0 midl and not by 1.0 midl. *)
(*  *)
(* added for oberon *)
  __int64 = LONGINT;   (* 64 bit, how can we do this in OBERON ???? *)
  LONGLONG = LONGINT;
  DWORDLONG = LONGINT; (* unsigned *)
  PLONGLONG = PLONG;
  PDWORDLONG = PLONG;

(*  Update Sequence Number *)
  USN = LONGINT;
(* ???? UNIONS????  *)
  LUID = RECORD [_NOTALIGNED]
    LowPart : WD.DWORD;
    HighPart: LONGINT;
  END;
  LARGE_INTEGER = RECORD [_NOTALIGNED]
     u   : ARRAY 8 OF WD.BYTE;
  END;
  (*LARGE_INTEGER = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: d       : LUID;
      |1: u       : LUID;
      |2: QuadPart: LONGLONG;
    END;
  END; *)
  
  PLARGE_INTEGER = POINTER TO LARGE_INTEGER;
  winnt_Struct = RECORD [_NOTALIGNED]
    LowPart : WD.DWORD;
    HighPart: WD.DWORD;
  END;
  (*
  ULARGE_INTEGER = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: d       : winnt_Struct;
      |1: u       : winnt_Struct;
      |2: QuadPart: DWORDLONG;
    END;
  END;  *)
  ULARGE_INTEGER = RECORD [_NOTALIGNED]
    u : ARRAY 8 OF WD.BYTE;
  END;
  PULARGE_INTEGER = POINTER TO ULARGE_INTEGER;

(*  end_ntminiport end_ntndis end_ntminitape *)
(*  *)
(*  Locally Unique Identifier *)
(*  *)
(* Type '_LUID' was declared here in the source file *)

  PLUID = POINTER TO LUID;
(* end unions ???? *)

(*  *)
(*  Define operations to logically shift an int64 by 0..31 bits and to multiply *)
(*  32-bits by 32-bits to form a 64-bit product. *)
(*  *)

(*  *)
(*   Doubly linked list structure.  Can be used as either a list head, or *)
(*   as link words. *)
(*  *)

  PLIST_ENTRY = POINTER TO LIST_ENTRY;
  LIST_ENTRY = RECORD [_NOTALIGNED]
    Flink: PLIST_ENTRY;
    Blink: PLIST_ENTRY;
  END;
    (* Type 'PLIST_ENTRY' was declared here in the source file *)
  PRLIST_ENTRY = POINTER TO LIST_ENTRY;

(*  *)
(*   Singly linked list structure. Can be used as either a list head, or *)
(*   as link words. *)
(*  *)

  PSINGLE_LIST_ENTRY = POINTER TO SINGLE_LIST_ENTRY;

  SINGLE_LIST_ENTRY = RECORD [_NOTALIGNED]
    Next: PSINGLE_LIST_ENTRY;
  END;
  (* Type 'PSINGLE_LIST_ENTRY' was declared here in the source file *)

(*  *)
(*  Base data structures for OLE support *)
(*  *)

  GUID = RECORD [_NOTALIGNED]
    (*  size is 16 *)
    Data1: WD.DWORD;
    Data2: WD.WORD;
    Data3: WD.WORD;
    Data4: ARRAY 8 OF WD.BYTE;
  END;

  OBJECTID = RECORD [_NOTALIGNED]
    (*  size is 20 *)
    Lineage   : GUID;
    Uniquifier: WD.DWORD;
  END;

  KSPIN_LOCK = LONGINT;

  TEB = RECORD [_NOTALIGNED]
    (* Declaration without definition *)
  END;
  LPTEB = POINTER TO TEB;

(*  *)
(*  Define function to return the current Thread Environment Block *)
(*  *)

  winnt_Struct0 = RECORD [_NOTALIGNED]
    BaseMid: WD.BYTE;
    Flags1 : WD.BYTE;        (*  Declare as bytes to avoid alignment *)
    Flags2 : WD.BYTE;        (*  Problems. *)
    BaseHi : WD.BYTE;
  END;
  winnt_Struct1 = RECORD
  data: LONGINT;
  END;
(*   BIT FIELD
  winnt_Struct1 = RECORD [_NOTALIGNED]
<* IF __GEN_C__ THEN *>
    BaseMid    : WD.DWORD;       (* H2D: bit field. BaseMid:8 *)
    Type       : WD.DWORD;       (* H2D: bit field. Type:5 *)
    Dpl        : WD.DWORD;       (* H2D: bit field. Dpl:2 *)
    Pres       : WD.DWORD;       (* H2D: bit field. Pres:1 *)
    LimitHi    : WD.DWORD;       (* H2D: bit field. LimitHi:4 *)
    Sys        : WD.DWORD;       (* H2D: bit field. Sys:1 *)
    Reserved_0 : WD.DWORD;       (* H2D: bit field. Reserved_0:1 *)
    Default_Big: WD.DWORD;       (* H2D: bit field. Default_Big:1 *)
    Granularity: WD.DWORD;       (* H2D: bit field. Granularity:1 *)
    BaseHi     : WD.DWORD;       (* H2D: bit field. BaseHi:8 *)
<* ELSE *>
    BaseMid    : PACKEDSET OF [0..31];   (* H2D: bit field. BaseMid:8, Type:5, Dpl:2, Pres:1, LimitHi:4, Sys:1, Reserved_0:1, Default_Big:1, Granularity:1, BaseHi:8. *)
<* END *>
  END;
 *)
 (* UNION
 winnt_Union = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: Bytes: winnt_Struct0;
      |1: Bits : winnt_Struct1;
    END;
  END; *)
  winnt_Union = RECORD [_NOTALIGNED]
    Bytes: ARRAY 4 OF WD.BYTE;
  END;
  
  LDT_ENTRY = RECORD [_NOTALIGNED]
    LimitLow: WD.WORD;
    BaseLow : WD.WORD;
    HighWord: winnt_Union;
  END;
  PLDT_ENTRY = POINTER TO LDT_ENTRY;

(*  *)
(*  Exception record definition. *)
(*  *)
 
  PEXCEPTION_RECORD = POINTER TO EXCEPTION_RECORD;
  EXCEPTION_RECORD = RECORD [_NOTALIGNED]
(* lint -e18  *)
    (*  Don't complain about different definitions *)
    ExceptionCode       : WD.DWORD;
(* lint +e18  *)
    (*  Resume checking for different definitions *)
    ExceptionFlags      : WD.DWORD;
    ExceptionRecord     : PEXCEPTION_RECORD;
    ExceptionAddress    : WD.LPVOID;
    NumberParameters    : WD.DWORD;
    ExceptionInformation: LONGINT;  (*ARRAY [EXCEPTION_MAXIMUM_PARAMETERS] OF 
                          WD.DWORD;*)
  END;
(* Type 'PEXCEPTION_RECORD' was declared here in the source file *)
(*  *)
(*  Typedef for pointer returned by exception_info() *)
(*  *)

(* added for oberon *)
  PCONTEXT = LONGINT;
(* there are 3 DEFINITIONS  of CONTEXT for MIPS, ALPHA, POWER PC*)

  EXCEPTION_POINTERS = RECORD [_NOTALIGNED]
    ExceptionRecord: PEXCEPTION_RECORD;
    ContextRecord  : PCONTEXT;
  END;
  PEXCEPTION_POINTERS = POINTER TO EXCEPTION_POINTERS;

  EXCEPTION_REGISTRATION_RECORD = TEB;
(* UNION
  winnt_Union0 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: FiberData: WD.LPVOID;
      |1: Version  : WD.DWORD;
    END;
  END;
*)
  winnt_Union0 = RECORD [_NOTALIGNED]
    FiberData: ARRAY 4 OF WD.BYTE;
  END;

  PNT_TIB = POINTER TO NT_TIB;
  NT_TIB = RECORD [_NOTALIGNED]
    ExceptionList       : LPTEB;
    StackBase           : WD.LPVOID;
    StackLimit          : WD.LPVOID;
    SubSystemTib        : WD.LPVOID;
    u                   : winnt_Union0;
    ArbitraryUserPointer: WD.LPVOID;
    Self                : PNT_TIB;
  END;
(* Type 'PNT_TIB' was declared here in the source file *)
 
  QUOTA_LIMITS = RECORD [_NOTALIGNED]
    PagedPoolLimit       : WD.DWORD;
    NonPagedPoolLimit    : WD.DWORD;
    MinimumWorkingSetSize: WD.DWORD;
    MaximumWorkingSetSize: WD.DWORD;
    PagefileLimit        : WD.DWORD;
    TimeLimit            : LARGE_INTEGER;
  END;
  PQUOTA_LIMITS = POINTER TO QUOTA_LIMITS;

  MEMORY_BASIC_INFORMATION = RECORD [_NOTALIGNED]
    BaseAddress      : WD.LPVOID;
    AllocationBase   : WD.LPVOID;
    AllocationProtect: WD.DWORD;
    RegionSize       : WD.DWORD;
    State            : WD.DWORD;
    Protect          : WD.DWORD;
    Type             : WD.DWORD;
  END;
  PMEMORY_BASIC_INFORMATION = POINTER TO MEMORY_BASIC_INFORMATION;


(*  Define the file notification information structure *)

  FILE_NOTIFY_INFORMATION = RECORD [_NOTALIGNED]
    NextEntryOffset: WD.DWORD;
    Action         : WD.DWORD;
    FileNameLength : WD.DWORD;
    FileName       : LONGINT;  (*ARRAY [1] OF WD.WCHAR;*)
  END;
  PFILE_NOTIFY_INFORMATION = POINTER TO FILE_NOTIFY_INFORMATION;

  PACCESS_TOKEN = WD.LPVOID;
  PSECURITY_DESCRIPTOR = WD.LPVOID;
  PSID = WD.LPVOID;

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                              ACCESS MASK                            // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  *)
(*   Define the access mask as a longword sized structure divided up as *)
(*   follows: *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +---------------+---------------+-------------------------------+ *)
(*       |G|G|G|G|Res'd|A| StandardRights|         SpecificRights        | *)
(*       |R|W|E|A|     |S|               |                               | *)
(*       +-+-------------+---------------+-------------------------------+ *)
(*  *)
(*       typedef struct _ACCESS_MASK { *)
(*           WORD   SpecificRights; *)
(*           BYTE  StandardRights; *)
(*           BYTE  AccessSystemAcl : 1; *)
(*           BYTE  Reserved : 3; *)
(*           BYTE  GenericAll : 1; *)
(*           BYTE  GenericExecute : 1; *)
(*           BYTE  GenericWrite : 1; *)
(*           BYTE  GenericRead : 1; *)
(*       } ACCESS_MASK; *)
(*       typedef ACCESS_MASK *PACCESS_MASK; *)
(*  *)
(*   but to make life simple for programmer's we'll allow them to specify *)
(*   a desired access mask by simply OR'ing together mulitple single rights *)
(*   and treat an access mask as a DWORD.  For example *)
(*  *)
(*       DesiredAccess = DELETE | READ_CONTROL *)
(*  *)
(*   So we'll declare ACCESS_MASK as DWORD *)
(*  *)
(*  begin_ntddk begin_nthal begin_ntifs *)

  ACCESS_MASK = LONGINT;
  PACCESS_MASK = PLCID;

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                              ACCESS TYPES                           // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  begin_ntddk begin_nthal begin_ntifs *)
(*  *)
(*   The following are masks for the predefined standard access types *)
(*  *)


(*  *)
(*   Define the generic mapping array.  This is used to denote the *)
(*   mapping of each generic access right to a specific access mask. *)
(*  *)
 
  GENERIC_MAPPING = RECORD [_NOTALIGNED]
    GenericRead   : ACCESS_MASK;
    GenericWrite  : ACCESS_MASK;
    GenericExecute: ACCESS_MASK;
    GenericAll    : ACCESS_MASK;
  END;
  PGENERIC_MAPPING = POINTER TO GENERIC_MAPPING;

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                         LUID_AND_ATTRIBUTES                         // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)

  LUID_AND_ATTRIBUTES = RECORD [_NOTALIGNED]
    Luid      : LUID;
    Attributes: WD.DWORD;
  END;
  PLUID_AND_ATTRIBUTES = POINTER TO LUID_AND_ATTRIBUTES;
  LUID_AND_ATTRIBUTES_ARRAY = ARRAY ANYSIZE_ARRAY OF LUID_AND_ATTRIBUTES;
  PLUID_AND_ATTRIBUTES_ARRAY = WD.LP;(*POINTER TO LUID_AND_ATTRIBUTES_ARRAY;*)

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*               Security Id     (SID)                                 // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)

(*  Pictorially the structure of an SID is as follows: *)
(*  *)
(*          1   1   1   1   1   1 *)
(*          5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0 *)
(*       +---------------------------------------------------------------+ *)
(*       |      SubAuthorityCount        |Reserved1 (SBZ)|   Revision    | *)
(*       +---------------------------------------------------------------+ *)
(*       |                   IdentifierAuthority[0]                      | *)
(*       +---------------------------------------------------------------+ *)
(*       |                   IdentifierAuthority[1]                      | *)
(*       +---------------------------------------------------------------+ *)
(*       |                   IdentifierAuthority[2]                      | *)
(*       +---------------------------------------------------------------+ *)
(*       |                                                               | *)
(*       +- -  -  -  -  -  -  -  SubAuthority[]  -  -  -  -  -  -  -  - -+ *)
(*       |                                                               | *)
(*       +---------------------------------------------------------------+ *)

(*  begin_ntifs *)

  SID_IDENTIFIER_AUTHORITY = RECORD [_NOTALIGNED]
    Value: ARRAY 6 OF WD.BYTE;
  END;
  PSID_IDENTIFIER_AUTHORITY = POINTER TO SID_IDENTIFIER_AUTHORITY;

  SID = RECORD [_NOTALIGNED]
    Revision           : WD.BYTE;
    SubAuthorityCount  : WD.BYTE;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority       : ARRAY ANYSIZE_ARRAY OF WD.DWORD;
  END;

  PISID = POINTER TO SID;

  SID_NAME_USE = LONGINT;
  PSID_NAME_USE = WD.LP;  (*POINTER TO SID_NAME_USE;*)

  SID_AND_ATTRIBUTES = RECORD [_NOTALIGNED]
    Sid       : PSID;
    Attributes: WD.DWORD;
  END;
  PSID_AND_ATTRIBUTES = POINTER TO SID_AND_ATTRIBUTES;
  SID_AND_ATTRIBUTES_ARRAY = ARRAY ANYSIZE_ARRAY OF SID_AND_ATTRIBUTES;
  PSID_AND_ATTRIBUTES_ARRAY = WD.LP;  (*POINTER TO SID_AND_ATTRIBUTES_ARRAY;*)

(* /////////////////////////////////////////////////////////////////////////// *)
(*                                                                          // *)
(*  Universal well-known SIDs                                               // *)
(*                                                                          // *)
(*      Null SID                     S-1-0-0                                // *)
(*      World                        S-1-1-0                                // *)
(*      Local                        S-1-2-0                                // *)
(*      Creator Owner ID             S-1-3-0                                // *)
(*      Creator Group ID             S-1-3-1                                // *)
(*      Creator Owner Server ID      S-1-3-2                                // *)
(*      Creator Group Server ID      S-1-3-3                                // *)
(*                                                                          // *)
(*      (Non-unique IDs)             S-1-4                                  // *)
(*                                                                          // *)
(* /////////////////////////////////////////////////////////////////////////// *)
 
  ACL = RECORD [_NOTALIGNED]
    AclRevision: WD.BYTE;
    Sbz1       : WD.BYTE;
    AclSize    : WD.WORD;
    AceCount   : WD.WORD;
    Sbz2       : WD.WORD;
  END;
  PACL = POINTER TO ACL;

(*  end_ntddk *)
(*  *)
(*   The structure of an ACE is a common ace header followed by ace type *)
(*   specific data.  Pictorally the structure of the common ace header is *)
(*   as follows: *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*       |            AceSize            |    AceFlags   |     AceType   | *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*  *)
(*   AceType denotes the type of the ace, there are some predefined ace *)
(*   types *)
(*  *)
(*   AceSize is the size, in bytes, of ace. *)
(*  *)
(*   AceFlags are the Ace flags for audit and inheritance, defined shortly. *)

  ACE_HEADER = RECORD [_NOTALIGNED]
    AceType : WD.BYTE;
    AceFlags: WD.BYTE;
    AceSize : WD.WORD;
  END;
  PACE_HEADER = POINTER TO ACE_HEADER;


(*   The following are the four predefined ACE types. *)
(*   Examine the AceType field in the Header to determine *)
(*   which structure is appropriate to use for casting. *)

  ACCESS_ALLOWED_ACE = RECORD [_NOTALIGNED]
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: WD.DWORD;
  END;
  PACCESS_ALLOWED_ACE = POINTER TO ACCESS_ALLOWED_ACE;

  ACCESS_DENIED_ACE = RECORD [_NOTALIGNED]
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: WD.DWORD;
  END;
  PACCESS_DENIED_ACE = POINTER TO ACCESS_DENIED_ACE;

  SYSTEM_AUDIT_ACE = RECORD [_NOTALIGNED]
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: WD.DWORD;
  END;
  PSYSTEM_AUDIT_ACE = POINTER TO SYSTEM_AUDIT_ACE;

  SYSTEM_ALARM_ACE = RECORD [_NOTALIGNED]
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: WD.DWORD;
  END;
  PSYSTEM_ALARM_ACE = POINTER TO SYSTEM_ALARM_ACE;

(*  end_ntifs *)
(*  *)
(*                                   COMPOUND ACE *)
(*  *)
(*        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*       |    AceFlags   | Resd  |Inherit|    AceSize    |     AceType   | *)
(*       +---------------+-------+-------+---------------+---------------+ *)
(*       |                              Mask                             | *)
(*       +-------------------------------+-------------------------------+ *)
(*       |     Compound ACE Type         |        Reserved (SBZ)         | *)
(*       +-------------------------------+-------------------------------+ *)
(*       |                                                               | *)
(*       +                                                               + *)
(*       |                                                               | *)
(*       +                              Sid                              + *)
(*       |                                                               | *)
(*       +                                                               + *)
(*       |                                                               | *)
(*       +---------------------------------------------------------------+ *)
(*  *)

  COMPOUND_ACCESS_ALLOWED_ACE = RECORD [_NOTALIGNED]
    Header         : ACE_HEADER;
    Mask           : ACCESS_MASK;
    CompoundAceType: WD.WORD;
    Reserved       : WD.WORD;
    SidStart       : WD.DWORD;
  END;
  PCOMPOUND_ACCESS_ALLOWED_ACE = POINTER TO COMPOUND_ACCESS_ALLOWED_ACE;
 
  ACL_INFORMATION_CLASS = LONGINT;

(*  *)
(*   This record is returned/sent if the user is requesting/setting the *)
(*   AclRevisionInformation *)
(*  *)

  ACL_REVISION_INFORMATION = RECORD [_NOTALIGNED]
    AclRevision: WD.DWORD;
  END;
  PACL_REVISION_INFORMATION = POINTER TO ACL_REVISION_INFORMATION;

(*  *)
(*   This record is returned if the user is requesting AclSizeInformation *)
(*  *)

  ACL_SIZE_INFORMATION = RECORD [_NOTALIGNED]
    AceCount     : WD.DWORD;
    AclBytesInUse: WD.DWORD;
    AclBytesFree : WD.DWORD;
  END;
  PACL_SIZE_INFORMATION = POINTER TO ACL_SIZE_INFORMATION;

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                              SECURITY_DESCRIPTOR                    // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  *)
(*   Define the Security Descriptor and related data types. *)
(*   This is an opaque data structure. *)
(*  *)
(*  begin_ntddk begin_ntifs *)
 
  SECURITY_DESCRIPTOR_CONTROL = INTEGER;
  PSECURITY_DESCRIPTOR_CONTROL = WD.PUSHORT;

(*  In general, this data structure should be treated opaquely to ensure future *)
(*  compatibility. *)
(*  *)
(*  *)
 
  SECURITY_DESCRIPTOR = RECORD [_NOTALIGNED]
    Revision: WD.BYTE;
    Sbz1    : WD.BYTE;
    Control : SECURITY_DESCRIPTOR_CONTROL;
    Owner   : PSID;
    Group   : PSID;
    Sacl    : PACL;
    Dacl    : PACL;
  END;

  PISECURITY_DESCRIPTOR = POINTER TO SECURITY_DESCRIPTOR;

(*  end_ntifs *)
(*  Where: *)
(*  *)
(*      Revision - Contains the revision level of the security *)
(*          descriptor.  This allows this structure to be passed between *)
(*          systems or stored on disk even though it is expected to *)
(*          change in the future. *)
(*  *)
(*      Control - A set of flags which qualify the meaning of the *)
(*          security descriptor or individual fields of the security *)
(*          descriptor. *)
(*  *)
(*      Owner - is a pointer to an SID representing an object's owner. *)
(*          If this field is null, then no owner SID is present in the *)
(*          security descriptor.  If the security descriptor is in *)
(*          self-relative form, then this field contains an offset to *)
(*          the SID, rather than a pointer. *)
(*  *)
(*      Group - is a pointer to an SID representing an object's primary *)
(*          group.  If this field is null, then no primary group SID is *)
(*          present in the security descriptor.  If the security descriptor *)
(*          is in self-relative form, then this field contains an offset to *)
(*          the SID, rather than a pointer. *)
(*  *)
(*      Sacl - is a pointer to a system ACL.  This field value is only *)
(*          valid if the DaclPresent control flag is set.  If the *)
(*          SaclPresent flag is set and this field is null, then a null *)
(*          ACL  is specified.  If the security descriptor is in *)
(*          self-relative form, then this field contains an offset to *)
(*          the ACL, rather than a pointer. *)
(*  *)
(*      Dacl - is a pointer to a discretionary ACL.  This field value is *)
(*          only valid if the DaclPresent control flag is set.  If the *)
(*          DaclPresent flag is set and this field is null, then a null *)
(*          ACL (unconditionally granting access) is specified.  If the *)
(*          security descriptor is in self-relative form, then this field *)
(*          contains an offset to the ACL, rather than a pointer. *)
(*  *)
(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                Privilege Related Data Structures                    // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(*  begin_ntddk begin_nthal begin_ntifs *)

(*  *)
(*   Privilege Set - This is defined for a privilege set of one. *)
(*                   If more than one privilege is needed, then this structure *)
(*                   will need to be allocated with more space. *)
(*  *)
(*   Note: don't change this structure without fixing the INITIAL_PRIVILEGE_SET *)
(*   structure (defined in se.h) *)
(*  *)
 
  PRIVILEGE_SET = RECORD [_NOTALIGNED]
    PrivilegeCount: WD.DWORD;
    Control       : WD.DWORD;
    Privilege     : LUID_AND_ATTRIBUTES_ARRAY;
  END;
  PPRIVILEGE_SET = POINTER TO PRIVILEGE_SET;

(* ////////////////////////////////////////////////////////////////////// *)
(*                                                                     // *)
(*                NT Defined Privileges                                // *)
(*                                                                     // *)
(* ////////////////////////////////////////////////////////////////////// *)
(* ////////////////////////////////////////////////////////////////// *)
(*                                                                 // *)
(*            Security Quality Of Service                          // *)
(*                                                                 // *)
(*                                                                 // *)
(* ////////////////////////////////////////////////////////////////// *)
(*  begin_ntddk begin_nthal begin_ntifs *)
(*  *)
(*  Impersonation Level *)
(*  *)
(*  Impersonation level is represented by a pair of bits in Windows. *)
(*  If a new impersonation level is added or lowest value is changed from *)
(*  0 to something else, fix the Windows CreateFile call. *)
(*  *)

  SECURITY_IMPERSONATION_LEVEL = LONGINT;
  PSECURITY_IMPERSONATION_LEVEL = LONGINT; (*POINTER TO SECURITY_IMPERSONATION_LEVEL;*)
 
  TOKEN_TYPE = LONGINT;
  PTOKEN_TYPE = LONGINT;  (*POINTER TO TOKEN_TYPE;*)

  TOKEN_INFORMATION_CLASS = LONGINT;
  PTOKEN_INFORMATION_CLASS = LONGINT;  (*POINTER TO TOKEN_INFORMATION_CLASS;*)

(*  *)
(*  Token information class structures *)
(*  *)

  TOKEN_USER = RECORD [_NOTALIGNED]
    User: SID_AND_ATTRIBUTES;
  END;
  PTOKEN_USER = POINTER TO TOKEN_USER;

  TOKEN_GROUPS = RECORD [_NOTALIGNED]
    GroupCount: WD.DWORD;
    Groups    : SID_AND_ATTRIBUTES_ARRAY;
  END;
  PTOKEN_GROUPS = POINTER TO TOKEN_GROUPS;

  TOKEN_PRIVILEGES = RECORD [_NOTALIGNED]
    PrivilegeCount: WD.DWORD;
    Privileges    : LUID_AND_ATTRIBUTES_ARRAY;
  END;
  PTOKEN_PRIVILEGES = POINTER TO TOKEN_PRIVILEGES;

  TOKEN_OWNER = RECORD [_NOTALIGNED]
    Owner: PSID;
  END;
  PTOKEN_OWNER = POINTER TO TOKEN_OWNER;

  TOKEN_PRIMARY_GROUP = RECORD [_NOTALIGNED]
    PrimaryGroup: PSID;
  END;
  PTOKEN_PRIMARY_GROUP = POINTER TO TOKEN_PRIMARY_GROUP;

  TOKEN_DEFAULT_DACL = RECORD [_NOTALIGNED]
    DefaultDacl: PACL;
  END;
  PTOKEN_DEFAULT_DACL = POINTER TO TOKEN_DEFAULT_DACL;
 
  TOKEN_SOURCE = RECORD [_NOTALIGNED]
    SourceName      : ARRAY TOKEN_SOURCE_LENGTH OF CHAR;
    SourceIdentifier: LUID;
  END;
  PTOKEN_SOURCE = POINTER TO TOKEN_SOURCE;

  TOKEN_STATISTICS = RECORD [_NOTALIGNED]
    TokenId           : LUID;
    AuthenticationId  : LUID;
    ExpirationTime    : LARGE_INTEGER;
    TokenType         : TOKEN_TYPE;
    ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL;
    DynamicCharged    : WD.DWORD;
    DynamicAvailable  : WD.DWORD;
    GroupCount        : WD.DWORD;
    PrivilegeCount    : WD.DWORD;
    ModifiedId        : LUID;
  END;
  PTOKEN_STATISTICS = POINTER TO TOKEN_STATISTICS;

  TOKEN_CONTROL = RECORD [_NOTALIGNED]
    TokenId         : LUID;
    AuthenticationId: LUID;
    ModifiedId      : LUID;
    TokenSource     : TOKEN_SOURCE;
  END;
  PTOKEN_CONTROL = POINTER TO TOKEN_CONTROL;

(*  Security Tracking Mode *)
  SECURITY_CONTEXT_TRACKING_MODE = CHAR;
  PSECURITY_CONTEXT_TRACKING_MODE = PTBYTE;

(*  *)
(*  Quality Of Service *)
(*  *)

  SECURITY_QUALITY_OF_SERVICE = RECORD [_NOTALIGNED]
    Length             : WD.DWORD;
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
    ContextTrackingMode: SECURITY_CONTEXT_TRACKING_MODE;
    EffectiveOnly      : WD.BOOL;
  END;
  PSECURITY_QUALITY_OF_SERVICE = POINTER TO SECURITY_QUALITY_OF_SERVICE;

(*  *)
(*  Used to represent information related to a thread impersonation *)
(*  *)

  SE_IMPERSONATION_STATE = RECORD [_NOTALIGNED]
    Token        : PACCESS_TOKEN;
    CopyOnOpen   : WD.BOOL;
    EffectiveOnly: WD.BOOL;
    Level        : SECURITY_IMPERSONATION_LEVEL;
  END;
  PSE_IMPERSONATION_STATE = POINTER TO SE_IMPERSONATION_STATE;

  SECURITY_INFORMATION = LONGINT;
  PSECURITY_INFORMATION = PLCID;
 
  IMAGE_DOS_HEADER = RECORD [_NOTALIGNED]
    (*  DOS .EXE header *)
    e_magic   : WD.WORD;                      (*  Magic number *)
    e_cblp    : WD.WORD;                      (*  Bytes on last page of file *)
    e_cp      : WD.WORD;                      (*  Pages in file *)
    e_crlc    : WD.WORD;                      (*  Relocations *)
    e_cparhdr : WD.WORD;                      (*  Size of header in paragraphs *)
    e_minalloc: WD.WORD;                      (*  Minimum extra paragraphs needed *)
    e_maxalloc: WD.WORD;                      (*  Maximum extra paragraphs needed *)
    e_ss      : WD.WORD;                      (*  Initial (relative) SS value *)
    e_sp      : WD.WORD;                      (*  Initial SP value *)
    e_csum    : WD.WORD;                      (*  Checksum *)
    e_ip      : WD.WORD;                      (*  Initial IP value *)
    e_cs      : WD.WORD;                      (*  Initial (relative) CS value *)
    e_lfarlc  : WD.WORD;                      (*  File address of relocation table *)
    e_ovno    : WD.WORD;                      (*  Overlay number *)
    e_res     : ARRAY 4 OF WD.WORD;    (*  Reserved words *)
    e_oemid   : WD.WORD;                      (*  OEM identifier (for e_oeminfo) *)
    e_oeminfo : WD.WORD;                      (*  OEM information; e_oemid specific *)
    e_res2    : ARRAY 10 OF WD.WORD;   (*  Reserved words *)
    e_lfanew  : LONGINT;                        (*  File address of new exe header *)
  END;
  PIMAGE_DOS_HEADER = POINTER TO IMAGE_DOS_HEADER;

  IMAGE_OS2_HEADER = RECORD [_NOTALIGNED]
    (*  OS/2 .EXE header *)
    ne_magic       : WD.WORD;   (*  Magic number *)
    ne_ver         : CHAR;     (*  Version number *)
    ne_rev         : CHAR;     (*  Revision number *)
    ne_enttab      : WD.WORD;   (*  Offset of Entry Table *)
    ne_cbenttab    : WD.WORD;   (*  Number of bytes in Entry Table *)
    ne_crc         : LONGINT;     (*  Checksum of whole file *)
    ne_flags       : WD.WORD;   (*  Flag word *)
    ne_autodata    : WD.WORD;   (*  Automatic data segment number *)
    ne_heap        : WD.WORD;   (*  Initial heap allocation *)
    ne_stack       : WD.WORD;   (*  Initial stack allocation *)
    ne_csip        : LONGINT;     (*  Initial CS:IP setting *)
    ne_sssp        : LONGINT;     (*  Initial SS:SP setting *)
    ne_cseg        : WD.WORD;   (*  Count of file segments *)
    ne_cmod        : WD.WORD;   (*  Entries in Module Reference Table *)
    ne_cbnrestab   : WD.WORD;   (*  Size of non-resident name table *)
    ne_segtab      : WD.WORD;   (*  Offset of Segment Table *)
    ne_rsrctab     : WD.WORD;   (*  Offset of Resource Table *)
    ne_restab      : WD.WORD;   (*  Offset of resident name table *)
    ne_modtab      : WD.WORD;   (*  Offset of Module Reference Table *)
    ne_imptab      : WD.WORD;   (*  Offset of Imported Names Table *)
    ne_nrestab     : LONGINT;     (*  Offset of Non-resident Names Table *)
    ne_cmovent     : WD.WORD;   (*  Count of movable entries *)
    ne_align       : WD.WORD;   (*  Segment alignment shift count *)
    ne_cres        : WD.WORD;   (*  Count of resource segments *)
    ne_exetyp      : WD.BYTE;   (*  Target Operating system *)
    ne_flagsothers : WD.BYTE;   (*  Other .EXE flags *)
    ne_pretthunks  : WD.WORD;   (*  offset to return thunks *)
    ne_psegrefbytes: WD.WORD;   (*  offset to segment ref. bytes *)
    ne_swaparea    : WD.WORD;   (*  Minimum code swap area size *)
    ne_expver      : WD.WORD;   (*  Expected Windows version number *)
  END;
  PIMAGE_OS2_HEADER = POINTER TO IMAGE_OS2_HEADER;

  IMAGE_VXD_HEADER = RECORD [_NOTALIGNED]
    (*  Windows VXD header *)
    e32_magic       : WD.WORD;                      (*  Magic number *)
    e32_border      : WD.BYTE;                      (*  The byte ordering for the VXD *)
    e32_worder      : WD.BYTE;                      (*  The word ordering for the VXD *)
    e32_level       : WD.DWORD;                     (*  The EXE format level for now = 0 *)
    e32_cpu         : WD.WORD;                      (*  The CPU type *)
    e32_os          : WD.WORD;                      (*  The OS type *)
    e32_ver         : WD.DWORD;                     (*  Module version *)
    e32_mflags      : WD.DWORD;                     (*  Module flags *)
    e32_mpages      : WD.DWORD;                     (*  Module # pages *)
    e32_startobj    : WD.DWORD;                     (*  Object # for instruction pointer *)
    e32_eip         : WD.DWORD;                     (*  Extended instruction pointer *)
    e32_stackobj    : WD.DWORD;                     (*  Object # for stack pointer *)
    e32_esp         : WD.DWORD;                     (*  Extended stack pointer *)
    e32_pagesize    : WD.DWORD;                     (*  VXD page size *)
    e32_lastpagesize: WD.DWORD;                     (*  Last page size in VXD *)
    e32_fixupsize   : WD.DWORD;                     (*  Fixup section size *)
    e32_fixupsum    : WD.DWORD;                     (*  Fixup section checksum *)
    e32_ldrsize     : WD.DWORD;                     (*  Loader section size *)
    e32_ldrsum      : WD.DWORD;                     (*  Loader section checksum *)
    e32_objtab      : WD.DWORD;                     (*  Object table offset *)
    e32_objcnt      : WD.DWORD;                     (*  Number of objects in module *)
    e32_objmap      : WD.DWORD;                     (*  Object page map offset *)
    e32_itermap     : WD.DWORD;                     (*  Object iterated data map offset *)
    e32_rsrctab     : WD.DWORD;                     (*  Offset of Resource Table *)
    e32_rsrccnt     : WD.DWORD;                     (*  Number of resource entries *)
    e32_restab      : WD.DWORD;                     (*  Offset of resident name table *)
    e32_enttab      : WD.DWORD;                     (*  Offset of Entry Table *)
    e32_dirtab      : WD.DWORD;                     (*  Offset of Module Directive Table *)
    e32_dircnt      : WD.DWORD;                     (*  Number of module directives *)
    e32_fpagetab    : WD.DWORD;                     (*  Offset of Fixup Page Table *)
    e32_frectab     : WD.DWORD;                     (*  Offset of Fixup Record Table *)
    e32_impmod      : WD.DWORD;                     (*  Offset of Import Module Name Table *)
    e32_impmodcnt   : WD.DWORD;                     (*  Number of entries in Import Module Name Table *)
    e32_impproc     : WD.DWORD;                     (*  Offset of Import Procedure Name Table *)
    e32_pagesum     : WD.DWORD;                     (*  Offset of Per-Page Checksum Table *)
    e32_datapage    : WD.DWORD;                     (*  Offset of Enumerated Data Pages *)
    e32_preload     : WD.DWORD;                     (*  Number of preload pages *)
    e32_nrestab     : WD.DWORD;                     (*  Offset of Non-resident Names Table *)
    e32_cbnrestab   : WD.DWORD;                     (*  Size of Non-resident Name Table *)
    e32_nressum     : WD.DWORD;                     (*  Non-resident Name Table Checksum *)
    e32_autodata    : WD.DWORD;                     (*  Object # for automatic data object *)
    e32_debuginfo   : WD.DWORD;                     (*  Offset of the debugging information *)
    e32_debuglen    : WD.DWORD;                     (*  The length of the debugging info. in bytes *)
    e32_instpreload : WD.DWORD;                     (*  Number of instance pages in preload section of VXD file *)
    e32_instdemand  : WD.DWORD;                     (*  Number of instance pages in demand load section of VXD file *)
    e32_heapsize    : WD.DWORD;                     (*  Size of heap - for 16-bit apps *)
    e32_res3        : ARRAY 12 OF WD.BYTE;   (*  Reserved words *)
    e32_winresoff   : WD.DWORD;
    e32_winreslen   : WD.DWORD;
    e32_devid       : WD.WORD;                      (*  Device ID for VxD *)
    e32_ddkver      : WD.WORD;                      (*  DDK version for VxD *)
  END;
  PIMAGE_VXD_HEADER = POINTER TO IMAGE_VXD_HEADER;

(*  *)
(*  File header format. *)
(*  *)

  IMAGE_FILE_HEADER = RECORD [_NOTALIGNED]
    Machine             : WD.WORD;
    NumberOfSections    : WD.WORD;
    TimeDateStamp       : WD.DWORD;
    PointerToSymbolTable: WD.DWORD;
    NumberOfSymbols     : WD.DWORD;
    SizeOfOptionalHeader: WD.WORD;
    Characteristics     : WD.WORD;
  END;
  PIMAGE_FILE_HEADER = POINTER TO IMAGE_FILE_HEADER;

(*  *)
(*  Directory format. *)
(*  *)

  IMAGE_DATA_DIRECTORY = RECORD [_NOTALIGNED]
    VirtualAddress: WD.DWORD;
    Size          : WD.DWORD;
  END;
  PIMAGE_DATA_DIRECTORY = POINTER TO IMAGE_DATA_DIRECTORY;

(*  *)
(*  Optional header format. *)
(*  *)

  IMAGE_OPTIONAL_HEADER = RECORD [_NOTALIGNED]
(*  Standard fields. *)
    Magic                      : WD.WORD;
    MajorLinkerVersion         : WD.BYTE;
    MinorLinkerVersion         : WD.BYTE;
    SizeOfCode                 : WD.DWORD;
    SizeOfInitializedData      : WD.DWORD;
    SizeOfUninitializedData    : WD.DWORD;
    AddressOfEntryPoint        : WD.DWORD;
    BaseOfCode                 : WD.DWORD;
    BaseOfData                 : WD.DWORD;

(*  NT additional fields. *)
    ImageBase                  : WD.DWORD;
    SectionAlignment           : WD.DWORD;
    FileAlignment              : WD.DWORD;
    MajorOperatingSystemVersion: WD.WORD;
    MinorOperatingSystemVersion: WD.WORD;
    MajorImageVersion          : WD.WORD;
    MinorImageVersion          : WD.WORD;
    MajorSubsystemVersion      : WD.WORD;
    MinorSubsystemVersion      : WD.WORD;
    Win32VersionValue          : WD.DWORD;
    SizeOfImage                : WD.DWORD;
    SizeOfHeaders              : WD.DWORD;
    CheckSum                   : WD.DWORD;
    Subsystem                  : WD.WORD;
    DllCharacteristics         : WD.WORD;
    SizeOfStackReserve         : WD.DWORD;
    SizeOfStackCommit          : WD.DWORD;
    SizeOfHeapReserve          : WD.DWORD;
    SizeOfHeapCommit           : WD.DWORD;
    LoaderFlags                : WD.DWORD;
    NumberOfRvaAndSizes        : WD.DWORD;
    DataDirectory              : ARRAY IMAGE_NUMBEROF_DIRECTORY_ENTRIES OF IMAGE_DATA_DIRECTORY;
  END;
  PIMAGE_OPTIONAL_HEADER = POINTER TO IMAGE_OPTIONAL_HEADER;

  IMAGE_ROM_OPTIONAL_HEADER = RECORD [_NOTALIGNED]
    Magic                  : WD.WORD;
    MajorLinkerVersion     : WD.BYTE;
    MinorLinkerVersion     : WD.BYTE;
    SizeOfCode             : WD.DWORD;
    SizeOfInitializedData  : WD.DWORD;
    SizeOfUninitializedData: WD.DWORD;
    AddressOfEntryPoint    : WD.DWORD;
    BaseOfCode             : WD.DWORD;
    BaseOfData             : WD.DWORD;
    BaseOfBss              : WD.DWORD;
    GprMask                : WD.DWORD;
    CprMask                : ARRAY 4 OF WD.DWORD;
    GpValue                : WD.DWORD;
  END;
  PIMAGE_ROM_OPTIONAL_HEADER = POINTER TO IMAGE_ROM_OPTIONAL_HEADER;
 
  IMAGE_NT_HEADERS = RECORD [_NOTALIGNED]
    Signature     : WD.DWORD;
    FileHeader    : IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER;
  END;
  PIMAGE_NT_HEADERS = POINTER TO IMAGE_NT_HEADERS;

  IMAGE_ROM_HEADERS = RECORD [_NOTALIGNED]
    FileHeader    : IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_ROM_OPTIONAL_HEADER;
  END;
  PIMAGE_ROM_HEADERS = POINTER TO IMAGE_ROM_HEADERS;

(* UNION ???? 
  winnt_Union1 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: PhysicalAddress: WD.DWORD;
      |1: VirtualSize    : WD.DWORD;
    END;
  END;
*)
  winnt_Union1 = RECORD [_NOTALIGNED]
   data: ARRAY 4 OF WD.BYTE;
  END;

  IMAGE_SECTION_HEADER = RECORD [_NOTALIGNED]
    Name                : ARRAY IMAGE_SIZEOF_SHORT_NAME OF WD.BYTE;
    Misc                : winnt_Union1;
    VirtualAddress      : WD.DWORD;
    SizeOfRawData       : WD.DWORD;
    PointerToRawData    : WD.DWORD;
    PointerToRelocations: WD.DWORD;
    PointerToLinenumbers: WD.DWORD;
    NumberOfRelocations : WD.WORD;
    NumberOfLinenumbers : WD.WORD;
    Characteristics     : WD.DWORD;
  END;
  PIMAGE_SECTION_HEADER = POINTER TO IMAGE_SECTION_HEADER;

(*  *)
(*  Symbol format. *)
(*  *)
 
  winnt_Struct2 = RECORD [_NOTALIGNED]
    Short: WD.DWORD;         (*  if 0, use LongName *)
    Long : WD.DWORD;         (*  offset into string table *)
  END;

(* UNION ?????
  winnt_Union2 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: ShortName: ARRAY 8 OF WD.BYTE;
      |1: Name     : winnt_Struct2;
      |2: LongName : ARRAY 2 OF WD.PBYTE;
    END;
  END;
 *)
  winnt_Union2 = RECORD [_NOTALIGNED]
    Name     : ARRAY 8 OF WD.BYTE;
  END;

  IMAGE_SYMBOL = RECORD [_NOTALIGNED]
    N                 : winnt_Union2;
    Value             : WD.DWORD;
    SectionNumber     : INTEGER;
    Type              : WD.WORD;
    StorageClass      : WD.BYTE;
    NumberOfAuxSymbols: WD.BYTE;
  END;
  PIMAGE_SYMBOL = POINTER TO IMAGE_SYMBOL;


(*  *)
(*  Auxiliary entry format. *)
(*  *)
  winnt_Struct4 = RECORD [_NOTALIGNED]
    Linenumber: WD.WORD;     (*  declaration line number *)
    Size      : WD.WORD;     (*  size of struct, union, or enum *)
  END;
  (* UNION ?????
  winnt_Union3 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: LnSz     : winnt_Struct4;
      |1: TotalSize: WD.DWORD;
    END;
  END;
  *)
   winnt_Union3 = RECORD [_NOTALIGNED]
     data     : ARRAY 4 OF WD.BYTE;
   END;

  winnt_Struct5 = RECORD [_NOTALIGNED]
(*  if ISFCN, tag, or .bb *)
    PointerToLinenumber  : WD.DWORD;
    PointerToNextFunction: WD.DWORD;
  END;

  winnt_Struct6 = RECORD [_NOTALIGNED]
(*  if ISARY, up to 4 dimen. *)
    Dimension: ARRAY 4 OF WD.WORD;
  END;
 (* UNION ???
  winnt_Union4 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: Function: winnt_Struct5;
      |1: Array   : winnt_Struct6;
    END;
  END;
  *)
  winnt_Union4 = RECORD [_NOTALIGNED]   
    Array   : ARRAY 8 OF WD.BYTE;
  END;

  winnt_Struct3 = RECORD [_NOTALIGNED]
    TagIndex: WD.DWORD;       (*  struct, union, or enum tag index *)
    Misc    : winnt_Union3;
    FcnAry  : winnt_Union4;
    TvIndex : WD.WORD;        (*  tv index *)
  END;

  winnt_Struct7 = RECORD [_NOTALIGNED]
    Name: ARRAY IMAGE_SIZEOF_SYMBOL OF WD.BYTE;
  END;

  winnt_Struct8 = RECORD [_NOTALIGNED]
    Length             : WD.DWORD;   (*  section length *)
    NumberOfRelocations: WD.WORD;    (*  number of relocation entries *)
    NumberOfLinenumbers: WD.WORD;    (*  number of line numbers *)
    CheckSum           : WD.DWORD;   (*  checksum for communal *)
    Number             : INTEGER;     (*  section number to associate with *)
    Selection          : WD.BYTE;    (*  communal selection type *)
  END;
  (* UNION ????
  IMAGE_AUX_SYMBOL = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: Sym    : winnt_Struct3;
      |1: File   : winnt_Struct7;
      |2: Section: winnt_Struct8;
    END;
  END;
  *)
  IMAGE_AUX_SYMBOL = RECORD [_NOTALIGNED]
    Section: ARRAY 18 OF WD.BYTE;
  END;
  PIMAGE_AUX_SYMBOL = POINTER TO IMAGE_AUX_SYMBOL;

(*  *)
(*  Relocation format. *)
(*  *)
(* UNION ????? 
  winnt_Union5 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: VirtualAddress: WD.DWORD;
      |1: RelocCount    : WD.DWORD;   (*  Set to the real count when IMAGE_SCN_LNK_NRELOC_OVFL is set *)
    END;
  END;
*)
  winnt_Union5 = RECORD [_NOTALIGNED]
    VirtualAddress: ARRAY 4 OF WD.BYTE;
  END;

  IMAGE_RELOCATION = RECORD [_NOTALIGNED]
    u               : winnt_Union5;
    SymbolTableIndex: WD.DWORD;
    Type            : WD.WORD;
  END;
  PIMAGE_RELOCATION = POINTER TO IMAGE_RELOCATION;


(*  *)
(*  Line number format. *)
(*  *)
(* UNION ????
  winnt_Union6 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: SymbolTableIndex: WD.DWORD;   (*  Symbol table index of function name if Linenumber is 0. *)
      |1: VirtualAddress  : WD.DWORD;   (*  Virtual address of line number. *)
    END;
  END;
*)
  winnt_Union6 = RECORD [_NOTALIGNED]
    SymbolTableIndex: ARRAY 4 OF WD.BYTE;   (*  Symbol table index of function name if Linenumber is 0. *)
  END;

  IMAGE_LINENUMBER = RECORD [_NOTALIGNED]
    Type      : winnt_Union6;
    Linenumber: WD.WORD;        (*  Line number. *)
  END;
  PIMAGE_LINENUMBER = POINTER TO IMAGE_LINENUMBER;
(*  *)
(*  Based relocation format. *)
(*  *)

  IMAGE_BASE_RELOCATION = RECORD [_NOTALIGNED]
    VirtualAddress: WD.DWORD;
    SizeOfBlock   : WD.DWORD;
(*   WORD    TypeOffset[1]; *)
  END;
  PIMAGE_BASE_RELOCATION = POINTER TO IMAGE_BASE_RELOCATION;

  IMAGE_ARCHIVE_MEMBER_HEADER = RECORD [_NOTALIGNED]
    Name     : ARRAY 16 OF WD.BYTE;   (*  File member name - `/' terminated. *)
    Date     : ARRAY 12 OF WD.BYTE;   (*  File member date - decimal. *)
    UserID   : ARRAY 6 OF WD.BYTE;    (*  File member user id - decimal. *)
    GroupID  : ARRAY 6 OF WD.BYTE;    (*  File member group id - decimal. *)
    Mode     : ARRAY 8 OF WD.BYTE;    (*  File member mode - octal. *)
    Size     : ARRAY 10 OF WD.BYTE;   (*  File member size - decimal. *)
    EndHeader: ARRAY 2 OF WD.BYTE;    (*  String to end header. *)
  END;
  PIMAGE_ARCHIVE_MEMBER_HEADER = POINTER TO IMAGE_ARCHIVE_MEMBER_HEADER;

(*  DLL support. *)

(*  Export Format *)

  PtrPDWORD = WD.LP;  (*POINTER TO WD.PDWORD;*)
  PtrPWORD = WD.LP;  (*POINTER TO WD.PWORD;*)

  IMAGE_EXPORT_DIRECTORY = RECORD [_NOTALIGNED]
    Characteristics      : WD.DWORD;
    TimeDateStamp        : WD.DWORD;
    MajorVersion         : WD.WORD;
    MinorVersion         : WD.WORD;
    Name                 : WD.DWORD;
    Base                 : WD.DWORD;
    NumberOfFunctions    : WD.DWORD;
    NumberOfNames        : WD.DWORD;
    AddressOfFunctions   : PtrPDWORD;
    AddressOfNames       : PtrPDWORD;
    AddressOfNameOrdinals: PtrPWORD;
  END;
  PIMAGE_EXPORT_DIRECTORY = POINTER TO IMAGE_EXPORT_DIRECTORY;

(*  Import Format *)

  IMAGE_IMPORT_BY_NAME = RECORD [_NOTALIGNED]
    Hint: WD.WORD;
    Name: LONGINT;  (*ARRAY [1] OF WD.BYTE;*)
  END;
  PIMAGE_IMPORT_BY_NAME = POINTER TO IMAGE_IMPORT_BY_NAME;
(* UNION ????
  winnt_Union7 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: ForwarderString: WD.PBYTE;
      |1: Function       : WD.PDWORD;
      |2: Ordinal        : WD.DWORD;
      |3: AddressOfData  : PIMAGE_IMPORT_BY_NAME;
    END;
  END;
*)
  winnt_Union7 = RECORD [_NOTALIGNED]
    Ordinal        : WD.DWORD;
  END;

  IMAGE_THUNK_DATA = RECORD [_NOTALIGNED]
    u1: winnt_Union7;
  END;
  PIMAGE_THUNK_DATA = POINTER TO IMAGE_THUNK_DATA;
  (* UNION ????
  winnt_Union8 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: Characteristics   : WD.DWORD;    (*  0 for terminating null import descriptor *)
      |1: OriginalFirstThunk: PIMAGE_THUNK_DATA;   (*  RVA to original unbound IAT *)
    END;
  END;
  *)
  winnt_Union8 = RECORD [_NOTALIGNED]
     Characteristics   : WD.DWORD;    (*  0 for terminating null import descriptor *)
  END;

  IMAGE_IMPORT_DESCRIPTOR = RECORD [_NOTALIGNED]
    u             : winnt_Union8;
    TimeDateStamp : WD.DWORD;       (*  0 if not bound, *)
(*  -1 if bound, and real date\time stamp *)
(*      in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND) *)
(*  O.W. date/time stamp of DLL bound to (Old BIND) *)
    ForwarderChain: WD.DWORD;       (*  -1 if no forwarders *)
    Name          : WD.DWORD;
    FirstThunk    : PIMAGE_THUNK_DATA;      (*  RVA to IAT (if bound this IAT has actual addresses) *)
  END;
  PIMAGE_IMPORT_DESCRIPTOR = POINTER TO IMAGE_IMPORT_DESCRIPTOR;

(*  New format import descriptors pointed to by DataDirectory[ IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT ] *)

  IMAGE_BOUND_IMPORT_DESCRIPTOR = RECORD [_NOTALIGNED]
    TimeDateStamp              : WD.DWORD;
    OffsetModuleName           : WD.WORD;
    NumberOfModuleForwarderRefs: WD.WORD;
(*  Array of zero or more IMAGE_BOUND_FORWARDER_REF follows *)
  END;
  PIMAGE_BOUND_IMPORT_DESCRIPTOR = POINTER TO IMAGE_BOUND_IMPORT_DESCRIPTOR;

  IMAGE_BOUND_FORWARDER_REF = RECORD [_NOTALIGNED]
    TimeDateStamp   : WD.DWORD;
    OffsetModuleName: WD.WORD;
    Reserved        : WD.WORD;
  END;
  PIMAGE_BOUND_FORWARDER_REF = POINTER TO IMAGE_BOUND_FORWARDER_REF;

(*  Thread Local Storage *)

  PIMAGE_TLS_CALLBACK = PROCEDURE [_APICALL] ( DllHandle: WD.LPVOID;  
                                  Reason: WD.DWORD; Reserved: WD.LPVOID );

  PtrPIMAGE_TLS_CALLBACK = WD.LP;  (*POINTER TO PIMAGE_TLS_CALLBACK;*)

  IMAGE_TLS_DIRECTORY = RECORD [_NOTALIGNED]
    StartAddressOfRawData: WD.DWORD;
    EndAddressOfRawData  : WD.DWORD;
    AddressOfIndex       : WD.PDWORD;
    AddressOfCallBacks   : PtrPIMAGE_TLS_CALLBACK;
    SizeOfZeroFill       : WD.DWORD;
    Characteristics      : WD.DWORD;
  END;
  PIMAGE_TLS_DIRECTORY = POINTER TO IMAGE_TLS_DIRECTORY;

(*  Resource Format. *)

(*  Resource directory consists of two counts, following by a variable length *)
(*  array of directory entries.  The first count is the number of entries at *)
(*  beginning of the array that have actual names associated with each entry. *)
(*  The entries are in ascending order, case insensitive strings.  The second *)
(*  count is the number of entries that immediately follow the named entries. *)
(*  This second count identifies the number of entries that have 16-bit integer *)
(*  Ids as their name.  These entries are also sorted in ascending order. *)
(*  *)
(*  This structure allows fast lookup by either name or number, but for any *)
(*  given resource entry only one form of lookup is supported, not both. *)
(*  This is consistant with the syntax of the .RC file and the .RES file. *)
(*  *)

  IMAGE_RESOURCE_DIRECTORY = RECORD [_NOTALIGNED]
    Characteristics     : WD.DWORD;
    TimeDateStamp       : WD.DWORD;
    MajorVersion        : WD.WORD;
    MinorVersion        : WD.WORD;
    NumberOfNamedEntries: WD.WORD;
    NumberOfIdEntries   : WD.WORD;
(*   IMAGE_RESOURCE_DIRECTORY_ENTRY DirectoryEntries[]; *)
  END;
  PIMAGE_RESOURCE_DIRECTORY = POINTER TO IMAGE_RESOURCE_DIRECTORY;

(*  Each directory contains the 32-bit Name of the entry and an offset, *)
(*  relative to the beginning of the resource directory of the data associated *)
(*  with this directory entry.  If the name of the entry is an actual text *)
(*  string instead of an integer Id, then the high order bit of the name field *)
(*  is set to one and the low order 31-bits are an offset, relative to the *)
(*  beginning of the resource directory of the string, which is of type *)
(*  IMAGE_RESOURCE_DIRECTORY_STRING.  Otherwise the high bit is clear and the *)
(*  low-order 16-bits are the integer Id that identify this resource directory *)
(*  entry. If the directory entry is yet another resource directory (i.e. a *)
(*  subdirectory), then the high order bit of the offset field will be *)
(*  set to indicate this.  Otherwise the high bit is clear and the offset *)
(*  field points to a resource data entry. *)
CONST
  Strukt9_NameOffset = 7FFFFFFFH;
  Strukt9_NameIsString=MIN(LONGINT);
TYPE
(* BITFIELD*)
  winnt_Struct9 = RECORD
    Data: LONGINT;
  END;
(*  winnt_Struct9 = RECORD [_NOTALIGNED]
<* IF __GEN_C__ THEN *>
    NameOffset  : WD.DWORD;       (* H2D: bit field. NameOffset:31 *)
    NameIsString: WD.DWORD;       (* H2D: bit field. NameIsString:1 *)
<* ELSE *>
    NameOffset  : PACKEDSET OF [0..31];   (* H2D: bit field. NameOffset:31, NameIsString:1. *)
<* END *>
  END;
*)
(* UNION ???
  winnt_Union9 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: u   : winnt_Struct9;
      |1: Name: WD.DWORD;
      |2: Id  : WD.WORD;
    END;
  END;
 *)
  winnt_Union9 = RECORD [_NOTALIGNED]
    u   : ARRAY 4 OF WD.BYTE;
  END;
(* UNION ????
  winnt_Union10 = RECORD [_NOTALIGNED]
    CASE : INTEGER OF
       0: OffsetToData: WD.DWORD;
      |1: p           : winnt_Struct9;
    END;
  END;
*)
  winnt_Union10 = RECORD [_NOTALIGNED]
    p : ARRAY 4 OF WD.BYTE;
  END;

  IMAGE_RESOURCE_DIRECTORY_ENTRY = RECORD [_NOTALIGNED]
    d: winnt_Union9;
    q: winnt_Union10;
  END;
  PIMAGE_RESOURCE_DIRECTORY_ENTRY = POINTER TO IMAGE_RESOURCE_DIRECTORY_ENTRY;

(*  For resource directory entries that have actual string names, the Name *)
(*  field of the directory entry points to an object of the following type. *)
(*  All of these string objects are stored together after the last resource *)
(*  directory entry and before the first resource data object.  This minimizes *)
(*  the impact of these variable length objects on the alignment of the fixed *)
(*  size directory entry objects. *)

  IMAGE_RESOURCE_DIRECTORY_STRING = RECORD [_NOTALIGNED]
    Length    : WD.WORD;
    NameString: LONGINT; (*ARRAY [1] OF CHAR;*)
  END;
  PIMAGE_RESOURCE_DIRECTORY_STRING = POINTER TO IMAGE_RESOURCE_DIRECTORY_STRING;

  IMAGE_RESOURCE_DIR_STRING_U = RECORD [_NOTALIGNED]
    Length    : WD.WORD;
    NameString: LONGINT; (*ARRAY [1] OF WD.WCHAR;*)
  END;
  PIMAGE_RESOURCE_DIR_STRING_U = POINTER TO IMAGE_RESOURCE_DIR_STRING_U;

(*  Each resource data entry describes a leaf node in the resource directory *)
(*  tree.  It contains an offset, relative to the beginning of the resource *)
(*  directory of the data for the resource, a size field that gives the number *)
(*  of bytes of data at that offset, a CodePage that should be used when *)
(*  decoding code point values within the resource data.  Typically for new *)
(*  applications the code page would be the unicode code page. *)

  IMAGE_RESOURCE_DATA_ENTRY = RECORD [_NOTALIGNED]
    OffsetToData: WD.DWORD;
    Size        : WD.DWORD;
    CodePage    : WD.DWORD;
    Reserved    : WD.DWORD;
  END;
  PIMAGE_RESOURCE_DATA_ENTRY = POINTER TO IMAGE_RESOURCE_DATA_ENTRY;

(*  Load Configuration Directory Entry *)

  IMAGE_LOAD_CONFIG_DIRECTORY = RECORD [_NOTALIGNED]
    Characteristics              : WD.DWORD;
    TimeDateStamp                : WD.DWORD;
    MajorVersion                 : WD.WORD;
    MinorVersion                 : WD.WORD;
    GlobalFlagsClear             : WD.DWORD;
    GlobalFlagsSet               : WD.DWORD;
    CriticalSectionDefaultTimeout: WD.DWORD;
    DeCommitFreeBlockThreshold   : WD.DWORD;
    DeCommitTotalFreeThreshold   : WD.DWORD;
    LockPrefixTable              : WD.LPVOID;
    MaximumAllocationSize        : WD.DWORD;
    VirtualMemoryThreshold       : WD.DWORD;
    ProcessHeapFlags             : WD.DWORD;
    ProcessAffinityMask          : WD.DWORD;
    Reserved                     : ARRAY 3 OF WD.DWORD;
  END;
  PIMAGE_LOAD_CONFIG_DIRECTORY = POINTER TO IMAGE_LOAD_CONFIG_DIRECTORY;

(*  Function table entry format for MIPS/ALPHA images.  Function table is *)
(*  pointed to by the IMAGE_DIRECTORY_ENTRY_EXCEPTION directory entry. *)
(*  This definition duplicates ones in ntmips.h and ntalpha.h for use *)
(*  by portable image file mungers. *)

  IMAGE_RUNTIME_FUNCTION_ENTRY = RECORD [_NOTALIGNED]
    BeginAddress    : WD.DWORD;
    EndAddress      : WD.DWORD;
    ExceptionHandler: WD.LPVOID;
    HandlerData     : WD.LPVOID;
    PrologEndAddress: WD.DWORD;
  END;
  PIMAGE_RUNTIME_FUNCTION_ENTRY = POINTER TO IMAGE_RUNTIME_FUNCTION_ENTRY;

(*  Debug Format *)

  IMAGE_DEBUG_DIRECTORY = RECORD [_NOTALIGNED]
    Characteristics : WD.DWORD;
    TimeDateStamp   : WD.DWORD;
    MajorVersion    : WD.WORD;
    MinorVersion    : WD.WORD;
    Type            : WD.DWORD;
    SizeOfData      : WD.DWORD;
    AddressOfRawData: WD.DWORD;
    PointerToRawData: WD.DWORD;
  END;
  PIMAGE_DEBUG_DIRECTORY = POINTER TO IMAGE_DEBUG_DIRECTORY;

  IMAGE_COFF_SYMBOLS_HEADER = RECORD [_NOTALIGNED]
    NumberOfSymbols     : WD.DWORD;
    LvaToFirstSymbol    : WD.DWORD;
    NumberOfLinenumbers : WD.DWORD;
    LvaToFirstLinenumber: WD.DWORD;
    RvaToFirstByteOfCode: WD.DWORD;
    RvaToLastByteOfCode : WD.DWORD;
    RvaToFirstByteOfData: WD.DWORD;
    RvaToLastByteOfData : WD.DWORD;
  END;
  PIMAGE_COFF_SYMBOLS_HEADER = POINTER TO IMAGE_COFF_SYMBOLS_HEADER;
  (* BITFIELD*)
 FPO_DATA = RECORD [_NOTALIGNED]
    ulOffStart: WD.DWORD;       (*  offset 1st byte of function code *)
    cbProcSize: WD.DWORD;       (*  # bytes in function *)
    cdwLocals : WD.DWORD;       (*  # bytes in locals/4 *)
    cdwParams : WD.WORD;        (*  # bytes in params/4 *)
  data: INTEGER;
  END;
  (*
  FPO_DATA = RECORD [_NOTALIGNED]
    ulOffStart: WD.DWORD;       (*  offset 1st byte of function code *)
    cbProcSize: WD.DWORD;       (*  # bytes in function *)
    cdwLocals : WD.DWORD;       (*  # bytes in locals/4 *)
    cdwParams : WD.WORD;        (*  # bytes in params/4 *)
<* IF __GEN_C__ THEN *>
    cbProlog  : WD.WORD;        (* H2D: bit field. cbProlog:8 *)
                                        (*  # bytes in prolog *)
    cbRegs    : WD.WORD;        (* H2D: bit field. cbRegs:3 *)
                                        (*  # regs saved *)
    fHasSEH   : WD.WORD;        (* H2D: bit field. fHasSEH:1 *)
                                        (*  TRUE if SEH in func *)
    fUseBP    : WD.WORD;        (* H2D: bit field. fUseBP:1 *)
                                        (*  TRUE if EBP has been allocated *)
    reserved  : WD.WORD;        (* H2D: bit field. reserved:1 *)
                                        (*  reserved for future use *)
    cbFrame   : WD.WORD;        (* H2D: bit field. cbFrame:2 *)
                                        (*  frame type *)
<* ELSE *>
    cbProlog  : PACKEDSET OF [0..15];   (* H2D: bit field. cbProlog:8, cbRegs:3, fHasSEH:1, fUseBP:1, reserved:1, cbFrame:2. *)
                                        (*  # bytes in prolog *)
                                        (*  # regs saved *)
                                        (*  TRUE if SEH in func *)
                                        (*  TRUE if EBP has been allocated *)
                                        (*  reserved for future use *)
                                        (*  frame type *)
<* END *>
  END;
  *)
  PFPO_DATA = POINTER TO FPO_DATA;

  IMAGE_DEBUG_MISC = RECORD [_NOTALIGNED]
    DataType: WD.DWORD;                    (*  type of misc data, see defines *)
    Length  : WD.DWORD;                    (*  total length of record, rounded to four *)
(*  byte multiple. *)
    Unicode : WD.BOOL;                    (*  TRUE if data is unicode string *)
    Reserved: ARRAY 3 OF WD.BYTE;
    Data    : LONGINT;  (*ARRAY 1 OF WD.BYTE;     Actual data*)
  END;
  PIMAGE_DEBUG_MISC = POINTER TO IMAGE_DEBUG_MISC;

(*  Function table extracted from MIPS/ALPHA images.  Does not contain *)
(*  information needed only for runtime support.  Just those fields for *)
(*  each entry needed by a debugger. *)
(*  *)

  IMAGE_FUNCTION_ENTRY = RECORD [_NOTALIGNED]
    StartingAddress: WD.DWORD;
    EndingAddress  : WD.DWORD;
    EndOfPrologue  : WD.DWORD;
  END;
  PIMAGE_FUNCTION_ENTRY = POINTER TO IMAGE_FUNCTION_ENTRY;

(*  Debugging information can be stripped from an image file and placed *)
(*  in a separate .DBG file, whose file name part is the same as the *)
(*  image file name part (e.g. symbols for CMD.EXE could be stripped *)
(*  and placed in CMD.DBG).  This is indicated by the IMAGE_FILE_DEBUG_STRIPPED *)
(*  flag in the Characteristics field of the file header.  The beginning of *)
(*  the .DBG file contains the following structure which captures certain *)
(*  information from the image file.  This allows a debug to proceed even if *)
(*  the original image file is not accessable.  This header is followed by *)
(*  zero of more IMAGE_SECTION_HEADER structures, followed by zero or more *)
(*  IMAGE_DEBUG_DIRECTORY structures.  The latter structures and those in *)
(*  the image file contain file offsets relative to the beginning of the *)
(*  .DBG file. *)

(*  If symbols have been stripped from an image, the IMAGE_DEBUG_MISC structure *)
(*  is left in the image file, but not mapped.  This allows a debugger to *)
(*  compute the name of the .DBG file, from the name of the image in the *)
(*  IMAGE_DEBUG_MISC structure. *)

  IMAGE_SEPARATE_DEBUG_HEADER = RECORD [_NOTALIGNED]
    Signature         : WD.WORD;
    Flags             : WD.WORD;
    Machine           : WD.WORD;
    Characteristics   : WD.WORD;
    TimeDateStamp     : WD.DWORD;
    CheckSum          : WD.DWORD;
    ImageBase         : WD.DWORD;
    SizeOfImage       : WD.DWORD;
    NumberOfSections  : WD.DWORD;
    ExportedNamesSize : WD.DWORD;
    DebugDirectorySize: WD.DWORD;
    SectionAlignment  : WD.DWORD;
    Reserved          : ARRAY 2 OF WD.DWORD;
  END;
  PIMAGE_SEPARATE_DEBUG_HEADER = POINTER TO IMAGE_SEPARATE_DEBUG_HEADER;
 
  MESSAGE_RESOURCE_ENTRY = RECORD [_NOTALIGNED]
    Length: WD.WORD;
    Flags : WD.WORD;
    Text  : LONGINT;  (*ARRAY [1] OF WD.BYTE;*)
  END;
  PMESSAGE_RESOURCE_ENTRY = POINTER TO MESSAGE_RESOURCE_ENTRY;

  MESSAGE_RESOURCE_BLOCK = RECORD [_NOTALIGNED]
    LowId          : WD.DWORD;
    HighId         : WD.DWORD;
    OffsetToEntries: WD.DWORD;
  END;
  PMESSAGE_RESOURCE_BLOCK = POINTER TO MESSAGE_RESOURCE_BLOCK;

  MESSAGE_RESOURCE_DATA = RECORD [_NOTALIGNED]
    NumberOfBlocks: WD.DWORD;
    Blocks        : LONGINT;  (*ARRAY [1] OF MESSAGE_RESOURCE_BLOCK;*)
  END;
  PMESSAGE_RESOURCE_DATA = POINTER TO MESSAGE_RESOURCE_DATA;

  PRTL_CRITICAL_SECTION_DEBUG = POINTER TO RTL_CRITICAL_SECTION_DEBUG;
  RTL_CRITICAL_SECTION = RECORD [_NOTALIGNED]
    DebugInfo     : PRTL_CRITICAL_SECTION_DEBUG;
(*   The following three fields control entering and exiting the critical *)
(*   section for the resource *)
    LockCount     : LONGINT;
    RecursionCount: LONGINT;
    OwningThread  : WD.HANDLE;             (*  from the thread's ClientId->UniqueThread *)
    LockSemaphore : WD.HANDLE;
    Reserved      : WD.DWORD;
  END;
  PRTL_CRITICAL_SECTION = POINTER TO RTL_CRITICAL_SECTION;
  
  RTL_CRITICAL_SECTION_DEBUG = RECORD [_NOTALIGNED]
    Type                 : WD.WORD;
    CreatorBackTraceIndex: WD.WORD;
    CriticalSection      : PRTL_CRITICAL_SECTION;
    ProcessLocksList     : LIST_ENTRY;
    EntryCount           : WD.DWORD;
    ContentionCount      : WD.DWORD;
    Spare                : ARRAY 2 OF WD.DWORD;
  END;
  (* Type 'PRTL_CRITICAL_SECTION_DEBUG' was declared here in the source file *)
  (* Type 'PRTL_CRITICAL_SECTION' was declared here in the source file *)

(*  Structure that defines the header of the Eventlog record. This is the *)
(*  fixed-sized portion before all the variable-length strings, binary *)
(*  data and pad bytes. *)

(*  TimeGenerated is the time it was generated at the client. *)
(*  TimeWritten is the time it was put into the log at the server end. *)

  EVENTLOGRECORD = RECORD [_NOTALIGNED]
    Length             : WD.DWORD;   (*  Length of full record *)
    Reserved           : WD.DWORD;   (*  Used by the service *)
    RecordNumber       : WD.DWORD;   (*  Absolute record number *)
    TimeGenerated      : WD.DWORD;   (*  Seconds since 1-1-1970 *)
    TimeWritten        : WD.DWORD;   (*  Seconds since 1-1-1970 *)
    EventID            : WD.DWORD;
    EventType          : WD.WORD;
    NumStrings         : WD.WORD;
    EventCategory      : WD.WORD;
    ReservedFlags      : WD.WORD;    (*  For use with paired events (auditing) *)
    ClosingRecordNumber: WD.DWORD;   (*  For use with paired events (auditing) *)
    StringOffset       : WD.DWORD;   (*  Offset from beginning of record *)
    UserSidLength      : WD.DWORD;
    UserSidOffset      : WD.DWORD;
    DataLength         : WD.DWORD;
    DataOffset         : WD.DWORD;   (*  Offset from beginning of record *)
(*  Then follow: *)
(*  WCHAR SourceName[] *)
(*  WCHAR Computername[] *)
(*  SID   UserSid *)
(*  WCHAR Strings[] *)
(*  BYTE  Data[] *)
(*  CHAR  Pad[] *)
(*  DWORD Length; *)
  END;
  PEVENTLOGRECORD = POINTER TO EVENTLOGRECORD;

  CM_SERVICE_NODE_TYPE = LONGINT;
  CM_SERVICE_LOAD_TYPE = LONGINT;
  CM_ERROR_CONTROL_TYPE = LONGINT;

  TAPE_ERASE = RECORD [_NOTALIGNED]
    Type     : WD.DWORD;
    Immediate: WD.BOOL;
  END;
  PTAPE_ERASE = POINTER TO TAPE_ERASE;

  TAPE_PREPARE = RECORD [_NOTALIGNED]
    Operation: WD.DWORD;
    Immediate: WD.BOOL;
  END;
  PTAPE_PREPARE = POINTER TO TAPE_PREPARE;

  TAPE_WRITE_MARKS = RECORD [_NOTALIGNED]
    Type     : WD.DWORD;
    Count    : WD.DWORD;
    Immediate: WD.BOOL;
  END;
  PTAPE_WRITE_MARKS = POINTER TO TAPE_WRITE_MARKS;

  TAPE_GET_POSITION = RECORD [_NOTALIGNED]
    Type     : WD.DWORD;
    Partition: WD.DWORD;
    Offset   : LARGE_INTEGER;
  END;
  PTAPE_GET_POSITION = POINTER TO TAPE_GET_POSITION;

  TAPE_SET_POSITION = RECORD [_NOTALIGNED]
    Method   : WD.DWORD;
    Partition: WD.DWORD;
    Offset   : LARGE_INTEGER;
    Immediate: WD.BOOL;
  END;
  PTAPE_SET_POSITION = POINTER TO TAPE_SET_POSITION;
 
  TAPE_GET_DRIVE_PARAMETERS = RECORD [_NOTALIGNED]
    ECC                  : WD.BOOL;
    Compression          : WD.BOOL;
    DataPadding          : WD.BOOL;
    ReportSetmarks       : WD.BOOL;
    DefaultBlockSize     : WD.DWORD;
    MaximumBlockSize     : WD.DWORD;
    MinimumBlockSize     : WD.DWORD;
    MaximumPartitionCount: WD.DWORD;
    FeaturesLow          : WD.DWORD;
    FeaturesHigh         : WD.DWORD;
    EOTWarningZoneSize   : WD.DWORD;
  END;
  PTAPE_GET_DRIVE_PARAMETERS = POINTER TO TAPE_GET_DRIVE_PARAMETERS;

(*  IOCTL_TAPE_SET_DRIVE_PARAMETERS definitions *)

  TAPE_SET_DRIVE_PARAMETERS = RECORD [_NOTALIGNED]
    ECC               : WD.BOOL;
    Compression       : WD.BOOL;
    DataPadding       : WD.BOOL;
    ReportSetmarks    : WD.BOOL;
    EOTWarningZoneSize: WD.DWORD;
  END;
  PTAPE_SET_DRIVE_PARAMETERS = POINTER TO TAPE_SET_DRIVE_PARAMETERS;

(*  IOCTL_TAPE_GET_MEDIA_PARAMETERS definitions *)

  TAPE_GET_MEDIA_PARAMETERS = RECORD [_NOTALIGNED]
    Capacity      : LARGE_INTEGER;
    Remaining     : LARGE_INTEGER;
    BlockSize     : WD.DWORD;
    PartitionCount: WD.DWORD;
    WriteProtected: WD.BOOL;
  END;
  PTAPE_GET_MEDIA_PARAMETERS = POINTER TO TAPE_GET_MEDIA_PARAMETERS;

(*  IOCTL_TAPE_SET_MEDIA_PARAMETERS definitions *)

  TAPE_SET_MEDIA_PARAMETERS = RECORD [_NOTALIGNED]
    BlockSize: WD.DWORD;
  END;

  PTAPE_SET_MEDIA_PARAMETERS = POINTER TO TAPE_SET_MEDIA_PARAMETERS;

(*  IOCTL_TAPE_CREATE_PARTITION definitions *)

  TAPE_CREATE_PARTITION = RECORD [_NOTALIGNED]
    Method: WD.DWORD;
    Count : WD.DWORD;
    Size  : WD.DWORD;
  END;
  PTAPE_CREATE_PARTITION = POINTER TO TAPE_CREATE_PARTITION;

PROCEDURE [_APICALL] NtCurrentTeb (  ): LPTEB;

PROCEDURE [_APICALL] RtlEqualMemory ( Source1: WD.LPVOID; Source2: WD.LPVOID;
                           Length: WD.DWORD ): WD.DWORD;

PROCEDURE [_APICALL] RtlCopyMemory ( Destination: WD.LPVOID; Source: WD.LPVOID;
                          Length: WD.DWORD );

PROCEDURE [_APICALL] RtlCopyMemory32 ( Destination: WD.LPVOID; Source: WD.LPVOID;
                            Length: WD.DWORD );

PROCEDURE [_APICALL] RtlMoveMemory ( Destination: WD.LPVOID; Source: WD.LPVOID;
                          Length: WD.DWORD );

PROCEDURE [_APICALL] RtlFillMemory ( Destination: WD.LPVOID; Length: WD.DWORD;
                          Fill: WD.BYTE );

PROCEDURE [_APICALL] RtlZeroMemory ( Destination: WD.LPVOID; Length: WD.DWORD );

(*Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] __TEXT ( quote: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / __TEXT ( quote: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] TEXT ( quote: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / TEXT ( quote: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros*)
(*Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] DECLARE_HANDLE ( name: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / DECLARE_HANDLE ( name: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros*)
(*  *)
(*  Calculate the byte offset of a field in a structure of type type. *)
(*  *)
(* Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] FIELD_OFFSET ( type, field: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / FIELD_OFFSET ( type, field: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  *)
(*  Calculate the address of the base of the structure given its type, and an *)
(*  address of a field within the structure. *)
(*  *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] CONTAINING_RECORD ( address, type, field: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / CONTAINING_RECORD ( address, type, field: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros *)
(*  *)
(*  *)
(*   Language ID creation/extraction macros: *)
(*  *)
(*     MAKELANGID    - construct language id from a primary language id and *)
(*                     a sublanguage id. *)
(*     PRIMARYLANGID - extract primary language id from a language id. *)
(*     SUBLANGID     - extract sublanguage id from a language id. *)
(*  *)
(*Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] MAKELANGID ( p, s: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / MAKELANGID ( p, s: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] PRIMARYLANGID ( lgid: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / PRIMARYLANGID ( lgid: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] SUBLANGID ( lgid: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / SUBLANGID ( lgid: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  *)
(*   A locale ID is a 32 bit value which is the combination of a *)
(*   language ID, a sort ID, and a reserved area.  The bits are *)
(*   allocated as follows: *)
(*  *)
(*        +-------------+---------+-------------------------+ *)
(*        |   Reserved  | Sort ID |      Language ID        | *)
(*        +-------------+---------+-------------------------+ *)
(*         31         20 19     16 15                      0   bit *)
(*  *)
(*  *)
(*   Locale ID creation/extraction macros: *)
(*  *)
(*     MAKELCID       - construct locale id from a language id and a sort id. *)
(*     LANGIDFROMLCID - extract language id from a locale id. *)
(*     SORTIDFROMLCID - extract sort id from a locale id. *)
(*  *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] MAKELCID ( lgid, srtid: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / MAKELCID ( lgid, srtid: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] LANGIDFROMLCID ( lcid: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / LANGIDFROMLCID ( lcid: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] SORTIDFROMLCID ( lcid: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / SORTIDFROMLCID ( lcid: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  *)
(*   Default System and User IDs for language and locale. *)
(*  *)
(*  begin_ntminiport begin_ntndis begin_ntminitape *)
(*  *)
(*  Macros used to eliminate compiler warning generated when formal *)
(*  parameters or local variables are not declared. *)
(*  *)
(*  Use DBG_UNREFERENCED_PARAMETER() when a parameter is not yet *)
(*  referenced but will be once the module is completely developed. *)
(*  *)
(*  Use DBG_UNREFERENCED_LOCAL_VARIABLE() when a local variable is not yet *)
(*  referenced but will be once the module is completely developed. *)
(*  *)
(*  Use UNREFERENCED_PARAMETER() if a parameter will never be referenced. *)
(*  *)
(*  DBG_UNREFERENCED_PARAMETER and DBG_UNREFERENCED_LOCAL_VARIABLE will *)
(*  eventually be made into a null macro to help determine whether there *)
(*  is unfinished work. *)
(*  *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] UNREFERENCED_PARAMETER ( P: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / UNREFERENCED_PARAMETER ( P: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] DBG_UNREFERENCED_PARAMETER ( P: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / DBG_UNREFERENCED_PARAMETER ( P: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] DBG_UNREFERENCED_LOCAL_VARIABLE ( V: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / DBG_UNREFERENCED_LOCAL_VARIABLE ( V: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros *)
(*  MACROS 
(*  Basic Type of  x *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] BTYPE ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / BTYPE ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  Is x a pointer? *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] ISPTR ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / ISPTR ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  Is x a function? *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] ISFCN ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / ISFCN ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  Is x an array? *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] ISARY ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / ISARY ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
(*  Is x a structure, union, or enumeration TAG? *)
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] ISTAG ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / ISTAG ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] INCREF ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / INCREF ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] DECREF ( x: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / DECREF ( x: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros *)
(* Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] IMAGE_SNAP_BY_ORDINAL ( Ordinal: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / IMAGE_SNAP_BY_ORDINAL ( Ordinal: ARRAY OF SYSTEM.BYTE );
<* END *>
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] IMAGE_ORDINAL ( Ordinal: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / IMAGE_ORDINAL ( Ordinal: ARRAY OF SYSTEM.BYTE );
<*- GENTYPEDEF *>
<* END *>
end Macros*)
(* Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] HEAP_MAKE_TAG_FLAGS ( b, o: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / HEAP_MAKE_TAG_FLAGS ( b, o: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros*)
(* Macros
<* IF __GEN_C__ THEN *>
(* H2D: this procedure corresponds to a macro. *)
PROCEDURE [_APICALL] IMAGE_FIRST_SECTION ( ntheader: ARRAY OF SYSTEM.BYTE );
<* ELSE *>
PROCEDURE [_APICALL]  / IMAGE_FIRST_SECTION ( ntheader: ARRAY OF SYSTEM.BYTE );
<* END *>
end Macros*)
END WinNT.
