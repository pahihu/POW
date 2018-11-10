(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Oberon-2 Debugger                                             *)
(*                                                                           *)
(* MODULE:     Debug                                       V 1.42.02         *)
(*                                                         2002MAR28         *)
(*  PURPOSE:   Processing Debug Infos                                        *)
(*                                                                           *)
(*  FUNCTIONS:                                                               *)
(*   CreateDebugEvents                                                       *)
(*   DebugTheProcess                                                         *)
(*             this is the debug thread's procedure                          *)
(*   Start     starts debugger                                               *)
(*   Suspend   suspends debugger                                             *)
(*   Resume    resumes debugger                                              *)
(*   Stop      closes debugger                                               *)
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

MODULE Debug;


IMPORT
  WinBase, WinDef, WinGDI, WinNT, WinUser,
  Strings, SYSTEM,
  Dump_DD_Debug, Global, StatusLine, View;


CONST
  Version*             =              "V 1.42.02";
  Module*              =              "Debug";
  Indentation          =              32;
  
  CLOSEDEBUGGER        =               0;
  SUSPENDDEBUGGER      =               1;
  RESUMEDEBUGGER       =               2;
  DEBUGACTIVE          =               3;
  nDEBUGEVENTS         =               4;
  
  STATUS_WAIT_0                    =   0000H;              (* WINDOWs Error Codes (low word) *)
  STATUS_ABANDONED_WAIT_0          =   0080H;    
  STATUS_USER_APC                  =   00C0H;    
  STATUS_TIMEOUT                   =   0102H;    
  STATUS_PENDING                   =   0103H;    
  STATUS_SEGMENT_NOTIFICATION      =   0005H;    
  STATUS_GUARD_PAGE_VIOLATION      =   0001H;    
  STATUS_DATATYPE_MISALIGNMENT     =   0002H;    
  STATUS_BREAKPOINT                =   0003H;    
  STATUS_SINGLE_STEP               =   0004H;    
  STATUS_ACCESS_VIOLATION          =   0005H;    
  STATUS_IN_PAGE_ERROR             =   0006H;    
  STATUS_INVALID_HANDLE            =   0008H;    
  STATUS_NO_MEMORY                 =   0017H;    
  STATUS_ILLEGAL_INSTRUCTION       =   001DH;    
  STATUS_NONCONTINUABLE_EXCEPTION  =   0025H;    
  STATUS_INVALID_DISPOSITION       =   0026H;    
  STATUS_ARRAY_BOUNDS_EXCEEDED     =   008CH;    
  STATUS_FLOAT_DENORMAL_OPERAND    =   008DH;    
  STATUS_FLOAT_DIVIDE_BY_ZERO      =   008EH;    
  STATUS_FLOAT_INEXACT_RESULT      =   008FH;    
  STATUS_FLOAT_INVALID_OPERATION   =   0090H;    
  STATUS_FLOAT_OVERFLOW            =   0091H;    
  STATUS_FLOAT_STACK_CHECK         =   0092H;    
  STATUS_FLOAT_UNDERFLOW           =   0093H;    
  STATUS_INTEGER_DIVIDE_BY_ZERO    =   0094H;    
  STATUS_INTEGER_OVERFLOW          =   0095H;    
  STATUS_PRIVILEGED_INSTRUCTION    =   0096H;    
  STATUS_STACK_OVERFLOW            =   00FDH;    
  STATUS_CONTROL_C_EXIT            =   013AH;    
  STATUS_FLOAT_MULTIPLE_FAULTS     =   02B4H;    
  STATUS_FLOAT_MULTIPLE_TRAPS      =   02B5H;    
  STATUS_ILLEGAL_VLM_REFERENCE     =   02C0H;     
  
  IDS_DBGEVNTCLOSEACK  =            1014;
  IDS_DBGEVNTACTIVE    =            1015;
  IDS_DBGEVNTCLOSE     =            1016;
  IDS_DBGEVNTSUSPEND   =            1017;
  IDS_DBGEVNTRESUME    =            1018;
  IDS_DBGEVNTSETTHREAD =            1019;
  IDS_DBGEVNTINITACK   =            1020;
  IDS_DBGEVNTSTOP      =            1021;
  IDS_DBGEVNTSTART     =            1022;
  IDS_DBGEVNTSETPROCESS =           1023;
  
  LineEmpty            =              "   ";
  Line001              =              "Debug started.";
  
  
TYPE
  LineOfTextT          =               ARRAY 256 OF CHAR;
  LineOfTextP          =               POINTER TO LineOfTextT;
  

VAR
  DebugEvents:                         ARRAY nDEBUGEVENTS OF WinDef.HANDLE;
  nIndex:                              LONGINT;
  Result:                              WinDef.LRESULT;
  ReturnCode:                          LONGINT;
  ResultBool:                          WinDef.BOOL;
  LineOfText:                          ARRAY 256 OF CHAR;
  szEvent:                             ARRAY 128 OF CHAR;
  SecurityAttributes:                  WinBase.SECURITY_ATTRIBUTES;


(*****************************************************************************)
(*                                                                           *)
(* CreateDebugEvents                                                         *)
(* local function creates debug event objects for thread synchronization     *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE [_APICALL] CreateDebugEvents*
                                      (VAR DebugEvents:      ARRAY OF WinDef.HANDLE)
                                      :BOOLEAN;

VAR
  szEvent:                             ARRAY Global.Max_Path_Length OF CHAR;
  
BEGIN

  (* Create DEBUG EVENT ACTIVE event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL), 
                                 IDS_DBGEVNTACTIVE, 
                                 SYSTEM.ADR(szEvent), 
                                 255);
  DebugEvents[DEBUGACTIVE]     := WinBase.CreateEventA (SecurityAttributes, 
                                                        WinDef.True, 
                                                        WinDef.True, 
                                                        SYSTEM.ADR(szEvent));
  IF DebugEvents[DEBUGACTIVE]=WinDef.False THEN
    RETURN FALSE;
  END;

  (* Create DEBUG EVENT CLOSE event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL), 
                                 IDS_DBGEVNTCLOSE, 
                                 SYSTEM.ADR(szEvent), 
                                 255);
  DebugEvents[CLOSEDEBUGGER]   := WinBase.CreateEventA (SecurityAttributes, 
                                                        WinDef.True, 
                                                        WinDef.False, 
                                                        SYSTEM.ADR(szEvent));
  IF DebugEvents[CLOSEDEBUGGER]=WinDef.False THEN
    Result := WinBase.CloseHandle (DebugEvents[DEBUGACTIVE]);
    RETURN FALSE;
  END;

  (* Create DEBUG EVENT STOP event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL), 
                                 IDS_DBGEVNTSTOP, 
                                 SYSTEM.ADR(szEvent), 
                                 255);
  DebugEvents[SUSPENDDEBUGGER] := WinBase.CreateEventA (SecurityAttributes, 
                                                        WinDef.True, 
                                                        WinDef.False, 
                                                        SYSTEM.ADR(szEvent));
  IF DebugEvents[SUSPENDDEBUGGER]=WinDef.False THEN
    Result := WinBase.CloseHandle (DebugEvents[DEBUGACTIVE]);
    Result := WinBase.CloseHandle (DebugEvents[CLOSEDEBUGGER]);
    RETURN FALSE;
  END;

  (* Create DEBUG EVENT START event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL), 
                                 IDS_DBGEVNTSTART, 
                                 SYSTEM.ADR(szEvent), 
                                 255);
  DebugEvents[RESUMEDEBUGGER]  := WinBase.CreateEventA (SecurityAttributes, 
                                                        WinDef.True, 
                                                        WinDef.False, 
                                                        SYSTEM.ADR(szEvent));
  IF DebugEvents[RESUMEDEBUGGER]=WinDef.False THEN
    Result := WinBase.CloseHandle (DebugEvents[DEBUGACTIVE]);
    Result := WinBase.CloseHandle (DebugEvents[CLOSEDEBUGGER]);
    Result := WinBase.CloseHandle (DebugEvents[SUSPENDDEBUGGER]);
    RETURN FALSE;
  END;

  RETURN TRUE;
END CreateDebugEvents;


(*****************************************************************************)
(*                                                                           *)
(* DebugTheProcess                                                           *)
(* main thread that is the debugger residing over a debuggee                 *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE [_APICALL] DebugTheProcess* (ADRDebugProcess:      LONGINT)
                                      :                      WinDef.DWORD;

VAR
  MyDebugThread,
  ActDebugThread:                      Global.DebugThreadP;
                                       (* describes the event causing the debugger to interrupt *)
  DebugEvent:                          WinBase.DEBUG_EVENT;
                                       (* DebugEvents are used to control the debugger from the GUI *)
  lpDebugEvents:                       LONGINT;
  DebugEvents:                         ARRAY nDEBUGEVENTS OF WinDef.HANDLE;
                                       (* local pointer to address the process information *)
  DebugProcessP:                       Global.DebugProcessP;
  hEvent:                              WinDef.HANDLE;
  szEvent:                             ARRAY Global.Max_Path_Length OF CHAR;
  SecurityAttributes:                  WinBase.SECURITY_ATTRIBUTES;
  StartupInfo:                         WinBase.STARTUPINFOA;
  ProcessInformation:                  WinBase.PROCESS_INFORMATION;
  ProcessAttributes,
  ThreadAttributes:                    WinBase.SECURITY_ATTRIBUTES;
  
  Create_Process_Debug_InfoP:          WinBase.LPCREATE_PROCESS_DEBUG_INFO;
  Create_Thread_Debug_InfoP:           WinBase.LPCREATE_THREAD_DEBUG_INFO;
  Exit_Thread_Debug_InfoP:             WinBase.LPEXIT_THREAD_DEBUG_INFO;
  Exception_Debug_InfoP:               WinBase.LPEXCEPTION_DEBUG_INFO;
  ExceptionRecordP:                    WinNT.PEXCEPTION_RECORD;
  Load_DLL_Debug_InfoP:                WinBase.LPLOAD_DLL_DEBUG_INFO;
  UnLoad_DLL_Debug_InfoP:              WinBase.LPUNLOAD_DLL_DEBUG_INFO;
  
  Done:                                BOOLEAN;
  ErrorCode:                           LONGINT;
  ErrorMessage:                        ARRAY 1024 OF CHAR;
  i,
  Index:                               INTEGER;
  LineOfText:                          ARRAY 256 OF CHAR;
  LineNumber:                          LONGINT;
  MyLine:                              LineOfTextP;
  MyChars:                             ARRAY  32 OF CHAR;
  nIndex:                              LONGINT;
  Result:                              WinDef.LRESULT;
  ReturnCode:                          LONGINT;
  ResultBool:                          WinDef.BOOL;
  
BEGIN

  SYSTEM.GET(ADRDebugProcess, DebugProcessP);

  LineOfText   := "Debug.DebugTheProcess: ";
  Strings.Append (LineOfText, DebugProcessP^.szModule);
  Strings.Append (LineOfText, " Started.");
  View.AppendLine(DebugProcessP^.hWnd, LineOfText);

  (* initialize process startup information *)
  StartupInfo.cb               := SIZE(WinBase.STARTUPINFO);
  StartupInfo.lpReserved       := WinDef.NULL;
  StartupInfo.lpDesktop        := WinDef.NULL;
  StartupInfo.lpTitle          := WinDef.NULL;
  StartupInfo.dwX              := 0;
  StartupInfo.dwY              := 0;
  StartupInfo.dwXSize          := 0;
  StartupInfo.dwYSize          := 0;
  StartupInfo.dwXCountChars    := 0;
  StartupInfo.dwYCountChars    := 0;
  StartupInfo.dwFillAttribute  := 0;
  StartupInfo.dwFlags          := WinBase.STARTF_FORCEOFFFEEDBACK + WinBase.STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow      := WinUser.SW_SHOWNORMAL;
  StartupInfo.cbReserved2      := 0;
  StartupInfo.lpReserved2      := WinDef.NULL;
  
  ProcessAttributes.nLength              := SIZE (WinUser.SECURITY_ATTRIBUTES);
  ProcessAttributes.lpSecurityDescriptor := 0;
  ProcessAttributes.bInheritHandle       := WinDef.False;

  ThreadAttributes.nLength               := SIZE (WinUser.SECURITY_ATTRIBUTES);
  ThreadAttributes.lpSecurityDescriptor  := 0;
  ThreadAttributes.bInheritHandle        := WinDef.False;

  (* generate signal completion of initialization to calling thread *)
  Result       := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                       IDS_DBGEVNTINITACK,
                                       SYSTEM.ADR(szEvent),
                                       255);
  hEvent       := WinBase.OpenEventA (WinNT.EVENT_MODIFY_STATE, WinDef.False, SYSTEM.ADR(szEvent));
  IF hEvent=0 THEN
    StatusLine.DisplayError (1);
  END;
  
  (* create debug process on module name *)
  IF WinBase.CreateProcessA  (WinDef.NULL,
                              SYSTEM.ADR(DebugProcessP^.szModule),
                              ProcessAttributes,
                              ThreadAttributes,
                              WinDef.False,
                              WinBase.DEBUG_PROCESS,
                              WinDef.NULL,
                              WinDef.NULL,
                              StartupInfo,
                              ProcessInformation)=WinDef.False THEN
    DebugProcessP^.dwProcessID := 0;
    ErrorCode                  := WinBase.GetLastError();
    IF ErrorCode#0 THEN
      StatusLine.DisplayError(ErrorCode);
    END (* IF ErrorCode#0 *);

    (* signal completion of initialization to calling thread *)
    Result       := WinBase.SetEvent (hEvent);
    Result       := WinBase.CloseHandle (hEvent);
    RETURN 0 
  END (* IF WinBase.CreateProcessA  (SYSTEM.ADR(DebugProcess.szModule), ...)=WinDef.False *);

  (* add initial thread to linked list *)
  NEW(DebugProcessP^.lpThreads);
  DebugProcessP^.lpThreads^.hThread        := ProcessInformation.hThread;
  DebugProcessP^.lpThreads^.dwThreadID     := ProcessInformation.dwThreadId;
  DebugProcessP^.lpThreads^.lpStartAddress := NIL;
  DebugProcessP^.lpThreads^.bfActive       := WinDef.True;
  DebugProcessP^.lpThreads^.nPriority      := SHORT(WinBase.GetThreadPriority (ProcessInformation.hThread));
  DebugProcessP^.lpThreads^.Next           := NIL;

  (* store process info *)
  DebugProcessP^.dwProcessID               := ProcessInformation.dwProcessId;
  DebugProcessP^.dwThreadID                := ProcessInformation.dwThreadId;
  DebugProcessP^.hProcess                  := ProcessInformation.hProcess;
  DebugProcessP^.ProcessPriority           := SHORT(WinBase.GetPriorityClass (ProcessInformation.hProcess));

  (* signal completion of initialization to calling thread *)
  Result       := WinBase.SetEvent (hEvent);
  IF Result=0 THEN
    StatusLine.DisplayError (1);
  END;
  Result        := WinBase.CloseHandle (hEvent);
  
  Done          := CreateDebugEvents (DebugEvents);
  
  lpDebugEvents := SYSTEM.VAL(LONGINT, SYSTEM.ADR(DebugEvents));

  (* start Debug event loop *)
  LOOP

    (* wait for Debugger active *)
    nIndex := WinBase.WaitForMultipleObjects (nDEBUGEVENTS, DebugEvents[0], WinDef.False, WinBase.INFINITE);
    CASE nIndex OF

      CLOSEDEBUGGER:
        (* terminate Debuggee process *)
        ResultBool := WinBase.TerminateProcess (DebugProcessP^.hProcess, 0);
        (* signal close acknowledge event *)
        Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                       IDS_DBGEVNTCLOSEACK,
                                       SYSTEM.ADR(szEvent),
                                       255);

        hEvent := WinBase.OpenEventA (WinNT.EVENT_MODIFY_STATE, WinDef.False, SYSTEM.ADR(szEvent));
        IF hEvent=0 THEN
          StatusLine.DisplayError (1);
        END;
        Result := WinBase.SetEvent (hEvent);

        (* close all Debug events *)
        FOR i:=0 TO nDEBUGEVENTS-1 DO
          Result := WinBase.CloseHandle (DebugEvents[i]);
        END (* FOR i:=0 TO nDEBUGEVENTS-1 *);

        Result := WinBase.SetEvent (hEvent);
        IF Result=0 THEN
          StatusLine.DisplayError (1);
        END;

        Result := WinBase.CloseHandle (hEvent);

        (* exit Debugger now *)
        WinBase.ExitThread(0);
      | (* CLOSEDEBUGGER *)

      SUSPENDDEBUGGER:
        View.AppendLine (DebugProcessP^.hWnd, "Suspend Debugger.");
        Result := WinBase.ResetEvent (DebugEvents[DEBUGACTIVE]);
        Result := WinBase.ResetEvent (DebugEvents[SUSPENDDEBUGGER]);
        ActDebugThread := DebugProcessP^.lpThreads;
        REPEAT
          Result           := WinBase.SuspendThread(ActDebugThread^.hThread);
          ActDebugThread   := ActDebugThread^.Next;
        UNTIL ActDebugThread=NIL;
      | (* SUSPENDDEBUGGER *)

      RESUMEDEBUGGER:
        View.AppendLine (DebugProcessP^.hWnd, "Resume Debugger.");
        Result := WinBase.SetEvent   (DebugEvents[DEBUGACTIVE]);
        Result := WinBase.ResetEvent (DebugEvents[RESUMEDEBUGGER]);
        ActDebugThread := DebugProcessP^.lpThreads;
        REPEAT
          Result           := WinBase.ResumeThread(ActDebugThread^.hThread);
          ActDebugThread   := ActDebugThread^.Next;
        UNTIL ActDebugThread=NIL;
      | (* RESUMEDEBUGGER *)

      DEBUGACTIVE:
        (* if Debug active *)
        IF (WinBase.WaitForDebugEvent(DebugEvent, 100)=WinDef.True) THEN

          IF DebugEvent.dwProcessId=DebugProcessP^.dwProcessID THEN

            CASE DebugEvent.dwDebugEventCode OF
              
              WinBase.EXCEPTION_DEBUG_EVENT:
                IF DebugProcessP^.FirstBreakpoint THEN
                  Exception_Debug_InfoP  := SYSTEM.VAL(WinBase.LPEXCEPTION_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                  ExceptionRecordP       := SYSTEM.VAL(WinNT.PEXCEPTION_RECORD, SYSTEM.ADR(Exception_Debug_InfoP^.ExceptionRecord));
                  LineOfText             := "Debug Active: Exception Debug Event  [";
                  Strings.UHexStr(ExceptionRecordP^.ExceptionCode, 4, MyChars);
                  Strings.RightAlign(MyChars, 9);
                  Strings.Append(LineOfText, MyChars);
                  Strings.Append(LineOfText, "]");
                  View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                  CASE SYSTEM.LOWORD(ExceptionRecordP^.ExceptionCode) OF
                    STATUS_ACCESS_VIOLATION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Access Violation!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to read from or write to a virtual address for which it does not have the appropriate access.");
                   | (* WinNT.STATUS_ *)
                    STATUS_ARRAY_BOUNDS_EXCEEDED:
                     View.AppendLine (DebugProcessP^.hWnd, "              Array Bounds Exceeded!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to access an array element that is out of bounds and the underlying hardware supports bounds checking.");
                   | (* WinNT.STATUS_ *)
                    STATUS_BREAKPOINT:
                     View.AppendLine (DebugProcessP^.hWnd, "              Breakpoint!");
                     View.AppendLine (DebugProcessP^.hWnd, "              A breakpoint was encountered.");
                   | (* WinNT.STATUS_ *)
                    STATUS_DATATYPE_MISALIGNMENT:
                     View.AppendLine (DebugProcessP^.hWnd, "              Datatype Misalignment!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to read or write data that is misaligned on hardware that does not provide alignment.");
                     View.AppendLine (DebugProcessP^.hWnd, "              For example, 16-bit values must be aligned on 2-byte boundaries; 32-bit values on 4-byte boundaries, and so on.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_DENORMAL_OPERAND:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Denormal Pperand!");
                     View.AppendLine (DebugProcessP^.hWnd, "              One of the operands in a floating-point operation is denormal.");
                     View.AppendLine (DebugProcessP^.hWnd, "              A denormal value is one that is too small to represent as a standard floating-point value.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_DIVIDE_BY_ZERO:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Divide By Zero!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to divide a floating-point value by a floating-point divisor of zero.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_INEXACT_RESULT:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Inexact Result!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The result of a floating-point operation cannot be represented exactly as a decimal fraction.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_INVALID_OPERATION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Invalid Operation!");
                     View.AppendLine (DebugProcessP^.hWnd, "              This exception represents any floating-point exception not included in this list.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_OVERFLOW:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Overflow!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The exponent of a floating-point operation is greater than the magnitude allowed by the corresponding type.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_STACK_CHECK:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Stack Check!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The stack overflowed or underflowed as the result of a floating-point operation.");
                   | (* WinNT.STATUS_ *)
                    STATUS_FLOAT_UNDERFLOW:
                     View.AppendLine (DebugProcessP^.hWnd, "              Float Underflow!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The exponent of a floating-point operation is less than the magnitude allowed by the corresponding type.");
                   | (* WinNT.STATUS_ *)
                    STATUS_ILLEGAL_INSTRUCTION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Illegal Instruction!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to execute an invalid instruction.");
                   | (* WinNT.STATUS_ *)
                    STATUS_IN_PAGE_ERROR:
                     View.AppendLine (DebugProcessP^.hWnd, "              In Page Error!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to access a page that was not present, and the system was unable to load the page.");
                     View.AppendLine (DebugProcessP^.hWnd, "              For example, this exception might occur if a network connection is lost while running a program over the network.");
                   | (* WinNT.STATUS_ *)
                    STATUS_INTEGER_DIVIDE_BY_ZERO:
                     View.AppendLine (DebugProcessP^.hWnd, "              Integer Divide By Zero!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to divide an integer value by an integer divisor of zero.");
                   | (* WinNT.STATUS_ *)
                    STATUS_INTEGER_OVERFLOW:
                     View.AppendLine (DebugProcessP^.hWnd, "              Integer Overflow!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The result of an integer operation caused a carry out of the most significant bit of the result.");
                   | (* WinNT.STATUS_ *)
                    STATUS_INVALID_DISPOSITION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Invalid Disposition!");
                     View.AppendLine (DebugProcessP^.hWnd, "              An exception handler returned an invalid disposition to the exception dispatcher.");
                     View.AppendLine (DebugProcessP^.hWnd, "              Programmers using a high-level language such as C should never encounter this exception.");
                   | (* WinNT.STATUS_ *)
                    STATUS_NONCONTINUABLE_EXCEPTION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Noncontinuable Exception!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to continue execution after a noncontinuable exception occurred.");
                   | (* WinNT.STATUS_ *)
                    STATUS_PRIVILEGED_INSTRUCTION:
                     View.AppendLine (DebugProcessP^.hWnd, "              Privileged Instruction!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread tried to execute an instruction whose operation is not allowed in the current machine mode.");
                   | (* WinNT.STATUS_ *)
                    STATUS_SINGLE_STEP:
                     View.AppendLine (DebugProcessP^.hWnd, "              Single Step!");
                     View.AppendLine (DebugProcessP^.hWnd, "              A trace trap or other single-instruction mechanism signaled that one instruction has been executed.");
                   | (* WinNT.STATUS_ *)
                    STATUS_STACK_OVERFLOW:
                     View.AppendLine (DebugProcessP^.hWnd, "              Stack Overflow!");
                     View.AppendLine (DebugProcessP^.hWnd, "              The thread used up its stack.");
                   (* WinNT.STATUS_ *)
                    ELSE
                     View.AppendLine (DebugProcessP^.hWnd, "              Unknown Exception.");
                  END;
                  LineOfText := "              at Address ";
                  Strings.UHexStr(ExceptionRecordP^.ExceptionAddress, 4, MyChars);
                  Strings.InsertChar(":", MyChars, 5);
                  Strings.RightAlign(MyChars, 12);
                  Strings.Append(LineOfText, MyChars);
                  View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                  Done       := Dump_DD_Debug.GetLineNumber(ExceptionRecordP^.ExceptionAddress, LineOfText, LineNumber);
                  Strings.Insert("              in Line ", LineOfText, 1);
                  Strings.AppendChar(LineOfText, "#");
                  Strings.Str(LineNumber, MyChars);
                  Strings.RightAlign(MyChars, 6);
                  Strings.Append(LineOfText, MyChars);
                  View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                  Result     := WinBase.SetEvent (DebugEvents[SUSPENDDEBUGGER]); (* Pause to have a look at what has happened *)
                ELSE
                  View.AppendLine (DebugProcessP^.hWnd,  "Debug Active: First Breakpoint encountered.");
                  View.AppendLine (DebugProcessP^.hWnd,  "              This means the process is running.");
                  DebugProcessP^.FirstBreakpoint := TRUE;
                END (* IF DebugProcessP^.FirstBreakpoint *);
              | (* WinBase.EXCEPTION_DEBUG_EVENT *)
      
              WinBase.CREATE_PROCESS_DEBUG_EVENT:
                Create_Process_Debug_InfoP               := SYSTEM.VAL(WinBase.LPCREATE_PROCESS_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                (* add process information to linked list *)
                DebugProcessP^.hFile                     := Create_Process_Debug_InfoP.hFile;
                DebugProcessP^.hProcess                  := Create_Process_Debug_InfoP.hProcess;
                DebugProcessP^.lpImage                   := Create_Process_Debug_InfoP.lpBaseOfImage;
                DebugProcessP^.dwDbgInfoOffset           := Create_Process_Debug_InfoP.dwDebugInfoFileOffset;
                DebugProcessP^.nDbgInfoSize              := Create_Process_Debug_InfoP.nDebugInfoSize;
                DebugProcessP^.FirstBreakpoint           := FALSE;

                (* add start address of initial thread *)
                DebugProcessP^.lpThreads^.lpStartAddress := Create_Process_Debug_InfoP.lpStartAddress;
        
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Create Process Debug Event.");
                LineOfText := "              Process ID";
                Strings.Str(DebugProcessP^.dwProcessID, MyChars);
                Strings.RightAlign(MyChars, 5);
                Strings.Append(LineOfText, MyChars);
                View.AppendLine (DebugProcessP^.hWnd, LineOfText);
              | (* WinBase.CREATE_PROCESS_DEBUG_EVENT *)
      
              WinBase.CREATE_THREAD_DEBUG_EVENT:
                (* add new thread to linked list *)
                Create_Thread_Debug_InfoP    := SYSTEM.VAL(WinBase.LPCREATE_THREAD_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                ActDebugThread := DebugProcessP^.lpThreads;
                WHILE ActDebugThread#NIL DO
                  ActDebugThread := ActDebugThread^.Next;
                END;
                NEW(ActDebugThread);
                ActDebugThread^.hThread      := Create_Thread_Debug_InfoP.hThread;
                ActDebugThread^.dwThreadID   := DebugEvent.dwThreadId;
                ActDebugThread^.lpStartAddress := Create_Thread_Debug_InfoP.lpStartAddress;
                ActDebugThread^.bfActive     := WinDef.True;
                ActDebugThread^.nPriority    := SHORT(WinBase.GetThreadPriority (Create_Thread_Debug_InfoP.hThread));
                ActDebugThread^.Next         := NIL;
                View.AppendLine (DebugProcessP^.hWnd,  "Debug Active: Create Thread Debug Event.");
              | (* WinBase.CREATE_THREAD_DEBUG_EVENT *)
      
              WinBase.EXIT_PROCESS_DEBUG_EVENT:
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Exit Process Debug Event.");
                (* Result := WinUser.PostMessageA (DebugProcessP^.hWnd, WinUser.WM_CLOSE, 0, 0); *)
              | (* WinBase.EXIT_PROCESS_DebugEventBUG_EVENT *)
      
              WinBase.EXIT_THREAD_DEBUG_EVENT:
                (* remove this thread from linked list *)
                Exit_Thread_Debug_InfoP      := SYSTEM.VAL(WinBase.LPEXIT_THREAD_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                ActDebugThread               := DebugProcessP^.lpThreads;
                MyDebugThread                := NIL;
                WHILE ((ActDebugThread^.dwThreadID#DebugEvent.dwThreadId) & (ActDebugThread^.Next#NIL)) DO
                  MyDebugThread  := ActDebugThread;
                  ActDebugThread := ActDebugThread^.Next;
                END;
                IF ActDebugThread^.dwThreadID=DebugEvent.dwThreadId THEN
                  MyDebugThread^.Next  := ActDebugThread^.Next;
                  DISPOSE(ActDebugThread);
                END (* IF ActDebugThread^.dwThreadID=DebugEvent.dwThreadId *);
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Exit Thread Debug Event.");
              | (* WinBase.EXIT_THREAD_DEBUG_EVENT *)
      
              WinBase.RIP_EVENT:
                (* standard rip event message *)
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: RIP Event.");
        
                (* check severity type of this rip event 
                CASE DebugEvent.u.RiProcessInformation.dwType OF
                  1:
                    View.AppendLine (DebugProcess.dwThreadID,
                                     "Level 1.");
                  | (* 1 *)
      
                  2:
                    View.AppendLine (DebugProcess.dwThreadID,
                                     "Level 2.");
                  | (* 2 *)
      
                  3:
                    View.AppendLine (DebugProcess.dwThreadID,
                                     "Level 3.");
                  (* 3 *)
                  ELSE
                    View.AppendLine (DebugProcess.dwThreadID,
                                     "Level unknown.");
                END  CASE DebugEvent.u.RiProcessInformation.dwType *);
      
                (* get formatted message from system 
                WinBase.FormatMessageA (WinBase.FORMAT_MESSAGE_ALLOCATE_BUFFER + WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                                        WinDef.NULL,
                                        DebugEvent.u.RipInfo.dwError,
                                        MAKELONG (MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), 0),
                                        lpszBuffer,
                                        0,
                                        WinDef.NULL);*)
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: RIP Event (closed).");
              | (* RIP_EVENT *)
  
              WinBase.LOAD_DLL_DEBUG_EVENT:
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Load DLL Debug Event.");
                Load_DLL_Debug_InfoP   := SYSTEM.VAL(WinBase.LPLOAD_DLL_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                IF Load_DLL_Debug_InfoP^.hFile=0 THEN
                  View.AppendLine (DebugProcessP^.hWnd,"              DLL's Name Not Available(0);");
                ELSE
                  Result := WinBase.GetModuleFileNameA (WinBase.GetModuleHandleA(WinDef.NULL), SYSTEM.ADR(LineOfText), 255);
                  IF Result>0 THEN
                    Strings.Insert("              ", LineOfText, 1);
                    View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                  ELSE
                    Result     := WinBase.FormatMessageA(WinBase.FORMAT_MESSAGE_FROM_SYSTEM,
                                         0,
                                         WinBase.GetLastError(),
                                         SYSTEM.MAKELONG(WinNT.SUBLANG_SYS_DEFAULT, WinNT.LANG_NEUTRAL),
                                         SYSTEM.ADR(LineOfText),
                                         LEN(LineOfText),
                                         0);
                    Strings.Insert("              DLL's Name Not Available(2); Err.", LineOfText, 1);
                    View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                    LineOfText := "              Process Handle: ";
                    Strings.UHexStr(DebugProcessP^.hProcess, 4, MyChars);
                    Strings.InsertChar(":", MyChars, 5);
                    Strings.RightAlign(MyChars, 12);
                    Strings.Append(LineOfText, MyChars);
                    Strings.Append(LineOfText, "     Module Handle: ");
                    Strings.UHexStr(WinBase.GetModuleHandleA(WinDef.NULL), 4, MyChars);
                    Strings.InsertChar(":", MyChars, 5);
                    Strings.RightAlign(MyChars, 12);
                    Strings.Append(LineOfText, MyChars);
                    View.AppendLine (DebugProcessP^.hWnd, LineOfText);
                  END (* IF Result>0 *);
                END (* IF ... *);
                LineOfText := "              at Address ";
                Strings.UHexStr(Load_DLL_Debug_InfoP^.lpBaseOfDll, 4, MyChars);
                Strings.InsertChar(":", MyChars, 5);
                Strings.RightAlign(MyChars, 12);
                Strings.Append(LineOfText, MyChars);
                View.AppendLine (DebugProcessP^.hWnd, LineOfText);
              | (* WinBase.LOAD_DLL_DEBUG_EVENT *)
  
              WinBase.UNLOAD_DLL_DEBUG_EVENT:
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: UnLoad DLL Debug Event.");
                UnLoad_DLL_Debug_InfoP := SYSTEM.VAL(WinBase.LPUNLOAD_DLL_DEBUG_INFO, SYSTEM.ADR(DebugEvent.u));
                LineOfText := "              at Address ";
                Strings.UHexStr(UnLoad_DLL_Debug_InfoP^.lpBaseOfDll, 4, MyChars);
                Strings.InsertChar(":", MyChars, 5);
                Strings.RightAlign(MyChars, 12);
                Strings.Append(LineOfText, MyChars);
                View.AppendLine (DebugProcessP^.hWnd, LineOfText);
              | (* WinBase.UNLOAD_DLL_DEBUG_EVENT *)
  
              WinBase.OUTPUT_DEBUG_STRING_EVENT:
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Output Debug String Event.");
              (* WinBase.OUTPUT_DEBUG_STRING_EVENT *)
  
              ELSE
                View.AppendLine (DebugProcessP^.hWnd, "Debug Active: Notify Of Sibling Process Debug Event.");
                (* notify of sibling process Debug event *)
            END (* CASE DebugEvent.dwDebugEventCode *)
          ELSE
            View.AppendLine (DebugProcessP^.hWnd, "Debug Active, but not my process.");
          END (* IF DebugEvent.dwProcessId=DebugProcess.dwProcessID *);
        END (* IF (WinBase.WaitForDebugEvent (DebugEvent, 100)=WinDef.True) *);
        
        Result := WinBase.ContinueDebugEvent (DebugEvent.dwProcessId, DebugEvent.dwThreadId, WinNT.DBG_CONTINUE);
      (* DEBUGACTIVE *)

      ELSE
        View.AppendLine (DebugProcessP^.hWnd, "ELSE.");
        ;

    END (* CASE nInDex *);
      
  END (* LOOP *);

  RETURN 0
    
END DebugTheProcess;


(*****************************************************************************)
(*                                                                           *)
(* Start                                                                     *)
(* Starts the Debugger                                                       *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE Start*                      (VAR MyDebugProcess: Global.DebugProcessP)
                                      :WinDef.HANDLE;

VAR
  Done:                                BOOLEAN;
  hEvent:                              WinDef.HANDLE;
  ThreadID:                            WinDef.DWORD;
  
BEGIN

  (* Create initialize event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 IDS_DBGEVNTINITACK,
                                 SYSTEM.ADR(szEvent),
                                 255);
  hEvent := WinBase.CreateEventA (SecurityAttributes, WinDef.True, WinDef.False, SYSTEM.ADR(szEvent));

  ThreadID                     :=  1;
  
  COPY (Global.MyFileDescription.Path, MyDebugProcess^.szModule);
  MyDebugProcess^.lpThreads    := NIL;
  
  MyDebugProcess^.hThread      := WinBase.CreateThread(SecurityAttributes,
                                                       4096,
                                                       DebugTheProcess,
                                                       SYSTEM.ADR(MyDebugProcess),
                                                       0,
                                                       ThreadID);
  MyDebugProcess^.dwProcessID  := ThreadID;
  IF MyDebugProcess^.hThread=0 THEN
    StatusLine.SetText("Debug.StartDebugger: Starting Thread failed.", StatusLine.NoticeField);
  ELSE
    StatusLine.SetText("Debug.StartDebugger: Thread started.", StatusLine.NoticeField);
    (* Wait for debugger to complete initialization *)
    Result := WinBase.WaitForSingleObject (hEvent, WinBase.INFINITE);
    Result := WinBase.CloseHandle(hEvent);
    IF MyDebugProcess^.dwProcessID=0 THEN
      StatusLine.SetText("Debug.StartDebugger: Process creation failed.", StatusLine.NoticeField);
      RETURN 0
    END;
  
    StatusLine.SetText("Debug.StartDebugger: Thread started & set event handle .", StatusLine.NoticeField);
  
    Done       := CreateDebugEvents (DebugEvents);
  
  END;
  
  RETURN MyDebugProcess^.hThread

END Start;


(*****************************************************************************)
(*                                                                           *)
(* Suspend                                                                   *)
(* Interrupts the debugger                                                   *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE Suspend*                    ()
                                      :WinDef.LRESULT;

BEGIN

  Result       := WinBase.SetEvent (DebugEvents[SUSPENDDEBUGGER]);
  
  RETURN 0

END Suspend;


(*****************************************************************************)
(*                                                                           *)
(* Resume                                                                    *)
(* Restarts the debugger                                                     *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE Resume*                     (MyDebugProcess:     Global.DebugProcessP)
                                      :WinDef.LRESULT;

BEGIN

  Result       := WinBase.SetEvent (DebugEvents[RESUMEDEBUGGER]);
  
  RETURN 0

END Resume;


(*****************************************************************************)
(*                                                                           *)
(* Stop                                                                      *)
(* Stop the debugger                                                         *)
(*                                                                           *)
(* INPUT:                                                                    *)
(*                                                                           *)
(* OUTPUT:                                                                   *)
(*  LRESULT    wie war's                                                     *)
(*                                                                           *)
(* MESSAGES:                                                                 *)
(*                                                                           *)
(* COMMENTS:                                                                 *)
(*                                                                           *)
(*****************************************************************************)
PROCEDURE Stop*                       (VAR MyDebugProcess: Global.DebugProcessP)
                                      :WinDef.HANDLE;

VAR
  Done:                                BOOLEAN;
  hEvent:                              WinDef.HANDLE;
  i:                                   INTEGER;
  
BEGIN

  (* Create acknowledge close event *)
  Result := WinUser.LoadStringA (WinBase.GetModuleHandleA (WinDef.NULL),
                                 IDS_DBGEVNTCLOSEACK,
                                 SYSTEM.ADR(szEvent),
                                 255);
  hEvent := WinBase.CreateEventA (SecurityAttributes, WinDef.True, WinDef.False, SYSTEM.ADR(szEvent));

  StatusLine.SetText("Debug.StopDebugger: Event IDS_DBGEVNTCLOSEACK generated.", StatusLine.NoticeField);

  (* Set close event for debug thread and wait ... *)
  Result       := WinBase.SetEvent (DebugEvents[CLOSEDEBUGGER]);
  Result := WinBase.WaitForSingleObject (hEvent, WinBase.INFINITE);
  StatusLine.SetText("Debug.StopDebugger: Thread closed.", StatusLine.NoticeField);

  FOR i:=0 TO nDEBUGEVENTS-1 DO
    Result := WinBase.CloseHandle(DebugEvents[i]);
  END;
  Result := WinBase.CloseHandle(hEvent);

  RETURN 0

END Stop;


(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)
BEGIN;

  SecurityAttributes.nLength   := SIZE(WinBase.SECURITY_ATTRIBUTES);
  SecurityAttributes.lpSecurityDescriptor  
                               := 0;
  SecurityAttributes.bInheritHandle
                               := WinDef.True;

END Debug.

