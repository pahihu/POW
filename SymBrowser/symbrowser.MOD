(* Project Changes
     @KDS 2005-05-25 : - New module "SBWin32", subset of WinApi
                         => removed win32.lib from project;
                       - Use WinApi calls iso Opal module 'File'
                       - removed module 'ExternM' from project (was not used)
                       - Opal.Utils.LoWord, HiWord replaced by SYSTEM.HIWORD, LOWORD
                       - 'PowDDE' : Bugfix in Callback procedure (no result returned)
                                         adapted to Type Safe interface defined in SBWin32
                       - 'Constant': do not display export marker for constants
                       - 'MiscSym': display [_APICALL] iso [WINDOWS] (procedure variables)
                                    Added whitespace after ";" & ":" in parameter lists
                                    export marker only for readonly vars ("-")
                       - 'SymBrowser: 'ofn' is global variable
                       - removed several unused variables
                       - 'SymBrowser': Better Parse Params procedure:
                           it is now possible to associate "*.Sym" with the browser in Windows explorer,
                           drag & drop and paste a sym file on the browser.exe
                           a browser window will show up if Pow! is running
                           Bugfix: lstrcpynA iso lstrcpyA: buffer overflow on 'cmd'; 'cmd' buffer size from 100 to 260
 *)
 
MODULE SymBrowser;

(********************************************************************************
 * Autor   : Gerhard Kurka                                                      *
 *           Koen Desaeger                                                      *
 * Project : Symbol file browser ( viewer ) for POW-Oberon-2 symbol-files       *
 ********************************************************************************
 * This is the main module, it implements the user                              *
 * interface to open symfiles.                                                  *
 ********************************************************************************)

IMPORT
  WD := SBWin32 (*@KDS 2005-05-25 *),
  WU := SBWin32 (*@KDS 2005-05-25 *),
  WB := SBWin32 (*@KDS 2005-05-25 *),
  CommDlg := SBWin32 (*@KDS 2005-05-25 *),
  SYSTEM,
  Strings,
  Parse, SymFile, PowDDE;
  
VAR (* KDS 2005-05-25 global => zero init *)
  ofn: CommDlg.OPENFILENAME;
  
  
(* Replaces the file-extension by .EXP *) 
PROCEDURE ChangeName(VAR PathName, NameOut: ARRAY OF CHAR; lenOfPathName:LONGINT);
VAR
  c,l,i,j:LONGINT;
BEGIN
  l := lenOfPathName-1;
  c := 0;
  IF l > 0 THEN
    i:=l;
    WHILE (i # -1) & (PathName[i] # '\') DO
      IF PathName[i] = '.' THEN
        c := c + 1;
      END;
      i := i - 1;
    END;
    FOR j := i + 1 TO l DO
      NameOut[j-(i+1)] := PathName[j];
    END;
    l := l - i;
    NameOut[l] := CHR(0);
    IF c > 0 THEN
      NameOut[l-1] := 'P';
      NameOut[l-2] := 'X';
      NameOut[l-3] := 'E';
    ELSE
      Strings.Append(NameOut, '.EXP');
    END;
  END;      
END ChangeName;

  
(*@kds
PROCEDURE ParseParams(VAR fileName: ARRAY OF CHAR; param:ARRAY OF CHAR):BOOLEAN;
VAR
  i,n :INTEGER;
BEGIN
  n := 0;
  SymFile.debug := FALSE;
  SymFile.allMod:= FALSE;
  IF param[0]='"' THEN
    i:=1;
    WHILE (param[i] # 0X) & (param[i] # '"') DO INC(i) END; (* skip program name *)
  ELSE
    i:=0;
    WHILE (param[i] # 0X) & (param[i] # " ") DO INC(i) END; (* skip program name *)
  END;
  IF param[i]#0X THEN INC(i) END;
  WHILE (param[i] # CHR(0)) DO
    WHILE (param[i] = CHR(32)) DO
      i := i + 1;
    END;
    IF param[i] = '-' THEN (* optionstring found *)
      i := i + 1;
      WHILE (param[i] # CHR(0)) & ((param[i]= 'a') OR (param[i]='g')) DO
        IF param[i] = 'g' THEN
          SymFile.debug := TRUE;
        END;
        IF param[i] = 'a' THEN
          SymFile.allMod := TRUE;
        END;
        i := i + 1;
      END;
      IF (param[i] # CHR(32)) & (param[i] # CHR(0)) THEN
        RETURN FALSE;
      END;
    END;
    IF (n # 0) & (param[i] # CHR(0)) & (param[i] # CHR(32)) THEN
      RETURN FALSE;
    END;
    WHILE (param[i] # CHR(0)) & (param[i] # CHR(32)) DO
      fileName[n] := param[i];
      i := i + 1;
      n := n + 1;
    END;
  END;
  fileName[n] := CHR(0);          
  RETURN TRUE;
END ParseParams;
*)

  (* Parses the option strings an extracts the full filename              *)
  (* IF an error occurs, the value returned by the function will be FALSE *)
  PROCEDURE ParseParams ( VAR fileName: ARRAY OF CHAR; param:ARRAY OF CHAR ): BOOLEAN;
    VAR i, n : LONGINT; invalidParam: BOOLEAN;
    BEGIN
      SymFile.debug := FALSE;
      SymFile.allMod:= FALSE;
      i := 0;
      WHILE (param[i] # ' ') & (param[i] # 0X) DO INC(i) END; (* skip program name *)
      n := 0; invalidParam := FALSE; 
      WHILE param[i] # 0X DO
        IF (param[i] = '"') THEN 
          INC(i)
        ELSIF (param[i] = ' ') THEN
          IF n > 0 THEN
            fileName[n] := 0X;
            n := -1
          END;
          INC(i)
        ELSIF param[i] = '-' THEN
          IF n > 0 THEN
            fileName[n] := param[i];
            INC(n);
            INC(i)
          ELSE(*optionstring found*)
            INC(i);
            IF param[i] = 'g' THEN
              SymFile.debug := TRUE
            ELSIF param[i] = 'a' THEN
              SymFile.allMod := TRUE
            ELSE invalidParam := TRUE
            END;
            IF param[i] # 0X THEN INC(i) END
          END
        ELSE
          IF n = -1 THEN
          RETURN FALSE
          ELSE
            fileName[n] := param[i];
            INC(i);
            INC(n)
          END
        END
      END;
      IF n >= 0 THEN fileName[n] := 0X END;
      IF invalidParam THEN
        IF WU.MessageBoxA(0, SYSTEM.ADR("Invalid parameter(s) ignored!"),
                             SYSTEM.ADR("SymBrowser") , WU.MB_OK) = WU.IDOK 
        THEN 
        END
      END;
      RETURN TRUE
    END ParseParams;


PROCEDURE CopyString(VAR d:ARRAY OF CHAR; s:ARRAY OF CHAR);
VAR i:INTEGER;
BEGIN
  i:= 0;
  d[i]:= s[i];
  WHILE d[i] # CHR(0) DO
    i := i + 1;
    d[i] := s[i];
  END;
END CopyString;

PROCEDURE UseFileDialog(VAR name: ARRAY OF CHAR): BOOLEAN(*LONGINT*);
VAR
  (*ret: BOOLEAN; @kds not used *)
  filter: ARRAY 40 OF CHAR;
BEGIN
  CopyString(filter, 'Oberon symbolfile_*.SYM');
  filter[17] := CHR(0);
  filter[24] := CHR(0);
  ofn.lStructSize       := SIZE(CommDlg.OPENFILENAME);
  ofn.lpstrFilter       := SYSTEM.ADR(filter);
  ofn.lpstrFile         := SYSTEM.ADR(name[0]);
  ofn.nMaxFile          := 80;
  ofn.lpstrTitle        := SYSTEM.ADR('Browser');
  ofn.Flags             := 0H(*CommDlg.OFN_FILEMUSTEXIST*);
  ofn.lpstrDefExt       := SYSTEM.ADR('SYM'); 
(* KDS 2005-05-25 ofn is a global iso local >> zero initialized 
  ofn.hwndOwner         := 0;
  ofn.hInstance         := 0;
  ofn.lpstrFilter       := SYSTEM.ADR(filter);
  ofn.lpstrCustomFilter := 0;
  ofn.nFilterIndex      := 0;
  ofn.nMaxCustFilter    := 0;
  ofn.lpstrFile         := SYSTEM.ADR(name);
  ofn.nMaxFile          := 80;
  ofn.lpstrFileTitle    := 0;
  ofn.nMaxFileTitle     := 0;
  ofn.lpstrInitialDir   := 0;
  ofn.lpstrTitle        := SYSTEM.ADR('Browser');
  ofn.Flags             := CommDlg.OFN_FILEMUSTEXIST;
  ofn.nFileOffset       := 0;
  ofn.lCustData         := 0;
  ofn.lpfnHook          := NIL;
  ofn.lpstrDefExt       := SYSTEM.ADR('SYM');
  *)
 RETURN CommDlg.GetOpenFileNameA(ofn)  
END UseFileDialog;

PROCEDURE [_APICALL] WinMain*(hInstance:WD.HINSTANCE;
                             lpCmdLine:WD.LPSTR; nCmdShow:LONGINT):LONGINT;
VAR
  cmd        : ARRAY 260 OF CHAR; (*kds was 100 *)
  adr        : LONGINT;
  paramText  : ARRAY 80 OF CHAR;
  str        : ARRAY 80 OF CHAR;
  ret        : BOOLEAN;
BEGIN
  ret := TRUE;
  adr := WB.lstrcpynA(SYSTEM.ADR(cmd), lpCmdLine, LEN(cmd));
  IF ParseParams(paramText, cmd) THEN
    IF paramText[0] = CHR(0) THEN
      CopyString(paramText, "*.SYM");
      ret := UseFileDialog(paramText);
    END;
    IF ret & PowDDE.CreatePowConnection() THEN
      ChangeName(paramText, str, Strings.Length(paramText));
      Strings.Insert("NewFile ",str,0);
      IF Parse.InitParser(paramText) THEN
        PowDDE.SendStringToPow(str);
        Parse.PSymFile();
      END;
      PowDDE.DestroyPowConnection();
    ELSE
      IF ret THEN
        ret := WU.MessageBoxA(0, SYSTEM.ADR('POW-DDE Connection Failed'),
                               SYSTEM.ADR('Browser DDE Error'), WU.MB_OK) = WU.IDOK;
      END;
    END;
  ELSE
    IF WU.MessageBoxA(0, SYSTEM.ADR('Wrong Parameters'),
                           SYSTEM.ADR('Browser Error'), WU.MB_OK) = 0 THEN END;
  END;  
  RETURN 1;
END WinMain;

END SymBrowser.
