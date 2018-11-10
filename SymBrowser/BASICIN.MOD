 (* Changes
     @KDS 2005-05-25 : uses WinBase Api iso Opal File module
     
 *)
 
MODULE BasicIn;

(********************************************************************************
 * Autor   : Gerhard Kurka                                                      *
 *           Koen Desager                                                       *
 * Project : Symbol file browser ( viewer ) for POW-Oberon-2 symbol-files       *
 ********************************************************************************
 * Provides simple LOW I/O input functions                                      *
 ********************************************************************************)

IMPORT SYSTEM

(*,File @KDS 2005-05-25*) ,WB := SBWin32
  ;

VAR result* : INTEGER; (* consider the FILE-MODULE *)
    size*: LONGINT;

TYPE (*@KDS 2005-05-25*)
  INT32 = LONGINT;

CONST (*@KDS 2005-05-25*)
  GENERIC_READ = MIN(LONGINT);
  FILE_SHARE_READ = 1H;
  INVALID_HANDLE_VALUE = -1;
  OPEN_EXISTING = 3;
  NOERROR = 0;
  EOFREACHED = 1;
  OTHER_ERROR = 2;
  
(*@KDS 2005-05-25*)
PROCEDURE ReadBlock ( hFile: INT32; VAR data : ARRAY OF SYSTEM.BYTE; n: INT32; VAR bytesRead: INT32; VAR res : INTEGER );
  VAR lres: INT32; 
  BEGIN
    lres := WB.ReadFile(hFile, SYSTEM.ADR(data[0]), n, bytesRead, 0);
    IF lres # 0 THEN
      IF bytesRead = 0 THEN
        res := EOFREACHED
      ELSE res := NOERROR
      END
    ELSE res := OTHER_ERROR;
    END
  END ReadBlock;
  
PROCEDURE ReadByte* ( VAR val: CHAR; fileHandle: LONGINT(*File.Handle*)): BOOLEAN;
VAR
  readN  : LONGINT;
BEGIN
  INC(size);
  (*File.*)ReadBlock(fileHandle, val, 1, readN, result);
  RETURN (readN = 1) & (result = (*File.*)NOERROR); 
END ReadByte;

PROCEDURE ReadShort* ( VAR val: INTEGER; fileHandle: LONGINT(*File.Handle*)): BOOLEAN;
VAR
  readN  : LONGINT;
BEGIN
  (*File.*)ReadBlock(fileHandle, val, 2, readN, result);
  RETURN (readN = 2) & (result = (*File.*)NOERROR);
END ReadShort;

PROCEDURE ReadLong*(VAR val:LONGINT; fileHandle: LONGINT(*File.Handle*)):BOOLEAN;
VAR
  readN  : LONGINT;
BEGIN
  (*File.*)ReadBlock(fileHandle, val, 4, readN, result);
  RETURN (readN = 4) & (result = (*File.*)NOERROR);
END ReadLong;  

PROCEDURE ReadReal*(VAR val: REAL; fileHandle: LONGINT(*File.Handle*)):BOOLEAN;
VAR
  readN : LONGINT;
BEGIN
  (*File.*)ReadBlock(fileHandle, val, 4, readN, result);
  RETURN (readN = 4) & (result = (*File.*)NOERROR);
END ReadReal;

PROCEDURE ReadLongReal*(VAR val: LONGREAL; fileHandle: LONGINT(*File.Handle*)):BOOLEAN;
VAR
  readN : LONGINT;
BEGIN
  (*File.*)ReadBlock(fileHandle, val, 8, readN, result);
  RETURN (readN = 8) & (result = (*File.*)NOERROR);
END ReadLongReal;

PROCEDURE OpenFile*(fileName: ARRAY OF CHAR; VAR fileHandle: LONGINT(*File.Handle*)): BOOLEAN;
VAR
  (*result : INTEGER;*)
BEGIN
  fileHandle := WB.CreateFileA(SYSTEM.ADR(fileName), GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
  RETURN fileHandle # INVALID_HANDLE_VALUE
  (* @KDS 2005-05-25
  File.Open(fileName, FALSE, File.DENYWRITE, File.READONLY, fileHandle, result);
  RETURN result = File.NOERROR;
  *)
END OpenFile;

PROCEDURE CloseFile*(fileHandle: LONGINT(*File.Handle*)):BOOLEAN;
BEGIN
  WB.CloseHandle(fileHandle);
  RETURN TRUE
  (* @KDS 2005-05-25
  File.Close(fileHandle);
  result := File.NOERROR;
  RETURN result = File.NOERROR;
  *)
END CloseFile;

PROCEDURE EOF*():BOOLEAN;
BEGIN
  RETURN result = (*File.*)EOFREACHED;
END EOF;

BEGIN
  size:=0;
END BasicIn.
