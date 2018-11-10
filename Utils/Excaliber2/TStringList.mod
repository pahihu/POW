(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Module                                                        *)
(*                                                                           *)
(* MODULE:     *)MODULE TStringList;(*                           V 1.0       *)
(*                                                                           *)
(* PURPOSE:  VIRTUAL LIST OF UP TO 32766 POINTERS                            *)
(*                                                                           *)
(* PROPETIES                                                                 *)
(*                                                                           *)
(* FUNCTIONS:  (p:PLIST) Init   * ();                                        *)
(*             (p:PLIST) Add    * (Pointer:SYSTEM.PTR);                      *)
(*             (p:PLSIT) Resize * ();                                        *)
(*             (p:PLIST) Remove * (itemToRemove:INTEGER);                    *)
(*             (p:PLIST) Item   * (itemToGet:INTEGER):SYSTEM.PTR;            *)
(*             (p:PLIST) Clear  * ();                                        *)
(*             (p:PLIST) Sort   * (lpCallback:SORTCALLBACK);                 *)
(*                                                                           *)
(* AUTHORS:    Steven Watson                                                 *) 
(*                                                                           *)
(* CONFIGURATION MANAGEMENT                                                  *)
(*                                                                           *)
(* CREATED    8 Sept 2001                                                    *)
(*                                                                           *)
(* UPDATED                                                                   *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(* RELEASED                                                                  *)
(*                                                                           *)
(*****************************************************************************)

IMPORT SYSTEM,WinBase,Display;
  
CONST

  MAX_LIST = MAX(INTEGER);

TYPE
  PCHAR* = POINTER TO ARRAY OF CHAR;  
  SORTCALLBACK = PROCEDURE(item1,item2:PCHAR):INTEGER;
  PTR_ARRAY = POINTER TO ARRAY OF PCHAR;
  
    
  PSTRINGLIST* = POINTER TO TSTRINGLIST;
    
  TSTRINGLIST = RECORD 
     List         : PTR_ARRAY;(* THE LIST *)
     Capacity*    : INTEGER;   (* SIZE OF THE LIST *)
     CurrPos      : INTEGER;   (* CURRENT POSITION OF LAST USED LIST INDEX *)
     Items-       : INTEGER;   (* HOW MANY ITEMS IN LIST *)
     Full-        : BOOLEAN;   (* LIST IS FULL *)
     Sorted-      : BOOLEAN;   (* IS LIST SORTED OR UNSORTED *)
  END;
  
  
PROCEDURE (p:PSTRINGLIST) Init*(); 

VAR

  i:INTEGER;
  temp:PSTRINGLIST;

BEGIN

    temp:=p;
   
    DISPOSE(temp.List);
 
    IF temp.Capacity > 0 THEN
    
        NEW(temp.List,temp.Capacity);
        ASSERT(temp # NIL); 
            FOR i:=0 TO temp.Capacity -1 DO

               DISPOSE(SYSTEM.VAL(PCHAR,temp.List[i]));
       
            END;
    END;

    temp.CurrPos:=-1;
    temp.Items:=0;
    temp.Sorted:=FALSE;
    temp.Full:=FALSE;
     
END Init ;

PROCEDURE (p:PSTRINGLIST) Resize*();

VAR

  ptrtemp:PTR_ARRAY;
  temp:PSTRINGLIST;
  i,test,count:INTEGER;

BEGIN
  
  temp:=p;
  i:=temp.Capacity;
  
  IF temp.Capacity < 5 THEN
    
    INC(temp.Capacity,4);

  ELSIF temp.Capacity < 17 THEN

      INC(temp.Capacity,8);
  
  ELSE

      test := MAX_LIST - temp.Capacity;
      
      IF test < 17 THEN

          INC(temp.Capacity,test);
      
      ELSE

          INC(temp.Capacity,16);
      
      END;
                
  END;
  
  NEW(ptrtemp,temp.Capacity);
  ASSERT(ptrtemp # NIL);
  
  FOR count:=0 TO i-1 DO

    ptrtemp^[count]:=temp^.List[count];
    temp.List[count]:=NIL;

  END;
  
  DISPOSE(temp.List);
  temp.List:=ptrtemp;
  
END Resize;



PROCEDURE (p:PSTRINGLIST) Add*(VAR Item:PCHAR);

VAR

temp:PSTRINGLIST;  

BEGIN

  temp:=p;
  
  IF (temp.CurrPos = temp.Capacity-1) & (temp.CurrPos+1 < MAX_LIST)  THEN

     temp.Resize();
      
  END; 

  IF temp.CurrPos < MAX_LIST -1  THEN
      temp.List[temp.CurrPos+1] := Item;
      INC(temp.CurrPos);
      INC(temp.Items);
  
  ELSE

      temp.Full:=TRUE;
  
  END;
   
END Add;

PROCEDURE (p:PSTRINGLIST)Item*(Pos:INTEGER):PCHAR;

VAR

  temp:PSTRINGLIST;

BEGIN

  temp:=p;
  
  IF (Pos <= temp.CurrPos) THEN 

    RETURN temp.List[Pos];
  
  END;
  
  RETURN NIL;

END Item;

PROCEDURE (p:PSTRINGLIST) Remove*(Pos:INTEGER);

VAR
   i:INTEGER;
   temp:PSTRINGLIST;

BEGIN
  temp:=p;
  
  IF temp.List[Pos] # NIL THEN

    DISPOSE(SYSTEM.VAL(PCHAR,temp.List[Pos]));

        FOR i:=Pos TO temp.CurrPos DO

          temp.List[i]:=temp.List[i+1];
        
        END;
    
    temp.List[i+1]:=NIL;
    DEC(temp.CurrPos);
    DEC(temp.Items);
  
  END;
  
END Remove;

PROCEDURE (p:PSTRINGLIST) Clear* ();

VAR

i:INTEGER;
temp:PSTRINGLIST; 

BEGIN

   temp:=p;
   
   FOR i:=0 TO temp.CurrPos   DO
     
     IF SYSTEM.VAL(PCHAR,temp.List[i]) # NIL THEN

       DISPOSE(SYSTEM.VAL(PCHAR,temp.List[i]));
      temp.List[i]:=NIL;
     
     END; 
   
   END;
   DISPOSE(temp.List);
   
END Clear;

PROCEDURE (p:PSTRINGLIST) Sort *(proc:SORTCALLBACK); 

VAR

  temp:PSTRINGLIST; 
  ptr,tempPtr: PCHAR;
  
PROCEDURE fn(item:PSTRINGLIST;left,right:INTEGER);

VAR

  i,j:INTEGER;

BEGIN

   i:=left;j:=right;
   ptr:=item.List[(left+right) DIV 2];
   
   WHILE i <= j DO

        WHILE ( proc(item.List[i],ptr ) < 0 ) & ( i < right ) DO INC(i); END;
        WHILE ( proc(item.List[j],ptr ) > 0 ) & (j > left ) DO DEC(j); END;

            IF i <= j THEN
              tempPtr:=item.List[i];
              item.List[i]:=item.List[j];
              item.List[j]:=tempPtr;
              INC(i);
              DEC(j);
        END;
   END; 
   
   IF (left < j )  THEN   fn(item,left,j) END;
   
   IF (i < right)  THEN   fn(item,i,right) END;

END fn;

BEGIN
  
    fn(p,0,p.CurrPos);
    p.Sorted:=TRUE;
END Sort;



PROCEDURE (p:PSTRINGLIST) Find *(SearchItem:ARRAY OF CHAR;VAR Pos:INTEGER):BOOLEAN; 
VAR
  temp:PSTRINGLIST;
  
(* BINARY SEARCH FOR SORTED LIST *)
PROCEDURE bn(p:PSTRINGLIST;Item:ARRAY OF CHAR;VAR Pos:INTEGER):BOOLEAN; 

VAR

  high,low,mid:INTEGER;
  done:BOOLEAN;

BEGIN

     low:=0; high:=p.Items -1;
     done:=FALSE;
     WHILE low <= high DO
         
         mid:=(low+high) DIV 2;

             IF WinBase.lstrcmpiA(SYSTEM.ADR(p.List[mid]^),SYSTEM.ADR(Item)) < 0 THEN high:=mid -1;
               ELSIF WinBase.lstrcmpiA(SYSTEM.ADR(p.List[mid]^),SYSTEM.ADR(Item)) > 0 THEN low :=mid +1;
             ELSE 
                 Pos:=mid;
                 done:= TRUE;
             END;
           
     END;
     RETURN done;
END bn;

(* SEQUENTIAL SEARCH UNSORTED SEARCH *)
PROCEDURE sn(p:PSTRINGLIST;Item:ARRAY OF CHAR;VAR Pos:INTEGER):BOOLEAN; 
VAR
  
 i:INTEGER;
 done:BOOLEAN; 

BEGIN

  done:=FALSE;
  FOR i:=0 TO p.CurrPos DO

      IF (WinBase.lstrcmpiA(SYSTEM.ADR(p.List[i]^),SYSTEM.ADR(Item)))=0 THEN
          Pos:=i;
          i:=p.CurrPos; (* CORRECT TERMINATION OF LOOP *)
          done := TRUE;
      END;

  END;
  
  RETURN done;

END sn;

BEGIN  (* Find*)

  temp := p;
  
  IF temp.Sorted THEN 

   RETURN bn(temp,SearchItem,Pos);
  
  ELSE

   RETURN sn(temp,SearchItem,Pos); 
  
  END;

END Find;
  
END TStringList.
