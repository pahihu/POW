(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Module                                                        *)
(*                                                                           *)
(* MODULE:     *)MODULE TList;(*                           V 1.0             *)
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

IMPORT SYSTEM;
  
CONST

  MAX_LIST = MAX(INTEGER);

TYPE
  
  SORTCALLBACK = PROCEDURE(item1,item2:SYSTEM.PTR):INTEGER;
  PTR_ARRAY = POINTER TO ARRAY OF SYSTEM.PTR;
  PCHAR = POINTER TO ARRAY OF CHAR;  
    
  PLIST* = POINTER TO TLIST;
    
  TLIST = RECORD 
     List         : PTR_ARRAY;(* THE LIST *)
     Capacity*    : INTEGER;   (* SIZE OF THE LIST *)
     CurrPos      : INTEGER;   (* CURRENT POSITION OF LAST USED LIST INDEX *)
     Items-       : INTEGER;   (* HOW MANY ITEMS IN LIST *)
     Full-        : BOOLEAN;   (* LIST IS FULL *)
     Sorted-      : BOOLEAN;   (* IS LIST SORTED OR UNSORTED *)
  END;
  
  
PROCEDURE (p:PLIST) Init*(); 

VAR

  i:INTEGER;
  temp:PLIST;

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

PROCEDURE (p:PLIST) Resize*();

VAR

  ptrtemp:PTR_ARRAY;
  temp:PLIST;
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



PROCEDURE (p:PLIST) Add*(VAR Item:SYSTEM.PTR);

VAR

temp:PLIST;  

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

PROCEDURE (p:PLIST)Item*(Pos:INTEGER):SYSTEM.PTR;

VAR

  temp:PLIST;

BEGIN

  temp:=p;
  
  IF (Pos <= temp.CurrPos) THEN 

    RETURN temp.List[Pos];
  
  END;
  
  RETURN NIL;

END Item;

PROCEDURE (p:PLIST) Remove*(Pos:INTEGER);

VAR
   i:INTEGER;
   temp:PLIST;

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

PROCEDURE (p:PLIST) Clear* ();

VAR

i:INTEGER;
temp:PLIST; 

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

PROCEDURE (p:PLIST) Sort *(proc:SORTCALLBACK); 

VAR

  temp:PLIST; 
  ptr,tempPtr: SYSTEM.PTR;
  
PROCEDURE fn(item:PLIST;left,right:INTEGER);

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

BEGIN
  
END TList.
