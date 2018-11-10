(*****************************************************************************)
(*                                                                           *)
(* PROJECT:    Module                                                        *)
(*                                                                           *)
(* MODULE:     *)MODULE List;(*                            V 1.0             *)
(*                                                         2001JUN16         *)
(* PURPOSE:  linked list                                                     *)
(*                                                                           *)
(* PROPETIES                                                                 *)
(*                                                                           *)
(* FUNCTIONS:                                                                *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(* AUTHORS:    Gene                                                          *) 
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



(*This implementation strongly relies on _garbage collection_ *)

IMPORT User;

TYPE
  DATAPTR = POINTER TO DATA;
  
  DATA = RECORD
               next,
               prev : DATAPTR;
               user : User.DATAPTR;
             END;
             
  LIST*    = RECORD (User.DATA)
               first,
               curr: DATAPTR;
             END;
(*****************************************)
PROCEDURE (VAR l:LIST) InitList*();
BEGIN
  (* Procedure creates a new List handle for user. *)
    l.first:= NIL;
    l.curr := NIL;
END InitList;
(*****************************************)
PROCEDURE (VAR l:LIST) DeleteList*();
BEGIN
  (* Procedure deletes List.
     Relying on garbage collection, procedure does not
     actually deallocate nor its own, nor user data. *)
    l.InitList();
END DeleteList;
(*****************************************)
(* The following procedures add          *)
(* user allocated data to the List object*)
(*****************************************)
PROCEDURE (VAR l:LIST) AddPrev*(user_data_ptr : User.DATAPTR);
  VAR
    d : POINTER TO DATA;
BEGIN
  (* Adds a new List element in front of the current member.
     The new element becomes current one *)
  NEW(d);
  IF d#NIL THEN
     d^.user  := user_data_ptr;
     IF l.curr = NIL THEN
        l.first  := d;
        d^.prev  := NIL;
        d^.next  := NIL;
     ELSE
        d^.prev := l.curr^.prev;
        d^.next := l.curr;
        IF l.curr^.prev=NIL THEN
           l.first := d;
        ELSE
           IF    l.curr^.prev^.next=l.curr THEN
                 l.curr^.prev^.next := d;
           ELSE
                 HALT(0);
           END;
        END;
        l.curr^.prev := d;
     END;
     l.curr   := d;
  END;
END AddPrev;
(*****************************************)
PROCEDURE (VAR l:LIST) AddNext*(user_data_ptr : User.DATAPTR);
  VAR
    d : POINTER TO DATA;
BEGIN
  (* Adds a new List member behind the current,
     the new member becomes current one *)
  NEW(d);
  IF d#NIL THEN
     d^.user  := user_data_ptr;
     IF l.curr = NIL THEN
        l.first := d;
        d^.prev  := NIL;
        d^.next  := NIL;
     ELSE
        d^.next := l.curr^.next;
        d^.prev := l.curr;
        IF l.curr^.next#NIL THEN
           l.curr^.next^.prev := d;
        END;
        l.curr^.next := d;
     END;
     l.curr  := d;
  END;
END AddNext;
(*****************************************)
PROCEDURE (VAR l:LIST) AlterData*(user_data_ptr : User.DATAPTR);
BEGIN
  (* Alters USER data assotiated with the current List element *)
  IF l.curr#NIL THEN
    l.curr^.user:=user_data_ptr;
  END;
END AlterData;
(*****************************************)
(* The following procedures exclude      *)
(* elements from the List                *)
(*****************************************)
PROCEDURE (VAR l:LIST) DelPrev*();
BEGIN
  (* Deletes current List member. 
     Relying on garbage collection, procedure adjusts links rather than
     actually deallocates anything. The previous member becomes the current *)
  IF l.curr#NIL THEN    
     (* adjust links *)
     IF l.curr^.prev # NIL THEN
        l.curr^.prev^.next :=l.curr^.next;
     END;
     IF l.curr^.next # NIL THEN
        l.curr^.next^.prev :=l.curr^.prev;
     END;
     (* set current *)
     IF (l.curr^.prev # NIL) THEN
       l.curr := l.curr^.prev;
     ELSE
       IF l.curr^.next # NIL THEN
          l.curr := l.curr^.next;
       ELSE
          l.curr := NIL;
          l.first:= NIL;
       END;
     END;
  END;
END DelPrev;
(*****************************************)
PROCEDURE (VAR l:LIST) DelNext*();
BEGIN
  (* Deletes current List member. 
     Relying on garbage collection, procedure adjusts links rather than
     actually deallocates anything. The next member becomes the current *)
  IF l.curr#NIL THEN    
     (* adjust links *)
     IF l.curr^.prev # NIL THEN
        l.curr^.prev^.next :=l.curr^.next;
     END;
     IF l.curr^.next # NIL THEN
        l.curr^.next^.prev :=l.curr^.prev;
     END;
     (* set current *)
     IF (l.curr^.next # NIL) THEN
       l.curr := l.curr^.next;
     ELSE
       IF l.curr^.prev # NIL THEN
          l.curr := l.curr^.prev;
       ELSE
          l.curr := NIL;
          l.first:= NIL;
       END;
     END;
  END;
END DelNext;
(*****************************************)
(* The following procedures navigate the *)
(* List and return ptr to user data      *)
(*****************************************)
PROCEDURE (VAR l:LIST) GetFirst*() : User.DATAPTR;
BEGIN
  (* Returns POINTER TO USER data assotiated with List first element and
     makes it current List element *)
  IF l.first#NIL THEN
    l.curr := l.first;
    RETURN l.first^.user;
  END;
  RETURN NIL;
END GetFirst;
(*****************************************)
PROCEDURE (VAR l:LIST) GetCurrent*() : User.DATAPTR;
BEGIN
  (* Returns POINTER TO USER data assotiated with the current List member *)
  IF l.curr#NIL THEN
    RETURN l.curr^.user;
  END;
  RETURN NIL;
END GetCurrent;
(*****************************************)
PROCEDURE (VAR l:LIST) GetNext*() : User.DATAPTR;
BEGIN
  (* Returns POINTER TO USER data assotiated with the next List member
     and makes it the current *)
  IF (l.curr#NIL) THEN
    IF l.curr^.next#NIL THEN
      l.curr := l.curr^.next;
      RETURN l.curr^.user;
    END;
  END;
  RETURN NIL;
END GetNext;
(*****************************************)
PROCEDURE (VAR l:LIST) GetPrev*() : User.DATAPTR;
BEGIN
  (* Returns POINTER TO USER data assotiated with the prev List member
     and makes it the currenl. *)
  IF l.curr#NIL THEN
    IF l.curr^.prev#NIL THEN
      l.curr := l.curr^.prev;
      RETURN l.curr^.user;
    END;
  END;
  RETURN NIL;
END GetPrev;
(*****************************************)
(* The following procedures navigate the *)
(* List and return nothing               *)
(*****************************************)
PROCEDURE (VAR l:LIST) First*();
BEGIN
  (* Makes List first element the current List element *)
  IF l.first#NIL THEN
    l.curr := l.first;
  END;
END First;
(*****************************************)
PROCEDURE (VAR l:LIST) Next*();
BEGIN
  (* Makes the next List element the current one *)
  IF (l.curr#NIL) THEN
    IF l.curr^.next#NIL THEN
      l.curr := l.curr^.next;
    END;
  END;
END Next;
(*****************************************)
PROCEDURE (VAR l:LIST) Prev*();
BEGIN
  (* Makes the previous List element the current one. *)
  IF l.curr#NIL THEN
    IF l.curr^.prev#NIL THEN
      l.curr := l.curr^.prev;
    END;
  END;
END Prev;
(*****************************************)
(* The following procedures supply info  *)
(* necessary to navigate List            *)
(*****************************************)
PROCEDURE (VAR l:LIST) NoPrev*():BOOLEAN;
BEGIN
  (* Returns TRUE if the current List member is the first*)
  IF l.curr#NIL THEN
    RETURN l.curr^.prev=NIL;
  ELSE
    RETURN TRUE;
  END;
END NoPrev;
(*****************************************)
PROCEDURE (VAR l:LIST) NoNext*():BOOLEAN;
BEGIN
  (* Returns TRUE if the current List member is the last*)
  IF l.curr#NIL THEN
    RETURN l.curr^.next=NIL;
  ELSE
    RETURN TRUE;
  END;
END NoNext;
(*****************************************)
PROCEDURE (VAR l:LIST) IsEmpty*():BOOLEAN;
BEGIN
  (* Returns TRUE if the current List has no members*)
  RETURN l.curr=NIL;
END IsEmpty;
(*****************************************)
END List.

 

 

