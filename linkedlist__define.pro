;+
; NAME:
;   LINKEDLIST
;
; FILENAME:
;   linkedlist__define.pro
;
; PURPOSE:
; 
;   The purpose of this program is to implement a list that
;   is linked in both the forward and backward directions. There
;   is no restriction as to what can be stored in a linked list
;   node. The linked list is implemented as an object.
;
; AUTHOR:
; 
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
; 
;   General programming.
;
; CALLING SEQUENCE:
; 
;   mylist = Obj_New('LINKEDLIST', item)
;
; OPTIONAL INPUTS:
; 
;   item: The first item added to the list. Items can be any
;     valid IDL variable type.
;
; COMMON BLOCKS:
; 
;   Are you kidding?!
;
; RESTRICTIONS:
; 
;   Be sure to destroy the LINKEDLIST object when you are finished
;   with it: Obj_Destroy, mylist
;
;   Node index numbers start at 0 and go to n-1, where n is the
;   number of items in the list.
;
; PUBLIC METHODS:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRO LINKEDLIST::ADD, item, index, $
;     AFTER=after, $
;     BEFORE=before, $
;     ERROR=error, $
;     NO_COPY=no_copy, $
;     REPLACE=replace
;     
;
;   The ADD method adds a data item to the list.
;
;   Parameters:
;
;   item: The data item to be added to the list. Required.
;
;   index: The location in the list where the data item is
;     to be added. If neither the AFTER or BEFORE keyword is
;     set, the item is added AFTER the item at the index location.
;     If index is missing, the index points to the last item in
;     the list. Optional.
;
;   Keywords:
;
;   AFTER: If this keyword is set, the item is added after the
;     item at the current index.
;
;   BEFORE: If this keyword is set, the item is added before the
;     item at the current index.
;     
;   ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;      
;   NO_COPY: If set, the item is transferred to the internal pointer using
;      a no copy method. This will cause the item variable to become undefined.
;      
;   REPLACE: If this keyword is set, the item will replace the current item at
;      the index location.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRO LINKEDLIST::DELETE, index, ALL=all, DESTROY=destroy, ERROR=error   
;      
;   The DELETE method deletes an item from the list.
;
;   Parameters:
;
;   index: The location in the list where the data item is
;     to be delete. If index is missing, the index points to
;     the last item in the list. Optional.
;
;   Keywords:
;
;   ALL: If this keyword is set, all items in the list are deleted.
;
;   DESTROY: If the item at the node is an object or pointer, the
;     item will be destroyed before the node is deleted. This keyword
;     is turned on (set to 1) by default. Set to 0 to prevent destruction.
;
;    ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FUNCTION LINKEDLIST::GET_COUNT
;
;   The GET_COUNT method returns the number of items in the list.
;
;   Return Value: The number of items stored in the linked list.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; FUNCTION LINKEDLIST::GET_ITEM, index, $
;    ALL=all, $                 ; This ASSUMES all items stored are the same type!!!
;    Dereference=dereference, $ ; Obsolete. Ignored. Always returns item.
;    ItemPtr=itemPtr, $         ; The pointer to the item, if needed. Output.
;    NO_COPY=no_copy, $         ; Copy from location with NO_COPY.
;    ERROR=errorMsg             ; The error message. Null string if no error.

;
;   Parameters:
;
;   index: The location in the list from which the data item is
;     to be retrieved. If not present, the last item in the list
;     is retrieved. Optional.
;
;   Keywords:
;
;   DEREFERENCE: This keyword obsolete and only provided for backward compatibility.
;
;   ALL: Set this keyword to return an n-element array containing all the list
;      items.  This requires that all list items be of the same type, and
;      if they are arrays, they have 7 dimensions or fewer. If index is passed, 
;      it is ignored.
;     
;   ITEMPTR: The pointer to the data item.
;   
;   NO_COPY: If this keyword is set, the item is transferred from the data
;      pointer using a NO_COPY method. This will undefine the item at that
;      indexed locaton.
;      
;    ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;
;   Return Value: The data item at this index on the list.
;     If ALL is set, then an array containing all the data items is returned.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FUNCTION LINKEDLIST::GET_NODE, index, ERROR=error
;
;   The GET_NODE method returns a pointer to the specified node
;   from the list.
;
;   Parameters:
;
;   index: The location in the list from which the data node is
;     to be retrieved. If not present, the last node in the list
;     is retrieved. The node is a structure with three fields:
;     Previous is a pointer to the previous node in the list.
;     Next is a pointer to the next node in the list. A null pointer
;     in the previous field indicates the first node on the list. A
;     null pointer in the next field indicates the last node on the
;     list. The item field is a pointer to the item stored in the
;     node. Optional.
;
;   ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;      
;   Return Value: A pointer to the specified node structure in
;     the linked list.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRO LINKEDLIST::HELP, PRINT=print
;
; The HELP method performs a HELP command on each item
; in the linked list.
;
;   Keywords:
;
;    PRINT: If this keyword is set, the PRINT command is used
;      instead of the HELP command on the items in the list.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRO LINKEDLIST::MOVE_NODE, nodeIndex, location, BEFORE=before, ERROR=error
;
;   The MOVE_NODE method moves a list node from one location to another.
;
;   Parameters:
;
;   nodeIndex: The location in the list of the node you are moving.
;     Required.
;
;   location: The location (index) you are moving the node to. If
;     location is missing, the location points to the node at the
;     end of the list.
;
;   Keywords:
;
;    BEFORE: If this keyword is set, the node is added to the
;      list before the location node. Otherwise, it is added after
;      the location node.
;
;    ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRO LINKEDLIST::REPLACE_ITEM, newItem, index, ERROR=error
;
;  Use this method to replace any item in the list with any other value.
;  This allows the caller to change an item without stepping through the
;  process of deleting an item then adding a new one.
;
;  Parameters:
;     index:  The location of the node you are replacing
;
;     newItem:  Any value of any data type.
;
;    ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTION LINKEDLIST::HAVE_ITEM, index, ERROR=error
;
;  Use this method to check to see if an item exits at a particular location
;  on the list. Returns a 1 if the item is there, otherwise a 0.
;
;  Parameters:
;     index:  The location of the node you are replacing
;      
;    ERROR: On return, if this is not a null string, an error occurred
;      and this value is set equal to the error message.
;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; EXAMPLE:
;
;   mylist = Obj_New("LINKEDLIST", 5)
;   mylist->Add, 10
;   mylist->Add, 7, 1, /Before
;   mylist->Add, 12
;   print, mylist->Get_Item(/All)
;   mylist->Add, 'Bob', 2, /Replace
;   mylist->Help
;   mylist->Delete, 0
;   mylist->Help, /Print
;
; MODIFICATION HISTORY:
;   Written by: David Fanning, 25 August 98.
;   25 August 99. Fixed several errors in various methods dealing with
;       moving nodes from one place to another. DWF.
;   13 June 2001. DWF. Added DEREFERENCE to the GET_ITEM method to
;       return the item itself, instead of the pointer to the item.
;   27 June 2001 Added REPLACE_ITEM method.  Ben Tupper.
;   7 April 2003. Added DESTROY keyword to DELETE method so that objects
;      and pointers could be cleaned up properly when they are deleted
;      from the linked list. DWF.
;   9 April 2003. Fixed a problem that occurs when deleting the last node. DWF.
;   3 Feb 2004. Make sure loop index vars are long.  Jeff Guerber
;   30 Jun 2004.  Added /ALL to GET_ITEM function.  Henry Throop, SWRI.
;   23 Nov 2004.  Fixed GET_ITEM, /ALL to accomodate structures and empty
;      lists.  Henry Throop.
;   21 February 2011. A complete refurbishing to incorporate changes and to fix bugs
;      I found in the SolarSoft version of this code. I've tried to make this compatible
;      with the version distributed with SolarSoft to reduce problems caused by two versions
;      of the software with the same name.
;    9 December 2011. Fixed a problem with the ALL keyword on the Get_Item method. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO LINKEDLIST::ADD, item, index, $
    After=after, $
    Before=before, $
    Error=errorMsg, $
    No_Copy=no_copy, $
    Replace=replace


    ; This method is the public interface to the private ADD_+ methods.
    ; If INDEX is not specified, the item is always added to the end
    ; of the list. If INDEX is specified, but neither the BEFORE or
    ; AFTER keywords are used, the item is added AFTER the INDEX specified.
    
    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Must supply an item to add to the list.
    IF N_Elements(item) EQ 0 THEN BEGIN
       errorMsg = 'Must supply an item to add to the list.'
       Message, errorMsg, /Continue
       RETURN
    END
    
    ; Check for index. If there is none, add to end of list.
    IF N_Elements(index) EQ 0 THEN BEGIN
       self->Add_To_End, item, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; If this is the last index, add it to the end of list.
    IF index GT (self.count-1) THEN BEGIN
       self->Add_To_End, item, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; Are we replacing an item?
    IF Keyword_Set(replace) THEN BEGIN
        self -> Replace_Item, item, index, NO_COPY=no_copy, ERROR=errorMsg
        RETURN
    ENDIF
    
    ; BEFORE keyword set.
    IF Keyword_Set(before) THEN BEGIN
       self->Add_Before, item, index, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; AFTER keyword set.
    IF Keyword_Set(after) THEN BEGIN
       self->Add_After, item, index, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; No BEFORE or AFTER keywords. Add to location AFTER index.
    self->Add_After, item, index, NO_COPY=no_copy, ERROR=errorMsg
    
END
;------------------------------------------------------------------------



PRO LINKEDLIST::ADD_AFTER, item, index, NO_COPY=no_copy, ERROR=errorMsg

    ; This method adds an item node AFTER the item specified by
    ; the index number. This is a private method. Use the public
    ; ADD method to add an item to the list.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Must supply an item to add to the list.
    IF N_Elements(item) EQ 0 THEN BEGIN
       errorMsg = 'Must supply an item to add to the list.'
       Message, errorMsg, /Continue
       RETURN
    END
    
    ; Check for index. If there is none, add to end of list.
    IF N_Elements(index) EQ 0 THEN BEGIN
       self->Add_To_End, item, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; If this is the last index, add it to the end of list.
    IF index GE (self.count-1) THEN BEGIN
       self->Add_To_End, item, NO_COPY=no_copy, ERROR=errorMsg
       RETURN
    ENDIF

    ; Create a new node and store the item in it.
    currentNode = Ptr_New( {LINKEDLIST_NODE} )
    (*currentNode).item = Ptr_New(item, NO_COPY=no_copy)
    self.count = self.count + 1
    
    ; Get the node currently located at the index.
    indexNode = self->Get_Node(index)
    
    ; Get the node that follows the indexNode.
    nextNode = (*indexNode).next
    
    ; Update pointers.
    (*indexNode).next = currentNode
    (*currentNode).previous = indexNode
    (*nextNode).previous = currentNode
    (*currentNode).next = nextNode

END
;------------------------------------------------------------------------



PRO LINKEDLIST::ADD_BEFORE, item, index, NO_COPY=no_copy, ERROR=errorMsg

    ; This method adds an item node BEFORE the item specified by
    ; the index number. This is a private method. Use the public
    ; ADD method to add an item to the list.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Must supply an item to add to the list.
    IF N_Elements(item) EQ 0 THEN BEGIN
       errorMsg = 'Must supply an item to add to the list.'
       Message, errorMsg, /Continue
       RETURN
    END
    
    ; Check for index. If there is none, add to head of list.
    IF N_Elements(index) EQ 0 THEN index = 0

    ; Create a new node and store the item in it.
    currentNode = Ptr_New( {LINKEDLIST_NODE} )
    (*currentNode).item = Ptr_New(item, NO_COPY=no_copy)
    self.count = self.count + 1
    
    ; Get the node currently located at the index.
    indexNode = self->Get_Node(index)
    
    ; Get the node that is before the indexNode.
    previousNode = (*indexNode).previous
    
    ; Update pointers.
    (*indexNode).previous = currentNode
    (*currentNode).previous = previousNode
    (*currentNode).next = indexNode
    IF Ptr_Valid(previousNode) THEN $
       (*previousNode).next = currentNode ELSE $
       self.head = currentNode

END
;------------------------------------------------------------------------



PRO LINKEDLIST::ADD_TO_END, item, NO_COPY=no_copy, ERROR=errorMsg

    ; This method adds an item to the tail of the list. This is 
    ; a private method. Use the public ADD method to add an 
    ; item to the list.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Must supply an item to add to the list.
    IF N_Elements(item) EQ 0 THEN BEGIN
       errorMsg = 'Must supply an item to add to the list.'
       Message, errorMsg, /Continue
       RETURN
    END
    
    ; Is this the first item we are adding to the list?
    IF self.count EQ 0 THEN BEGIN
    
        ; Create a new node.
        currentNode = Ptr_New({ LINKEDLIST_NODE })
    
        ; Add the item to the node.
        (*currentNode).item = Ptr_New(item, NO_COPY=no_copy)
    
         ; The head and tail point to current node.
        self.head = currentNode
        self.tail = currentNode
     
        ; Update the node count.
        self.count = self.count + 1
     
    ENDIF ELSE BEGIN ; Not first, but must add a node.
     
        ; Create a new node.
        currentNode = Ptr_New({ LINKEDLIST_NODE })
    
        ; Set the next field of the previous node.
        (*self.tail).next = currentNode
     
        ; Add the item to the current node.
        (*currentNode).item = Ptr_New(item, NO_COPY=no_copy)
    
        ; Set the previous field to point to previous node.
        (*currentNode).previous = self.tail
    
        ; Update the tail field to point to current node.
        self.tail = currentNode
    
        ; Update the node count.
        self.count = self.count + 1
        
    ENDELSE
    
END
;------------------------------------------------------------------------



PRO LINKEDLIST::DELETE, index, All=all, DESTROY=destroy, ERROR=errorMsg

    ; This method is the public interface to the private DELETE_+ methods.
    ; If INDEX is not specified, the last item on the list is always deleted.
    ; The ALL keyword will delete all the items on the list.
    ; The DESTROY keyword will destroy any object or pointer item stored in
    ; the node before the node is deleted.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Assume you want to destroy the items in the node.
    IF (N_Elements(destroy) EQ 0) $
        THEN destroy = 1 $
        ELSE destroy = Keyword_Set(destroy)
    
    ; Delete all the nodes?
    IF Keyword_Set(all) THEN BEGIN
       self->Delete_Nodes, DESTROY=destroy, ERROR=errorMsg
       RETURN
    ENDIF

    ; Check for index. If there is none, delete last node on list.
    IF N_Elements(index) EQ 0 THEN BEGIN
       self->Delete_Last_Node, DESTROY=destroy, ERROR=errorMsg
       RETURN
    ENDIF
    
    ; Delete specified node.
    IF index EQ (self.count - 1) THEN $
       self->Delete_Last_Node, DESTROY=destroy, ERROR=errorMsg ELSE $
       self->Delete_Node, index, DESTROY=destroy, ERROR=errorMsg
       
END
;------------------------------------------------------------------------



PRO LINKEDLIST::DELETE_LAST_NODE, DESTROY=destroy, ERROR=errorMsg

    ; This method deletes the last node in the list. This is a 
    ; private method. Use the public DELETE method to delete an 
    ; item from the list.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Assume you want to destroy the items in the node.
    IF (N_Elements(destroy) EQ 0) $
        THEN destroy = 1 $
        ELSE destroy = Keyword_Set(destroy)
    
    ; If there is nothing here, return.
    IF self.count EQ 0 THEN RETURN
    
    ; This should be the last node.
    currentNode = self.tail
    
    ; Free the item pointer and everything it points to.
    IF Keyword_Set(destroy) THEN BEGIN
        IF Ptr_Valid((*currentNode).item) THEN Heap_Free, (*currentNode).item
    ENDIF    
    
    ; Is this the last node in the list?
    IF NOT Ptr_Valid((*currentNode).previous) THEN BEGIN
        self.head = Ptr_New()
        self.tail = Ptr_New()
        self.count = 0
        Ptr_Free, (*currentNode).next
    ENDIF ELSE BEGIN
        previousNode = (*currentNode).previous
        self.tail = previousNode
        Ptr_Free, (*self.tail).next
        (*self.tail).next = Ptr_New()
        self.count = self.count - 1
    ENDELSE
    
    ; Release the currentNode pointer.
    Ptr_Free, currentNode
END
;------------------------------------------------------------------------



PRO LINKEDLIST::DELETE_NODE, index, DESTROY=destroy, ERROR=errorMsg

    ; This method deletes the indicated node from the list. This is a 
    ; private method. Use the public DELETE method to delete an 
    ; item from the list.
    
    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Assume you want to destroy the items in the node.
    IF (N_Elements(destroy) EQ 0) $
        THEN destroy = 1 $
        ELSE destroy = Keyword_Set(destroy)
    
    IF self.count EQ 0 THEN BEGIN
       errorMsg = 'No nodes to delete.'
       Message, errorMsg, /Continue
       RETURN
    ENDIF
    
    IF index GT (self.count - 1) THEN BEGIN
       errorMsg = 'No node with the requested index number.'
       Message, errorMsg, /Continue
       RETURN
    ENDIF
      
    ; Get the current node and free the item pointer.
    currentNode = self->Get_Node(index)
    IF ~Ptr_Valid(currentNode) THEN BEGIN
        errorMsg = 'The current node is not a valid pointer.'
        Message, errorMsg, /Continue
        RETURN
    ENDIF
    
    ; Free the item pointer and everything it points to.
    IF Keyword_Set(destroy) THEN $
    BEGIN
      theItem = *(*currentNode).item
      CASE Size(theItem, /TNAME) OF
         'OBJREF': Obj_Destroy, theItem
         'POINTER': Ptr_Free, theItem
         ELSE:
      ENDCASE
    ENDIF
    Ptr_Free, (*currentNode).item
    
    ; Is this the last node?
    IF index EQ (self.count - 1) THEN BEGIN
        self->Delete_Last_Node, DESTROY=destroy, ERROR=errorMsg
        RETURN
    ENDIF
    
    ; Is this the first node in the list?
    IF NOT Ptr_Valid((*currentNode).previous) THEN BEGIN
            nextNode = (*currentNode).next
            Ptr_Free, (*nextNode).previous
            (*nextNode).previous = Ptr_New()
            self.head = nextNode
    ENDIF ELSE BEGIN
            previousNode = (*currentNode).previous
            nextNode = (*currentNode).next
            (*nextNode).previous = previousNode
            (*previousNode).next = nextNode
    ENDELSE

    ; Release the currentNode pointer. 
    Ptr_Free, currentNode
    self.count = self.count - 1
END
;------------------------------------------------------------------------




PRO LINKEDLIST::DELETE_NODES, DESTROY=destroy, ERROR=errorMsg

    ; This method deletes all of the nodes. This is a private method. 
    ; Use the public DELETE method to delete all items from the list.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Assume you want to destroy the items in the node.
    IF (N_Elements(destroy) EQ 0) $
        THEN destroy = 1 $
        ELSE destroy = Keyword_Set(destroy)
    
    ; Cycle through the list, deleting as you go.
    WHILE Ptr_Valid(self.head) DO BEGIN
        currentNode = *self.head
        IF Keyword_Set(destroy) THEN $
        BEGIN
          theItem = *(currentNode.item)
          CASE Size(theItem, /TNAME) OF
             'OBJREF': Obj_Destroy, theItem
             'POINTER': Ptr_Free, theItem
             ELSE:
          ENDCASE
        ENDIF
        Ptr_Free, currentNode.previous
        Ptr_Free, currentNode.item
        self.head = currentNode.next
    ENDWHILE

    ; Free up that last pointer.
    Ptr_Free, self.tail

    ; Update the count.
    self.count = 0

END
;------------------------------------------------------------------------



FUNCTION LINKEDLIST::GET_COUNT

    ; This method returns the number of items in the list.
    RETURN, self.count
END
;------------------------------------------------------------------------



FUNCTION LINKEDLIST::GET_ITEM, index, $
    ALL=all, $         ; This ASSUMES all items stored are the same type!!!
    Dereference=dereference, $ ; Obsolete. Ignored. Always returns item.
    ItemPtr=itemPtr, $ ; The pointer to the item, if needed. Output.
    NO_COPY=no_copy, $
    ERROR=errorMsg

    ; This method returns the item stored at the index location.
    ; The DEREFERENCE keyword is obsolete and is provide only
    ; to allow backward compatibility. It is always set to 1.
    ; If you wish to obtain the pointer to the item, use the
    ; ItemPtr keyword. If an INDEX is not provided, the
    ; last item is returned.
    
    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN, Ptr_New()
    ENDIF

    ; Assume no error.
    errorMsg = ""
    dereference = 1
    
    ; Last item, if index is unspecified.
    IF N_Elements(index) EQ 0 THEN index = self.count - 1
    
    ; Make sure there are items in the list.
    IF self.count EQ 0 THEN BEGIN
       errorMsg = 'Nothing is currently stored in the list.'
       Message, errorMsg, /CONTINUE
       RETURN, Ptr_New()
    ENDIF
    
    ; The index has to be in range.
    IF index GT (self.count-1) OR index LT 0 THEN BEGIN
       errorMsg = 'Requested node index is out of range.'
       Message, errorMsg, /CONTINUE
       RETURN, Ptr_New()
    ENDIF
    
    ; Start at the head of the list.
    currentNode = self.head
    
    ; Is this a valid pointer?
    IF ~Ptr_Valid(currentNode) THEN BEGIN
        errorMsg = 'The current node pointer is invalid.'
        Message, errorMsg, /CONTINUE
        RETURN, Ptr_New()
    ENDIF
    
    ; Is the ALL keyword set. ; We are going *assume* the items are all the same type.
    IF Keyword_Set(ALL) THEN BEGIN 
    
        item  = self->Get_Item()
        num   = self.count
        
        ; If items in list are x * y arrays, then for n items, we return a n * x * y ... array
        ; Since IDL's maximum arrays are 8D, this means that this particular routine works only up to 7D arrays.
        ;
        ; There is an unadvertised limitation in IDL's MAKE_ARRAY() function, in that it does not allow
        ; the automatic creation of structures by passing TYPE=8.  Instead, we use the VALUE= keyword
        ; keyword to MAKE_ARRAY.  This takes a scalar and not a vector.
          dim_item      = Size(item, /N_DIMENSIONS) 
          IF (dim_item gt 0) THEN BEGIN
            size_item   = Size(item, /DIMENSIONS)
            type_item   = Size(item, /TYPE)
            arr         =  Make_Array(dimension=[num,size_item], val=item[0])
        
          ENDIF ELSE BEGIN
            type_item   = Size(item, /TYPE)
            arr         = Make_Array(dimension=[num], val=item[0])
          ENDELSE
        
          dim_item_save = dim_item
          type_item_save= type_item
        
          ; Rather than call Get_Item for each element, it is much faster to march through the list
          ; sequentially and extract every item as we get to it.
          ; After we extract each item, we check to make sure that its type and size are the same as for the
          ; first item; if they're not, we generate an error and return.  This is a bit conservative -- we
          ; could promote ints to floats, for instance -- but it's safe.
          currentNode = self.head
          FOR i = 0L, num-1 DO BEGIN
        
                item        =  *((*currentNode).item)
                dim_item    = Size(item, /N_DIMENSIONS)            ; number of dimensions
                type_item    = Size(item, /TYPE)
                IF ((type_item NE type_item_save) OR (dim_item NE dim_item_save)) THEN BEGIN
                    errorMsg = 'Inconsistent type or size for Get_Item(/ALL).  Use Get_Item(index) instead.'
                    Message, errorMsg, /CONTINUE
                    RETURN, Ptr_New()
                ENDIF
            
                ; How many dimensions are there?
                CASE dim_item OF
                  0 : arr[i]               = item
                  1 : arr[i,*]             = item
                  2 : arr[i,*,*]           = item
                  3 : arr[i,*,*,*]         = item
                  4 : arr[i,*,*,*,*]       = item
                  5 : arr[i,*,*,*,*,*]     = item
                  6 : arr[i,*,*,*,*,*,*]   = item
                  7 : arr[i,*,*,*,*,*,*,*] = item
                  ELSE : BEGIN
                    errorMsg = 'Maximum array size for Get_Item(/ALL) exceeded.  Use Get_Item(index) instead.'
                    Message, errorMgs, /CONTINUE
                    RETURN, Ptr_New()
                  END
                ENDCASE
                currentNode = (*currentNode).next
          ENDFOR
        
          RETURN, arr
    
    ENDIF ; if keyword ALL is set.

    ; Find the item asked for by traversing the list.
    FOR j=0L, index-1 DO currentNode = (*currentNode).next
    
    ; Get the item pointer.
    itemPtr = (*currentNode).item
    
    ; Return the item.
    IF Ptr_Valid(itemPtr) THEN BEGIN
        RETURN, *itemPtr 
    ENDIF ELSE BEGIN
        errorMsg = 'The item pointer is invalid.'
        Message, errorMsg, /CONTINUE
        RETURN, Ptr_New()
   ENDELSE
    
END
;------------------------------------------------------------------------



FUNCTION LINKEDLIST::GET_NODE, index, ERROR=errorMsg

    ; This method returns a pointer to the asked-for node.
    ; Ask for the node by number or order in the list
    ; (node numbers start at 0).
    
    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, errorMsg, /Continue
       RETURN, Ptr_New()
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Gets last node by default.
    IF N_Elements(index) EQ 0 THEN index = self.count - 1
    
    ; Make sure there are items in the list.
    IF self.count EQ 0 THEN BEGIN
       errorMsg = 'Nothing is currently stored in the list.'
       Message, errorMsg, /Continue
       RETURN, Ptr_New()
    ENDIF
    
    IF index GT (self.count - 1) OR index LT 0 THEN BEGIN
       errorMsg = 'Sorry. Requested node is not in list.'
       Message, errorMsg, /Continue
       RETURN, Ptr_New()
    ENDIF  
    
    ; Start at the head of the list.
    currentNode = self.head
   
    ; Find the item asked for by traversing the list.
    FOR j=0L, index-1 DO currentNode = (*currentNode).next
    
    ; Return the pointer to the node.
    RETURN, currentNode
    
END
;------------------------------------------------------------------------



FUNCTION LINKEDLIST::GET_VALUE, index, NO_COPY=no_copy, ERROR=errorMsg

    ; This method returns the item stored at index in the list.
    ; It is a synonym for Get_Item. Added here to be compatible with
    ; SolarSoft changes to LinkImage.
    
    RETURN, self -> Get_Item(index, NO_COPY=no_copy, ERROR=errorMsg)

END
;------------------------------------------------------------------------


FUNCTION LINKEDLIST::HAVE_ITEM, index, ERROR=errorMsg

    ; This method returns a 1 if there is a valid item stored at the
    ; index location. Otherwise, it returns a 0.
    
    ; Try to get the item. See if you were successful.
    item = self -> Get_Item(ERROR=errorMsg, ITEMPTR=itemPtr)
    IF errorMsg NE "" THEN RETURN, 0
    IF ~Ptr_Valid(itemPtr) THEN RETURN, 0
    
    RETURN, 1
    
END
;------------------------------------------------------------------------


PRO LINKEDLIST::HELP, Print=print

    ; This method performs a HELP command on the items
    ; in the linked list. If the PRINT keyword is set, the
    ; data items are printed instead.
    
    ; Are there nodes to work with?
    IF NOT Ptr_Valid(self.head) THEN BEGIN
        ok = Widget_Message('No nodes in Linked List.')
        RETURN
    ENDIF
    
    ; First node.
    currentNode = *self.head
    IF Keyword_Set(print) THEN Print, *currentNode.item ELSE $
        Help, *currentNode.item
    
    ; The rest of the nodes. End of list indicated by null pointer.
    WHILE currentNode.next NE Ptr_New() DO BEGIN
        nextNode = *currentNode.next
        IF Keyword_Set(print) THEN Print, *nextNode.item ELSE $
            Help, *nextNode.item
        currentNode = nextNode
    ENDWHILE

END
;------------------------------------------------------------------------



PRO LINKEDLIST::MOVE_NODE, nodeIndex, location, Before=before, Error=errormsg

    ; This method moves the requested node to a new location.
    ; The node is added AFTER the target location, unless the BEFORE
    ; keyword is used.

    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, !Error_State.MSG, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; A node index is required.
    IF N_Elements(nodeIndex) EQ 0 THEN BEGIN
       errorMsg = 'A node index is required in MOVE_NODE method.'
       Message, errorMsg, /Continue
       RETURN
    ENDIF

    ; If location is not specified the node is moved to the
    ; end of the list.
    IF N_Elements(location) EQ 0 THEN BEGIN
       location = (self->Get_Count()) - 1
    ENDIF

    ; Add the node to the list.
    currentNode = self->Get_Node(nodeIndex)
    
    ; If the current node is not a valid pointer, there is problem.
    IF ~Ptr_Valid(currentNode) THEN BEGIN
        errorMsg = 'The requested node is not a valid pointer.'
        Message, errorMsg, /Continue
        RETURN
    ENDIF

    IF Keyword_Set(before) THEN BEGIN
       self->Add, *(*currentNode).item, location, /Before
    ENDIF ELSE BEGIN
       self->Add, *(*currentNode).item, location, /After
    ENDELSE

    ; Delete the node from its current location.
    IF location LT nodeIndex THEN $
       self->Delete, nodeIndex + 1 ELSE $
       self->Delete, nodeIndex


END
;------------------------------------------------------------------------



PRO LINKEDLIST::REPLACE_ITEM, item, index, NO_COPY=no_copy, ERROR=errorMsg

    ; This method replace an item with another item at a particular
    ; index. If index is not supplied, the the last index is used.
    
    Catch, error
    IF error NE 0 THEN BEGIN
       Catch, /Cancel
       errorMsg = !Error_State.MSG
       Message, !Error_State.MSG, /Continue
       RETURN
    ENDIF

    ; Assume no error.
    errorMsg = ""
    
    ; Must supply an item to add to the list.
    IF N_Elements(item) EQ 0 THEN BEGIN
       errorMsg = 'Must supply an item to add to the list.'
       Message, errorMsg, /Continue
       RETURN
    END
    
    ; Check for index. If there is none, add to end of list.
    IF N_Elements(index) EQ 0 THEN index = self.count - 1
    
    ; Start at the head of the list.
    currentNode = self.head
    
    ; Is this a valid pointer?
    IF ~Ptr_Valid(currentNode) THEN BEGIN
        errorMsg = 'The current node pointer is invalid.'
        Message, errorMsg, /CONTINUE
        RETURN
    ENDIF
    
    ; Find the item asked for by traversing the list.
    FOR j=0L, index-1 DO currentNode = (*currentNode).next
    
    ; Get the item pointer.
    itemPtr = (*currentNode).item
    
    ; Replace the item, if you can.
    IF Ptr_Valid(itemPtr) THEN BEGIN
    
        ; Stuff the new item into the place of the old item.
        IF Keyword_Set(no_copy) THEN BEGIN
            *(*currentNode).item = Temporary(item)
        ENDIF ELSE BEGIN
             *(*currentNode).item = item  
        ENDELSE
        
    ENDIF ELSE BEGIN
    
        errorMsg = 'The item pointer is invalid.'
        Message, errorMsg, /CONTINUE
        RETURN
        
   ENDELSE

END 
;------------------------------------------------------------------------



PRO LINKEDLIST::CLEANUP

    ; This method deletes all of the nodes and cleans up
    ; the objects pointers.
    
    self->Delete_Nodes
    Ptr_Free, self.head
    Ptr_Free, self.tail
    
END
;------------------------------------------------------------------------



FUNCTION LINKEDLIST::INIT, item

    ; Initialize the linked list. Add an item if required.
    
    IF N_Params() EQ 0 THEN RETURN, 1
    self->Add, item
    RETURN, 1
    
END
;------------------------------------------------------------------------



PRO LINKEDLIST__DEFINE

   ; The implementation of a LINKEDLIST object.

   struct = { LINKEDLIST_NODE, $    ; The LINKEDLIST NODE structure.
              previous:Ptr_New(), $ ; A pointer to the previous node.
              item:Ptr_New(), $     ; A pointer to the data item.
              next:Ptr_New()  $     ; A pointer to the next node.
              }

   struct = { LINKEDLIST, $         ; The LINKEDLIST object.
              head:Ptr_New(), $     ; A pointer to the first node.
              tail:Ptr_New(), $     ; A pointer to the last node.
              count:0L $            ; The number of nodes in the list.
              }

END
;------------------------------------------------------------------------
