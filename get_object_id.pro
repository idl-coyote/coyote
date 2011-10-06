;+
; NAME:
;    GET_OBJECT_ID
;
; PURPOSE:
;
;    The purpose of this function is to be able to obtain a unique
;    object identifier string for a heap variable (object or pointer).
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
;    Utility.
;
; CALLING SEQUENCE:
;
;    objectID = Get_Object_ID(theObject)
;
; INPUTS:
;
;    theObject:    The object or pointer for which an identifier is requested. If
;                  this is a null object, the function returns the string
;                  "NullObject". If it is a null pointer, "NullPointer". 
;
; OUTPUTS:
;
;    objectID:     The unique object or pointer identifier.
;
; KEYWORDS:
;
;    NUMBER:       If this keyword is set, the function returns the unique
;                  number identifier associated with a valid pointer or object.
;                  The number is returned as a STRING variable. The string 
;                  "-999" is returned if the pointer or object is invalid and
;                  this keyword is set.
;
; EXAMPLE:
;
;    Create a trackball object and obtain its unique object ID.
;
;       IDL> theObject = Obj_New('TRACKBALL', [100,100], 50)
;       IDL> objectID = Get_Object_ID(theObject, NUMBER=number)
;       IDL> Print, objectID
;               ObjHeapVar71(TRACKBALL)
;       IDL> Print, number
;               71
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 4 September 2003.
;    Added NUMBER keyword. DWF, 22 September 2008.
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
FUNCTION Get_Object_ID, theObject, NUMBER=number

   On_Error, 2

   ; Store information about the object in a string variable.
   Help, theObject, Output=theInfo
   theInfo = theInfo[0]

   ; Search the string for the identifying information and return it.
   f_bracket = StrPos(theInfo, '<')
   r_bracket = StrPos(theInfo, '>')
   id = StrMid(theInfo, f_bracket+1, r_bracket-f_bracket-1)
   
   ; Check to see if the number variable should be changed.
   IF Keyword_Set(number) THEN BEGIN
      CASE 1 OF
         Obj_Valid(theObject): BEGIN
            openParen = StrPos(id, '(')
            id = StrMid(id, 10, openParen-10)
            END
         Ptr_Valid(theObject): id = StrMid(id, 10)
         ELSE: id = "-999"
      ENDCASE
   ENDIF
   
   RETURN, id

END
