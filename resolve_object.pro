;+
; NAME:
;  RESOLVE_OBJECT
;
; PURPOSE:
;
;   The purpose of this function is to resolve object methods in files that have the object
;   methods in the same file as the object class definition module (i.e., object__define.pro).
;   It is particularly useful in restoring object methods for objects that have been saved and
;   are being restored. Restored objects often do not know about their methods unless an object
;   of the same object class has been previously compiled in that IDL session.
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
;   Utilities
;
; CALLING SEQUENCE:
;
;   Resolve_Object, obj_or_class
;
; ARGUMENTS:
;
;  obj_or_class:   Either an IDL object or the class name of an IDL object. Required parameter.
;
; KEYWORDRS:
;
;  ROUTINE_INFO:   Not strictly used by the user of the program, but this provides a mechanism by which
;                  currently compiled routine names can be checked, so that object code is not being 
;                  recompiled unnecessarily. It is actually used internally in the code in a sort of
;                  recursive approach to handling object superclasses.
;
; INFORMATION:
;
;  A discussion of this routine, and of the problem the routine was written to address can
;  be found here:
;  
;     http://www.idlcoyote.com/tips/saved_objects.html
;     
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, August 20, 2009, and based on code written by JD Smith and
;     discussed in the article above.
;-
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
PRO Resolve_Object, obj_or_class, ROUTINE_INFO=ri

      ON_ERROR, 2 ; Return to caller.

      IF N_Params() NE 0 THEN BEGIN 
         IF Size(obj_or_class, /TNAME) EQ 'STRING' THEN BEGIN
             class = StrUpCase(obj_or_class) 
         ENDIF ELSE BEGIN
             IF ~Obj_Valid(obj_or_class) THEN BEGIN
                Message, 'Object is not valid.'
             ENDIF 
             class = Obj_Class(obj_or_class)
         ENDELSE
      ENDIF ELSE Message, 'One argument, either an object or an object class name, is required.'
      
      IF N_Elements(ri) EQ 0 THEN ri = Routine_Info()
     
      FOR j=0,N_Elements(class)-1 DO BEGIN 
      
         defpro = class[j]+'__DEFINE'
         
         IF (Where(ri EQ defpro))[0] EQ -1 THEN BEGIN
         
            ;; Compile and define the class. Silent error handler
            ;; to skip the case where the superclass is a built-in
            ;; IDL routine.
            Catch, theError
            IF theError NE 0 THEN BEGIN
                CONTINUE
                GOTO, skipcall
            ENDIF
            
            Call_Procedure, defpro
            
            skipcall:
            Catch, /CANCEL
            
         ENDIF 
         
         ; Do superclass objects here.
         supers = Obj_Class(class[j], /SUPERCLASS, COUNT=cnt)
         IF cnt GT 0 THEN Resolve_Object, supers, ROUTINE_INFO=ri
         
      ENDFOR 
      
   END
