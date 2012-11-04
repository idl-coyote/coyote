; docformat = 'rst'
;
; NAME:
;   cgObj_Isa
;
; PURPOSE:
;   The purpose of this utility routine is to check to be sure the object argument is 
;   valid and belongs to the specified object class. It is more useful than the built-in
;   Obj_Isa function, because it can handle undefined variable arguments.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;
;+
; The purpose of this utility routine is to check to be sure the object argument is 
; valid and belongs to the specified object class. It is more useful than the built-in
; Obj_Isa function, because it can handle undefined variable arguments.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    The function returns a 0 to indicate FALSE or a 1 to indicate TRUE. If passed
;    an array of objects, it will return an array of 0s and 1s for each object.
;    
; :Params:
;    object: in, optional, type=object
;       The object reference to check. It may be undefined. This variable may
;       also be an array.
;    classname: in, optional, type=string
;       The object class name. If undefined, the function always returns false.   
;         
; :Examples:
;    Here is how to use this program::
;       IDL> IF cgObj_ISA(object, 'cgKML_Feature') THEN object -> SetProperty, PLACENAME='Boulder'
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 4 Novemeber 2012 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgOBJ_ISA, object, classname

   Compile_Opt idl2
   
   ; Return to caller on error.
   On_Error, 2
   
   ; Get the number of objects.
   numObjects = N_Elements(object)
   CASE numObjects OF
      0 : RETURN, 0B
      1 : ok = 1B
      ELSE : ok = Bytarr(numObjects) + 1B
   ENDCASE

   ; If classname is missing, return FALSE.
   IF (N_Elements(classname) NE 1) THEN RETURN, ok * 0B

   FOR j= 0,numObjects-1 DO BEGIN
      IF (~Obj_Valid(object[j])) THEN ok[j] = 0B $
      ELSE IF (~Obj_ISA(object[j], classname)) THEN ok[j] = 0B
   ENDFOR

   RETURN, ok
END