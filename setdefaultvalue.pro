; docformat = 'rst'
;
; NAME:
;   SetDefaultValue
;
; PURPOSE:
;   This procedure sets default values for positional and keyword arguments to
;   IDL procedures and functions.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This procedure sets default values for positional and keyword arguments to
; IDL procedures and functions.
;
; :Categories:
;    Utilities
;
; :Params:
;    argument: in, required
;         The augument variable you are setting the default value of. If this variable
;         is undefined, the `defaultValue` will be assigned to it. Otherwise, the argument
;         variable will not change.
;    defaultvalue: in, required
;         The default value that will be assigned to the argument variable ONLY if the argument
;         variable is undefined. If this variable is undefined, the argument variable will
;         be treated as if the BOOLEAN keyword had been set.
;
; :Keywords:
;    boolean: in, optional, type=integer
;         If this keyword is set, the argument value will always be forced to return with a 
;         value of 0 or 1.
;    range: in, optional
;         A two-element array that gives the accepted range of the variable. The output argument
;         will be forced into this data range: range[0] > argument < range[1].
;
; :Examples:
;    Here is how to use this program::
;    
;      FUNCTION Action, arg1, arg2, MULTIPLY=multiply
;  
;         SetDefaultValue, arg1, 1
;         SetDefaultValue, arg2, 2
;         SetDefaultValue, multiply, 1, /BOOLEAN 
;     
;         IF multiply THEN RETURN, arg1 * arg2 ELSE RETURN, arg1 + arg2
;     
;      END
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
;        Written by: David W. Fanning, November 26, 2008, from suggestion by Carsten Lechte on
;           IDL newsgroup on this date.
;        Made a change to the way the BOOLEAN keyword works. Now argument is set to BOOLEAN before
;           return, if required. 3 Dec 2008. DWF.
;        Added RANGE keyword. 22 March 2014. DWF.
;
; :Copyright:
;     Copyright (c) 2008-2014, Fanning Software Consulting, Inc.
;-
PRO SetDefaultValue, argument, defaultValue, RANGE=range, BOOLEAN=boolean

   ; Return to the caller if there is an error.
   On_Error, 2

   ; We only need change if the argument is undefined.
   IF N_Elements(argument) EQ 0 THEN BEGIN
        
      ; If the default value is undefined, treat as BOOLEAN.
      ; Otherwise, assign default value to the argument.
      IF N_Elements(defaultValue) EQ 0 THEN BEGIN
         argument = Keyword_Set(argument)
      ENDIF ELSE BEGIN
         argument = defaultValue
      ENDELSE
            
   ENDIF
        
   ; Require boolean.
   IF Keyword_Set(boolean) THEN argument = Keyword_Set(argument)    
        
   ; Confine to a range?
   IF N_Elements(range) NE 0 THEN BEGIN
       argument = range[0] > argument < range[1]
   ENDIF
     
END ;-----------------------------------------------------------------------------------------
