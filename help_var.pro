;+
; NAME:
;       HELP_VAR
;
; PURPOSE:
;
;       The purpose of this program is to display HELP on just
;       the variables at the level in which HELP_VAR is called.
;       It is similar to the HELP command, except that compiled
;       functions and procedures are not displayed.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Utilities.
;
; CALLING SEQUENCE:
;
;       HELP_VAR
;
; REQUIRED INPUTS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Memory is allocated for each variable, in turn, then deleted.
;       Uses undefined and unsupported ROUTINE_NAMES function. May not
;       work in all versions of IDL, including future versions.
;
; EXAMPLE:
;
;
;       PRO HELP_VAR_TEST
;          a = 4.0
;          b = Lindgen(11)
;          HELP_VAR
;       END
;
;       IDL> help_var
;            A          FLOAT     =       4.00000
;            B          LONG      = Array[11]
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 8 August 2003.
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

PRO HELP_VAR_UNDEFINE, varname
IF (N_Elements(varname) NE 0) THEN tempvar = Temporary(varname)
END


PRO HELP_VAR

;What level are we on?

levelNumber = ROUTINE_NAMES(/LEVEL)

; Get the names of the variables one level above me.

names = ROUTINE_NAMES(Variables=levelNumber - 1)

; Check to see if you can grab those variables.
; You can only set a value if the variable is define.
;This routine will skill all undefined variables.

FOR j=0, N_Elements(names) - 1 DO BEGIN

   ; Get the variable if it is defined.
   IF N_Elements(ROUTINE_NAMES(names[j], FETCH=levelNumber - 1)) GT 0 THEN BEGIN
   value  = ROUTINE_NAMES(names[j], FETCH=levelNumber - 1)

   ; What is this variable?
   HELP, value, Output=s

   ; Delete the variable so we don't double our memory
   ; allocation. :-(

   Help_Var_UnDefine, value

   ; Substitute the actual name of the variable for VALUE
   ; and print it out.

   Print, StrUpCase(names[j]) + StrMid(s, 6)

   ENDIF

ENDFOR

END
