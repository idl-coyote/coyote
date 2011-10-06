;+
; NAME:
;       FPUFIX
;
; PURPOSE:
;
;       This is a utility routine to examine a variable and fix problems
;       that will create floating point underflow errors.
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
;       Utilities
;
; CALLING SEQUENCE:
;
;       fixedData = FPUFIX(data)
;
; ARGUMENTS:
;
;       data :         A numerical variable to be checked for values that will cause
;                      floating point underflow errors. Suspect values are set to 0.
;
; KEYWORDS:
;
;       None.

; RETURN VALUE:
;
;       fixedData:    The output is the same as the input, except that any values that
;                     will cause subsequent floating point underflow errors are set to 0.
;
; COMMON BLOCKS:
;       None.
;
; EXAMPLES:
;
;       data = FPTFIX(data)
;
; RESTRICTIONS:
;
;     None.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, from Mati Meron's example FPU_FIX. Mati's
;          program is more robust that this (ftp://cars3.uchicago.edu/midl/),
;          but this serves my needs and doesn't require other programs from
;          Mati's library.  24 February 2006.
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
FUNCTION FPUFIX, data

   ; Return to caller on error after setting !Except.
   On_Error, 2
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      Message, !Error_State.Msg
      IF N_Elements(except) THEN !EXCEPT = except
      RETURN, data
   ENDIF

   ; Only want to deal with numerical data types.
   ; Return all other kinds.
   dataType = Size(data, /Type)
   nogoodtypes = [0,7,8,10,11]
   void = Where(nogoodTypes EQ dataType, count)
   IF count GT 0 THEN RETURN, data

   ; Floating underflow error we are trying to fix.
   fpu_error = 32

   ; Save current !EXCEPT. Don't report exceptions here.
   except = !EXCEPT
   !EXCEPT = 1

   ; Clear math error status.
   void = Check_Math()

   ; Do something with the data that will cause floating underflow errors.
   void = Min(data, /NAN)

   ; Check the math error status now.
   check = Check_Math()

   ; If this is a floating underflow error, then fix it.
   IF check EQ fpu_error THEN BEGIN
      info = MaChar(DOUBLE=(dataType EQ 5 OR dataType EQ 9))
      indices = Where(Abs(data) LT info.xmin, count)
      IF count GT 0 THEN data[indices] = 0
   ENDIF

   ; Clean up.
   !EXCEPT = except
   void = Check_Math()

   ; Return the repaired data.
   RETURN, data

END
