;+
; NAME:
;    JN2TIME
;
; PURPOSE:
;
;    The purpose of this function is to convert a Julian number into
;    a time string of the form "16 Mar 2009 15:34:26."
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    result = JN2TIME(jnumber)
;
; INPUTS:
;
;    jnumber:   A Julian number or array of Julian numbers. If absent,
;                today's current local time is returned.
;
; OUTPUTS:
;
;    result:     A scalar or vector of time strings of the form "16 Mar 2009 15:34:26".
;
; KEYWORDS:
;
;    SHORT:      Set this keyword to return just the day, month, year portion of the string.
;    
; DEPENDENCIES:
; 
;    Requires THEMONTHS from the Coyote Library.
;    
;         http://www.dfanning.com/programs/themonths.pro
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 25 June 2009.
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
FUNCTION JN2Time, jnumber, SHORT=short

    ; Return to caller, on error
    On_Error, 2
    
    ; Get today's date, if argument is not present.
    IF N_Elements(jnumber) EQ 0 THEN jnumber = Systime(/JULIAN)
    
    ; Set up an array for storing the time string.
    num = N_Elements(jnumber)
    timeString = StrArr(num)
    
    ; Process the Julian day values.
    CalDat, jnumber, month, day, year, hour, minute, second
    IF Keyword_Set(short) THEN BEGIN
       timeString = String(day, FORMAT='(I2.2)') + ' ' + $
           theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
            String(year, FORMAT='(I4)')   
    ENDIF ELSE BEGIN
       timeString = String(day, FORMAT='(I2.2)') + ' ' + $
            theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
            String(year, FORMAT='(I4)') + ' ' + $
            String(hour, FORMAT='(I2.2)') + ':' + $
            String(minute, FORMAT='(I2.2)') + ':'  + $
            String(second, FORMAT='(I2.2)')  
    ENDELSE
     
    ; Make sure you return a scalar, if needed.
    IF num EQ 1 THEN Return, timeString[0] ELSE RETURN, timeString
   
END