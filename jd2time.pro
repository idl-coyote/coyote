; docformat = 'rst'
;
; NAME:
;   jd2Time
;
; PURPOSE:
;   The purpose of this function is to convert a Julian day number into
;   a time string of the form "16 Mar 2009".
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to convert a Julian day number into
; a time string of the form "16 Mar 2009".
; 
; :Categories:
;    Utilities
;    
; :Returns:
;    A scalar or vector of time strings of the form "16 Mar 2009 15:34:26".
;    
; :Params:
;    jdnumber: in, optional, type=integer
;       A Julian day number or array of Julian day numbers. If absent,
;       today's current Julian day number.
;    jdyear: in, optional, type=integer
;       The year for which the Julian day number applies. If absent, the current year.
;       
; :Keywords:
;     day: out, optional, type=integer
;        The day of the month as an integer.
;     month: out, optional, type=integer
;        The month as an integer.
;     year: out, optional, type=integer
;        The year as an integer.
;        
; :Examples:
;    Used like the IDL AXIS command::
;       IDL> cgPlot, cgDemoData(1), YStyle=8, Position=[0.1, 0.1, 0.85, 0.9], /Window
;       IDL> cgAxis, /YAxis, Color='red', YRange=[-500, 500], /Save, /Window
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
;        Written by: David W. Fanning, 25 June 2009.
;        Added DAY, MONTH, and YEAR keywords. 18 Sept 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2009-2012, Fanning Software Consulting, Inc.
;-
FUNCTION JD2Time, jdnumber, jdyear, DAY=day, MONTH=month, YEAR=year

    ; Return to caller, on error
    On_Error, 2
    
    ; Get today's date, if argument is not present.
    IF N_Elements(jdnumber) EQ 0 THEN BEGIN
        jnumber = Systime(/JULIAN)
        CalDat, jnumber, month, day, year
        RETURN, String(day, FORMAT='(I2.2)') + ' ' + $
                theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
                String(year, FORMAT='(I4)')
    ENDIF
    
    ; Make sure this is a Julian Day Number.
    num = N_Elements(jdnumber)
    error = BytArr(num)
    FOR j=0,num-1 DO BEGIN
        IF jdnumber[j] LT 1 OR jdnumber[j] GT 366 THEN error[j] = 1
    ENDFOR
    IF Total(error) NE 0 THEN Message, 'Julian Day Numbers must be in the range 1-366.'
    
    ; Make sure you have a year and that the number matches the number of days.
    IF N_Elements(jdyear) EQ 0 THEN jdyear = Replicate(Fix(StrMid(Systime(), 19)), num)
    IF N_Elements(jdyear) NE num THEN jdyear = Replicate(jdyear, num)
    year = jdyear
    
    ; Calculate the date.
    CalDat, Julday(1, jdnumber, jdyear), month, day
    timeString = String(day, FORMAT='(I2.2)') + ' ' + $
         theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
         String(jdyear, FORMAT='(I4)')   
     
   ; Make sure you return a scalar, if needed.
   IF num EQ 1 THEN Return, timeString[0] ELSE RETURN, timeString
   
END
