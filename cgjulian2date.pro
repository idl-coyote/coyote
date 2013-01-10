; docformat = 'rst'
;
; NAME:
;   cgJulian2Date
;
; PURPOSE:
;   The purpose of this function is to convert a Julian number into a date string. The format
;   of the string is selected with the Format keyword.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to convert a Julian number into a date string. The format
; of the string is selected with the `Format` keyword.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    A string or string array of dates corresponding to the Julian number that is input.
;    
; :Params:
;    jnumber: in, required, type=long
;         The Julian number that is to be converted. This may be an array of Julian numbers.
;       
; :Keywords:
;    format: in, optional, type=integer, default=0
;         An integer value that selects the date format. 
;         Possible values are:
;              0: 09 Jan 1951
;              1: 09 Jan 1951 08:21:10
;              2: 09 Jan 1951 08h 21m 10s
;              3: Jan 9, 1951
;              4: Jan 9, 1951 08:21:10
;              5: Jan 9, 1951 08h 21m 10s
;              6: 09 JAN 1951
;              7: 09 JAN 1951 08:21:10
;              8: 09 JAN 1951 08h 21m 10s
;              9: JAN 9, 1951
;             10: JAN 9, 1951 08:21:10
;             11: JAN 9, 1951 08h 21m 10s
;             12: 09 January 1951
;             13: 09 January 1951 08:21:10
;             14: 09 January 1951 08h 21m 10s
;             15: January 9, 1951
;             16: January 9, 1951 08:21:10
;             17: January 9, 1951 08h 21m 10s
;         
; :Examples:
;    IDL> jnumber = Julday(8, 9, 1951, 8, 21, 10)
;    IDL> Print, cgJulian2Date(jnumber, Format=1)
;         09 Aug 1951 12:00:00
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
;        Written, 9 January 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgJulian2Date, jnumber, FORMAT=format

    ; Return to caller, on error
    On_Error, 2
    
    ; Get today's date, if argument is not present.
    IF N_Elements(jnumber) EQ 0 THEN jnumber = Systime(/JULIAN)
    
    IF N_Elements(format) EQ 0 THEN format = 0
    
    ; Set up an array for storing the time string.
    num = N_Elements(jnumber)
    timeString = StrArr(num)
    
    ; Process the Julian day values.
    CalDat, jnumber, month, day, year, hour, minute, second
    
    ; Format the values.
    CASE format OF
        0:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION) +  $
                ' ' + String(year, Format='(I4)')
        1:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)') 
        2:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'
        3:  date = cgMonths(month, /ABBREVIATION) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)')       
        4:  date = cgMonths(month, /ABBREVIATION) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)')       
        5:  date = cgMonths(month, /ABBREVIATION) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'       
        6:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION, /ALLCAPS) +  $
                ' ' + String(year, Format='(I4)')
        7:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION, /ALLCAPS) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)') 
        8:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month, /ABBREVIATION, /ALLCAPS) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'
        9:  date = cgMonths(month, /ABBREVIATION, /ALLCAPS) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)')       
        10:  date = cgMonths(month, /ABBREVIATION, /ALLCAPS) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)')       
        11:  date = cgMonths(month, /ABBREVIATION, /ALLCAPS) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'       
        12:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month) +  $
                ' ' + String(year, Format='(I4)')
        13:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)') 
        14:  date = String(day, Format='(I2.2)') + ' ' + cgMonths(month) +  $
                ' ' + String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'
        15:  date = cgMonths(month) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)')       
        16:  date = cgMonths(month) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + ':' + $
                String(minute, Format='(I2.2)')  + ':' + String(second, Format='(I2.2)')       
        17:  date = cgMonths(month) +  ' ' + String(day, Format='(I0.2)') + ', ' + $
                String(year, Format='(I4)') + ' ' + String(hour, Format='(I2.2)')  + 'h ' + $
                String(minute, Format='(I2.2)')  + 'm ' + String(second, Format='(I2.2)') + 's'       
    ENDCASE
     
    ; Make sure you return a scalar, if needed.
    IF num EQ 1 THEN RETURN, date[0] ELSE RETURN, date
   
END
