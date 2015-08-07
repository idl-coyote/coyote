; docformat = 'rst'
;
; NAME:
;   cgTimeStamp
;
; PURPOSE:
;   This is a utility program to obtain a time stamp string suitable
;   for using in a filename or some other string.
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
; This is a utility program to obtain a time stamp string suitable
; for using in a filename or some other string.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    A string variable containing the time stamp is returned from the function.
;    
; :Params:
;    format: in, optional, type=integer, default=0
;       An integer value that selects the time stamp format. Default is 0. Possible values are::
;          0: _sun_feb_07_10:56:37_2010
;          1: _Sun_Feb_07_10:56:37_2010
;          2: _07_02_2010_10:56:37
;          3: _07022010_10:56:37
;          4: _07022010
;          5: _07Feb2010_10:56:37
;          6: _07feb2010_10:56:37
;          7: _07FEB2010_10:56:37
;          8: _02_07_2010_10:56:37
;          9: _02072010_10:56:37
;          10: _02072010
;          11: _20100207@10:56:37
;       
; :Keywords:
;    no_prefix: in, optional, type=boolean, default=0
;         Set this keyword to remove the underscore prefix normally present at the start of the time stamp.
;    random_digits: in, optional, type=integer
;         Set this keyword to the number of random digits you would like to append to the time stamp.
;    utc: in, optional, type=boolean, default=0
;         Set this keyword to use UTC time in the time stamp, rather than the local time.
;    valid: in, optional, type=boolean, default=0
;         If this keyword is set, the time stamp is passed through the IDL program IDL_VALIDNAME, and 
;         all characters that are not allowed in IDL variable names are replaced by ones that are allowed. 
;         In other words, a character like ":", which is not allowed, is turned into the character "_", which
;         is allowed.
;    
; :Examples:
;    Here is how to use this program::
;       IDL> filename = 'mydatafile' + cgTimeStamp() + '.dat'
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
;       Written by David W. Fanning, 7 February 2010.
;       Added format number 11: YYYYMMDD@HH:MM:SS so that when files are created they
;          will list in descending time order. Matt Savoie suggestion. DWF. 10 Sept 2010.
;       Added NO_PREFIX keyword that, if set, will prevent an underscore character from being
;          added to the time stamp. 10 Sept 2010. DWF.
;       Discovered that Windows and UNIX computers report the UTC time differently, resulting
;          in the day string being a one digit integer on UNIX and a two digit integer on
;          Windows. Fixed so the day string is always forced to be two digits. 8 October 2010.
;       Renamed from TimeStamp to cgTimeStamp to avoid conflict with IDL's own TimeStamp function
;          introduced in IDL 8.2.2. 6 February 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2010-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgTimeStamp, format, $
    No_Prefix=no_prefix, $
    Random_Digits=random_digits, $
    UTC=utc, $
    Valid=valid

    On_Error, 2

    ; Set keyword values.
    no_prefix = Keyword_Set(no_prefix)
    IF N_Elements(format) EQ 0 THEN format = 0
    IF N_Elements(random_digits) GT 0  THEN BEGIN
        DEFSYSV, '!FSC_RandomNumbers', EXISTS=exists
        IF exists THEN BEGIN
            randomNumber = !FSC_RandomNumbers -> GetRandomDigits(random_digits)
        ENDIF ELSE BEGIN
            DEFSYSV, '!FSC_RandomNumbers', Obj_New('RandomNumberGenerator')
            randomNumber = !FSC_RandomNumbers -> GetRandomDigits(random_digits)
        ENDELSE
    ENDIF
    
    ; Get some values for the current time.
    time = Systime(UTC=Keyword_Set(utc))
    day = Strmid(time, 0, 3)
    date = String(StrMid(time, 8, 2), Format='(I2.2)') ; Required because UNIX and Windows differ in time format.
    month = Strmid(time, 4, 3)
    year = Strmid(time, 20, 4)
    stamp = Strmid(time, 11, 8)
    months = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
    m = (Where(months EQ StrUpCase(month))) + 1
   
    ; Select the time stamp format.
    CASE format OF
        1:  timeStamp = day + '_' +month + '_' + date + '_' + stamp + '_' + year
        2:  timestamp = date + '_' + String(m, FORMAT='(I2.2)') + '_' + year + '_' + stamp
        3:  timestamp = date + String(m, FORMAT='(I2.2)') + year + '_' + stamp
        4:  timestamp = date + String(m, FORMAT='(I2.2)') + year
        5:  timestamp = date + cgMonths(m, /ABBREVIATION) + year + '_' + stamp
        6:  timestamp = date + cgMonths(m, /ABBREVIATION, /LOWCASE) + year + '_' + stamp
        7:  timestamp = date + cgMonths(m, /ABBREVIATION, /ALLCAPS) + year + '_' + stamp
        8:  timestamp = String(m, FORMAT='(I2.2)') + '_' + date + '_' + year + '_' + stamp
        9:  timestamp = String(m, FORMAT='(I2.2)') + date + year + '_' + stamp        
        10: timestamp = String(m, FORMAT='(I2.2)') + date + year
        11: timestamp = year + String(m, FORMAT='(I2.2)') + date + '@' + stamp
        ELSE: timeStamp = StrLowCase(day) + '_' + StrLowCase(month) + '_' + date + '_' + stamp + '_' + year
    ENDCASE

    ; Add an first-letter underscore, unless the user explicitly asked not to.
    IF ~no_prefix THEN BEGIN 
       timestamp = '_' + timestamp
    ENDIF

    ; Convert to a valid string, if required.
    IF Keyword_Set(valid) THEN timestamp = IDL_Validname(timeStamp, /CONVERT_ALL)
    
    ; Add the random numbers, if required.
    IF N_Elements(randomNumber) NE 0 THEN BEGIN
        timeStamp = timeStamp + '_' + randomNumber
    ENDIF
    
    RETURN, timeStamp
    
END
