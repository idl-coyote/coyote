;+
; NAME:
;       TheMonths
;
; PURPOSE:
;
;       This is a utility program for obtaining the months of the
;       year in various string formats. Primarily used for graphic
;       labeling and the like.
;
; AUTHOR:
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
;       Utilites
;
; CALLING SEQUENCE:
;
;       listOfMonths = theMonths()
;
; RETURN VALUE:
;
;       listOfMonths: The list of months as a string or string array,
;                     depending upon which keywords are set.
;
; INPUTS:
;
;       index:        The index of the month you are interested in
;                     returning. Integer from 1 to 12.
;
;  KEYWORDS:
;
;       ABBREVIATION: Set this keyword if you wish to return the months
;                     as a three letter abbreviation.
;
;       ALLCAPS:      Set this keyword if you wish to return the months
;                     in all capital letters. (Default is first letter
;                     capitalized.)
;
;       FIRSTLETTER:  Set this keyword to return just the first letter
;                     of the months.
;
;       LOWCASE:      Set this keyword to return all lowercase letters.
;
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 8 Nov 2007.
;       Retired 9 January 2013 and replaced with cgMonths.
;
;
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
FUNCTION TheMonths, index, $
   FIRSTLETTER=firstletter, $
   ALLCAPS=allcaps, $
   ABBREVIATION=abbreviation, $
   LOWCASE=lowcase

   ON_ERROR, 2 ; Return to caller on error.

   ;; Was a month index passed in tothe program?
   IF N_Elements(index) NE 0 THEN monthIndex = (1 > index < 12) - 1

   ;; Define the months.
   months = ['January', 'February', 'March', 'April', 'May', 'June', $
             'July', 'August', 'September', 'October', 'November', 'December']

   allMonths = months
   IF Keyword_Set(firstletter) THEN allMonths = StrMid(months, 0, 1)
   IF Keyword_Set(abbreviation) THEN allMonths = StrMid(months, 0, 3)
   IF Keyword_Set(allcaps) THEN allMonths = StrUpCase(allMonths)
   IF Keyword_Set(lowcase) THEN allMonths = StrLowCase(allMonths)
   IF Keyword_Set(lowcase) AND Keyword_Set(allcaps) THEN $
      Message, 'Keywords LOWCASE and ALLCAPS are mutually exclusive.'

   ;; If an index was returned, return that month.
   ;; Otherwise return all the months.
   IF N_Elements(monthIndex) NE 0 THEN BEGIN
      RETURN, allMonths[monthIndex]
   ENDIF ELSE BEGIN
        RETURN, allMonths
   ENDELSE

END
