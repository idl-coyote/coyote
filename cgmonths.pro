; docformat = 'rst'
;
; NAME:
;   cgMonths
;
; PURPOSE:
;   This is a utility program for obtaining the months of the year in various string formats. 
;   It is primarily used for labeling axes on graphics plots.
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
; This is a utility program for obtaining the months of the year in various string formats. 
; It is primarily used for labeling axes on graphics plots.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    The program returns a list of months as a string or string array, depending upon 
;    which keywords are set.
;    
; :Params:
;    index: in, required, type=integer
;         The index of the month you are interested in returning. An integer or
;         integer array from 1 to 12.
;       
; :Keywords:
;    abbreviation: in, optional, type=boolean, default=0
;          Set this keyword if you wish to return the month as a three letter abbreviation,
;          for example, "aug". Keywords may be used in conjunction with other keywords.
;    allcaps: in, optional, type=boolean, default=0
;          Set this keyword if you wish to return the month in all capital letters,
;          for example, "AUGUST". Keywords may be used in conjunction with other keywords.
;    firstletter: in, optional, type=boolean, default=0
;          Set this keyword if you wish to return the month as with its first letter capitalized,
;          for example, "August". Keywords may be used in conjunction with other keywords.
;    lowcase: in, optional, type=boolean, default=0
;          Set this keyword if you wish to return the month in all lower case letters,
;          for example, "august". Keywords may be used in conjunction with other keywords.
;         
; :Examples:
;    Here is how to use this program::
;       IDL> Print, cgMonths([2,4,6], /FirstLetter, /Abbreviation)
;            Feb Apr Jun
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
;        Written, 8 Nov 2007 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2007-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgMonths, index, $
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
