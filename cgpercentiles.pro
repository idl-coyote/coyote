; docformat = 'rst'
;
; NAME:
;   cgPercentiles
;
; PURPOSE:
;   This program calculates user-specified percentiles of a data set.
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
; This program calculates user-specified percentiles of a data set.
; A percentile is the value of a variable below which a certain percent 
; of observations fall. In other words, 75% of the values in a data set 
; fall below the 75th percentile of the data. Computing percentiles is 
; really nothing more than counting in a sorted input array. A box-and-whisker
; plot usually will display the 25th, 50th, and 75th percentiles.
;
; :Categories:
;    Math
;    
; :Returns:
;    The return value is either a scalar or vector of data values corresponding to 
;    the number of percentiles asked for with the `Percentiles` keyword, or a -1 if
;    there is an error in the program.
;    
; :Params:
;    data: in, required
;         The data from which percentiles are desired. A vector or an array.
;       
; :Keywords:
;    percentiles: in, optional, type=float
;         Set this keyword to a scalar or vector of values between 0.0 and 1.0 
;         to indicate the percentile desired. A value of 0.5 indicates the 50th
;         percentile. Default value is [0.25, 0.50, 0.75].
;         
; :Examples:
;    To return percentile values for 0.25, 0.50, and 0.75 of a data set::
;       IDL> data = Randomu(3L, 100) * 100
;       IDL> Print, cgPercentiles(data, Percentiles=[0.25, 0.5, 0.75])
;                27.4920      45.3172      69.3138
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
;        Written, 3 June 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgPercentiles, data, Percentiles=percentiles

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN, -1
   ENDIF

   ; Assume the worst.
   result = -1
   
   ; Input data is required.
   IF N_Elements(data) EQ 0 THEN Message, 'Input data is required.'
   
   ; Need default values for percentiles?
   IF N_Elements(percentiles) EQ 0 THEN percentiles = [0.25, 0.50, 0.75]
   
   ; Percentile values must be GE 0 and LE 1.0.
   index = Where((percentiles LT 0.0) OR (percentiles GT 1.0), count)
   IF count GT 0 THEN Message, 'Percentiles must be numbers between 0.0 and 1.0.'
   
   ; Count the data elements.
   num = N_Elements(data)
   
   ; Sort the data and find percentiles.
   sortIndex = Sort(data)
   sortIndices = cgScaleVector(Findgen(num+1), 0.0, 1.0)
   dataIndices = Value_Locate(sortIndices, percentiles)
   result = data[sortIndex[dataIndices]]
   
   RETURN, result
   
END