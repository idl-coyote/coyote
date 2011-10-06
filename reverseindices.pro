; docformat = 'rst'
;
; NAME:
;   ReverseIndices
;
; PURPOSE:
;   Provides a simple way to obtain the data indices from a Histogram REVERSE_INDICES
;   vector. Returns a -1 if no indices are available. Check the COUNT keyword for the
;   number of indices returned.
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
; :Description:
;   Provides a simple way to obtain the data indices from a Histogram REVERSE_INDICES
;   vector. Returns a -1 if no indices are available. Check the COUNT keyword for the
;   number of indices returned.
;
; :Categories:
;    Utilities
;    
; :Params:
;    reverse_indices: in, required, type=integer
;         The REVERSE_INDICES vector that is returned from the HISTOGRAM command.
;    index: in, required, type=integer
;         The zero-based index into the REVERSE_INDICES vector from which to obtain
;         the indices. For example, and index value of 4 will return the indices in
;         the 5th bin (zero based counting) of the histogram. 
;       
; :Keywords:
;     count: out, optional, type=Long
;         The number of indices returned by the function.
;         
; :Return Value:
;      indices:
;         The indices that were put into the indexth bin of the histogram. A -1
;         is returned if no indices are in that particular bin.
;          
; :Examples:
;    Used with the HISTOGRAM command::
;       IDL> image = cgDemoData(7)
;       IDL> h = Histogram(image, REVERSE_INDICES=ri)
;       IDL> indices = ReverseIndices(ri, 4, COUNT=cnt)
;       IDL> Help, indices, cnt, h[4]
;       INDICES         LONG      = Array[948]
;       CNT             LONG      =          948
;       <Expression>    LONG      =          948
;       IDL> image[indices] = 0

;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Written by David W. Fanning at suggestion of Ben Tupper. 7 January 2011.
;     
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION ReverseIndices, ri, index, COUNT=count

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        count = 0
        RETURN, -1
   ENDIF
   
   ; Need two parameters.
   IF N_Params() NE 2 THEN Message, 'Two positional parameters, REVERSE_INDICES and INDEX, are required.'
   
   ; Return the indices, if there are any.
   IF ri[index] NE ri[index+1] THEN BEGIN
      indices = ri[ri[index]:ri[index+1]-1]
      count = N_Elements(indices)
   ENDIF ELSE BEGIN
      indices = -1
      count = 0
   ENDELSE
   
   RETURN, indices
   
END
