; docformat = 'rst'
;
; NAME:
;   cgRandomIndex
;
; PURPOSE:
; This function returns random indices without replacement. You can, for example,
; select 100 random, unique indices from a vector of 10000 random indices.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;+
; This function returns random indices without replacement. You can, for example,
; select 100 random, unique indices from a vector of 10000 random indices.
; 
; The program uses an `algorithm offered by JD Smith <http://www.idlcoyote.com/code_tips/randomindex.html>`
; on the IDL newsgroup.
; 
; :Categories:
;    Utilities
;    
; :Params:
;     length: in, required, type=long
;        The number of random values to be considered in the selection process. In
;        other words, the length of the selection vector.
;     number: in, required, type=long
;        The number of unique, random indices you want the function to return out of
;        the selection vector.
;  
; :Keywords:
;      seed: in, optional, type=long
;         The seed for the random number generator, RandomU. This is also an output
;         variable, which you should use as the input to the next cgRandomIndices call,
;         if you need to do several random selections very quickly in a program.
;         
; :Examples:
;    To select 10 random indices from a list of 100::
;       indices = cgRandomIndices(100, 10, SEED=seed)
;       Print, indices
;            7  13  20  21  32  44  50  66  80  93
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
;     Change History::
;        Written, 22 December 2011 from an algorithm by JD Smith. David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgRandomIndices, length, number, SEED=seed

     Compile_Opt idl2
    
     ; Error handling.
     Catch, theError
     IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, -1
     ENDIF

     ; If number is greater than half the length, then instead of accumulating
     ; good indices, we will discard bad indices and use Histogram to generate
     ; the "real" indices list.
     swap = number GT length/2
     IF swap THEN n = length - number ELSE n = number
     
     ; Make an array of the proper length for the indices.
     indices = LonArr(n, /NOZERO)
     
     ; Accumulate unique indices in the array.
     M = n
     WHILE n GT 0 DO BEGIN
        indices[M-n] = Long( RandomU(seed, n) * length )
        indices = indices[Sort(indices)]
        u = Uniq(indices)
        n = M - N_Elements(u)
        indices[0] = indices[u]
     ENDWHILE
     
     ; If you are discarding, then the indices are selected with HISTOGRAM.
     IF swap THEN indices = Where(Histogram(indices, MIN=0, MAX=length-1) EQ 0)
     RETURN, indices
     
END
