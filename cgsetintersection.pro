; docformat = 'rst'
;
; NAME:
;   cgSETINTERSECTION
;
; PURPOSE:
;   This function is used to find the intersection between two sets of integers.
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
; This function is used to find the intersection between two sets of integers.
;
; :Categories:
;    Utilities
;    
; :Returns:
;    A vector of values that are found in both set_a and set_b.
;
; :Params:
;    set_a: in, required, type=integer
;       A vector of integers.
;    set_b: in, required, type=integer
;       A vector of integers.
;
; :Keywords:
;    count: out, optional, type=integer
;         This keyword contains the number of elements in the intersection vector.
;    indices_a: out, optional, type=integer
;         The indices in vector A where the intersected values appear. Note, this requires
;         the intersected points be unique in each vector. The `Positions` keyword will 
;         return ALL the positions of the match, even if there are non-unique matches.
;    indices_b: out, optional, type=integer
;         The indices in vector B where the intersected values appear. This assumes that
;         the intersected points are represented uniquely in the A and B vectors.
;    noresult: in, optional
;         Set this keyword to a value that will be returned from the function
;         if no intersection between the two sets of numbers is found. By default, -1.
;    positions: out, optional, type=integer
;         This keyword returns the positions or locations in A where the values
;         in B appear.
;    success: out, optional, type=boolean
;         This keyword is set to 1 if an intersection was found, and to 0 otherwise.
;
; :Examples:
;    Here is how to use this program::
;      IDL> set_a = [1,2,3,4,5]
;      IDL> set_b = [4,5,6,7,8,9,10,11]
;      IDL> Print, cgSetIntersection(set_a, set_b)
;           4   5
;
;  See http://www.idlcoyote.com/tips/set_operations.html for other types of set operations.
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
;        Written by: David W. Fanning, October 31, 2009, from code originally supplied to the IDL
;           newsgroup by Research Systems software engineers.
;        Yikes, bug in original code only allowed positive integers. Fixed now. 2 Nov 2009. DWF.
;        Fixed a problem when one or both of the sets was a scalar value. 18 Nov 2009. DWF.
;        Added a POSITIONS keyword. 30 Nov 2012. DWF.
;        Added a COUNT keyword 3 Dec 2012. DWF.
;        Added INDICES_A and INDICES_B keywords at R.G. Stockwell's suggestion. 13 Dec 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2009-2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgSetIntersection, set_a, set_b, $
    COUNT=count, $
    INDICES_A=indices_a, $
    INDICES_B=indices_b, $
    NORESULT=noresult, $
    POSITIONS=positions, $
    SUCCESS=success
    
    Compile_Opt StrictArr, DefInt32
    
    ; Set up noresult value.
    IF N_Elements(noresult) EQ 0 THEN noresult = -1
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, noresult
    ENDIF
    
    ; Check parameters.
    IF N_Params() NE 2 THEN Message, 'Two input parameters or sets are required.'
    
    ; The input sets must be integers.
    IF (Size(set_a, /TYPE) GT 3) AND (Size(set_a, /TYPE) LT 12) THEN $
        Message, 'Set A must be an integer array.'
    IF (Size(set_b, /TYPE) GT 3) AND (Size(set_b, /TYPE) LT 12) THEN $
        Message, 'Set B must be an integer array.'

    ; If either of the sets is a scalar, make it a vector.
    IF N_Elements(set_a) EQ 1 && (Size(set_a))[0] EQ 0 THEN set_a = [set_a]
    IF N_Elements(set_b) EQ 1 && (Size(set_b))[0] EQ 0 THEN set_b = [set_b]

    ; Assume success.
    success = 1
    count = 0
   
    ; Find the intersection of the ranges.
    mina = Min(set_a, Max=maxa) 
    minb = Min(set_b, Max=maxb)
    minab = mina > minb
    maxab = maxa < maxb

    ; If the set ranges don't intersect, leave now.
    IF ((maxa LT minab) AND (minb GT maxab)) OR ((maxb LT minab) AND (mina GT maxab)) THEN BEGIN
        success = 0
        RETURN, noresult
    ENDIF
    
    ; Find the intersection.
    r = Where((Histogram(set_a, Min=minab, Max=maxab, REVERSE_INDICES=ra) NE 0) AND  $
              (Histogram(set_b, Min=minab, Max=maxab, REVERSE_INDICES=rb) NE 0), count)
              
    ; Was there an intersection? If not, leave now.
    IF count EQ 0 THEN BEGIN
        success = 0
        RETURN, noresult 
    ENDIF 
    
    ; Do you want the positions in A where B is found?
    IF Arg_Present(positions) THEN BEGIN
        FOR j=0,N_Elements(r)-1 DO BEGIN
           IF N_Elements(thesePositions) EQ 0 THEN BEGIN
               thesePositions = [cgReverseIndices(ra, r[j])]
           ENDIF ELSE BEGIN
               thesePositions = [thesePositions, cgReverseIndices(ra, r[j])]
           ENDELSE
        ENDFOR
        positions = thesePositions
    ENDIF
    
    ; Do you want the indices of the matches? Code provided by
    ; R.G. Stockwell. Note that if you ask for indices, the sets
    ; may NOT have duplicate values in them. Each value in both sets
    ; must be unique.
    IF Arg_Present(indices_a) || Arg_Present(indices_b) THEN BEGIN
        aindices = LonArr(count)
        bindices = LonArr(count)
        FOR matchCounter=0,count-1 DO BEGIN
            j = r[matchCounter]
            aindices[matchcounter] = ra[ra[j]:ra[j+1]-1]
            bindices[matchcounter] = rb[rb[j]:rb[j+1]-1]
        ENDFOR
        indices_a = Temporary(aindices)
        indices_b = Temporary(bindices)
    ENDIF
    
    ; Here is the result.
    result = Temporary(r) + minab

    ; Return the result. Make sure to return scalar if only a single element.
    IF N_Elements(result) EQ 1 THEN RETURN, result[0] ELSE RETURN, result
    
END
