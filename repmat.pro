;+
; NAME:
;       REPMAT
;
; PURPOSE:
;
;       This program replicates a matrix or array in the style of
;       the MATLAB RebMat command. The matrix or array is "tiled"
;       in some integer number of columns and rows.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       tiledMatrix = RepMat(matrix, ncol, nrow)
;
; AUGUMENTS:
;
;       matix:         The matrix or array to be tiled.
;       
;       ncol:          The number of columns in the tile.
;       
;       nrow:          The number of rows in the tile.
;
; RETURN_VALUE:
;
;      tiledMatrix:    If (xdim,ydim) is the size of the original 2D matrix, then the
;                      output matrix is sized (ncol*xdim, nrow*ydim).
;
; KEYWORDS:
;
;     None.
;     
; EXAMPLE:
; 
;        IDL> matrix = Reform(Indgen(6) + 1, 3, 2)
;        IDL> Print, matrix, FORMAT='(3I3)'
;             1  2  3
;             4  5  6
;        IDL> Print, RepMat(matrix, 3, 2), FORMAT='(9I3)'
;             1  2  3  1  2  3  1  2  3
;             4  5  6  4  5  6  4  5  6
;             1  2  3  1  2  3  1  2  3
;             4  5  6  4  5  6  4  5  6
;        
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 8 May 2009.
;       Algorithm significantly improved by Ronn Kling, 4 August 2009.
;       Added line to handle an input matrix with a trailing 1 dimension correctly. DJ 8 March 2011.
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
FUNCTION RepMat, matrix, ncol, nrow

    ; On error, return to caller.
    ON_ERROR, 2

    ; Check parameters.
    IF N_Elements(matrix) EQ 0 THEN Message, 'Must pass an array or matrix to replicate.'
    IF N_Params() EQ 2 THEN nrow = ncol ; Number of columns and rows is the same.
    IF N_Elements(ncol) EQ 0 THEN ncol = 1
    IF N_Elements(nrow) EQ 0 THEN nrow = 1
    
    s = Size(matrix, /DIMENSIONS)
    IF N_Elements(s) EQ 1 THEN s = [s,1] ; Handle the case of a vector being passed in
    
    ; Create array.
    array = Make_Array(s[0]*ncol, s[1]*nrow, TYPE=Size(matrix,/TYPE))
    array[0,0] = matrix
    
    ; Replicate rows first.
    IF nrow GT 1 THEN BEGIN
        FOR nrow=1,nrow-1 DO $ 
            array[0,nrow*s[1]] = matrix
    ENDIF
    
    ; Replicate columns next.
    IF ncol GT 1 THEN BEGIN
        rmatrix = array[0:s[0]-1,*]
        FOR ncol=1,ncol-1 DO $ 
            array[(ncol*s[0]),0] = rmatrix
    ENDIF

    RETURN, array
END
