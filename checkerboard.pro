;+
; NAME:
;       Checkerboard
;
; PURPOSE:
;       This function returns a 2D image, with boxes of alternating colors.
;       Checkerboard images are useful in certain types of image processing
;       procedures and for making blended image masks.
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
;       Image Processing
;
; CALLING SEQUENCE:
;
;        board = Checkerboard()
;
; RETURN VALUE:
;
;        board:      A 2D long array of alternating colored boxes.
;
; ARGUMENTS:
;
;        boxes:      The number of boxes of alternating colors on each side
;                    of the resulting image. Must be an even integer greater
;                    than or equal to two. Optional. Default is 8 (normal
;                    checkerboard).
;
; INPUT KEYWORDS:
;
;   BLACK:           The value of the "black" boxes. By default, 0.
;
;   WHITE:           The value of the "white" boxes. By default, 255.
;
;   XSIZE:           The X size of the returned image. By default, 400.
;
;   YSIZE:           The Y size of the returned image. By default, 400.
;
; COMMON BLOCKS:
;
;   None.
;
; EXAMPLE:
;
;        IDL> cgImage, Checkerboard()
;
; MODIFICATION HISTORY:
;
;  Written by David W. Fanning, 26 September 2007, based on suggestions
;  of JD Smith on IDL newsgroup 25-26 Septermber 2007.
;-
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
FUNCTION Checkerboard, boxes, WHITE=white, BLACK=black, XSIZE=xsize, YSIZE=ysize

   ON_ERROR, 2 ; Return to caller

   IF N_Elements(boxes) EQ 0 THEN boxes = 8 ELSE boxes = Long(boxes) > 2L
   IF boxes MOD 2 NE 0 THEN boxes = boxes + 1L
   IF N_Elements(white) EQ 0 THEN white = 255B
   IF N_Elements(black) EQ 0 THEN black = 0B
   IF N_Elements(xsize) EQ 0 THEN xsize = 400L
   IF N_Elements(ysize) EQ 0 THEN ysize = 400L

   ;; Alternating 0s and 1s.
   array = LIndGen(boxes, boxes)
   board =  ( (array MOD 2) LT 1 ) XOR ( ((array / boxes) MOD 2) GE 1 )

   ;; Make sure colors are correct.
   whitePixels = Where(board EQ 0, COMPLEMENT=blackPixels)
   board[whitePixels] = white
   board[blackPixels] = black

   RETURN, Congrid(board, xsize, ysize)

END
