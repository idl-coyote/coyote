;+
; NAME:
;  ASPECT
;
; PURPOSE:
;
;  This function calculates and returns the normalized position
;  coordinates necessary to put a plot with a specified aspect ratio
;  into the currently active graphics window. It works on the display
;  output window as well as in a PostScript output window.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;  Graphics
;
; CALLING SEQUENCE:
;
;  position = ASPECT(aspectRatio)
;
; INPUTS:
;
;  aspectRatio: A floating point value that is the desired aspect
;     ratio (ratio of heigth to width) of the plot in the current
;     graphics output window. If this parameter is missing, an aspect
;     ratio of 1.0 (a square plot) is assumed.
;
; KEYWORD PARAMETERS:
;
;  MARGIN:  The margin around the edges of the plot. The value must be
;     a floating point value between 0.0 and 0.5. It is expressed in
;     normalized coordinate units. The default margin is 0.15.
;
;  WINDOWASPECT: The aspect ratio of the target window. If not provided,
;     the value is obtained from the current graphics window.
;
; OUTPUTS:
;
;  position: A four-element floating array of normalized coordinates.
;     The order of the elements is [x0, y0, x1, y1], similar to the
;     !P.POSITION system variable or the POSITION keyword on any IDL
;     graphic command.
;
; EXAMPLE:
;
;  To create a plot with an aspect ratio of 1:2 and a margin of
;  0.10 around the edge of the output window, do this:
;
;     plotPosition = ASPECT(0.5, Margin=0.10)
;     PLOT, Findgen(11), POSITION=plotPosition
;
;  Notice this can be done in a single IDL command, like this:
;
;     PLOT, Findgen(11), POSITION=ASPECT(0.5, Margin=0.10)
;
; MODIFICATION HISTORY:
;
;  Written by: David Fanning, November 1996.
;       Added better error checking, 18 Feb 1997, DWF.
;       Added WindowAspect keyword. 10 Feb 2000. DWF
;       Added double precision tolerance for aspectRatio. 9 NOV 2001 BT
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
FUNCTION ASPECT, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

ON_ERROR, 1

   ; Check for aspect ratio parameter and possibilities.

IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0

IF aspectRatio EQ 0 THEN BEGIN
   MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
   aspectRatio = 1.0
ENDIF

s = SIZE(aspectRatio)
IF (s(s(0)+1) NE 4) AND  (s(s(0)+1) NE 5) THEN $
   MESSAGE, 'Aspect Ratio is not a FLOAT or DOUBLE. Take care...', /Informational

   ; Check for margins.

IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15

   ; Error checking.

IF margin LT 0 OR margin GE 0.5 THEN $
   MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'

   ; Calculate the aspect ratio of the current window.

IF N_Elements(wAspectRatio) EQ 0 THEN wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Calculate normalized positions in window.

IF (aspectRatio LE wAspectRatio) THEN BEGIN
   xstart = margin
   ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
   xend = 1.0 - margin
   yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
ENDIF ELSE BEGIN
   xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
   ystart = margin
   xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
   yend = 1.0 - margin
ENDELSE

position = [xstart, ystart, xend, yend]

RETURN, position
END
