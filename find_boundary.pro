;+
; NAME:
;       FIND_BOUNDARY
;
; PURPOSE:
;
;       This program finds the boundary points about a region of interest (ROI)
;       represented by pixel indices. It uses a "chain-code" algorithm for finding
;       the boundary pixels.
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
;
;       Graphics, math.
;
; CALLING SEQUENCE:
;
;       boundaryPts = Find_Boundary(indices, XSize=xsize, YSize=ysize)
;
; OPTIONAL INPUTS:
;
;       indices - A 1D vector of pixel indices that describe the ROI. For example,
;            the indices may be returned as a result of the WHERE function.
;
; OUTPUTS:
;
;       boundaryPts - A 2-by-n points array of the X and Y points that describe the
;            boundary. The points are scaled if the SCALE keyword is used.
;
; INPUT KEYWORDS:
;
;       SCALE - A one-element or two-element array of the pixel scale factors, [xscale, yscale],
;            used to calculate the perimeter length or area of the ROI. The SCALE keyword is
;            NOT applied to the boundary points. By default, SCALE=[1,1].
;
;       XSIZE - The X size of the window or array from which the ROI indices are taken.
;            Set to !D.X_Size by default.
;
;       YSIZE - The Y size of the window or array from which the ROI indices are taken.
;            Set to !D.Y_Size by default.
;
; OUTPUT KEYWORDS:
;
;       AREA - A named variable that contains the pixel area represented by the input pixel indices,
;            scaled by the SCALE factors.
;
;       CENTER - A named variable that contains a two-element array containing the center point or
;            centroid of the ROI. The centroid is the position in the ROI that the ROI would
;            balance on if all the index pixels were equally weighted. The output is a two-element
;            floating-point array in device coordinate system, unless the SCALE keyword is used,
;            in which case the values will be in the scaled coordinate system.
;
;       PERIM_AREA - A named variable that contains the (scaled) area represented by the perimeter
;            points, as indicated by John Russ in _The Image Processing Handbook, 2nd Edition_ on
;            page 490. This is the same "perimeter" that is returned by IDLanROI in its
;            ComputeGeometry method, for example. In general, the perimeter area will be
;            smaller than the pixel area.
;
;       PERIMETER - A named variable that will contain the perimeter length of the boundary
;            upon returning from the function, scaled by the SCALE factors.
;
;  EXAMPLE:
;
;       LoadCT, 0, /Silent
;       image = BytArr(400, 300)+125
;       image[125:175, 180:245] = 255B
;       indices = Where(image EQ 255)
;       Window, XSize=400, YSize=300
;       TV, image
;       PLOTS, Find_Boundary(indices, XSize=400, YSize=300, Perimeter=length), $
;           /Device, Color=cgColor('red')
;       Print, length
;           230.0
;
; DEPENDENCIES:
;
;       Requires ERROR_MESSAGE from the Coyote Library.
;
;           http://www.idlcoyote.com/programs/error_message.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, April 2002. Based on an algorithm written by Guy
;       Blanchard and provided by Richard Adams.
;       Fixed a problem with distinction between solitary points and
;          isolated points (a single point connected on a diagonal to
;          the rest of the mask) in which the program can't get back to
;          the starting pixel. 2 Nov 2002. DWF
;       Added the ability to return the perimeter length with PERIMETER and
;           SCALE keywords. 2 Nov 2002. DWF.
;       Added AREA keyword to return area enclosed by boundary. 2 Nov 2002. DWF.
;       Fixed a problem with POLYFILLV under-reporting the area by removing
;           POLYFILLV and using a pixel counting method. 10 Dec 2002. DWF.
;       Added the PERIM_AREA and CENTER keywords. 15 December 2002. DWF.
;       Replaced the ERROR_MESSAGE routine with the latest version. 15 December 2002. DWF.
;       Fixed a problem in which XSIZE and YSIZE have to be specified as integers to work. 6 March 2006. DWF.
;       Fixed a small problem with very small ROIs that caused the program to crash. 1 October 2008. DWF.
;       Modified the algorithm that determines the number of boundary points for small ROIs. 28 Sept 2010. DWF.
;-
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
FUNCTION Find_Boundary_Outline, mask, darray, boundaryPts, ptIndex, $
   xsize, ysize, from_direction

On_Error, 2
Catch, theError
IF theError NE 0 THEN stop

FOR j=1,7 DO BEGIN

   to_direction = (from_direction + j) MOD 8
   newPt = boundaryPts[*,ptIndex-1] + darray[*,to_direction]

      ; If this is the edge, assume it is a background point.

   IF (newpt[0] LT 0 OR newpt[0] GE xsize OR newpt[1] LT 0 OR $
       newpt[1] GE ysize) THEN CONTINUE
   IF mask[newPt[0], newPt[1]] NE 0 THEN BEGIN
      boundaryPts[*,ptIndex] = newPt

     ; Return the "from" direction.

      RETURN, (to_direction + 4) MOD 8
   ENDIF

ENDFOR

   ; If we get this far, this is either a solitary point or an isolated point.

IF TOTAL(mask GT 0) GT 1 THEN BEGIN ; Isolated point.
   newPt = boundaryPts[*,ptIndex-1] + darray[*,from_direction]
   boundaryPts[*,ptIndex] = newPt
   RETURN, (from_direction + 4) MOD 8
ENDIF ELSE BEGIN ; Solitary point.
   boundaryPts[*,ptIndex] = boundaryPts[*,ptIndex-1]
   RETURN, -1
ENDELSE
END ; ------------------------------------------------------------------------------------------



FUNCTION Find_Boundary, indices, $
   AREA=area, $
   CENTER=center, $
   PERIM_AREA=perim_area, $
   PERIMETER=perimeter, $
   SCALE=scale, $
   XSIZE=xsize, $
   YSIZE=ysize


Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message()
   RETURN, -1
ENDIF


IF N_Elements(indices) EQ 0 THEN Message, 'Indices of boundary region are required. Returning...'
IF N_Elements(scale) EQ 0 THEN BEGIN
   diagonal = SQRT(2.0D)
ENDIF ELSE BEGIN
   scale = Double(scale)
   diagonal = SQRT(scale[0]^2 + scale[1]^2)
ENDELSE
IF N_Elements(xsize) EQ 0 THEN xsize = !D.X_Size ELSE xsize = Long(xsize)
IF N_Elements(ysize) EQ 0 THEN ysize = !D.Y_Size ELSE ysize = Long(ysize)
IF Arg_Present(perimeter) THEN perimeter = 0.0

   ; Create a mask with boundary region inside.

indices = indices[Uniq(indices)]
mask = BytArr(xsize, ysize)
mask[indices] = 255B

   ; Set up a direction array.

darray = [[1,0],[1,1],[0,1],[-1,1],[-1,0],[-1,-1],[0,-1],[1,-1]]

   ; Find a starting point. The pixel to the left of
   ; this point is background

i = Where(mask GT 0)
firstPt = [i[0] MOD xsize, i[0] / xsize]
from_direction = 4

   ; Set up output points array. For narrow ROIs, we need to construct
   ; a different sort of algoritm for the number of boundary points.
IF (xsize LT 4) OR (ysize LT 4) THEN BEGIN
    boundaryPts = LonArr(2, (2*Max([xsize,ysize]) + 2*Min([xsize,ysize])))
ENDIF ELSE BEGIN
    boundaryPts = LonArr(2, (Long(xsize) * ysize / 4L) + 1)
ENDELSE
boundaryPts[0] = firstPt
ptIndex = 0L

   ;   We shall not cease from exploration
   ;   And the end of all our exploring
   ;   Will be to arrive where we started
   ;   And know the place for the first time
   ;
   ;                     T.S. Eliot
REPEAT BEGIN
   ptIndex = ptIndex + 1L
   from_direction = Find_Boundary_Outline(mask, darray, $
      boundaryPts, ptIndex, xsize, ysize, from_direction)

   IF N_Elements(perimeter) NE 0 THEN BEGIN
      IF N_Elements(scale) EQ 0 THEN BEGIN
         CASE from_direction OF
            0: perimeter = perimeter + 1.0D
            1: perimeter = perimeter + diagonal
            2: perimeter = perimeter + 1.0D
            3: perimeter = perimeter + diagonal
            4: perimeter = perimeter + 1.0D
            5: perimeter = perimeter + diagonal
            6: perimeter = perimeter + 1.0D
            7: perimeter = perimeter + diagonal
            ELSE: perimeter = 4
         ENDCASE
       ENDIF ELSE BEGIN
         CASE from_direction OF
            0: perimeter = perimeter + scale[0]
            1: perimeter = perimeter + diagonal
            2: perimeter = perimeter + scale[1]
            3: perimeter = perimeter + diagonal
            4: perimeter = perimeter + scale[0]
            5: perimeter = perimeter + diagonal
            6: perimeter = perimeter + scale[1]
            7: perimeter = perimeter + diagonal
            ELSE: perimeter = (2*scale[0]) + (2*scale[1])
         ENDCASE
      ENDELSE
   ENDIF
ENDREP UNTIL (boundaryPts[0,ptIndex] EQ firstPt[0] AND $
            boundaryPts[1,ptIndex] EQ firstPt[1])

boundaryPts = boundaryPts[*,0:ptIndex-1]

   ; Calculate area.

IF N_Elements(scale) EQ 0 THEN BEGIN

   area = N_Elements(i)

      ; Calculate area from the perimeter.
      ; The first point must be the same as the last point. Method
      ; of Russ, p.490, _Image Processing Handbook, 2nd Edition_.

   bx = Double(Reform(boundaryPts[0,*]))
   by = Double(Reform(boundaryPts[1,*]))
   bx = [bx, bx[0]]
   by = [by, by[0]]
   n = N_Elements(bx)
   perim_area = Total( (bx[1:n-1] + bx[0:n-2]) * (by[1:n-1] - by[0:n-2]) ) / 2.


ENDIF ELSE BEGIN

   area = N_Elements(i) * scale[0] * scale[1]

      ; Calculate area from the perimeter.
      ; The first point must be the same as the last point. Method
      ; of Russ, p.490, _Image Processing Handbook, 2nd Edition_.

   bx = Double(Reform(boundaryPts[0,*])) * scale[0]
   by = Double(Reform(boundaryPts[1,*])) * scale[1]
   bx = [bx, bx[0]]
   by = [by, by[0]]
   n = N_Elements(bx)
   perim_area = Total( (bx[1:n-1] + bx[0:n-2]) * (by[1:n-1] - by[0:n-2]) ) / 2.

   boundaryPts = Double(Temporary(boundaryPts))
   boundaryPts[0,*] = boundaryPts[0,*] * scale[0]
   boundaryPts[1,*] = boundaryPts[1,*] * scale[1]
ENDELSE

   ; Calculate the centroid

mask = mask GT 0
totalMass = Total(mask)
xcm = Total( Total(mask, 2) * Indgen(xsize) ) / totalMass
ycm = Total( Total(mask, 1) * Indgen(ysize) ) / totalMass
center = [xcm, ycm]

RETURN, boundaryPts
END ; ------------------------------------------------------------------------------------------
