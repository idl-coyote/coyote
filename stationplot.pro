;+
; NAME:
;       STATIONPLOT
;
; PURPOSE:
;
;       This is routine for drawing station plots on a map or other display.
;       Normally, this routine is used in conjunction with WINDBARB.
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

;       Graphics.
;
; CALLING SEQUENCE:
;
;       StationPlot, x, y
;
; REQUIRED INPUTS:
;
;       x:            The X location of the center of the station plot, expressed in data coordinates.
;
;       y:            The Y location of the center of the station plot, expressed in data coordinates.
;
; KEYWORDS:
;
;      COLOR:         The name of the color to draw the station plot in. May be a vector
;                     the same length as X. Colors are those available in cgColor.
;
;      RADIUS:        The radius of the station plot circle in normalized coordinates.
;
; RESTRICTIONS:
;
;       Requires cgColor from the Coyote Library:
;
;           http://www.idlcoyote.com/programs/cgColor.pro
;
; EXAMPLE:
;
;   seed = -3L
;   lon = Randomu(seed, 20) * 360 - 180
;   lat = Randomu(seed, 20) * 180 - 90
;   speed = Randomu(seed, 20) * 100
;   direction = Randomu(seed, 20) * 180 + 90
;   Erase, cgColor('Ivory', !P.Background)
;   Map_Set, /Cylindrical,Position=[0.1, 0.1, 0.9, 0.9], Color=cgColor('Steel Blue'), /NoErase
;   Map_Grid, Color=cgColor('Charcoal', !D.Table_Size-2)
;   Map_Continents, Color=cgColor('Sea Green', !D.Table_Size-3)
;   StationPlot, lon, lat, Color='Indian Red'
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 20 May 2003, based on TVCircle from the
;       NASA Astonomy Library.
;       Added THICK keyword. 23 February 2005. DWF.
;
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
PRO StationPlot, xc, yc, Radius=radius, Color=color, Thick=thick

   ; Return to caller in event of an error.

On_Error, 2

   ; Correct number of positional parameters?

IF N_Params() NE 2 THEN BEGIN
   Print, 'Correct Syntax:  StationPlot, x, y, Radius=radius, Color=color'
   Message, 'Incorrect number of positional parameters'
ENDIF

   ; Check keyword values.

IF N_Elements(radius) EQ 0 THEN BEGIN
   IF Total(!X.Window) EQ 0 THEN BEGIN
      radius = 1.0 / 50.0
   ENDIF ELSE BEGIN
      radius = (!X.Window[1] - !X.Window[0]) / 50.0
   ENDELSE
ENDIF
nradius = Convert_Coord(radius, 0, /Normal, /To_Device)
nradius = Round(nradius[0])
IF N_Elements(color) EQ 0 THEN color = Make_Array(N_Elements(xc), /String, Value='Yellow')
IF N_Elements(color) EQ 1 THEN color = Replicate(color, N_Elements(xc))
IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Initial variables.

x = 0
y = nradius
d = 3 - 2 * nradius

   ; Find the X and Y coordinates for one-eighth of a circle.

xhalfquad = Make_Array(nradius + 1, /Integer)
yhalfquad = xhalfquad
path = 0

WHILE x LT y DO BEGIN

   xhalfquad[path] = x
   yhalfquad[path] = y
   path = path + 1

   IF d LT 0 THEN BEGIN
      d = d + (4*x) + 6
   ENDIF ELSE BEGIN
      d = d + (4 * (x-y)) + 10
      y = y - 1
   ENDELSE
   x = x + 1
ENDWHILE

   ; Fill in last point, if needed.

IF x EQ y THEN BEGIN

   xhalfquad[path] = x
   yhalfquad[path] = y
   path = path + 1

ENDIF

   ; Shrink the arrays to their correct size.

xhalfquad = xhalfquad[0:path-1]
yhalfquad = yhalfquad[0:path-1]

   ; Convert the eighth circle into a quadrant.

xquad = [xhalfquad, Rotate(yhalfquad, 5)]
yquad = [yhalfquad, Rotate(xhalfquad, 5)]

   ; Prepare to convert the quadrants into a full circle.

xquadrev = Rotate(xquad[0L:2L*path-2], 5)
yquadrev = Rotate(yquad[0L:2L*path-2], 5)

   ; Create full-circle coordinates.

x = [xquad, xquadrev, -xquad[1:*], -xquadrev]
y = [yquad, -yquadrev, -yquad[1:*], yquadrev]

   ; Plot the coordinates about the given center after converting
   ; to DEVICE coordinates.

coord = Convert_Coord(xc, yc, /Data, /To_Device)
xcenter = Round(coord[0,*])
ycenter = Round(coord[1,*])

FOR j=0L, N_Elements(xcenter)-1 DO BEGIN
   Plots, x + xcenter[j], y + ycenter[j], Color=cgColor(color[j]), /Device, Thick=thick
ENDFOR

x = [xquad,  xquadrev]
y = [yquad, -yquadrev]
u = [xquadrev,  -xquad[1:*]]
v = [-yquadrev, -yquad[1:*]]
FOR j=0L, N_Elements(xcenter)-1 DO BEGIN
   Polyfill, x + xcenter[j], y + ycenter[j], Color=cgColor(color[j]), /Device
   Polyfill, u + xcenter[j], v + ycenter[j], Color=cgColor(color[j]), /Device
ENDFOR

END
