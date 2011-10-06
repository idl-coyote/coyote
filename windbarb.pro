;+
; NAME:
;       WINDBARB
;
; PURPOSE:
;
;       This is routine for drawing wind barbs on a map.
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
;       Windbarb, x, y, speed, direction
;
; REQUIRED INPUTS:
;
;       x:            The X location of the wind barb, expressed in data coordinates.
;                     Positive X is pointing in EAST direction.
;
;       y:            The Y location of the wind barb, expressed in data coordinates.
;                     Positive Y is pointing in NORTH direction.
;
;       speed:        The wind speed, expressed in knots.
;
;       direction:    The wind direction in degrees clockwise from north. Winds from
;                     the NE come at 45 degrees, and the wind "arrow" points in the
;                     direction from which the window is blowing. (The wind arrow
;                     points in the direction of the station circle, with the "barbs"
;                     of the arrow at the end of the arrow from which the wind is coming.)
;
; KEYWORDS:
;
;      ASPECT:        The aspect ratio of the map or plot in the display window.
;
;      CLIP:          A four-element array in normalized coordinates [x0,y0,x1,y1] giving
;                     the lower-left and upper-right corner of a cliping rectangle. This
;                     is normally the extent of your plot. See the example below.
;
;      COLOR:         The name of the color to draw the wind barbs in. May be a vector
;                     the same length as X.
;
;      LENGTH:        The approximate length of the wind barb in normalized coordinates.
;                     Will be set to 0.066 of the plot distance in the X direction by default.
;
;      MAP_ROTATION:  The clockwise rotation in degrees of the map North from the
;                     top of the plot. Will be set to 0.0 by default.
;
;      SOUTHERN_HEMISPHERE: Windbarb "feathers" are traditionally drawn in the clockwise
;                     direction in the northern hemispere and countercolockwise in the
;                     southern hemisphere. Default is "northern" type feathers. Set this
;                     keyword to select "southern" type feathers.
;
;      STATION:       Set this keyword if you want to draw the wind barbs with station symbols.
;                     (Requires STATIONPLOT from the Coyote Library.)
;
; RESTRICTIONS:
;
;       Requires cgColor and STATIONPLOT from the Coyote Library:
;
;           http://www.idlcoyote.com/programs/cgColor.pro
;           http://www.idlcoyote.com/programs/stationplot.pro
;
; EXAMPLE:
;
;    Window, Title='Wind Barbs', /Free
;    seed = -3L
;    lon = Randomu(seed, 9) * 360 - 180
;    lat = Randomu(seed, 9) * 180 - 90
;    speed = Randomu(seed, 9) * 100 + 5.0
;    direction = Indgen(9)*45
;    Erase, Color=cgColor('Ivory', !P.Background)
;    Polyfill,[0.1, 0.1, 0.9, 0.9, 0.1], [0.1, 0.9, 0.9, 0.1, 0.1], /Normal, Color=cgColor('light gray')
;    Map_Set, /Cylindrical, Position=[0.1, 0.1, 0.9, 0.9], Color=cgColor('Steel Blue'), /NoErase
;    Map_Grid, Color=cgColor('Charcoal', !D.Table_Size-2)
;    Map_Continents, Color=cgColor('Sea Green', !D.Table_Size-3)
;    Windbarb, lon, lat, speed, direction, /Station, Color='Indian Red', /Southern_Hemisphere
;
;    To clip the windbards that fall outside the plot, substitute these two lines
;    for the last line in the example above:
;
;    clip = [0.1, 0.1, 0.9, 0.9]
;    Windbarb, lon, lat, speed, direction, /Station, Color='Indian Red', Clip=clip
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 20 May 2003.
;       It has been called to my attention that the wind barbs are pointing
;         in *exactly* the wrong direction. Sigh... Rotated by 180 degrees. DWF. 8 June 2004.
;       Now someone complains that the *corrected* version is off by 180 degrees! Sheesh!
;         Clearly, I'm no meteorologist. Both lines of code are in the file. Please use the one
;         you like the best. :-) (Line 177-178) 20 July 2004. DWF.
;       Added a CLIP keyword so you can clip the output to the extend of your graphics plot. 12 Nov 2004. DWF.
;       Added THICK keyword 23 February 2005. DWF.
;       After further research, I've reverted to the direction specified originally.
;       And I have changed the "feathers" to point clockwise normally, and counterdlockwise
;         if the SOUTHERN_HEMISPHERE keyword is set. Here are my sources (21 July 2005. DWF):
;
;            http://ww2010.atmos.uiuc.edu/(Gh)/guides/maps/sfcobs/wnd.rxml
;            http://www.al.noaa.gov/WWWHD/pubdocs/windbarb.html
;      Fixed a small CLIP problem. 21 July 2005. DWF.
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

PRO Windbarb, x, y, wspeed, wdirection, $
   Aspect=aspect, $
   Clip=clip, $
   Color=color, $
   Length=length, $
   Map_Rotation=map_rotation, $
   Station=station, $
   Southern_Hemisphere=southFeathers, $
   Thick=thick

   On_Error, 2

   ; Check positional parameters.

   IF N_PARAMS() NE 4 THEN BEGIN
      Print, 'Required Syntax: Windbard x, y, speed, direction, Aspect=aspect, Length=length, Map_Rotation=map_rotation'
      Message, 'Incorrect number of positional parameters.'
   ENDIF

   ; Check keywords

   IF N_Elements(aspect) EQ 0 THEN BEGIN
      IF Total(!X.Window) EQ 0 THEN BEGIN
         aspect = Float(!D.Y_Size) / !D.X_Size
      ENDIF ELSE BEGIN
         aspect = ((!Y.Window[1] - !Y.Window[0]) * !D.Y_Size) / ((!X.Window[1] - !X.Window[0]) * !D.X_Size)
      ENDELSE
   ENDIF
   IF N_Elements(color) EQ 0 THEN color = Make_Array(N_Elements(x), /String, Value='Yellow')
   IF N_Elements(color) EQ 1 THEN color = Replicate(color, N_Elements(x))
   IF N_Elements(length) EQ 0 THEN BEGIN
      IF Total(!X.Window) EQ 0 THEN BEGIN
         length = 1.0 / 15.0
      ENDIF ELSE BEGIN
         length = (!X.Window[1] - !X.Window[0]) / 15.0
      ENDELSE
   ENDIF
   IF N_Elements(map_rotation) EQ 0 THEN map_rotation = 0.0
   IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Initialize variables.

   sr = length * 0.25
   staff_len = length - sr
   barb_len = staff_len * 0.8
   half_len = staff_len * 0.45

   coord = Convert_Coord(x, y, /Data, /To_Normal)
   xx = coord[0,*]
   yy = coord[1,*]

   ; Make sure you have a clipping rectangle.
   IF N_Elements(clip) EQ 0 THEN BEGIN
      minxx = Min(xx, Max=maxxx)
      minyy = Min(yy, Max=maxyy)
      clip = [minxx-0.25, minyy-0.25, maxxx+0.25, maxyy+0.25]
   ENDIF

   ; Loop through all the elements of the array.

   FOR j=0L, N_Elements(x) - 1 DO BEGIN

      ; If the speed is less that 2.5 knots, draw a station plot, if needed.

      IF wspeed[j] LT 2.5 THEN BEGIN
         IF Keyword_Set(station) THEN StationPlot, x[j], y[j], Radius=sr, Color=color[j], Thick=thick
         CONTINUE
      ENDIF

      ; Set up directions for staff and barbs.

      dr  = (wdirection[j] + map_rotation) * !DtoR
      IF dr GT (2*!Pi) THEN dr = dr - (2 * !Pi)
      drb = (wdirection[j] + 60 + map_rotation) * !DtoR

      IF Keyword_Set(southFeathers) THEN BEGIN
         sindr =  Sin(dr)
         sindrb = -Sin(drb)
         cosdr =  Cos(dr)
         cosdrb = -Cos(drb)
      ENDIF ELSE BEGIN
         sindr =  Sin(dr)
         sindrb = Sin(drb)
         cosdr =  Cos(dr)
         cosdrb = Cos(drb)
      ENDELSE

      ; Count the number of 50 knot pennants and 10 knot barbs we will need.

      num50 = 0
      num10 = 0
      sp = wspeed[j] + 2.5 ; Rounded to nearest 2.5 knots.
      WHILE sp GE 50 DO BEGIN
         num50 = num50 + 1
         sp = sp - 50
      ENDWHILE
      WHILE sp GE 10 DO BEGIN
         num10 = num10 + 1
         sp = sp - 10
      ENDWHILE

      ; Draw the staff.

      x1 = clip[0] > (xx[j] + sindr * sr) < clip[2]
      y1 = clip[1] > (yy[j] + cosdr * sr * aspect) < clip[3]
      x2 = clip[0] > (x1 + sindr * staff_len) < clip[2]
      y2 = clip[1] > (y1 + cosdr * staff_len * aspect) < clip[3]
      IF Keyword_Set(station) THEN StationPlot, x[j], y[j], Radius=sr, Color=color[j]
      PLOTS, [x1, x2], [y1,y2], /Normal, Color=cgColor(color[j]), Clip=clip, Thick=thick

      ; Draw any half-barbs.

      IF sp GE 5 THEN BEGIN
         x1 = x2 + sindrb * half_len
         y1 = y2 + cosdrb * half_len * aspect
         IF x1 LT clip[0] OR x1 GT clip[2] THEN CONTINUE
         IF x2 LT clip[0] OR x2 GT clip[2] THEN CONTINUE
         IF y1 LT clip[1] OR y1 GT clip[3] THEN CONTINUE
         IF y2 LT clip[1] OR y2 GT clip[3] THEN CONTINUE
         PLOTS, [x1, x2], [y1,y2], /Normal, Color=cgColor(color[j]), Clip=clip, Thick=thick
         IF (num50 EQ 0) AND (num10 EQ 0) THEN BEGIN
            x1 = x2 + sindr * half_len
            y1 = y2 + cosdr * half_len * aspect
            IF x1 LT clip[0] OR x1 GT clip[2] THEN CONTINUE
            IF x2 LT clip[0] OR x2 GT clip[2] THEN CONTINUE
            IF y1 LT clip[1] OR y1 GT clip[3] THEN CONTINUE
            IF y2 LT clip[1] OR y2 GT clip[3] THEN CONTINUE
            PLOTS, [x1, x2], [y1,y2], /Normal, Color=cgColor(color[j]), Clip=clip, Thick=thick
         ENDIF
      ENDIF

      x1 = x2
      y1 = y2

      ; Draw full barbs.

      FOR i = 1, num10 DO BEGIN
         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         x3 = x2 + sindrb * staff_len
         y3 = y2 + cosdrb * staff_len * aspect
         IF x1 LT clip[0] OR x1 GT clip[2] THEN CONTINUE
         IF x2 LT clip[0] OR x2 GT clip[2] THEN CONTINUE
         IF x3 LT clip[0] OR x3 GT clip[2] THEN CONTINUE
         IF y1 LT clip[1] OR y1 GT clip[3] THEN CONTINUE
         IF y2 LT clip[1] OR y2 GT clip[3] THEN CONTINUE
         IF y3 LT clip[1] OR y3 GT clip[3] THEN CONTINUE
         PLOTS, [x1, x2, x3], [y1, y2, y3], /Normal, Color=cgColor(color[j]), Clip=clip, Thick=thick
         x1 = x2
         y1 = y2
      ENDFOR

      ; Draw pennants.

      FOR i = 1, num50 DO BEGIN

         x0 = x1
         y0 = y1

         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         x3 = x2 + sindrb * staff_len
         y3 = y2 + cosdrb * staff_len * aspect

         x1 = x2
         y1 = y2

         x2p = x0 + sindr * half_len
         y2p = y0 + cosdr * half_len * aspect

         x2 = x1 + sindr * half_len
         y2 = y1 + cosdr * half_len * aspect
         IF x0 LT clip[0] OR x0 GT clip[2] THEN CONTINUE
         IF x2 LT clip[0] OR x2 GT clip[2] THEN CONTINUE
         IF y0 LT clip[1] OR y0 GT clip[3] THEN CONTINUE
         IF y2 LT clip[1] OR y2 GT clip[3] THEN CONTINUE
         PLOTS, [x2, x0], [y2, y0], /Normal, Color=cgColor(color[j]), Clip=clip, Thick=thick
         POLYFILL, [x1, x2, x3], [y1, y2, y3], /Normal, Color=cgColor(color[j]), Clip=clip

         x1 = x2p
         y1 = y2p

      ENDFOR

   ENDFOR

END
