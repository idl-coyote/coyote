;+
; NAME:
;       PrecipMap
;
; PURPOSE:
;
;       This is a program that demonstrates how to place an IDL map projection
;       onto an image that is already in a map projection space. It uses a NOAA
;       precipitation image that is in a polar stereographic map projection, and
;       for which the latitudes and longitudes of its four corners are known.
;
;       For additional details, see http://www.idlcoyote.com/map_tips/precipmap.html.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;       Map Projection
;
; CALLING SEQUENCE:
;
;       IDL> PrecipMap, filename
;
; INPUTS:
;
;      filename:   The name of the precipitation file. For demo, download
;                  ST4.2005010112.24h.bin from http://www.idlcoyote.com/data/ST4.2005010112.24h.bin.
;                  
; KEYWORDS:
; 
;      DATA:   Set this keyword to a named variable that on output will contain the scaled data.
;      
;      PALETTE:    Set this keyword to a named variable that on output will contain the color 
;                  palette used to display the data.
;                   
;
; RESTRICTIONS:
;
;     Requires files from the Coyote Library:
;
;     http://www.idlcoyote.com/documents/programs.html
;
; MODIFICATION HISTORY:
;
;  Written by David W. Fanning, 28 April 2006 from code and discussion supplied
;       by James Kuyper in the IDL newsgroup.
;  Renamed Colorbar procedure to cgColorbar to avoid conflict with IDL 8 Colorbar function.
;        26 September 2010. DWF.
;  Got the polar stereo map projection correct. 5 September 2011. DWF.
;  Added DATA, MAP, and PALETTE output keywords and updated to use more modern Coyote
;        Library mapping routines. 2 November 2012. DWF.
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

FUNCTION PrecipMap_Annotate, axis, index, value

   ; This is a function for annotating the colorbar.

   plevels = Indgen(15)
   levels = [0, 2, 5, 10, 15, 20, 25, 50, 75, 100, 125, 150, 200, 250]
   labels = StrTrim(levels, 2)
   text = [labels[0:12], labels[13] + '+', '']
   selection = Where(plevels EQ value)
   RETURN, (text[selection])[0]

END ;----------------------------------------------------------------------



PRO PrecipMap, filename, DATA=data, MAP=map, Palette=palette

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

   ; Need a filename?
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(Filter='*.bin') ; Demo file.
      IF filename EQ "" THEN RETURN
   ENDIF
   OPENR, lun, filename, /Get_Lun
   image = FltArr(1121, 881)
   ReadU, lun, image
   Free_Lun, lun

   ; Set up 15 colors, roughly equvilent to NOAA precipitation colors.
   ; Use black for missing values.
   colors = ['dark green', 'lime green', 'light sea green', 'yellow', 'khaki', $
             'dark goldenrod', 'light salmon', 'orange', 'red', 'sienna', 'purple', $
             'orchid', 'thistle', 'sky blue', 'black']
   TVLCT, cgColor(colors, /Triple), 1

   ; Process image data. Missing values = 9.999e20.
   s = Size(image, /Dimensions)
   scaledImage = BytArr(s[0],s[1])
   levels = [0, 2, 5, 10, 15, 20, 25, 50, 75, 100, 125, 150, 200, 250]
   FOR j=1,13 DO BEGIN
      index = Where(image GE levels[j-1] AND image LT levels[j], count)
      IF count GT 0 THEN scaledImage[index] = Byte(j)
   ENDFOR
   index = Where(image GT 250 AND image LT 9.999e20, count)
   IF count GT 0 THEN scaledImage[index] = 14B
   missing = Where(image EQ 9.999e20, missing_count)
   IF missing_count GT 0 THEN scaledImage[missing] = 15B ; Will be black.

   ; Set up the map structure. This image is a stereographic image.
   mapCoord = Obj_New('cgMap', "Polar Stereographic", CENTER_LONGITUDE=-105, CENTER_LATITUDE=70)

   ; Set up latitutes and longitudes at the corners of the image. (ll, ul, ur, lr)
   longitude = [-119.023D, -134.039, -59.959, -80.7500]
   latitude =  [  23.117D,   53.509,  45.619,  19.8057]

   ; Project those lat/lon points into UV space.
   uv = mapCoord -> Forward(longitude, latitude)
   
   ; To set up map projection space, we need values at left, top, right, and bottom
   ; of image. We calculate these in UV space. Note that these values are in the
   ; center of the pixel. We have to move them to the edge of the pixel below.
   ; The U direction corresponds to longitude, and the V direction to latitude.
   topv =   (uv[1,1] + uv[1,2]) * 0.5
   botv =   (uv[1,0] + uv[1,3]) * 0.5
   leftu =  (uv[0,0] + uv[0,1]) * 0.5
   rightu = (uv[0,2] + uv[0,3]) * 0.5

   ; Calculate the scales in UV directions. The variable s contains the image dimensions.
   xscale = (rightu-leftu) / (s[0]-1)
   yscale = (topv-botv) / (s[1]-1)

   ; Get the range of the UV box that envelopes the image.
   xrange = [uv[0,1] - (0.5 * xscale), uv[0,2] + (0.5 * xscale)]
   yrange = [uv[1,0] - (0.5 * yscale), uv[1,1] + (0.5 * yscale)]

   ; Decide on a position of the image in the window.
   pos = [0.05, 0.25, 0.95, 0.95]
   
   ; Update the map coordinate object.
   mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange, POSITION=pos

   ; Display the image. The variable POS will change (probably) to keep the aspect.
   cgDisplay, 500, 500
   cgImage, scaledImage, POSITION=pos, /KEEP_ASPECT, /ERASE

   ; Set up a map coordinate space on top of the image in UV coordinates.
   mapCoord -> Draw
;
;   ; Add continent and state outlines, along with grid labels.
   cgMap_Continents, /HIRES, Color='medium gray', /USA, MAP=mapCoord
   cgMap_Grid, LATS=Indgen(8)*5+Round(MIN(latitude)), /LABEL, $
             LONS = Indgen(8)*10+Round(Min(longitude)), $
             COLOR='gray', MAP=mapCoord

   ; Add a colorbar. Non-linear scaling requires use of tick formatting function.
   cgColorbar, NColors=13, Bottom=1, Position=[pos[0], 0.1, pos[2], 0.15], $
      Divisions=14, Title='24 Hour Precipitation (mm)', AnnotateColor='black', $
      /Discrete, OOB_High=14, XTickFormat='PrecipMap_Annotate'
  
   ; Need output keywords?
   IF Arg_Present(data) THEN data = scaledImage
   IF Arg_Present(palette) THEN BEGIN
      TVLCT, r, g, b, /Get
      palette = [[r],[g],[b]]
   ENDIF
   IF Arg_Present(map) THEN map = mapCoord
        
END ;----------------------------------------------------------------------
