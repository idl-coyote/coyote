;+
; NAME:
;      MAP_GSHHS_SHORELINE
;
; PURPOSE:
;
;      Uses files from the Globally Self-consistent Hierarchical High-resolution Shoreline
;      (GSHHS) data base to draw shorelines in the manner of MAP_CONTINENTS. In other words,
;      it is assumed that a map coordinate data space has been established prior to calling
;      this routine. See the example below. The GSHHS data files can be downloaded from this
;      location:
;
;         http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
;
;      An article describing how to use this program can be found here.
;
;         http://www.dfanning.com/map_tips/gshhs.html
;         
;      Note, the authors of the GSHHS software *continually* change the header
;      structure, which you MUST know to read the data file. There are are now
;      at least four different structures in common use. Please find the one
;      you need from the commented list below. The current code uses the structure
;      for the 2.0 version of the GSHHS software.
;
; AUTHOR:
;
;      FANNING SOFTWARE CONSULTING
;      David Fanning, Ph.D.
;      1645 Sheely Drive
;      Fort Collins, CO 80526 USA
;      Phone: 970-221-0438
;      E-mail: davidf@dfanning.com
;      Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;      Mapping Utilities
;
; CALLING SEQUENCE:
;
;      Map_GSHHS_Shoreline, filename
;
; ARGUMENTS:
;
;      filename:      The name of the GSHHS input file.
;
; KEYWORDS:
;
;      COLOR:         The name of the drawing color. By default, "WHITE".
;
;      FILL:          Set this keyword to draw filled outlines.
;
;      LAND_COLOR:    The name of the land color (for FILL). By default, "INDIAN RED".
;
;      LEVEL:         The polygon LEVEL. All polygons less than or equal to this value
;                     are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;                     By default, 2 (land and lake outlines).
;
;     MAP_PROJECTION: A map projection structure (from MAP_PROJ_INIT). If using a map projection
;                     structure, a map coordinate system must be set up for the entire display window.
;
;     MINAREA:        The minimum feature area. By default, 500 km^2.
;
;     OUTLINE:        Set this keyword to draw shorelines. Set by default if FILL=0.
;
;     WATER_COLOR:    The name of the water color. By default, "SKY BLUE".
;
; RESTRICTIONS:
;
;     Requires the following programs from the Coyote Library:
;
;         http://www.dfanning.com/programs/error_message.pro
;         http://www.dfanning.com/programs/fsc_color.pro
;         http://www.dfanning.com/programs/undefine.pro
;
; EXAMPLE:
;
;     Example using MAP_SET to set up the map coordinate space.
;
;         datafile = 'gshhs_h.b'
;         Window, XSize=500, YSize=350
;         pos = [0.1,0.1, 0.9, 0.8]
;         Map_Set, -25.0, 135.0, Position=pos, Scale=64e6, /Mercator, /NoBorder
;         Polyfill, [pos[0], pos[0], pos[2], pos[2], pos[0]], $
;                   [pos[1], pos[3], pos[3], pos[1], pos[1]], $
;                   /Normal, Color=FSC_Color('Almond')
;         Map_GSHHS_Shoreline, datafile, /Fill, Level=3, /Outline
;         XYOutS, 0.5, 0.85, 'Australia', Font=0, Color=FSC_Color('Almond'), $
;               /Normal, Alignment=0.5
;
;     Example using MAP_PROJ_INIT to set up the map coordinate space.
;
;         datafile = 'gshhs_h.b'
;         Window, XSize=500, YSize=350
;         Erase, Color=FSC_Color('IVORY')
;
;        ; Lambert Azimuthal Projection
;        map = Map_Proj_Init(111, Limit=[40, -95, 50, -75], $
;            Center_Lat=45, Center_Lon=-85)
;
;        ; Create a data coordinate space based on map positions.
;       Plot, map.uv_box[[0, 2]], map.uv_box[[1, 3]], Position=[0.1, 0.1, 0.90, 0.75], $
;          /NoData, XStyle=5, YStyle=5, /NoErase
;       Map_GSHHS_Shoreline, datafile, /Fill, Level=3, Map_Projection=map, $
;          Water='DODGER BLUE', NoClip=0
;       Map_Grid, /Label, /Box, Color=FSC_Color('CHARCOAL'), Map_Structure=map
;       Map_Continents, /USA, Map_Structure=map
;       XYOutS, 0.5, 0.85, 'Great Lakes Region', Font=0, Color=FSC_Color('CHARCOAL'), $
;         /Normal, Alignment=0.5
;
; MODIFICATION HISTORY:
;
;     Written by David W. Fanning, 5 February 2006.
;     Based on programs by Liam Gumley at ftp://ftp.ssec.wisc.edu/pub/gumley/IDL/gshhs/.
;     Bit of a dog's dish at the moment reading GSHHS files. I've contacted the author or the
;        data files, but haven't heard yet. Choose *one* of the headers below and see which 
;        one works for you on the data you have. Best I can do, sorry. 24 December 2008. DWF.
;     In version 2.0 of the GSHHS they have changed the header again! Unbelievable. Modified
;        the header structure once again to cope. 4 June 2010. DWF.
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
PRO Map_GSHHS_Shoreline, filename, $ ; The name of the GSHHS data file to open
   COLOR=color, $                    ; The name of the drawing color. By default, "WHITE".
   FILL=fill, $                      ; Set this keyword to draw filled outlines.
   LAND_COLOR=land_color, $          ; The name of the land color (for FILL). By default, "INDIAN RED".
   LEVEL=level, $                    ; The polygon LEVEL. All polygons less than or equal to this value
                                     ; are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
                                     ; By default, 2 (land and lake outlines).
   MAP_PROJECTION=map_projection, $  ; A map projection structure (from MAP_PROJ_INIT).
   MINAREA=minarea, $                ; The minimum feature area. By default, 500 km^2.
   OUTLINE=outline, $                ; Set this keyword to draw shorelines. Set by default if FILL=0.
   WATER_COLOR=water_color, $        ; The name of the water color. By default, "SKY BLUE".
   _EXTRA=extra                      ; Any PLOTS or POLYFILL keywords are allowed.


   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(lun) NE 0 THEN Free_Lun, lun
      RETURN
   ENDIF

   ; Default values.
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(Filter='*.b', Title='Select GSHHS File...')
      IF filename EQ "" THEN RETURN
   ENDIF
   IF Keyword_Set(fill) THEN temp_outline = 0 ELSE temp_outline = 1
   fill = Keyword_Set(fill)
   IF N_Elements(outline) EQ 0 THEN outline = temp_outline ELSE outline = Keyword_Set(outline)
   IF N_Elements(level) EQ 0 THEN level = 2 ELSE level = 1 > level < 4
   IF N_Elements(minArea) EQ 0 THEN minArea = 500.0 ; square kilometers.
   IF N_Elements(color) EQ 0 THEN color = 'WHITE'
   IF N_Elements(water_color) EQ 0 THEN water_color = 'SKY BLUE'
   IF N_Elements(land_color) EQ 0 THEN land_color = 'INDIAN RED'

   ; Open the GSHHS data file.
   OPENR, lun, filename, /Get_Lun, /Swap_If_Little_Endian

;   ; Define the polygon header. This is for versions of the GSHHS software of 1.3 and earlier.
;   header = { id: 0L, $        ; A unique polygon ID number, starting at 0.
;              npoints: 0L, $   ; The number of points in this polygon.
;              polygonLevel: 0L, $ ; 1 land, 2 lake, 3 island-in-lake, 4 pond-in-island.
;              west: 0L, $      ; West extent of polygon boundary in micro-degrees.
;              east: 0L, $      ; East extent of polygon boundary in micro-degrees.
;              south: 0L, $     ; South extent of polygon boundary in micro-degrees.
;              north: 0L, $     ; North extent of polygon boundary in micro-degrees.
;              area: 0L, $      ; The area of polygon in 1/10 km^2.
;              version: 0L, $   ; Polygon version, always set to 3 in this version.
;              greenwich: 0S, $ ; Set to 1 if Greenwich median is crossed by polygon.
;              source: 0S }     ; Database source: 0 WDB, 1 WVS.

   ; Define the polygon header, for GSHHS software 1.4 through 1.11, which uses a 40 byte
   ; header structure. For example, gshhs_i.b from the gshhs_1.10.zip file.
;   header = { id: 0L, $        ; A unique polygon ID number, starting at 0.
;              npoints: 0L, $   ; The number of points in this polygon.
;              flag: 0L, $      ; Contains polygonlevel, version, greenwich, and source values.
;              west: 0L, $      ; West extent of polygon boundary in micro-degrees.
;              east: 0L, $      ; East extent of polygon boundary in micro-degrees.
;              south: 0L, $     ; South extent of polygon boundary in micro-degrees.
;              north: 0L, $     ; North extent of polygon boundary in micro-degrees.
;              area: 0L, $      ; Database source: 0 WDB, 1 WVS.
;              junk:bytarr(8)}  ; Eight bytes of junk to pad header.     

   ; Define the polygon header, for GSHHS software 1.4 through 1.11, which uses a 32 byte
   ; header structure. For example, gshhs_h.b from the gshhs_1.11.zip.
;   header = { id: 0L, $        ; A unique polygon ID number, starting at 0.
;              npoints: 0L, $   ; The number of points in this polygon.
;              flag: 0L, $      ; Contains polygonlevel, version, greenwich, and source values.
;              west: 0L, $      ; West extent of polygon boundary in micro-degrees.
;              east: 0L, $      ; East extent of polygon boundary in micro-degrees.
;              south: 0L, $     ; South extent of polygon boundary in micro-degrees.
;              north: 0L, $     ; North extent of polygon boundary in micro-degrees.
;              area: 0L}        ; Database source: 0 WDB, 1 WVS.
              
   ; Define the polygon header, for GSHHS software 2.0, which uses a 44 byte
   ; header structure. For example, gshhs_h.b from the gshhs_2.0.zip.
   header = { id: 0L, $        ; A unique polygon ID number, starting at 0.
              npoints: 0L, $   ; The number of points in this polygon.
              flag: 0L, $      ; Contains polygon level, version, greenwich, source, and river values.
              west: 0L, $      ; West extent of polygon boundary in micro-degrees.
              east: 0L, $      ; East extent of polygon boundary in micro-degrees.
              south: 0L, $     ; South extent of polygon boundary in micro-degrees.
              north: 0L, $     ; North extent of polygon boundary in micro-degrees.
              area: 0L, $      ; Area of polygon in 1/10 km^2.
              area_full: 0L, $ ; Area of origiinal full-resolution polygon in 1/10 km^2.
              container: 0L, $ ; ID of container polygon that encloses this polygon (-1 if "none").
              ancestor: 0L }   ; ID of ancestor polygon in the full resolution set that was the source
                               ; of this polygon (-1 of "none").

   ; Read the data and plot if required.
   count = 0L
   WHILE (EOF(lun) NE 1) DO BEGIN
   
      READU, lun, header
      
      ; Parse the flag. Version 6 corresponds to 1.1x. Version 7 cooresponds to 2.0.
      f = header.flag
      version = ISHFT(f, -8) AND 255B
      IF version LT 7 THEN BEGIN
          IF version GT 3 THEN BEGIN
             polygonLevel = (f AND 255B) 
          ENDIF ELSE BEGIN
             polygonLevel = header.level
          ENDELSE
          greenwich = ISHFT(f, -16) AND 255B
          source = ISHFT(f, -24) AND 255B
      ENDIF ELSE BEGIN
          level = f AND 255B
          polygonLevel = (f AND 255B) 
          greenwich = ISHFT(f, -16) AND 1B
          source = ISHFT(f, -24) AND 1B
          river = ISHFT(f, -25) AND 1B
      ENDELSE
      
      ; Get the polygon coordinates. Convert to lat/lon.
      polygon = LonArr(2, header.npoints, /NoZero)
      READU, lun, polygon
      lon = Reform(polygon[0,*] * 1.0e-6)
      lat = Reform(polygon[1,*] * 1.0e-6)
      Undefine, polygon

      ; Discriminate polygons based on header information.
      polygonArea = header.area * 0.1
      IF polygonLevel GT level THEN CONTINUE
      IF polygonArea LE minArea THEN CONTINUE

      ; If you have a MAP_PROJECTION structure, use it to warp LAT/LON coordinates.
      ; No point in displaying polygons that are completely outside plot
      ; area set up by a map projection (MAP_SET) or some other plotting
      ; command.
      IF N_Elements(map_projection) GT 0 THEN BEGIN

         xy = Map_Proj_Forward(lon, lat, MAP_STRUCTURE=map_projection)
         lon = Reform(xy[0,*])
         lat = Reform(xy[1,*])
      ENDIF

      xy = Convert_Coord(lon, lat, /Data, /To_Normal)
      check = ((xy[0,*] GE !X.Window[0]) AND (xy[0,*] LE !X.Window[1])) AND $
              ((xy[1,*] GE !Y.Window[0]) AND (xy[1,*] LE !Y.Window[1]))
      usePolygon = Max(check)

      IF usePolygon EQ 0 THEN CONTINUE

      ; Draw polygons. Outlines drawn with PLOTS, filled polygons drawn with
      ; POLYFILL. Assumes lat/lon data coordinate space and colors are set up.
      IF Keyword_Set(fill) THEN BEGIN

         IF (polygonLevel EQ 1) OR (polygonLevel EQ 3) THEN $
             POLYFILL, lon, lat, Color=FSC_Color(land_color), NoClip=0, _EXTRA=extra ELSE $
             POLYFILL, lon, lat, Color=FSC_Color(water_color), NoClip=0, _EXTRA=extra

      ENDIF ELSE BEGIN

         PLOTS, lon, lat, Color=FSC_Color(color), _EXTRA=extra

      ENDELSE

      ; Need outlines with a fill?
      IF Keyword_Set(fill) AND Keyword_Set(outline) THEN $
         PLOTS, lon, lat, Color=FSC_Color(color), _EXTRA=extra

   ENDWHILE
   Free_Lun, lun

END
