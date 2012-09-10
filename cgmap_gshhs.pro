; docformat = 'rst'
;
; PURPOSE:
; 
;      Uses files from the Globally Self-consistent Hierarchical High-resolution Shoreline
;      (GSHHS) data base to draw shorelines in the manner of MAP_CONTINENTS. In other words,
;      it is assumed that a map coordinate data space has been established prior to calling
;      this routine. See the example below. The GSHHS data files are
;      `available for downloading <http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html>`.
;      An `article describing how to use this program <http://www.idlcoyote.com/map_tips/gshhs.html>`
;      is also available.
;         
;      Note, the authors of the GSHHS software *continually* change the header
;      structure, which you MUST know to read the data file. There are are now
;      at least four different structures in common use. Please find the one
;      you need from the commented list below. The current code uses the structure
;      for the 2.2 version of the GSHHS software.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;+--------------------------------------------------------------------------
; The program uses files from the Globally Self-consistent Hierarchical High-resolution Shoreline
; (GSHHS) data base to draw shorelines in the manner of MAP_CONTINENTS. In other words,
; it is assumed that a map coordinate data space has been established prior to calling
; this routine. See the example below. The GSHHS data files are
; `available for downloading <http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html>`.
; An `article describing how to use this program <http://www.idlcoyote.com/map_tips/gshhs.html>`
; is also available.
;         
; Note, the authors of the GSHHS software *continually* change the header
; structure, which you MUST know to read the data file. There are are now
; at least four different structures in common use. Please find the one
; you need from the commented list in the code itself. The current code uses the structure
; for the 2.2 version of the GSHHS software.
; 
; I have noticed that the polygon areas in the 2.2 version of the GSHHS software seem
; to contain completely bogus information. For example, the Great Lakes polygon is 1.7e7 
; square kilometers, while a tiny lake near-by is listed as 3.5e7 square kilometers. I have
; no explanation for why these values seem to be so wrong. I haven't tested this on other
; GSHHS versions of the data.
;
; :Categories:
;    Graphics, Map Projections
;    
; :Params:
;    filename: in, optional, type=string
;       The name of the GSHHS file to open. If not provided, the user will
;       be asked to select the file with a file selection tool.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. Note that a map projection command must be 
;        added to the window before this command is added to be effective.
;    color: in, optional, type=string, default="opposite"
;        The name of the drawing color.
;    fill: in, optional, type=boolean, default=0
;        Set this keyword to draw filled outlines.
;    land_color: in, optional, type=string, default="tan8"
;        The name of the land color (for FILL). 
;    level: in, optional, type=integer, default=2
;        The polygon LEVEL. All polygons less than or equal to this value
;        are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;        By default, 2 (land and lake outlines).
;    map_structure: in, optional, type=varies
;        A map projection structure (as created from MAP_PROJ_INIT) or a map coordinate
;        object (i.e., cgMap). If using a map projection structure, a map coordinate system 
;        must be set up for the entire display window.
;    minarea: in, optional, type=float, default=500
;        The minimum feature area in square kilometers. Features with area below
;        the minimum area will not be drawn on the map.
;    noclip: in, optional, type=boolean, default=0
;        Normally the polygons and outlines are clipped to the plot boundaries.
;        Set this keyword to turn this feature off.
;    outline: in, optional, type=boolean
;        Set this keyword to draw outlines on filled polygons. Set to 1, by default, 
;        if FILL=0. Set to zero to not draw outlines on filled polygons.
;    thick: in, optional, type=integer, default=!P.Thick
;        Set this keyword to the thickness of the lines that are drawn.
;    water_color: in, optional, type=string, default="sky blue"
;        The name of color to draw water features in.
;      
; :Author:
;    FANNING SOFTWARE CONSULTING::
;    David W. Fanning 
;    1645 Sheely Drive 
;    Fort Collins, CO 80526 USA 
;    Phone: 970-221-0438 <
;    E-mail: david@idlcoyote.com 
;    Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :Examples:
;     Example using cgMAP_SET to set up the map coordinate space::
;
;         datafile = 'gshhs_h.b'
;         cgDisplay, 500, 350
;         pos = [0.1,0.1, 0.9, 0.8]
;         cgMap_Set, -25.0, 135.0, Position=pos, /Mercator, Scale=64e6, Color='black'
;         cgColorfill, [pos[0], pos[0], pos[2], pos[2], pos[0]], $
;                      [pos[1], pos[3], pos[3], pos[1], pos[1]], $
;                      /Normal, Color='sky blue'
;         cgMap_GSHHS, datafile, Fill=1, Level=3, Color='black', /Outline
;         cgText, 0.5, 0.85, 'Australia', Font=0, Color='black', /Normal, Alignment=0.5
;         cgPlotS, [pos[0], pos[0], pos[2], pos[2], pos[0]], $
;                  [pos[1], pos[3], pos[3], pos[1], pos[1]], $
;                  /Normal, Color='black', Thick=2
;
;     Example using MAP_PROJ_INIT to set up the map coordinate space::
;
;         datafile = 'gshhs_h.b'
;         cgDisplay
;         map = Obj_New('cgMap', "Lambert Azimuthal", Limit=[40, -95, 50, -75], $
;             Center_Lat=45, Center_Lon=-85)
;         map -> SetProperty, Position=[0.1, 0.1, 0.90, 0.75], /Draw
;         cgMap_GSHHS, datafile, /Fill, Level=3, Map_Structure=map, $
;             Water='DODGER BLUE'
;         cgMap_Grid, /Label, /Box, Color='charcoal', Map_Structure=map
;         cgMap_Continents, /USA, Map_Structure=map
;         cgText, 0.5, 0.85, 'Great Lakes Region', Font=0, Color='charcoal', $
;             /Normal, Alignment=0.5
;
; :History:
;    Modification History::
;        Written by David W. Fanning, from Map_GSHHS_Shoreline program, 12 November 2011.
;        Added THICK keyword. 28 Dec 2011. DWF.
;        Modified so you can pass a map object with the MAP_STRUCTURE keyword and not
;           have it change the object to a structure. 5 April 2012. DWF.
;    
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
PRO cgMap_GSHHS, filename, $         ; The name of the GSHHS data file to open
   ADDCMD=addcmd, $                  ; Add this command to a resizeable graphics window.
   COLOR=color, $                    ; The name of the drawing color. 
   FILL=fill, $                      ; Set this keyword to draw filled outlines.
   LAND_COLOR=land_color, $          ; The name of the land color (for FILL). 
   LEVEL=level, $                    ; The polygon LEVEL. 
   MAP_STRUCTURE=map_structure, $    ; A map projection structure (from MAP_PROJ_INIT).
   MINAREA=minarea, $                ; The minimum feature area. By default, 500 km^2.
   NOCLIP=noclip, $                  ; Clip the polygons and lines to the plot boundaries.
   OUTLINE=outline, $                ; Set this keyword to draw shorelines. Set by default if FILL=0.
   THICK=thick, $                    ; The thickness of the line that is drawn.
   WATER_COLOR=water_color           ; The name of the water color. 


   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(lun) NE 0 THEN Free_Lun, lun
      IF N_Elements(thisState) NE 0 THEN SetDecomposedState, thisState
      IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
      RETURN
   ENDIF

    IF Keyword_Set(addcmd) THEN BEGIN
    
       cgWindow, 'cgMap_GSHHS', filename, $ ; The name of the GSHHS data file to open
           COLOR=color, $                    ; The name of the drawing color. 
           FILL=fill, $                      ; Set this keyword to draw filled outlines.
           LAND_COLOR=land_color, $          ; The name of the land color (for FILL). 
           LEVEL=level, $                    ; The polygon LEVEL. 
           MAP_STRUCTURE=map_structure, $    ; A map projection structure (from MAP_PROJ_INIT).
           MINAREA=minarea, $                ; The minimum feature area. By default, 500 km^2.
           OUTLINE=outline, $                ; Set this keyword to draw shorelines. Set by default if FILL=0.
           THICK=thick, $                    ; The thickness of the line that is drawn.
           WATER_COLOR=water_color, $        ; The name of the water color. 
           ADDCMD=1
          
       RETURN
       
    ENDIF
    
   ; Do this in decomposed color mode if possible.
   SetDecomposedState, 1, CurrentState=thisState
   TVLCT, r, g, b, /Get
   
   ; Default values.
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(Filter='*.b', Title='Select GSHHS File...')
      IF filename EQ "" THEN RETURN
   ENDIF
   
   ; Make sure the filename exists.
   IF ~File_Test(filename, /Read) THEN Message, 'Cannot find or read the file ' + filename + '.'
   
   IF Keyword_Set(fill) THEN temp_outline = 0 ELSE temp_outline = 1
   fill = Keyword_Set(fill)
   
   ; If you are doing graphics on a display device and there is no window
   ; open one so you can do Convert_Coord correctly.)
   IF (!D.Name EQ 'WIN') || (!D.Name EQ 'X') && (!D.Window LT 0) THEN cgDisplay
 
   ; If you got a map object, use it to recover a map structure
   ; and to set up the map coordinate space.
   IF N_Elements(map_structure) NE 0 THEN BEGIN
       IF Obj_Valid(map_structure) THEN BEGIN
          mapObj = map_structure
          mapStruct = mapObj -> GetMapStruct()           
          mapObj -> Draw, /NoGraphics
       ENDIF ELSE mapStruct = map_structure
   ENDIF 
   
   IF N_Elements(outline) EQ 0 THEN outline = temp_outline ELSE outline = Keyword_Set(outline)
   IF N_Elements(level) EQ 0 THEN level = 2 ELSE level = 1 > level < 4
   SetDefaultValue, minArea, 500.0 ; square kilometers.
   noclip = Keyword_Set(noclip)
   SetDefaultValue, color, 'opposite'
   SetDefaultValue, water_color, 'sky blue'
   SetDefaultValue, land_color, 'tan8'
   SetDefaultValue, thick, !P.Thick

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

      ; If you have a MAP_STRUCTURE variable, use it to warp LAT/LON coordinates.
      ; No point in displaying polygons that are completely outside plot
      ; area set up by a map projection (MAP_SET) or some other plotting
      ; command.
      IF N_Elements(mapStruct) NE 0 THEN BEGIN
      
         ; Convert lons from -180 to 180 to 0 to 360.
         lon = ((lon + 180) MOD 360) - 180
         ;lon = (lon + 360.0) MOD 360.0
         xy = Map_Proj_Forward(lon, lat, MAP_STRUCTURE=mapStruct)
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
         IF (polygonLevel EQ 1) OR (polygonLevel EQ 3) THEN BEGIN
             POLYFILL, lon, lat, Color=cgColor(land_color), NOCLIP=noclip 
         ENDIF ELSE BEGIN
             POLYFILL, lon, lat, Color=cgColor(water_color), NOCLIP=noclip
         ENDELSE

      ENDIF ELSE BEGIN

         PLOTS, lon, lat, Color=cgColor(color), NOCLIP=noclip, Thick=thick

      ENDELSE

      ; Need outlines with a fill?
      IF Keyword_Set(fill) AND Keyword_Set(outline) THEN $
         PLOTS, lon, lat, Color=cgColor(color), NOCLIP=noclip, Thick=thick

   ENDWHILE
   Free_Lun, lun

   ; Restore decomposition state.
   SetDecomposedState, thisState
   
   IF !D.NAME NE 'Z' THEN TVLCT, r, g, b
END
