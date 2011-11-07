;+
; NAME:
;       CGMAPCONTINENTS__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for either cg_MAP_CONTINENTS or MAP_GSHHS_SHORELINE.
;       It provides a simple way to allow map overlays on images which use a cgMAP
;       object to set up the map projection space. A map coordinate space must be in
;       effect at the time the Draw method of this object is used. Map outlines can be
;       provided from built-in IDL continental databases, or the GSHHS Shoreline
;       data base can be used. (For information on the GSHHS Shoreline data base, see
;       http://www.idlcoyote.com/map_tips/gshhs.html.)
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
;       Graphics
;
; CALLING SEQUENCE:
;
;       mapCoord = Obj_New('cgMap', 111, CENTER_LON=0, CENTER_LAT=90)
;       outlineObject = Obj_New('cgMapContinents', MAP_OBJECT=mapCoord)
;       mapCoord -> Draw, /Erase
;       outlineObject -> Draw
;       Obj_Destroy, mapCoord, outlineObject
;       
; AUGUMENTS:
; 
;       parent:        The parent object. Optional.
;
;       mapCoordObj:   A map coordinate object which can return a map structure for converting coordinates
;                      to/from XY coordinates to lon/lat coordinates. Typically, a MAPCOORD object. An
;                      alternative way of specifying a map coordinate object is to use the MAP_OBJECT
;                      keyword. But don't do both. Note, this object is *not* destroyed when the CGMAPCONTINENTS
;                      object is destroyed. You are responsible for destroying the map coordinate object.
;                      A map coordinate object is REQUIRED. So, if you don't specify a parent, use the
;                      MAP_OBJECT keyword to pass this object into the program.
;
; COMMON KEYWORDS (used with either MAP_CONTINENTS of MAP_GSHHS_SHORELINE):
;
;       COLOR:         The name of a color (used with cgColor) to draw the output in.
;       
;       FILL:          Set this keyword to create a filled polygon output, rather than an outline.
;       
;       HIRES:         If this keyword is set, the high resolution dataset supplied with IDL is used
;                      with MAP_CONTINENTS. If this keyword is set, and the GSHHS keyword is set, and
;                      the FILENAME keyword is NOT used, then the default filename is "gshhs_h.b". Note
;                      that the high resolution dataset must be installed to be used.
;                    
;      MAP_OBJECT:      A MAPCOORD object or equivalent which had the ability to provide a map
;                       structure with a GetMapStructure method. Don't use this keyword if you have
;                       passed a map coordinate object as a positional parameter. This keyword should
;                       be used ONLY if you are not specifying a parent parameter.
;      
; MAP_CONTINENTS KEYWORDS (apply only if you are using MAP_CONTINENTS to draw outlines):
; 
;        COASTS:       Set this keyword if you want coasts to be drawn.
;        
;        CONTINENTS:   Set this keyword if you want continental outlines to be drawn. This will be
;                      set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;                      to zero.
;                      
;        LINESTYLE:    Set to the type of linestyle in drawing outlines. Set to 0 or solid lines by default.
;        
;        RIVERS:       Set this keyword if you wish to draw rivers.
;        
;        T3D:          Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        
;        USA:          Set this keyword if you wish do draw United States state boundaries.
;        
;        ZVALUE:       Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        
; MAP_GSHHS_SHORELINE KEYWORDS (apply only if you are using MAP_GSHHS_SHORELINE to draw outlines):
; 
;        FILENAME:     The root name of the GSHHS file to open. By default, "gshhs_l.b" unless the HIRES
;                      keyword is selected, in which case it will be "gshhs_h.b". The GSHHS file must be
;                      in a "resource" directory or in one of the directories on your IDL path, or it
;                      must be an absolute path to the file.
;                      
;        GSHHS:        Set this keyword to use the GSHHS Shoreline data. The default is to use IDL's
;                      built-in database.  
;                      
;        LAND_COLOR:   The name of a color to be used for "land". Used with filled polygons 
;                      (e.g., the FILL keyword). By default, 'INDIAN RED'.
;        
;        LEVEL:        The polygon LEVEL. All polygons less than or equal to this value
;                      are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;                      By default, 2 (land and lake outlines).
;
;        MINAREA:      The minimum feature area. By default, 500 km^2. Polygons with areas less
;                      than this are not drawn.
;
;        OUTLINE:      Set this keyword to draw shorelines. Set by default if FILL=0.
;
;        WATER_COLOR:  The name of the water color. By default, "SKY BLUE".
;
; DEPENDENCIES:
;
;       The following programs (at least) are required from the Coyote Library:
;
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/find_resource_file.pro
;                     http://www.dfanning.com/programs/cgColor.pro
;                     http://www.dfanning.com/programs/map_gshhs_shoreline.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 3 January 2009.
;       Fixed a problem in setting MAP_OBJECT in SetProperty method. 12 June 2009. DWF.
;       Fixed a problem when drawing filled outlines using IDL's map database when the
;           LAND_COLOR is different from the outline COLOR. 30 July 2009. DWF.
;       Previous problem introduced another. If LAND_COLOR is undefined, it is now set
;           to the same color as COLOR. 10 August 2009. DWF.
;       Circular parent references when passed a MAP_OBJECT was fixed, preventing memory
;          leakage. 30 August 2009. DWF.
;       Removed the MAP_STRUCTURE keyword, which caused MASSIVE problems and added a mapCoordObj
;          parameter. 9 November 2009. DWF.
;        Made a change that allows the GSHHS filename to be an absolute path to the
;           gshhs_*.b file. 4 June 2010. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO cgMapContinents::AddCmd, REPLACE=replace

   currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
   IF wincnt EQ 0 THEN cgWindow
   IF Keyword_Set(replace) $
      THEN cgWindow, "Draw", self, /Method, /ReplaceCmd $ ; Replace all commands in the window
      ELSE cgWindow, "Draw", self, /Method, /AddCmd       ; Add this command to the window.
   
END ;--------------------------------------------------------------------------


PRO cgMapContinents::Draw, _EXTRA=extrakeywords

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.map_object) THEN BEGIN
        mapStruct = self.map_object -> GetMapStruct() 
    ENDIF
    
    ; Do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENT=currentState
    
    ; Draw the appropriate map outline.
    IF self.gshhs THEN BEGIN
        rootName = File_Basename(self.filename)
        IF StrLowCase(rootName) EQ StrLowCase(self.filename) THEN BEGIN
            gshhsFileName = Find_Resource_File(rootName, SUCCESS=success)
        ENDIF ELSE BEGIN
            gshhsFileName = self.filename
            IF self.filename EQ "" THEN success = 0 ELSE success = 1
        ENDELSE
        IF ~success THEN Message, 'Cannot file requested GSHHS Shoreline Data File.'
        Map_GSHHS_Shoreline, gshhsFileName, $ 
           COLOR=self.color, $                    
           FILL=self.fill, $                      
           LAND_COLOR=self.land_color, $          
           LEVEL=self.level, $                    
           MAP_PROJECTION=mapStruct, $  
           MINAREA=self.minarea, $                
           OUTLINE=self.outline, $                
           WATER_COLOR=self.water_color
    ENDIF ELSE BEGIN
        IF self.fill AND (self.color NE self.land_color) THEN BEGIN
            cg_MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=self.land_color, $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=1, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
            cg_MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=self.color, $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=0, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
        ENDIF ELSE BEGIN
            cg_MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=self.color, $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=self.fill, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
         ENDELSE
    ENDELSE
    SetDecomposedState, currentState
    
END ; --------------------------------------------------------------------------------------------


PRO cgMapContinents::GetProperty, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
    ZVALUE=zvalue, $
    _REF_EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get the properties.
    coasts = self.coasts
    color = self.color 
    continents = self.continents
    filename = self.filename
    fill = self.fill
    gshhs = self.gshhs
    hires = self.hires 
    land_color = self.land_color
    level = self.level
    linestyle = self.linestyle 
    map_object = self.map_object
    IF Arg_Present(map_structure) THEN map_structure = self.map_object -> GetMapStructure()
    minarea = self.minarea
    outline = self.outline
    thick = self.thick
    rivers = self. rivers 
    usa = self.usa
    t3d = self.t3d 
    water_color = self.water_color
    zvalue = self.zvalue 
    
    IF N_Elements(extra) NE 0 THEN self -> cgContainer::GetProperty, _EXTRA=extra
    
END ; --------------------------------------------------------------------------------------------



PRO cgMapContinents::SetProperty, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
    ZVALUE=zvalue, $
    _EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set the properties.
    IF N_Elements(coasts) NE 0 THEN self.coasts = Keyword_Set(coasts)
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(continents) NE 0 THEN self.continents = Keyword_Set(continents)
    IF N_Elements(filename) NE 0 THEN self.filename = filename
    IF N_Elements(fill) NE 0 THEN self.fill = Keyword_Set(fill)
    IF N_Elements(gshhs) NE 0 THEN self.gshhs = Keyword_Set(gshhs)    
    IF N_Elements(hires) NE 0 THEN self.hires = Keyword_Set(hires)
    IF N_Elements(land_color) NE 0 THEN self.land_color = land_color
    IF N_Elements(level) NE 0 THEN self.level = level
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(map_object) NE 0 THEN self.map_object = map_object
    IF N_Elements(minarea) NE 0 THEN self.minarea = minarea
    IF N_Elements(outline) NE 0 THEN self.outline = outline
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(rivers) NE 0 THEN self. rivers = Keyword_Set(rivers)
    IF N_Elements(usa) NE 0 THEN self.usa = Keyword_Set(usa)
    IF N_Elements(t3d) NE 0 THEN self.t3d = Keyword_Set(t3d)
    IF N_Elements(water_color) NE 0 THEN self.water_color = water_color
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue

    IF N_Elements(extra) NE 0 THEN self -> cgContainer::SetProperty, _EXTRA=extra

END ; --------------------------------------------------------------------------------------------


PRO cgMapContinents::CLEANUP

    self -> cgContainer::CLEANUP    
    
END ; --------------------------------------------------------------------------------------------


FUNCTION cgMapContinents::INIT, mapCoordObj, $
    ADDCMD=addcmd, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
    WINDOW=window, $
    ZVALUE=zvalue, $
    _EXTRA=extra
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF

    ; Initialize superclass object,
     ok = self -> cgContainer::INIT( _EXTRA=extra ) 
     IF ~ok THEN RETURN, 0

    ; Default values.
    IF (Keyword_Set(coasts) AND Keyword_Set(countries) AND Keyword_Set(rivers) $
        AND Keyword_Set(usa)) EQ 0 THEN continents = 1
    coasts = Keyword_Set(coasts)
    countries = Keyword_Set(countries)
    IF Keyword_Set(color) EQ 0 THEN color = 'white'
    IF N_Elements(land_color) EQ 0 THEN land_color = color
    continents = Keyword_Set(continents)
    fill = Keyword_Set(fill)
    gshhs = Keyword_set(gshhs)
    hires = Keyword_Set(hires)    
    IF N_Elements(level) EQ 0 THEN level = 2 ELSE level = 1 > level < 4
    IF Keyword_Set(linestyle) EQ 0 THEN linestyle = 0
    IF N_Elements(minArea) EQ 0 THEN minArea = 500.0 ; square kilometers.
    IF Keyword_Set(thick) EQ 0 THEN thick = 1
    rivers = Keyword_Set(rivers)
    usa = Keyword_Set(usa)
    t3d = Keyword_Set(t3d)
    IF N_Elements(water_color) EQ 0 THEN water_color = 'SKY BLUE'
    IF Keyword_Set(zvalue) EQ 0 THEN zvalue = 0.0
    
    IF gshhs AND N_Elements(filename) EQ 0 THEN BEGIN
        IF hires THEN filename = 'gshhs_h.b' ELSE filename = 'gshhs_l.b'
    ENDIF
    
    self.coasts = coasts
    self.color = color
    self.continents = continents
    self.countries = countries
    self.fill = fill
    IF N_Elements(filename) NE 0 THEN self.filename = filename
    self.hires = hires
    self.gshhs = gshhs
    self.land_color = land_color
    self.level = level
    self.linestyle = linestyle
    self.minArea = minarea
    self. rivers = rivers
    self.t3d = t3d
    self.thick = thick
    self.usa = usa
    self.water_color = water_color
    self.zvalue = zvalue
    
    ; If a map object exists, simply put it in the right place. Do NOT
    ; make yourself a parent, because this object might contain YOU!
    IF Obj_Valid(mapCoordObj) THEN self.map_object = mapCoordObj
    IF Obj_Valid(map_object) AND Obj_Valid(self.map_object) THEN $
        Message, 'Cannot use both mapCoordObj parameter and MAP_OBJECT keyword.'
    IF Obj_Valid(map_object) AND ~Obj_Valid(self.map_object) THEN $
        self.map_object = map_object
        
    ; Make sure you have a valid map object at this point.
    IF ~Obj_Valid(self.map_object) THEN Message, 'A valid map object is required to create a CGMAPCONTINENTS object.'

    ; Need to add this command to a resizeable cgWindow?
    IF Keyword_Set(window) THEN self -> AddCmd, /REPLACE
    IF Keyword_Set(addcmd) THEN self -> AddCmd
   
    RETURN, 1
END ; --------------------------------------------------------------------------------------------


PRO cgMapContinents__DEFINE, class

    class = { cgMapContinents, $
              coasts: 0B, $                ; A flag that indicates coasts, island, lakes are drawn.
              continents: 0B, $            ; A flat that indicates continental outlines should be drawn.
              color: "", $                 ; The name of the color to draw outlines in.
              countries: 0B, $             ; A flag to indicate continental boundaries should be drawn.
              fill: 0B, $                  ; Set to do polygon fill instead of outline.
              filename: "", $              ; The root name of a GSHHS file to read from, eg. gshhs_h.b.
              hires: 0B, $                 ; Set to use high resolution maps rather than low resoluton maps.
              gshhs: 0B, $                 ; A flag to signify that a GSHHS file should be used.
              map_object: Obj_New(), $
              land_color: "", $            ; The name of the land color (for color fill).
              level: 0, $                  ; The level of GSHHS file to draw.
              linestyle: 0, $              ; The linestyle of the outline. By default, solid.
              minarea: 0.0D, $             ; Polygons less than the minimum area are not drawn.
              outline: 0B, $               ; Set if you want coastal outlines drawn.
              rivers: 0B, $                ; Set this keyword to draw rivers.
              t3d: 0B, $                   ; Set this keyword to draw in 3D space using T3D matrix.
              thick: 0, $                  ; The thickness of the outline
              usa: 0B, $                   ; Set this keyword to draw USA boundaries.
              water_color: "", $           ; The name of the water color.
              zvalue: 0.0D, $              ; The Z value. Default is 0.
              INHERITS cgContainer $
             }
            
END ; --------------------------------------------------------------------------------------------
 