; docformat = 'rst'
;
; NAME:
;   cgMapContinents
;
; PURPOSE:
;   Provides an object wrapper to the cgMAP_CONTINENTS or the cgMap_GSHHS commands.
;   Coyote Library routines are required.
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
;   Provides an object wrapper to the cgMAP_CONTINENTS or the cgMap_GSHHS commands.
;   Coyote Library routines are required.
;
; :Categories:
;    Graphics, Map Projections
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
; :History:
;    Written by David W. Fanning, 7 November 2011.
;    Added a BACKGROUND keyword. 31 Aug 2012. DWF.
;        
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------


;+--------------------------------------------------------------------------
;   The initialization routine for the cgMapContinents object. 
;
; :Params:
;    mapCoord: in, required, type=object
;       A map coordinate object that will set up a map coordinate data space.
;       Required to convert lat/lon values to projected meter space. A cgMap object.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. Note that a map projection command must be 
;        added to the window before this command is added to be effective.
;     background: in, optional, type=string
;        The name of the background color. A polygon of this color is drawn
;        in the map space set up by the map projection before continents and
;        other items are drawn.
;     coasts: in, optional, type=boolean, default=0
;        Set this keyword if you want coasts to be drawn. This keyword is ignored if using FILENAME.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;     continents: in, optional, type=boolean  
;        Set this keyword if you want continental outlines to be drawn. This will be
;        set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;        to zero. This keyword is ignored if using FILENAME.
;     filename: in, optional, type=string
;        The name of the GSHHS file to open. If a fully qualified file name is not provided,
;        the program will look for the GSHHS file using Find_Resource_File() function. Only if
;        this fails and a file is not found, will an error be issued. This keyword applies only
;        to GSHHS data files currently. If the GSHHS keyword is set, and this keyword is undefined,
;        then the filename will be set to "gshhs_i.b", unless the HIRES keyword is set, in which
;        case it will be set to "gshhs_h.b."
;     fill: in, optional, type=boolean, default=0
;        Set this keyword to draw filled polygons rather than outlines.
;     gshhs: in, optional, type=boolean, default=0
;        Set this keyword to indicate you want to draw continents with the GSHHS data set.
;        Using the FILENAME keyword automatically sets this keyword.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword to use the high-resolution data supplied with IDL for MAP_CONTINENTS.
;        This keyword is ignored if using FILENAME.
;     land_color: in, optional, type=string, default='tan8'         
;        The name of a color to be used for "land". Used when the FILL keyword is set.
;     level: in, optional, type=integer, default=2  
;        The polygon LEVEL. All polygons less than or equal to this value
;        are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;        This keyword applies only to GSHHS data sets.
;     linestyle:  in, optional, type=integer, default=0
;        Set to the type of linestyle in drawing outlines. Set solid lines by default.
;     minarea: in, optional, type=float, default=500 km^2
;        The minimum feature area. By default, 500 km^2. Polygons with areas less
;        than this are not drawn. This keyword applies only to GSHHS data sets.
;     outline: in, optional, type=boolean, default=0
;        Set this keyword to draw shorelines. Set by default if FILL=0. This keyword applies 
;        only to GSHHS data sets.
;     rivers: in, optional, type=boolean, default=0  
;        Set this keyword if you wish to draw rivers. This keyword is ignored if using FILENAME.
;     t3d: in, optional, type=boolean, default=0  
;        Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        This keyword is ignored if using FILENAME.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the lines that are drawn.
;     usa: in, optional, type=boolean, default=0  
;        Set this keyword if you wish do draw United States state boundaries. This keyword is 
;        ignored if using FILENAME.
;     water_color: in, optional, type=string, default='sky blue'
;        The name of the water color. This keyword applies only to GSHHS data sets.
;     zvalue: in, optional, type=float, default=0.0  
;        Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        This keyword is ignored if using FILENAME.
;---------------------------------------------------------------------------
FUNCTION cgMapContinents::INIT, mapCoord, $
    ADDCMD=addcmd, $
    BACKGROUND=background, $
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
    IF N_Elements(color) EQ 0 THEN color = 'opposite'
    IF N_Elements(land_color) EQ 0 THEN land_color = 'tan8'
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
    IF N_Elements(water_color) EQ 0 THEN water_color = 'sky blue'
    IF Keyword_Set(zvalue) EQ 0 THEN zvalue = 0.0
    
    ; If only the GSHHS keyword is set, suggest a filename.
    IF gshhs && (N_Elements(filename) EQ 0) THEN BEGIN
        IF hires THEN filename = 'gshhs_h.b' ELSE filename = 'gshhs_i.b'
    ENDIF
    
    ; Can you find the file? Is this a fully-qualified file path?
    IF N_Elements(filename) NE 0 THEN BEGIN
       gshhs = 1
       confirmedName = self -> Confirm_Filename(filename)
       IF confirmedName NE "" THEN filename = confirmedName ELSE $
            Message, 'Cannot locate the file ' + filename + '.'
    ENDIF
        
    IF N_Elements(background) EQ 0 THEN self.background = Ptr_New(/Allocate_Heap) ELSE self.background = Ptr_New(background)
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
    IF Obj_Valid(mapCoord) THEN self.map_object = mapCoord
        
    ; Make sure you have a valid map object at this point.
    IF ~Obj_Valid(self.map_object) THEN $
       Message, 'A valid map object is required to create a cgMapContinents object.'

    ; Need to add this command to a resizeable cgWindow?
    IF Keyword_Set(addcmd) THEN self -> AddCmd
   
    RETURN, 1
END 


;+--------------------------------------------------------------------------
;   Adds the object as a command (the DRAW method is called) in a cgWindow 
;   resizeable graphics window. 
;---------------------------------------------------------------------------
PRO cgMapContinents::AddCmd

   cgWindow, "Draw", self, /Method, /AddCmd
   
END 


;+--------------------------------------------------------------------------
;   Makes sure the filename exists. If it doesn't it will search for the
;   file using FIND_RESOURSE_FILE. 
;
; :Params:
;    filename: in, required, type=string
;       The name of the file to confirm.
;---------------------------------------------------------------------------
FUNCTION cgMapContinents::Confirm_Filename, filename

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(returnName) NE 0 THEN RETURN, returnName ELSE RETURN, ""
    ENDIF
    
    IF N_Elements(filename) EQ 0 THEN Message, 'A file name is required.'
    
    ; In case something goes wrong.
    returnName = filename

    ; Can you locate the file?
    found = File_Test(filename, /READ)
       
    ; If you can't find it, do a search in resource directories for it.
    IF ~found THEN BEGIN
       resourceFile = Find_Resource_File(filename, SUCCESS=success)
       IF success THEN BEGIN
           returnName = resourceFile
       ENDIF ELSE BEGIN
           Message, 'Cannot locate the file: ' + filename + '.', /Informational
           returnName = ""
       ENDELSE
    ENDIF ELSE BEGIN
       
        ; Is this a fully-qualified path to the file?
        IF StrUpCase(returnName) EQ StrUpCase(File_Basename(returnName)) THEN BEGIN
           CD, CURRENT=thisDir
            returnName = Filepath(ROOT_DIR=thisDir, returnName)
        ENDIF
    ENDELSE

    ; Return the filename.
    RETURN, returnName
       
END


;+--------------------------------------------------------------------------
;   Draw the continental outlines in a graphics window.
;
;---------------------------------------------------------------------------
PRO cgMapContinents::Draw

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
        cgMap_GSHHS, gshhsFileName, $ 
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
            cgMAP_CONTINENTS, $
                BACKGROUND=*self.background, $
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
            cgMAP_CONTINENTS, $
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
            cgMAP_CONTINENTS, $
                BACKGROUND=*self.background, $
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
    
END 


;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. 
;
; :Keywords:
;     background: in, optional, type=string
;        The name of the background color. 
;     coasts: in, optional, type=boolean, default=0
;        Set this keyword if you want coasts to be drawn. This keyword is ignored if using FILENAME.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;     continents: in, optional, type=boolean  
;        Set this keyword if you want continental outlines to be drawn. This will be
;        set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;        to zero. This keyword is ignored if using FILENAME.
;     filename: in, optional, type=string
;        The name of the GSHHS file to open. If a fully qualified file name is not provided,
;        the program will look for the GSHHS file using Find_Resource_File() function. Only if
;        this fails and a file is not found, will an error be issued. This keyword applies only
;        to GSHHS data files currently. If the GSHHS keyword is set, and this keyword is undefined,
;        then the filename will be set to "gshhs_i.b", unless the HIRES keyword is set, in which
;        case it will be set to "gshhs_h.b."
;     fill: in, optional, type=boolean, default=0
;        Set this keyword to draw filled polygons rather than outlines.
;     gshhs: in, optional, type=boolean, default=0
;        Set this keyword to indicate you want to draw continents with the GSHHS data set.
;        Using the FILENAME keyword automatically sets this keyword.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword to use the high-resolution data supplied with IDL for MAP_CONTINENTS.
;        This keyword is ignored if using FILENAME.
;     land_color: in, optional, type=string, default='tan8'         
;        The name of a color to be used for "land". Used when the FILL keyword is set.
;     level: in, optional, type=integer, default=2  
;        The polygon LEVEL. All polygons less than or equal to this value
;        are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;        This keyword applies only to GSHHS data sets.
;     linestyle:  in, optional, type=integer, default=0
;        Set to the type of linestyle in drawing outlines. Set solid lines by default.
;     minarea: in, optional, type=float, default=500 km^2
;        The minimum feature area. By default, 500 km^2. Polygons with areas less
;        than this are not drawn. This keyword applies only to GSHHS data sets.
;     outline: in, optional, type=boolean, default=0
;        Set this keyword to draw shorelines. Set by default if FILL=0. This keyword applies 
;        only to GSHHS data sets.
;     rivers: in, optional, type=boolean, default=0  
;        Set this keyword if you wish to draw rivers. This keyword is ignored if using FILENAME.
;     t3d: in, optional, type=boolean, default=0  
;        Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        This keyword is ignored if using FILENAME.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the lines that are drawn.
;     usa: in, optional, type=boolean, default=0  
;        Set this keyword if you wish do draw United States state boundaries. This keyword is 
;        ignored if using FILENAME.
;     water_color: in, optional, type=string, default='sky blue'
;        The name of the water color. This keyword applies only to GSHHS data sets.
;     zvalue: in, optional, type=float, default=0.0  
;        Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        This keyword is ignored if using FILENAME.
;---------------------------------------------------------------------------
PRO cgMapContinents::GetProperty, $
    BACKGROUND=background, $
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
    IF N_Elements(*self.background) NE 0 THEN background = *self.background
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
    
END 



;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   
; :Keywords:
;     background: in, optional, type=string
;        The name of the background color. A polygon of this color is drawn
;        in the map space set up by the map projection before continents and
;        other items are drawn.
;     coasts: in, optional, type=boolean, default=0
;        Set this keyword if you want coasts to be drawn. This keyword is ignored if using FILENAME.
;     color: in, optional, type=string, default='opposite'
;        The name of the drawing color for the program.
;     continents: in, optional, type=boolean  
;        Set this keyword if you want continental outlines to be drawn. This will be
;        set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;        to zero. This keyword is ignored if using FILENAME.
;     filename: in, optional, type=string
;        The name of the GSHHS file to open. If a fully qualified file name is not provided,
;        the program will look for the GSHHS file using Find_Resource_File() function. Only if
;        this fails and a file is not found, will an error be issued. This keyword applies only
;        to GSHHS data files currently. If the GSHHS keyword is set, and this keyword is undefined,
;        then the filename will be set to "gshhs_i.b", unless the HIRES keyword is set, in which
;        case it will be set to "gshhs_h.b."
;     fill: in, optional, type=boolean, default=0
;        Set this keyword to draw filled polygons rather than outlines.
;     gshhs: in, optional, type=boolean, default=0
;        Set this keyword to indicate you want to draw continents with the GSHHS data set.
;        Using the FILENAME keyword automatically sets this keyword.
;     hires: in, optional, type=boolean, default=0
;        Set this keyword to use the high-resolution data supplied with IDL for MAP_CONTINENTS.
;        This keyword is ignored if using FILENAME.
;     land_color: in, optional, type=string, default='tan8'         
;        The name of a color to be used for "land". Used when the FILL keyword is set.
;     level: in, optional, type=integer, default=2  
;        The polygon LEVEL. All polygons less than or equal to this value
;        are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;        This keyword applies only to GSHHS data sets.
;     linestyle:  in, optional, type=integer, default=0
;        Set to the type of linestyle in drawing outlines. Set solid lines by default.
;     minarea: in, optional, type=float, default=500 km^2
;        The minimum feature area. By default, 500 km^2. Polygons with areas less
;        than this are not drawn. This keyword applies only to GSHHS data sets.
;     outline: in, optional, type=boolean, default=0
;        Set this keyword to draw shorelines. Set by default if FILL=0. This keyword applies 
;        only to GSHHS data sets.
;     rivers: in, optional, type=boolean, default=0  
;        Set this keyword if you wish to draw rivers. This keyword is ignored if using FILENAME.
;     t3d: in, optional, type=boolean, default=0  
;        Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        This keyword is ignored if using FILENAME.
;     thick: in, optional, type=integer, default=1
;        Set this keyword to the thickness of the lines that are drawn.
;     usa: in, optional, type=boolean, default=0  
;        Set this keyword if you wish do draw United States state boundaries. This keyword is 
;        ignored if using FILENAME.
;     water_color: in, optional, type=string, default='sky blue'
;        The name of the water color. This keyword applies only to GSHHS data sets.
;     zvalue: in, optional, type=float, default=0.0  
;        Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        This keyword is ignored if using FILENAME.
;---------------------------------------------------------------------------
PRO cgMapContinents::SetProperty, $
    BACKGROUND=background, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    DRAW=draw, $
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
    IF N_Elements(background) NE 0 THEN *self.background = background
    IF N_Elements(coasts) NE 0 THEN self.coasts = Keyword_Set(coasts)
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(continents) NE 0 THEN self.continents = Keyword_Set(continents)
    IF N_Elements(filename) NE 0 THEN BEGIN
       newfilename = self -> Confirm_Filename(filename)
       IF newfilename NE "" THEN self.filename = newfilename
    ENDIF
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
    
    ; Need a draw?
    IF Keyword_Set(draw) THEN self -> Draw

END 


;+--------------------------------------------------------------------------
;   This method is the clean-up method for the object.
;---------------------------------------------------------------------------
PRO cgMapContinents::CLEANUP

    self -> cgContainer::CLEANUP
    Ptr_Free, self.background    
    
END 


;+--------------------------------------------------------------------------
;   This is the class definition module. Structures used to manipulate
;   map projection information are also created here.
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;---------------------------------------------------------------------------
PRO cgMapContinents__DEFINE, class

    class = { cgMapContinents, $
              background: Ptr_New(), $     ; The name of the background color.
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
 