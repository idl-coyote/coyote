;*****************************************************************************************************
;+
; NAME:
;       cgMAP__DEFINE
;
; PURPOSE:
;
;       The purpose of this object is to set up a map coordinate space for
;       for other objects. The program assumes you will use MAP_PROJ_INIT
;       to set up the map structure that is the basis for the map projection
;       space. 
;       Switch UTM datum from WGS84 to WALBECK to avoid UTM projection bug in all versions
;            of IDL prior to IDL 8.2, when it is suppose to be fixed. For more information,
;            see this article: http://www.idlcoyote.com/map_tips/utmwrong.php. 31 Oct 2011. DWF.
;-
PRO cgMap::AddOverlay, overLayObject

   ; Required parameter must be a valid object with a DRAW method.
   IF N_Elements(overLayObject) EQ 0 THEN $
      Message, 'A map overlay object is a required parameter.'
   IF ~Obj_Valid(overLayObject) THEN Message, 'A valid overlay object is required.'
   IF Float(!Version.Release) GT 6.4 THEN BEGIN
        hasMethod = Call_Function('Obj_HasMethod', overLayObject, 'DRAW')
        IF ~hasMethod THEN Message, 'The overlay object must have a DRAW method.'
   ENDIF
   
   ; Add the object to the overlay container.
   self._cg_overlays -> Add, overLayObject   
   
END ;--------------------------------------------------------------------------


PRO cgMap::Draw, _EXTRA=extra
 
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
    
    ; Do you need to erase in the background color?
    IF self._cg_erase THEN cgErase, Color=self._cg_background
   
   ; Draw the map data coordinate system.
    mapStruct = self -> SetMapProjection()
    self -> cgCoord::Draw, _EXTRA=extra
    
    ; Draw overlays?
    count = self._cg_overlays -> Count()
    IF count GT 0 THEN BEGIN
    
        ; Get the overlay objects out of the overlay container.
        FOR j=0,count-1 DO BEGIN
            thisOverlay = self._cg_overlays -> Get(POSITION=j)
            IF Obj_Valid(thisOverlay) THEN thisOverlay -> Draw
        ENDFOR
    ENDIF
    
    ; Draw a border around the map?
    IF ~Keyword_Set(self._cg_noborder) THEN BEGIN
        p = self._cg_position
        cgPlots, [p[0],p[0],p[2],p[2],p[0]], [p[1],p[3],p[3],p[1],p[1]], $
            /NORMAL, COLOR=self._cg_color
    ENDIF
    
    ; Draw a title?
    IF self._cg_title NE "" THEN BEGIN
       p = self._cg_position
       px = (p[2]-p[0])/2.0 + p[0]
       py = p[3]+ (0.025 * (512.0/!D.Y_Size))
       cgText, px, py, /Normal, Alignment=0.5, self._cg_title
    ENDIF
    
END ;--------------------------------------------------------------------------


;*****************************************************************************************************
;+
; NAME:
;       cgMap::GETMAPSTRUCTURE
;
; PURPOSE:
;
;       This method allows the user to obtain the map projection structure.
;
; SYNTAX:
;
;       mapStruct = theObject -> GetMapStructure()
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
FUNCTION cgMap::GetMapStruct
    RETURN, self -> SetMapProjection()
END ;--------------------------------------------------------------------------


PRO cgMap::GetProperty, $
    BACKGROUND=background, $
    COLOR=color, $
    CONTINENETS=continents, $
    OVERLAYS=overlays, $
    ERASE=erase, $
    GRID=grid_object, $
    LATLON_RANGES=latlon_ranges, $
    MAP_STRUCTURE=mapStruct, $
    OUTLINE_OBJECT=outline_object, $
    POSITION=position, $
    MAP_OVERLAY=map_overlay, $
    MAP_PROJ_KEYWORDS=map_proj_keywords, $
    MAP_PROJECTION=map_projection, $
    NOBORDER=noborder, $
    OVERLAY_POSITION=overlay_position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    DATUM=datum, $
    ELLIPSOID=ellipsoid, $
    SPHERE_RADIUS=sphere_radius, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    LIMIT=limit, $
    RADIANS=radians, $
    ZONE=zone, $
    _REF_EXTRA=extraKeywords

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
   
   ; Make sure the map structure is up to date by always calculating it in real-time.
   mapStruct = self -> SetMapProjection() 
   
   IF Arg_Present(overlays) THEN overlays = self._cg_draw_overlays -> Get(/ALL)
   position = self._cg_position
   IF Arg_Present(xrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            xrange = Reform(llcoords[0,*])
      ENDIF ELSE xrange = self._cg_xrange
   ENDIF
   IF Arg_Present(yrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._cg_xrange, self._cg_yrange, MAP_STRUCTURE=mapStruct)
            yrange = Reform(llcoords[1,*])
      ENDIF ELSE yrange = self._cg_yrange
   ENDIF
   IF Arg_Present(grid_object) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN grid_object = self._cg_overlays-> Get(Position=1) $
            ELSE grid_object = self._cg_overlays-> Get(Position=overlay_position)
   ENDIF
   IF Arg_Present(outline_object) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN outline_object = self._cg_overlays -> Get(Position=0) $
            ELSE outline_object = self._cg_overlays -> Get(Position=overlay_position)
   ENDIF
   IF Arg_Present(map_overlay) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN map_overlay = self._cg_overlays-> Get(/ALL) $
            ELSE map_overlay = self._cg_overlays -> Get(Position=overlay_position)
   ENDIF
   
   ; Other keywords.
   background = self._cg_background
   color = self._cg_color
   center_latitude = self._cg_center_latitude
   center_longitude = self._cg_center_longitude
   erase = self._cg_erase
   IF N_Elements(*self._cg_limit) NE 0 THEN limit = *self._cg_limit
   noborder = self._cg_noborder
   map_projection = self._cg_thisProjection.name
   IF Arg_Present(map_proj_keywords) THEN BEGIN
        IF Ptr_Valid(self._cg_map_projection_keywords) THEN BEGIN
            IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN $
                map_proj_keywords = *self._cg_map_projection_keywords
        ENDIF
   ENDIF
   datum = self._cg_thisDatum.name
   ellipsoid = self._cg_thisDatum.name
   sphere_radius = self._cg_thisDatum.semimajor_axis
   semimajor_axis = self._cg_thisDatum.semimajor_axis
   semiminor_axis = self._cg_thisDatum.semiminor_axis
   zone = self._cg_zone
   
   ; Superclass keywords.
   IF (N_ELEMENTS(extraKeywords) GT 0) THEN self -> cgCOORD::GetProperty, _EXTRA=extraKeywords

END ;--------------------------------------------------------------------------


;*****************************************************************************************************
;+
; NAME:
;       cgMap::MapInfo
;
; PURPOSE:
;
;       The purpose of this routine is to return information about the current map
;       projection in an IDL structure variable. Fields of the structure will reflect
;       values that are used in MAP_PROJ_INIT to create a map structure.
;
; SYNTAX:
;
;       information = object -> MapInfo()
;
; ARGUMENTS:
;
;       None.
;       
; KEYWORDS: 
;       
;       None.
;
;-
;*****************************************************************************************************
FUNCTION cgMap::MapInfo

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
    ENDIF
   
   
   map_keywords = Create_Struct( $
       'projection', self._cg_thisProjection.name, $
       'datum', self._cg_thisDatum.name, $
       'gctp', 1, $
       'center_latitude', self._cg_center_latitude, $
       'center_longitude', self._cg_center_longitude )
   IF N_Elements(*self._cg_limit) NE 0 THEN map_keywords = Create_Struct(map_keywords, 'limit', *self._cg_limit)
   IF self._cg_thisProjection.sphereOnly THEN BEGIN
       map_keywords = Create_Struct(map_keywords, 'sphere_radius', self._cg_thisDatum.semimajor_axis)
   ENDIF ELSE BEGIN
       map_keywords = Create_Struct(map_keywords, $
            'semimajor_axis', self._cg_thisDatum.semimajor_axis, $
            'semiminor_axis', self._cg_thisDatum.semiminor_axis)        
   ENDELSE
   IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN BEGIN
        keywords = *self._cg_map_projection_keywords
        fields = Tag_Names(keywords)
        FOR j=0,N_Elements(fields) DO BEGIN
           map_keywords = Create_Struct(map_keywords, fields[j], keywords[j])
        ENDFOR
   ENDIF   
   RETURN, map_keywords
   
END ;--------------------------------------------------------------------------


PRO cgMap::SetProperty, $
    BACKGROUND=background, $
    COLOR=color, $
    CONTINENTS=continents, $
    DRAW_OVERLAYS=draw_overlays, $
    ERASE=erase, $
    GRID=grid, $
    LATLON_RANGES=latlon_ranges, $
    PARENT=parent, $
    POSITION=position, $
    NOBORDER=noborder, $
    MAP_OVERLAY=map_overlay, $
    MAP_PROJ_KEYWORDS=map_proj_keywords, $
    MAP_PROJECTION=map_projection, $
    OVERLAY_POSITION=overlay_position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    DATUM=datum, $
    ELLIPSOID=ellipsoid, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    LIMIT=limit, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    _EXTRA=extraKeywords
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
    ENDIF
      
   ; Are we changing the map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        projections = *self._cg_theProjections
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) THEN BEGIN
            index = Where(projections.index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF
        self._cg_thisProjection = projections[index]
   ENDIF
   
   ; Are we changing the map datum.
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > datum < 19]
        self._cg_thisDatum = thisDatum
   ENDIF
   
   ; Are we changing the map ellipsoid.
   IF N_Elements(ellipsoid) NE 0 THEN BEGIN
        IF Size(ellipsoid, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(ellipsoid))
            IF index[0] EQ -1 THEN Message, 'Cannot find ellipsoid ' + ellipsoid + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > ellipsoid < 19]
        self._cg_thisDatum = thisDatum
   ENDIF

   ; Are there map projection keywords to deal with?
   IF N_Elements(map_proj_keywords) NE 0 THEN BEGIN
   
        ; Make the pointer a valid pointer, if necessary.
        IF ~Ptr_Valid(self._cg_map_projection_keywords) THEN self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        
        ; Is there a NULL field in the current structure that means erase what is currently in the pointer?
        index = Where(Tag_Names(map_proj_keywords) EQ 'NULL', count)
        IF count GT 0 THEN BEGIN
            IF map_proj_keywords.(index) EQ 1 THEN self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        
        ; Add these fields to the structure, or modify the tag value if it is already present.
        IF N_Elements(*self._cg_map_projection_keywords) GT 0 THEN BEGIN
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               index = Where(Tag_Names(*self._cg_map_projection_keywords) EQ thisTag, count)
               IF count GT 0 THEN BEGIN
                   (*self._cg_map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   *self._cg_map_projection_keywords = Create_Struct(*self._cg_map_projection_keywords, thisTag, map_proj_keywords.(j))
               ENDELSE
            ENDFOR
        ENDIF ELSE BEGIN
            
            ; Add all the tags, except for NULL tags
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               IF thisTag EQ 'NULL' THEN Continue
               IF N_Elements(*self._cg_map_projection_keywords) EQ 0 THEN BEGIN
                    count = 0
               ENDIF ELSE BEGIN
                    index = Where(Tag_Names(*self._cg_map_projection_keywords) EQ thisTag, count)
               ENDELSE
               IF count GT 0 THEN BEGIN
                   (*self._cg_map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   IF N_Elements(*self._cg_map_projection_keywords) EQ 0 THEN BEGIN
                        *self._cg_map_projection_keywords = Create_Struct(thisTag, map_proj_keywords.(j))
                   ENDIF ELSE BEGIN
                        *self._cg_map_projection_keywords = Create_Struct(*self._cg_map_projection_keywords, $
                            thisTag, map_proj_keywords.(j))
                   ENDELSE
               ENDELSE
            ENDFOR
        ENDELSE
        
        ; For debugging purposes.
        ;Help, *self._cg_map_projection_keywords, /Structure
   ENDIF
   
   IF N_Elements(background) NE 0 THEN self._cg_background = background
   IF N_Elements(color) NE 0 THEN self._cg_color = color
   IF N_Elements(center_latitude) NE 0 THEN self._cg_center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self._cg_center_longitude = center_longitude
   IF N_Elements(erase) NE 0 THEN self._cg_erase = Keyword_Set(erase)
   IF N_Elements(noborder) NE 0 THEN self._cg_noborder = Keyword_Set(noborder)
   
   ; If you change the limit, you really also need to change the XRANGE and YRANGE.
   changedLimit = 0
   IF N_Elements(limit) NE 0 THEN BEGIN
        *self._cg_limit = limit
        changedLimit = 1
   ENDIF
   IF N_Elements(draw_overlays) NE 0 THEN self._cg_draw_overlays = Keyword_Set(draw_overlays)
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self._cg_thisDatum.semimajor_axis = sphere_radius
      self._cg_thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self._cg_thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self._cg_thisDatum.semiminor_axis = semiminor_axis
   
   ; Make sure the map structure is up to date.
   map_structure = self -> SetMapProjection() 

   IF N_Elements(parent) NE 0 THEN self -> cgCOORD::SetProperty, PARENT=parent
   IF N_Elements(position) NE 0 THEN self -> cgCOORD::SetProperty, POSITION=position
   IF N_Elements(xrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward(xrange, [-5000,5000], MAP_STRUCTURE=map_structure)
        xrange = Reform(uvcoords[0,*])   
      ENDIF
      self -> cgCOORD::SetProperty, XRANGE=xrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            xrange = map_structure.uv_box[[0,2]]
            self -> cgCOORD::SetProperty, XRANGE=xrange
      ENDIF
   ENDELSE
   IF N_Elements(yrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward([-5000,5000], yrange, MAP_STRUCTURE=map_structure)
        yrange = Reform(uvcoords[1,*])     
      ENDIF
      self -> cgCOORD::SetProperty, YRANGE=yrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            yrange = map_structure.uv_box[[1,3]]
            self -> cgCOORD::SetProperty, YRANGE=yrange
      ENDIF
   ENDELSE
   IF N_Elements(outline_object) NE 0 THEN BEGIN
   
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 0 ELSE thisPosition = overlay_position
        
        ; Parent will be added to object in SetOverlay method.
        self -> SetOverlay, outline_object, thisPosition, /OVERWRITE
                
   ENDIF 
   IF N_Elements(grid_object) NE 0 THEN BEGIN
   
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 1 ELSE thisPosition = overlay_position

        ; Parent will be added to object in SetOverlay method.
        self -> SetOverlay, grid_object, thisPosition, /OVERWRITE
        
   ENDIF 
   IF N_Elements(map_overlay) NE 0 THEN BEGIN
   
        count = N_Elements(map_overlay)
        IF N_Elements(overlay_position) EQ 0 THEN BEGIN
            FOR j=0,count-1 DO BEGIN
                thisOverlay = map_overlay[j]
                IF Obj_Valid(thisOverlay) THEN self -> SetOverlay, thisOverlay
            ENDFOR
        ENDIF ELSE BEGIN
            FOR j=0,count-1 DO BEGIN
                self -> SetOverlay, map_overlay[j], overlay_position[j]
            ENDFOR
        ENDELSE
   ENDIF
   
   IF (N_ELEMENTS(extraKeywords) GT 0) THEN self -> cgCOORD::SetProperty,  _EXTRA=extraKeywords
   
END ;--------------------------------------------------------------------------


FUNCTION cgMap::SetMapProjection, map_projection, $
    LATLON_RANGES=latlon_ranges, $
    POSITION=position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    DATUM=datum, $
    ELLIPSOID=ellipsoid, $
    LIMIT=limit, $
    RADIANS=radians, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    ZONE=zone, $
    _EXTRA=extraKeywords

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 1
    ENDIF
   
   ; Need a new map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        ; Find the map projection.
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theProjections).name) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) AND (N_Elements(map_projection) NE 0) THEN BEGIN
            index = Where((*self._cg_theProjections).index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF 
        thisProjection = (*self._cg_theProjections)[index]
        self._cg_thisProjection = thisProjection
   ENDIF
   
   ; Need a new datum?
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > datum < 19]
        self._cg_thisDatum = thisDatum
   ENDIF

   ; Need a new ellipsoid?
   IF N_Elements(ellipsoid) NE 0 THEN BEGIN
        IF Size(ellipsoid, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self._cg_theDatums).name) EQ StrUpCase(ellipsoid))
            IF index[0] EQ -1 THEN Message, 'Cannot find ellipsoid ' + ellipsoid + ' in datum list.' 
            thisDatum = (*self._cg_theDatums)[index]
        ENDIF ELSE thisDatum = (*self._cg_theDatums)[0 > ellipsoid < 19]
        self._cg_thisDatum = thisDatum
   ENDIF
   
   ; Other map keywords?
   IF N_Elements(center_latitude) NE 0 THEN self._cg_center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self._cg_center_longitude = center_longitude
   IF N_Elements(radians) NE 0 THEN self._cg_radians = radians
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self._cg_thisDatum.semimajor_axis = sphere_radius
      self._cg_thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self._cg_thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self._cg_thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(limit) NE 0 THEN *self._cg_limit = limit
   IF N_Elements(zone) NE 0 THEN self._cg_zone = zone
   IF N_Elements(extrakeywords) NE 0 THEN *self._cg_map_projection_keywords = extrakeywords
   
   ; Extract the values you need to call MAP_PROJ_INIT.
   thisProjection = self._cg_thisProjection.name
   sphereOnly = self._cg_thisProjection.sphereOnly
   thisDatum = self._cg_thisDatum.name
   radians = self._cg_radians
   semimajor_axis = self._cg_thisDatum.semimajor_axis 
   semiminor_axis = self._cg_thisDatum.semiminor_axis
   center_lon = self._cg_center_longitude
   center_lat = self._cg_center_latitude
   IF N_Elements(*self._cg_limit) NE 0 THEN limit = *self._cg_limit
   zone = self._cg_zone
   IF N_Elements(*self._cg_map_projection_keywords) NE 0 THEN keywords = *self._cg_map_projection_keywords
   
   ; Center latitudes are not allowed in some projections. Here are the ones where
   ; they are prohibited.
   centerlatOK = 1
   badprojstr = ['GOODES HOMOLOSINE', 'STATE PLANE', 'MERCATOR', 'SINUSOIDAL', 'EQUIRECTANGULAR', $
      'MILLER CYLINDRICAL', 'ROBINSON', 'SPACE OBLIQUE MERCATOR A', 'SPACE OBLIQUE MERCATOR B', $
      'ALASKA CONFORMAL', 'INTERRUPTED GOODE', 'MOLLWEIDE', 'INTERRUPED MOLLWEIDE', 'HAMMER', $
      'WAGNER IV', 'WAGNER VII', 'INTEGERIZED SINUSOIDAL']
   void = Where(badprojstr EQ StrUpCase(thisProjection), count)
   IF count GT 0 THEN centerlatOK = 0
    
    ; UTM and State Plane projections have to be handled differently.
    IF (StrUpCase(thisProjection) EQ 'UTM') OR (StrUpCase(thisProjection) EQ 'STATE PLANE') THEN BEGIN
    
        CASE StrUpCase(thisProjection) OF
            'UTM': BEGIN
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self._cg_thisDatum.(0), /GCTP, $
                    CENTER_LATITUDE=center_lat, CENTER_LONGITUDE=center_lon, RADIANS=radians, ZONE=zone)
                END
            'STATE PLANE': BEGIN
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self._cg_thisDatum.(0), /GCTP, $
                    RADIANS=radians, ZONE=zone)
                END
        ENDCASE
        
    ENDIF ELSE BEGIN

        ; Call MAP_PROJ_INIT to get the map projection structure.
        CASE 1 OF
        
            centerLatOK AND sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords)
                END
                
            ~centerLatOK AND sphereOnly: BEGIN

                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords)
                END
                
            ~centerLatOK AND ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords)
                END
    
            centerLatOK AND ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, RADIANS=radians, $
                    _EXTRA=keywords)
                END
        ENDCASE
   ENDELSE
        
    RETURN, mapStruct
    
END ;--------------------------------------------------------------------------


PRO cgMap::CLEANUP

   Ptr_Free, self._cg_limit
   Ptr_Free, self._cg_map_projection_keywords
   Ptr_Free, self._cg_theDatums
   Ptr_Free, self._cg_theProjections
   Obj_Destroy, self._cg_overlays
   
   self -> cgCOORD::CLEANUP 

END ;--------------------------------------------------------------------------


FUNCTION cgMap::INIT, map_projection, $
    BACKGROUND=background, $
    COLOR=color, $
    CONTINENTS=continents, $
    ERASE=erase, $
    GRID=grid, $
    LATLON_RANGES=latlon_ranges, $
    NOBORDER=noborder, $
    POSITION=position, $
    TITLE=title, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    DATUM=datum, $
    ELLIPSOID=ellipsoid, $
    LIMIT=limit, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    ZONE=zone, $
    ; Superclass keywords that must be defined here because of the way I have had to use _EXTRA
    ; keywords to pick up map projection keywords.
    NAME=name, $
    UVALUE=uvalue, $
    _EXTRA=extraKeywords

   Compile_Opt idl2
    
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF
      
   ; Structures used in the object.
   datumStruct = { cgMap_DATUM }
   void = { cgMap_PROJECTION }
   
   ; If you specify a BACKGROUND keyword, then ERASE is set automatically.
   IF N_Elements(background) NE 0 THEN BEGIN
       erase = 1
   ENDIF ELSE background = 'white'
   erase = Keyword_Set(erase)
   IF N_Elements(color) EQ 0 THEN color = "opposite"
   IF N_Elements(position) EQ 0 THEN position = [0.075, 0.075, 0.925, 0.825]
   IF N_Elements(title) EQ 0 THEN title = ""

   ; Default map projection.
   IF N_Elements(map_projection) EQ 0 THEN BEGIN
        this_map_projection = 'Equirectangular'
        IF (N_Elements(datum) EQ 0) && (N_Elements(ellipsoid) EQ 0) THEN datum = 'SPHERE'
        map_projection = 117
        limit = [-90, -180, 90, 180]
   ENDIF
   
    projections=[ {cgMap_PROJECTION, 'UTM', 101, 0 }, $  ; GCTP 101
                  {cgMap_PROJECTION, 'State Plane', 102, 0 }, $  ; GCTP 102
                  {cgMap_PROJECTION, 'Albers Equal Area', 103, 0 }, $  ; GCTP 103
                  {cgMap_PROJECTION, 'Lambert Conformal Conic', 104, 0 }, $  ; GCTP 104
                  {cgMap_PROJECTION, 'Mercator', 105, 0 }, $  ; GCTP 105
                  {cgMap_PROJECTION, 'Polar Stereographic', 106, 0 }, $  ; GCTP 106
                  {cgMap_PROJECTION, 'Polyconic', 107, 0 }, $  ; GCTP 107
                  {cgMap_PROJECTION, 'Equidistant Conic A', 108, 0 }, $  ; GCTP 108
                  {cgMap_PROJECTION, 'Transverse Mercator', 109, 0 }, $  ; GCTP 109
                  {cgMap_PROJECTION, 'Stereographic', 110, 1 }, $  ; GCTP 110
                  {cgMap_PROJECTION, 'Lambert Azimuthal', 111, 1 }, $  ; GCTP 111
                  {cgMap_PROJECTION, 'Azimuthal', 112, 1 }, $  ; GCTP 112
                  {cgMap_PROJECTION, 'Gnomonic', 113, 1 }, $  ; GCTP 113
                  {cgMap_PROJECTION, 'Orthographic', 114, 1 }, $  ; GCTP 114
                  {cgMap_PROJECTION, 'Near Side Perspective', 115, 1 }, $  ; GCTP 115
                  {cgMap_PROJECTION, 'Sinusoidal', 116, 1 }, $  ; GCTP 116
                  {cgMap_PROJECTION, 'Equirectangular', 117, 1 }, $  ; GCTP 117
                  {cgMap_PROJECTION, 'Miller Cylindrical', 118, 1 }, $  ; GCTP 118
                  {cgMap_PROJECTION, 'Van der Grinten', 119, 1 }, $  ; GCTP 119
                  {cgMap_PROJECTION, 'Hotine Oblique Mercator A', 120, 0 }, $ ; GCTP 120
                  {cgMap_PROJECTION, 'Robinson', 121, 1 }, $ ; GCTP 121
                  {cgMap_PROJECTION, 'Space Oblique Mercator A', 122, 0 }, $ ; GCTP 122
                  {cgMap_PROJECTION, 'Alaska Conformal', 123, 0 }, $ ; GCTP 123
                  {cgMap_PROJECTION, 'Interrupted Goode', 124, 1 }, $  ; GCTP 124
                  {cgMap_PROJECTION, 'Mollweide', 125, 1 }, $ ; GCTP 125
                  {cgMap_PROJECTION, 'Interrupted Mollweide', 126, 1 }, $ ; GCTP 126
                  {cgMap_PROJECTION, 'Hammer', 127, 1 }, $  ; GCTP 127
                  {cgMap_PROJECTION, 'Wagner IV', 128, 1 }, $ ; GCTP 128
                  {cgMap_PROJECTION, 'Wagner VII', 129, 1 }, $ ; GCTP 129
                  {cgMap_PROJECTION, 'Integerized Sinusoidal', 131, 1 }, $ ; GCTP 131
                  {cgMap_PROJECTION, 'Equidistant Conic B', 208, 0 }, $ ; GCTP 208
                  {cgMap_PROJECTION, 'Hotine Oblique Mercator B', 220, 0 }, $ ; GCTP 220
                  {cgMap_PROJECTION, 'Space Oblique Mercator B', 222, 0 }] ; GCTP 222
                  
    IF Float(!Version.Release) GE 8.0 THEN BEGIN
        projections = [projections, {cgMap_PROJECTION, 'Cylindrical Equal Area', 132, 0 }]
    ENDIF

    ; Find the map projection.
    IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
        index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
        IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the CGTP projection list.'
    ENDIF ELSE BEGIN
        index = Where(projections.index EQ map_projection, count)
        IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in GCTP projection list.' 
    ENDELSE
    this_map_projection = projections[index]
   
   ; Find the datum.
   theDatums = Replicate(datumStruct, 21)
   theDatums[0] =  { cgMap_DATUM,  0, 'Clark 1866', 6378206.4 , 6356583.8  }
   theDatums[1] =  { cgMap_DATUM,  1, 'Clark 1880', 6378249.145, 6356514.86955  }
   theDatums[2] =  { cgMap_DATUM,  2, 'Bessel', 6377397.155, 6356078.96284 }
   theDatums[3] =  { cgMap_DATUM,  3, 'International 1967', 6378157.5, 6356772.2 }
   theDatums[4] =  { cgMap_DATUM,  4, 'International 1909', 6378388.0, 6356911.94613  }
   theDatums[5] =  { cgMap_DATUM,  5, 'WGS 72', 6378135.0, 6356750.519915  }
   theDatums[6] =  { cgMap_DATUM,  6, 'Everst', 6377276.3452 , 6356075.4133 }
   theDatums[7] =  { cgMap_DATUM,  7, 'WGS 66', 6378145.0 , 6356759.769356  }
   theDatums[8] =  { cgMap_DATUM,  8, 'WGS 84', 6378137.0, 6356752.314245 }
   theDatums[9] =  { cgMap_DATUM,  9, 'Airy', 6377563.396, 6356256.91  }
   theDatums[10] = { cgMap_DATUM, 10, 'Modified Everest', 6377304.063, 6356103.039 }
   theDatums[11] = { cgMap_DATUM, 11, 'Modified Airy', 6377340.189, 6356034.448  }
   theDatums[12] = { cgMap_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   theDatums[13] = { cgMap_DATUM, 13, 'Southeast Asia', 6378155.0, 6356773.3205 }
   theDatums[14] = { cgMap_DATUM, 14, 'Australian National', 6378160.0, 6356774.719 }
   theDatums[15] = { cgMap_DATUM, 15, 'Krassovsky', 6378245.0, 6356863.0188 }
   theDatums[16] = { cgMap_DATUM, 16, 'Hough', 6378270.0 , 6356794.343479  }
   theDatums[17] = { cgMap_DATUM, 17, 'Mercury 1960', 6378166.0, 6356784.283666  }
   theDatums[18] = { cgMap_DATUM, 18, 'Modified Mercury 1968', 6378150.0, 6356768.337303 }
   theDatums[19] = { cgMap_DATUM, 19, 'Sphere', 6370997.0, 6370997.0 }
   theDatums[20] = { cgMap_DATUM,  8, 'GRS 1980', 6378137.0, 6356752.31414 }
   
   ; Since I already have "WGS 84" in the list, and since IDL 8 introduces an ellipsoid 
   ; with this name, I am going to use index 24 to list the more commonly used "WGS84" name.
   IF Float(!Version.Release) GE 8.0 THEN BEGIN
      theDatums = [theDatums, $
                  { cgMap_DATUM, 20, 'Clarke IGN', 6378249.2, 6356515.0 }, $
                  { cgMap_DATUM, 21, 'Helmert 1906', 6378200.0, 6356818.2 }, $
                  { cgMap_DATUM, 22, 'Modified Fischer 1960', 6378115.0, 6356773.3 }, $
                  { cgMap_DATUM, 23, 'South American 1969', 6378160.0, 6356774.7 }, $
                  { cgMap_DATUM, 24, 'WGS84', 6378137.0, 6356752.314245 }]
       
   ENDIF
   
   IF (N_Elements(datum) EQ 0) && (N_Elements(ellipsoid) NE 0) THEN datum = ellipsoid
   IF N_Elements(datum) EQ 0 THEN BEGIN
        thisDatum = theDatums[19] 
   ENDIF ELSE BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(theDatums.name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = theDatums[index]
        ENDIF ELSE thisDatum = theDatums[0 > datum < (N_Elements(theDatums)-1)]
   ENDELSE
   
   ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
   ; produces the wrong result when a UTM projection is used in conjunction
   ; with a WGS84 datum (the most common datum used in this projection). Here
   ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
   ; results in position errors of less than a meter typically.
   IF (StrUpCase(thisDatum.Name) EQ 'WGS 84') && $
      (StrUpCase(this_map_projection.Name) EQ 'UTM') && $
      (Float(!version.release) LE 8.2) THEN BEGIN
          Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
          thisDatum = { MAPCOORD_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   ENDIF
   
   ; Modify the radii?
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
        semimajor_axis = sphere_radius
        semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(zone) EQ 0 THEN zone = 1
   
      ; Default MAP_PROJ_INIT keywords.
   IF N_Elements(center_latitude) EQ 0 THEN self._cg_center_latitude = 0 ELSE self._cg_center_latitude = center_latitude
   IF N_Elements(center_longitude) EQ 0 THEN self._cg_center_longitude = 0 ELSE self._cg_center_longitude = center_longitude
   IF N_Elements(limit) NE 0 THEN self._cg_limit = Ptr_New(limit) ELSE self._cg_limit = Ptr_New(/ALLOCATE_HEAP)
   IF N_Elements(extraKeywords) NE 0 $
        THEN self._cg_map_projection_keywords = Ptr_New(extraKeywords) $
        ELSE self._cg_map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
   
   ; Load the object.
   self._cg_background = background
   self._cg_color = color
   self._cg_erase = erase
   self._cg_noborder = Keyword_Set(noborder)
   self._cg_radians = Keyword_Set(radians)
   self._cg_theDatums = Ptr_New(theDatums)
   self._cg_theProjections = Ptr_New(projections)
   self._cg_thisDatum = thisDatum
   self._cg_thisProjection = this_map_projection
   self._cg_title = title
   self._cg_zone = zone
   self._cg_overlays = Obj_New('cgContainer')
   
   ; Get the map structure.
   mapStruct = self -> SetMapProjection()
   
   ; Need ranges?
   IF N_Elements(xrange) EQ 0 THEN BEGIN
       xrange = mapStruct.uv_box[[0,2]]
       latlon_ranges = 0
   ENDIF
   IF N_Elements(yrange) EQ 0 THEN BEGIN
        yrange = mapStruct.uv_box[[1,3]]
        latlon_ranges = 0
   ENDIF
   
   ; Are the ranges in lat/lon space?
   IF Keyword_Set(latlon_ranges) THEN BEGIN
      uvcoords = Map_Proj_Forward(xrange, yrange, MAP_STRUCTURE=mapStruct)
      xrange = Reform(uvcoords[0,*])
      yrange = Reform(uvcoords[1,*])
   ENDIF

      ; Call the SUPERCLASS object INIT method.
   ok = self -> cgCOORD::INIT ($
        POSITION=position, $
        XRANGE=xrange, $
        YRANGE=yrange, $
        XLOG=0, YLOG=0, $ ; No log axis on maps!
        NAME=name, $
        UVALUE=uvalue)
        
   IF ~ok THEN RETURN, 0
   
   ; Do you need overlay objects in this map?
   IF N_Elements(continents) NE 0 THEN BEGIN
   
       ; Is this a continental object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(continents) THEN BEGIN
           continents -> GetProperty, NAME=continentsName
           IF continentsName EQ "" THEN continentsName = 'MAPCONTINENTS'
           self._cg_overlays -> Add, continents
       ENDIF ELSE BEGIN
           continents = Obj_New('cgContinents', self, /COUNTRIES, $
               COLOR=self._cg_color, NAME='MAPCONTINENTS')
           self._cg_overlays -> Add, continents
       ENDELSE
   ENDIF
   
   IF N_Elements(grid) NE 0 THEN BEGIN
   
       ; Is this a grid object? If so add it, if not
       ; create one and add it.
       IF Obj_Valid(grid) THEN BEGIN
           grid -> GetProperty, NAME=gridName
           IF gridName EQ "" THEN gridName = 'MAPGRID'
           self._cg_overlays -> Add, grid
       ENDIF ELSE BEGIN
           grid = Obj_New('cgMapGrid', self, /AUTODRAW, COLOR=self._cg_color, NAME='MAPGRID')
           self._cg_overlays -> Add, grid
       ENDELSE
   ENDIF

   RETURN, 1

END ;--------------------------------------------------------------------------


PRO cgMap__Define, class

   ; Structures used in the object.
   datumStruct = { cgMap_DATUM, index:0, name:"", semimajor_axis:0.0D, semiminor_axis:0.0D }
   mapStruct =   { cgMap_PROJECTION, name:"", index:0, sphereOnly:0 }

   class = { cgMap, $
             _cg_background: "", $                      ; The background color for erasing the display.
             _cg_center_latitude: 0.0D, $               ; The latitude at the center of the map projection.
             _cg_center_longitude:0.0D, $               ; The lontigude at the center of the map projection.
             _cg_color: "", $                           ; The name of the color in which to draw a border.
             _cg_erase: 0B, $                           ; A flag that indicates the map should erase the display before drawing.
             _cg_limit: Ptr_New(), $                    ; The limit of the map projection.
             _cg_map_projection_keywords: Ptr_New(), $  ; A storage location for MAP_PROJ_INIT keywords.
             _cg_noborder: 0B, $                        ; A flag that indicates a border should not be drawn.
             _cg_overlays: Obj_New(), $                 ; A storage location for map overlays.
             _cg_radians: 0B, $                         ; A flag that indicated values are in radians, not degrees.
             _cg_theDatums: Ptr_New(), $                ; Information about available map datums.
             _cg_thisDatum: datumStruct, $              ; The particular datum structure for this map projection.
             _cg_theProjections: Ptr_New(), $           ; Information about available map projections.
             _cg_thisProjection: mapStruct, $           ; The particular map projection structure for this map projection.
             _cg_title: "", $                           ; The map title.
             _cg_zone: 0, $                             ; The UTM zone of the map projection.
             INHERITS cgCOORD $                         ; The superclass object.
           }

END ;--------------------------------------------------------------------------


