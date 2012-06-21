;+
; This method creates a cgMap map coordinate object for georeferencing the
; map image returned by Google maps. Use this object to establish a geocoordinate
; reference rectangle for drawing on top of the map image.
;-
PRO GoogleMapWidget::CreateMapCoordObject

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       Obj_Destroy, mapCoord
       RETURN
    ENDIF
    
    ; Create a map object for navigating the map. Mercator projection, WGS-84 datum.
    mapCoord = Obj_New('cgMap', 105, Datum=8)
  
    ; The metersPerPixel value is determined by using 6371000. meters for the
    ; radius of the Earth, calculating the number of meters per deg lon, then
    ; the number of pixels per deg lon (256/360 for zoom level 0), then figuring
    ; in the zoom level.
    metersPerPixel = Google_MetersPerPixel(self.zoomlevel)
    xy = mapCoord -> Forward(self.centerLon, self.centerLat)
    xrange = [xy[0] - ((self.xsize/2.0)*metersPerPixel), xy[0] + ((self.xsize/2.0)*metersPerPixel)]
    yrange = [xy[1] - ((self.ysize/2.0)*metersPerPixel), xy[1] + ((self.ysize/2.0)*metersPerPixel)]
    mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange, POSITION=[0,0,1,1]
    
    ; If there is current a map coordinate object, destroy it first.
    IF Obj_Valid(self.mapCoord) THEN Obj_Destroy, self.mapCoord
    self.mapCoord = mapCoord
 
END



;+
; The purpose of this method is obtain the map from Google as an image and display
; it in the draw widget window.
;-
PRO GoogleMapWidget::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF
    
    ; Going to take some time!
    Widget_Control, /Hourglass
    
    ; Basic string for obtaining a Google Static Map.
    googleString = 'http://maps.googleapis.com/maps/api/staticmap?center='

    ; Add the center latitude and longitude
    googleString = googleString + String(self.centerLat,format='(F0.4)') + ',' + $
       String(self.centerLon,format='(F0.4)')
       
    ; Add the zoom level.
    googleString = googleString + '&zoom=' + StrTrim(self.zoomLevel,2)
    
    ; Add the size of the image.
    googleString = googleString + '&size=' + StrTrim(self.xsize) + 'x' + StrTrim(self.ysize,2)
    
    ; Add the map type.
    googleString = googleString + '&maptype=' + StrTrim(StrLowCase(self.mapType),2) 
    
    ; Add sensor information.
    googleString = googleString + '&sensor=false
    
    ; Add image format information.
    googleString = googleString + '&format=' + StrTrim(StrLowCase(self.imageType),2)
    
    ; Create an IDL object to connect to the Internet with a URL.
    netObject = Obj_New('IDLnetURL')
    void = netObject -> Get(URL=googleString, FILENAME=self.filename)
    Obj_Destroy, netObject
    
    ; Read the image and display it in the draw widget window.
    mapImage = Read_Image(self.filename, r, g, b)
    IF ~self.keep_image THEN File_Delete, self.filename
    IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
    WSet, self.wid
    cgImage, mapImage
   
    ; Store the image
    IF ~Ptr_Valid(self.mapImage) THEN BEGIN
      self.mapImage = Ptr_New(mapImage, /No_Copy) 
    ENDIF ELSE *self.mapImage = mapImage
       
    Widget_Control, Hourglass=0
END


;+
; The purpose of this method is handle draw widget events. Currently,
; no events are being generated or handled.
; 
;-
PRO GoogleMapWidget::DrawWidgetEvents, event

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF
        
    ; Not doing anything with these events currently.
    RETURN
    
END

;+
; This method returns the map coordinate object that sets up the georeferencing
; coordinate system (in projected meter space) for drawing on top of the map image.
;-
FUNCTION GoogleMapWidget::GetMapCoord

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN, Obj_New()
    ENDIF
    
    ; If the map coordinate is not valid, then create one.
    IF ~Obj_Valid(self.mapCoord) THEN self -> CreateMapCoordObject
    
    ; Return the current map coordinate object.
    RETURN, self.mapCoord
    
END

;+
; The properties of the object are retrieved with this method.
;
; :Keywords:
;     centerlat: out, optional, type=float
;         The center latitude of the requested Google map. 
;     centerlon: out, optional, type=float
;         The center longitude of the requested Google map. 
;     event_method: out, optional, type='string'
;         The name of the event handler method for the draw widget. 
;     filename: out, optional, type='string'
;         The name of the file where the map image is stored.
;     imagemap: out, optional, type=bytarr
;         The image variable containing the Goggle map. The size and dimensions
;         of the image depend upon what was retrieved from Google.
;     imagetype: out, optional, type=string
;         The type of image format the Google map should be returned in. 
;     mapcoord: out, optional, type=object
;         The map coordinate object. Another way to obtain the map coordinate object is
;         to use the GetMapCoord method.
;     maptype: out, optional, type=boolean
;         The type of Google map requested by the program.
;     xsize: out, optional, type=int
;         The X size of the program's draw widget.
;     ysize: out, optional, type=int
;         The Y size of the program's draw widget.
;     zoomlevel: out, optional, type=integer
;         The zoom level of the requested Google map. 
;-
PRO GoogleMapWidget::GetProperty, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    EVENT_METHOD=event_method, $
    FILENAME=filename, $
    IMAGEMAP=imageMap, $
    IMAGETYPE=imageType, $
    MAPCOORD=mapCoord, $
    MAPTYPE=maptype, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMLEVEL=zoomlevel

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF
    
    IF Arg_Present(centerLat) THEN centerLat = self.centerLat
    IF Arg_Present(centerLon) THEN centerLon = self.centerLon
    IF Arg_Present(filename) THEN filename = self.filename
    IF Arg_Present(imageMap) THEN imageMap = *self.imageMap
    IF Arg_Present(imageType) THEN imageType = self.imageType
    IF Arg_Present(event_method) THEN event_method = self.event_method
    IF Arg_Present(mapCoord) THEN mapCoord = self.mapCoord
    IF Arg_Present(maptype) THEN maptype = self.maptype
    IF Arg_Present(xsize) THEN xsize = self.xsize
    IF Arg_Present(ysize) THEN ysize = self.ysize
    IF Arg_Present(zoomlevel) THEN zoomlevel = self.zoomlevel
    
END



;+
; The purpose of this method is to display a map with a particular map type.
; 
; :Params:
;    event: in, required, type=varies
;       The event structure passed to this event handler method from which the map
;       type can be obtained. Or, the map type itself, passed as a string.
;-
PRO GoogleMapWidget::Map_Type, event

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF

    IF Size(event, /TNAME) EQ 'STRING' THEN BEGIN
       mapType = event
    ENDIF ELSE BEGIN
       mapType = Widget_Info(event.id, /UNAME)
    ENDELSE
    mapType = StrLowCase(mapType)
    
    validTypes = ['roadmap', 'terrain', 'satellite', 'hybrid']
    index = Where(validTypes EQ mapType, count)
    IF count EQ 0 THEN Message, 'Invalid map type: ' + StrUpCase(mapType)
    
    ; Store it.
    self.mapType = mapType
    
    ; Draw the new map.
    self -> Draw
       
END


;+
; The purpose of this method is to draw the initial map plot in the draw widget.
; 
;-
PRO GoogleMapWidget::Notify_Realize

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

    ; Get the window index number of this draw widget.
    Widget_Control, self.drawID, Get_Value=wid
    self.wid = wid
    
    ; Set the draw widget up to handle events.
    IF self.event_method EQ "" THEN BEGIN
        eventMethod = 'DrawWidgetEvents' 
    ENDIF ELSE BEGIN
        eventMethod = self.event_method
    ENDELSE
    Widget_Control, self.drawID, Set_UValue={object:self, method:eventMethod}
    
     ; Draw the initial map image.
    self -> Draw
    
END


;+
; The purpose of this method is to set some of the object's properties. If
; you wish to retrive a new map after updating the object properties, be sure
; to set the DRAW keyword.
;
; :Keywords:
;     centerlat: in, optional, type=float, default=40.60
;         The center latitude of the requested Google map. If not provided, the latitude of
;         Fort Collins, Colorado, home of Coyote. Latitudes are only recognized to four
;         decimals values of precision.
;     centerlon: in, optional, type=float, default=-105.10
;         The center longitude of the requested Google map. If not provided, the longitude of
;         Fort Collins, Colorado, home of Coyote. Longitude are only recognized to four
;         decimals values of precision.
;     draw: in, optional, type=boolean, default=0
;         Set this keyword if you want to immediate retrieve and display a new map with the
;         updated properties.
;     event_method: in, optional, type='string'
;         The name of the event handler method for the draw widget. If you use this keyword,
;         you will also need to write this event handler module. It gets sent one positional
;         parameter, the event structure created by the draw widget.
;     imagetype: in, optional, type=string, default='png32'
;         The type of image format the Google map should be returned in. The default is
;         a 32-bit full color PNG file. The image types are given in the Google Static Map
;         API documentation and are as follows: png or png8, png32, gif, jpg, and jpg-baseline.
;     maptype: in, optional, type=string, default='terrain'
;         Set this keyword to the type of map you wish to retrieve from Google Maps. The
;         choices are listed in the Google Static Map API documentation and are: "roadmap",
;         "terrain", "satellite", and "hybrid".
;     xsize: in, optional, type=int, default=600
;         The X size of the program's draw widget.
;     ysize: in, optional, type=int, default=600
;         The Y size of the program's draw widget.
;     zoomlevel: in, optional, type=integer, default=12
;         The zoom level of the requested Google map. Should be an integer between 0 and 21.

PRO GoogleMapWidget::SetProperty, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    DRAW=draw, $
    EVENT_METHOD=event_method, $
    IMAGETYPE=imageType, $
    MAPTYPE=maptype, $
    ZOOMLEVEL=zoomlevel

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF
    
    IF N_Elements(centerLat) NE 0 THEN self.centerLat = centerLat
    IF N_Elements(centerLon) NE 0 THEN self.centerLon = centerLon
    IF N_Elements(imageType) NE 0 THEN self.imageType = imageType
    IF N_Elements(event_method) NE 0 THEN BEGIN
         self.event_method = event_method
         Widget_Control, self.drawID, Set_UValue={object:self, method:event_method}
    ENDIF
    IF N_Elements(maptype) NE 0 THEN self.maptype = maptype
    IF N_Elements(zoomlevel) NE 0 THEN self.zoomlevel = zoomlevel
        
    ; Things have changed. Update the map coordinate object.
    self -> CreateMapCoordObject
    
    ; Need to update the image?
    IF Keyword_Set(draw) THEN self -> Draw
    
END


;+
; The purpose of this method is to zoom into the map by
; increasing the zoom factor.
; 
; :Params:
;    event: in, optional, type=structure
;       The event structure passed to this event handler method. Not used currently.
;-
PRO GoogleMapWidget::Zoom_In, event

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF

    self.zoomlevel = (self.zoomlevel + 1) < 21
    self -> Draw
    self -> CreateMapCoordObject
       
END


;+
; The purpose of this method is to zoom out of the map by
; decreasing the zoom factor.
; 
; :Params:
;    event: in, optional, type=structure
;       The event structure passed to this event handler method. Not used currently.
;-
PRO GoogleMapWidget::Zoom_Out, event

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF

    self.zoomlevel = (self.zoomlevel - 1) > 0
    self -> Draw
    self -> CreateMapCoordObject
       
END


;+
; The clean-up method for the object. When the object is destroyed, 
; this method will free the object's pointers and objects. If you
; wanted to save the map image file, this is where you do it.
;-
PRO GoogleMapWidget::CLEANUP
    Obj_Destroy, self.mapCoord
    Obj_Destroy, self.random
    Ptr_Free, self.markers
    Ptr_Free, self.mapImage
    IF self.keep_image THEN BEGIN
        filename = cgPickFile(/Write, File=File_BaseName(self.filename), $
           Title='Save Google Map Image As...')
        IF filename NE "" THEN BEGIN
           File_Move, self.filename, filename 
        ENDIF ELSE BEGIN
           File_Delete, self.filename
        ENDELSE
    ENDIF
    IF Obj_Valid(self.tlb) THEN Widget_Control, self.tlb, /Destroy
END


;+
; This is the initialization method of the GoogleMapWidget object. 
; 
; :Params:
;    parent: in, optional, type=long
;         The identifier of the parent widget of the draw widget that is going to be 
;         created by the program. If not provided, the program will create its own 
;         top-level base widget as the parent.
;       
; :Keywords:
;     button_events: in, optional, type=boolean, default=0
;         Set this keyword to turn button events on for the draw widget in the program.
;     centerlat: in, optional, type=float, default=40.60
;         The center latitude of the requested Google map. If not provided, the latitude of
;         Fort Collins, Colorado, home of Coyote. Latitudes are only recognized to four
;         decimals values of precision.
;     centerlon: in, optional, type=float, default=-105.10
;         The center longitude of the requested Google map. If not provided, the longitude of
;         Fort Collins, Colorado, home of Coyote. Longitude are only recognized to four
;         decimals values of precision.
;     event_method: in, optional, type='string'
;         The name of the event handler method for the draw widget. If you use this keyword,
;         you will also need to write this event handler module. It gets sent one positional
;         parameter, the event structure created by the draw widget.
;     imagetype: in, optional, type=string, default='png32'
;         The type of image format the Google map should be returned in. The default is
;         a 32-bit full color PNG file. The image types are given in the Google Static Map
;         API documentation and are as follows: png or png8, png32, gif, jpg, and jpg-baseline.
;     keep_image: in, optional, type=boolean, default=0
;         Set this keyword if you wish to save the Google map as an image when the object
;         is destroyed.
;     maptype: in, optional, type=string, default='terrain'
;         Set this keyword to the type of map you wish to retrieve from Google Maps. The
;         choices are listed in the Google Static Map API documentation and are: "roadmap",
;         "terrain", "satellite", and "hybrid".
;     motion_events: in, optional, type=boolean, default=0
;         Set this keyword to turn motion events on for the draw widget in the program.
;     tempDir: in, optional, type=string
;         The directory where the image containing the Google map is written. By default,
;         it is obtained from the environment like this: tempdir = GetEnv('IDL_TMPDIR').
;     xsize: in, optional, type=int, default=600
;         The X size of the program's draw widget.
;     ysize: in, optional, type=int, default=600
;         The Y size of the program's draw widget.
;     zoomlevel: in, optional, type=integer, default=12
;         The zoom level of the requested Google map. Should be an integer between 0 and 21.
;-
FUNCTION GoogleMapWidget::INIT, parent, $
    BUTTON_EVENTS=button_events, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    EVENT_METHOD=event_method, $
    IMAGETYPE=imageType, $
    KEEP_IMAGE=keep_image, $
    MAPTYPE=maptype, $
    MOTION_EVENTS=motion_events, $
    TEMPDIR=tempdir, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMLEVEL=zoomlevel
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN, 0
    ENDIF
    
    ; Check keyword parameters.
    button_events = Keyword_Set(button_events)
    IF N_Elements(centerLat) EQ 0 THEN centerLat = 40.600
    IF N_Elements(centerLon) EQ 0 THEN centerLon = -105.1
    IF N_Elements(event_method) EQ 0 THEN event_method = ""
    IF N_Elements(imageType) EQ 0 THEN imageType = 'png32' ELSE imageType = StrLowCase(imageType)
    keep_image = Keyword_Set(keep_image)
    IF N_Elements(mapType) EQ 0 THEN mapType = 'terrain' ELSE mapType = StrLowCase(mapType)
    motion_events = Keyword_Set(motion_events)
    IF N_Elements(tempdir) EQ 0 THEN tempdir = GetEnv('IDL_TMPDIR')
    IF N_Elements(xsize) EQ 0 THEN xsize = 600 ELSE xsize = xsize < 640
    IF N_Elements(ysize) EQ 0 THEN ysize = 600 ELSE ysize = ysize < 640
    IF N_Elements(zoomlevel) EQ 0 THEN zoomlevel = 12 ELSE zoomlevel = 0 > zoomLevel < 21
    
    ; Add a random number generator.
    self.random = Obj_New('RandomNumberGenerator')
    
    ; Use the random number generator to produce a filename for the program.
    filerootName = 'google_map_' + self.random->GetRandomDigits(4)
    CASE imageType OF
       'png': filename = filerootName + '.png'
       'png8': filename = filerootName + '.png'
       'png32': filename = filerootName + '.png'
       'jpg': filename = filerootName + '.jpg'
       'jpg-baseline': filename = filerootName + '.jpg'
       'gif': filename = filerootName + '.jpg'
       ELSE: Message, 'Unknown image type for Google Map: ' + imageType
    ENDCASE
    filename = Filepath(Root_Dir=tempDir, filename)
    
    ; If there is no parent, then create a top-level base widget to display your self in.
    IF N_Elements(parent) EQ 0 THEN BEGIN
       createparent = 1
       parent = Widget_Base(Title='Google Map Widget', COLUMN=1, /BASE_ALIGN_CENTER)
    ENDIF ELSE createparent = 0
    drawID = Widget_Draw(parent, XSIZE=xsize, YSIZE=ysize, $
        NOTIFY_REALIZE='GoogleMapWidget_Notify_Realize', $
        UVALUE={object:self, method:'Notify_Realize'}, $
        BUTTON_EVENTS=button_events, MOTION_EVENTS=motion_events, $
        EVENT_PRO='GoogleMapWidget_Events')
    IF createparent THEN BEGIN
        buttonBase = Widget_Base(parent, ROW=1)
        void = Widget_Button(buttonBase, Value='Zoom In', UVALUE={object:self, method:'ZOOM_IN'})
        void = Widget_Button(buttonBase, Value='Zoom Out', UVALUE={object:self, method:'ZOOM_OUT'})
        menuID = Widget_Button(buttonBase, Value='Map Type...', /MENU)
        void = Widget_Button(menuID, Value='Terrain', UNAME='TERRAIN', $
            UVALUE={object:self, method:'MAP_TYPE'})
        void = Widget_Button(menuID, Value='Satellite', UNAME='SATELLITE', $
            UVALUE={object:self, method:'MAP_TYPE'})
        void = Widget_Button(menuID, Value='Roadmap', UNAME='ROADMAP', $
            UVALUE={object:self, method:'MAP_TYPE'})
        void = Widget_Button(menuID, Value='Hybrid', UNAME='HYBRID', $
            UVALUE={object:self, method:'MAP_TYPE'})
     ENDIF
    
    ; Load the object.
    self.centerLat = centerLat
    self.centerLon = centerLon
    self.tlb = parent
    self.drawID = drawID
    self.event_method = event_method
    self.filename = filename
    self.keep_image = keep_image
    self.imageType = imageType
    self.mapType = mapType
    self.xsize = xsize
    self.ysize = ysize
    self.tempDir = tempDir
    IF N_Elements(markers) EQ 0 $
        THEN self.markers = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.markers = Ptr_New(markers)
    self.zoomlevel = zoomlevel
    
    ; Create a map coordinate object for navigating the Google Map.
    self.mapCoord = self -> GetMapCoord()
    
    ; Realize the widget if you created the parent.
    IF createparent THEN Widget_Control, parent, /REALIZE, SET_UVALUE=self
    
    ; Start the program if you created the parent.
    IF createparent THEN BEGIN
       XManager, 'googlemapwidget', parent, /No_Block, $
          Cleanup='GoogleMapWidget_Cleanup', Event_Handler='GoogleMapWidget_Events'
    ENDIF
    
    RETURN, 1
END



;+
; This is the realize notify routine for the widget. Its function call the
; Realize_Notify method to draw the initial plot in the display window.
; 
; :Params:
;    id: in, required, type=int
;        The widget identifier of the widget that has been realized.
;-
PRO GoogleMapWidget_Notify_Realize, id

   Widget_Control, id, Get_UValue=struct
   Call_Method, 'Notify_Realize', struct.object
   
END


;+
; This is the cleanup routine for the widget. Its function is to destroy
; the underlying program object.
; 
; :Params:
;    tlb: in, required, type=int
;        The widget identifier of the parent base widget that just died.
;-
PRO GoogleMapWidget_Cleanup, tlb

   Widget_Control, tlb, Get_UValue=self
   Obj_Destroy, self
   
END


;+
; This is the main event handler for the program. All events come here
; to be distributed to the appropriate event handler method according
; to instructions packed into the UVALUE of any widget generating an
; event.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager.
;-
PRO GoogleMapWidget_Events, event

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF
   
    Widget_Control, event.id, Get_UValue=instructions
    Call_Method, instructions.method, instructions.object, event

END


;+
; The object class definition for the GoogleMapWidget class.
; 
; :Params:
;     class: out, optional, type=structure
;        The object class definition as a structure. Occasionally, comes in handy.
;-
PRO GoogleMapWidget__Define, class

    class = { GOOGLEMAPWIDGET, $
              centerLat: 0.0D, $       ; The center latitude of the Google map.
              centerLon: 0.0D, $       ; The center longitude of the Google map.
              drawID: 0L, $            ; The draw widget identifier.
              event_method: "", $      ; The event method of the draw widget.
              filename: "", $          ; The name of the image file containing the map after download.
              keep_image: 0B, $        ; Set this flag to save the Google map as an image.
              imageType: "", $         ; The type of image to download (png, jpeg, etc.).
              mapImage: Ptr_New(), $   ; The Goggle map as an image variable.
              mapType: "", $           ; The type of Google map (terrain, roadmap, satellite, etc.).
              mapCoord: Obj_New(), $   ; A map coordinate object for drawing on top of the map.
              markers: Ptr_New(), $    ; Markers that Google puts on the map.
              random: Obj_New(), $     ; A random number generator object.
              tlb: 0L, $               ; The parent of this widget-object.
              tempDir: "", $           ; The directory where the map is downloaded as an image.
              wid: 0L, $               ; The window index number of the draw widget.
              xsize: 0L, $             ; The X size of the draw widget. No larger than 640 pixels.
              ysize: 0L, $             ; The Y size of the draw widget. No larger than 640 pixels.
              zoomlevel: 0 $           ; The zoom level of the Google Map (0 to 21).
            }
             
END


