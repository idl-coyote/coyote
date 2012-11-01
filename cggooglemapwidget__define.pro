; docformat = 'rst'
;
; NAME:
;   cgGoogleMapWidget__Define
;
; PURPOSE:
;   This is a compound widget object that obtains a Google static map from Google Maps 
;   and displays it in the program's draw widget window. It can be used either as a 
;   stand-alone program or to create a map image in draw widget in a larger widget program.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;+
; This is a compound widget object that obtains a Google static map from Google Maps 
; and displays it in the program's draw widget window. It can be used either as a 
; stand-alone program or to create a map image in draw widget in a larger widget program.
; 
; This program implements a subset of the Google Static Map API, which can be found in
; more detail here: https://developers.google.com/maps/documentation/staticmaps/. The 
; program works by building a URL for a map image. A connection to the Internet is 
; required to then request a map image (in JPEG or PNG format) to be returned from
; Goggle Maps. The returned image is then read and loaded into a draw widget window
; of the right size for the returned map image. The default is to delete the image file
; that is created, but user can also set keywords to retain the image that is downloaded
; from Google Maps. Users are able to control button and motion events in the resulting
; draw widget with their own event handler module. A cgMap coordinate object is created
; to establish a map reference coordinate system on top of the returned map image, allowing
; other map information to be drawn on top of the returned map image.
;
; .. image:: cggooglemapwidget.png
; 
; :Categories:
;    Graphics
;    
;    
; :Examples:
;    Used to put two markers on a map of Fort Collins, Colorado, in a stand-alone window::
;        PRO cgGoogleMapWidget_Test
;            marker1 = {cgGOOGLEMAPMARKER, 'normal', 'dodger blue', 'A', Ptr_New(40.600), Ptr_New(-105.100)}
;            marker2 = {cgGOOGLEMAPMARKER, 'normal', 'purple',      'B', Ptr_New(40.605), Ptr_New(-105.105)}
;            googleObject = Obj_New('cgGoogleMapWidget', MARKERS=[marker1, marker2], MAPTYPE='Terrain')
;        END
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 25 June 2012. DWF.
;        Set the RETAIN keyword on the draw widget for UNIX machines. 28 June 2012. DWF.
;        Beefed up and changed error handling when failing to obtain a map from Google Maps. 28 June 2012. DWF.
;        Added NoForwardFix keyword to call to cgMap to allow better drawing of markers. 29 June 2012. DWF.
;        Added the ability to turn markers on or off with VisibleMarkers keyword and property. 29 June 2012. DWF.
;        Added a WID keyword to the GetProperty method to all the user to obtain the Goggle Map 
;            window index number. 29 Aug 2012. DWF.
;            
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-

;
;
;+
; This is the initialization method of the cgGoogleMapWidget object. 
; 
; :Params:
;    parent: in, optional, type=long
;         The identifier of the parent widget of the draw widget that is going to be 
;         created by the program. If not provided, the program will create its own 
;         top-level base widget as the parent.
;       
; :Keywords:
;     box_axes: in, optional, type=boolean, default=0
;         Set this keyword to draw box axes around the Google Map.
;     button_events: in, optional, type=boolean, default=0
;         Set this keyword to turn button events on for the draw widget in the program.
;     center_latitude: in, optional, type=float, default=40.60
;         The center latitude of the requested Google map. If not provided, the latitude of
;         Fort Collins, Colorado, home of Coyote. Latitudes are only recognized to four
;         decimals values of precision.
;     center_longitude: in, optional, type=float, default=-105.10
;         The center longitude of the requested Google map. If not provided, the longitude of
;         Fort Collins, Colorado, home of Coyote. Longitude are only recognized to four
;         decimals values of precision.
;     event_method: in, optional, type='string'
;         The name of the event handler method (a procedure) for the draw widget. If you use this keyword,
;         you will also need to write this event handler module. It gets sent one positional
;         parameter, the event structure created by the draw widget.
;     event_pro: in, optional, type='string'
;         The name of an external event handler procedure for the draw widget. The event handler
;         procedure gets sent one positional parameter, the event structure created by the draw widget.
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
;     markers: in, optional, type=structure
;         A scalar or array of cgGoogleMapMarker structures. If present, the markers will
;         be requested with the map from Google. The cgGoogleMapMarker structure is defined
;         like this::
;            struct = { cgGOOGLEMAPMARKER, $
;               size: "", $         ; The marker size ("tiny", "small", "mid" or "normal")
;               color: "", $        ; A color name as provided by cgColor.
;               label: "", $        ; A single uppercase character label from the set {A-Z,0-9}.
;               lats: Ptr_New(), $  ; A pointer to one or more latitude values.
;               lons: Ptr_New() }   ; A pointer to one or more longitude values.
;         Note that the user will be responsible for freeing the pointers in the MARKERS
;         structure. This program will not do that.
;     motion_events: in, optional, type=boolean, default=0
;         Set this keyword to turn motion events on for the draw widget in the program.
;     tempDir: in, optional, type=string
;         The directory where the image containing the Google map is written. By default,
;         it is obtained from the environment like this: tempdir = GetEnv('IDL_TMPDIR').
;     visiblemarkers: in, optional, type=boolean, default=1
;         Set this keyword to 0 to temporarily turn off the display of the markers. Normally,
;         markers are drawn (if present), unless this flag is set to 0.
;     xsize: in, optional, type=int, default=600
;         The X size of the program's draw widget. A maximum of 690 if box axes are requested
;         and a maximum of 640 if no box axes are requested. Box axes require a 25 pixel border
;         and the maximum size of a Google Map is 640 pixels.
;     ysize: in, optional, type=int, default=600
;         The Y size of the program's draw widget. A maximum of 690 if box axes are requested
;         and a maximum of 640 if no box axes are requested. Box axes require a 25 pixel border
;         and the maximum size of a Google Map is 640 pixels.
;     zoomlevel: in, optional, type=integer, default=12
;         The zoom level of the requested Google map. Should be an integer between 0 and 21.
;-
FUNCTION cgGoogleMapWidget::INIT, parent, $
    BOX_AXES=box_axes, $
    BUTTON_EVENTS=button_events, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    EVENT_METHOD=event_method, $
    EVENT_PRO=event_pro, $
    IMAGETYPE=imageType, $
    KEEP_IMAGE=keep_image, $
    MAPTYPE=maptype, $
    MARKERS=markers, $
    MOTION_EVENTS=motion_events, $
    TEMPDIR=tempdir, $
    VISIBLEMARKERS=visiblemarkers, $
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
    box_axes = Keyword_Set(box_axes)
    maxWindowSize = box_axes ? 690 : 640
    button_events = Keyword_Set(button_events)
    IF N_Elements(centerLat) EQ 0 THEN centerLat = 40.600
    IF N_Elements(centerLon) EQ 0 THEN centerLon = -105.1
    IF N_Elements(event_method) EQ 0 THEN event_method = ""
    IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
    IF N_Elements(imageType) EQ 0 THEN imageType = 'png32' ELSE imageType = StrLowCase(imageType)
    keep_image = Keyword_Set(keep_image)
    IF N_Elements(mapType) EQ 0 THEN mapType = 'terrain' ELSE mapType = StrLowCase(mapType)
    motion_events = Keyword_Set(motion_events)
    IF N_Elements(tempdir) EQ 0 THEN tempdir = GetEnv('IDL_TMPDIR')
    IF N_Elements(visibleMarkers) EQ 0 THEN visibleMarkers = 1
    IF N_Elements(xsize) EQ 0 THEN xsize = 600 ELSE xsize = xsize < maxWindowSize
    IF N_Elements(ysize) EQ 0 THEN ysize = 600 ELSE ysize = ysize < maxWindowSize
    IF N_Elements(zoomlevel) EQ 0 THEN zoomlevel = 12 ELSE zoomlevel = 0 > zoomLevel < 21
    map_xsize = box_axes ? (xsize-50) : xsize
    map_ysize = box_axes ? (ysize-50) : ysize
    
    ; Make sure the temporary directory exists.
    IF ~File_Test(tempDir, /DIRECTORY) THEN File_MkDir, tempDir    

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
    retain = (StrUpCase(!Version.OS_Family) EQ 'UNIX') ? 2 : 1
    drawID = Widget_Draw(parent, XSIZE=xsize, YSIZE=ysize, $
        NOTIFY_REALIZE='cgGoogleMapWidget_Notify_Realize', $
        UVALUE={object:self, method:'Notify_Realize'}, $
        BUTTON_EVENTS=button_events, MOTION_EVENTS=motion_events, $
        EVENT_PRO='cgGoogleMapWidget_Events', RETAIN=retain)
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
    self.box_axes = box_axes
    self.centerLat = centerLat
    self.centerLon = centerLon
    self.tlb = parent
    self.drawID = drawID
    self.event_method = event_method
    self.event_pro = event_pro
    self.filename = filename
    self.keep_image = keep_image
    self.imageType = imageType
    self.map_xsize = map_xsize
    self.map_ysize = map_ysize
    self.mapType = mapType
    IF box_axes THEN BEGIN
       self.map_position = [25./xsize, 25./ysize, (xsize-25.)/xsize, (ysize-25.)/ysize]
    ENDIF ELSE BEGIN
       self.map_position = [0., 0., 1., 1.]
    ENDELSE
    self.visiblemarkers = Keyword_Set(visibleMarkers)
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
       XManager, 'cgGoogleMapWidget', parent, /No_Block, $
          Cleanup='cgGoogleMapWidget_Cleanup', Event_Handler='cgGoogleMapWidget_Events'
    ENDIF
    
    RETURN, 1
END


;+
; The clean-up method for the object. When the object is destroyed, 
; this method will free the object's pointers and objects. If you
; wanted to save the map image file, this is where you do it.
;-
PRO cgGoogleMapWidget::CLEANUP
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
; This method creates a cgMap map coordinate object for georeferencing the
; map image returned by Google maps. Use this object to establish a geocoordinate
; reference rectangle for drawing on top of the map image.
;-
PRO cgGoogleMapWidget::CreateMapCoordObject

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
    metersPerPixel = cgGoogle_MetersPerPixel(self.zoomlevel)
    xy = mapCoord -> Forward(self.centerLon, self.centerLat)
    xrange = [xy[0] - ((self.map_xsize/2.0)*metersPerPixel), xy[0] + ((self.map_xsize/2.0)*metersPerPixel)]
    yrange = [xy[1] - ((self.map_ysize/2.0)*metersPerPixel), xy[1] + ((self.map_ysize/2.0)*metersPerPixel)]
    mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange, POSITION=self.map_position
    
    ; If there is current a map coordinate object, destroy it first.
    IF Obj_Valid(self.mapCoord) THEN Obj_Destroy, self.mapCoord
    self.mapCoord = mapCoord
 
END



;+
; The purpose of this method is obtain the map from Google as an image and display
; it in the draw widget window.
; 
; :Keywords:
;     success: out, optional, type=boolean
;        On return, if set to 1 a map image was successfully obtained from Google. Otherwise, 0.
;-
PRO cgGoogleMapWidget::Draw, SUCCESS=success

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Dialog_Message('There was a problem in obtaining a map from Google Maps. See Command Log for details')
       Print, !Error_State.Msg
       IF Obj_Valid(netObject) THEN Obj_Destroy, netObject
       IF (!D.Flags AND 256) NE 0 THEN WSet, self.wid
       cgErase
       cgText, 0.05, 0.05, /Normal, 'Google Map Image is Unavailable', FONT=0
       success = 0
       RETURN
    ENDIF
    
    ; Assume success
    success = 1
    
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
    googleString = googleString + '&size=' + StrTrim(self.map_xsize,2) + 'x' + StrTrim(self.map_ysize,2)
    
    ; Add the map type.
    googleString = googleString + '&maptype=' + StrTrim(StrLowCase(self.mapType),2) 
    
    ; Add sensor information.
    googleString = googleString + '&sensor=false
    
    ; Add image format information.
    googleString = googleString + '&format=' + StrTrim(StrLowCase(self.imageType),2)
    
    ; Are there markers that have to be dealt with?
    numMarkers = N_Elements(*self.markers)   
    IF (numMarkers GT 0) && self.visibleMarkers THEN BEGIN
       markerStr = ""
       FOR j=0,numMarkers-1 DO BEGIN
       
           ; This marker.
           thisMarker = (*self.markers)[j]
           
           ; Default values.
           IF thisMarker.size EQ "" THEN markerSize = 'normal' ELSE markerSize = thisMarker.size
           IF thisMarker.color EQ "" THEN markerColor = 'red' ELSE markerColor = thisMarker.color
           
           ; Colors have to be done in a different way.
           triple = cgColor(markerColor, /Triple)
           color = String(triple[0], Format='(Z2.2)') +  String(triple[1], Format='(Z2.2)') +  String(triple[2], Format='(Z2.2)')
               markerStr = markerStr + "&markers=size:" + StrLowCase(markerSize) + "%7C"
           markerStr = markerStr + "color:0x" + color
           
           ; Need a label?
           IF thisMarker.label NE "" THEN markerStr = markerStr + "%7C" + 'label:' + StrUpCase(thisMarker.label)
           
           ; Make a list of lats and lons.
           lats = *thisMarker.lats
           lons = *thisMarker.lons
           FOR k=0,N_Elements(lats)-1 DO BEGIN
               markerStr = markerStr + "%7C" + Strtrim(lats[k],2) + ',' + StrTrim(lons[k],2)
           ENDFOR
       ENDFOR
       
       ; Add the marker string to the Google string.
       googleString = googleString + markerStr
       
    ENDIF
    
    ; Create an IDL object to connect to the Internet with a URL.
    netObject = Obj_New('IDLnetURL')
    returnName = netObject -> Get(URL=googleString, FILENAME=self.filename)
    Obj_Destroy, netObject
    
    ; Read the image and display it in the draw widget window.
    IF N_Elements(returnName) NE 0 THEN BEGIN
        mapImage = Read_Image(self.filename, r, g, b)
        IF ~self.keep_image THEN File_Delete, self.filename
        IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
        IF (!D.Flags AND 256) NE 0 THEN WSet, self.wid
        cgImage, mapImage, Position=self.map_position, KEEP_ASPECT=1, /Erase, Background='white'
        
        ; Do you need box axes?
        IF self.box_axes THEN BEGIN
            mapCoord = self -> GetMapCoord(/UPDATE)
            cgMap_Grid, /Box_Axes, MAP=mapCoord
        ENDIF
        
       
        ; Store the image
        IF ~Ptr_Valid(self.mapImage) THEN BEGIN
          self.mapImage = Ptr_New(mapImage, /No_Copy) 
        ENDIF ELSE *self.mapImage = mapImage
    ENDIF ELSE BEGIN
        IF (!D.Flags AND 256) NE 0 THEN WSet, self.wid
        success = 0
        cgErase
        cgText, 0.05, 0.05, /Normal, 'Google Map Image is Unavailable', FONT=0
    ENDELSE
       
    Widget_Control, Hourglass=0
END


;+
; The purpose of this method is handle draw widget events. 
; 
;-
PRO cgGoogleMapWidget::DrawWidgetEvents, event

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN
    ENDIF
        
    ; Need to call a method or procedure?
    IF self.event_method NE "" THEN Call_Method, self.event_method, self, event
    IF self.event_pro NE "" THEN Call_Procedure, self.event_pro, event
    
END

;+
; This method returns the map coordinate object that sets up the georeferencing
; coordinate system (in projected meter space) for drawing on top of the map image.
; 
; :Keywords:
;     update: in, optional, type=boolean, default=0
;         Set this keyword to make sure a new map coordinate object is created.
;-
FUNCTION cgGoogleMapWidget::GetMapCoord, UPDATE=update

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
       RETURN, Obj_New()
    ENDIF
    
    ; If updating, we will need a new map coordinate object.
    IF Keyword_Set(update) THEN Obj_Destroy, self.mapCoord
    
    ; If the map coordinate is not valid, then create one.
    IF ~Obj_Valid(self.mapCoord) THEN self -> CreateMapCoordObject
    
    ; Return the current map coordinate object.
    RETURN, self.mapCoord
    
END

;+
; The properties of the object are retrieved with this method.
;
; :Keywords:
;     center_latitude: out, optional, type=float
;         The center latitude of the requested Google map. 
;     center_longitude: out, optional, type=float
;         The center longitude of the requested Google map. 
;     event_method: out, optional, type='string'
;         The name of the event handler method for the draw widget. 
;     filename: out, optional, type='string'
;         The name of the file where the map image is stored.
;     imagetype: out, optional, type=string
;         The type of image format the Google map should be returned in. 
;     mapcoord: out, optional, type=object
;         The map coordinate object. Another way to obtain the map coordinate object is
;         to use the GetMapCoord method.
;     mapimage: out, optional, type=bytarr
;         The image variable containing the Goggle map. The size and dimensions
;         of the image depend upon what was retrieved from Google.
;     mapposition: out, optional, type=fltarr
;         The position of the map in the display window.
;     maptype: out, optional, type=boolean
;         The type of Google map requested by the program.
;     xsize: out, optional, type=int
;         The X size of the program's draw widget.
;     ysize: out, optional, type=int
;         The Y size of the program's draw widget.
;     zoomlevel: out, optional, type=integer
;         The zoom level of the requested Google map. 
;-
PRO cgGoogleMapWidget::GetProperty, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    EVENT_METHOD=event_method, $
    FILENAME=filename, $
    MAPIMAGE=mapimage, $
    IMAGETYPE=imageType, $
    MAPPOSITION=map_position, $
    MAPCOORD=mapCoord, $
    MAPTYPE=maptype, $
    MARKERS=markers, $
    WID=wid, $
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
    IF Arg_Present(imageType) THEN imageType = self.imageType
    IF Arg_Present(event_method) THEN event_method = self.event_method
    IF Arg_Present(mapCoord) THEN mapCoord = self.mapCoord
    IF Arg_Present(mapImage) THEN mapImage = *self.mapImage
    IF Arg_Present(map_position) THEN map_position = self.map_position
    IF Arg_Present(maptype) THEN maptype = self.maptype
    IF Arg_Present(markers) THEN IF Ptr_Valid(self.markers) THEN markers = *self.markers
    IF Arg_Present(wid) THEN wid = self.wid
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
PRO cgGoogleMapWidget::Map_Type, event

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
PRO cgGoogleMapWidget::Notify_Realize

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
   Widget_Control, self.drawID, Set_UValue={object:self, method:'DrawWidgetEvents'}
    
     ; Draw the initial map image.
    self -> Draw
    
END


;+
; The purpose of this method is to set some of the object's properties. If
; you wish to retrive a new map after updating the object properties, be sure
; to set the DRAW keyword.
;
; :Keywords:
;     center_latitude: in, optional, type=float, default=40.60
;         The center latitude of the requested Google map. If not provided, the latitude of
;         Fort Collins, Colorado, home of Coyote. Latitudes are only recognized to four
;         decimals values of precision.
;     center_longitude: in, optional, type=float, default=-105.10
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
;     event_pro: in, optional, type='string'
;         The name of an external event handler procedure for the draw widget. The event handler
;         procedure gets sent one positional parameter, the event structure created by the draw widget.
;     imagetype: in, optional, type=string, default='png32'
;         The type of image format the Google map should be returned in. The default is
;         a 32-bit full color PNG file. The image types are given in the Google Static Map
;         API documentation and are as follows: png or png8, png32, gif, jpg, and jpg-baseline.
;     maptype: in, optional, type=string, default='terrain'
;         Set this keyword to the type of map you wish to retrieve from Google Maps. The
;         choices are listed in the Google Static Map API documentation and are: "roadmap",
;         "terrain", "satellite", and "hybrid".
;     markers: in, optional, type=structure
;         A scalar or array of cgGoogleMapMarker structures. If present, the markers will
;         be requested with the map from Google. The GoogleMapMarker structure is defined
;         like this::
;            struct = { cgGOOGLEMAPMARKER, $
;               size: "", $         ; The marker size ("tiny", "small", "mid" or "normal")
;               color: "", $        ; A color name as provided by cgColor.
;               label: "", $        ; A single uppercase character label from the set {A-Z,0-9}.
;               lats: Ptr_New(), $  ; A pointer to one or more latitude values.
;               lons: Ptr_New() }   ; A pointer to one or more longitude values.
;         Note that the user will be responsible for freeing the pointers in the MARKERS
;         structure. This program will not do that.
;     visiblemarkers: in, optional, type=boolean, default=1
;         Set this keyword to 0 to temporarily turn off the display of the markers. Normally,
;         markers are drawn (if present), unless this flag is set to 0.
;     zoomlevel: in, optional, type=integer, default=12
;         The zoom level of the requested Google map. Should be an integer between 0 and 21.

PRO cgGoogleMapWidget::SetProperty, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    DRAW=draw, $
    EVENT_METHOD=event_method, $
    EVENT_PRO=event_pro, $
    IMAGETYPE=imageType, $
    MAPTYPE=maptype, $
    MARKERS=markers, $
    VISIBLEMARKERS=visiblemarkers, $
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
    IF N_Elements(event_method) NE 0 THEN self.event_method = event_method
    IF N_Elements(event_pro) NE 0 THEN self.event_pro = event_pro
    IF N_Elements(maptype) NE 0 THEN self.maptype = maptype
    IF N_Elements(markers) NE 0 THEN BEGIN
       IF Ptr_Valid(self.markers) THEN *self.markers = markers ELSE self.markers = Ptr_New(markers)
    ENDIF
    IF N_Elements(visiblemarkers) NE 0 THEN self.visiblemarkers = Keyword_Set(visiblemarkers)
    IF N_Elements(zoomlevel) NE 0 THEN self.zoomlevel = zoomlevel
        
    ; Things have changed. Update the map coordinate object.
    self -> CreateMapCoordObject
    
    ; Need to update the image?
    IF Keyword_Set(draw) THEN self -> Draw
    
END

;+
; The purpose of this method is to make the draw widget window the current window.
;-
PRO cgGoogleMapWidget::SetWindow

    WSet, self.wid
    
END

;+
; The purpose of this method is to zoom into the map by
; increasing the zoom factor.
; 
; :Params:
;    event: in, optional, type=structure
;       The event structure passed to this event handler method. Not used currently.
;-
PRO cgGoogleMapWidget::Zoom_In, event

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
PRO cgGoogleMapWidget::Zoom_Out, event

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
; This is the realize notify routine for the widget. Its function call the
; Realize_Notify method to draw the initial plot in the display window.
; 
; :Params:
;    id: in, required, type=int
;        The widget identifier of the widget that has been realized.
;-
PRO cgGoogleMapWidget_Notify_Realize, id

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
PRO cgGoogleMapWidget_Cleanup, tlb

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
PRO cgGoogleMapWidget_Events, event

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
; The object class definition for the cgGoogleMapWidget class.
; 
; :Params:
;     class: out, optional, type=structure
;        The object class definition as a structure. Occasionally, comes in handy.
;-
PRO cgGoogleMapWidget__Define, class

    class = { cgGoogleMapWidget, $
              box_axes: 0B, $          ; A flag that indicates box axes should be drawn on the image.
              centerLat: 0.0D, $       ; The center latitude of the Google map.
              centerLon: 0.0D, $       ; The center longitude of the Google map.
              drawID: 0L, $            ; The draw widget identifier.
              event_method: "", $      ; The event method of the draw widget.
              event_pro: "", $         ; An external event handler procedure for the draw widget.
              filename: "", $          ; The name of the image file containing the map after download.
              keep_image: 0B, $        ; Set this flag to save the Google map as an image.
              imageType: "", $         ; The type of image to download (png, jpeg, etc.).
              mapImage: Ptr_New(), $   ; The Goggle map as an image variable.
              map_position: FltArr(4), $ ; The position of the map in the draw widget.
              map_xsize: 0L, $         ; The X size of the map.
              map_ysize: 0L, $         ; The Y size of the map.
              mapType: "", $           ; The type of Google map (terrain, roadmap, satellite, etc.).
              mapCoord: Obj_New(), $   ; A map coordinate object for drawing on top of the map.
              markers: Ptr_New(), $    ; Markers that Google puts on the map.
              random: Obj_New(), $     ; A random number generator object.
              tlb: 0L, $               ; The parent of this widget-object.
              tempDir: "", $           ; The directory where the map is downloaded as an image.
              visibleMarkers: 0B, $    ; A flag that indicates if the markers are shown or not.
              wid: 0L, $               ; The window index number of the draw widget.
              xsize: 0L, $             ; The X size of the draw widget. No larger than 640 pixels.
              ysize: 0L, $             ; The Y size of the draw widget. No larger than 640 pixels.
              zoomlevel: 0 $           ; The zoom level of the Google Map (0 to 21).
            }
             
END


