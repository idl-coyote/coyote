; docformat = 'rst'
;
; NAME:
;   cgGoogleMapWidget
;
; PURPOSE:
;   This is the driver for the cgGoggleMapWidget object, which is a compound widget
;   object that obtains a Google static map from Google Maps and displays it in the
;   program's draw widget window. It can be used either as a stand-alone program or
;   to create a map image in draw widget in a larger widget program.
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
; This is the driver for the cgGoggleMapWidget object, which is a compound widget
; object that obtains a Google static map from Google Maps and displays it in the
; program's draw widget window. It can be used either as a stand-alone program or
; to create a map image in draw widget in a larger widget program.
;
; This program implements a subset of the Google Static Map API, which can be found in
; more detail here: https://developers.google.com/maps/documentation/staticmaps/. The 
; program works by building a URL for a map image. A connection to the Internet is 
; required to then request a map image (in GIF, JPEG or PNG format) to be returned from
; Goggle Maps. The returned image is then read and loaded into a draw widget window
; of the right size for the returned map image. The default is to delete the image file
; that is created, but the user can also set keywords to retain the image that is downloaded
; from Google Maps. Users are able to control button and motion events in the resulting
; draw widget with their own event handler module. A cgMap coordinate object is created
; to establish a map reference coordinate system on top of the returned map image, allowing
; other map information to be drawn on top of the returned map image.
;
; :Categories:
;    Graphics, Widgets
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
;
; :Returns:
;    The return value is an instance of a cgGoggleMapWidget object.
;    
; :Examples:
;    Used to put two markers on a map of Fort Collins, Colorado, in a stand-alone window::
;        PRO cgGoggleMapWidget_Test
;            marker1 = {cgGOOGLEMAPMARKER, 'normal', 'dodger blue', 'A', Ptr_New(40.600), Ptr_New(-105.100)}
;            marker2 = {cgGOOGLEMAPMARKER, 'normal', 'purple',      'B', Ptr_New(40.605), Ptr_New(-105.105)}
;            googleObject = cgGoggleMapWidget(MARKERS=[marker1, marker2], MAPTYPE='Terrain')
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
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgGoogleMapWidget, parent, $
    BOX_AXES=box_axes, $
    BUTTON_EVENTS=button_events, $
    CENTER_LATITUDE=centerLat, $
    CENTER_LONGITUDE=centerLon, $
    IMAGETYPE=imageType, $
    KEEP_IMAGE=keep_image, $
    EVENT_METHOD=event_method, $
    EVENT_PRO=event_pro, $
    MAPTYPE=maptype, $
    MARKERS=markers, $
    MOTION_EVENTS=motion_events, $
    TEMPDIR=tempdir, $
    VISIBLEMARKERS=visibleMarkers, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMLEVEL=zoomlevel
    
    mapObject = Obj_New('cgGoogleMapWidget', parent, $
        BOX_AXES=box_axes, $
        BUTTON_EVENTS=button_events, $
        CENTER_LATITUDE=centerLat, $
        CENTER_LONGITUDE=centerLon, $
        IMAGETYPE=imageType, $
        KEEP_IMAGE=keep_image, $
        EVENT_METHOD=event_method, $
        EVENT_PRO=event_pro, $
        MAPTYPE=maptype, $
        MARKERS=markers, $
        MOTION_EVENTS=motion_events, $
        TEMPDIR=tempdir, $
        VISIBLEMARKERS=visibleMarkers, $
        XSIZE=xsize, $
        YSIZE=ysize, $
        ZOOMLEVEL=zoomlevel)
            
    IF Obj_Valid(mapObject) THEN RETURN, mapObject ELSE RETURN, Obj_New()
    
END
