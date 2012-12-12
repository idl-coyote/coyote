; docformat = 'rst'
;
; NAME:
;   cgKML_ScreenOverlay
;
; PURPOSE:
;   This program implements the KML ScreenOverlay element. A ScreenOverlay element draws 
;   an image onto the Google Earth display or screen.
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
;   This program implements the KML ScreenOverlay element. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#ScreenOverlay>`.
;   A ScreenOverlay element draws an image onto the Google Earth display or screen.
;
; :Categories:
;    Graphics, FileIO
;    
; :Examples:
;    See the `cgKML_File` object for examples of how to create a KML file.
;    
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
;        Written, 3 November 2012 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-


;+
; The purpose of this method is to establish an abstract KML ScreenOverlay class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference?hl=fr#ScreenOverlay.
;
; :Keywords:
;     color: in, optional, type=string
;         Color values are expressed in hexadecimal notation, including opacity (alpha) values. 
;         The order of expression is alpha, blue, green, red (aabbggrr). The range of values for 
;         any one color is 0 to 255 (00 to ff). For opacity, 00 is fully transparent and ff is 
;         fully opaque. For example, if you want to apply a blue color with 50 percent opacity 
;         to an overlay, you would specify the following: COLOR = "7fff0000".
;     draworder: in, optional, type=integer
;         This element defines the stacking order for the images in overlapping overlays. 
;         Overlays with higher values are drawn on top of overlays with lower values.
;     href: in, optional, type=string
;         A URL that identifies the location of the image associated with this Overlay. The
;         location can be either a local file (e.g., 'myimage.png') or a URL to an image
;         on a web server (e.g., 'http://www.idlcoyote.com/images/myimage.png').
;     overlay_xy: in, required, type=dblarr
;         This keyword specifies a point [x,y] in the image overlay that is mapped to the 
;         screen coordinate `Screen_XY`. The location of the point is specified by the values 
;         in `Overlay_Units_X` and 'Overlay_Units_Y`. 
;     overlay_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     overlay_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     rotation: in, optional, type=double, default=0.0
;         This keyword indicates the angle of rotation of the screen overlay in degrees counterclockwise
;         from North. A value from -180 to 180.
;     screen_xy: in, optional, type=dblarr
;         This keyword specifies a point [x,y] relative to the screen origin that the overlay image 
;         is mapped to. The location of the point is specified by the values in `XUnits` and 'YUnits`. 
;     screen_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     screen_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     size_xy: in, required, type=dblarr
;         This keyword specifies the size of the image for the screen overlay as follows. A -1
;         indicates to use the native dimensions of the image. A 0 indicates that the aspect ratio
;         of the image should be preserved. Any other value sets the output dimension of the image,
;         according to the "unit" values.
;     size_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     size_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.         
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_ScreenOverlay::INIT, $
  COLOR=color, $
  DRAWORDER=draworder, $
  HREF=href, $
  OVERLAY_XY=overlay_xy, $
  OVERLAY_UNIT_X=overlay_unit_x, $
  OVERLAY_UNIT_Y=overlay_unit_y, $
  SCREEN_XY=screen_xy, $
  SCREEN_UNIT_X=screen_unit_x, $
  SCREEN_UNIT_Y=screen_unit_y, $
  SIZE_XY=size_xy, $
  SIZE_UNIT_X=size_unit_x, $
  SIZE_UNIT_Y=size_unit_y, $
  ROTATION=rotation, $
  _REF_EXTRA=extra

  Compile_Opt idl2
  
  ; Error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Call the superclass object's INIT method.
  IF ~self -> cgKML_Overlay::INIT(COLOR=color, DRAWORDER=draworder, HREF=href, _Extra=extra) THEN RETURN, 0
  
  ; Check optional parameters.
  IF Total(N_Elements(overlay_xy) + N_Elements(overlay_unit_x) + N_Elements(overlay_unit_y)) EQ 0 THEN BEGIN
     overlay_xy = [0.0, 1.0]
     overlay_unit_x = 'fraction'
     overlay_unit_y = 'fraction'
  ENDIF 
  IF Total(N_Elements(screen_xy) + N_Elements(screen_unit_x) + N_Elements(screen_unit_y)) EQ 0 THEN BEGIN
     screen_xy = [0.025, 0.975]
     screen_unit_x = 'fraction'
     screen_unit_y = 'fraction'
  ENDIF 
  IF Total(N_Elements(size_xy) + N_Elements(size_unit_x) + N_Elements(size_unit_y)) EQ 0 THEN BEGIN
     size_xy = [0, 0]
     size_unit_x = 'fraction'
     size_unit_y = 'fraction'
  ENDIF 
  IF N_Elements(overlay_unit_x) EQ 0 THEN overlay_unit_x = 'fraction'
  IF N_Elements(overlay_unit_y) EQ 0 THEN overlay_unit_y = 'fraction'
  IF N_Elements(screen_unit_x) EQ 0 THEN screen_unit_x = 'fraction'
  IF N_Elements(screen_unit_y) EQ 0 THEN screen_unit_y = 'fraction'
  IF N_Elements(size_unit_x) EQ 0 THEN size_unit_x = 'fraction'
  IF N_Elements(size_unit_y) EQ 0 THEN size_unit_y = 'fraction'
  IF N_Elements(size_xy) EQ 0 THEN size_xy = [-1,-1]
  IF N_Elements(rotation) EQ 0 THEN rotation = 0.0
  
  ; Check required parameters.
  IF N_Elements(overlay_xy) EQ 0 THEN Message, 'An Overlay_XY point is required.'
  IF N_Elements(screen_xy) EQ 0 THEN Message, 'An Screen_XY point is required.'
  
  ; Load the object.
  self.overlay_xy = overlay_xy
  self.overlay_unit_x = overlay_unit_x
  self.overlay_unit_y = overlay_unit_y
  self.screen_xy = screen_xy
  self.screen_unit_x = screen_unit_x
  self.screen_unit_y = screen_unit_y
  self.size_xy = size_xy
  self.size_unit_x = size_unit_x
  self.size_unit_y = size_unit_y
  self.rotation = rotation

  RETURN, 1

END


;+
; This method opens the <ScreenOverlay> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_ScreenOverlay::Head, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; We require a logical unit number.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number (LUN) is required in this method.'
    
  ; Write the overlay elements.
  IF self.id NE "" THEN BEGIN
    PrintF, lun, '   <ScreenOverlay id="' + StrTrim(self.id,2) + '">'
  ENDIF ELSE BEGIN
    PrintF, lun, '   <ScreenOverlay>'
  ENDELSE
  
END


;+
; This method adds ScreenOverlay elements to the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_ScreenOverlay::Body, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; We require a logical unit number.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number (LUN) is required in this method.'
  
      
  ; Write the overlay elements.
  PrintF, lun, '      <overlayXY ' + $
               'x="' + String(self.overlay_xy[0], Format='(F0.6)') + '" ' + $
               'y="' + String(self.overlay_xy[1], Format='(F0.6)') + '" ' + $
               'xunits="' + self.overlay_unit_x + '" ' + $
               'yunits="' + self.overlay_unit_y + '" />' 
  PrintF, lun, '      <screenXY ' + $
               'x="' + String(self.screen_xy[0], Format='(F0.6)') + '" ' + $
               'y="' + String(self.screen_xy[1], Format='(F0.6)') + '" ' + $
               'xunits="' + self.screen_unit_x + '" ' + $
               'yunits="' + self.screen_unit_y + '" />' 
  PrintF, lun, '      <size ' + $
               'x="' + String(self.size_xy[0], Format='(F0.6)') + '" ' + $
               'y="' + String(self.size_xy[1], Format='(F0.6)') + '" ' + $
               'xunits="' + self.size_unit_x + '" ' + $
               'yunits="' + self.size_unit_y + '" />' 
  self -> XMLTag, 'rotation', self.rotation, LUN=lun, SPACE=6       
    
END


;+
; This method closes the <ScreenOverlay> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_ScreenOverlay::Tail, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; We require a logical unit number.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number (LUN) is required in this method.'
    
  ; Write the overlay elements.
  PrintF, lun, '   </ScreenOverlay>'
  
END


;+
; This BUILD method builds the ScreenOverlay section in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_ScreenOverlay::Build, LUN=lun

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  self -> Head, LUN=lun
  self -> cgKML_Overlay::Build, LUN=lun
  self -> Body, LUN=lun
  self -> Tail, LUN=lun
 
END


;+
; The purpose of this method is to return object properties.
; 
; :Keywords:
;     overlay_xy: out, required, type=dblarr
;         This keyword specifies a point [x,y] in the image overlay that is mapped to the 
;         screen coordinate `Screen_XY`. The location of the point is specified by the values 
;         in `Overlay_Units_X` and 'Overlay_Units_Y`. 
;     overlay_unit_x: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     overlay_unit_y: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     rotation: out, optional, type=double, default=0.0
;         This keyword indicates the angle of rotation of the screen overlay in degrees counterclockwise
;         from North. A value from -180 to 180.
;     screen_xy: out, optional, type=dblarr
;         This keyword specifies a point [x,y] relative to the screen origin that the overlay image 
;         is mapped to. The location of the point is specified by the values in `XUnits` and 'YUnits`. 
;     screen_unit_x: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     screen_unit_y: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     size_xy: out, required, type=dblarr
;         This keyword specifies the size of the image for the screen overlay as follows. A -1
;         indicates to use the native dimensions of the image. A 0 indicates that the aspect ratio
;         of the image should be preserved. Any other value sets the output dimension of the image,
;         according to the "unit" values.
;     size_unit_x: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     size_unit_y: out, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image. 
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_ScreenOverlay::GetProperty, $
   OVERLAY_XY=overlay_xy, $
   OVERLAY_UNIT_X=overlay_unit_x, $
   OVERLAY_UNIT_Y=overlay_unit_y, $
   SCREEN_XY=screen_xy, $
   SCREEN_UNIT_X=screen_unit_x, $
   SCREEN_UNIT_Y=screen_unit_y, $
   SIZE_XY=size_xy, $
   SIZE_UNIT_X=size_unit_x, $
   SIZE_UNIT_Y=size_unit_y, $
   ROTATION=rotation, $
   _REF_EXTRA=extra
   
   IF Arg_Present(overlay_xy) THEN overlay_xy = self.overlay_xy
   IF Arg_Present(overlay_unit_x) THEN overlay_unit_x = self.overlay_unit_x
   IF Arg_Present(overlay_unit_y) THEN overlay_unit_y = self.overlay_unit_y
   IF Arg_Present(screen_xy) THEN screen_xy = self.screen_xy
   IF Arg_Present(screen_unit_x) THEN screen_unit_x = self.screen_unit_x
   IF Arg_Present(screen_unit_y) THEN screen_unit_y = self.screen_unit_y
   IF Arg_Present(size_xy) THEN size_xy = self.size_xy
   IF Arg_Present(size_unit_x) THEN size_unit_x = self.size_unit_x
   IF Arg_Present(size_unit_y) THEN size_unit_y = self.size_unit_y
   IF Arg_Present(rotation) THEN rotation = self.rotation

   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Overlay::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to set object properties.
; 
; :Keywords:
;     overlay_xy: in, required, type=dblarr
;         This keyword specifies a point [x,y] in the image overlay that is mapped to the 
;         screen coordinate `Screen_XY`. The location of the point is specified by the values 
;         in `Overlay_Units_X` and 'Overlay_Units_Y`. 
;     overlay_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     overlay_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     rotation: in, optional, type=double, default=0.0
;         This keyword indicates the angle of rotation of the screen overlay in degrees counterclockwise
;         from North. A value from -180 to 180.
;     screen_xy: in, optional, type=dblarr
;         This keyword specifies a point [x,y] relative to the screen origin that the overlay image 
;         is mapped to. The location of the point is specified by the values in `XUnits` and 'YUnits`. 
;     screen_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     screen_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image.
;     size_xy: in, required, type=dblarr
;         This keyword specifies the size of the image for the screen overlay as follows. A -1
;         indicates to use the native dimensions of the image. A 0 indicates that the aspect ratio
;         of the image should be preserved. Any other value sets the output dimension of the image,
;         according to the "unit" values.
;     size_unit_x: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the X value is a fraction of the image;
;         (2) "pixels" indicates the the X value is in pixels; and (3) "insetPixels" indicates the X
;         value is an indent from the right edge of the image.
;     size_unit_y: in, optional, type=string, default='fraction'
;         Three values are possible: (1) "fraction" indicates that the Y value is a fraction of the image;
;         (2) "pixels" indicates the the Y value is in pixels; and (3) "insetPixels" indicates the Y
;         value is an indent from the top edge of the image. 
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_ScreenOverlay::SetProperty, $
   OVERLAY_XY=overlay_xy, $
   OVERLAY_UNIT_X=overlay_unit_x, $
   OVERLAY_UNIT_Y=overlay_unit_y, $
   SCREEN_XY=screen_xy, $
   SCREEN_UNIT_X=screen_unit_x, $
   SCREEN_UNIT_Y=screen_unit_y, $
   SIZE_XY=size_xy, $
   SIZE_UNIT_X=size_unit_x, $
   SIZE_UNIT_Y=size_unit_y, $
   ROTATION=rotation, $
   _REF_EXTRA=extra
   
   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   IF N_Elements(overlay_xy) NE 0 THEN self.overlay_xy = overlay_xy
   IF N_Elements(overlay_unit_x) NE 0 THEN self.overlay_unit_x = overlay_unit_x
   IF N_Elements(overlay_unit_y) NE 0 THEN self.overlay_unit_y = overlay_unit_y
   IF N_Elements(screen_xy) NE 0 THEN self.screen_xy = screen_xy
   IF N_Elements(screen_unit_x) NE 0 THEN self.screen_unit_x = screen_unit_x
   IF N_Elements(screen_unit_y) NE 0 THEN self.screen_unit_y = screen_unit_y
   IF N_Elements(size_xy) NE 0 THEN self.size_xy = size_xy
   IF N_Elements(size_unit_x) NE 0 THEN self.size_unit_x = size_unit_x
   IF N_Elements(size_unit_y) NE 0 THEN self.size_unit_y = size_unit_y
   IF N_Elements(rotation) NE 0 THEN self.rotation = rotation
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Overlay::SetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_ScreenOverlay::CLEANUP

    ; Call the superclass object's CLEANUP method
    self -> cgKML_Overlay::Cleanup
    
END


;+
; The cgKML_ScreenOverlay class definition module. It is a container object and 
; represents part of the KML file hierarchy.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_ScreenOverlay__Define, class

   class = { cgKML_ScreenOverlay, $
             INHERITS cgKML_Overlay, $    ; An extended cgOverlay object.
             overlay_xy:DblArr(2), $
             overlay_unit_x: "", $
             overlay_unit_y: "", $
             screen_xy:DblArr(2), $
             screen_unit_x: "", $
             screen_unit_y: "", $
             size_xy: DblArr(2), $
             size_unit_x: "", $
             size_unit_y: "", $
             rotation: 0.0d $
           }
 
END