; docformat = 'rst'
;
; NAME:
;   cgKML_GroundOverlay
;
; PURPOSE:
;   This program implements the KML GroundOverlay element. A GroundOverlay element draws 
;   an image overlay draped onto the terrain of a Google Earth display.
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
;   This program implements the KML GroundOverlay element. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#groundoverlay>`.
;   A GroundOverlay element draws an image overlay draped onto the terrain
;   of a Google Earth display.
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
;        Written, 28 October 2012 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-


;+
; The purpose of this method is to establish an abstract KML GroundOverlay class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference?hl=fr#groundoverlay.
;
; :Keywords:
;     altitude: in, optional, type=double
;         Specifies the distance above the earth's surface, in meters, and is interpreted 
;         according to the altitude mode
;     altmode: in, optional, type=string, default='clampToGround'
;         The altitude mode. Possible values are "clampToGround", which ignores the `Altitude`
;         value and drapes the overlay over the terrain. The other possible mode is "absolute",
;         which sets the altitude of the overlay relative to sea level, regardless of the 
;         actual elevation of the terrain beneath the element. For example, if you set the 
;         altitude of an overlay to 10 meters with an absolute altitude mode, the overlay will 
;         appear to be at ground level if the terrain beneath is also 10 meters above sea level. 
;         If the terrain is 3 meters above sea level, the overlay will appear elevated above the 
;         terrain by 7 meters. It is also possible to set the mode to "clampToSeaFloor".
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
;     latlonbox: in, optional, type=fltarr
;         A four-element float array that specifies the top, bottom, right, and left sides
;         of a bounding box that the ground overlay is aligned to. The elements of the array
;         also correspond to [north, south, east, west] coordinates, which should be specified
;         in decimal degrees. North/South coordinates are in the range -90 to +90, and East/West
;         coordinates are in the range -180 to 180.
;     latlonquad: in, optional, type=fltarr
;         An 8-element array describing the four corners of a quadrilateral defining the overlay area. 
;         Exactly four coordinate pairs have to be provided, each consisting of floating point values 
;         for longitude and latitude. The coordinates must be specified in counter-clockwise order 
;         with the first coordinate corresponding to the lower-left corner of the overlayed image. 
;         The shape described by these corners must be convex.
;     rotation: in, optional, type=float, default=0.0
;          Specifies a rotation of the overlay about its center, in degrees. Values can be ±180. 
;          The default is 0 (north). Rotations are specified in a counterclockwise direction.
;          Used only with the `LatLonBox` array.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_GroundOverlay::INIT, $
  ALTITUDE=altitude, $
  ALTMODE=altmode, $
  COLOR=color, $
  DRAWORDER=draworder, $
  HREF=href, $
  LATLONBOX=latlonbox, $
  LATLONQUAD=latlonquad, $
  ROTATION=rotation, $
  _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Call the superclass object's INIT method.
  IF ~self -> cgKML_Overlay::INIT(COLOR=color, DRAWORDER=draworder, HREF=href, _Extra=extra) THEN RETURN, 0

  IF N_Elements(altmode) EQ 0 THEN altmode = 'clampToGround'
  IF N_Elements(rotation) EQ 0 THEN rotation =  0.0
  IF (N_Elements(latlonbox) EQ 0) && (N_Elements(latlonquad) EQ 0) THEN BEGIN
     latlonbox = [90, -90, -180, 180]
  ENDIF
  
  ; Load the object.
  IF N_Elements(altitude) NE 0 THEN self.altitude = altitude 
  IF N_Elements(altmode) NE 0 THEN self.altmode = altmode 
  IF N_Elements(latlonbox) NE 0 THEN self.latlonbox = Ptr_New(latlonbox)
  IF N_Elements(latlonquad) NE 0 THEN self.latlonquad = Ptr_New(latlonquad)
  IF N_Elements(rotation) NE 0 THEN self.rotation = rotation 

  RETURN, 1

END


;+
; This method opens the <GroundOverlay> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_GroundOverlay::Head, LUN=lun
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
    PrintF, lun, '   <GroundOverlay id="' + StrTrim(self.id,2) + '">'
  ENDIF ELSE BEGIN
    PrintF, lun, '   <GroundOverlay>'
  ENDELSE
  
END


;+
; This method adds GroundOverlay elements to the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_GroundOverlay::Body, LUN=lun
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
  self -> XMLTag, 'altitude', self.altitude, LUN=lun, SPACE=6
  CASE StrUpCase(self.altmode) OF
     'CLAMPTOGROUND': self -> XMLTag, 'altitudeMode', self.altmode, LUN=lun, SPACE=6
     'CLAMPTOSEAFLOOR': self -> XMLTag, 'gx:altitudeMode', self.altmode, LUN=lun, SPACE=6
     'ABSOLUTE': self -> XMLTag, 'altitudeMode', self.altmode, LUN=lun, SPACE=6
     ELSE: Message, 'Altitude Mode (' + self.altmode + ') is not recognized as valid.'
  ENDCASE
  IF Ptr_Valid(self.latlonBox) && Ptr_Valid(self.latlonQuad) THEN $
      Message, 'It is illegal to define both a LatLonBox and a LatLonQuad.'
  IF Ptr_Valid(self.latLonBox) THEN BEGIN
     PrintF, lun, '      <LatLonBox>
     self -> XMLTag, 'north', -90.0 > (*self.latlonbox)[0] < 90.0, LUN=lun, SPACE=9
     self -> XMLTag, 'south', -90.0 > (*self.latlonbox)[1] < 90.0, LUN=lun, SPACE=9
     self -> XMLTag, 'east', -180.0 > (*self.latlonbox)[2] < 180.0, LUN=lun, SPACE=9
     self -> XMLTag, 'west', -180.0 > (*self.latlonbox)[3] < 180.0, LUN=lun, SPACE=9
     self -> XMLTag, 'rotation', -180.0 > self.rotation < 180.0, LUN=lun, SPACE=9
     PrintF, lun, '      </LatLonBox>'
  ENDIF
  IF Ptr_Valid(self.latlonQuad) THEN BEGIN
     PrintF, lun, '      <gx:LatLonQuad><coordinates>
     PrintF, lun, '         ' + StrJoin(StrTrim((*self.latlonquad)[0:1],2), ',') + ' ' + $
                                StrJoin(StrTrim((*self.latlonquad)[2:3],2), ',') + ' ' + $
                                StrJoin(StrTrim((*self.latlonquad)[4:5],2), ',') + ' ' + $
                                StrJoin(StrTrim((*self.latlonquad)[6:7],2), ',') 
  ENDIF
  
END


;+
; This method closes the <GroundOverlay> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_GroundOverlay::Tail, LUN=lun
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
  PrintF, lun, '   </GroundOverlay>'
  
END


;+
; This BUILD method builds the GroundOverlay section in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_GroundOverlay::Build, LUN=lun

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
;     altitude: out, optional, type=double
;         Specifies the distance above the earth's surface, in meters, and is interpreted 
;         according to the altitude mode
;     altmode: out, optional, type=string, default='clampToGround'
;         The altitude mode. Possible values are "clampToGround", which ignores the `Altitude`
;         value and drapes the overlay over the terrain. The other possible mode is "absolute",
;         which sets the altitude of the overlay relative to sea level, regardless of the 
;         actual elevation of the terrain beneath the element. For example, if you set the 
;         altitude of an overlay to 10 meters with an absolute altitude mode, the overlay will 
;         appear to be at ground level if the terrain beneath is also 10 meters above sea level. 
;         If the terrain is 3 meters above sea level, the overlay will appear elevated above the 
;         terrain by 7 meters.
;     latlonbox: out, optional, type=fltarr
;         A four-element float array that specifies the top, bottom, right, and left sides
;         of a bounding box that the ground overlay is aligned to. The elements of the array
;         also correspond to [north, south, east, west] coordinates, which should be specified
;         in decimal degrees. North/South coordinates are in the range -90 to +90, and East/West
;         coordinates are in the range -180 to 180.
;     latlonquad: out, optional, type=fltarr
;         An 8-element array describing the four corners of a quadrilateral defining the overlay area. 
;         Exactly four coordinate pairs have to be provided, each consisting of floating point values 
;         for longitude and latitude. The coordinates must be specified in counter-clockwise order 
;         with the first coordinate corresponding to the lower-left corner of the overlayed image. 
;         The shape described by these corners must be convex.
;     rotation: out, optional, type=float, default=0.0
;          Specifies a rotation of the overlay about its center, in degrees. Values can be ±180. 
;          The default is 0 (north). Rotations are specified in a counterclockwise direction.
;          Used only with the `LatLonBox` array.
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_GroundOverlay::GetProperty, $
   ALTITUDE=altitude, $
   ALTMODE=altmode, $
   LATLONBOX=latlonbox, $
   LATLONQUAD=latlonquad, $
   ROTATION=rotation, $
   _REF_EXTRA=extra
   
  IF Arg_Present(altitude) THEN altitude = self.altitude
  IF Arg_Present(altmode) THEN altmode = self.altmode
  IF Arg_Present(rotation) THEN rotation = self.rotation   
  IF Arg_Present(latlonbox) THEN BEGIN
     IF Ptr_Valid(self.latlonbox) THEN latlonbox = *self.latlonbox
  ENDIF
  IF Arg_Present(latlonquad) THEN BEGIN
     IF Ptr_Valid(self.latlonquad) THEN latlonquad = *self.latlonquad
  ENDIF
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Overlay::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to set object properties.
; 
; :Keywords:
;     altitude: in, optional, type=double
;         Specifies the distance above the earth's surface, in meters, and is interpreted 
;         according to the altitude mode
;     altmode: in, optional, type=string, default='clampToGround'
;         The altitude mode. Possible values are "clampToGround", which ignores the `Altitude`
;         value and drapes the overlay over the terrain. The other possible mode is "absolute",
;         which sets the altitude of the overlay relative to sea level, regardless of the 
;         actual elevation of the terrain beneath the element. For example, if you set the 
;         altitude of an overlay to 10 meters with an absolute altitude mode, the overlay will 
;         appear to be at ground level if the terrain beneath is also 10 meters above sea level. 
;         If the terrain is 3 meters above sea level, the overlay will appear elevated above the 
;         terrain by 7 meters.
;     latlonbox: in, optional, type=fltarr
;         A four-element float array that specifies the top, bottom, right, and left sides
;         of a bounding box that the ground overlay is aligned to. The elements of the array
;         also correspond to [north, south, east, west] coordinates, which should be specified
;         in decimal degrees. North/South coordinates are in the range -90 to +90, and East/West
;         coordinates are in the range -180 to 180.
;     latlonquad: in, optional, type=fltarr
;         An 8-element array describing the four corners of a quadrilateral defining the overlay area. 
;         Exactly four coordinate pairs have to be provided, each consisting of floating point values 
;         for longitude and latitude. The coordinates must be specified in counter-clockwise order 
;         with the first coordinate corresponding to the lower-left corner of the overlayed image. 
;         The shape described by these corners must be convex.
;     rotation: in, optional, type=float, default=0.0
;          Specifies a rotation of the overlay about its center, in degrees. Values can be ±180. 
;          The default is 0 (north). Rotations are specified in a counterclockwise direction.
;          Used only with the `LatLonBox` array.
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_GroundOverlay::SetProperty, $
   ALTITUDE=altitude, $
   ALTMODE=altmode, $
   LATLONBOX=latlonbox, $
   LATLONQUAD=latlonquad, $
   ROTATION=rotation, $
   _REF_EXTRA=extra
   
  IF N_Elements(altitude) NE 0 THEN self.altitude = altitude 
  IF N_Elements(altmode) NE 0 THEN self.altmode = altmode 
  IF N_Elements(latlonbox) NE 0 THEN BEGIN
     IF Ptr_Valid(self.latlonBox) THEN *self.latlonbox = latlonbox ELSE self.latlonbox = Ptr_New(latlonbox) 
  ENDIF
  IF N_Elements(latlonquad) NE 0 THEN BEGIN
     IF Ptr_Valid(self.latlonquad) THEN *self.latlonquad = latlonquad ELSE self.latlonquad = Ptr_New(latlonquad) 
  ENDIF
  IF N_Elements(rotation) NE 0 THEN self.rotation = rotation 
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Overlay::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_GroundOverlay::CLEANUP

    Ptr_Free, self.latlonBox
    Ptr_Free, self.latlonQuad

    ; Call the superclass object's CLEANUP method
    self -> cgKML_Overlay::Cleanup
    
END


;+
; The cgKML_GroundOverlay class definition module. It is a container object 
; and represents part of the KML file hierarchy.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_GroundOverlay__Define, class

   class = { cgKML_GroundOverlay, $
             INHERITS cgKML_Overlay, $    ; An extended cgOverlay object.
             altitude: 0.0D, $
             altmode: "", $
             latlonbox: Ptr_New(), $
             latlonquad: Ptr_New(), $
             rotation: 0.0 $
           }
 
END