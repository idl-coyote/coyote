; docformat = 'rst'
;
; NAME:
;   cgKML_LookAt
;
; PURPOSE:
;   This program implements the KML abstract LOOKAT class.
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
;   This program implements the KML abstract LookAt class. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#lookat>`.
;   A LookAt object extends a KML Object. The purpose of this object is to allow
;   Google Earth to "fly to" a particular location.
;   
;
; :Categories:
;    Graphics, FileIO
;    
; :Examples:
;    See the `cgKML_File` object for examples of how to create a KML file.
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
;        Written, 31 December 2012 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-


;+
; The purpose of this method is to establish a KML LookAt object class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference#lookat.
;
; :Keywords:
;    altitudemode: in, optional, type=string, default="relativeToGround"
;         The mode of the altitude. Possible values are: "clampToGround", "relativeToGround"
;         and "absolute".
;    heading: in, optional, type=float, default=0.0
;         The angle of the view. Valid values 0 to 360.
;    height: in, optional, type=double, default=11000.0
;         The altitude (in km) of the eye with respect to the lat/lon point of the eye.
;         Called "altitude" in the KML documentation, but called "height" here to differentiate
;         keyword from "altitudeMode".  Positive values in kilometers.
;    latitude: in, optional, type=float, default=0.0
;         The input latitude where the eye should be located. Valid values -90 to 90.
;    longitude: in, optional, type=float, default=0.0 
;         The input longitude where the eye should be located. Valid values -180 to 180.
;    tilt: in, optional, type=float, default=0.0
;         The tilt angle of the eye. Valid values 0 to 90.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_LookAt::INIT, $
  ALTITUDEMODE=altitudeMode, $
  HEADING=heading, $
  HEIGHT=height, $
  LATITUDE=latitude, $
  LONGITUDE=longitude, $
  TILT=tilt, $
   _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Call the superclass object's INIT method.
  IF ~self -> cgKML_Object::INIT(_Strict_Extra=extra) THEN RETURN, 0
  
  ; Default values for keywords.
  IF N_Elements(altitudeMode) EQ 0 THEN altitudeMode = "relativeToGround"
  IF N_Elements(heading) EQ 0 THEN heading = 0.0
  heading = 0.0 > heading < 360.0
  IF N_Elements(height) EQ 0 THEN height = 11000 
  height = height * 1000.0
  IF N_Elements(latitude) EQ 0 THEN latitude = 0.0
  latitude = -90.0 > latitude < 90.0
  IF N_Elements(longitude) EQ 0 THEN longitude = 0.0
  longitude = -180.0 > longitude < 180.0
  IF N_Elements(tilt) EQ 0 THEN tilt = 0.0
  tilt = 0.0 > tilt < 90.0
  IF Size(altitudeMode, /TNAME) NE 'STRING' THEN Message, 'The altitudeMode keyword must be a string.'
  
  CASE 1 OF
      StrUpCase(altitudeMode) EQ 'RELATIVETOGROUND': altitudeMode = "relativeToGround"
      StrUpCase(altitudeMode) EQ 'CLAMPTOGROUND': altitudeMode = "clampToGround"
      StrUpCase(altitudeMode) EQ 'ABSOLUTE': altitudeMode = "absolute"
      ELSE: Message, 'Unknown altitude mode of : ' + altitudeMode
  ENDCASE
  
  ; Load the object.
  self.altitude = height
  self.altitudeMode = altitudeMode
  self.heading = heading
  self.latitude = latitude
  self.longitude = longitude
  self.tilt = tilt
  
  RETURN, 1

END


;+
; This method builds the feature in a KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_LookAt::Build, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
    
  ; Write the LookAt object into the file.
  self -> Head, LUN=lun
  self -> Body, LUN=lun
  self -> Tail, LUN=lun
  
END


;+
; This method adds LookAt elements to the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_LookAt::Body, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; We require a logical unit number.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number (LUN) is required in this method.'
    
  ; Write the lookAt elements.
  self -> XMLTag, 'longitude', self.longitude, LUN=lun, SPACE=6
  self -> XMLTag, 'latitude', self.latitude, LUN=lun, SPACE=6
  self -> XMLTag, 'altitude', self.altitude, LUN=lun, SPACE=6
  self -> XMLTag, 'heading', self.heading, LUN=lun, SPACE=6
  self -> XMLTag, 'tilt', self.tilt, LUN=lun, SPACE=6
  CASE StrUpCase(self.altitudeMode) OF
     'CLAMPTOGROUND': self -> XMLTag, 'altitudeMode', self.altitudeMode, LUN=lun, SPACE=6
     'RELATIVETOGROUND': self -> XMLTag, 'gx:altitudeMode', self.altitudeMode, LUN=lun, SPACE=6
     'ABSOLUTE': self -> XMLTag, 'altitudeMode', self.altitudeMode, LUN=lun, SPACE=6
     ELSE: Message, 'Altitude Mode (' + self.altitudeMode + ') is not recognized as valid.'
  ENDCASE
    
END

;+
; This method opens the <LookAt> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_LookAt::Head, LUN=lun
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
    PrintF, lun, '   <LookAt id="' + StrTrim(self.id,2) + '">'
  ENDIF ELSE BEGIN
    PrintF, lun, '   <LookAt>'
  ENDELSE
  
END

;+
; This method closes the <LookAt> tag in the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_LookAt::Tail, LUN=lun
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
  PrintF, lun, '   </LookAt>'
  
END


;+
; The purpose of this method is to set the object's properties.
; 
; :Keywords:
;    altitudemode: out, optional, type=string
;         The mode of the altitude. Possible values are: "clampToGround", "relativeToGround"
;         and "absolute".
;    heading: out, optional, type=float
;         The angle of the view. Valid values 0 to 360.
;    height: out, optional, type=double
;         The altitude (in km) of the eye with respect to the lat/lon point of the eye.
;         Called "altitude" in the KML documentation, but called "height" here to differentiate
;         keyword from "altitudeMode".
;    latitude: out, optional, type=float
;         The input latitude where the eye should be located. Valid values -90 to 90.
;    longitude: out, optional, type=float
;         The input longitude where the eye should be located.
;    tilt: out, optional, type=float
;         The tilt angle of the eye. Valid values 0 to 90.
;     _ref_extra: out, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
PRO cgKML_LookAt::GetProperty, $
  ALTITUDEMODE=altitudeMode, $
  HEADING=heading, $
  HEIGHT=height, $
  LATITUDE=latitude, $
  LONGITUDE=longitude, $
  TILT=tilt, $
   _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  IF N_Elements(altitudeMode) NE 0 THEN self.altitudeMode = altitudeMode
  IF N_Elements(heading) NE 0 THEN self.heading = heading
  IF N_Elements(height) NE 0 THEN self.altitude = height
  IF N_Elements(latitude) NE 0 THEN self.latitude = latitude
  IF N_Elements(longitude) NE 0 THEN self.longitude = longitude
  IF N_Elements(tilt) NE 0 THEN self.tilt = tilt
  
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Object::GetProperty, _Strict_Extra=extra

END


;+
; The purpose of this method is to set the object's properties.
; 
; :Keywords:
;    altitudemode: in, optional, type=string, default="relativeToGround"
;         The mode of the altitude. Possible values are: "clampToGround", "relativeToGround"
;         and "absolute".
;    heading: in, optional, type=float, default=0.0
;         The angle of the view. Valid values 0 to 360.
;    height: in, optional, type=double, default=500.0
;         The altitude (in km) of the eye with respect to the lat/lon point of the eye.
;         Called "altitude" in the KML documentation, but called "height" here to differentiate
;         keyword from "altitudeMode".  Positive values in kilometers.
;    latitude: in, optional, type=float, default=0.0
;         The input latitude where the eye should be located. Valid values -90 to 90.
;    longitude: in, optional, type=float, default=0.0 
;         The input longitude where the eye should be located. Valid values -180 to 180.
;    tilt: in, optional, type=float, default=0.0
;         The tilt angle of the eye. Valid values 0 to 90.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
PRO cgKML_LookAt::SetProperty, $
  ALTITUDEMODE=altitudeMode, $
  HEADING=heading, $
  HEIGHT=height, $
  LATITUDE=latitude, $
  LONGITUDE=longitude, $
  TILT=tilt, $
   _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  IF N_Elements(altitudeMode) NE 0 THEN self.altitudeMode = altitudeMode
  IF N_Elements(heading) NE 0 THEN self.heading = heading
  IF N_Elements(height) NE 0 THEN self.altitude = height
  IF N_Elements(latitude) NE 0 THEN self.latitude = latitude
  IF N_Elements(longitude) NE 0 THEN self.longitude = longitude
  IF N_Elements(tilt) NE 0 THEN self.tilt = tilt
  
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Object::GetProperty, _Strict_Extra=extra

END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_LookAt::CLEANUP

    ; Call the superclass object's CLEANUP method
    self -> cgKML_Object::Cleanup
    
END


;+
; The cgKML_LookAt class definition module. This is a class that allows the
; Google Earth application to "fly to" the position indicated.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_LookAt__Define, class

   class = { cgKML_LOOKAT, $
             INHERITS cgKML_Object, $  ; An extended cgKML_Object class.
             latitude: 0.0, $          ; -90 to 90
             longitude: 0.0, $         ; -180 to 180
             altitude: 0.0D, $         ; In meters
             heading: 0.0, $           ; 0 to 360
             tilt: 0.0, $              ; 0 to 90
             range: 0.0D, $            ; Currently unused.
             altitudeMode: "" $        ; "clampToGroupd", "relativeToGround", "absolute"       
           }
END
             