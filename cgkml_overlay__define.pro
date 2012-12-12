; docformat = 'rst'
;
; NAME:
;   cgKML_Overlay
;
; PURPOSE:
;   This program implements the KML abstract OBJECT class.
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
;   This program implements the KML abstract OVERLAY class. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#overlay>`.
;   An Overlay object is a basic KML class, and should be inherited by,
;   for example, a cgKML_GroundOverlay object, which extends and implements 
;   this KML abstract class.
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
; The purpose of this method is to establish an abstract KML Overlay class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference?hl=fr#overlay.
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
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_Overlay::INIT, $
  COLOR=color, $
  DRAWORDER=draworder, $
  HREF=href, $
  _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Call the superclass object's INIT method.
  IF ~self -> cgKML_Feature::INIT(_Extra=extra) THEN RETURN, 0
  
  ; Load the object.
  IF N_Elements(color) NE 0 THEN self.color = color
  IF N_Elements(draworder) NE 0 THEN self.draworder = draworder
  IF N_Elements(href) NE 0 THEN self.href = href

  RETURN, 1

END


;+
; This method adds Overlay elements to the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_Overlay::Body, LUN=lun
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
  IF self.color NE "" THEN self -> XMLTag, 'color', self.color, LUN=lun, SPACE=6
  self -> XMLTag, 'drawOrder', self.drawOrder, LUN=lun, SPACE=6
  PrintF, lun, '      <Icon>
  self -> XMLTag, 'href', self.href, LUN=lun, SPACE=9
  PrintF, lun, '      </Icon>'
  
  
END


;+
; This BUILD method is an abstract method that should be overridden by any
; object that you intend to add to a KML file. It is used to build the
; actual KML code for the element in question.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_Overlay::Build, LUN=lun

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  self -> cgKML_Feature::Build, LUN=lun
  self -> cgKML_Overlay::Body, LUN=lun
  
  
END


;+
; The purpose of this method is to return object properties.
; 
; :Keywords:
;     color: out, optional, type=string
;         Color values are expressed in hexadecimal notation, including opacity (alpha) values. 
;         The order of expression is alpha, blue, green, red (aabbggrr). The range of values for 
;         any one color is 0 to 255 (00 to ff). For opacity, 00 is fully transparent and ff is 
;         fully opaque. For example, if you want to apply a blue color with 50 percent opacity 
;         to an overlay, you would specify the following: COLOR = "7fff0000".
;     draworder: out, optional, type=integer
;         This element defines the stacking order for the images in overlapping overlays. 
;         Overlays with higher values are drawn on top of overlays with lower values.
;     href: out, optional, type=string
;         A URL that identifies the location of the image associated with this Overlay. The
;         location can be either a local file (e.g., 'myimage.png') or a URL to an image
;         on a web server (e.g., 'http://www.idlcoyote.com/images/myimage.png').
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_Overlay::GetProperty, $
   COLOR=color, $
   DRAWORDER=draworder, $
   HREF=href, $
   _REF_EXTRA=extra
   
  IF Arg_Present(color) THEN color = self.color
  IF Arg_Present(draworder) THEN draworder = self.draworder
  IF Arg_Present(href) THEN href = self.href   
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Feature::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to set object properties.
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
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_Overlay::SetProperty, $
   COLOR=color, $
   DRAWORDER=draworder, $
   HREF=href, $
   _REF_EXTRA=extra
   
  IF N_Elements(color) NE 0 THEN self.color = color 
  IF N_Elements(draworder) NE 0 THEN self.draworder = draworder 
  IF N_Elements(href) NE 0 THEN self.href = href 
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Feature::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_Overlay::CLEANUP

    ; Call the superclass object's CLEANUP method
    self -> cgKML_Feature::Cleanup
    
END


;+
; The cgKML_Overlay class definition module. This is an abstract class
; that will be inherited by overlay objects (GroundOverlay, and ScreenOverlay, among others).
; Basically, any KML element that should be added to a KML file will inherit this
; object. It is a container object and represents part of the KML file hierarchy.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_Overlay__Define, class

   class = { cgKML_Overlay, $
             INHERITS cgKML_Feature, $    ; An extended cgFeature object.
             color: "", $
             drawOrder: 0, $
             href: "" $               ; Nested in <Icon> element.      
           }
 
END