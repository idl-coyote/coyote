; docformat = 'rst'
;
; NAME:
;   cgKML_Feature
;
; PURPOSE:
;   This program implements the KML abstract FEATURE class.
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
;   This program implements the KML abstract FEATURE class. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#feature>`.
;   A Feature object extends a KML Object, and both should be inherited by,
;   for example, a KML Overlay object, which further extends this KML abstract
;   class.
;   
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
;        Fixed typo in spelling of N_Elements. 16 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-


;+
; The purpose of this method is to establish a KML Feature object class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference#feature.
;
; :Keywords:
;    abstractview: in, optional, type=object
;        Defines a viewpoint associated with any element derived from Feature. Either a cgKML_Camera or
;        cgKML_LookAt object.
;    address: in, optional, type=string
;        A string value representing an unstructured address written as a standard street, city, 
;        state address, and/or as a postal code. You can use the <address> tag to specify the 
;        location of a point instead of using latitude and longitude coordinates. (However, if 
;        a <Point> is provided, it takes precedence over the <address>.)
;    author: in, optional, type=struct
;        An anonymous structure with two tags. Tag "NAME" is a string that gives the author's name,
;        and tag "LINK" is a string that provides a URL to the author's web site.
;    description: in, optional, type=string
;        User-supplied content that appears in the description balloon. Can be complicated. See the
;        KML Reference for "Feature", cited above.
;    extendeddata: in, optional, type=structure
;         A scalar or vector of anonymous structures containing a tag NAME and a tag VALUE, which
;         are both strings. An optional third field DISPLAYNAME can also be present. See the
;         <ExtendedData> reference (https://developers.google.com/kml/documentation/kmlreference#extendeddata)
;         for additional information.
;    open: in, optional, type=boolean, default=0
;       This keyword specifies whether a Document or Folder appears closed or open when first loaded 
;       into the Places panel. The default is to show the Document or Folder collapsed rather than
;       expanded.
;    phonenumber: in, optional, type=string
;        A phone number. Used only by Google Maps Mobile.
;    placename: in, optional, type=string
;        This is the <name> element in a Feature object. It is user-defined text that is used as
;        the label for an object in Google Earth.
;    snippet: in, optional, type=string
;        A short description of the feature. This description is displayed in the Places panel 
;        under the name of the feature. If not supplied, the first two lines of the `Description` 
;        are used.
;    styleurl: in, optional, type=string
;        The URL of a <Style> or <StyleMap> defined in a Document. If the style is in the same 
;        file, use a # reference. If the style is defined in an external file, use a full URL 
;        along with # referencing.
;    timeprimitive: in, optional, type=object
;        Associates this feature with a period of time (cgKML_Timespan object) or a point in time
;        (cgKML_Timestamp object).
;    visibility: in, optional, type=boolean, default=1
;       This keyword specifies whether the feature is drawn in the 3D viewer when it is initially loaded. In 
;       order for a feature to be visible, the <visibility> tag of all its ancestors must also be 
;       set to 1. In the Google Earth List View, each Feature has a checkbox that allows the user 
;       to control visibility of the Feature.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_Feature::INIT, $
   ABSTRACTVIEW=abstractView, $
   ADDRESS=address, $
   AUTHOR=author, $
   DESCRIPTION=description, $
   EXTENDEDDATA=extendedData, $
   OPEN=open, $
   PHONENUMBER=phonenumber, $
   PLACENAME=placename, $
   SNIPPET=snippet, $
   STYLEURL=styleURL, $
   TIMEPRIMITIVE=timeprimitive, $
   VISIBILITY=visibility, $
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
  IF N_Elements(visibility) EQ 0 THEN visibility = 1
  
  ; Load the object.
  IF Obj_Valid(abstractView) THEN self.abstractView = abstractView
  IF N_Elements(address) NE 0 THEN self.address = address
  IF N_Elements(author) NE 0 THEN self.author = Ptr_New(author)
  IF N_Elements(description) NE 0 THEN self.description = description
  IF N_Elements(extendedData) NE 0 THEN self.extendedData = Ptr_New(extendedData)
  IF N_Elements(open) NE 0 THEN self.open = open
  IF N_Elements(phonenumber) NE 0 THEN self.phonenumber = phonenumber
  IF N_Elements(placename) NE 0 THEN self.placename = placename
  IF N_Elements(snippet) NE 0 THEN BEGIN
     CASE N_Elements(snippet) OF
        1: self.snippet[0] = snippet
        2: self.snippet = snippet
        ELSE: Message, 'The SNIPPET attribute is restricted to two lines of input.'
     ENDCASE
  ENDIF
  IF N_Elements(styleURL) NE 0 THEN self.styleURL = styleURL
  IF Obj_Valid(timeprimitive) THEN self.timeprimitive = timeprimitive
  IF N_Elements(visibility) NE 0 THEN self.visibility = visibility
  
  RETURN, 1

END


;+
; This method adds Feature elements to the KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_Feature::Body, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; We require a logical unit number.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number (LUN) is required in this method.'
    
  ; Write the feature elements.
  IF self.placeName NE "" THEN self -> XMLTag, 'name', self.placeName, LUN=lun, SPACE=6
  self -> XMLTag, 'visibility', Keyword_Set(self.visibility), LUN=lun, SPACE=6
  self -> XMLTag, 'open', Keyword_Set(self.open), LUN=lun, SPACE=6
  IF Ptr_Valid(self.author) THEN BEGIN
    PrintF, lun, '      <atom:author>'
    PrintF, lun, '         <atom:name>' + (*self.author).NAME + '</atom:name>
    PrintF, lun, '      </atom:author>'
    IF (*self.author).LINK NE "" THEN BEGIN
       PrintF, lun, '      <atom:link href="' + (*self.author).LINK + '" />'
    ENDIF
  ENDIF
  IF self.address NE "" THEN self -> XMLTag, 'address', self.address, LUN=lun, SPACE=6
  IF self.phoneNumber NE "" THEN self -> XMLTag, 'phoneNumber', self.phoneNumber, LUN=lun, SPACE=6
  CASE (Max(StrLen(self.snippet)) GT 1) OF
      0:
      ELSE: BEGIN
         nlines = N_Elements(self.snippet)
         PrintF, lun, '<Snippet maxLines="' + StrTrim(nlines,2) + '">'
         FOR j=0,nlines-1 DO PrintF, lun, '   ' + self.snippet[j]
         PrintF, lun, '</Snippet>'
      END
  ENDCASE
  IF self.description NE "" THEN self -> XMLTag, 'description', self.description, LUN=lun, SPACE=6
  IF Obj_Valid(self.AbstractView) THEN self.AbstractView -> Build, LUN=lun
  IF Obj_Valid(self.TimePrimitive) THEN self.TimePrimitive -> Build, LUN=lun
  IF self.styleURL NE "" THEN self -> XMLTag, 'styleURL', self.styleURL, LUN=lun, SPACE=6
  IF Ptr_Valid(self.ExtendedData) THEN BEGIN
    PrintF, lun, '      <ExtendedData>'
    FOR j=0,N_Elements(*self.ExtendedData)-1 DO BEGIN
       thisStruct = (*self.ExtendedData)[j]
       IF thisStruct.name EQ "" THEN BEGIN
          PrintF, lun, '         <Data>' 
       ENDIF ELSE BEGIN
          PrintF, lun, '         <Data name="' + thisStruct.name + '">
       ENDELSE
       IF thisStruct.displayName NE "" THEN BEGIN
          self -> XMLTag, 'displayName', thisStruct.displayName, LUN=lun, SPACE=12
       ENDIF
       self -> XMLTag, 'value', thisStruct.value, LUN=lun, SPACE=12
       PrintF, lun, '         </Data>'
    ENDFOR
    PrintF, lun, '      </ExtendedData>'
  ENDIF

  
END


;+
; This method builds the feature in a KML file.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;-
PRO cgKML_Feature::Build, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
    
  ; Only the body can be written, since there is no head or tail in a feature object.
  self -> cgKML_Feature::Body, LUN=lun
  
END



;+
; The purpose of this method is to return object properties.
; 
; :Keywords:
;    abstractview: out, optional, type=object
;        Defines a viewpoint associated with any element derived from Feature. Either a cgKML_Camera or
;        cgKML_LookAt object.
;    address: out, optional, type=string
;        A string value representing an unstructured address written as a standard street, city, 
;        state address, and/or as a postal code. You can use the <address> tag to specify the 
;        location of a point instead of using latitude and longitude coordinates. (However, if 
;        a <Point> is provided, it takes precedence over the <address>.)
;    author: out, optional, type=struct
;        An anonymous structure with two tags. Tag "NAME" is a string that gives the author's name,
;        and tag "LINK" is a string that provides a URL to the author's web site.
;    description: out, optional, type=string
;        User-sullied content that appears in the description balloon. Can be complicated. See the
;        KML Reference for "Feature", cited above.
;    extendeddata: out, optional, type=structure
;         A scalar or vector of anonymous structures containing a tag NAME and a tag VALUE, which
;         are both strings. An optional third field DISPLAYNAME can also be present. See the
;         <ExtendedData> reference (https://developers.google.com/kml/documentation/kmlreference#extendeddata)
;         for additional information.
;    open: out, optional, type=boolean, default=0
;       This keyword specifies whether a Document or Folder appears closed or open when first loaded 
;       into the Places panel. The default is to show the Document or Folder collapsed rather than
;       expanded.
;    phonenumber: out, optional, type=string
;        A phone number. Used only by Google Maps Mobile.
;    placename: out, optional, type=string
;        This is the <name> element in a Feature object. It is user-defined text that is used as
;        the label for an object in Google Earth.
;    snippet: in, optional, type=string
;        A short description of the feature. This description is displayed in the Places panel 
;        under the name of the feature. If not supplied, the first two lines of the `Description` 
;        are used.
;    styleurl: out, optional, type=string
;        The URL of a <Style> or <StyleMap> defined in a Document. If the style is in the same 
;        file, use a # reference. If the style is defined in an external file, use a full URL 
;        along with # referencing.
;    timeprimitive: out, optional, type=object
;        Associates this feature with a period of time (cgKML_Timespan object) or a point in time
;        (cgKML_Timestamp object).
;    visibility: out, optional, type=boolean, default=1
;       This keyword specifies whether the feature is drawn in the 3D viewer when it is initially loaded. In 
;       order for a feature to be visible, the <visibility> tag of all its ancestors must also be 
;       set to 1. In the Google Earth List View, each Feature has a checkbox that allows the user 
;       to control visibility of the Feature.
;     _ref_extra: out, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
PRO cgKML_Feature::GetProperty, $
   ABSTRACTVIEW=abstractView, $
   ADDRESS=address, $
   AUTHOR=author, $
   DESCRIPTION=description, $
   EXTENDEDDATA=extendedData, $
   OPEN=open, $
   PLACENAME=placename, $
   PHONENUMBER=phonenumber, $
   SNIPPET=snippet, $
   STYLEURL=styleURL, $
   TIMEPRIMITIVE=timeprimitive, $
   VISIBILITY=visibility, $
   _REF_EXTRA=extra
   
   IF Arg_Present(abstractView) THEN abstractView = self.abstractView
   IF Arg_Present(address) THEN address = self.address
   IF Arg_Present(author) THEN IF Ptr_Valid(author) $
      THEN author = *self.author ELSE author = self.author
   IF Arg_Present(description) THEN description = self.description
   IF Arg_Present(extendedData) THEN IF Ptr_Valid(extendedData) $
      THEN extendedData = *self.extendedData ELSE extendedData = self.extendedData
   IF Arg_Present(open) THEN open = self.open
   IF Arg_Present(phonenumber) THEN phonenumber = self.phonenumber
   IF Arg_Present(placename) THEN placename = self.placename
   IF Arg_Present(snippet) THEN snippet = self.snippet
   IF Arg_Present(styleURL) THEN styleURL = self.styleURL
   IF Arg_Present(timeprimitive) THEN timeprimitive = self.timeprimitive
   IF Arg_Present(visibility) THEN visibility = self.visibility
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Object::GetProperty, _Extra=extra
   
END


;+
; The purpose of this method is to set object properties.
; 
; :Keywords:
;    abstractview: in, optional, type=object
;        Defines a viewpoint associated with any element derived from Feature. Either a cgKML_Camera or
;        cgKML_LookAt object.
;    address: in, optional, type=string
;        A string value representing an unstructured address written as a standard street, city, 
;        state address, and/or as a postal code. You can use the <address> tag to specify the 
;        location of a point instead of using latitude and longitude coordinates. (However, if 
;        a <Point> is provided, it takes precedence over the <address>.)
;    author: in, optional, type=struct
;        An anonymous structure with two tags. Tag "NAME" is a string that gives the author's name,
;        and tag "LINK" is a string that provides a URL to the author's web site.
;    description: in, optional, type=string
;        User-sullied content that appears in the description balloon. Can be complicated. See the
;        KML Reference for "Feature", cited above.
;    extendeddata: in, optional, type=structure
;         A scalar or vector of anonymous structures containing a tag NAME and a tag VALUE, which
;         are both strings. An optional third field DISPLAYNAME can also be present. See the
;         <ExtendedData> reference (https://developers.google.com/kml/documentation/kmlreference#extendeddata)
;         for additional information.
;    open: in, optional, type=boolean, default=0
;       This keyword specifies whether a Document or Folder appears closed or open when first loaded 
;       into the Places panel. The default is to show the Document or Folder collapsed rather than
;       expanded.
;    phonenumber: in, optional, type=string
;        A phone number. Used only by Google Maps Mobile.
;    placename: in, optional, type=string
;        This is the <name> element in a Feature object. It is user-defined text that is used as
;        the label for an object in Google Earth.
;    snippet: in, optional, type=string
;        A short description of the feature. This description is displayed in the Places panel 
;        under the name of the feature. If not supplied, the first two lines of the `Description` 
;        are used.
;    styleurl: in, optional, type=string
;        The URL of a <Style> or <StyleMap> defined in a Document. If the style is in the same 
;        file, use a # reference. If the style is defined in an external file, use a full URL 
;        along with # referencing.
;    timeprimitive: in, optional, type=object
;        Associates this feature with a period of time (cgKML_Timespan object) or a point in time
;        (cgKML_Timestamp object).
;    visibility: in, optional, type=boolean, default=1
;       This keyword specifies whether the feature is drawn in the 3D viewer when it is initially loaded. In 
;       order for a feature to be visible, the <visibility> tag of all its ancestors must also be 
;       set to 1. In the Google Earth List View, each Feature has a checkbox that allows the user 
;       to control visibility of the Feature.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
PRO cgKML_Feature::SetProperty, $
   ABSTRACTVIEW=abstractView, $
   ADDRESS=address, $
   AUTHOR=author, $
   DESCRIPTION=description, $
   EXTENDEDDATA=extendedData, $
   OPEN=open, $
   PHONENUMBER=phonenumber, $
   PLACENAME=placename, $
   SNIPPET=snippet, $
   STYLEURL=styleURL, $
   TIMEPRIMITIVE=timeprimitive, $
   VISIBILITY=visibility, $
   _REF_EXTRA=extra
   
  IF N_Elements(abstractView) NE 0 THEN BEGIN
    Obj_Destroy, self.abstractView
    self.abstractView = abstractView 
  ENDIF
  IF N_Elements(timeprimitive) NE 0 THEN BEGIN
    Obj_Destroy, self.timeprimitive
    self.timeprimitive = timeprimitive 
  ENDIF
  IF N_Elements(author) NE 0 THEN IF Ptr_Valid(self.author) $
     THEN *self.author = author ELSE self.author = Ptr_New(author)
  IF N_Elements(extendedData) NE 0 THEN IF Ptr_Valid(self.extendedData) $
     THEN *self.extendedData = extendedData ELSE self.extendedData = Ptr_New(extendedData)
  IF N_Elements(description) NE 0 THEN self.description = description 
  IF N_Elements(open) NE 0 THEN self.open = open 
  IF N_Elements(phonenumber) NE 0 THEN self.phonenumber = phonenumber 
  IF N_Elements(placename) NE 0 THEN self.placename = placename 
  IF N_Elements(snippet) NE 0 THEN self.snippet = snippet 
  IF N_Elements(styleURL) NE 0 THEN self.styleURL = styleURL 
  IF N_Elements(visibility) NE 0 THEN self.visibility = visibility 
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgKML_Object::GetProperty, _Strict_Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_Feature::CLEANUP

    Ptr_Free, self.author
    Ptr_Free, self.extendedData
    Obj_Destroy, self.abstractView
    Obj_Destroy, self.timePrimitive
    
    ; Call the superclass object's CLEANUP method
    self -> cgKML_Object::Cleanup
    
END


;+
; The cgKML_Feature class definition module. This is a mostly abstract class
; that will be inherited by cgKML_Overlay and cgKML_Container objects (among others).
; Basically, any KML element that should be added to a KML file will inherit this
; object. It is a container object and represents part of the KML file hierarchy.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_Feature__Define, class

   class = { cgKML_FEATURE, $
             INHERITS cgKML_Object, $  ; An extended cgKML_Object class.
             author: Ptr_New(), $                 
             visibility: 0B, $      
             open: 0B, $
             address: "", $
             phoneNumber: "", $
             placeName: "", $
             snippet:StrArr(2), $
             description: "", $
             abstractView: Obj_New(), $
             timePrimitive: Obj_New(), $
             styleURL: "", $
             extendedData: Ptr_New() $
           }
 
END