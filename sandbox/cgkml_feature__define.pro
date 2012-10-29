;+
; The purpose of this method is to establish an abstract KML Object class. For reference, see
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
;        User-sullied content that appears in the description balloon. Can be complicated. See the
;        KML Reference for "Feature", cited above.
;    extendeddata: in, optional, type=structure
;         A scalar or vector of anonymous structures containing a tag NAME and a tag VALUE, which
;         are both strings. An optional third field DISPLAYNAME can also be present. See the
;         <ExtendedData> reference (https://developers.google.com/kml/documentation/kmlreference#extendeddata)
;         for additional information.
;    name: in, optional, type=string
;       User-defined text displayed in the 3D viewer as the label for the object (eg., for a Placemark).
;    open: in, optional, type=boolean, default=0
;       This keyword specifies whether a Document or Folder appears closed or open when first loaded 
;       into the Places panel. The default is to show the Document or Folder collapsed rather than
;       expanded.
;    phonenumber: in, optional, type=string
;        A phone number. Used only by Google Maps Mobile.
;    snippet: in, optional, type=string
;        A short description of the feature. Restricted to two lines (may be StrArr of 2 elements). This
;        description is displayed in the Places panel under the name of the feature. If not supplied, the
;        first two lines of the `Description` are used.
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
   NAME=name, $
   OPEN=open, $
   PHONENUMBER=phonenumber, $
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
  IF ~self -> cgObject::INIT(_Strict_Extra=extra) THEN RETURN, 0
  
  ; Load the object.
  IF N_Elements(id) NE 0 THEN self.id = id
  IF N_Elements(targetID) NE 0 THEN self.targetID = targetID

  RETURN, 1

END


;+
; This BUILD method is an abstract method that should be overridden by any
; object that you intend to add to a KML file. It is used to build the
; actual KML code for the element in question.
;-
PRO cgKML_Feature::Build, LUN=lun
  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  Message, 'The BUILD method should be overridden by the object you are adding to the KML file.'
  
  ;self -> Head, LUN=lun
  ;self -> Body, LUN=lun
  ;self -> Tail, LUN=lun
  
END


;+
; The purpose of this function method is to return the parent object reference.
;-
FUNCTION cgKML_Feature::GetParent
   RETURN, self.parent
END


;+
; The purpose of this method is to return object properties.
; 
; :Keywords:
;     id: out, optional, type=string
;         The object ID.
;     parent: out, optional, type=objref
;         The parent object reference.
;     targetid: out, optional, type=string
;         The targetID of the object.
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_Feature::GetProperty, $
   ID=id, $
   PARENT=parent, $
   TARGETID=targetID, $
   _REF_EXTRA=extra
   
  IF Arg_Present(id) THEN id = self.id
  IF Arg_Present(parent) THEN parent = self.parent
  IF Arg_Present(targetID) THEN targetID = self.targetID   
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgObject::GetProperty, _Strict_Extra=extra
   
END


;+
; The purpose of this method is to set object properties.
; 
; :Keywords:
;     id: in, optional, type=string
;         The object ID.
;     parent: in, optional, type=objref
;         The parent object reference.
;     targetid: in, optional, type=string
;         The targetID of the object.
;     _ref_extra: out, optional
;         Any keywords for the superclass objects are allowed.
;-
PRO cgKML_Feature::SetProperty, $
   ID=id, $
   PARENT=parent, $
   TARGETID=targetID, $
   _REF_EXTRA=extra
   
  IF N_Elements(id) NE 0 THEN self.id = id 
  IF N_Elements(parent) NE 0 THEN self.parent = parent 
  IF N_Elements(targetID) NE 0 THEN self.targetID = targetID 
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgObject::GetProperty, _Strict_Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_Feature::CLEANUP

    ; Destroy all the objects in this container.
    childObjs = self -> Get(/ALL, COUNT=count)
    FOR j=0,count-1 DO Obj_Destroy, childObjs
    
    ; Call the superclass object's CLEANUP method
    self -> cgObject::Cleanup
    
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
             INHERITS cgKML_Object, $  ; A modified IDL_CONTAINER.
             name: "", $                 
             visibility: 0B, $      
             open: 0B, $
             author: "", $
             address: "", $
             phoneNumber: "", $
             snippet:StrArr(2), $
             description: "", $
             abstractView: Obj_New(), $
             timePrimitive: Obj_New(), $
             styleURL: "", $
             extendedData: "" $
           }
 
END