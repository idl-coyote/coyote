;+
; The purpose of this method is to establish an abstract KML Object class. For reference, see
; https://developers.google.com/kml/documentation/kmlreference#object.
;
; :Params:
;     id: in, optional, type=string
;         An ID attribute which allows unique identification of a KML element. For reference, see
;         https://developers.google.com/kml/documentation/kmlreference#object. The ID attribute must
;         be assigned in the <Update> mechanism is to be used.
;         
; :Keywords:
;     targetid: in, optional, type=string
;         A targetID attribute which is used to reference objects that have already been loaded
;         into Google Earch. For reference, see
;         https://developers.google.com/kml/documentation/kmlreference#object.
;     _ref_extra: in, optional
;         Any keywords appropriate for superclass objects may be passed into the program.
;-
FUNCTION cgKML_Object::INIT, id, TARGETID=targetID, _REF_EXTRA=extra

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Call the superclass object's INIT method.
  IF ~self -> cgContainer::INIT(_Strict_Extra=extra) THEN RETURN, 0
  
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
PRO cgKML_Object::Build, LUN=lun
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
FUNCTION cgKML_Object::GetParent
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
PRO cgKML_Object::GetProperty, $
   ID=id, $
   PARENT=parent, $
   TARGETID=targetID, $
   _REF_EXTRA=extra
   
  IF Arg_Present(id) THEN id = self.id
  IF Arg_Present(parent) THEN parent = self.parent
  IF Arg_Present(targetID) THEN targetID = self.targetID   
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgCONTAINER::GetProperty, _Strict_Extra=extra
   
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
PRO cgKML_Object::SetProperty, $
   ID=id, $
   PARENT=parent, $
   TARGETID=targetID, $
   _REF_EXTRA=extra
   
  IF N_Elements(id) NE 0 THEN self.id = id 
  IF N_Elements(parent) NE 0 THEN self.parent = parent 
  IF N_Elements(targetID) NE 0 THEN self.targetID = targetID 
   
   ; Superclass keywords.
   IF N_Elements(extra) NE 0 THEN self -> cgCONTAINER::GetProperty, _Strict_Extra=extra
   
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_Object::CLEANUP

    ; Destroy all the objects in this container.
    childObjs = self -> Get(/ALL, COUNT=count)
    FOR j=0,count-1 DO Obj_Destroy, childObjs
    
    ; Call the superclass object's CLEANUP method
    self -> cgContainer::Cleanup
    
END


;+
; The cgKML_OBJECT class definition module. This is a mostly abstract class
; that will be inherited by cgKML_Feature and cgKML_Geometery objects (among others).
; Basically, any KML element that should be added to a KML file will inherit this
; object. It is a container object and represents part of the KML file hierarchy.
; Only objects of cgKML_Object class can be added to cgKML_File objects.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_Object__Define, class

   class = { cgKML_OBJECT, $
             INHERITS cgContainer, $   ; A modified IDL_CONTAINER.
             ID: "", $                 ; The "identifier" of this element.
             parent: Obj_New(), $      ; The "parent" object.
             targetID: "" $            ; The targetID reference.
           }
 
END