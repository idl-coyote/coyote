; docformat = 'rst'
;
; NAME:
;   cgKML_Object
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
;   This program implements the KML abstract OBJECT class. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference#object>`.
;   A Object object is a basic KML class, and should be inherited by,
;   for example, a cgKML_Feature object, which extends this KML abstract
;   class.
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
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
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
; This method writes an XML tag into the file. All parameters and
; keywords are required. Sequence is: <tag>value</tag>.
; 
; :Params:
;    tag: in, required, type='string'
;        The name of the XML tag to write to the file.
;    value: in, required
;        The value of the tag. May be any type of data that can be converted
;        to a string.
; 
; :Keywords:
;     lun: in, required, type=integer
;        The logical unit number of the open KML file to write to.
;     space
;-
PRO cgKML_Object::XMLTag, tag, value, LUN=lun, SPACE=space

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; All positional parameters are required.
  IF (N_Params() NE 2) THEN Message, 'Calling sequence: obj->XMLTag, tag, value, LUN=lun'
  
  ; LUN is required
  IF N_Elements(lun) EQ 0 THEN Message, 'Calling sequence: obj->XMLTag, tag, value, LUN=lun'
  
  ; Handle indenting.
  IF N_Elements(space) EQ 0 THEN BEGIN
     space = "" 
  ENDIF ELSE BEGIN
     space = String(BytArr(space) + 32B)
  ENDELSE
  PrintF, lun, space + '<' + tag + '>' + StrTrim(value, 2) + '</' + tag + '>'
  
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