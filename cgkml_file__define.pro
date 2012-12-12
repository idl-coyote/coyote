; docformat = 'rst'
;
; NAME:
;   cgKML_File
;
; PURPOSE:
;   This program creates a KML file that can be displayed with Google Earth.
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
;   This program creates a KML file that can be displayed with Google Earth.
;   It implements some (not all) of the objects or elements allowed in KML files. For 
;   reference, see  the `Google KML Reference Documentation <https://developers.google.com/kml/documentation/kmlreference>`.
;
; :Categories:
;    Graphics, FileIO
;    
; :Examples:
;    To create an image overlay object::
;    
;      kml = Obj_New('cgKML_File', 'test.kml')
;      overlay = Obj_New('cgKML_GroundOverlay', $
;          HREF='myimage.tif', $
;          LATLONBOX=[])
;      kml -> Add, overlay
;      kml -> Save
;      kml -> Destroy
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
; The purpose of this method is to establish the name of a KML file that is to be written. Optionally,
; a hint attribute can be specified that indicated to Google Earth that is should switch to sky view or
; to another celestial body, such as Mars.
;
; :Params:
;     filename: in, optional, type=string
;         The name of the KML file to be created. The output file should have a file extension of *.kml.
;         
; :Keywords:
;     hint: in, optional, type=string
;         A hint attribute (http://simplekml.readthedocs.org/en/latest/kml.html). Hint attributes are
;         used as a signal to Google Earth to switch to sky view or to another celestial body, such as Mars.
;-
FUNCTION cgKML_File::INIT, filename, HINT=hint

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN, 0
  ENDIF
  
  ; Need a filename?
  IF N_Elements(filename) EQ 0 THEN BEGIN
     filename = cgPickfile(Filter='*.kml', Title='Select a KML Filename...', /WRITE)
     IF filename EQ "" THEN RETURN, 0
  ENDIF
    
  ; Call the superclass object's INIT method.
  IF ~self -> cgContainer::INIT(_Strict_Extra=extra) THEN RETURN, 0

  ; Load the object.
  self.filename = filename
  IF N_Elements(hint) NE 0 THEN self.hint = hint

  RETURN, 1

END


;+
; This method allows KML objects to be added to the KML file.
; 
; :Params:
;     theobject: in, required, type=object
;         Only objects of class cgKML_OBJECT can be added to a KML file object.
;-
PRO cgKML_File::Add, theObject

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; Error checking
  IF N_Elements(theObject) EQ 0 THEN Message, 'An object is required in this method.'
  IF Obj_Valid(theObject) EQ 0 THEN Message, 'The object to be added in invalid.'

  ; Add the object, if it is of the correct type.
  IF Obj_ISA(theObject, 'cgKML_OBJECT') THEN BEGIN
  
      ; Add yourself as the parent of this object.
      theObject -> SetProperty, PARENT=parent
      self -> IDL_CONTAINER::Add, theObject 
      
  ENDIF ELSE Message, 'Object must be of type cgKML_OBJECT to be added to a KML file.'
      
END


;+
; This method writes the contents of each KML object included in the file
; to the file in a recursive way.
; 
; :Keywords:
;     lun: in, required, type=integer
;         The logical unit number that the KML file is attached to.
;-
PRO cgKML_File::Body, LUN=lun

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; A logical unit number is required.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number must be supplied with the LUN keyword.'
  
  ; How many objects are in the container?
  numObjects = self -> Count()
  
  ; Call them one after the other.
  FOR j=0,numObjects-1 DO BEGIN
      thisObject = self -> Get(Position=j)
      IF Obj_Valid(thisObject) THEN BEGIN
         thisObject -> Build, LUN=lun
      ENDIF ELSE Message, 'Object ' + StrTrim(j,2) + ' is invalid.', /Informational
  ENDFOR

END


;+
; This method destroys the KML_File object.
;-
PRO cgKML_File::Destroy
   Obj_Destroy, self
END


;+
; This method allows the user to obtain properties of the object.
;
; :Keywords:
;     filename: out, optional, type=string
;         The name of the KML file being created.
;     hint: out, optional, type=string
;         The hint attribute that was input to the file via the INIT or SetProperty methods.
;     level: out, optional, type=integer
;         The heirarchtical level of this object.
;     
;-
PRO cgKML_File::GetProperty, $
  FILENAME=filename, $
  HINT=hint, $
  LEVEL=level, $
  _REF_EXTRA=extra

  IF Arg_Present(filename) THEN filename = self.filename
  IF Arg_Present(hint) THEN hint = self.hint
  IF Arg_Present(level) THEN level = self.level
  IF N_Elements(extra) NE 0 THEN self -> cgCONTAINER::GetProperty, _EXTRA=extra
  
END

;+
; This method opens the <kml> element and writes it to the file.
;
; :Keywords:
;     lun: in, required, type=integer
;         The logical unit number that the KML file is attached to.
;-
PRO cgKML_File::Head, LUN=lun

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; A logical unit number is required.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number must be supplied with the LUN keyword.'
  
  ; The opening xml tag. Google and Atom extensions are also present.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number must be supplied with LUN keyword.'
  PrintF, lun, '<?xml version="1.0" encoding="UTF-8"?>'
  PrintF, lun, '<kml xmlns="http://www.opengis.net/kml/2.2"'
  IF self.hint NE "" THEN  PrintF, lun, ' hint="target=' + self.hint  
  PrintF, lun, ' xmlns:gx="http://www.google.com/kml/ext/2.2"'
  PrintF, lun, ' xmlns:atom="http://www.w3.org/2005/Atom">'
  PrintF, lun, '<Document>'
  
END


;+
; This method converts a KML file to a KMZ file.
; 
;-
PRO cgKML_File::KML2KMZ

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  Message, 'The KML2KMZ method has not yet been implemented.'
 
END


;+
; This method saves the KML file and writes it to disk.
; 
; :Keywords:
;     include: in, optional, type=strarr
;         A string array of files and/or directories that should be included in the KMZ file
;         along with the KML file.
;     kmz: in, required, type=boolean, default=0
;         If this keyword is set, the KML file is zipped into a KMZ.
;-
PRO cgKML_File::Save, INCLUDE=include, KMZ=kmz

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; Open the file for writing.
  Get_Lun, lun
  OpenW, lun, self.filename
  
  ; Write the file contents.
  self -> Head, LUN=lun
  self -> Body, LUN=lun
  self -> Tail, LUN=lun
  
  ; Free up the logical unit number
  Free_Lun, lun
  
  ; Need to convert this to a KMZ file?
  IF Keyword_Set(kmz) THEN self -> KML2KMZ, include
  
END


;+
; This method allows the user to set properties of the object.
;
; :Keywords:
;     filename: in, optional, type=string
;         The name of the KML file being created.
;     hint: in, optional, type=string
;         A hint attribute (http://simplekml.readthedocs.org/en/latest/kml.html). Hint attributes are
;         used as a signal to Google Earth to switch to sky view or to another celestial body, such as Mars.     
;-
PRO cgKML_File::SetProperty, $
  FILENAME=filename, $
  HINT=hint, $
  _REF_EXTRA=extra

  IF Arg_Present(filename) THEN filename = self.filename
  IF Arg_Present(hint) THEN hint = self.hint
  IF N_Elements(extra) NE 0 THEN self -> cgCONTAINER::SetProperty, _EXTRA=extra
  
END


;+
; This method closes the <kml> element.
;
; :Keywords:
;     lun: in, required, type=integer
;         The logical unit number that the KML file is attached to.
;-
PRO cgKML_File::Tail, LUN=lun

  Compile_Opt idl2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
  
  ; A logical unit number is required.
  IF N_Elements(lun) EQ 0 THEN Message, 'A logical unit number must be supplied with the LUN keyword.'
  
  ; Close the element.
  PrintF, lun, '</Document>'
  PrintF, lun, '</kml>'
  
END


;+
; The purpose of this method is to destroy anything we created that may leak
; memory or need to be released from use.
;-
PRO cgKML_File::CLEANUP

    ; Destroy all the objects in this container.
    childObjs = self -> Get(/ALL, COUNT=count)
    FOR j=0,count-1 DO Obj_Destroy, childObjs
    
    ; Call the superclass object's CLEANUP method
    self -> cgContainer::Cleanup
    
END


;+
; The cgKML_FILE class definition module.
;
; :Params:
;     class: out, optional, type=structure
;         The class definition returned as a structure variable. Occassionally useful.
;-
PRO cgKML_File__Define, class

   Compile_Opt idl2
   
   class = { cgKML_FILE, $
             INHERITS cgContainer, $   ; A modified IDL_CONTAINER.
             filename: "", $           ; The name of the KML file being created.
             hint: "", $               ; A hint attribute for Google Earth.
             level: 0 $                ; The input level. Used to indent KML output.
           }
END