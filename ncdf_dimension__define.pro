;+
; NAME:
;       NCDF_DIMENSION
;
; PURPOSE:
;
;       The pupose of this NCDF_Dimension object is to store information about
;       a netCDF dimension. The object is principally used as a utility routine 
;       for the NCDF_FILE object. Given the dimension name, the object will 
;       acquire additional information about the dimension from the netCDF file 
;       containing the dimension.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;       File I/O
;
; CALLING SEQUENCE:
;
;       IDL> dimObj = Obj_New('NCDF_DIMENSION', dimName, parent)
;
; ARGUMENTS:
;
;       dimName:   The case sensitive name of a netCDF dimension that is stored in the 
;                  netCDF file. (Input and required.)
;
;       parent:    The object reference (NCDF_FILE object) of the netCDF file. In other words, the
;                  object reference of the file that contains this attribute. (Input and required.)
;
; KEYWORD PARAMETERS:
;       
;       None.
;
; METHODS:
;
;     The following methods are available. Each is documented in front of the method.
;
;     dimName = dimObject -> GetID()
;     dimName = dimObject -> GetName()
;     dimName = dimObject -> GetSize()
;     propertyValue = dimObject -> GetProperty(dimProperty)
;     dimValue = dimObject -> GetValue()
;     dimName = dimObject -> GetUnlimited()
;     dimObject -> ParseAttribute
;     
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 3 Feb 2010.
;-
;******************************************************************************************;
;  Copyright (c)2010, by Fanning Software Consulting, Inc.                                 ;
;  All rights reserved.                                                                    ;
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


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;    NCDF_Dimension::GetID                                                              
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns the ID of the dimension.                                   
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    dimID = obj -> GetID()                                             
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
; Return Value:                                                         
;                                                                       
;    dimID:  A long integer containing the dimension identifier.        
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetID
    RETURN, self.id
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;    NCDF_Dimension::GetName                                                            
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns the name of the dimension.                                 
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    dimName = obj -> GetName()                                         
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
; Return Value:                                                         
;                                                                       
;    dimName:  A string variable containing the dimension name.         
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetName
    RETURN, self.name
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;    NCDF_Dimension::GetSize                                                            
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns the size of the dimension.                                 
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    dimSize = obj -> GetSize()                                         
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
; Return Value:                                                         
;                                                                       
;    dimSize:  A long integer containing the dimension size.            
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetSize
    RETURN, self.size
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;                                                                       
;    NCDF_Dimension::GetUnlimited                                                       
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns a 1 if the dimension is unlimited, and a 0 otherwise.      
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    unlimitedFlag = obj -> GetUnlimited()                              
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
; Return Value:                                                         
;                                                                       
;    unlimitedFlag:  Set to 1 if the dimension is unlimited, and to 0 otherwise.           ;
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetUnlimited
    RETURN, self.unlimited
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;    NCDF_Dimension::GetProperty                                                        
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns various properties of the object one at a time. This is a shorthand and       ;
;    generic way to get the value of an object's "properties", which are defined as        ;
;    the IDL variables in the object's class structure.                 
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    propertyValue = obj -> GetProperty(thisProperty)                   
;                                                                       
; Auguments:                                                            
;                                                                       
;    thisProperty:   A string variable that is equivalent to a field in the object's       ;
;                    class structure. See the *__DEFINE procedure for which properties     ;
;                    can be returned. The property is case insensitive. 
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                              
;                                                                       
; Return Value:                                                         
;                                                                       
;    propertyValue:  The value of a particular object property. Note that pointer          ;
;                    properties will return the variable the pointer points to.            ;
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetProperty, thisProperty

    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL

        ; Log the error.
        (self.parent -> GetProperty('ERRORLOGGER')) -> AddError

        ; No success finding the property requested.
        RETURN, 0
    ENDIF
    
    ; Get the self structure as a structure, rather than as an object.
    Call_Procedure, StrLowCase(Obj_Class(self)) + '__define', classStruct

    ; Find the property in this class structure.
    index = Where(StrPos(Tag_Names(classStruct), StrUpCase(thisProperty)) EQ 0, count)
    index = index[0]
    
    ; What happened?
    CASE count OF
        0: Message, 'Property ' + StrUpCase(thisProperty) + ' could not be found.'
        1: propertyValue = self.(index)
        ELSE: Message, 'Ambiguous property. Use more characters to specify it.'
    ENDCASE

    ; If this is a pointer, you want the thing pointed to.
    IF Size(propertyValue, /TNAME) EQ 'POINTER' THEN propertyValue = *propertyValue
    RETURN, propertyValue
    
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:                                                                 
;    NCDF_Dimension::GetValue                                                           
;                                                                       
; Purpose:                                                              
;                                                                       
;    Returns the size of the dimension. Note this function is a pseudonym for the          ;
;    GetSize function. Used mostly for consistency across netCDF objects.                  ;
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    dimSize = obj -> GetValue()                                         
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
; Return Value:                                                         
;                                                                       
;    dimSize:  A long integer containing the dimension size.            
;                                                                       
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Dimension::GetValue
    RETURN, self -> GetSize()
END


;------------------------------------------------------------------------------------------;
;                                                                       
; NAME:
;    NCDF_Dimension::ParseDimension                                                     
;                                                                       
; Purpose:                                                              
;                                                                       
;    Gathers information about the dimension from the netCDF file.      
;                                                                       
; Method Syntax:                                                        
;                                                                       
;    dimObject -> ParseDimension                                        
;                                                                       
; Auguments:                                                            
;                                                                       
;    None.                                                              
;                                                                       
; Keywords:                                                             
;                                                                       
;    None.                                                                
;                                                                       
;------------------------------------------------------------------------------------------;
PRO NCDF_Dimension::ParseDimension

    On_Error, 2

    ; Gather information about the file.
    fileID = self.parent -> GetFileID()
    info = NCDF_Inquire(fileID)
   
   ; Is there an unlimited dimension in this file?
   IF info.recdim NE -1 THEN BEGIN
        NCDF_DIMINQ, fileID, info.recdim, unlimitedName, unlimitedSize
        unlimitedID = info.recdim
   ENDIF ELSE unlimitedName = ""

   ; Does this name match the dimension name?
   IF StrUpCase(unlimitedName) EQ StrUpCase(self.name) THEN BEGIN
        self.unlimited = 1
        self.size = unlimitedSize
        self.id = info.recdim
   ENDIF ELSE BEGIN
        self.id = NCDF_DIMID(fileID, self.name)
        NCDF_DIMINQ, fileID, self.id, dimName, dimSize
        self.size = dimSize
   ENDELSE
   
END



PRO NCDF_Dimension::CLEANUP
END


FUNCTION NCDF_Dimension::INIT, dimName, parent

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling. Return 0 if can't finish.
    CATCH, theError
    IF theError NE 0 THEN BEGIN
       CATCH, /CANCEL
        (self.parent -> GetProperty('ERRORLOGGER')) -> AddError
       RETURN, 0
    ENDIF

    ; Check parameters.
    IF N_Elements(dimName) EQ 0 THEN Message, 'An dimension name is required'
    IF Size(dimName, /TNAME) NE 'STRING' THEN Message, 'The dimension name must be a string.'
    self.name = dimName
    IF N_Elements(parent) EQ 0 THEN Message, 'A parent object is required.'
    IF Size(parent, /TNAME) NE 'OBJREF' THEN Message, 'The parent parameter must be an object reference.'
    self.parent = parent

    ; Obtain information about the dimension and fill out the dimension object.
    self -> ParseDimension
        
    RETURN, 1
    
END

PRO NCDF_Dimension__DEFINE, class

    class = { NCDF_DIMENSION, $
              name: "", $              ; The name of the dimension.
              ID: 0L, $                ; The dimension ID.
              parent: Obj_New(), $     ; The NCDF_FILE object where this variable resides. 
              unlimited: 0L, $         ; A flag that indicates this is an unlimited dimension.
              size: 0L $               ; The size of the dimension.
            }
          
END
