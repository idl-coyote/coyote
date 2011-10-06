;+
; NAME:
;       NCDF_ATTRIBUTE
;
; PURPOSE:
;
;       The pupose of this NCDF_Attribute object is to store information about
;       a netCDF global or variable attribute. The object is principally used
;       as a utility routine for the NCDF_FILE object. Given the attribute name,
;       the object will acquire additional information about the attribute from
;       the netCDF file containing the attribute.
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
;       IDL> attrObj = Obj_New('NCDF_ATTRIBUTE', attrName, parent, VARNAME=varName)
;
; ARGUMENTS:
;
;       attrName:  The case sensitive name of a netCDF attribute that is stored in the 
;                  netCDF file. (Input and required.)
;
;       parent:    The object reference (NCDF_FILE object) of the netCDF file. In other words, the
;                  object reference of the file that contains this attribute. (Input and required.)
;
; KEYWORD PARAMETERS:
;       
;       varName:   If this is a variable attribute, this is the case sensitive name of the
;                  variable that the attribute is attached to. (Input and required for variable
;                  attributes.) Note that a variable object reference may be used in place of the
;                  variable name.
;
; METHODS:
;
;     The following methods are available. Each is documented in front of the method.
;
;     attrName = attrObject -> GetName()
;     propertyValue = attrObject -> GetProperty(attrProperty)
;     attrValue = attrObject -> GetValue()
;     attrObject -> ParseAttribute
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
;-----------------------------------------------------------------------------------------;
;                                                                        
; NAME:                                                                  
;    NCDF_Attribute::GetName                                                             
;                                                                        
; Purpose:                                                               
;                                                                        
;    Returns the name of the attribute.                                  
;                                                                        
; Method Syntax:                                                         
;                                                                        
;    attrName = obj -> GetName()                                         
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
;    attrName:  A string variable containing the attribute name.         
;                                                                        
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Attribute::GetName
    RETURN, self.name
END


;-----------------------------------------------------------------------------------------;
;                                                                        
; NAME:                                                                  
;    NCDF_Attribute::GetProperty                                                         
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
FUNCTION NCDF_Attribute::GetProperty, thisProperty

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

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
    
    ; Strings are stored as bytes in netCDF files, so convert these back to strings.
    IF (Size(propertyValue, /TNAME) EQ 'BYTE') && (self.datatype EQ 'CHAR') THEN BEGIN
        propertyValue = String(propertyValue)
    ENDIF
    
    ; Make sure you return scalars, if needed.
    IF N_Elements(propertyValue) EQ 1 THEN propertyValue = propertyValue[0]

    RETURN, propertyValue
    
END


;-----------------------------------------------------------------------------------------;
;                                                                        
; NAME:                                                                  
;    NCDF_Attribute::GetValue                                                            
;                                                                        
; Purpose:                                                               
;                                                                        
;    Returns the value of the attribute.                                 
;                                                                        
; Method Syntax:                                                         
;                                                                        
;    attrValue = obj -> GetValue(DATATYPE=datatype)                      
;                                                                        
; Auguments:                                                             
;                                                                        
;    None.                                                               
;                                                                        
; Keywords:                                                              
;                                                                        
;    DATATYPE:    An output keyword that contains the netCDF data type of the attribute.   ;  
;                                                                        
; Return Value:                                                          
;                                                                        
;    attrValue:  A variable containing the attribute's value.            
;                                                                        
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Attribute::GetValue, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    On_Error, 2

    ; Get the value.
    theValue = *self.value
    datatype = self.datatype
    
    ; Strings are stored as bytes in netCDF files, so convert these back to strings.
    IF (Size(theValue, /TNAME) EQ 'BYTE') && (self.datatype EQ 'CHAR') THEN BEGIN
        theValue = String(theValue)
    ENDIF
    
    ; Make sure you return scalars, if needed.
    IF N_Elements(theValue) EQ 1 THEN theValue = theValue[0]
    
    RETURN, theValue
    
END


;-----------------------------------------------------------------------------------------;
;                                                                        
; NAME:                                                                  
;    NCDF_Attribute::ParseAttribute                                                      
;                                                                        
; Purpose:                                                               
;                                                                        
;    Gathers information about the attribute from the netCDF file.       
;                                                                        
; Method Syntax:                                                         
;                                                                        
;    attrObject -> ParseAttribute                                        
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
PRO NCDF_Attribute::ParseAttribute

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    On_Error, 2

    ; Gather information about the attribute.
    fileID = self.parent -> GetFileID()
    IF (self.varID GE 0) THEN BEGIN
        attrInfo = NCDF_AttInq(fileID, self.varID, self.name)
        NCDF_AttGet, fileID, self.varID, self.name, value
    ENDIF ELSE BEGIN
        attrInfo = NCDF_AttInq(fileID, GLOBAL=1, self.name)
        NCDF_AttGet, fileID, self.name, value, GLOBAL=1
    ENDELSE
    self.datatype = StrUpCase(attrInfo.datatype)
    self.length = attrInfo.length
    self.value = Ptr_New(value, /NO_COPY)
        
END


PRO NCDF_Attribute::CLEANUP

    Ptr_Free, self.value

END



FUNCTION NCDF_Attribute::INIT, attrName, parent, VARNAME=varName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling. Return 0 if can't finish.
    CATCH, theError
    IF theError NE 0 THEN BEGIN
       CATCH, /CANCEL
       errorLogger = parent -> GetProperty('ERRORLOGGER')
       errorLogger -> AddError
       RETURN, 0
    ENDIF

    ; Check parameters.
    IF N_Elements(attrName) EQ 0 THEN Message, 'An attribute name is required'
    IF Size(attrName, /TNAME) NE 'STRING' THEN $
        Message, 'The attribute name must be a string.'
    self.name = attrName
    IF N_Elements(parent) EQ 0 THEN Message, 'A parent object is required.'
    IF Size(parent, /TNAME) NE 'OBJREF' THEN $
        Message, 'The parent parameter must be an object reference.'
    self.parent = parent
    
    ; Check to see what you got as a variable reference.
    IF N_Elements(varName) NE 0 THEN BEGIN
        CASE Size(varName, /TNAME) OF
        
            'STRING': BEGIN
                check = self.parent -> HasVar(varName, OBJECT=varObj)
                IF check THEN BEGIN
                    varID = varObj -> GetID()
                ENDIF ELSE Message, 'Cannot find a variable named "' + $
                    varName + '" in the file.'
                END
            'OBJREF': BEGIN
                varObj = varName
                varID = varObj -> GetID()
                END
            ELSE: BEGIN ; Treat as the variable ID.
                varID = varName
                END
        ENDCASE
    ENDIF
    
    ; Store the variable ID associated with this attribute, if there is one.
    IF N_Elements(varID) NE 0 THEN BEGIN
        self.varID = varID
        self.global = 0
     ENDIF ELSE BEGIN
        self.varID = -1
        self.global = 1
     ENDELSE
    
    ; Obtain information about the attribute and fill out the attribute object.
    self -> ParseAttribute
    
    RETURN, 1
    
END

PRO NCDF_Attribute__DEFINE, class
    
    class = { NCDF_ATTRIBUTE, $
              name: "", $             ; The attribute name.
              parent: Obj_New(), $    ; The NCDF_FILE object where this attribute resides.
              global: 0B, $           ; A flag that indicates a global attribute.
              varID: 0L, $            ; If this is a variable attribute, the ID of the variable.
              length: 0L, $           ; The length of the attribute.
              value: Ptr_New(), $     ; The value of the attribute.
              datatype: "" $          ; The datatype of the attribute.
            }
            
END
