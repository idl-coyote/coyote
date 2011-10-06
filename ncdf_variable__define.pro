;+
; NAME:
;       NCDF_VARIABLE
;
; PURPOSE:
;
;       The pupose of this NCDF_Variable object is to store information about
;       a netCDF variable. The object is principally used as a utility routine 
;       for the NCDF_FILE object. Given the variable name, the object will acquire 
;       additional information about the variable from the netCDF file containing 
;       the variable.
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
;
;       File I/O
;
; CALLING SEQUENCE:
;
;       IDL> varObj = Obj_New('NCDF_VARIABLE', varName, parent)
;
; ARGUMENTS:
;
;       varName:   The case sensitive name of a netCDF variable that is stored in the 
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
;     varObject -> AddAttr
;     varAttrNames = varObject -> GetAttrNames()
;     dimIDs = varObject -> GetDimIDs()
;     dimNames = varObject -> GetDimNames()
;     varAttrValue = varObject -> GetAttrValue()
;     varID = varObject -> GetID()
;     varName = varObject -> GetName()
;     propertyValue = varObject -> GetProperty(attrProperty)
;     varValue = varObject -> GetValue()
;     varObject -> ParseVariable
;     
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 3 Feb 2010.
;       Changes to the way dimensions of length 0 are handled. 11 Feb 2010. DWF.
;       Added GetInfo method. 20 Mar 2010. DWF.
;       Added MISSINGINDICES and FILLVALUE keywords to GetValue method to return the indices 
;           and the value of missing data. 20 Mar 2010. DWF.
;       Modified the GetValue method so that if the data returned is scaled and/or offset
;           then the "missing" data value is preserved, although its data type may change.
;           In other words, the "missing" data is not scaled or offset. 20 Mar 2010. DWF.
;       Added output keywords SCALE_FACTOR, ADD_OFFSET, and DATATYPE to the GetValue method
;           so that these values can be returned to the caller at run-time. 29 April 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
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
;    NCDF_Variable::AddAttr                                                                   
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Adds a variable attribute to the object.                                  
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    varObject -> AddAttr, attrName, attrValue, DATATYPE=datatype              
;                                                                              
; Auguments:                                                                   
;                                                                              
;    attrName:     A case sensitive name of a variable attribute.              
;    attrValue:    The value of the variable attribute.                        
;                                                                              
; Keywords:                                                                    
;                                                                              
;    DATATYPE:    An input keyword that contains the netCDF data type of       
;                 the variable attribute. If not provided the data type is     
;                 determined from the attribute value.                           
;                                                                              
;------------------------------------------------------------------------------------------;
PRO NCDF_Variable::AddAttr, attrName, attrValue, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorlogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Elements(attrName) EQ 0 THEN Message, 'The attribute name is required.'
    IF N_Elements(attrValue) EQ 0 THEN Message, 'The attribute value is required.'
   
    ; Set the appropriate netCDF data type keyword.
    IF N_Elements(datatype) EQ 0 THEN datatype = NCDF_CastDataType(attrValue)
    CASE StrUpCase(datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
        ELSE: Message, 'Unknown DATATYPE for netCDF files: ' + datatype
    ENDCASE
    
    ; The file has to be writable to add an attribute.
    IF ~self.parent -> GetProperty('WRITABLE') THEN $
        Message, 'Cannot add a attribute to a READ-ONLY file.'
    
    ; Put the file into define mode
    self.parent -> SetMode, /DEFINE
    
    ; Check to see if this attribute in _FillValue. If it is,
    ; make sure the data type of the attribute matches the 
    ; data type of the variable, or there can be lots of trouble
    ; later on.
    IF (attrName EQ '_FillValue') THEN BEGIN
        IF self.datatype NE datatype THEN BEGIN
            Message, "The _FillValue attribute's datatype (" + $
                datatype + ") does not match the datatype (" + $
                self.datatype + ") of the variable."
        ENDIF
    ENDIF
    
    ; Add the attribute to the file.
    fileID = self.parent -> GetFileID()
    NCDF_AttPut, fileID, self.ID, attrName, attrValue, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=length, $
        LONG=tlong, $
        SHORT=tshort
    
    ; Add the attribute to this object's attribute list. Use "self" rather
    ; then the variable name to avoid excess trips through ParseFile from the
    ; HASVAR method.
    attrObj = Obj_New('NCDF_Attribute', attrName, self.parent, VARNAME=self)
    self.attrs -> Add, attrObj

END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetAttrNames                                                              
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns a string array with the names of all the variable's attributes.   
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    attrNames = GetAttrNames(COUNT=attrCount)                                 
;                                                                              
; Auguments:                                                                   
;                                                                              
;    None.                                                                     
;                                                                              
; Keywords:                                                                    
;                                                                              
;    COUNT:      An output keyword containing the number of attribute names found.           
;                                                                              
;                                                                              
; Return Value:                                                                
;                                                                              
;    attrNames:  A string array with the names of all the variable's attributes.           
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetAttrNames, COUNT=attrCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Count the number of attribute objects.
    attrCount = self.attrs -> Count()
    
    ; If there are no attributes, return null string.
    IF attrCount EQ 0 THEN RETURN, ""
    
    attrNames = StrArr(attrCount)
    FOR j=0,attrCount-1 DO BEGIN
        thisObj = self.attrs -> Get(POSITION=j)
        attrNames[j] = thisObj -> GetName()
    ENDFOR
    
    ; Return a scalar if necessary.
    IF N_Elements(attrNames) EQ 1 THEN attrNames = attrNames[0]
    
    RETURN, attrNames

END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetAttrValue                                                              
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the value of a variable attribute.                                
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    attrValue = GetAttrValue(attrName, DATATYPE=datatype)                     
;                                                                              
; Auguments:                                                                   
;                                                                              
;    attrName:     A case sensitive name of a variable attribute.              
;                                                                              
; Keywords:                                                                    
;                                                                              
;    DATATYPE:    An ouput keyword that contains the netCDF data type of       
;                 the variable attribute.                                        
;                                                                              
; Return Value:                                                                
;                                                                              
;    attrValue:  The value of the variable's attribute.                        
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetAttrValue, attrName, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(attrName) EQ 0 THEN Message, 'An attribute name or object reference is required.'

    ; Were you passed the name of a attribute or an attribute object?
    CASE Size(attrName, /TNAME) OF
    
        'STRING': BEGIN
            attrObj = self.attrs -> FindByName(attrName, COUNT=attrcount, /CASE_SENSITIVE)
            IF attrcount EQ 0 THEN Message, 'Cannot find an attribute object with name ' + attrName + '.'
            IF ~Obj_Valid(attrObj) THEN Message, 'Invalid object with name "' + attrName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            attrObj = attrName
            END
    
        ELSE: Message, 'Input attribute name or object is the wrong data type.'
    ENDCASE

    ; Get the data.
    attrData = attrObj -> GetValue()
    datatype = attrObj -> GetProperty('DATATYPE')
    
    RETURN, attrData
    
END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetDimIDs                                                                 
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the dimension IDs of the dimensions associated with this image.   
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    dimIDs = GetDimIDs(COUNT=dimCount)                                        
;                                                                              
; Auguments:                                                                   
;                                                                              
;    None.                                                                     
;                                                                              
; Keywords:                                                                    
;                                                                              
;    COUNT:     An ouput keyword that contains number of dimension IDs returned.           ;  
;                                                                              
; Return Value:                                                                
;                                                                              
;    dimIDs:    A long arry of dimension identifiers.                          
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetDimIDs, COUNT=dimCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    ; Get the dimension IDs.
    IF Ptr_Valid(self.dims) THEN BEGIN
        dimIDs = *self.dims 
        dimCount = self.ndims
    ENDIF ELSE BEGIN
        dimIDs = -1
        dimCount = 0
    ENDELSE
    
    RETURN, dimIDs

END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetDimNames                                                               
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the dimension names of the dimensions associated with this image. 
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    dimNames = GetDimNames(COUNT=dimCount)                                    
;                                                                              
; Auguments:                                                                   
;                                                                              
;    None.                                                                     
;                                                                              
; Keywords:                                                                    
;                                                                              
;    COUNT:     An ouput keyword that contains number of dimension names returned.          
;                                                                              
; Return Value:                                                                
;                                                                              
;    dimNames:    A string array containing the dimension names.               
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetDimNames, COUNT=dimCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    ; Get the dimension names.
    IF Ptr_Valid(self.dimNames) THEN BEGIN
        dimNames = *self.dimNames
        dimCount = self.ndims
     ENDIF ELSE BEGIN
        dimNames = ""
        dimCount = 0
    ENDELSE
    
    RETURN, dimNames

END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetDimSizes                                                               
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the dimension sizes of the dimensions associated with this image. 
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    dimSizes = GetDimSizes(COUNT=dimCount)                                    
;                                                                              
; Auguments:                                                                   
;                                                                              
;    None.                                                                     
;                                                                              
; Keywords:                                                                    
;                                                                              
;    COUNT:     An ouput keyword that contains number of dimension sizes returned.           
;                                                                              
; Return Value:                                                                
;                                                                              
;    dimSizes:    A long array of dimension sizes.                             
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetDimSizes, COUNT=dimCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    ; Get the dimension sizes.
    IF Ptr_Valid(self.dimensions) THEN BEGIN
        dimSizes = *self.dimensions
        dimCount = N_Elements(dimSizes)
    ENDIF ELSE BEGIN
        dimSizes = 0
        dimCount = 0
    ENDELSE
    
    RETURN, dimSizes

END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetID                                                                     
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the netCDF variable ID.                                           
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    varID = GetID()                                                           
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
;    varID:    A long integer containing the netCDF variable ID.               
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetID
    RETURN, self.ID
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_Variable::GetInfo                                                                
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns information about the variable from the file.                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    info = obj -> GetInfo()       
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
;    info:       A structure contains the following fields.
;    
;                    info = { dims: varObj -> GetDimSizes(), $
;                             dimNames: varObj -> GetDimNames(), $
;                             attrNames: varObj -> GetAttrNames(), $
;                             dataType: varObj -> GetProperty('datatype'), $
;                             nattrs: varObj -> GetProperty('nattrs'), $
;                             ndims: varObj -> GetProperty('ndims') }
;                            
;                In addition, the structure will contain the fields "scale_factor," "add_offset,"
;                and "_FillValue" if these attributes are available for the variable.
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetInfo

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF
    

    ; Collect and return information about the variable.
    info = { dims: self -> GetDimSizes(), $
             dimNames: self -> GetDimNames(), $
             attrNames: self -> GetAttrNames(), $
             dataType: self -> GetProperty('datatype'), $
             nattrs: self -> GetProperty('nattrs'), $
             ndims: self -> GetProperty('ndims') }
             
    ; Add additional information to the info structure, if it is available.
    ; This will include scale_factor, add_offset, and _FillValue attribute values.
    attrNames = self -> GetAttrNames()
    attrIndex = Where(attrNames EQ 'scale_factor', count)
    IF count GT 0 THEN info = Create_Struct(info, 'scale_factor', $
        self -> GetAttrValue('scale_factor'))
    attrIndex = Where(attrNames EQ 'add_offset', count)
    IF count GT 0 THEN info = Create_Struct(info, 'add_offset', $
        self -> GetAttrValue('add_offset'))
    attrIndex = Where(attrNames EQ 'missing_value', count)
    IF count GT 0 THEN info = Create_Struct(info, '_FillValue', $
        self -> GetAttrValue('missing_value'))
    attrIndex = Where(attrNames EQ '_FillValue', count)
    IF count GT 0 THEN info = Create_Struct(info, '_FillValue', $
        self -> GetAttrValue('_FillValue'))
    
    RETURN, info
    
END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetName                                                                   
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the variable name.                                                
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    varName = GetName()                                                       
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
;    varName:    A string variable containing the variable name.               
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetName
    RETURN, self.name
END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::GetProperty                                                               
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns various properties of the object one at a time. This is a shorthand and       
;    generic way to get the value of an object's "properties", which are defined as        
;    the IDL variables in the object's class structure.                        
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    propertyValue = obj -> GetProperty(thisProperty)                          
;                                                                              
; Auguments:                                                                   
;                                                                              
;    thisProperty:   A string variable that is equivalent to a field in the object's       
;                    class structure. See the *__DEFINE procedure for which properties     
;                    can be returned. The property is case insensitive.        
;                                                                              
; Keywords:                                                                    
;                                                                              
;    None.                                                                     
;                                                                              
; Return Value:                                                                
;                                                                              
;    propertyValue:  The value of a particular object property. Note that pointer          
;                    properties will return the variable the pointer points to.
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetProperty, thisProperty

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
;    NCDF_Variable::GetValue                                                                  
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Returns the the value of the variable. That is, the variable's data.      
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    varValue = GetValue(COUNT=count, OFFSET=offset, STRIDE=stride, MISSING=missing)            
;                                                                              
; Auguments:                                                                   
;                                                                              
;    None.                                                                     
;                                                                              
; Input Keywords:                                                                    
;                                                                              
;    COUNT:      An optional vector containing the counts to be used in reading the        
;                variable. Count is a 1-based vector with an element for each dimension.    
;                The default matches the size of the variable so that all data is          
;                written out.                                                    
;    OFFSET:     An optional vector containing the starting position for the read.         
;                The default start position is [0, 0, ...].      
;    STRIDE:     An optional vector containing the strides, or sampling intervals,         
;                between accessed values of the netCDF variable. The default stride        
;                vector is that for a contiguous read, [1, 1, ...].              
;                                                                              
; Output Keywords:   
;
;    ADD_OFFSET:  The add_offset value for the variable, if there is one.
;
;    DATATYPE:    The data type of the variable, before the scale and offset are applied.
;                 The same as what comes back from datatype = Size(rawVariable, /TNAME).
;
;    FILLVALUE:   The value that is being used for the "missing" value in this variable.
;                                                                              
;    MISSINGINDICES: A vector containing the missing indices in the returned data. Missing
;                 data is identified by either the depreciated "missing_value" attribute
;                 or the approved "_FillValue" attribute.  
;       
;    SCALE_FACTOR: The scale factor for the variable, if there is one.
; 
; Return Value:                                                                
;                                                                              
;    varValue:    The actual data of the variable. If scale_factor and add_offset          
;                 attributes are present, the data is scaled and offset before it          
;                 is returned to the caller. The "missing" data (see the MISSING 
;                 keyword) value is not changed, although its data type might change
;                 in the scaling and offset process.                                  
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::GetValue, $
    ADD_OFFSET=add_offset, $
    COUNT=count, $
    DATATYPE=datatype, $
    FILLVALUE=missingvalue, $
    OFFSET=offset, $
    SCALE_FACTOR=scale_factor, $
    STRIDE=stride, $
    MISSINGINDICES=missingIndices

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Read the data for this variable.
    NCDF_VarGet, self.parent->GetFileID(), self.id, data, COUNT=count, OFFSET=offset, STRIDE=stride
    dataType = Size(data, /TNAME)
    
    ; Does this variable contain "missing" values. If so, identify and return
    ; the missing data indices so they can be identified after scaling.
    missingCount = 0
    IF self -> HasAttr('missing_value') THEN BEGIN
        missingValue = self -> GetAttrValue('missing_value')
        missingIndices = Where(data EQ missingValue, missingCount)
    ENDIF
    IF self -> HasAttr('_FillValue') THEN BEGIN
        missingValue = self -> GetAttrValue('_FillValue')
        missingIndices = Where(data EQ missingValue, missingCount)
    ENDIF
    
    ; If there are scale_factor and add_offset attributes defined for this variable, the
    ; data will be scaled and offset before it is returned. Take care to preserve missing
    ; values in the data. (Data type may change!) 
    IF (self -> HasAttr('scale_factor') OR self -> HasAttr('add_offset')) THEN BEGIN
        IF self -> HasAttr('scale_factor') THEN scale_factor = self -> GetAttrValue('scale_factor') ELSE scale_factor = 1B
        IF self -> HasAttr('add_offset') THEN add_offset = self -> GetAttrValue('add_offset') ELSE add_offset = 0B
        data = (data * scale_factor) + add_offset
        IF missingCount GT 0 THEN data[missingIndices] = missingValue
    ENDIF
    
    ; Is this CHAR data? If so, convert it back to a string.
    IF self.datatype EQ 'CHAR' THEN data = String(Temporary(data))
    
    ; Return the data.
    RETURN, data
    
END 


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::HasAttr                                                                   
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Indicates, by returning a 1, that this particular variable attribute is found in the  ;
;    file. If not found, this function returns a 0.                                                                      
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    isFound = obj -> HasAttr(attrName, OBJECT=object)                         
;                                                                              
; Auguments:                                                                   
;                                                                              
;    attrName:     The case sensitive name of a variable attribute.            
;                                                                              
; Keywords:                                                                    
;                                                                              
;    OBJECT:        If the attribute exists, this keyword returns the attribute's          
;                   object reference.                                          
;                                                                              
; Return Value:                                                                
;                                                                              
;    isFound:       If a variable attribute with this name is found, this variable         
;                   is set to 1. It is set to 0 otherwise.                     
;                                                                              
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_Variable::HasAttr, attrName, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        errorLogger = self.parent -> GetProperty('ErrorLogger')
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Can you find an attribute object with this name?
    object = self.attrs -> FindByName(attrName, COUNT=count)
    
    IF count GT 0 THEN RETURN, 1 ELSE RETURN, 0
    
END


;------------------------------------------------------------------------------------------;
;                                                                              
; NAME:                                                                         
;    NCDF_Variable::ParseVariable                                                             
;                                                                              
; Purpose:                                                                     
;                                                                              
;    Gathers information about the variable from the netCDF file.              
;                                                                              
; Method Syntax:                                                               
;                                                                              
;    dimObject -> ParseVariable                                                
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
PRO NCDF_Variable::ParseVariable

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    On_Error, 2

    ; Gather information about the variable.
    fileID = self.parent -> GetFileID()
    self.id = NCDF_VarID(fileID, self.name)
    varinfo = NCDF_VarInq(fileID, self.id)
    self.datatype = StrUpCase(varinfo.datatype)
    self.ndims = varinfo.ndims
    self.nattrs = varinfo.natts
    self.dims = Ptr_New(varinfo.dim)
    
    ; Get the actual dimensions of the variable.
    IF self.ndims GT 0 THEN BEGIN
        dimensions = LonArr(self.ndims)
        dimNames = StrArr(self.ndims)
        FOR j=0,self.ndims-1 DO BEGIN
            NCDF_DIMINQ, fileID, (*self.dims)[j], dimName, dimSize
            dimensions[j] = dimSize
            dimNames[j] = dimName
        ENDFOR
    ENDIF ELSE BEGIN
        dimensions = 0
        dimNames = ""
    ENDELSE
    self.dimensions = Ptr_New(dimensions)
    self.dimNames = Ptr_New(dimNames)
        
    ; If you have variable attributes, parse these, too.
    self.attrs -> Remove, /ALL
    FOR j=0,self.nattrs-1 DO BEGIN
        attrName = NCDF_AttName(fileID, self.id, j)
        attrObj = Obj_New('NCDF_Attribute', attrName, self.parent, VARNAME=self)
        self.attrs -> Add, attrObj
    ENDFOR
    
    
END    


PRO NCDF_Variable::CLEANUP

    Obj_Destroy, self.attrs
    Ptr_Free, self.dims
    Ptr_Free, self.dimensions
    Ptr_Free, self.dimNames

END



FUNCTION NCDF_Variable::INIT, varName, parent

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
    IF N_Elements(varName) EQ 0 THEN Message, 'A variable name is required'
    IF Size(varName, /TNAME) NE 'STRING' THEN Message, 'The variable name must be a string.'
    self.name = varName
    IF N_Elements(parent) EQ 0 THEN Message, 'A parent object is required.'
    IF Size(parent, /TNAME) NE 'OBJREF' THEN Message, 'The parent parameter must be an object reference.'
    self.parent = parent
    self.attrs = Obj_New('NCDF_Container')
    
    ; Obtain information about the variable and fill out the variable object.
    self -> ParseVariable
    
    RETURN, 1
    
END

PRO NCDF_Variable__DEFINE, class
    
    class = { NCDF_VARIABLE, $
              name: "", $                 ; The variable name.
              ID: 0L, $                   ; The variable ID.
              dimensions: Ptr_New(), $    ; The actual dimensions of the variable.
              dimNames: Ptr_New(), $      ; A vector of dimension names.
              dims: Ptr_New(), $          ; The dimension IDs of the dimensions
              nattrs: 0L, $               ; The number of attributes the variable has.
              ndims: 0L, $                ; The number of dimensions the variable has.
              datatype: "",  $            ; The netCDF data byte of this variable.
              parent: Obj_New(), $        ; The NCDF_FILE object where this variable resides.
              attrs: Obj_New() $          ; The variable attributes (objects).
            }
            
END
