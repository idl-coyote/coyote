;+
; NAME:
;       NCDF_FILE
;
; PURPOSE:
;
;       The pupose of this NCDF_File object is three-fold. (1) Allow the user to easily
;       determine what information is inside a netCDF file and allow easy access
;       to such information. (2) Allow the user to easily create a netCDF file from
;       scratch. (3) Allow the user to easily copy information from one netCDF 
;       file to another.
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
;       IDL> nCDFObject = Obj_New('NCDF_FILE', filename)
;
; ARGUMENTS:
;
;       filename:  The name of a netCDF file to read, write to, or browse.
;
; KEYWORD PARAMETERS:
;       
;       ALERT:     Set this keyword if you wish to have alert from the object's error logger.
;                  Input. Default is 1.
;       
;       BROWSE:    If this keyword is set, the Browse Window is invoked as soon
;                  as the object is initiated. Input. Default is 0.
;
;       CLOBBER:   Set this keyword if you are opening a netCDF file that already exists and 
;                  you want to overwrite the existing file. Input. Default is 0.
;                  
;       CREATE:    Set this keyword if you wish to create a new netCDF file to write
;                  into. Input. Default is 0, which means the file will be opened as 
;                  "read-only".
;       
;       DELETE_ON_DESTROY:  Set this keyword if you wish to delete the error log file when
;                  the ErrorLogger object is destroyed. This will only happen if the ErrorLogger
;                  object is not in an error state. Input. Default is 1.
;                  
;       MODIFY:    Set this keyword if you wish to modify (write to) a file you are opening.
;                  If not set, the file will be opened as "read-only".
;
;
; REQUIRES:
;
;     The following programs are required from the Coyote Library. And it is always a
;     good idea to make sure you have the latest version of the Coyote Library code,
;     as updates are irregular and frequent.
;
;              http://www.idlcoyote.com/programs/ncdf_attribute__define.pro
;              http://www.idlcoyote.com/programs/ncdf_data__define.pro
;              http://www.idlcoyote.com/programs/ncdf_browser.pro
;              http://www.idlcoyote.com/programs/ncdf_castdatatype.pro
;              http://www.idlcoyote.com/programs/ncdf_container__define.pro
;              http://www.idlcoyote.com/programs/ncdf_dimension__define.pro
;              http://www.idlcoyote.com/programs/ncdf_variable__define.pro
;              http://www.idlcoyote.com/programs/errorlogger__define.pro
;              http://www.idlcoyote.com/programs/error_message.pro
;              http://www.idlcoyote.com/programs/cgcentertlb.pro
;              http://www.idlcoyote.com/programs/undefine.pro
;              http://www.idlcoyote.com/programs/textbox.pro
;              http://www.idlcoyote.com/programs/cgrootname.pro
;              http://www.idlcoyote.com/programs/textlineformat.pro
;              
;     These files may be (almost certainly are!) dependent on other Coyote Library files.
;
; METHODS:
;
;     The following methods are available. Each is documented in front of the method.
;
;     ncdfObject -> Browse 
;     ncdfObject -> CopyVarAttrTo, varName, attrName, destObj
;     ncdfObject -> CopyVarDataTo, varName, destObj, COUNT=count, OFFSET=offset, STRIDE=stride
;     ncdfObject -> CopyVarDefTo, varName, destObj
;     ncdfObject -> CopyGlobalAttrTo, attrName, destObj
;     ncdfObject -> CopyDimTo, dimName, destObj
;     dimNames = ncdfObject -> GetDimNames(COUNT=dimCount)
;     dimValue = ncdfObject -> GetDimValue(dimName)
;     fileID = ncdfObject -> GetFileID()
;     globalAttrNames = ncdfObject -> GetGlobalAttrNames(COUNT=attrCount)
;     attrValue = ncdfObject -> GetGlobalAttrValue(attrName, DATATYPE=datatype)
;     ncdfObject -> GetProperty, ....
;     property = ncdfObject -> GetProperty(thisProperty)
;     varAttrNames = ncdfObject -> GetVarAttrNames(varName, COUNT=attrCount)
;     varAttrValue = ncdfObject -> GetVarAttrValue(varName, varAttrName, COUNT=attrCount)
;     varNames = ncdfObject -> GetVarNames(COUNT=varCount)
;     varData = ncdfObject -> GetVarData(varName, COUNT=count, OFFSET=offset, STRIDE=stride)
;     answer = ncdfObject -> HasGlobalAttr(attrName, OBJECT=object)
;     answer = ncdfObject -> HasDim(dimName, OBJECT=object)
;     answer = ncdfObject -> HasVar(varName, OBJECT=object)
;     answer = ncdfObject -> HasVarAttr(varName, attrName, OBJECT=object)
;     ncdfObject -> PrintFileInfo 
;     ncdfObject -> ParseFile
;     ncdfObject -> SetMode, DEFINE=define, DATA=data
;     ncdfObject -> WriteVarData, varName, data, COUNT=count, OFFSET=offset, STRIDE=stride
;     ncdfObject -> WriteVarDef, varName, dimNames, DATATYPE=datatype, VAROBJ=varObj
;     ncdfObject -> WriteDim, dimName, dimSize, UNLIMITED=unlimited
;     ncdfObject -> WriteGlobalAttr, attrName, attrValue, DATATYPE=datatype
;     ncdfObject -> WriteVarAttr, attrName, attrValue, varObj, DATATYPE=datatype
;     
; NOTES:
; 
;     Note that all variable, attribute, and dimension names in a netCDF file are CASE SENSITIIVE!!
;     Thus, it is a good idea to use the methods provided in this object to obtain and examine
;     information in the file, as these names are handled in a case sensitive manner.
;     
;     Whenever you are creating a new netCDF file, you should try to create the file in
;     the following way.
;        1. Create your global attributes.
;        2. Create the dimensions you will be using to describe the variables.
;        3. Define the variables. To do this correctly, dimensions MUST be defined.
;        4. Define variable attributes.
;        5. Load your variables with data.
;        
;        Note that the data type of the _FillValue variable attribute MUST match the
;        data type of the variable data. Otherwise, you will have MANY problems! This
;        is a common source of error.
;        
;        Note that in almost all cases where you see the names "varName", "dimName", or
;        "attrName" used as input variables, you can substitute the proper object 
;        reference in place of the actual name. In other words, you could get the value
;        of a variable attribute by doing something like this:
;        
;            check = ncdfObject -> HasAttr('history', OBJECT=attrObj)
;            IF check THEN attrValue = ncdfObject -> GetGlobalAttrValue(attrObj)
;           
;         as opposed to this:
;            
;            IF check THEN attrValue = ncdfObject -> GetGlobalAttrValue('history')
; EXAMPLE:
;
;       IDL> filename = 'example.nc'
;       IDL> ncdfObj = Obj_New('NCDF_FILE', filename)
;       IDL> ncdfObj -> Browse
;       IDL> Obj_Destroy, ncdfObj
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 3 Feb 2010, using (stealing, really) plenty of ideas
;          from Mark Hadfield's Motley Library. Mark's mghncfile object is terrific, but it
;          had a number of limitations for my particular application, which I have attemped
;          to correct in my version of the software. But I wouldn't have even attempted this
;          had Mark not blazed the trail and Matt Savoie not insisted that I look at Mark's
;          wonderful library.
;       Changes in the way dimensions with a zero length are handled. 11 Feb 2010, DWF.
;       Added GetVarInfo method. 20 March 2010. DWF.
;       Added MISSINGINIDCES and FILLVALUE output keywords to GetVarData method. 20 March 2010. DWF.
;       Added output keywords SCALE_FACTOR, ADD_OFFSET, and DATATYPE to GetVarData method
;           so that these values can be obtained with the data. 29 Apr 2010. DWF.
;       I changed "missingValue" to "fillValue" some time ago, but I missed one in
;           the GetVarData method. Fixed. 7 June 2010. DWF.
;       Used the undefine procedure OBJ_DELETE, rather than OBJ_DESTROY. Sheesh! 18 June 2010. DWF.
;       Added NETCDF4_FORMAT keyword. 13 Feb 2012. DWF.
;       Added a bunch of new IDL 8.0 and 8.1 keyword to the WriteVarDef method to allow
;           access to these keywords in NCDF_VarDef. Also modified the NETCDF4_FORMAT keyword
;           to apply only in IDL versions 8.0 and higher. 21 Feb 2012. DWF.
;       
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
;    NCDF_File::Browse                                                                 
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Allows the user to browse the netCDF file interactively. Variables, attributes,    
;    and dimensions can be saved to the main IDL level or command line where they can   
;    be manipulated further.                                                                                          
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> Browse                                                          
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    TITLE:       A text string that will be the title of the browser window. (Optional)
;    XOFFSET:     The X offset of the top-left corner of the browser. (Optional)        
;    YOFFSET:     The Y offset of the top-left corner of the browser. (Optional)        
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::Browse, TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset

    NCDF_Browser, self.filename, $
        /NO_NEW_FILE, $
        TITLE=title, $
        XOFFSET=xoffset, $
        YOFFSET=yoffset

END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::Close_File                                                             
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Closes the netCDF file, if open.                                                                                 
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> Browse                                                          
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
PRO NCDF_File::Close_File
    IF self.fileID GT 0 THEN NCDF_Close, self.fileID
    self.fileID = -1
END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CreateVarObj                                                           
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Creates a NCDF_Variable object and adds it the the variable container.                                           
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CreateVarObj, varName                                           
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:  The case sensitive name of the variable.                     
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
; Notes: An internal method.                                                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CreateVarObj, varName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
 
    varObj = Obj_New('NCDF_Variable', varName, self)
    self.vars -> Add, varObj

END   


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CreateAttrObj                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Creates a NCDF_Attribute object and adds it the the attribute container.                                                     
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CreateAttrObj, attrName                                         
;                                                                           
; Auguments:                                                                
;                                                                           
;    attrName:  The case sensitive name of the attribute.                   
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
; Notes: An internal method.                                                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CreateAttrObj, attrName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
   attrObj = Obj_New('NCDF_Attribute', attrName, self)
   self.attrs -> Add, attrObj
   
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CreateDimObj                                                           
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Creates a NCDF_Dimension object and adds it the the dimension container.                                                     
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CreateDimObj, dimensionName                                     
;                                                                           
; Auguments:                                                                
;                                                                           
;    dimensionName:  The case sensitive name of the dimension.              
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
; Notes: An internal method.                                                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CreateDimObj, dimensionName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    dimObj = Obj_New('NCDF_Dimension', dimensionName, self)
    self.dims -> Add, dimObj
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyVarAttrTo                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Copies a variable attribute from this object to another NCDF_FILE object.                                                    
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyVarAttrTo, varName, attrName, destObj                       
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to copy.  
;    attrName:   The case sensitive name of the variable attribute you wish to copy.    
;    destObj:    The object reference of a NCDF_FILE object you wish to copy
;                the variable attribute to.                                 
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
; Notes: The variable will have had to have been previously defined for the file.       
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyVarAttrTo, varName, attrName, destObj

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'
    IF N_Elements(attrName) EQ 0 THEN $
               Message, 'An attribute name or object reference is required.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Get and return the variable's attribute value.
    attrValue = varObj -> GetAttrValue(attrName, DATATYPE=attrDataType)
    
    ; Write the variable attribute in the destination object.
    destObj -> WriteVarAttr, varName, attrName, attrValue, DATATYPE=attrDataType
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyVariableTo                                                           
;                                                                           
; Purpose:                                                                  
;                                                                           
;    The NCDF_File object has methods to do low-level manipulation of netCDF files, but
;    this method is a high-level method to copy a variable from one file to another.
;    This method will find all of the variable parts it needs in the file (dimensions, 
;    variable definition, variable attributes, and even variable data) and will copy 
;    everything it finds (if needed!) into the destination file. This eliminates a lot 
;    of the druge work that goes into understanding exactly how everything works.                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyVariableTo, varName, destObj                                  
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to copy   
;                the variable definition from.                              
;    destObj:    The object reference of a NCDF_FILE object you wish to copy the        
;                variable definition to.                                    
;                                                                           
; Keywords:                                                                 
;                                                                           
;    NODATA:     If this keyword is set, the variable's data is not copied to the file.                                                                    
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyVariableTo, varName, destObj

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'
    IF N_Elements(destObj) EQ 0 THEN $
               Message, 'A destination NCDF_FILE object is required.'
    IF ~Obj_Valid(destObj) THEN $
               Message, 'The destination NCDF_FILE object is not valid.'
    IF ~destObj -> GetProperty('Writable') THEN $
               Message, 'The destination NCDF_FILE object is not writeable.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=strCount, /CASE_SENSITIVE)
            IF strCount EQ 0 THEN Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE
    
    ; Start with this variable's dimensions. Have these been defined
    ; for the variable in the destination object? If not, define them.
    dimNames = VarObj -> GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
        thisName = dimNames[j]
        IF ~destObj->HasDim(thisName) THEN self -> CopyDimTo, thisName, destObj
    ENDFOR
    
    ; Has this variable been defined in the destination object. ? If not, do it.
    IF ~destObj->HasVar(varName) THEN self -> CopyVarDefTo, varName, destObj
    
    ; Does the variable have attributes? Copy these, too.
    varAttrNames = varObj -> GetAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
        thisAttrName = varAttrNames[j]
        IF ~destObj->HasVarAttr(varName, thisAttrName) THEN $
            self -> CopyVarAttrTo, varName, thisAttrName, destObj
    ENDFOR
    
    ; Finally, copy the variable's data to the destination object.
    IF ~Keyword_Set(nodata) THEN self -> CopyVarDataTo, varName, destObj

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyVarDataTo                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Copies variable data from this object to another NCDF_FILE object.                                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyVarDataTo, varName, destObj, COUNT=count, OFFSET=offset, STRIDE=stride  
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to copy the data from.
;    destObj:    The object reference of a NCDF_FILE object you wish to copy the data to.  
;                                                                           
; Keywords:                                                                 
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
; Notes: The variable will have had to have been previously defined for the file.       
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyVarDataTo, varName, destObj, COUNT=count, OFFSET=offset, STRIDE=stride

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'
    IF N_Elements(destObj) EQ 0 THEN $
               Message, 'A destination NCDF_FILE object is required.'
    IF ~Obj_Valid(destObj) THEN $
               Message, 'The destination NCDF_FILE object is not valid.'
    IF ~destObj -> GetProperty('Writable') THEN $
               Message, 'The destination NCDF_FILE object is not writeable.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=strCount, /CASE_SENSITIVE)
            IF strCount EQ 0 THEN Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE
    
    ; Gather information.
    data = varObj ->GetValue(COUNT=count, OFFSET=offset, STRIDE=stride)
    
    ; Copy the information to the destination object.
    destObj -> WriteVarData, varName, data, COUNT=count, OFFSET=offset, STRIDE=stride
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyVarDefTo                                                           
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Copies a variable definition from this object to another NCDF_FILE object.         
;    Note that dimension IDs are required to define a variable. This method assumes           
;    that whatever dimensions are defined for the variable you are copying are already  
;    defined in the file object you are copying this variable to.                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyVarDefTo, varName, destObj                                  
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to copy   
;                the variable definition from.                              
;    destObj:    The object reference of a NCDF_FILE object you wish to copy the        
;                variable definition to.                                    
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyVarDefTo, varName, destObj

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'
    IF N_Elements(destObj) EQ 0 THEN $
               Message, 'A destination NCDF_FILE object is required.'
    IF ~Obj_Valid(destObj) THEN $
               Message, 'The destination NCDF_FILE object is not valid.'
    IF ~destObj -> GetProperty('Writable') THEN $
               Message, 'The destination NCDF_FILE object is not writeable.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
                Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE
    
    ; Gather information.
    dimNames = varObj -> GetDimNames(COUNT=dimCount)
    datatype = varObj -> GetProperty('DATATYPE')
    
    ; See if you can match the names of the dimensions with the dimensions
    ; that are current defined for the destination object.
    IF dimCount GT 0 THEN BEGIN
        destDimContainer = destObj -> GetProperty('DIMS')
        dimIDs = LonArr(dimCount)
        FOR j=0,dimCount-1 DO BEGIN
            thisObj = destDimContainer -> FindByName(dimNames[j], COUNT=found)
            IF found EQ 0 THEN Message, 'Cannot find a dimension named "' + $
                dimNames[j] + '" in the destination object.'
            dimIDs[j] = thisObj -> GetID()
        ENDFOR
    ENDIF
    
    ; Copy the information to the destination object.
    IF dimCount EQ 0 THEN BEGIN
        destObj -> WriteVarDef, varName, DATATYPE=datatype
    ENDIF ELSE BEGIN
        destObj -> WriteVarDef, varName, dimIDs, DATATYPE=datatype
    ENDELSE
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyGlobalAttrTo                                                       
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Copies a global attribute from this object to another NCDF_FILE object.
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyGlobalAttrTo, attrName, destObj                             
;                                                                           
; Auguments:                                                                
;                                                                           
;    attrName:   The case sensitive name of the global attribute you wish to copy       
;                to the destination object.                                 
;    destObj:    The object reference of a NCDF_FILE object you wish to copy the        
;                variable definition to.                                    
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyGlobalAttrTo, attrName, destObj

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(attrName) EQ 0 THEN $
               Message, 'A global attrubute name or object reference is required.'
    IF N_Elements(destObj) EQ 0 THEN $
               Message, 'A destination NCDF_FILE object is required.'
    IF ~Obj_Valid(destObj) THEN $
               Message, 'The destination NCDF_FILE object is not valid.'
    IF ~destObj -> GetProperty('Writable') THEN $
               Message, 'The destination NCDF_FILE object is not writeable.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of an attribute or an attribute object?
    CASE Size(attrName, /TNAME) OF
    
        'STRING': BEGIN
            attrObj = self.attrs -> FindByName(attrName, COUNT=attrCount, /CASE_SENSITIVE)
            IF attrCount EQ 0 THEN Message, 'Cannot find a dimension object with name ' + attrName + '.'
            IF ~Obj_Valid(attrObj) THEN Message, 'Invalid object with name "' + attrName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            attrObj = attrName
            END
    
        ELSE: Message, 'Input global attribute name or object is the wrong data type.'
    ENDCASE
    
    ; Gather information.
    attrValue = attrObj -> GetValue()
    dataType = attrObj -> GetProperty('DATATYPE')
    
    ; Copy the information to the destination object. 
        destObj -> WriteGlobalAttr, attrName, attrValue, DATATYPE=datatype
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::CopyDimTo                                                              
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Copies a dimension from this object to another NCDF_FILE object.       
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> CopyDimTo, dimName, destObj                                     
;                                                                           
; Auguments:                                                                
;                                                                           
;    dimName:    The case sensitive name of the dimension you wish to copy to the       
;                destination object.                                        
;    destObj:    The object reference of a NCDF_FILE object you wish to copy the        
;                dimension to.                                              
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::CopyDimTo, dimName, destObj

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF

    IF N_Elements(dimName) EQ 0 THEN $
               Message, 'A dimension name or object reference is required.'
    IF N_Elements(destObj) EQ 0 THEN $
               Message, 'A destination NCDF_FILE object is required.'
    IF ~Obj_Valid(destObj) THEN $
               Message, 'The destination NCDF_FILE object is not valid.'
    IF ~destObj -> GetProperty('Writable') THEN $
        Message, 'The destination NCDF_FILE object is not writeable.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a dimension or an dimenison object?
    CASE Size(dimName, /TNAME) OF
    
        'STRING': BEGIN
            dimObj = self.dims -> FindByName(dimName, COUNT=dimCount, /CASE_SENSITIVE)
            IF dimCount EQ 0 THEN $
               Message, 'Cannot find a dimension object with name ' + dimName + '.'
             IF ~Obj_Valid(dimObj) THEN $
               Message, 'Invalid object with name "' + dimName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            dimObj = dimName
            END
    
        ELSE: Message, 'Input dimension name or object is the wrong data type.'
    ENDCASE
    
    ; Gather information.
    dimSize = dimObj -> GetValue()
    unlimited = dimObj -> GetProperty('UNLIMITED')
    
    ; Copy the information to the destination object.
    destObj -> WriteDim, dimName, dimSize, UNLIMITED=unlimited
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetDimNames                                                            
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the names of all the dimensions in the file.                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    dimNames = obj -> GetDimNames(COUNT=dimCount)                          
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    COUNT:     An output keyword that reports the number of dimension names found.       
;                                                                           
; Return Value:                                                             
;                                                                           
;    dimNames:  A string array containing the names of the dimensions in the file.      
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetDimNames, COUNT=dimCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile
 
    ; Count the number of dimension objects.
    dimCount = self.dims -> Count()
    
    ; If there are no dimensions, return null string.
    IF dimCount EQ 0 THEN RETURN, ""
    
    dimNames = StrArr(dimCount)
    FOR j=0,dimCount-1 DO BEGIN
        thisObj = self.dims -> Get(POSITION=j)
        dimNames[j] = thisObj -> GetName()
    ENDFOR
    
    ; Return a scalar if necessary.
    IF N_Elements(dimNames) EQ 1 THEN dimNames = dimNames[0]
    
    RETURN, dimNames

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetDimValue                                                            
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the value (the size) of a dimension.                           
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    dimValue = obj -> GetDimValue(dimName)                                 
;                                                                           
; Auguments:                                                                
;                                                                           
;    dimName:    The case sensitive name of the dimension you want the value (size) of. 
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                    
;                                                                           
; Return Value:                                                             
;                                                                           
;    dimValue:  An integer that gives the size of the dimension.            
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetDimValue, dimName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(dimName) EQ 0 THEN $
               Message, 'A dimension name or object reference is required.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a dimension or dimension object?
    CASE Size(dimName, /TNAME) OF
    
        'STRING': BEGIN
            dimObj = self.dims -> FindByName(dimName, COUNT=dimCount, /CASE_SENSITIVE)
            IF dimCount EQ 0 THEN $
               Message, 'Cannot find a dimension object with name ' + dimName + '.'
            IF ~Obj_Valid(dimObj) THEN $
               Message, 'Invalid object with name "' + dimName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            dimObj = dimName
            END
    
        ELSE: Message, 'Input dimension name or object is the wrong data type.'
    ENDCASE

    ; Get the data.
    dimData = dimObj -> GetValue()
    
    RETURN, dimData
    
END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetFileID                                                              
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the netCDF file identifier.                                    
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    fileID = obj -> GetFileID()                                            
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
;    fileID:  The netCDF file identifier that is required to interact with the file.       ;
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetFileID
    RETURN, self.fileID
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetGlobalAttrNames                                                     
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the names of all the global attributes in the file.            
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    attrNames = obj -> GetGlobalAttrNames(COUNT=attrCount)                 
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    COUNT:     An output keyword that reports the number of dimension names found.       
;                                                                           
; Return Value:                                                             
;                                                                           
;    attrNames:  A string array containing the names of the global attributes in the file. 
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetGlobalAttrNames, COUNT=attrCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile
 
    ; Count the number of global attribute objects.
    attrCount = self.attrs -> Count()
    
    ; If there are no global attributes, return null string.
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
;    NCDF_File::GetGlobalAttrValue                                                     
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the value of a global attributes in the file.                  
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    attrValue = obj -> GetGlobalAttrValue(attrName, DATATYPE=datatype)     
;                                                                           
; Auguments:                                                                
;                                                                           
;    attrName:    The case sensitive name of a global attribute.            
;                                                                           
; Keywords:                                                                 
;                                                                           
;    DATATYPE:    An output keyword that contains the data type of the global attribute.  
;                                                                           
; Return Value:                                                             
;                                                                           
;    attrValue:   The value of the global attribute.                        
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetGlobalAttrValue, attrName, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(attrName) EQ 0 THEN $
               Message, 'A global attribute name or object reference is required.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of an attribute or an attribute object?
    CASE Size(attrName, /TNAME) OF
    
        'STRING': BEGIN
            attrObj = self.attrs -> FindByName(attrName, COUNT=attrCount, /CASE_SENSITIVE)
            IF attrCount EQ 0 THEN $
               Message, 'Cannot find an attribute object with name ' + attrName + '.'
            IF ~Obj_Valid(attrObj) THEN $
               Message, 'Invalid object with name "' + attrName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            attrObj = attrName
            END
    
        ELSE: Message, 'Input attribute name or object is the wrong data type.'
    ENDCASE

    ; Get the data.
    attrData = attrObj -> GetValue(DATATYPE=datatype)
    
    RETURN, attrData
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetProperty                                                            
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns various properties of the object via output keyword parameters.
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> GetProperty, ....                                               
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    ALL:            If set, return all properties of the object in a structure variable.    
;    ATTRNAMES:      This output variable returns all the global attribute names.       
;    DEFINE:         This output variable returns a 1 if the file is in DEFINE mode.    
;    DIMNAMES:       This output variable returns all the dimension names.  
;    ERRORLOGGER:    This output variable returns the errorlogger object.   
;    FILEID:         This output variable returns the netCDF file identifier.           
;    FILENAME:       This output variable returns the name of the netCDF file.          
;    FILEHASBEENPARSED:  This output variable returns a 1 if the file has been parsed.  
;    N_ATTRS:         This output variable returns the number of global attributes in   
;                     the file.    
;    N_DIMS:          This output variable returns the number of dimensions in the file.
;    N_VARS:          This output variable returns the number of variables in the file. 
;    UNLIMITED:       This output variable returns a vector of 0s and 1s, one element   
;                     for each dimension, indicating if the dimension is unlimited or not. 
;    VARNAMES:        This output variable returns the names of variables in the file.  
;    WRITEABLE:       This output variable returns a 1 if the file is writable.                                                  
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::GetProperty, $
     ALL=all, $
     ATTRNAMES=attrnames, $
     DEFINE=define, $
     DIMNAMES=dim_names, $
     DIMENSIONS=dimensions, $
     ERRORLOGGER=errorLogger, $
     FILEID=fileID, $
     FILENAME=filename, $
     FILEHASBEENPARSED=fileHasBeenParsed, $
     N_DIMS=n_dims, $
     N_VARS=n_vars, $
     N_ATTRS=n_attrs, $
     UNLIMITED=unlimited, $
     VARNAMES=varnames, $
     WRITABLE=writable

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile
    
    ; Easily returned information.
    errorLogger = self.errorLogger
    filename = self.filename
    fileID = self.fileID
    fileHasBeenParsed = self.fileHasBeenParsed
    define = self.define
    writable = self.writable

    ; What's in the file?
    info = NCDF_Inquire(self.fileID)
    n_dims  = info.ndims
    n_vars  = info.nvars
    n_attrs = info.ngatts

    ; Get the attribute names, if needed.
    IF Arg_Present(attrnames) || Arg_Present(all) THEN BEGIN
       IF n_attrs EQ 0 THEN BEGIN
            attrnames = ""
       ENDIF ELSE BEGIN
            attrnames = StrArr(n_attrs)
            FOR j=0,n_attrs-1 DO BEGIN
                thisAttr = self.attrs -> Get(POSITION=j)
                attrnames[j] = thisAttr -> GetProperty('NAME')
            ENDFOR
       ENDELSE
     ENDIF
     
     ; Get the dimension names, and dimension IDs, if needed.
     IF Arg_Present(dimNames) || Arg_Present(dimensions) || Arg_Present(all) THEN BEGIN
            IF n_dims EQ 0 THEN BEGIN
                dimNames = ""
                dimensions = 0
                unlimited = 0
            ENDIF ELSE BEGIN
                dimNames = StrArr(n_dims)
                dimensions = LonArr(n_dims)
                unlimited = BytArr(n_dims)
                FOR j=0, n_dims-1 DO BEGIN
                    dimsObj = self.dims -> Get(POSITION=j)
                    dimNames[j] = dimsObj -> GetProperty('NAME')
                    dimensions[j] = dimsObj -> GetProperty('SIZE')
                    unlimited[j] = dimsObj -> GetProperty('UNLIMITED')
                ENDFOR
            ENDELSE
     ENDIF

     ; Get the variable names, if needed.
     IF Arg_Present(varNames) || Arg_Present(all) THEN BEGIN
        IF n_vars EQ 0 THEN BEGIN
            varNames = ""
        ENDIF ELSE BEGIN
            varNames = StrArr(n_vars)
            FOR j=0,n_vars-1 DO BEGIN
                thisVar = self.vars -> Get(POSITION=j)
                varNames[j] = thisVar -> GetProperty('NAME')
            ENDFOR
        ENDELSE
     ENDIF

     IF Arg_Present(all) THEN BEGIN
        all = { ATTRNAMES:attrnames, $
                DEFINE:define, $
                DIMNAMES:dim_names, $
                DIMENSIONS:dimensions, $
                ERRORLOGGER:errorLogger, $
                FILEID:fileID, $
                FILENAME:filename, $
                FILEHASBEENPARSED:fileHasBeenParsed, $
                N_DIMS:n_dims, $
                N_VARS:n_vars, $
                N_ATTRS:n_attrs, $
                UNLIMITED:unlimited, $
                VARNAMES:varnames, $
                WRITABLE:writable }
     ENDIF

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetProperty                                                            
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
FUNCTION NCDF_File::GetProperty, thisProperty

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
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
;    NCDF_File::GetVarAttrNames                                                        
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the names of variable attributes in the file.                  
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    attrNames = obj -> GetVarAttrNames(varName, COUNT=varAttrCount)        
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you want the attributes of.    
;                                                                           
; Keywords:                                                                 
;                                                                           
;    COUNT:    The number of variable attributes found.                       
;                                                                           
; Return Value:                                                             
;                                                                           
;    attrNames:   A string array containing the names of the variable attributes.       
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetVarAttrNames, varName, COUNT=varAttrCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Get the variable attribute names.
    attrNames = varObj -> GetAttrNames(COUNT=varAttrCount)
    
    RETURN, attrNames
    
END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetVarAttrValue                                                        
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the value of a variable attribute in the file.                 
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    attrValue = obj -> GetVarAttrValue(varName, attrName, DATATYPE=datatype)           
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:     The case sensitive name of a variable whose attribute you want        
;                 to obtain.                                                
;    attrName:    The case sensitive name of a global attribute.            
;                                                                           
; Keywords:                                                                 
;                                                                           
;    DATATYPE:    An output keyword that contains the data type of the attribute.         
;                                                                           
; Return Value:                                                             
;                                                                           
;    attrValue:   The value of the variable attribute.                      
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetVarAttrValue, varName, attrName, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or object reference is required.'
    IF N_Elements(attrName) EQ 0 THEN $
               Message, 'An attribute name or object reference is required.'

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Get and return the variable's attribute value.
    attrValue = varObj -> GetAttrValue(attrName, DATATYPE=datatype)
    RETURN, attrValue
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetVarNames                                                            
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the names of the variables in the file.                        
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    varNames = obj -> GetVarNames(COUNT=varCount)                          
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    COUNT:    The number of variables found.                                 
;                                                                           
; Return Value:                                                             
;                                                                           
;    varNames:   A string array containing the names of the variables in the file.      
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetVarNames, COUNT=varCount

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile
 
    ; Count the number of global attribute objects.
    varCount = self.vars -> Count()
    
    ; If there are no global attributes, return null string.
    IF varCount EQ 0 THEN RETURN, ""
    
    varNames = StrArr(varCount)
    FOR j=0,varCount-1 DO BEGIN
        thisObj = self.vars -> Get(POSITION=j)
        varNames[j] = thisObj -> GetName()
    ENDFOR
    
    ; Return a scalar if necessary.
    IF varCount EQ 1 THEN varNames = varNames[0]
    
    RETURN, varNames

END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetVarData                                                                
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns the variable data from the file.                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    data = obj -> GetVarData(varName, COUNT=count, OFFSET=offset, STRIDE=stride)       
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:   The case sensitive name of a variable whose data you want to obtain.    
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
;    data:       The data obtained from the variable. If there is an ADD_OFFSET and
;                SCALE_FACTOR attribute for this variable, the returned data is scaled
;                and offset before returning. The "missing" or "fill value" is not 
;                changed by scaling and offsetting.                     
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::GetVarData, varName, $
    ADD_OFFSET=add_offset, $
    COUNT=count, $
    DATATYPE=datatype, $
    FILLVALUE=fillValue, $
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
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or variable object reference is required.'
     
    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Get the data.
    varData = varObj -> GetValue( $
        ADD_OFFSET=add_offset, $
        COUNT=count, $
        DATATYPE=datatype, $
        FILLVALUE=fillvalue, $
        OFFSET=offset, $
        MISSINGINDICES=missingindices, $
        SCALE_FACTOR=scale_factor, $
        STRIDE=stride)
    
    RETURN, varData
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::GetVarInfo                                                                
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Returns information about a specified variable from the file.                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    info = obj -> GetVarInfo(varName)       
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:   The case sensitive name of a variable whose information you want to obtain.    
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
FUNCTION NCDF_File::GetVarInfo, varName

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
    
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or variable object reference is required.'
     
    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile

    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Get the information you need.
    hasVar = self -> HasVar(varName, OBJECT=varObj)
    IF ~hasVar THEN Message, 'Cannot find a variable with the name ' + varName + '.'
    
    RETURN, varObj -> GetInfo()
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::HasGlobalAttr                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Indicates, by returning a 1, that this particular global attribute is found in the 
;    file. If not found, this function returns a 0.                                                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    isFound = obj -> HasGlobalAttr(attrName, OBJECT=object)                
;                                                                           
; Auguments:                                                                
;                                                                           
;    attrName:     The case sensitive name of a global attribute.           
;                                                                           
; Keywords:                                                                 
;                                                                           
;    OBJECT:        If the attribute exists, this keyword returns the global attribute's
;                   object reference. Such a reference can be used in place of the      
;                   global attribute's name in most methods.                
;                                                                           
; Return Value:                                                             
;                                                                           
;    isFound:       If an attribute with this name is found, this variable is set to    
;                   1. It is set to 0 otherwise.                            
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::HasGlobalAttr, attrName, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Can you find a global attribute object with this name?
    object = self.attrs -> FindByName(attrName, COUNT=count)
    
    IF count GT 0 THEN RETURN, 1 ELSE RETURN, 0
    
END

;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::HasDim                                                                 
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Indicates, by returning a 1, that this particular dimension is found in the        
;    file. If not found, this function returns a 0.                                                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    isFound = obj -> HasDim(dimName, OBJECT=object)                        
;                                                                           
; Auguments:                                                                
;                                                                           
;    dimName:     The case sensitive name of a dimension.                   
;                                                                           
; Keywords:                                                                 
;                                                                           
;    OBJECT:        If the dimension exists, this keyword returns the dimension's       
;                   object reference. Such a reference can be used in place of the      
;                   dimension's name in most methods.                       
;                                                                           
; Return Value:                                                             
;                                                                           
;    isFound:       If a dimension with this name is found, this variable is set to     
;                   1. It is set to 0 otherwise.                            
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::HasDim, dimName, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
 
    ; Can you find a dimension object with this name?
    object = self.dims -> FindByName(dimName, COUNT=count)
    
    IF count GT 0 THEN RETURN, 1 ELSE RETURN, 0
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::HasVar                                                                 
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Indicates, by returning a 1, that this particular variable is found in the         
;    file. If not found, this function returns a 0.                                                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    isFound = obj -> HasVar(varName, OBJECT=object)                        
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:      The case sensitive name of a variable.                   
;                                                                           
; Keywords:                                                                 
;                                                                           
;    OBJECT:        If the variable exists, this keyword returns the variable's         
;                   object reference. Such a reference can be used in place of the      
;                   variable's name in most methods.                        
;                                                                           
; Return Value:                                                             
;                                                                           
;    isFound:       If a variable with this name is found, this variable is set to      
;                   1. It is set to 0 otherwise.                            
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::HasVar, varName, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF
 
    ; Can you find a variable object with this name?
    object = self.vars -> FindByName(varName, COUNT=count)
    
    IF count GT 0 THEN RETURN, 1 ELSE RETURN, 0
    

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::HasVarAttr                                                             
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Indicates, by returning a 1, that this particular variable attribute is found in the  ;
;    file. If not found, this function returns a 0.                                                                   
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    isFound = obj -> HasVarAttr(varName, attrName, OBJECT=object)          
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:      The case sensitive name of a variable whose attribute we want to find.
;    attrName:     The case sensitive name of a variable attribute.         
;                                                                           
; Keywords:                                                                 
;                                                                           
;    OBJECT:        If the variable exists, this keyword returns the variable attribute's 
;                   object reference. Such a reference can be used in place of the      
;                   variable attribute's name in most methods.              
;                                                                           
; Return Value:                                                             
;                                                                           
;    isFound:       If a variable attribute with this name is found, this variable      
;                   is set to 1. It is set to 0 otherwise.                  
;                                                                           
;------------------------------------------------------------------------------------------;
FUNCTION NCDF_File::HasVarAttr, varName, varAttrName, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN, ""
    ENDIF

    ; Can you find a variable object with this name?
    varObj = self.vars -> FindByName(varName, COUNT=count)
    
    IF count EQ 0 THEN Message, 'Cannot find a variable with name "' + varName + '".'
    
    ; Can you find a variable attribute with this name.
    hasAttr = varObj[0] -> HasAttr(varAttrName, OBJECT=object)
        
    RETURN, hasAttr
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::PrintFileInfo                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Prints file information out to the IDL console window or, optionally, to a file.                                             
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> PrintFileInfo, outputFile                                                          
;                                                                           
; Auguments:                                                                
;                                                                           
;    outputFile:      An optional filename. If present, the output is written to this   
;                     file instead of to the console.                       
;                                                                           
; Keywords:                                                                 
;                                                                           
;    None.                                                                  
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::PrintFileInfo, outputFile

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        IF lun NE -1 THEN Free_Lun, lun
        RETURN
    ENDIF
    
    ; Make sure the file has been parsed.
    IF ~self.fileHasBeenParsed THEN self -> ParseFile
    
    ; Are we writing to a file or to standard output?
    IF N_Elements(outputFile) NE 0 THEN BEGIN
        OpenW, lun, outputFile, /GET_LUN
    ENDIF ELSE lun = -1
    
    ; Gather information.
    numGAttrs = self.attrs -> Count()
    numDims = self.dims -> Count()
    numVars = self.vars -> Count()
    PrintF, lun, 'File Information: ', self.filename
    PrintF, lun, 'Number of Global Attributes: ', StrTrim(numGAttrs,2)
    PrintF, lun, 'Number of Dimensions: ', StrTrim(numDims,2)
    PrintF, lun, 'Number of Variables: ', StrTrim(numVars,2)
    PrintF, lun, ''
    
    PrintF, lun, 'Global Attributes: '
    FOR j=0,numGAttrs-1 DO BEGIN
        attrObj = self.attrs -> Get(POSITION=j)
        name = attrObj -> GetProperty('NAME')
        PrintF, lun, '     ', name
    ENDFOR
    PrintF, lun, ''
    
    PrintF, lun, 'Dimensions: '
    FOR j=0,numDims-1 DO BEGIN
        dimsObj = self.dims -> Get(POSITION=j)
        name = dimsObj -> GetProperty('NAME')
        size = dimsObj -> GetProperty('SIZE')
        unlimited = dimsObj -> GetProperty('UNLIMITED')
        IF unlimited EQ 1 THEN unlimited = ' (unlimited)' ELSE unlimited = ""
        PrintF, lun, '     ', name, ': ', StrTrim(size,2), unlimited
    ENDFOR
    PrintF, lun, ''
    
    PrintF, lun, 'Variables: '
    FOR j=0,numVars-1 DO BEGIN
        varObj = self.vars -> Get(POSITION=j)
        name = varObj -> GetProperty('NAME')
        attrs = varObj -> GetProperty('ATTRS')
        PrintF, lun, '     ', name
        nattrs = attrs -> Count()
        FOR k=0,nattrs-1 DO BEGIN
            attrObj = attrs -> Get(POSITION=k)
            attrname = attrObj -> GetProperty('NAME')
            PrintF, lun, '          ', attrname
        ENDFOR
    ENDFOR
    PrintF, lun, ''
    
    ; Clean up.
    IF lun NE -1 THEN Free_Lun, lun
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::ParseFile                                                              
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Parses the file and creates the appropriate file objects for all subsequent        
;    object operations.                                                                                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> ParseFile                                                       
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
PRO NCDF_File::ParseFile

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; Only need to parse this file once.
    IF self.fileHasBeenParsed THEN RETURN
    
    ; Purge all containers and destroy all objects in the container.
    theseObjects = self.attrs -> Get(/ALL, COUNT=objCount)
    self.attrs -> Remove, /ALL
    FOR j=0,objCount-1 DO Obj_Destroy, theseObjects[j]
    
    theseObjects = self.dims -> Get(/ALL, COUNT=objCount)
    self.dims -> Remove, /ALL
    FOR j=0,objCount-1 DO Obj_Destroy, theseObjects[j]

    theseObjects = self.vars -> Get(/ALL, COUNT=objCount)
    self.vars -> Remove, /ALL
    FOR j=0,objCount-1 DO Obj_Destroy, theseObjects[j]
    
    ; Gather information about the file.
    info = NCDF_Inquire(self.fileID)
    
    ; Parse the global variables.
    num_attr = info.ngatts
    IF num_attr GT 0 THEN BEGIN
       FOR j=0,num_attr-1 DO BEGIN
           attribute_name = NCDF_AttName(self.fileID, j, /GLOBAL)
           NCDF_AttGet, self.fileID, attribute_name, theAttribute, /GLOBAL
           self -> CreateAttrObj, attribute_name
       ENDFOR
    ENDIF
    
    ; Parse the dimensions.
    num_dims = info.ndims
    IF num_dims GT 0 THEN BEGIN
        FOR j=0,num_dims-1 DO BEGIN
            NCDF_DIMINQ, self.fileID, j, dimension_name, dimension_size
            self -> CreateDimObj, dimension_name
        ENDFOR
    ENDIF

    ; Parse the variables.
    num_vars = info.nvars
    IF num_vars GT 0 THEN BEGIN
        FOR j=0,num_vars-1 DO BEGIN
            varinfo = NCDF_VarInq(self.fileID, j)
            self -> CreateVarObj, varinfo.name
         ENDFOR
    ENDIF
      
    self.fileHasBeenParsed = 1
      
END



;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::SetMode                                                                
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Sets the file mode to DEFINE or DATA, as needed. Only one of the two keywords      
;    should be used.                                                                                                  
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> SetMode, DATA=data , DEFINE=define                              
;                                                                           
; Auguments:                                                                
;                                                                           
;    None.                                                                  
;                                                                           
; Keywords:                                                                 
;                                                                           
;    DATA:       If this keyword is set, the file is put into DATA mode.    
;    DEFINE:     If this keyword is set, the file is put into DEFINE mode.  
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::SetMode, DATA=data, DEFINE=define

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling. 
    CATCH, theError
    IF theError NE 0 THEN BEGIN
       CATCH, /CANCEL
       self.errorLogger -> AddError
       RETURN
    ENDIF
    
    ; Can't have both keywords set.
    IF Keyword_Set(define) && Keyword_Set(data) THEN BEGIN
        Message, 'You cannot set both DEFINE and DATA keywords.'
    ENDIF
    
    ; If nothing is set, put us into DEFINE mode.
    IF ~Keyword_Set(define) && ~Keyword_Set(data) THEN define = 1

    ; Set define mode.
    IF Keyword_Set(define) THEN BEGIN
        data = 0
        IF self.define NE 1 THEN BEGIN
            NCDF_Control, self.fileID, /REDEF
            self.define = 1
        ENDIF
    ENDIF

    ; Set data mode.
    IF Keyword_Set(data) THEN BEGIN
        IF self.define NE 0 THEN BEGIN
            NCDF_Control, self.fileID, /ENDEF
            self.define = 0
        ENDIF
    ENDIF

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::Sync                                                              
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes data in memory to the disk.                                                                                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> Sync                                                       
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
PRO NCDF_File::Sync

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling. 
    CATCH, theError
    IF theError NE 0 THEN BEGIN
       CATCH, /CANCEL
       self.errorLogger -> AddError
       RETURN
    ENDIF
    
    ; The file has to be writable to sync the file.
    IF ~self.writable THEN Message, 'Cannot sync a READ-ONLY file.'

    ; The file has to be in DATA mode.
    self -> SetMode, /DATA
    
    ; Sync the file.
    NCDF_Control, self.fileID, /SYNC
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::WriteVarData                                                          
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes variable data into this netCDF file. It assumes the variable has previously 
;    been defined for this file.                                                                                      
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> WriteVarData, varName, data, COUNT=count, OFFSET=offset, STRIDE=stride      
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to write data to.     
;    data:       The data to be written into this variable.                 
;                                                                           
; Keywords:                                                                 
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
; Notes: The variable will have had to have been previously defined for the file.       
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::WriteVarData, varName, data, COUNT=count, OFFSET=offset, STRIDE=stride

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; The file has to be writable to add a variable definition.
    IF ~self.writable THEN Message, 'Cannot add a variable definition to a READ-ONLY file.'

    ; Check parameters.
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'A variable name or variable object reference is required.'
    IF N_Elements(data) EQ 0 THEN $
               Message, 'Data is required to write the variable into the file.'
    
    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
               Message, 'Invalid object with name ' + varName + ' has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE
    
    ; Get the variable ID.
    varName = varObj -> GetName()
    
    ; Put the file into data mode.
    self -> SetMode, /DATA

    ; Write the data to the file.
    NCDF_VarPut, self.fileID, varName, data, COUNT=count, OFFSET=offset, STRIDE=stride
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::WriteVarDef                                                            
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes the variable definition into this netCDF file.                                                            
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> WriteVarDef, varName, dimNames, DATATYPE=datatype, OBJECT=object
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive name of the variable you wish to define.
;    dimNames:   The names of dimensions that have been previously defined in the       
;                file and that are associated with this variable. A string array.       
;                If dimNames is missing, then the variable is assumed to be a scalar.   
;                                                                           
; Keywords:        
; 
;    CHUNK_DIMENSIONS: Set this keyword equal to a vector containing the chunk dimensions for the variable.
;                A new NetCDF variable is chunked by default, using a default chunk value that is 
;                the full dimension size for limited dimensions, and 1 for unlimited dimensions.
;                CHUNK_DIMENSIONS must have the same number of elements as the number of dimensions 
;                specified by Dim. If the CONTIGUOUS keyword is set, the value of the 
;                CHUNK_DIMENSIONS keyword is ignored. Available only in IDL 8.0 and higher.
;    CONTINUOUS:  Set this keyword to store a NetCDF variable as a single array in a file. 
;                Contiguous storage works well for smaller variables such as coordinate variables.
;                Contiguous storage works only for fixed-sized datasets (those without any unlimited 
;                dimensions). You can’t use compression or other filters with contiguous data.
;                If the CONTIGUOUS keyword is set, the value of the CHUNK_DIMENSIONS keyword is ignored.
;                The CONTIGUOUS keyword is ignored if the GZIP keyword is set. Available only in 
;                IDL 8.0 and higher.
;    DATATYPE:   The netCDF data type of the variable. This is REQUIRED. The appropriate
;                netCDF data types are: "BYTE", "CHAR", "SHORT", "LONG" "FLOAT", or     
;                "DOUBLE". In IDL 8.1, the data types "STRING", "UBYTE", UINT64",
;                "ULONG" and "USHORT" were added.      
;    GZIP:       Set this keyword to an integer between zero and nine to specify the level 
;                of GZIP compression applied to the variable. Lower compression values result 
;                in faster but less efficient compression. This keyword is ignored if the 
;                CHUNK_DIMENSIONS keyword is not set. This keyword is ignored if the CONTIGUOUS 
;                keyword is set. If the GZIP keyword is set, the CONTIGUOUS keyword is ignored.
;                You can only use GZIP compression with NCDF 4 files. Available only in 
;                IDL 8.0 and higher.
;                
;    OBJECT:     If a variable is successfully defined, this keyword will return the    
;                object reference to that variable.         
;    SHUFFLE:    Set this keyword to apply the shuffle filter to the variable. If the GZIP 
;                keyword is not set, this keyword is ignored. The shuffle filter de-interlaces blocks 
;                of data by reordering individual bytes. Byte shuffling can sometimes 
;                increase compression density because bytes in the same block positions 
;                often have similar values, and grouping similar values together often 
;                leads to more efficient compression. Available only in IDL 8.0 and higher.             
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::WriteVarDef, varName, dimNames, $
    CHUNK_DIMENSIONS=chunk_dimensions, $
    CONTINUOUS=continuous, $
    DATATYPE=datatype, $
    GZIP=gzip, $
    OBJECT=object, $
    SHUFFLE=shuffle

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; The file has to be writable to add a variable definition.
    IF ~self.writable THEN $
               Message, 'Cannot add a variable definition to a READ-ONLY file.'

    ; Check parameters.
    IF N_Elements(varName) EQ 0 THEN $
               Message, 'Variable name is required for variable definition.'
    IF N_Elements(datatype) EQ 0 THEN Message, 'Variable data type is required.'
    
    ; Check the data type to see that it conforms to netCDF protocol.
    CASE StrUpCase(datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
        'INT': tshort = 1
        'STRING': tchar = 1
        'UBYTE': tubtye = 1
        'ULONG': tulong = 1
        'UINT64': tuint64 = 1
        'UINT': tuint = 1
        'USHORT': tushort = 1
        ELSE: Message, 'Unknown DATATYPE for netCDF files: ' + datatype
    ENDCASE
    
    ; If the dimension names are present, use them to get the dimension IDs, which are
    ; needed to define the variable.
    ndims = N_Elements(dimNames)
    IF ndims NE 0 THEN BEGIN
        dimIDs = LonArr(ndims)
        IF Size(dimNames, /TNAME) EQ 'STRING' THEN BEGIN
            FOR j=0,ndims-1 DO BEGIN
                dimObj = self.dims -> FindByName(dimNames[j],  COUNT=dimCount, /CASE_SENSITIVE)
                CASE dimCount OF
                    0: Message, 'Cannot find dimension object with the name: ' + dimNames[j]
                    1: BEGIN
                       dimIDs[j] = dimObj -> GetID()
                       END
                    2: Message, 'Found more than one dimension with the name: ' + dimNames[j]
                 ENDCASE
            ENDFOR
        ENDIF ELSE BEGIN
            dimIDs = dimNames
        ENDELSE
    ENDIF
    
    ; Put the file into define mode.
    self -> SetMode, /DEFINE
    
    ; Define the variable.
    IF N_Elements(dimIDs) EQ 0 THEN BEGIN
    
        release = Float(!Version.Release)
        CASE 1 OF
        
            (release LT 8.0) && (!Version.Release NE '7.1.1'): BEGIN
                varID = NCDF_VarDef(self.fileID, varName, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    LONG=tlong, $
                    SHORT=tshort)
                 END
                 
            (!Version.Release EQ '7.1.1') || ((release GE 7.2) && (release LT 8.1)): BEGIN
                varID = NCDF_VarDef(self.fileID, varName, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    CHUNK_DIMENSIONS=chunk_dimensions, $
                    CONTIGUOUS=contiguous, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    GZIP=gzip, $
                    LONG=tlong, $
                    SHORT=tshort, $
                    SHUFFLE=shuffle)
                 END
                 
            release GE 8.1: BEGIN
                varID = NCDF_VarDef(self.fileID, varName, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    CHUNK_DIMENSIONS=chunk_dimensions, $
                    CONTIGUOUS=contiguous, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    GZIP=gzip, $
                    LONG=tlong, $
                    SHORT=tshort, $
                    SHUFFLE=shuffle, $
                    STRING=tchar, $
                    UBYTE=tubyte, $
                    UINT64=tuint64, $
                    ULONG=tulong, $
                    USHORT=tushort)
                 END
        ENDCASE
        
    ENDIF ELSE BEGIN
    
        release = Float(!Version.Release)
        CASE 1 OF
        
            (release LT 8.0) && (!Version.Release NE '7.1.1'): BEGIN
                varID = NCDF_VarDef(self.fileID, varName, dimIDs, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    LONG=tlong, $
                    SHORT=tshort)
                 END
                 
            (!Version.Release EQ '7.1.1') || ((release GE 7.2) && (release LT 8.1)): BEGIN
                varID = NCDF_VarDef(self.fileID, varName, dimIDs, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    CHUNK_DIMENSIONS=chunk_dimensions, $
                    CONTIGUOUS=contiguous, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    GZIP=gzip, $
                    LONG=tlong, $
                    SHORT=tshort, $
                    SHUFFLE=shuffle)
                 END
                 
            release GE 8.1: BEGIN
                varID = NCDF_VarDef(self.fileID, varName, dimIDs, $
                    BYTE=tbyte, $
                    CHAR=tchar, $
                    CHUNK_DIMENSIONS=chunk_dimensions, $
                    CONTIGUOUS=contiguous, $
                    DOUBLE=tdouble, $
                    FLOAT=tfloat, $
                    GZIP=gzip, $
                    LONG=tlong, $
                    SHORT=tshort, $
                    SHUFFLE=shuffle, $
                    STRING=tchar, $
                    UBYTE=tubyte, $
                    UINT64=tuint64, $
                    ULONG=tulong, $
                    USHORT=tushort)
                 END
         ENDCASE
    ENDELSE
    
    ; Create a variable object and add it to the variable list.
    self -> CreateVarObj, varName
    
    ; Need to return the variable object?
    IF Arg_Present(object) THEN BEGIN
        object = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
        IF varCount EQ 0 THEN $
            Message, 'Cannot find an object with name ' + varName + $
                ' in the object container.'
    ENDIF
    
END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::WriteDim                                                               
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes the variable definition into this netCDF file.                                                            
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> WriteDim, dimName, dimSize, UNLIMITED=unlimited, OBJECT=object  
;                                                                           
; Auguments:                                                                
;                                                                           
;    dimName:    The case sensitive name of the dimension you wish to define.           
;    dimSize:    The size of the dimension. Required, unless UNLIMITED is set.          
;                                                                           
; Keywords:                                                                 
;                                                                           
;    UNLIMITED:  Set this keyword if you wish this to be an unlimited dimension.        
;                In general, only one unlimited dimension is allowed per netCDF file.   
;    OBJECT:     If a dimension is successfully defined, this keyword will return the   
;                dimension object reference to that dimension.                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::WriteDim, dimName, dimSize, UNLIMITED=unlimited, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; The file has to be writable to add a dimension.
    IF ~self.writable THEN Message, 'Cannot add a dimension to a READ-ONLY file.'

    ; Check parameters.
    IF N_Elements(dimName) EQ 0 THEN $
               Message, 'The name of the dimension is required.'
    IF ~Keyword_Set(unlimited) THEN BEGIN
        IF N_Elements(dimSize) EQ 0 THEN $
               Message, 'The size of the dimension is required.'
    ENDIF
    
    ; Put the file into define mode
    self -> SetMode, /DEFINE
    
    ; Add the dimension to the file.
    IF Keyword_Set(unlimited) THEN BEGIN
        void = NCDF_DimDef(self.fileID, dimName, /UNLIMITED)
    ENDIF ELSE BEGIN
        void = NCDF_DimDef(self.fileID, dimName, dimSize)
    ENDELSE
    
    ; Create a dimension object and add it to the dimension container.
    self -> CreateDimObj, dimName
    
    ; Need to return the dimension object?
    IF Arg_Present(object) THEN BEGIN
        object = self.dims -> FindByName(dimName, COUNT=dimCount, /CASE_SENSITIVE)
        IF dimCount EQ 0 THEN $
            Message, 'Cannot find an object with name ' + dimName + $
                ' in the object container.'
    ENDIF

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::WriteGlobalAttr                                                        
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes a global attribute into this netCDF file.                                                                 
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> WriteGlobalAttr, attrName, attrValue, DATATYPE=datatype, OBJECT=object      
;                                                                           
; Auguments:                                                                
;                                                                           
;    attrName:   The case sensitive name of the global attribute you wish to write.     
;    attrValue:  The value of the attribute. Required.                      
;                                                                           
; Keywords:                                                                 
;                                                                           
;    DATATYPE:   The netCDF data type of the variable. This is REQUIRED. The appropriate
;                netCDF data types are: "BYTE", "CHAR", "SHORT", "LONG" "FLOAT", or     
;                "DOUBLE".                                                  
;    OBJECT:     If a dimension is successfully defined, this keyword will return the   
;                dimension object reference to that dimension.                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::WriteGlobalAttr, attrName, attrValue, DATATYPE=datatype, OBJECT=object

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; The file has to be writable to add a global attribute.
    IF ~self.writable THEN $
               Message, 'Cannot add a global attribute to a READ-ONLY file.'
    
    ; Check parameters.
    IF N_Elements(attrName) EQ 0 THEN Message, 'The attribute name is required.'
    IF N_Elements(attrValue) EQ 0 THEN Message, 'The attribute value is required.'
    IF N_Elements(datatype) EQ 0 THEN datatype = NCDF_CastDataType(attrValue)
    
    ; Make sure the attribute name doesn't have spaces.
    parts = StrSplit(attrName, /EXTRACT)
    IF N_Elements(parts) GT 1 THEN $
        Message, 'Global attribute names cannot have spaces in them.'
    
    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
        'INT': tshort = 1
        'STRING': tchar = 1
        'ULONG': tlong = 1
        'UINT': tlong = 1
        ELSE: Message, 'Unknown DATATYPE for netCDF files: ' + datatype        
    ENDCASE
    
    ; Put the file into define mode
    self -> SetMode, /DEFINE
    
    ; Add the attribute to the file.
    NCDF_AttPut, self.fileID, attrName, attrValue, $
        /GLOBAL, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=length, $
        LONG=tlong, $
        SHORT=tshort
    
    ; Create an attribute object and add it to the attribute container.
    self -> CreateAttrObj, attrName
    
    ; Need to return the dimension object?
    IF Arg_Present(object) THEN BEGIN
        object = self.attrs -> FindByName(attrName, COUNT=attrCount, /CASE_SENSITIVE)
        IF attrCount EQ 0 THEN $
            Message, 'Cannot find an object with name ' + attrName + $
                ' in the object container.'
    ENDIF

END


;------------------------------------------------------------------------------------------;
;                                                                           
; NAME:                                                                     
;    NCDF_File::WriteVarAttr                                                           
;                                                                           
; Purpose:                                                                  
;                                                                           
;    Writes a variable attribute into this netCDF file.                                                               
;                                                                           
; Method Syntax:                                                            
;                                                                           
;    obj -> WriteVarAttr, varName, attrName, attrValue, DATATYPE=datatype   
;                                                                           
; Auguments:                                                                
;                                                                           
;    varName:    The case sensitive variable name for which the attribute is being      
;                defined.                                                   
;    attrName:   The case sensitive name of the global attribute you wish to write.     
;    attrValue:  The value of the attribute. Required.                      
;                                                                           
; Keywords:                                                                 
;                                                                           
;    DATATYPE:   The netCDF data type of the variable. The appropriate netCDF data types
;                are: "BYTE", "CHAR", "SHORT", "LONG" "FLOAT", or "DOUBLE". 
;    OBJECT:     If a dimension is successfully defined, this keyword will return the   
;                dimension object reference to that dimension.                
;                                                                           
;------------------------------------------------------------------------------------------;
PRO NCDF_File::WriteVarAttr, varName, attrName, attrValue, DATATYPE=datatype

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        self.errorLogger -> AddError
        RETURN
    ENDIF
    
    ; The file has to be writable to add a variable attribute.
    IF ~self.writable THEN Message, 'Cannot add a variable attribute to a READ-ONLY file.'
    
    ; Check parameters.
    IF N_Elements(attrName) EQ 0 THEN Message, 'The attribute name is required.'
    IF N_Elements(attrValue) EQ 0 THEN Message, 'The attribute value is required.'
    IF N_Elements(varName) EQ 0 THEN Message, 'A variable name or object is required.'
    IF N_Elements(datatype) EQ 0 THEN datatype = NCDF_CastDataType(attrValue)
    
    ; Make sure the attribute name doesn't have spaces.
    parts = StrSplit(attrName, /EXTRACT)
    IF N_Elements(parts) GT 1 THEN $
        Message, 'Variable attribute names cannot have spaces in them.'
    
    ; Were you passed the name of a variable or a variable object?
    CASE Size(varName, /TNAME) OF
    
        'STRING': BEGIN
            varObj = self.vars -> FindByName(varName, COUNT=varCount, /CASE_SENSITIVE)
            IF varCount EQ 0 THEN $
               Message, 'Cannot find a variable object with name ' + varName + '.'
            IF ~Obj_Valid(varObj) THEN $
                Message, 'Invalid object with name "' + varName + '" has been found.'
            END
            
         'OBJREF': BEGIN
            varObj = varName
            END
    
        ELSE: Message, 'Input variable name or object is the wrong data type.'
    ENDCASE

    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
        'INT': tshort = 1
        'STRING': tchar = 1
        'ULONG': tlong = 1
        'UINT': tlong = 1
        ELSE: Message, 'Unknown DATATYPE for netCDF files: ' + datatype
    ENDCASE
    
    ; Is this a valid variable object?
    IF ~Obj_Valid(varObj) THEN Message, 'Invalid variable object. Cannot add attribute.'
    
    ; Put the file into define mode
    self -> SetMode, /DEFINE
    
    ; Add the attribute to the variable.
    varObj -> AddAttr, attrName, attrValue, DATATYPE=datatype
    
END


;
;--------------------------------------------------------------------------------------------------
; NAME:
;       NCDF_FILE::CLEANUP
;
; PURPOSE:
;
;       The cleanup method for the NCDF_FILE object.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;       
;       None.
;
;----------------------------------------------------------------------------------------------
PRO NCDF_File::CLEANUP

   IF self.fileID GT 0 THEN NCDF_Close, self.fileID
   self.errorLogger -> CloseFile
   Obj_Destroy, self.errorLogger
   Obj_Destroy, self.vars
   Obj_Destroy, self.attrs
   Obj_Destroy, self.dims
   
END
   

;--------------------------------------------------------------------------------------------------
; NAME:
;       NCDF_FILE::INIT
;
; PURPOSE:
;
;       The initialization method for the NCDF_FILE object.
;
; ARGUMENTS:
;
;       filename:  The name of a netCDF file to open.
;
; KEYWORD PARAMETERS:
;       
;       ALERT:     Set this keyword if you wish to have alert from the object's error logger.
;                  Input. Default is 1.
;       
;       BROWSE:    If this keyword is set, the Browse Window is invoked as soon
;                  as the object is initiated. Input. Default is 0.
;
;       CLOBBER:   Set this keyword if you are opening a netCDF file that already exists and 
;                  you want to overwrite the existing file. Input. Default is 0.
;                  
;       CREATE:    Set this keyword if you wish to create a new netCDF file to write
;                  into. Input. Default is 0, which means the file will be opened as 
;                  "read-only".
;       
;       DELETE_ON_DESTROY:  Set this keyword if you wish to delete the error log file when
;                  the ErrorLogger object is destroyed. This will only happen if the ErrorLogger
;                  object is not in an error state. Input. Default is 1.
;                  
;       ERRORLOGGERNAME: The name of the ErrorLogger filename that captures errors from this
;                   program. Optional. If not provided a default name will be provided, based
;                   on the current local system time.
;                  
;       MODIFY:    Set this keyword if you wish to modify (write to) a file you are opening.
;                  If not set, the file will be opened as "read-only".
;                  
;       NETCDF4_FORMAT: Set this keyowrd to create a new NetCDF 4 file. In NetCDF 4 files, data 
;                  is created and accessed with the HDF5 library. NetCDF 4 files are valid HDF5 files, 
;                  and may be read with HDF5 routines. Note that if a NetCDF 4 file is modified using 
;                  the HDF5 routines, rather than with the NetCDF 4 routines, the file is no longer a 
;                  valid NetCDF 4 file, and may no longer be readable with the NetCDF routines.
;                  You need IDL 8.0 to use this keyword.
;                  
;       NOCLUTTER: Set the keyword to set the ErrorLogger NOCLUTTER keyword.
;
;       TIMESTAMP: Set this keyword is you want the ErrorLogger filename to have a time stamp
;                  appended to it.
;
;----------------------------------------------------------------------------------------------
FUNCTION NCDF_FILE::INIT, filename, $
    ALERT=alert, $
    BROWSE=browse, $
    CLOBBER=clobber, $
    CREATE=create, $
    DELETE_ON_DESTROY=delete_on_destroy, $
    ERRORLOGGERNAME=errorLoggerName, $
    MODIFY=modify, $
    NETCDF4_FORMAT=netcdf4_format, $
    NOCLUTTER=noclutter, $
    TIMESTAMP=timestamp

    ; Compiler options.
    Compile_Opt DEFINT32
    Compile_Opt STRICTARR
    Compile_Opt STRICTARRSUBS
    Compile_Opt LOGICAL_PREDICATE

    ; Error handling. Return 0 if can't finish.
    CATCH, theError
    IF theError NE 0 THEN BEGIN
       CATCH, /CANCEL
       self.errorLogger -> AddError
       Obj_Destroy, self.errorLogger
       IF self.fileID GT 0 THEN NCDF_Close, self.fileID
       RETURN, 0
    ENDIF
   
    ; Create the error logger.
    self.errorLogger = Obj_New('ErrorLogger', errorLoggerName, ALERT=1, $
        DELETE_ON_DESTROY=1, TIMESTAMP=Keyword_Set(timestamp), NOCLUTTER=noclutter)
        
    ; Check keywords.
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filters = ['*.nc', '*.ncdf*']
        filename = Dialog_Pickfile(FILTER=filters, TITLE='Select netCDF File...')
        IF filename EQ "" THEN RETURN, 0
    ENDIF

    ; Make sure you have an absolute filename.
    root_name = File_Basename(filename)
    IF root_name EQ filename THEN BEGIN
        CD, CURRENT=thisDir
        absFilename = FilePath(ROOT_DIR=thisDir, filename)
        filename = absFilename
    ENDIF
    
    ; Set default values, if keywords are not already set.
    SetDefaultValue, alert, 1, /Boolean
    SetDefaultValue, create, 0, /Boolean
    SetDefaultValue, clobber, 0, /Boolean
    SetDefaultValue, modify, 0, /Boolean
    SetDefaultValue, delete_on_destroy, 1, /Boolean
    SetDefaultValue, noclutter, 0, /Boolean
    SetDefaultValue, netcdf4_format, 0, /Boolean
    self.errorLogger -> SetProperty, ALERT=alert, DELETE_ON_DESTROY=delete_on_destroy, NOCLUTTER=noclutter
    
    ; If you are not going to create the file, you are going to open it.
    ; If you want to modify the file, it is going to be writable.
    mode = create ? 'CREATE' : 'OPEN'
    self.writable = (mode EQ 'OPEN' && modify) || mode EQ 'CREATE'
    
    ; Are you creating the file? Make sure it doesn't already exist. If it does,
    ; you will have to clobber it.
    IF mode EQ 'CREATE' THEN BEGIN
        IF File_Test(filename) THEN BEGIN
            CASE clobber OF
                1: File_Delete, filename, /ALLOW_NONEXISTENT
                0: Message, "The specified netCDF file already exists and cannot " + $
                        "be overwritten unless CLOBBER is set."
            ENDCASE
        ENDIF
    ENDIF

    ; If the user wants to open the file, make sure it is possible.
    IF mode EQ 'OPEN' THEN BEGIN
        IF ~File_Test(filename, /READ) THEN $
            Message, "The specified netCDF file cannot be opened for reading: " + filename
        IF modify THEN BEGIN
            IF ~File_Test(filename, /WRITE) THEN $
                Message, "The specified netCDF file cannot be opened for writing: " + filename
        ENDIF
    ENDIF
    
    ; Store the filename.
    self.filename = filename

    ; Open or create the file.
    CASE mode OF
        'OPEN': BEGIN
               self.fileID = NCDF_Open(self.filename, WRITE=self.writable)
               self.define = 0
               END
        'CREATE': BEGIN
               IF (!Version.Release EQ '7.1.1') || (Float(!Version.Release) GE 7.2) THEN BEGIN
                   self.fileID = NCDF_Create(self.filename, CLOBBER=clobber, NETCDF4_FORMAT=netcdf4_format)
               ENDIF ELSE BEGIN
                   self.fileID = NCDF_Create(self.filename, CLOBBER=clobber)
               ENDELSE
               self.define = 1
               END
    ENDCASE
    
    ; Initialize object containers for contents.
    self.vars = Obj_New('NCDF_Container')
    self.attrs = Obj_New('NCDF_Container')
    self.dims = Obj_New('NCDF_Container')
    
    ; Did the user want to browse this file?
    IF Keyword_Set(browse) THEN self -> Browse
    
    ; If this file has been opened for reading or modifying, then parse it.
    IF ~self.writable THEN self -> Parsefile
    IF Keyword_Set(modify) THEN self -> Parsefile
        
    RETURN, 1
    
END ; --------------------------------------------------------------------------------------------


PRO NCDF_File__DEFINE, class

    class = { NCDF_FILE, $
              filename: "", $           ; The name of the netCDF file.
              fileID: 0L, $             ; The netCDF file ID.
              errorlogger: Obj_New(), $ ; The error logger object.
              vars: Obj_New(), $        ; An object container containing variable objects.
              attrs: Obj_New(), $       ; An object container containing global attribute objects.
              dims: Obj_New(), $        ; An object container containing dimension objects.
              writable: 0B, $           ; A flag that indicates the file is writable.
              fileHasBeenParsed: 0B, $  ; A flag that is 1 when the file has been parsed.
              file_extension: "", $     ; A file extension that is used with Dialog_Pickfile()
              define: 0B  $             ; A flag that indicates the file is in define mode. 
            }
            
END ; --------------------------------------------------------------------------------------------
