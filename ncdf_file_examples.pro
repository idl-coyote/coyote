;+
; NAME:
;       NCDF_FILE_EXAMPLES
;
; PURPOSE:
;
;       This is a utility routine demonstrates the several ways it is possible
;       to use the NCDF_FILE object to create netCDF files, copy information
;       from one netCDF file to another, and to read information from a netCDF
;       file.
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
;       Utilities
;
; CALLING SEQUENCE:
;
;       NCDF_File_Examples
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 3 February 2010.
;       Updated to use a time variable for the frame number. 29 Oct 2011.
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
PRO NCDF_File_Examples

; This is an example program to demonstrate some of the capabilities
; of the NCDF_FILE objects and its related objects.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        Obj_Destroy, fileObj
        Obj_Destroy, sObj
        Obj_Destroy, dObj
        RETURN
    ENDIF
    
    ; Initial definition of objects, in case there is a problem.
    ; This will allow proper cleanup of open netCDF files.
    fileObj = Obj_New()
    sObj = Obj_New()
    dObj = Obj_New()

    ; Open a new file for writing.
    fileObj = Obj_New("NCDF_File", /CREATE, /CLOBBER, 'ncdf_example.nc')
    IF Obj_Valid(fileObj) EQ 0 THEN $
        Message, 'Invalid file object returned from NCDF_FILE INIT.'
    
    ; Create some data to put into the file.
    data = cgDemoData(18)
    dims = Size(data, /DIMENSIONS)
    colortable = 34
    
    ; Add global attributes to the file.
    fileObj -> WriteGlobalAttr, DATATYPE='CHAR', 'title', $
        'NCDF_FILE Example File.'
    fileObj -> WriteGlobalAttr, DATATYPE='CHAR', 'description', $
        'An example of how to create a netCDF file with the NCDF_FILE object.'
    fileObj -> WriteGlobalAttr, 'created', Systime(), DATATYPE='CHAR'
    
    ; Add dimensions to the file.
    fileObj -> WriteDim, 'xsize', dims[0], OBJECT=xdimObj
    fileObj -> WriteDim, 'ysize', dims[1], OBJECT=ydimObj
    fileObj -> WriteDim, 'time', 24, OBJECT=timeDimObj
    
    ; Get the dimension names.
    dimNames = [xdimObj->GetName(), ydimObj->GetName(), timeDimObj->GetName()]

    ; Define a variable for the file.
    fileObj -> WriteVarDef, 'data', dimNames, DATATYPE='BYTE', OBJECT=dataObj
    IF Obj_Valid(dataObj) EQ 0 THEN Message, 'Invalid data object returned.'
    
    ; Define variable attributes.
    fileObj -> WriteVarAttr, dataObj, 'comment', 'Frames of a data animation.'
    fileObj -> WriteVarAttr, 'data', 'colortable', colortable, DATATYPE='SHORT'
    
    ; Define a time variable for the file. Months, starting 15 Jan 2008 and going
    ; for 24 months. Offset to 1 Jan 1600 00:00:00.
    offset = Julday(1,1,1600,0,0,0)
    time = TimeGen(24, START=Julday(01,15,2008, 0, 0, 0), UNITS='Months', STEP_SIZE=1) - offset
    fileObj -> WriteVarDef, 'time', [timeDimObj->GetName()], DATATYPE='DOUBLE', OBJECT=timeObj
    IF Obj_Valid(timeObj) EQ 0 THEN Message, 'Invalid time object returned.'
    
    ; Define variable attributes.
    fileObj -> WriteVarAttr, timeObj, 'Units', 'Julian days since 1 January 1600 00:00:00.'

    ; Write the data to the file. Here we are going to write 24 frames of data.
    FOR j=0,23 DO BEGIN
        fileObj -> WriteVarData, 'data', cgDemoData(18), OFFSET=[0,0,j]
    ENDFOR
    
    ; Write the time into the file.
    fileObj -> WriteVarData, 'time', time
    
    ; Sync the file by writing memory to disk.
    fileObj -> Sync
    
    ; Browse the file.
    fileObj -> Browse, XOFFSET=250, YOFFSET=250, TITLE='Example File'
    
    ; Destroy this file object.
    Obj_Destroy, fileObj

;**************************************************************************************
;**************************************************************************************

    ; Open the file you just created and copy the information in it to another file.
    sourceFile = 'ncdf_example.nc'
    destFile = 'ncdf_example_copy.nc'
    
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', sourceFile, $
        ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ; Open the destination file for writing.
    dObj = Obj_New('NCDF_FILE', destFile, /CREATE, /CLOBBER, $
        ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    
    ; Find all the global attributes in the source file and copy them.
    attrNames = sObj -> GetGlobalAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
        sObj -> CopyGlobalAttrTo, attrNames[j], dObj
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    dimNames = sObj -> GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
        sObj -> CopyDimTo, dimNames[j], dObj
    ENDFOR
  
    ; Find all the variable definitions, attributes and data in the 
    ; source file and copy them.
    varNames = sObj -> GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
        sObj -> CopyVarDefTo, varNames[j], dObj
        varAttrNames = sObj -> GetVarAttrNames(varNames[j], COUNT=varAttrCount)
        FOR k=0,varAttrCount-1 DO BEGIN
            sObj -> CopyVarAttrTo, varNames[j], varAttrNames[k], dObj
        ENDFOR
        sObj -> CopyVarDataTo, varNames[j], dObj
    ENDFOR

    ; Sync the destination file.
    dObj -> Sync
    
    ; Browse the destination file.
    dObj -> Browse, XOFFSET=350, YOFFSET=375, TITLE='Copy of Example File'
    
    ; Destroy both the source and destination objects.
    Obj_Destroy, dObj
    Obj_Destroy, sObj
    
;**************************************************************************************
;**************************************************************************************

    ; Open the original file and make an annimation out of the data in the file.
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', sourceFile)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ct = sObj -> GetVarAttrValue('data', 'colortable')
    LoadCT, ct, /SILENT
    
    check = sObj -> HasDim('xsize', OBJECT=xsizeObj)
    IF check THEN xsize = xsizeObj -> GetSize() ELSE Message, 'Cannot find dimension XSIZE.'

    check = sObj -> HasDim('ysize', OBJECT=ysizeObj)
    IF check THEN ysize = ysizeObj -> GetSize() ELSE Message, 'Cannot find dimension YSIZE.'
    
    check = sObj -> HasDim('time', OBJECT=timeDimObj)
    IF check THEN numMonths = timeDimObj -> GetSize() ELSE Message, 'Cannot find dimension TIME.'
    
    ; Set up the animation.
    XInterAnimate, SET=[xsize, ysize, numMonths], /SHOWLOAD
    time = sObj -> GetVarData('time')
    time = time + Julday(1,1,1600,0,0,0)
    FOR j=0,numMonths-1 DO BEGIN
        data = sObj -> GetVarData('data', COUNT=[xsize, ysize, 1], OFFSET=[0,0,j])
        cgImage, data, /KEEP, /NOINTERP
        CalDat, time[j], month, day, year
        theDate = StrTrim(day,2) + ' ' + cgMonths(month) + ' ' + StrTrim(year,2)
        cgText, 0.1, 0.05, /NORMAL, 'Date: ' + theDate, COLOR='black', FONT=0
        XInterAnimate, FRAME=j, WINDOW=!D.Window
    ENDFOR
    XInterAnimate, 25
    
    ; Destroy the file object.
    Obj_Destroy, sObj
    END
