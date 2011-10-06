;+
; NAME:
;    NCDF_ISVALIDFILE
;
; PURPOSE:
;
;    Utility routine to determine if a file is a valid netCDF file or not.
;    Returns a 1 if the file is valid and a 0 otherwise.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    result = NCDF_IsValidFile(filename)
;
; INPUTS:
;
;    filename:    The name of a filename to open to see if it is a valid netCDF file.
;
; RETURN VALUE:
;
;    result:  A 1 if the file can be opened as a netCDF file. A 0 otherwise.
;
; KEYWORDS:
;
;    None.
;
; ALGORITHM:
;
;    Try to open the file. If you fail, it is not an netCDF file.
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 21 February 2010.
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
FUNCTION NCDF_IsValidFile, filename

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        RETURN, 0
    ENDIF
    
    ; Need a filename.
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filename = Dialog_Pickfile(TITLE='Select a File', FILTER=['*.nc','*.ncdf'])
        IF filename EQ "" THEN RETURN, 0
    ENDIF
    
    ; Try to open the file. If you succeed, file is valid.
    fileID = NCDF_Open(filename)
    NCDF_Close, fileID
    
    RETURN, 1
    
END
