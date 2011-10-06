;+
; NAME:
;       NCDF_CastDataType
;
; PURPOSE:
;
;       This is a utility routine to turn IDL data types into the equivalent
;       netCDF data type. In other words, change 'STRING' to 'CHAR' and so on.
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
;       ncdf_datatype = NCDF_CastDataType(variable)
;
; ARGUMENTS:
;
;       variable:      The IDL variable for which you want a netCDF data type.
;                      Or, if the TYPE keyword is set, the variable type index you wish
;                      to convert. Or, if the TNAME keyword is set, the variable type
;                      name you wish to convert.
;                      
; KEYWORDS:
; 
;        TYPE:         If set, the positional argument is an IDL variable type of
;                      the sort returned by the SIZE function with the TYPE keyword set.
;                      
;                       type = Size(variable, /TYPE)
;
;        TNAME:        If set, the positional argument is an IDL variable type of
;                      the sort returned by the SIZE function with the TNAME keyword set.
;                      
;                       type = Size(variable, /TNAME)
;
; RETURN VALUE:
;
;       ncdf_datatype: The netCDF data type of the variable. Possible values are
;                      'BYTE', 'CHAR', 'SHORT', 'LONG', 'FLOAT' and 'DOUBLE'.
;
; NOTES:
;
;     The program is designed to work with the NCDF_FILE object and related programs.
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 3 February 2010.
;       Made a UINT data type be cast to LONG, rather than SHORT. 29 April 2010. DWF.
;       Added TYPE and TNAME keywords. 5 May 2010. DWF.
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
FUNCTION NCDF_CastDataType, variable, TNAME=tname, TYPE=type

    ; Return to caller on error.
    On_Error, 2
    
    ; Only one keyword can be set at a time.
    IF (Keyword_Set(tname) + Keyword_Set(type)) EQ 2 THEN $
        Message, 'Only one keyword can be used at a time in NCDF_CastDataType.'
        
    ; Get the data type of the variable. This may be wrong, depending on which
    ; keywords are set, if any.
    datatype = Size(variable, /TNAME)
    
    ; Is the TNAME keyword set?
    IF Keyword_Set(tname) THEN datatype = variable
    
    ; Is the TYPE keyword set?
    IF Keyword_Set(type) THEN BEGIN
        CASE variable OF
            0: datatype = 'UNDEFINED'
            1: datatype = 'BYTE'
            2: datatype = 'INT'
            3: datatype = 'LONG'
            4: datatype = 'FLOAT'
            5: datatype = 'DOUBLE'
            6: datatype = 'COMPLEX'
            7: datatype = 'STRING'
            8: datatype = 'STRUCT'
            9: datatype = 'DCOMPLEX'
            10: datatype = 'POINTER'
            11: datatype = 'OBJREF'
            12: datatype = 'UINT'
            13: datatype = 'ULONG'
            14: datatype = 'LONG64'
            15: datatype = 'ULONG64'
            
        ENDCASE
    ENDIF

    ; What is the IDL data type of the variable? Cast
    ; to NCDF data type.
    CASE datatype OF
            'BYTE':
            'STRING': datatype = 'CHAR'
            'INT': datatype = 'SHORT'
            'UINT': datatype = 'LONG'
            'ULONG': datatype = 'LONG'
            'LONG': 
            'FLOAT':
            'DOUBLE':
            ELSE: Message, 'This IDL data type is not supported in netCDF files: ' + datatype
    ENDCASE

    RETURN, datatype
END
