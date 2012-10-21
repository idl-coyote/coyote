; docformat = 'rst'
;
; NAME:
;   cgHasImageMagick
;
; PURPOSE:
;   Searches for the ImageMagick "convert" command to see if ImageMagick is available 
;   on the system.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; :Description:
;   Searches for the ImageMagick "convert" command to see if ImageMagick is available 
;   on the system.
;
; :Categories:
;    Utilities
;    
; :Keywords:
;     version: out, optional, type=string
;        Returns the version number of the ImageMagick convert command, if found.
;          
; :Examples:
;    Used to determine if the ImageMagick convert command is available::
;       IDL> available = cgHasImageMagick(Version=version)
;       IDL> IF available THEN Print, version
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
;        Written, 17 January 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgHasImageMagick, VERSION=version

    ; Silent error handler. If anything goes wrong, we definitely
    ; can't use ImageMagick.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Return, 0
    ENDIF
    
    ; Assume failure.
    retval = 0
    
    ; To search for ImageMagick, I am going to spawn a call to convert.
    ; If the error_result is a null string, I assume this command worked.
    ; Otherwise, it didn't find ImageMagick.
    Spawn, 'convert -version', result, error_result
    
    ; If nothing is put in the error result, then we are good to go.
    IF error_result[0] EQ "" THEN retval = 1 
    
    ; For some reason Macs can't deal with the version number.
    IF retval EQ 1 && Arg_Present(version) THEN BEGIN
       parts = StrSplit(result[0], /EXTRACT)
       IF StrUpCase(!Version.OS) NE "DARWIN" THEN version = parts[2]
    ENDIF
    
    RETURN, retval
    
END
