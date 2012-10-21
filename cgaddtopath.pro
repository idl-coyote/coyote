; docformat = 'rst'
;
; NAME:
;   cgAddToPath
;
; PURPOSE:
; 
;   This procedure adds a directory to the beginning of the IDL !PATH
;   system variable. If a directory name is not passed in, the current
;   IDL working directory is used. The directory must currently exist
;   to be added.
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; Adds a directory to the beginning of the !PATH system variable.
;
; :Categories:
;    Utility
;    
; :Params:
;    theDirectory: in, optional, type=string
;       The name of the directory to add to !PATH. If missing, the current directory.
;       
; :Keywords:
;     verbose: in, optional, type=boolean
;       Set this keyword if you wish to receive confirmation of action.
;
; :Examples:
;    For example, to add a "programs" directory to the IDL path::
;       cgAddToPath, '/usr/david/IDL/programs/'
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
;     Written, 20 Sept 2010.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgADDTOPATH, theDirectory, VERBOSE=verbose

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF

    ; Need a directory?
    IF N_Elements(theDirectory) EQ 0 THEN CD, Current=theDirectory
    
    ; We don't need a final directory marker.
    IF StrMid(theDirectory, 0, 1, /REVERSE_OFFSET) EQ Path_Sep() THEN BEGIN
        strlength = StrLen(theDirectory)
        theDirectory = StrMid(theDirectory, 0, strlength-1)
    ENDIF
    
    ; Does the directory exist?
    IF File_Test(theDirectory, /DIRECTORY) EQ 0 THEN BEGIN
        Message, 'The directory (' + theDirectory + ') does not exist. Returning.'
    ENDIF
    
    ; Need confirmation?
    IF Keyword_Set(verbose) THEN BEGIN
        Print, 'Prepending the following directory to !PATH: "' + theDirectory + '"'
    ENDIF
    
    ; Add it to the path.
    dirSep = (StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? ';' : ':'
    !PATH = theDirectory + dirSep + !PATH
END
