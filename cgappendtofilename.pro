; docformat = 'rst'
;
; NAME:
;   cgAppendToFilename
;
; PURPOSE:
;    The purpose of this routine is to allow the user append text to the end of the
;    root name of an absolute file name. The text is appended to the end of the root
;    name (see cgRootName) and before the final file extension.
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
; The purpose of this routine is to allow the user append text to the end of the
; root name of an absolute file name. The text is appended to the end of the root
; name (see `cgRootName`) and before the final file extension.
;
; :Categories:
;    Utilities
;    
; :Returns: 
;      The appended filename.
;      
; :Params:
;    filename: in, required, type=string
;        This file name the text is to be to be appended to. It may be a relative or absolute path
;        name to a file.
;    thetext: in, required, type=string
;        The text to append to the end of the file root name.
;        
; :Examples:
;    Append "processed" to a filename::
;       IDL> thePath = "C:\rsi\idl7.8\lib\jester.img"
;       IDL> outFileName = cgAppendToFilename(thePath, '_processed')
;       IDL> Print, outFileName
;            C:\rsi\idl7.8\lib\jester_processed.img
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
;        Written by: David W. Fanning, 19 September 2012.
;
; :Copyright:
;     Copyright (c)2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgAppendToFilename, filename, theText

   Compile_Opt idl2

   ; Return to caller on error.
   On_Error, 2
   
   IF N_Params() NE 2 THEN BEGIN
        Print, 'Calling syntax: outfile = cgAppendToFilename(filename, textToAppend)'
        RETURN, ""
   ENDIF
   
   ; Parse the filename.
   rootName = cgRootName(filename, DIRECTORY=theDir, EXTENSION=ext)
   
   ; Test to see if this is a relative or absolute filename path.
   testname = Filepath(ROOT_DIR=theDir, rootname + '.' + ext)
   IF StrLen(testname) EQ StrLen(filename) THEN relative = 0 ELSE relative = 1
   
   ; Create the new filename.
   CD, Current=currentDir
   testOutFileName = Filepath(ROOT_DIR=theDir, rootName + theText + '.' + ext)
   
   ; Do the right thing if this is a relative filename or not.
   IF relative THEN BEGIN
       outFileName = File_Basename(testOutFileName) 
   ENDIF ELSE BEGIN
       outFileName = testOutFileName
   ENDELSE

   RETURN, outFileName
     
END