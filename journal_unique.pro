; docformat = 'rst'
;+
; NAME:
;   Journal_Unique
;
; PURPOSE:
; 
;   This procedure creates a new journal file with a unique journal
;   name based on the current time.
;
; CATEGORY:
; 
;    Utility
;  
; CALLING SEQUENCE:
; 
;    Journal_Unique, theDirectory
;
; ARGUMENTS:
; 
;  theDirectory: The directory where you wish to create the journal file.
;                If not provided, the current working directory.
;     
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com
;   
; MODIFICATION HISTORY:
; 
;    Written by: David W. Fanning, 20 September 2010.
;-
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
; :Description:
;    Creates a new journal file with a unique journal name based on the current time.
; 
; :Categories:
;    Utility
;
; :Params:
;    theDirectory: in, optional, type=string
;       The name of the directory containing the new journal file.
;       
; :Examples:
;    Journal_Unique
;
; :Author:
;    David W. Fanning (david@dfanning.com)
;    
; :History:
;     Written, 20 Sept 2010.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;
;
PRO Journal_Unique, theDirectory

   Compile_Opt idl2
   
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        void = Error_Message()
        RETURN
    ENDIF

   ; Need a directory?
   IF N_Elements(theDirectory) EQ 0 THEN CD, CURRENT=theDirectory
   
   ; Compose an unique journal name.
   newName = 'journal_' + StrLowCase(IDL_ValidName(SysTime(), /CONVERT_ALL) + '.pro')
   
   ; Add the directory to it.
   filename = Filepath(ROOT_DIR=theDirectory, newName)
   
   ; Does a system variable with this name exist? If it does, see if
   ; the filename is currently open, if so, close it and open a new one.
   DefSysV, '!fsc_journal_unique_', EXISTS=exists
   IF exists THEN BEGIN
      Help, /FILES, OUTPUT=fileout
      oldname = !fsc_journal_unique_
      positions = StrPos(fileout, oldname)
      index = Where(positions NE -1, count)
      IF count GT 0 THEN Journal
   ENDIF
   
   ; Create the journal file.
   Journal, filename
   
   ; Store the name in the system variable.
   DefSysV, '!fsc_journal_unique_', filename
   
   ; Let the user know where it was created.
   Print, 'New Journal File: ' + filename
   
END