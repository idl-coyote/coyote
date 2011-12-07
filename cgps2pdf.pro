; docformat = 'rst'
;
; NAME:
;   cgPS2PDF
;
; PURPOSE:
;   Converts a PostScript file to a PDF file.
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
; Converts a PostScript file to a PDF file. This program requires
; the `Ghostscript <http://www.ghostscript.com/download/>` program 
; to be installed on the user's computer.
;
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     ps_file: in, required, type=string
;         The name of the input PostScript file that is being converted to a PDF file.
;         If not provided, the user will be asked to select a file.
;     pdf_file: in, optional, type=string
;         The name of the output PDF file. If not provide, the name is constructed from
;         the input PostScript file name.
;
; :Keywords:
;     delete_ps: in, optional, type=boolean, default=0
;         If this keyword is set, the PostScript file will be deleted after conversion.
;     gs_path: in, optional, type=string
;         This program assumes that UNIX users can access Ghostscript with the "gs"
;         command. It assumes WINDOWS users have installed Ghostscript in either
;         the C:\gs or C:\Program Files\gs directories. If either of these assumptions
;         is incorrect, you can specify the directory where the Ghostscript executable
;         resides with this keyword. (The WINDOWS executable is named gswin32c.exe.)
;     pagetype: in, optional, type=string, default="LETTER"
;         Set this keyword to the "type" of page. Possible values are::
;
;            "Letter" - 8.5 by 11 inches.
;            "Legal" - 8.5 by 14 inches.
;            "Ledger" - 11 by 17 inches.
;            "A4" - 21.0 by 29.7 centimeters.
;            
;     showcmd: in, optional, type=boolean, default=0
;          Set this keyword to print the command that is spawned in the command output window.
;     silent: in, optional, type=boolean, default=0
;          Set this keyword to suppress output messages.
;     success: out, optional, type=boolean
;          Set this keyword to a named variable that on output will contain a 1 to 
;          indicate successful completion of the command, or to 0 otherwise.
;     version: out, optional, type=string
;         On exit, contains the version of Ghostscipt that was used. Not available on Macs.
;          
; :Examples:
;    A typical sequence of commands to create a test.pdf file::
;    
;       PS_Start, Filename='test.ps'
;       cgHistoplot, cgDemoData(7), /Fill
;       PS_End
;       cgPS2PDF, 'test.ps'
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 6 December 2011, from code supplied to me by Paul Krummel. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgPS2PDF, ps_file, pdf_file, $
   DELETE_PS=delete_ps, $
   GS_PATH=gs_path, $
   PAGETYPE=pagetype, $
   SHOWCMD=showcmd, $
   SILENT=silent, $
   SUCCESS=success, $
   VERSION=version

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      IF ~Keyword_Set(silent) THEN void = Error_Message()
      success = 0
      RETURN
   ENDIF

   ; Assume failure. 
   success = 0

   ; Need an input file?
   IF N_Elements(ps_file) EQ 0 THEN BEGIN
      file = cgPickfile(FILTER='*.ps', TITLE='Select a PostScript file...')
      IF file EQ "" THEN RETURN
      ps_file = file
   ENDIF
   
   ; Need to construct the PDF output file name?
   IF N_Elements(pdf_file) EQ 0 THEN BEGIN
      root_name = FSC_Base_Filename(ps_file, DIRECTORY=thisDir)
      pdf_file = Filepath(ROOT_DIR=thisDir, root_name + ".pdf")
   ENDIF
   
   ; Set default values for keywords.
   SetDefaultValue, pagetype, "LETTER"
   
   ; Set up a Windows variable for later.
   exefile = ""
   
   ; Set the name of the Ghostscript executable.
   CASE StrUpCase(!Version.OS) OF 
   
      'WIN32': BEGIN

          ; Look for default locations, if not told otherwise.
          IF N_Elements(gs_path) EQ 0 THEN gs_path = ['c:\program files\gs\','c:\gs\']
          IF !Version.Arch EQ 'x86_64' THEN exefile = 'gswin64c.exe' ELSE exefile = 'gswin32c.exe'
          file = File_Search(gs_path, exefile, COUNT=count, /FOLD_CASE)
          IF count EQ 0 THEN BEGIN
             IF ~Keyword_Set(silent) THEN Message, 'The Ghostscript executable file GSWIN32C.EXE cannot be found. Exiting without conversion.', /INFORMATIONAL
             RETURN
          ENDIF ELSE gs_exe = '"' + file[count-1] + '"'
     
          END
          
      'DARWIN': BEGIN
      	 
          IF N_Elements(gs_path) EQ 0 THEN gs_path = '/usr/bin/'
          file = File_Search(gs_path, 'pstopdf', COUNT=count, /FOLD_CASE)
          IF count EQ 0 THEN BEGIN
             IF ~Keyword_Set(silent) THEN Message, 'The Ghostscript executable file "pstopdf" cannot be found. Exiting without conversion.', /INFORMATIONAL
             RETURN
          ENDIF ELSE gs_exe = file[count-1]
     
          END
          
       ELSE: BEGIN
        
           IF N_Elements(gs_path) NE 0 THEN BEGIN
                file = File_Search(gs_path, 'gs', COUNT=count)
                IF count GT 0 THEN gs_exe = file[count-1]
            ENDIF ELSE gs_exe = 'gs'
            
          END
   ENDCASE
      
   ; Try the command to get the GhostScript version number. If it doesn't work,
   ; we assume we can't use the command.
   testcmd = gs_exe + ' -version'
   
    ; Spawn the command, note the need for extra quotes here to handle spaces in directory names.
    IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
        Spawn, '"' + testcmd + '"', /HIDE, /LOG_OUTPUT, result, error_result
    ENDIF ELSE BEGIN
        IF StrUpCase(!Version.OS) NE 'DARWIN' THEN BEGIN
            Spawn, testcmd, result, error_result
        ENDIF ELSE BEGIN
           result = ""
           error_result = ""
        ENDELSE
    ENDELSE
    
    ; If no errors, then you can continue. Otherwise, stop here.
    IF error_result[0] EQ "" THEN BEGIN
       version = result[0]
    ENDIF ELSE BEGIN
       Message, 'Cannot successfully SPAWN a Ghostscript command. No conversion possible.'
    ENDELSE
   
   ; Construct the command that is to be spawned. Can't set antialiasing in 64-bit Windows versions.
   IF exefile EQ 'gswin64c.exe' THEN BEGIN
       cmd = gs_exe + ' -sDEVICE=pdfwrite -q -dNOPAUSE -dBATCH'+ $
            ' -sPAPERSIZE='+StrLowCase(pagetype)+' -sOutputFile="'+pdf_file+'" "'+ps_file+'"'
   ENDIF ELSE BEGIN
        IF StrUpCase(!Version.OS) EQ 'DARWIN' THEN BEGIN
            cmd = gs_exe + " " + ps_file[0] + " -o " + pdf_file
         ENDIF ELSE BEGIN
            cmd = gs_exe + ' -sDEVICE=pdfwrite -q -dNOPAUSE -dBATCH'+ $
                ' -sPAPERSIZE='+StrLowCase(pagetype)+' -sOutputFile="'+pdf_file+'" "'+ps_file+'"'
            ENDELSE
    ENDELSE
    
   ; Spawn the command, note the need for extra quotes here to handle spaces in directory names.
    IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
        Spawn, '"' + cmd + '"', /HIDE, result, error_result
    ENDIF ELSE BEGIN
        Spawn,  cmd,  result, error_result
    ENDELSE
    
    ; If you generated an error, report it. Errors in the SPAWNed command 
    ; don't seem to cause errors on Windows machines. :-(
    IF error_result[0] NE "" THEN Message, error_result[0]
    
    ; If you get here, hurrah!
    IF Keyword_Set(showcmd) THEN Print, cmd
    IF ~Keyword_Set(silent) THEN Print, 'Output PDF file: ' + pdf_file
    success = 1
   
   ; Delete the PostScript file?
   IF Keyword_Set(delete_ps) THEN File_Delete, ps_file
END