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
; to be installed on the user's computer, unless you are using a 
; Macintosh computer or an alterntive UNIX command to do the conversion
; for you. If you are on a Macintosh, the supplied pstopdf
; program is used instead. Use the `UNIX_Convert_Cmd` keyword to select
; an alternative UNIX command (e.g., pstopdf or epstopdf).
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Utilities, Graphics
;    
; :Params:
;     ps_file: in, required, type=string
;         The name of the input PostScript file that is being converted to a PDF file.
;         If not provided, the user will be asked to select a file.
;     pdf_file: in, optional, type=string
;         The name of the output PDF file. If not provided, the name is constructed from
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
;         resides with this keyword. (The Windows 32-bit executable is named gswin32c.exe
;         and the 64-bit executable is named gswin64c.exe.)
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
;     unix_convert_cmd: in, optional, type=string
;          There are a number of commands on UNIX machines for converting PostScript files
;          to PDF files. This program assumes you are using Ghostscript to do the conversion
;          for you. The Ghostscript command on most UNIX machines is "gs", which is used if
;          this keyword is undefined. However, if you would prefer to use another program to do
;          the conversion for you, you can specify the name of the command here. For example,
;          "pstopdf" or "epstopdf". In creating the actual command, this command will be
;          separated by a space from the input file name. In other words, if the alternative
;          conversion command was "pstopdf", the actual command would be "pstopdf" + " " + ps_file.
;          Any output filename is ignored. This command does not apply to Macintosh or Windows 
;          computers.
;     version: out, optional, type=string
;         On exit, contains the version of Ghostscipt that was used. Not available on Macs
;         or if an alternative UNIX command was used.
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
   UNIX_CONVERT_CMD=unix_convert_cmd, $
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

   ; Assume failure. Sigh...
   success = 0

   ; Need an input file?
   IF N_Elements(ps_file) EQ 0 THEN BEGIN
      file = cgPickfile(FILTER='*.ps', TITLE='Select a PostScript file...')
      IF file EQ "" THEN RETURN
      ps_file = file
   ENDIF
   
   ; Need to construct the PDF output file name?
   IF N_Elements(pdf_file) EQ 0 THEN BEGIN
      root_name = cgRootName(ps_file, DIRECTORY=thisDir)
      pdf_file = Filepath(ROOT_DIR=thisDir, root_name + ".pdf")
   ENDIF
   
   ; Set default values for keywords.
   SetDefaultValue, pagetype, "LETTER"
   delete_ps = Keyword_Set(delete_ps)
   showcmd = Keyword_Set(showcmd)
   silent = Keyword_Set(silent)
   
   ; Set up a Windows variable for later, so it doesn't cause problems
   ; for me with Macs and UNIX machines.
   exefile = ""
   
   ; Set the name of the Ghostscript executable or alternative conversion command.
   ; Also set the docmdtest variable. If set to 0, the test to see if we can acutally
   ; execute the command will be skipped. The test cannot be reliably executed on
   ; all machines and for all conversion commands.
   CASE StrUpCase(!Version.OS) OF 
   
      'WIN32': BEGIN

          ; Look for default locations, if not told otherwise.
          IF (N_Elements(gs_path) EQ 0) || (gs_path EQ "") THEN gs_path = ['c:\program files\gs\','c:\gs\']
          IF !Version.Arch EQ 'x86_64' THEN exefile = 'gswin64c.exe' ELSE exefile = 'gswin32c.exe'
          file = File_Search(gs_path, exefile, COUNT=count, /FOLD_CASE)
          IF count EQ 0 THEN BEGIN
             IF ~Keyword_Set(silent) THEN Message, 'The Ghostscript executable file GSWIN32C.EXE cannot be found. Exiting without conversion.', /INFORMATIONAL
             RETURN
          ENDIF ELSE gs_exe = '"' + file[count-1] + '"'
          docmdtest = 1
          END
          
      'DARWIN': BEGIN
      	 
          IF (N_Elements(gs_path) EQ 0) || (gs_path EQ "") THEN gs_path = '/usr/bin/'
          file = File_Search(gs_path, 'pstopdf', COUNT=count, /FOLD_CASE)
          IF count EQ 0 THEN BEGIN
             IF ~Keyword_Set(silent) THEN Message, 'The Ghostscript executable file "pstopdf" cannot be found. Exiting without conversion.', /INFORMATIONAL
             RETURN
          ENDIF ELSE gs_exe = file[count-1]
          docmdtest = 0
          END
          
       ELSE: BEGIN ; UNIX flavors
        
           ; Maybe the user provided an alternative command. If not, use the
           ; standard "gs" command.
           IF (N_Elements(unix_convert_cmd) NE 0) && (unix_convert_cmd NE "") THEN BEGIN
               gs_exe = unix_convert_cmd
               docmdtest = 0
           ENDIF ELSE BEGIN
               IF (N_Elements(gs_path) NE 0) && (gs_path NE "") THEN BEGIN
                    file = File_Search(gs_path, 'gs', COUNT=count)
                    IF count GT 0 THEN gs_exe = file[count-1]
               ENDIF ELSE gs_exe = 'gs'
               docmdtest = 1
           ENDELSE
            
          END
   ENDCASE
      
   ; Try the command to get the GhostScript version number. If it doesn't work,
   ; we assume we can't use the command. This test cannot be done in all cases.
   IF docmdtest THEN BEGIN
   
       testcmd = gs_exe + ' -version'
       result = ""
       error_result = ""
       
        ; Spawn the command, note the need for extra quotes here to handle spaces in directory names.
        IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
            Spawn, '"' + testcmd + '"', /HIDE, /LOG_OUTPUT, result, error_result
        ENDIF ELSE BEGIN
            Spawn, testcmd, result, error_result
        ENDELSE
        
        ; If no errors, then you can continue. Otherwise, stop here.
        IF error_result[0] EQ "" THEN BEGIN
           version = result[0]
        ENDIF ELSE BEGIN
           Message, 'Cannot successfully SPAWN a Ghostscript command. No conversion possible.'
        ENDELSE
        
   ENDIF
   
   ; Construct the command that is to be spawned. 
   IF StrUpCase(!Version.OS) EQ 'DARWIN' THEN BEGIN
       cmd = gs_exe + " " + ps_file[0] + " -o " + pdf_file
   ENDIF ELSE BEGIN
   
       IF (N_Elements(unix_convert_cmd) NE 0) && (unix_convert_cmd NE "") THEN BEGIN
           cmd = gs_exe + " " + ps_file 
       ENDIF ELSE BEGIN 
           cmd = gs_exe + ' -sDEVICE=pdfwrite -q -dNOPAUSE -dBATCH'+ $
                ' -sPAPERSIZE=' + StrLowCase(pagetype) + $
                ' -sOutputFile="' + pdf_file + '" "' + ps_file + '"'
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