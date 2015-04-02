; docformat = 'rst'
;
; NAME:
;   cgKML2KMZ
;
; PURPOSE:
;   This program simply collects files created from the cgImage2KML program and moves
;   the files to a zip file with the *.kmz file extention.
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
;   This program simply collects files created from the cgImage2KML program and moves
;   the files to a zip file with a *.kmz file extention. The program requires that the
;   file archiver 7-Zip be installed on the user's computer. The 7-Zip program is freeware
;   and is available for Windows, Mac, and UNIX users. Addtional information about the 7-Zip
;   program can be found on the `7-Zip web page <http://www.7-zip.org/>`. The current program
;   runs on Windows computers only, and with 7-Zip installed in the C:\Program Files directory.
;   You may have to modify the path the the 7z.exe executable file to get the program to work
;   correctly for you. The line to modify is clearly marked below.
;   
;   KMZ files are simply zip files containing KML files and support files. You can create your
;   own KMZ files with any archiving software that supports the ZIP compression format.
;
; :Categories:
;    Utility
;    
; :Params:
;    kml_filename: in, optional, type=string
;         The name of a KML file to package as a KMZ file.
;         
;    supportfiles: in, optional, type=string
;         A scalar or vector of files to be included with the KML file to be packaged.
;         If not present, files in the same directory as the KML file and having the
;         same base filename will be used.
;         
; :Keywords:
;     showcmd: in, optional, type=boolean, default=0
;         Set this keyword if you wish to see the command being spawned by the program.
;     silent: in, optional, type=boolean, default=0
;         Set this kewyord to avoid output (except program errors) from being printed.
;       
; :Examples:
;    Here is how to use this program with the Google Earth Image example in the
;    `Coyote Gallery <http://www.idlcoyote.com/gallery/>`. The result is a file
;    named "google_earth_image.kmz" that can be loaded into Google Earth.
;       IDL> Google_Earth_Image
;       IDL> kml2kmz, 'google_earth_image.kml'
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
;        Written, 22 Fabruary 2013 by David W. Fanning.
;        Modified from using the old, unsupported and undocumented IDLKML_SaveKMZ routine to using 
;           the Open Source 7-Zip routine to do the compression. 16 March 2014. DWF.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO cgKML2KMZ, kml_filename, supportFiles, SILENT=silent, SHOWCMD=showcmd

   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = cgErrorMsg()
      RETURN
   ENDIF
   
   ; Modify this path to the 7-Zip executable file to use on your machine.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   path_to_7zip_exe = '"c:\program files\7-zip\7z.exe"'
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   ; Need a KML file name?
   IF N_Elements(kml_filename) EQ 0 THEN BEGIN
      kml_filename = cgPickfile(Filter='*.kml', Title='Select KML File...')
      IF kml_filename EQ "" THEN RETURN
   ENDIF
   
   ; Get the root name of the KML file.
   rootName = cgRootname(kml_filename, Extension=kmlExt, Directory=kmlDir)
   IF StrUpCase(kmlExt) NE 'KML' THEN Message, 'Input file does not appear to be a KML file.'
   
   ; Construct the output filename.
   zipFilename = Filepath(ROOT_Dir=kmlDir, rootName + '.zip')
   kmzFilename = Filepath(ROOT_Dir=kmlDir, rootName + '.kmz')
   
   ; Are there any supporting files?
   IF N_Elements(supportFiles) EQ 0 THEN BEGIN
      CD, kmlDir, Current=thisDir
      supportFiles = File_Search(rootName + '*.png', Count=count)
   ENDIF
   
   ; Append the KML filename to the supportFile, if there are some.
   IF N_Elements(supportFiles) NE 0 THEN supportFiles = [supportFiles]
   
   ; Move the files to the KMZ file.
   cmd = path_to_7zip_exe + " a " +  zipFilename 
   cmd = cmd + ' ' + kml_filename
   FOR j=0,N_Elements(supportFiles)-1 DO BEGIN
       thisFile = FilePath(ROOT_DIR=kmlDir, supportfiles[j])
       cmd = cmd + ' ' + thisFile 
   ENDFOR
   
   ; Print the command if needed.
   IF Keyword_Set(showcmd) && ~Keyword_Set(silent) THEN Print, 'Spawned Command: ', cmd
   
   ; Spawn the 7-zip command to archive the files.
   Spawn, cmd, result, err_result 
   
   ; Rename the zip file to use a kmz extension.
   IF err_result[0] EQ "" THEN File_Move, zipFilename, kmzFilename, /OVERWRITE
   
   ; Print the result message, if any, and not being silent.
   IF ~Keyword_Set(silent) THEN BEGIN
       IF N_Elements(result) GT 1 THEN BEGIN
           FOR k=0,N_Elements(result)-1 DO Print, result[k]
       ENDIF ELSE BEGIN
            Message, 'A SPAWN to 7-Zip failed. Check 7-Zip path to correct.'
       ENDELSE
       
       IF File_Test(kmzFilename, /REGULAR) && ~File_Test(kmzFilename, /ZERO_LENGTH) THEN BEGIN
           Print, ""
           Print, 'Output file located here: ' + kmzFilename
       ENDIF ELSE BEGIN
           Message, 'A KMZ file was not created properly. Check 7-Zip availability and path.'
       ENDELSE
   ENDIF
   
END
   