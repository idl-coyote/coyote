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
;   the files to a zip file with a *.kmz file extention. The method used is an
;   undocumented and unsupported method for creating KMZ files in IDL 8. It should not
;   be relied upon, and it will not work in earlier versions of IDL. The cross-platform 
;   zip file functionality is scheduled to be exposed to IDL users in an IDL release 
;   sometime in 2013, but it is not known when or if this will occur.
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
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO cgKML2KMZ, kml_filename, supportFiles

   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Only in IDL 8 or above.
   IF Float(!Version.Release) LT 8.0 THEN BEGIN
      Message, 'The kml2kmz program is only supported in IDL 8 or higher.'
   ENDIF
   
   ; Initialize the object.
   void = {IDLitWriteKML}
   
   ; Need a KML file name?
   IF N_Elements(kml_filename) EQ 0 THEN BEGIN
      kml_filename = cgPickfile(Filter='*.kml', Title='Select KML File...')
      IF kml_filename EQ "" THEN RETURN
   ENDIF
   
   ; Get the root name of the KML file.
   rootName = cgRootname(kml_filename, Extension=kmlExt, Directory=kmlDir)
   IF StrUpCase(kmlExt) NE 'KML' THEN Message, 'Input file does not appear to be a KML file.'
   
   ; Construct the output filename.
   kmzFilename = Filepath(ROOT_Dir=kmlDir, rootName + '.kmz')
   
   ; Are there any supporting files?
   IF N_Elements(supportFiles) EQ 0 THEN BEGIN
      CD, kmlDir, Current=thisDir
      supportFiles = File_Search(rootName + '*.png', Count=count)
   ENDIF
   
   ; Append the KML filename to the supportFile, if there are some.
   IF N_Elements(supportFiles) NE 0 THEN supportFiles = [kml_filename, supportFiles]
   
   ; Move the files to the KMZ file.
   void = IDLKML_SaveKMZ(kmzFilename, supportFiles)
   
END
   