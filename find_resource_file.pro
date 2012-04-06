;+
; NAME:
;  FIND_RESOURCE_FILE
;
; PURPOSE:
;
;  This function is designed to search for and return the fully qualified
;  path to a resource file. The order of search is as follows:
;
;    1. In the same directory as this file is found in.
;    2. In the directories rooted at the IDL resource directory (IDL_DIR/resource).
;    3. In the directories rooted at a resource directory found in the same directory this file is in.
;    4. In the directories rooted at a resource directory found in the a directory one above the directory this file is in.
;    5. Anywhere in the IDL PATH. Note that a *.pro file *must* be in a directory for the directory to be on
;       the IDL PATH.
; 
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
;  Utilities
;
; CALLING SEQUENCE:
;
;  fullPath = Find_Resource_File(resourceFilename)
;
; INPUTS:
;
;  resourceFilename:   The root name of a resource file. For example, 'arc.bmp'.
;
; KEYWORD PARAMETERS:
;
;  SUCCESS:            An output keyword whose value is set to 1 if the program
;                      successfully finds the resource file. Otherwise, set to 0.
;                      
;  VERBOSE:            Set this keyword to enable output that prints the full path 
;                      to the resource file, or tells you that the file cannot be found.
;
; RETURN_VALUE:
;
;  fullpath:           A fully-qualified file path to the resource file.
;
; EXAMPLE:
;
;  IDL> print, find_resource_file('gshhs_i.b', success=s) & print, 'success: ', s
;       /home/fanning/IDL/incubator/gshhs_i.b
;       success:            1
;  IDL> print, find_resource_file('dataviewer_splash.jpg', success=s) & print, 'success: ', s
;       /home/fanning/IDL/dataviewer/resources/dataviewer_splash.jpg
;       success:            1
;  IDL> print, find_resource_file('arc.bmp', success=s) & print, 'success: ', s
;       /usr/local/rsi/idl70/resource/bitmaps/arc.bmp
;       success:            1
;  IDL> print, find_resource_file('toast_and_jam.txt', success=s) & print, 'success: ', s
;
;      success:            0
; MODIFICATION HISTORY:
;
;  Written by: David W Fanning, December 12, 2008.
;  It seems all my resource files are in "resources" directories, not  "resource" directories.
;     So now the program looks in both places. 6 January 2009.
;  The file did not seem to be looking in the IDL path. It does now. 21 April 2010. DWF.
;  The file will also look in the directory of the caller of this program. 21 April 2010. DWF.
;  Fixed a problem when File_Search finds several files with the same name. Always takes the
;      first file found now. 4 June 2010. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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
Function Find_Resource_File, filename, SUCCESS=success, VERBOSE=verbose

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        success = 0
        RETURN, ""
    ENDIF
    
    ; Default values.
    success = 0
    verbose = Keyword_Set(verbose)
    
    ; Require the name of a file resource.
    IF N_Elements(filename) EQ 0 THEN Message, 'The base name of the resource file is required.'
    filename = File_BaseName(filename)
    
    ; Look in the same directory this program is found in.
    thisDir = ProgramRootDir()
    resourceName = Filepath(ROOT_DIR=thisDir, filename)
    resourceName = resourceName[0]
    IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
        success = 1
        IF verbose THEN Print, 'Resource File Found: ', resourceName
        RETURN, resourceName
    ENDIF

    ; Look in the IDL resource directory
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resource', "")
    resourceName = File_Search(resourceDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            RETURN, resourceName
        ENDIF
    ENDIF
    
    ; Look in the directory where the program that *called* this program lives.
    HELP, CALLS=calls
    callingRoutine = (StrSplit(calls[1], /EXTRACT))[0]
    IF callingRoutine NE '$MAIN$' THEN BEGIN
       longPath = File_Which(StrLowCase(callingRoutine) + '.pro')
       thisDir = File_Dirname(longpath)
       resourceName = File_Search(thisDir, filename, COUNT=count)
        resourceName = resourceName[0]
       IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
       ENDIF
    ENDIF
       
    ; Look for a file in a resource directory, rooted on this directory.
    thisDir = ProgramRootDir()
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resource', "")
    resourceName = File_Search(thisDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
        ENDIF
    ENDIF

    ; Look for a file in a resources directory, rooted on this directory.
    thisDir = ProgramRootDir()
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resources', "")
    resourceName = File_Search(thisDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
        ENDIF
    ENDIF
    
    ; Look for a file in a resource directory, rooted oneup from this directory.
    thisDir = ProgramRootDir(/ONEUP)
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resource', "")
    resourceName = File_Search(thisDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
        ENDIF
    ENDIF
    
    ; Look for a file in a resource directory, rooted oneup from this directory.
    thisDir = ProgramRootDir(/ONEUP)
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resources', "")
    resourceName = File_Search(thisDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
        ENDIF
    ENDIF

    ; Look for the file in the IDL PATH.
    directories = Expand_Path(!PATH, /ARRAY)
    FOR j=0L, N_Elements(directories)-1 DO BEGIN
        resourceName = File_Which(directories[j], filename, /INCLUDE_CURRENT_DIR)
        resourceName = resourceName[0]
        IF resourceName NE "" THEN BEGIN
            success = 1
            IF verbose THEN Print, 'Resource File Found: ', resourceName
            RETURN, resourceName
        ENDIF
    ENDFOR

    IF success EQ 0 THEN resourceName = ""
    IF verbose THEN Print, 'Resource File Has NOT Been Found: ', filename
    
    RETURN, resourceName
END
