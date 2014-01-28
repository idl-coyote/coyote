; docformat = 'rst'
;
; NAME:
;   cgFindPathTo
;
; PURPOSE:
;   The purpose of this function is to search for a file, primarily in "resources" 
;   directories. It might be used, for example, to find the name of bitmap files to
;   use as widget button values.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2014, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to search for a file, primarily in "resources" 
; directories. It might be used, for example, to find the name of bitmap files to
; use as widget button values. This program can be slow, depending upon how many 
; directories it has to search. If you are looking for a *.pro file, it will probably
; be faster to search for the file with File_Which. This routine is primarily used to
; search for files without *.pro file extensions in places outside the normal IDL path.
; Namely in "resources" directories that may reside in the main IDL directory structure
; or may reside in your own application that you have built with IDL. The order of 
; search is as follows::
; 
;    1. In the current directory.
;    2. In the same directory as this file is found in.
;    3. In the directories rooted at the IDL resource directory (IDL_DIR/resource).
;    4. In the directories rooted at a resources directory found in the same directory this file is in.
;    5. In the directories rooted at a resourcs directory found in the same directory this file is in.
;    6. In the directories rooted at a resources directory found in the a directory one above the directory this file is in.
;    7. In the directories rooted at a resource directory found in the a directory one above the directory this file is in.
;    8. Anywhere in the IDL PATH. Note that a *.pro file *must* be in a directory for the directory to be on
;       the IDL PATH.
;
; :Categories:
;    Utilities
;
; :Params:
;    filename: in, required, type=string
;      The root name of the file you are searching for. For example, 'arrow.bmp'.
;      Do not use a full path name to the file.
;
; :Keywords:
;    success: out, optional, type=boolean
;       On output, this keyword contains a 1 if the file was found, and a 0 otherwise.
;       
; :Examples:
;    Here is how to use this program::
;      IDL> Print, cgFindPathTo('gshhs_i.b', success=s) & Print, 'success: ', s
;           /home/fanning/IDL/incubator/gshhs_i.b
;           success:  1
;      IDL> Print, cgFindPathTo('dataviewer_splash.jpg', success=s) & print, 'success: ', s
;           /home/fanning/IDL/dataviewer/resources/dataviewer_splash.jpg
;           success:  1
;      IDL> Print, cgFindPathTo('arc.bmp', success=s) & Print, 'success: ', s
;           /usr/local/rsi/idl70/resource/bitmaps/arc.bmp
;           success:  1
;      IDL> Print, cgFindPathTo('toast_and_jam.txt', success=s) & Print, 'success: ', s
;           success:  0
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
;        Written, 15 January 2014 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2014, Fanning Software Consulting, Inc.
;-
FUNCTION cgFindPathTo, filename, SUCCESS=success

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        success = 0
        RETURN, ""
    ENDIF
    
    ; Assume defeat. :-(
    success = 0
    
    ; Require the name of a file resource.
    IF N_Elements(filename) EQ 0 THEN Message, 'The base name of the resource file is required.'
    filename = File_BaseName(filename)
    
    ; Look in the current directory first.
    CD, Current=thisDir
    resourceName = Filepath(ROOT_DIR=thisDir, filename)
    resourceName = resourceName[0]
    IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
        success = 1
        RETURN, resourceName
    ENDIF    

    ; Look in the same directory this program is found in.
    thisDir = cgSourceDir()
    resourceName = Filepath(ROOT_DIR=thisDir, filename)
    resourceName = resourceName[0]
    IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
        success = 1
        RETURN, resourceName
    ENDIF
   
    ; Look in the IDL resource directory
    resourceDir = Filepath(ROOT_DIR=!Dir, SUBDIRECTORY='resource', "")
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
            RETURN, resourceName
       ENDIF
    ENDIF
    
    ; Look for a file in a resources directory, rooted on this directory.
    thisDir = cgSourceDir()
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resources', "")
    resourceName = File_Search(resourceDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            RETURN, resourceName
        ENDIF
    ENDIF

    ; Look for a file in a resource directory, rooted on this directory.
    thisDir = cgSourceDir()
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resource', "")
    resourceName = File_Search(resourceDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            RETURN, resourceName
        ENDIF
    ENDIF
        
    ; Look for a file in a resources directory, rooted oneup from this directory.
    thisDir = cgSourceDir(/ONEUP)
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resources', "")
    resourceName = File_Search(resourceDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
            RETURN, resourceName
        ENDIF
    ENDIF

    ; Look for a file in a resources directory, rooted oneup from this directory.
    thisDir = cgSourceDir(/ONEUP)
    resourceDir = Filepath(ROOT_DIR=thisDir, SUBDIRECTORY='resource', "")
    resourceName = File_Search(resourceDir, filename, COUNT=count)
    IF count GE 0 THEN BEGIN
        resourceName = resourceName[0]
        IF File_Test(resourceName, /REGULAR, /READ) THEN BEGIN
            success = 1
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
            RETURN, resourceName
        ENDIF
    ENDFOR

    ; If we didn't find anything, return a null string.
    IF success EQ 0 THEN resourceName = ""
    RETURN, resourceName
    
END
