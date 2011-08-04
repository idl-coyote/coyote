;+
; NAME:
;       cgPickfile
;
; PURPOSE:
;
;       This is a utility program for selecting data files. It is a wrapper
;       for DIALOG_PICKFILE, with the additional functionality of being
;       able to "remember" the name and directory of the last file 
;       selected by the program. It basically allows you to start the
;       next file selection from the location of the previous file selection.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Utilites
;
; CALLING SEQUENCE:
;
;       filename = cgPickfile()
;
; RETURN VALUE:
;
;       filename:     The fully-qualified name of the selected file or a
;                     null string if the CANCEL button was selected.
;
;  KEYWORDS:
;
;       DATADIR:      Set this keyword to the name of the data directory.
;                     If not set, the program assumes the data directory is rooted 
;                     in the directory that holds this program file, or the directory 
;                     just above it. If it can't find a "data" directory in either 
;                     of these two locations, the current directory is used as 
;                     the "data" directory. The data directory is ONLY used if
;                     there is no "current" last directory.
;       
;       DEMO:         If set, starts in the !DIR/examples/data directory.
;       
;       JPEG:         If set, starts in the "jpeg" directory. It assumes the jpeg
;                     directory is rooted in the "data" directory.
;
;       HDF:          If set, starts in the "hdf" directory. It assumes the hdf
;                     directory is rooted in the "data" directory.
;       
;       LAST_FILE:    If set, the name of the last file opened is placed in the
;                     filename widget.
;
;       LIDAR:        If set, starts in the "lidar" directory. It assumes the lidar
;                     directory is rooted in the "data" directory.
;
;       NCDF:         If set, starts in the "ncdf" directory. It assumes the ncdf
;                     directory is rooted in the "data" directory.
;
;       PNG:          If set, starts in the "png" directory. It assumes the png
;                     directory is rooted in the "data" directory.
;
;       TIFF:         If set, starts in the "tiff" directory. It assumes the tiff
;                     directory is rooted in the "data" directory.
;       
;       EXTRA:        Accepts any input keywords to DIALOG_PICKFILE (e.g., FILTER).
;
; MODIFICATION HISTORY:
;
;       Adapted from FSC_PICKFILE to be a Coyote Graphics routine by David W. Fanning, 4 Aug 2011.
;
;
;******************************************************************************************;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION cgPickfile, $
    DATADIR=datadir, $
    DEMO=demo, $
    JPEG=jpeg, $
    HDF=hdf, $
    LAST_FILE=last_file, $
    LIDAR=lidar, $
    NCDF=ncdf, $
    PNG=png, $
    TIFF=tiff, $
    _REF_EXTRA=extra

    On_Error, 2 ; Return to caller.

    Compile_Opt idl2

    ; Check keywords to see if anything is set.
    demo =  Keyword_Set(demo)
    jpeg =  Keyword_Set(jpeg)
    hdf =   Keyword_Set(hdf)
    lidar = Keyword_Set(lidar)
    ncdf =  Keyword_Set(ncdf)
    png =   Keyword_Set(png)
    tiff =  Keyword_Set(tiff)
    keywordsAreSet = Total(demo+jpeg+hdf+lidar+ncdf+png+tiff) GT 0
    
    ; Find the "data" subdirectory. If it can't be found, then the
    ; current directory is the data directory.
    IF N_Elements(dataDir) EQ 0 THEN BEGIN
        dataDir = ProgramRootDir() + 'data'
        IF ~File_Test(dataDir, /DIRECTORY) THEN BEGIN
            dataDir = ProgramRootDir(/ONEUP) + 'data'
            IF ~File_Test(dataDir, /DIRECTORY) THEN CD, CURRENT=dataDir
        ENDIF
    ENDIF
    
    ; Be sure the dataDir exists.
    IF ~File_Test(dataDir, /DIRECTORY) THEN BEGIN
        Message, 'Specified data directory cannot be found. Using current directory.', /Informational
        CD, CURRENT=dataDir
    ENDIF
    
    ; If a keyword is set, define the image directory.
    IF keywordsAreSet THEN BEGIN
    
        CASE 1 OF
            jpeg:  imageDir = 'jpeg'
            hdf:   imageDir = 'hdf'
            lidar: imageDir = 'lidar'
            ncdf:  imageDir = 'netCDF'
            png:   imageDir = 'png'
            tiff:  imageDir = 'tiff'
            ELSE:
        ENDCASE
    
        ; If imageDir is defined, use it. Otherwise, use the dataDir.
        IF N_Elements(imageDir) NE 0 THEN BEGIN
            checkDir = dataDir + Path_Sep() + imageDir 
        ENDIF ELSE BEGIN
            checkDir = dataDir
        ENDELSE
        
        ; Did the user chose a demo directory?
        IF Keyword_Set(demo) THEN BEGIN
            checkDir= File_DirName(Filepath(SUBDIRECTORY=['examples','data'], '*'))
        ENDIF
        
        IF ~File_Test(checkDir, /DIRECTORY) THEN fileDir = dataDir ELSE fileDir = checkDir
        
    ENDIF ELSE BEGIN
    
        ; If the lastDir was saved, use it. Otherwise, the dataDir.
        DEFSYSV, '!cgPickfile_LastDir', EXISTS=exists
        IF ~exists THEN fileDir = dataDir ELSE fileDir = !cgPickfile_LastDir
        
    ENDELSE
    
    ; Should we use the last file?
    IF Keyword_Set(last_file) THEN BEGIN
    
        ; If the last file was saved, use it. Otherwise, the nothing to use.
        DEFSYSV, '!cgPickfile_LastFile', EXISTS=exists
        IF exists THEN lastFile =  !cgPickfile_LastFile
    
    ENDIF
    
    ; Has the fileDir been defined. If not, use the dataDir.
    IF N_Elements(fileDir) EQ 0 THEN fileDir = dataDir
    file = Dialog_Pickfile(PATH=fileDir, GET_PATH=selectedDir, $
        FILE=lastFile, _STRICT_EXTRA=extra)
    
    ; Save the last directory and filename. Make sure you are working
    ; with a scalar.
    IF file[0] NE "" THEN BEGIN
        DEFSYSV, '!cgPickfile_LastDir', EXISTS=exists
        IF (exists AND (N_Elements(selectedDir) NE 0)) THEN BEGIN
            !cgPickfile_LastDir = selectedDir 
        ENDIF ELSE BEGIN
            IF N_Elements(selectedDir) NE 0 THEN DEFSYSV, '!cgPickfile_LastDir', selectedDir
        ENDELSE
        DEFSYSV, '!cgPickfile_LastFile', EXISTS=exists
        IF exists THEN BEGIN
            !cgPickfile_LastFile = File_BaseName(file[0])
        ENDIF ELSE BEGIN
            DEFSYSV, '!cgPickfile_LastFile', File_BaseName(file[0])
        ENDELSE
    ENDIF 
    
    RETURN, file
END