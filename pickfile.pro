;+
; NAME:
;       Pickfile
;
; PURPOSE:
;
;       This is a utility program for selecting data files. It is a wrapper
;       for DIALOG_PICKFILE, with special keywords to go to the specified
;       data directories in my IDL program layout. The advantage of using
;       Pickfile is that it remembers the directory in which you selected the 
;       last data file and returns there for the next file selection. The 
;       last directory selection made is stored in the system variable 
;       !Coyote_LastDir.
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
;       filename = Pickfile()
;
; RETURN VALUE:
;
;       filename:     The fully-qualified name of the selected file or a
;                     null string if the CANCEL button was selected.
;
;  KEYWORDS:
;
;       DATA:         If set, starts in the "data" directory.
;       
;       DEMO:         If set, starts in the !DIR/examples/data directory.
;       
;       JPEG:         If set, starts in the "jpeg" directory.
;
;       HDF:          If set, starts in the "hdf" directory.
;
;       LIDAR:        If set, starts in the "lidar" directory.
;
;       NCDF:         If set, starts in the "netCDF" directory.
;
;       PNG:          If set, starts in the "png" directory.
;
;       TIFF:         If set, starts in the "tiff" directory.
;       
;       EXTRA:        Accepts any input keywords to DIALOG_PICKFILE (e.g., FILTER).
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 22 February 2010.
;
;
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION Pickfile, $
    DATA=data, $
    DEMO=demo, $
    JPEG=jpeg, $
    HDF=hdf, $
    LIDAR=lidar, $
    NCDF=ncdf, $
    PNG=png, $
    TIFF=tiff, $
    _REF_EXTRA=extra

    On_Error, 2 ; Return to caller.

    Compile_Opt idl2

    ; Check keywords to see if anything is set.
    data =  Keyword_Set(data)
    demo =  Keyword_Set(demo)
    jpeg =  Keyword_Set(jpeg)
    hdf =   Keyword_Set(hdf)
    lidar = Keyword_Set(lidar)
    ncdf =  Keyword_Set(ncdf)
    png =   Keyword_Set(png)
    tiff =  Keyword_Set(tiff)
    keywordsAreSet = Total(data+demo+jpeg+hdf+lidar+ncdf+png+tiff) GT 0
    
    ; Find the "data" subdirectory.
    dataDir = ProgramRootDir(/ONEUP) + 'data'
    IF ~File_Test(dataDir, /DIRECTORY) THEN dataDir = ProgramRootDir(/ONEUP)
    
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
        DEFSYSV, '!Coyote_LastDir', EXISTS=exists
        IF ~exists THEN fileDir = dataDir ELSE fileDir = !Coyote_LastDir
        
    ENDELSE
    
    ; Has the fileDir been defined. If not, use the dataDir.
    IF N_Elements(fileDir) EQ 0 THEN fileDir = dataDir
    file = Dialog_Pickfile(PATH=fileDir, GET_PATH=selectedDir, _STRICT_EXTRA=extra)
    
    ; Save the last directory.
    IF file NE "" THEN BEGIN
        DEFSYSV, '!Coyote_LastDir', EXISTS=exists
        IF (exists AND (N_Elements(selectedDir) NE 0)) THEN BEGIN
            !Coyote_LastDir = selectedDir 
        ENDIF ELSE BEGIN
            IF N_Elements(selectedDir) NE 0 THEN DEFSYSV, '!Coyote_LastDir', selectedDir
        ENDELSE
    ENDIF 
    
    RETURN, file
END