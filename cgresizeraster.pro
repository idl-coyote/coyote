; docformat = 'rst'
;
; NAME:
;   cgResizeRaster
;
; PURPOSE:
;   The purpose of this program is to resize a raster file according to 
;   instructions from the user.
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
; The purpose of this program is to resize a raster file according to 
; instructions from the user. It was designed specifically to resize the
; extremely large raster files created by IDL's function graphics commands
; from within an IDL program. It requires ImageMagick be installed and able
; to be called from a SPAWN command from within IDL.
;
; :Categories:
;    Graphics
;
; :Params:
;    rasterfile: in, required, type=string
;         Set this parameter to the name of the raster file to be resized. The resized
;         file will have this same name unless the `KEEP_ORIGINAL` keyword is set, in
;         which case an "_rs" will be appended to the base name given by this parameter.
;    percentage: in, optional, type=float
;         A resize percentage. A value of 50, for example, will make the output file
;         half its original size. A file may be made bigger by specifying a percentage
;         greater than 100 (e.g. 150). The default is to calculate a precentage that makes
;         the longest dimension of the output file approximately 600 pixels.
;
; :Keywords:
;    keep_original: in, optional, type=boolean
;         Set this keyword to keep the original file. Otherwise, the file is overwritten with
;         the resized file. If this keyword is set, the output file will use the same name as
;         the input file, except an "_rs" will be appended to the base name of the input file.
;    height: in, optional, type=integer
;         Set this keyword to set the height of the resulting raster file in pixel units. The width of the
;         raster will be such as to preserve the aspect ratio of the starting image. This keyword
;         cannot be used if the `WIDTH` keyword is used at the same time.
;    silent: in, optional, type=boolean, default=0
;         Set this keyword to suppress printed output from the program. 
;    width: in, optional, type=integer
;         Set this keyword to set the resulting width of the raster file. The height of the
;         raster will be such as to preserve the aspect ratio of the starting image. This keyword
;         cannot be used if the `HEIGHT` keyword is used at the same time.
;
; :Examples:
;    Here is how to use this program::
;       IDL> cgResizeRaster, 'picture.png', /Keep_Original, Width=500
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
PRO cgResizeRaster, rasterfile, percentage, $
   KEEP_ORIGINAL=keep_original, $
   HEIGHT=height, $
   SILENT=silent, $
   WIDTH=width

   Compile_Opt idl2
   
   ; Standard error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = cgErrorMsg()
       RETURN
   ENDIF
   
   IF (N_Elements(rasterfile) EQ 0) || (Size(rasterfile, /TNAME) NE 'STRING') THEN BEGIN
       rasterfile = cgPickfile(Title='Select a raster file for resizing...')
       IF rasterfile EQ "" THEN RETURN
   ENDIF
   
   ; If you don't have ImageMagick installed, there is no point in going on.
   IF ~cgHasImageMagick() THEN Message, 'ImageMagick cannot be found on this machine and is required.'
   
   ; Is this a valid type of file?
   validTypes = ['gif', 'jpeg', 'jpg', 'png', 'tiff', 'tif', 'bmp', 'ppm', 'jpf', 'jpx']
   rootname = cgRootname(rasterfile, EXTENSION=extension, DIRECTORY=directory)
   IF Keyword_Set(keep_original) THEN BEGIN
       outputFileName = Filepath(ROOT_DIR=directory, rootname + '_rs.' + extension)
   ENDIF ELSE BEGIN
       outputFileName = rasterfile
   ENDELSE
   index = Where(validTypes EQ StrLowCase(extension), count)
   IF count EQ 0 THEN Message, 'Cannot resize an image file with this extension: ' + extension + '.'
   
   ; The default percentage is choosen to create an image whose largest dimension is approximately 
   ; 600 pixels. In any case, the percentage must be greater than 0
   IF N_Elements(percentage) EQ 0 THEN BEGIN
       void = Query_Image(rasterfile, DIMENSIONS=dims)
       maxsize = Max(dims)
       percentage = 100.0 * (600.0 / maxsize)
   ENDIF ELSE percentage = 0 > percentage
   
   ; You can resize by HEIGHT or by WIDTH, but not by both. 
   IF (N_Elements(width) NE 0) && (N_Elements(height) NE 0) THEN BEGIN
       Message, 'HEIGHT and WIDTH keywords cannot be used together. Using WIDTH.', /Informational
       Undefine, height
   ENDIF
   
   ; If you use either of the HEIGHT or WIDTH keywords, the resize percentage is ignored.
   IF (N_Elements(width) NE 0) || (N_Elements(height) NE 0) THEN percentage = 0
   
   ; Need to resize to a specific width or height?
   IF (N_Elements(width) NE 0) || (N_Elements(height) NE 0) THEN BEGIN
       void = Query_Image(rasterfile, DIMENSIONS=dims)
       IF dims[0] GT dims[1] THEN landscape = 1 ELSE landscape = 0
       CASE 1 OF
           (N_Elements(width) NE 0) && (N_Elements(height) EQ 0): BEGIN
               resize_cmd = ' -resize ' + StrCompress(Fix(width), /REMOVE_ALL)
           END
           (N_Elements(width) EQ 0) && (N_Elements(height) NE 0): BEGIN
               resize_cmd = ' -resize x' + StrCompress(height, /REMOVE_ALL)
           END
       ENDCASE
   ENDIF
   
   ; We will do the normal resize, unless this has already been done. Two checks here.
   IF (percentage NE 0) && (N_Elements(resize_cmd) EQ 0) THEN BEGIN
       resize_cmd =  ' -resize '+ StrCompress(percentage, /REMOVE_ALL) + '%'
   ENDIF
    
   ; Construct the proper ImageMagick command.
   cmd = 'convert ' + rasterfile + resize_cmd + ' ' + outputFileName
   
   ; Execute the ImageMagick command.
   Spawn, cmd, result, err_result 

   ; Either print the error, or a message locating the file.
   IF ~Keyword_Set(silent) THEN BEGIN
       IF err_result[0] NE "" THEN BEGIN
           FOR k=0,N_Elements(err_result)-1 DO Print, err_result[k]
       ENDIF ELSE BEGIN
           Print, 'Output file located here: ' + outputFileName
       ENDELSE
   ENDIF
   
   END