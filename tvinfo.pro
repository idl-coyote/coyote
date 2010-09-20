;+
; NAME:
;     TVINFO
;
; PURPOSE:
;
;     The purpose of this program is allow interactive inquiry of image
;     position and values for images displayed with either TVIMAGE or TVSCALE.
;     After a call to TVIMAGE or TVSCALE, TVINFO can be called and the user
;     can use the cursor to click in the image display window. If the user clicks
;     inside the image, the image location and value will be printed out in the
;     user's IDL console window.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;     Graphics display.
;
; CALLING SEQUENCE:
;
;     TVINFO, image
;
; INPUTS:
;
;     image:    A 2D or 3D image array. Values will be returned from this image. (Required)
;
;     position: A four-element array giving image position in the window in normalized
;               coordinates. If not provided, uses the stored position from TVIMAGE or TVSCALE.
;
; KEYWORD PARAMETERS:
;
;     None.
;
; OUTPUTS:
;
;     Image locations and values are printed in the IDL console window.
;
; SIDE EFFECTS:
;
;     TVINFO blocks the IDL command line until the RIGHT mouse button is clicked
;     in the image display window.
;
; RESTRICTIONS:
;
;     TVINFO *only* works for the last image displayed with TVIMAGE or TVSCALE
;     on a Windows or X device. Precautions are taken to help you avoid shooting
;     yourself in the foot, but I can't anticipate every foolish action at the
;     IDL command line. Expecially if you close the image window before exiting
;     the program with the RIGHT mouse button, you are COMPLETELY on your own!
;
;     Coyote Library programs are required to use TVINFO. Among the ones I know
;     about are these:
;
;      TVIMAGE, TVSCALE, SCALE_VECTOR, IMAGE_DIMENSIONS, ERROR_MESSAGE, CONVERT_TO_TYPE, FPUFIX.
;
;      If the program doesn't compile, you probably need library routines. They
;      can be found here:
;
;           http:/www.dfanning.com/documents/programs.html
;
; EXAMPLE:
;
;     To display an image with axes and then inquire about it:
;
;        filename = FILEPATH(SUBDIR=['examples','data'], 'worldelv.dat')
;        image = BYTARR(360,360)
;        OPENR, lun, filename, /GET_LUN
;        READU, lun, image
;        FREE_LUN, lun
;
;        thisPosition = [0.1, 0.1, 0.9, 0.9]
;        TVIMAGE, image, /KEEP_ASPECT,  POSITION=thisPosition, /AXES
;        TVINFO, image
;
; MODIFICATION HISTORY:
;      Written by: David W Fanning, 16 March 2008.
;      Added ability to specify position directly in call. 20 March 2008. DWF
;      Changed cursor operation to conform with expected differences
;         between Windows and UNIX. 20 March 2008, DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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
PRO TVINFO, image, position

    COMPILE_OPT idl2

    ; Error Handling.
    CATCH, theError
    IF theError NE 0 THEN BEGIN
        CATCH, /CANCEL
        void = Error_Message()
        
        ; Set the cursor back to original shape.
        IF !D.Name EQ 'WIN' THEN BEGIN
            Device, /CURSOR_ORIGINAL
        ENDIF ELSE BEGIN
            Device, CURSOR_STANDARD=30
        ENDELSE
        RETURN
     ENDIF

    ; Set up a common block to access TVIMAGE informaton.
    COMMON FSC_$TVIMAGE, _tvimage_xsize, _tvimage_ysize, $
                         _tvimage_winxsize, _tvimage_winysize, $
                         _tvimage_position, _tvimage_winID, $
                         _tvimage_current

    ; Check to see if we can proceed. First see if we have an image.
    IF N_Elements(image) EQ 0 THEN Message, 'Must supply image for reporting values.'

    ; Only 2D and 24-bit images allowed.
    ndims = Size(image, /N_DIMENSIONS)
    IF ndims LT 2 OR ndims GT 3 THEN Message, 'Image must be 2D or 3D (True-Color).'
    s = Size(image, /Dimensions)

    ; Does this device support windows?
    IF (!D.FLAGS AND 256) EQ 0 THEN Message, 'TVINFO only works on devices that support windows.'

    ; Has a call to TVIMAGE or TVSCALE preceeded this call?
    IF _tvimage_current EQ 0 THEN Message, 'Must call TVIMAGE or TVSCALE prior to calling TVINFO.'

    ; Is the image window still open?
    Device, WINDOW_STATE=theWindows
    IF theWindows[_tvimage_winID] EQ 0 THEN Message, 'The image window has been closed.'

    ; Is the image that you got here the same size as the TVIMAGE image?
    void = Image_Dimensions(image, XSIZE=imgxsize, YSIZE=imgysize)
    IF imgxsize NE _tvimage_xsize OR imgysize NE _tvimage_ysize THEN $
        Message, 'Image dimensions to not match those of last displayed image.'

    ; Make the image window the current graphics window.
    thisWindow = !D.Window
    WSet, _tvimage_winID

    ; Are the window sizes still right?
    IF !D.X_Size NE _tvimage_winxsize OR !D.Y_Size NE _tvimage_winysize THEN BEGIN
        WSet, thisWindow
        Message, 'The image window size has changed size from when image was last displayed.'
    ENDIF

    ; If a position was supplied, use that rather than the stored position.
    IF N_Elements(position) NE 0 THEN thePos = position ELSE thePos = _tvimage_position

    ; Print instructions to the user.
    Print, ''
    Print, 'Move cursor in image window (window ' + StrTrim(_tvimage_winID,2) + ') to get image values.'
    Print, 'Use LEFT mouse to inquire.'
    Print, 'Click the RIGHT mouse button to exit program.'
    Print, ''
    Print, 'Expecting cursor clicks in image window...'

    ; Start the image loop.
    !MOUSE.BUTTON = 0
    WHILE !MOUSE.BUTTON NE 4 DO BEGIN

       ; Change to arrow cursor in window.
       IF !D.Name EQ 'WIN' THEN BEGIN
           Device, CURSOR_STANDARD=32649
       ENDIF ELSE BEGIN
           Device, /CURSOR_ORIGINAL
       ENDELSE

       ; Get the location in the window.
       Cursor, x, y, /NORMAL, /DOWN

       ; Are you inside or outside the image?
       inside = 0
       IF (x GE thePos[0]) AND (x LE thePos[2]) AND $
          (y GE thePos[1]) AND (y LE thePos[3]) THEN inside = 1

       ; If inside, then convert location to image pixel dimension and return value
       ; of the image at the location. Be sure you do this differently for 2D and 3D images.
       IF inside THEN BEGIN

          ; Create vectors for locating image dimensions with VALUE_LOCATE
          xvec = Scale_Vector(Findgen(_tvimage_xsize+1), thePos[0], thePos[2])
          yvec = Scale_Vector(Findgen(_tvimage_ysize+1), thePos[1], thePos[3])
          xpixel = Value_Locate(xvec, x)
          ypixel = Value_Locate(yvec, y)

          ; Output depends in whether this is 2D or 3D image.
          dims = Size(image, /Dimensions)
          trueIndex = Where(dims EQ 3)

          ; I guess a legit dimension could be three, too, but I
          ; don't know what to do in that case.
          IF N_Elements(trueIndex) GT 1 THEN $
             Message, 'Dude. This is one strange image! Returning.'

          IF trueIndex[0] EQ -1 THEN BEGIN

             value = image[xpixel, ypixel]
             IF Size(value, /TNAME) EQ 'BYTE' THEN value = Fix(value)
             Print, 'Value at (' + StrTrim(xpixel,2) + ',' + StrTrim(ypixel,2) + ') is ', StrTrim(value,2)

          ENDIF ELSE BEGIN

            ; 3D image processing here.
            CASE trueindex[0] OF
                0: rgb = image[*, xpixel, ypixel]
                1: rgb = image[xpixel, *, ypixel]
                2: rgb = image[xpixel, ypixel, *]
            ENDCASE
            IF Size(rgb, /TNAME) EQ 'BYTE' THEN rgb = Fix(rgb)
            value = '[' + StrTrim(rgb[0],2) + ', ' + StrTrim(rgb[1],2) + ', ' + StrTrim(rgb[2],2) + ']'
            Print, 'Value at (' + StrTrim(xpixel,2) + ',' + StrTrim(ypixel,2) + ') is ' + value
          ENDELSE
       ENDIF ELSE Print, 'Outside image'

    ENDWHILE

    ; Set the cursor back to original shape.
    IF !D.Name EQ 'WIN' THEN BEGIN
        Device, /CURSOR_ORIGINAL
    ENDIF ELSE BEGIN
        Device, CURSOR_STANDARD=30
    ENDELSE

    ; Set the window back to entry window.
    WSet, thisWindow

    ; Print confirmation.
    Print, ''
    Print, 'Good-bye. TVINFO has returned control to IDL.'
    Print, ''

END