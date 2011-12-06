; docformat = 'rst'
;
; NAME:
;   cgImageInfo
;
; PURPOSE:
;     The purpose of this program is allow interactive inquiry of image
;     position and values for images displayed with cgImage.
;     After a call to cgImage, cgImageInfo can be called and the user
;     can use the cursor to click in the image display window. If the user clicks
;     inside the image, the image location and value will be printed out in the
;     user's IDL console window.
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
;  The purpose of this program is allow interactive inquiry of image
;  position and values for images displayed with cgImage.
;  After a call to cgImage, cgImageInfo can be called and the user
;  can use the cursor to click in the image display window. If the user clicks
;  inside the image, the image location and value will be printed out in the
;  user's IDL console window. The cgImageInfo program blocks the IDL command 
;  line until the RIGHT mouse button is clicked in the image display window.
;  
;  This cgImageInfo program is designed to work for the last image displayed with cgImage 
;  on a Windows or X device. Precautions are taken to help you avoid shooting
;  yourself in the foot, but I can't anticipate every action a user might take at the 
;  IDL command line. Pay particular attention to exiting the program with the RIGHT mouse
;  button before you close or kill the current graphics window. Failure to do so will 
;  put you into a very strange and precarious state from which no rescue is possible.
;  You might want to consider using the Catalyst Program `IMGWIN <http://www.idlcoyote.com/catalyst/imgwin.html>`
;   as an alternative to cgImageInfo.
;
; :Categories:
;    Graphics
;    
; :Params:
;    image: in, required, 
;         A 2D or 3D image array. Values will be returned from this image.
;         In versions of IDL < 8.0, it is possible to use a HASH object of
;         keys/images where this program will describe the value for each of
;         the images in the HASH object.
;    position: in, optional, type=float
;         A four-element floating array giving the position of the image in
;         the display window. If not provided, the image position will be
;         retrieved from the last image position used with cgImage.
;         
; :Examples:
;     To display an image with axes and then inquire about it::
;
;         filename = FILEPATH(SUBDIR=['examples','data'], 'worldelv.dat')
;         image = BYTARR(360,360)
;         OPENR, lun, filename, /GET_LUN
;         READU, lun, image
;         FREE_LUN, lun
;
;         thisPosition = [0.1, 0.1, 0.9, 0.9]
;         cgImage, image, /KEEP_ASPECT,  POSITION=thisPosition, /AXES
;         cgImageInfo, image
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
;      Written by: David W Fanning, 16 March 2008.
;      Added ability to specify position directly in call. 20 March 2008. DWF
;      Changed cursor operation to conform with expected differences
;         between Windows and UNIX. 20 March 2008, DWF.
;      Slightly modified screen directions. 16 November 2010. DWF.
;      Modified so that multiple images/grids can be described 18 May 2011. MHS
;
; :Copyright:
;     Copyright (c) 2008-2011, Fanning Software Consulting, Inc.
;-
PRO cgImageInfo, image, position

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

    ; Set up a common block to access cgImage informaton.
    COMMON FSC_$CGIMAGE, _cgimage_xsize, _cgimage_ysize, $
                         _cgimage_winxsize, _cgimage_winysize, $
                         _cgimage_position, _cgimage_winID, $
                         _cgimage_current



    ;;  First check to see if we're using a HASH as input for the variable image.
    hsh = ''
    IF total( obj_valid( image ) EQ 1 ) &&  obj_isa( image, 'HASH' ) THEN BEGIN 
       ;; If so, we need to pull out the first image from the hash so that all of the
       ;; information can be set up properly before the user selects data from the
       ;; displayed image
       hsh = image

       IF n_elements( hsh ) EQ 0 THEN Message, 'Must supply a hash of names/images to examine.'
       ;; 
       keys = hsh -> keys( ) 
       image = hsh[ keys[ 0 ] ]

       ;; check that each image in the hash has same dimensions.
       void =  Image_Dimensions( image, xsize = first_xsize, ysize = first_ysize )
       FOR k = 1, n_elements( keys ) - 1 DO BEGIN
          img =  hsh[ keys[ k ] ]
          void =  Image_Dimensions( img,  xsize = xsize,  ysize = ysize )
          IF xsize NE first_xsize OR ysize NE first_ysize THEN $
             Message, 'Each Image must have same X & Y dimensions'
       ENDFOR 
       
    ENDIF 

    ; Check to see if we can proceed. First see if we have an image.
    IF N_Elements(image) EQ 0 THEN Message, 'Must supply image for reporting values.'

    ; Only 2D and 24-bit images allowed.
    ndims = Size(image, /N_DIMENSIONS)
    IF ndims LT 2 OR ndims GT 3 THEN Message, 'Image must be 2D or 3D (True-Color).'
    s = Size(image, /Dimensions)

    ; Does this device support windows?
    IF (!D.FLAGS AND 256) EQ 0 THEN Message, 'cgImageInfo only works on devices that support windows.'

    ; Has a call to cgImage preceeded this call?
    IF _cgimage_current EQ 0 THEN Message, 'Must call cgImage prior to calling cgImageInfo.'

    ; Is the image window still open?
    Device, WINDOW_STATE=theWindows
    IF theWindows[_cgimage_winID] EQ 0 THEN Message, 'The image window has been closed.'

    ; Is the image that you got here the same size as the cgImage image?
    void = Image_Dimensions(image, XSIZE=imgxsize, YSIZE=imgysize)
    IF imgxsize NE _cgimage_xsize OR imgysize NE _cgimage_ysize THEN $
        Message, 'Image dimensions to not match those of last displayed image.'

    ; Make the image window the current graphics window.
    thisWindow = !D.Window
    WSet, _cgimage_winID

    ; Are the window sizes still right?
    IF !D.X_Size NE _cgimage_winxsize OR !D.Y_Size NE _cgimage_winysize THEN BEGIN
        WSet, thisWindow
        Message, 'The image window size has changed size from when image was last displayed.'
    ENDIF

    ; If a position was supplied, use that rather than the stored position.
    IF N_Elements(position) NE 0 THEN thePos = position ELSE thePos = _cgimage_position

    ; Print instructions to the user.
    Print, ''
    Print, 'Click cursor in image window (window ' + StrTrim(_cgimage_winID,2) + ') to get image values.'
    Print, 'Use LEFT mouse buton to inquire.'
    Print, 'Use RIGHT mouse button to exit program.'
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
          xvec = Scale_Vector(Findgen(_cgimage_xsize+1), thePos[0], thePos[2])
          yvec = Scale_Vector(Findgen(_cgimage_ysize+1), thePos[1], thePos[3])
          xpixel = Value_Locate(xvec, x)
          ypixel = Value_Locate(yvec, y)

          ;; If a hash wasn't input, use the old output convention.
          cgImageInfoDescribeValues, image,  xpixel,  ypixel, hsh
          
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
    Print, 'Good-bye. cgImageInfo has returned control to IDL.'
    Print, ''

END
