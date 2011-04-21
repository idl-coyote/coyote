;+
; NAME:
;     cgIMAGE
;
; PURPOSE:
;     This purpose of cgIMAGE is to create a device-independent TV command
;     with the power and functionality to be used in sophisticated graphics
;     programs, as well as at the IDL command line. It can be thought of as 
;     a "smart" TV command.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;     Graphics display.
;
; CALLING SEQUENCE:
;
;     cgIMAGE, image
;
; INPUTS:
;     image:    A 2D (MxN) or 24-bit (e.g., MxNx3) byte array. A
;               24-bit image with an alpha channel (e.g., MxNx4) is
;               also permitted. The alpha channel will be blended with
;               the current background image. See the AlphaBackgroundImage
;               keyword for details.
;
;       x  :    The X position of the lower-left corner of the image.
;               This parameter is only recognized if the TV keyword is set.
;               If the Y position is not used, X is taken to be the image
;               "position" in the window. See the TV command documenation
;               for details.
;
;       y  :    The Y position of the lower-left corner of the image.
;               This parameter is only recognized if the TV keyword is set.
;
; KEYWORD PARAMETERS:
;
;     ADDCMD:   Set this keyword to add the cgImage command to an cgWindow
;               command list. Setting this command will force ERASEIT to be set
;               to 0, so the cgImage command can exist peacefully with other commands
;               in an cgWindow command list. Setting this keyword automatically sets
;               the WINDOW keyword.
;               
;     ALPHABACKGROUNDIMAGE: Normally, when a image with an alpha channel is displayed,
;               the image is blended with the image currently in the display window.
;               This means, the program has to obtain that background image. This is not a 
;               problem on devices (e.g., WIN, X, Z) that allow this kind of operation,
;               but it is on devices (e.g., the PostScript device, PS) that do not.
;               To get around this problem, you can pass the background image to the
;               cgImage program, along with the alpha channel image you wish to display
;               (via the image parameter) and the alpha channel image will be blended
;               with this image appropriately on all devices. If an alpha channel image
;               is displayed on a device in which there is no way to obtain the background
;               image, and this keyword is not used to pass a background image, then
;               the alpha channel image will be blended with a white background image.
;               This keyword is only used if an alpha channel image is passed to the 
;               program via the image parameter. The AlphaBackgroundImage does not need
;               to have the same dimensions as the alpha channel image.
;             
;     ALPHABGPOSITION: Normally, the alpha background image is displayed in the current graphics
;               window, or is taken from the current graphics window. This is not always correct.
;               Sometimes you want to actually position both the background and foreground image
;               in a window. If this keyword is used to specify a position, the alphabackground image
;               will be positioned at this location before the blending occurs.
;
;     AXES:     Set this keyword to draw a set of axes around the image. Setting this
;               keyword also sets SAVE=1, unless told otherwise.
;
;     AXKEYWORDS:   An IDL structure variable of PLOT keywords as structure fields
;               and keyword values as the values of the fields. Pass directly to the
;               PLOT command that draws the axes for the image. Ignored unless the
;               AXES keyword is set. For example,
;
;               cgImage, image, /AXES, AXKEYWORDS={TICKLEN:-0.025}
;               
;               The axis color, range, title, font and character size must be 
;               set with cgImage keywords ACOLOR, [XY]RANGE, [XY]TITLE, FONT, and
;               CHARSIZE.
;
;     BACKGROUND:   This keyword specifies the name of the background color. The default is "white".
;
;     BOTTOM:   IF SCALE=1, the image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of BOTTOM is 0 by default.
;
;     COLOR:    Set this keyword to the name of an axis color. Default = "black".
;     
;     CHARSIZE: Sets the character size. Used only if the AXES keyword is also set.
;               By default, the value from cgDefCharsize().
;
;     FONT:     Set this to the type of font wanted on axis annotation. By default
;               FONT = !P.Font. Used only if the AXES keyword is also set.
;
;     _EXTRA:   This keyword picks up any TV keywords you wish to use.
;     
;     INTERPOLATE: Normally, nearest neightbor sampling is done when the image size changes.
;               Setting this keyword will cause linear interpolation to be used instead.
;
;     KEEP_ASPECT: An image uses the POSITION keyword to located itself in a graphics
;               window. If this keyword is set, then the image arranges itself in the
;               window in a manner that maintains its orginal aspect ratio (ratio of
;               height to width). 
;
;     LAYOUT:   This keyword specifies a grid with a graphics window and determines 
;               where the graphic should appear. The syntax of LAYOUT is a 3-element 
;               array: [ncolumns, nrows, location]. The grid is determined by the 
;               number of columns (ncolumns) by the number of rows (nrows). The location 
;               of the graphic is determined by the third number. The grid numbering 
;               starts in the upper left (1) and goes sequentually by column and then
;               by row.
;               
;     MARGIN:   A single value, expressed as a normalized coordinate, that
;               can easily be used to calculate a position in the window.
;               The margin is used to calculate a POSITION that gives
;               the image an equal margin around the edge of the window.
;               The margin must be a number in the range 0.0 to 0.333. This
;               keyword is ignored if the POSITION or OVERPLOT keywords are
;               used. It is also ignored when cgImage is executed in a
;               multi-plot window, EXCEPT if it's value is zero. In this
;               special case, the image will be drawn into its position in
;               the multi-plot window with no margins whatsoever. (The
;               default is to have a slight margin about the image to separate
;               it from other images or graphics. The default margin is 0.05.
;
;      MAXVALUE: If defined, the data is linearly scaled between MINVALUE
;               and MAXVALUE. MAXVALUE is set to MAX(image) by default.
;               Setting this keyword to a value implies SCALE=1. If the maximum
;               value of the image is GT 255, this implies SCALE=1.
;
;      MINVALUE: If defined, the data is linearly scaled between MINVALUE
;               and MAXVALUE. MINVALUE is set to MIN(image) by default.
;               Setting this keyword to a value implies SCALE=1. If the minimum
;               value of the image is LT 0, this implies SCALE=1.
;               
;      MULTIMARGIN: Sometimes, when displaying multiple images with !P.Multi, you
;               want the images to be slightly smaller than the position created
;               by !P.Multi so you can add, for example, a colorbar or an annotation
;               to the image. This keyword can be used to adjust the image position
;               by a small margin. A four-element array, the margins apply to the 
;               [bottom, left, top, right] of the image position. So, to
;               leave room at the top of an image for a color bar, you might
;               type this:
;               
;                  cgImage, image, MultiMargin=[0, 0, 4, 0]
;                  
;               This keyword applies *only* to images displayed with !P.Multi, and if
;               passed a scalar value, will use the same value for all four positions.
;               
;     MINUS_ONE: The value of this keyword is passed along to the FSC_RESIZE_IMAGE
;               command. It prevents FSC_RESIZE_IMAGE from adding an extra row and
;               column to the resulting array, which can be a problem with
;               small image arrays. This keyword is set to 0 by default.
;
;     NCOLORS:  If this keyword is supplied, the TOP keyword is ignored and
;               the TOP keyword is set equal to  NCOLORS - 1. This
;               keyword is provided to make cgImage easier to use with the
;               color-loading programs such as LOADCT:
;
;                  LoadCT, 5, NColors=100, Bottom=100
;                  cgImage, image, NColors=100, Bottom=100
;                  
;               Setting this keyword to a value implies SCALE=1.
;               
;     NOERASE:  The default behavior is to erase the display before displaying
;               the image. Setting this keyword prevents the erasure.
;
;     NORMAL:   Setting this keyword means image position coordinates x and y
;               are interpreted as being in normalized coordinates. This keyword
;               is only valid if the TV keyword is set.
;               
;     OPOSITION: If the KEEP_ASPECT keyword is set, there is a good chance the
;               image's actual position in the window will differ from its input
;               position. This keyword will report the image's actual output
;               position, after it has been displayed and its position adjusted
;               to account for aspect ratio. Note that the POSITION keyword is 
;               both an input and output keyword if the input position is passed
;               as a variable. The OPOSITION keyword allows you to obtain the
;               output position even if you pass the position in as a value.
;
;     OVERPLOT: Setting this keyword causes the POSITION keyword to be ignored
;               and the image is positioned in the location established by the
;               last graphics command. For example:
;
;                    Plot, Findgen(11), Position=[0.1, 0.3, 0.8, 0.95]
;                    cgImage, image, /Overplot
;
;     PALETTE:  Set this keyword to a 3 x N or N x 3 byte array containing
;               the RGB color vectors to be loaded before the image is displayed.
;               Such vectors can be obtained, for example, from cgLoadCT with the
;               RGB_TABLE keyword:
;               
;                    cgLoadCT, 4, /BREWER, /REVERSE, RGB_TABLE=palette
;                    cgImage, cgDemoData(7), PALETTE=palette
;                    
;     POSITION: The location of the image in the output window. This is
;               a four-element floating array of normalized coordinates of
;               the type given by !P.POSITION or the POSITION keyword to
;               other IDL graphics commands. The form is [x0, y0, x1, y1].
;               The default is [0.0, 0.0, 1.0, 1.0]. Note that this keyword is ALSO
;               an output keyword. That is to say, upon return from cgImage
;               this keyword (if passed by reference) contains the actual
;               position in the window where the image was displayed. This
;               may be different from the input values if the KEEP_ASPECT_RATIO
;               keyword is set, or if you are using cgImage with the POSITION
;               keyword when !P.MULTI is set to something other than a single
;               plot. One use for the output values might be to position other
;               graphics (e.g., a colorbar) in relation to the image.
;
;               Note that the POSITION keyword should not, normally, be used
;               when displaying multiple images with !P.MULTI. If it *is* used,
;               its meaning differs slightly from its normal meaning. !P.MULTI
;               is responsible for calculating the position of graphics in the
;               display window. Normally, it would be a mistake to use a POSITION
;               graphics keyword on a graphics command that was being drawn with
;               !P.MULTI. But in this special case, cgImage will use the POSITION
;               coordinates to calculate an image position in the actual position
;               calculated for the image by !P.MULTI. The main purpose of this
;               functionality is to allow the user to display images along with
;               colorbars when using !P.MULTI. See the example below.
;
;    QUIET:      There are situations when you would prefer that cgIMAGE does not
;                advertise itself by filling out the FSC_$CGIMAGE common block. For
;                example, if you are using cgImage to draw a color bar, it would
;                not be necessary. Setting this keyword means that cgImage just
;                goes quietly about it's business without bothering anyone else.
;             
;    SAVE:      Set this to cause a data coordinate system to be established
;               for the image. The XRANGE and YRANGE keyword values will be used
;               to establish a data coordinate system coincident with the final
;               image position. Setting the AXES keyword automatically sets SAVE=1.
;
;    SCALE:     Set this keyword to byte scale the image before display. If this
;               keyword is not set, the image is not scaled before display.
;               
;    TITLE:     The title annotation. Used only if the keyword AXES is set.
;               
;    TOP:       IF SCALE=1, the image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of TOP is !D.Table_Size by default.
;
;    TV:        Setting this keyword makes the cgImage command work much
;               like the TV command, although better. That is to say, it
;               will still set the correct DECOMPOSED state depending upon
;               the kind of image to be displayed (8-bit or 24-bit). It will
;               also allow the image to be "positioned" in the window by
;               specifying the coordinates of the lower-left corner of the
;               image. The NORMAL keyword is activated when the TV keyword
;               is set, which will indicate that the position coordinates
;               are given in normalized coordinates rather than device
;               coordinates.
;
;               Setting this keyword will ensure that the keywords
;               KEEP_ASPECT_RATIO, MARGIN, MINUS_ONE, MULTI, and POSITION
;               are ignored. Alpha channels are also ignored when this keyword
;               is set.
;               
;      WINDOW:  Set this keyword to add the command to an cgWindow application.
;               Setting this keyword ALWAYS sets the ERASEIT keyword. If you want
;               to add an image to an cgWindow without the ERASEIT keyword set,
;               use the ADDCMD keyword.
;               
;      XRANGE:  If the AXES keyword is set, this keyword is a two-element vector
;               giving the X axis range. By default, [0, size of image in X].
;            
;      XTITLE:  The X title string to use for the X image axis. Used only if the AXES
;               keyword is set.
;
;      YRANGE:  If the AXES keyword is set, this keyword is a two-element vector
;               giving the Y axis range. By default, [0, size of image in Y].
;
;      YTITLE:  The Y title string to use for the Y image axis. Used only if the AXES
;               keyword is set.
; OUTPUTS:
;     None.
;
; SIDE EFFECTS:
;     Unless the KEEP_ASPECT keyword is set, the displayed image
;     may not have the same aspect ratio as the input data set.
;
; RESTRICTIONS:
;     If the POSITION keyword and the KEEP_ASPECT keyword are
;     used together, there is an excellent chance the POSITION
;     parameters will change. If the POSITION is passed in as a
;     variable, the new positions will be returned in the same variable
;     as an output parameter.
;
; EXAMPLE:
;     To display an image with a contour plot on top of it, type:
;
;        filename = FILEPATH(SUBDIR=['examples','data'], 'worldelv.dat')
;        image = BYTARR(360,360)
;        OPENR, lun, filename, /GET_LUN
;        READU, lun, image
;        FREE_LUN, lun
;
;        thisPostion = [0.1, 0.1, 0.9, 0.9]
;        cgIMAGE, image, POSITION=thisPosition, /KEEP_ASPECT_RATIO
;        CONTOUR, image, POSITION=thisPosition, /NOERASE, XSTYLE=1, $
;            YSTYLE=1, XRANGE=[0,360], YRANGE=[0,360], NLEVELS=10
;
;     To display four images in a window without spacing between them:
;
;     !P.Multi=[0,2,2]
;     cgImage, image, Margin=0
;     cgImage, image, Margin=0
;     cgImage, image, Margin=0
;     cgImage, image, Margin=0
;     !P.Multi = 0
;
;     To display four image in a window with associated color bars:
;
;     !P.Multi=[0,2,2]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 0
;     cgImage, image, Position=p
;     cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 2
;     cgImage, image, Position=p
;     cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 3
;     cgImage, image, Position=p
;     cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 5
;     cgImage, image, Position=p
;     cgColorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     !P.Multi =0
;
; MODIFICATION HISTORY:
;      Written by: David W. Fanning, from modifications to TVIMAGE. 3 Feb 2011.
;      8 Feb 2011. Added OPOSITION keyword. DWF.
;      27 Feb 2011. Added keywords to make cgImage more compatible with TVImage calls. DWF.
;      Color table vectors must be obtained AFTER loading the color palette. 6 March 2011. DWF.
;      I have been convinced (conversations with Wayne Landsman) that if the 
;         CENTER keyword is set, the MINUS_ONE keyword is not needed, since 
;         it was created to solve the same problem. So, I have changed the 
;         default setting of MINUS_ONE to 0. 14 March 2011. DWF.
;       Corrected a problem with restoring color tables if a PALETTE is used. 31 March 2011. DWF.
;-
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
FUNCTION cgImage_PREPARE_ALPHA, image, position, alphaBackgroundImage, $
    ALPHABGPOSITION=alphapos, $
    NOINTERP=nointerp, $
    TV=tv

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       IF Ptr_Valid(ptr) THEN BEGIN
            image = Temporary(*ptr)
            Ptr_Free, ptr
       ENDIF
       IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
       RETURN, image
    ENDIF


    ; Prepare an alpha image, if needed.
    index = Where(Size(image,/DIMENSIONS) EQ 4)
    CASE index OF
         0: aImage = Transpose(image, [1,2,0])
         1: aImage = Transpose(image, [0,2,1])
         ELSE: aImage = image
    ENDCASE
       
    ; Separate the alpha channel.
    alpha_channel = aImage[*,*,3]
       
    ; If this is acting like a TV command, then there is no alpha channel.
    ; Exit now.
    IF Keyword_Set(tv) THEN RETURN, aImage[*,*,0:2]
    
    ; If this version of IDL is 6.4 or older, we can't do this.
    thisRelease = Float(!Version.Release)
    IF thisRelease LT 6.5 THEN BEGIN
       Message, 'IDL 6.5 or higher required to correctly display alpha images.', /INFORMATIONAL
       RETURN, aImage[*,*,0:2]
    ENDIF
                    
    ; Some alpha channels are screwy. Just ignore those and return now.
    IF MIN(alpha_channel) EQ MAX(alpha_channel) THEN RETURN, aImage[*,*,0:2]
       
    ; Now we have an alpha channel.
    alpha_channel = Scale_Vector(alpha_channel, 0.0, 1.0)
    foregndImage = aImage[*,*,0:2]
       
    ; If we don't have a background image, check to see if we
    ; are on a device where we could get such an image.
    IF N_Elements(alphaBackgroundImage) EQ 0 THEN BEGIN
        IF (!D.Flags AND 256) NE 0 THEN BEGIN
           alphaBackgroundImage = cgSnapshot()
        ENDIF ELSE BEGIN
            ss = Size(foregndImage, /DIMENSIONS)
            alphaBackgroundImage = BytArr(ss[0], ss[1], 3) + 255B
        ENDELSE
    ENDIF
    
    ; Get the size and dimensions of the background image.
    ndim = Size(alphaBackgroundImage, /N_DIMENSIONS)
    CASE ndim OF
        2: BEGIN
           TVLCT, r, g, b, /GET
           s = Size(alphaBackgroundImage, /DIMENSIONS)
           bImage = BytArr(s[0], s[1], 3)
           bImage[*,*,0] = r[alphaBackgroundImage]
           bImage[*,*,1] = g[alphaBackgroundImage]
           bImage[*,*,2] = b[alphaBackgroundImage]
           END
        3: BEGIN
           index = Where(Size(alphaBackgroundImage,/DIMENSIONS) EQ 3)
           CASE index OF
              0: bImage = Transpose(alphaBackgroundImage, [1,2,0])
              1: bImage = Transpose(alphaBackgroundImage, [0,2,1])
              ELSE: bImage = alphaBackgroundImage
           ENDCASE
           END
      ELSE: Message, 'Unexpected dimensions of the background image.'
    ENDCASE
    
    ; Now that we have a background image, display that in
    ; the Z-Graphics buffer.
    sb = Size(bImage, /DIMENSIONS)
    sf = Size(foregndImage, /DIMENSIONS)
    thisDevice = !D.Name
    Set_Plot, 'Z'
    Device, Get_Decomposed=theState
    Device, Set_Resolution=sb[0:1], Decomposed=1, Set_Pixel_Depth=24
   
    IF N_Elements(alphapos) EQ 0 THEN BEGIN
        cgImage, bImage, /NOINTERP
    ENDIF ELSE BEGIN
        cgImage, bImage, Position=alphapos, /NOINTERP
    ENDELSE
    
    ; Calculate the parameters for taking a snapshot of the
    ; relevant portion of the window.
    xstart = position[0]*sb[0]
    cols = (position[2] - position[0]) * sb[0]
    ystart = position[1]*sb[1]
    rows = (position[3] - position[1]) * sb[1]
            
    ; Take a snapshot
    bImage = TVRD(xstart, ystart, cols, rows, TRUE=3)
            
    ; Get the size of the snapshot.
    sb = Size(bImage, /DIMENSIONS)
    Device, Decomposed=theState
    Set_Plot, thisDevice
            
     ; Make the foreground image the right size.
     foregndImage = FSC_Resize_Image(foregndImage, cols, rows)
     alpha = FSC_Resize_Image(alpha_channel, sb[0], sb[1], /INTERPOLATE)
     alpha = Rebin(alpha, sb[0], sb[1], 3)
            
     ; Blend the two images.
     outImage = foregndImage*alpha + (1 - alpha)*bImage   
            
     ; Put the dimensions back the way they came in.   
     index = Where(Size(foregndImage,/DIMENSIONS) EQ 3)
     CASE index OF
        0: outImage = Transpose(outImage, [2,0,1])
        1: outImage = Transpose(outImage, [1,0,2])
        ELSE: outImage = outImage
     ENDCASE
    
     RETURN, outimage
END
;--------------------------------------------------------------------------



PRO cgImage, image, x, y, $
   ADDCMD=addcmd, $
   ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
   ALPHABGPOSITION=alphapos, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BOTTOM=bottom, $
   COLOR=color, $
   ERASE=obsolte_erase, $ ; Added for compatibility with TVIMAGE.
   INTERPOLATE=interp, $
   KEEP_ASPECT_RATIO=keep_aspect, $
   LAYOUT=layout, $
   MARGIN=margin, $
   MAXVALUE=max, $
   MINUS_ONE=minusOne, $
   MINVALUE=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOERASE=noerase, $
   NOINTERPOLATION=obsolete_nointerpolation, $ ; Added for compatibility with TVIMAGE.
   NORMAL=normal, $
   OPOSITION=oposition, $
   OVERPLOT=overplot, $
   PALETTE=palette, $
   POSITION=position, $
   QUIET=quiet, $
   SAVE=save, $
   SCALE=scale, $
   TOP=top, $
   TV=tv, $
   WINDOW=window, $
   XRANGE=plotxrange, $
   XTITLE=plotxtitle, $
   YRANGE=plotyrange, $
   YTITLE=plotytitle, $
   _REF_EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
       RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgImage, image'
        RETURN
    ENDIF
    
    ; Handle obsolete keywords.
    IF N_Elements(obsolete_erase) NE 0 THEN noerase = 1 - obsolete_erase
    IF N_Elements(obsolete_nointerpolation) NE 0 THEN interp = 1 - obsolete_nointerpolation
    
    ; Set up a common block as input to TVINFO.
    COMMON FSC_$CGIMAGE, _cgimage_xsize, _cgimage_ysize, $
                         _cgimage_winxsize, _cgimage_winysize, $
                         _cgimage_position, _cgimage_winID, $
                         _cgimage_current
    
    ; Add the command to cgWindow?
    IF Keyword_Set(addcmd) THEN BEGIN
        noerase = 1
        window = 1
    ENDIF
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        ; If we are adding a command, we have to do something different.
        IF Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgImage', image, x, y, $
               ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
               ALPHABGPOSITION=alphapos, $
               AXIS=axis, $
               AXES=axes, $
               AXKEYWORDS=axkeywords, $
               BACKGROUND=background, $
               BOTTOM=bottom, $
               COLOR=color, $
               INTERPOLATE=interp, $
               KEEP_ASPECT_RATIO=keep_aspect, $
               LAYOUT=layout, $
               MARGIN=margin, $
               MAXVALUE=max, $
               MINUS_ONE=minusOne, $
               MINVALUE=min, $
               MULTIMARGIN=multimargin, $
               NCOLORS=ncolors, $
               NOERASE=noerase, $
               NORMAL=normal, $
               OPOSITION=oposition, $
               OVERPLOT=overplot, $
               PALETTE=palette, $
               POSITION=position, $
               QUIET=quiet, $
               SAVE=save, $
               SCALE=scale, $
               TOP=top, $
               TV=tv, $
               XRANGE=plotxrange, $
               XTITLE=plotxtitle, $
               YRANGE=plotyrange, $
               YTITLE=plotytitle, $
               ADDCMD=1, $
               _EXTRA=extra
                 RETURN
        ENDIF
        
        ; Otherwise, we are replacing the commands in a new or existing window.
        cgWindow, 'cgImage', image, x, y, $
               ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
               ALPHABGPOSITION=alphapos, $
               AXIS=axis, $
               AXES=axes, $
               AXKEYWORDS=axkeywords, $
               BACKGROUND=background, $
               BOTTOM=bottom, $
               COLOR=color, $
               INTERPOLATE=interp, $
               KEEP_ASPECT_RATIO=keep_aspect, $
               LAYOUT=layout, $
               MARGIN=margin, $
               MAXVALUE=max, $
               MINUS_ONE=minusOne, $
               MINVALUE=min, $
               MULTIMARGIN=multimargin, $
               NCOLORS=ncolors, $
               NOERASE=noerase, $
               NORMAL=normal, $
               OPOSITION=oposition, $
               OVERPLOT=overplot, $
               PALETTE=palette, $
               POSITION=position, $
               QUIET=quiet, $
               SAVE=save, $
               SCALE=scale, $
               TOP=top, $
               TV=tv, $
               XRANGE=plotxrange, $
               XTITLE=plotxtitle, $
               YRANGE=plotyrange, $
               YTITLE=plotytitle, $
               REPLACECMD=replacecmd, $
               _EXTRA=extra
             RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Which release of IDL is this?
    thisRelease = Float(!Version.Release)
    
    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    keep_aspect = Keyword_Set(keep_aspect)
    
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            overplot = 0
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
    ENDIF

     ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(color) EQ 0 THEN acolorname = 'black' ELSE acolorname = color
    interp = Keyword_Set(interp)
    
    ; Doing multiple plots?
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    
    ; Check for image parameter and keywords.
    IF N_Elements(image) EQ 0 THEN MESSAGE, 'You must pass a valid image argument.'
    
    ; Did the user want to scale the image?
    ; If either MIN or MAX are set, this implies SCALE=1.
    ; If min LT 0 or max GT 255, this implies SCALE=1.
    ; If NCOLORS is used, this implies SCALE=1.
    IF N_Elements(min) EQ 0 THEN min = Min(image, /NAN) ELSE scale = 1
    IF N_Elements(max) EQ 0 THEN max = Max(image, /NAN) ELSE scale = 1
    IF (min LT 0) OR (max GT 255) THEN scale = 1
    IF N_Elements(top) EQ 0 THEN top = !D.TABLE_SIZE - 1
    IF N_Elements(bottom) EQ 0 THEN bottom = 0B
    IF N_Elements(ncolors) NE 0 THEN BEGIN
        top = (ncolors - 1) < 255
        scale = 1
    ENDIF
    ncolors = top-bottom+1
    scale = Keyword_Set(scale)

    ; Check for mis-spelling of AXES as AXIS.
    IF Keyword_Set(axis) THEN axes = 1    
    axes = Keyword_Set(axes)
    
    ; If you want axes, then save the coordinate system, unless s
    ; pecifically asked not to.
    IF axes THEN IF N_Elements(save) EQ 0 THEN save = 1
    
    ; If axes are set and MARGIN and POSITION are NOT set and you are NOT
    ; doing multiplots, then set a normal "plot" margin.
    IF Keyword_Set(axes) AND ((N_Elements(margin) EQ 0) AND (N_Elements(position) EQ 0) $
        AND (multi EQ 0)) THEN margin = 0.1
    
    ; Check other keywords.
    interp = Keyword_Set(interp)
    IF N_Elements(minusOne) EQ 0 THEN minusOne = 0
    minusOne = Keyword_Set(minusOne)
        
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF Size(background, /TNAME) EQ 'STRING' THEN BEGIN
        IF StrUpCase(background) EQ 'WHITE' THEN BEGIN
           IF N_Elements(acolorname) EQ 0 THEN acolorname = 'black 
        ENDIF 
    ENDIF
    noerase = Keyword_Set(noerase) ; Don't change, used in PS output.
    
    ; Choose an axis color.
    IF N_Elements(acolorname) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                acolorname = 'OPPOSITE' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN acolorname = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN acolorname = 'WHITE'
                    IF N_Elements(acolorname) EQ 0 THEN acolorname = 'OPPOSITE'
                ENDIF ELSE acolorname = 'OPPOSITE'
           ENDELSE
     ENDIF
    IF N_Elements(acolorname) EQ 0 THEN acolor = !P.Color ELSE acolor = acolorname
    IF Size(acolor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN acolor = Byte(color)
    IF Size(acolor, /TYPE) LE 2 THEN acolor = StrTrim(Fix(acolor),2)
 
    ; Load the color palette if you are using one.
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
        TVLCT, p_red, p_grn, p_blu, /Get ; Save the color vectors before loading the palette.
        TVLCT, palette
    ENDIF
    
    ; Before you do anything, get the current color table vectors
    ; so they can be restored later. Must do AFTER loading a palette!
    TVLCT, rr, gg, bb, /Get
    
    ; Do you need to erase the window before image display?
    IF ~Keyword_Set(noerase) && (!P.MULTI[0] EQ 0) && (N_Elements(layout) EQ 0) THEN BEGIN
         IF (!D.Flags AND 256) NE 0 THEN BEGIN
            cgErase, background
         ENDIF ELSE BEGIN
            IF (!D.NAME EQ 'Z') THEN BEGIN
                cgErase, background
            ENDIF
            
            ; Do you need a PostScript background color? Lot's of problems here!
            ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
            ; plot of any sort erases the background color. So, I have to draw a 
            ; plot, store the new system variables, then draw my background, etc.
            ; I have tried LOTS of options. This is the only one that worked.
            IF !D.Name EQ 'PS' THEN BEGIN
               IF ~noerase THEN BEGIN
               
                   ; I only have to do this, if this is the first plot.
                   IF !P.MULTI[0] EQ 0 THEN BEGIN
                   
                        ; Save the current system variables. Will need to restore later.
                        bangx = !X
                        bangy = !Y
                        bangp = !P
                        
                        ; Draw the plot that doesn't draw anything.
                        Plot, [0], POSITION=position, /NODATA, XSTYLE=4, YSTYLE=4, ZSTYLE=4
                        
                        ; Save the "after plot" system variables. Will use later. 
                        afterx = !X
                        aftery = !Y
                        afterp = !P     
                        
                        ; Draw the background color and set the variables you will need later.
                        PS_Background, background
                        psnodraw = 1
                        tempNoErase = 1
                        
                        ; Restore the original system variables so that it is as if you didn't
                        ; draw the invisible plot.
                        !X = bangx
                        !Y = bangy
                        !P = bangp
                        TVLCT, rr, gg, bb
                    ENDIF ELSE tempNoErase = noerase
                ENDIF ELSE tempNoErase = noerase
             ENDIF ELSE tempNoErase = noerase
         ENDELSE
    ENDIF
    
    ; Need a margin around the plot?
    IF (N_Elements(margin) GT 0) THEN BEGIN
       IF Keyword_Set(margin) EQ 0 THEN BEGIN
          IF N_Elements(multimargin) EQ 0 THEN multimargin=[0., 0., 0., 0.] 
       ENDIF ELSE BEGIN
          IF N_Elements(multimargin) EQ 0 THEN multimargin=[1., 1., 1., 1.]
       ENDELSE
       IF margin[0] EQ 1 THEN margin = 0.075  ; Comes from /MARGIN
    ENDIF 
    
    ; Make sure the multimargin has four elements.
    IF N_Elements(multimargin) EQ 0 THEN multimargin = [0., 0., 0., 0.] 
    IF N_Elements(multimargin) EQ 1 THEN multimargin = [multimargin, multimargin, multimargin, multimargin]
    IF N_Elements(multimargin) NE 4 THEN Message, 'The keyword MULTIMARGIN must be a four-element array.'
    
    ; Check image size.
    s = Size(image)
    IF s[0] LT 2 OR s[0] GT 3 THEN $
       MESSAGE, 'Argument does not appear to be an image. Returning...'
    alphaImage = 0
    
    ; Allow 24-bit images and 2D images that are sent in as 3D
    ; arrays where one dimension is a 1. 24-bit images can have an
    ; alpha channel.
    IF s[0] EQ 3 THEN BEGIN
    
       ; We are going to fake doing something with the alpha channel here.
       i = Where(s[1:3] EQ 3, threeCnt)
       i = Where(s[1:3] EQ 4, fourCnt)
       IF threeCnt EQ 0 AND fourCnt NE 0 THEN BEGIN
            s[i+1] = 3
            alphaImage = 1
       ENDIF ELSE alphaImage = 0
       
       ; Now handle normal 24-bit images and suspect 2D images.
       IF (s[1] NE 3L) AND (s[2] NE 3L) AND (s[3] NE 3L) THEN BEGIN
          IF (s[1] NE 1L) AND (s[2] NE 1L) AND (s[3] NE 1L) THEN BEGIN
             MESSAGE, 'Argument does not appear to be a 24-bit image. Returning...'
          ENDIF ELSE BEGIN
             IF s[1] EQ 1 THEN single = 1
             IF s[2] EQ 1 THEN single = 2
             IF s[3] EQ 1 THEN single = 3
             CASE single OF
                1: image = Reform(image, s[2], s[3])
                2: image = Reform(image, s[1], s[3])
                3: image = Reform(image, s[1], s[2])
             ENDCASE
             s = Size(image)
          ENDELSE
       ENDIF
    ENDIF ELSE s = Size(image)
    
    ; If a window is not open, open one, otherwise in X devices you get incorrect
    ; window size information the first time you call cgImage.
    IF (!D.FLAGS AND 256) NE 0 THEN IF (!D.Window EQ -1) THEN cgDisplay
    
    ; Check for position and overplot keywords.
    IF N_Elements(position) EQ 0 THEN BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1) THEN BEGIN
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
             XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], $
             NOERASE=N_Elements(layout) EQ 0 ? tempNoErase : 1
          position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          TVLCT, rr, gg, bb
       ENDIF ELSE BEGIN
          IF Keyword_Set(overplot) THEN BEGIN
             position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          ENDIF ELSE position = [0.0, 0.0, 1.0, 1.0]
       ENDELSE
    ENDIF ELSE BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1)THEN BEGIN
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
              XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], $
             NOERASE=N_Elements(layout) EQ 0 ? tempNoErase : 1
          TVLCT, rr, gg, bb
          ; Use position coordinates to indicate position in this set of coordinates.
          xrange = !X.Window[1] - !X.Window[0]
          xstart = !X.Window[0] + position[0]*xrange
          xend = xrange * (position[2] - position[0]) + xstart
    
          yrange = !Y.Window[1] - !Y.Window[0]
          ystart = !Y.Window[0] + position[1]*yrange
          yend = yrange * (position[3] - position[1]) + ystart
    
          ; New position based on !P.MULTI position.
          position = [xstart, ystart, xend, yend]
    
       ENDIF ELSE BEGIN
          IF Keyword_Set(overplot) THEN BEGIN
             position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          ENDIF ELSE position = Float(position)
       ENDELSE
    ENDELSE
    
    ; Check for margin keyword.
    IF (Keyword_Set(multi) EQ 0) AND (Keyword_Set(overplot) EQ 0) THEN BEGIN
       IF N_Elements(margin) NE 0 THEN BEGIN
               margin = 0.0 > margin < 0.33
               position = [position[0] + margin, position[1] + margin, $
                           position[2] - margin, position[3] - margin]
       ENDIF
    ENDIF
    
    ; 2D image.
    IF s[0] EQ 2 THEN BEGIN
    
       imgXsize = FLOAT(s[1])
       imgYsize = FLOAT(s[2])
       true = 0
    
       ; Decomposed color off if device supports it.
       CASE  StrUpCase(!D.NAME) OF
            'X': BEGIN
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'WIN': BEGIN
    
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'MAC': BEGIN
                Device, Get_Visual_Depth=thisDepth
                IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
                Device, Decomposed=0
                ENDCASE
            'Z': BEGIN
                ; Fix for 24-bit Z-buffer.
                IF (thisRelease GE 6.4) THEN BEGIN
                   Device, Get_Decomposed=thisDecomposed, Get_Pixel_Depth=thisDepth
                   Device, Decomposed=0
                ENDIF ELSE thisDepth = 8
                ENDCASE
            'PS': BEGIN
                IF (thisRelease GE 7.1) THEN BEGIN
                   thisDecomposed = DecomposedColor(Depth=thisDepth)
                   Device, Decomposed=0
                ENDIF ELSE thisDepth = 8
                ENDCASE
            ELSE: thisDepth = 8
       ENDCASE
    
    ENDIF
    
    ; 3D image.
    IF s[0] EQ 3 THEN BEGIN
    
      ; What kind of pixel interleaving?
      IF s[1] EQ 3 THEN true = 1 ; Pixel interleaved
      IF s[2] EQ 3 THEN true = 2 ; Row interleaved
      IF s[3] EQ 3 THEN true = 3 ; Band interleaved
    
       ; Decomposed color on if device supports it.
       CASE StrUpCase(!D.NAME) OF
          'X': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'WIN': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'MAC': BEGIN
             Device, Get_Visual_Depth=thisDepth
             IF thisRelease GE 5.2 THEN Device, Get_Decomposed=thisDecomposed
             IF thisDepth GT 8 THEN Device, Decomposed=1
             ENDCASE
          'Z': BEGIN
             ; Fix for 24-bit Z-buffer.
             IF (Float(!Version.Release) GE 6.4) THEN BEGIN
                Device, DECOMPOSED=1, Set_Pixel_Depth=24
                thisDepth = 24
             ENDIF ELSE thisDepth = 8
             ENDCASE
          'PS': BEGIN
             IF (Float(!Version.Release) GE 7.1) THEN BEGIN
                   thisDecomposed = DecomposedColor(Depth=thisDepth)
                   TVLCT, r, g, b, /GET
                   LoadCT, 0, /Silent
                   Device, DECOMPOSED=1, BITS_PER_PIXEL=8, COLOR=1
                   TVLCT, r, g, b
             ENDIF ELSE thisDepth = 8
             ENDCASE
          
          ELSE: thisDepth = 8
       ENDCASE
    
       CASE true OF
          1: BEGIN
             imgXsize = FLOAT(s[2])
             imgYsize = FLOAT(s[3])
             ENDCASE
          2: BEGIN
             imgXsize = FLOAT(s[1])
             imgYsize = FLOAT(s[3])
             ENDCASE
          3: BEGIN
             imgXsize = FLOAT(s[1])
             imgYsize = FLOAT(s[2])
             ENDCASE
       ENDCASE
    
    ENDIF
    
    ; Check for TV keyword. If present, then act like a TV command.
    IF Keyword_Set(tv) THEN BEGIN
    
       IF N_Params() GE 3 OR N_Params() EQ 1 THEN BEGIN
         IF N_Elements(x) EQ 0 THEN x = 0
         IF N_Elements(y) EQ 0 THEN y = 0
         IF Keyword_Set(normal) THEN BEGIN
            IF alphaImage THEN BEGIN
               outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
                    TV=1, ALPHABGPOSITION=alphapos)
               TV, outImage, x, y, True=3, _STRICT_EXTRA=extra, /Normal
            ENDIF ELSE BEGIN
               CASE scale OF
                    0: TV, image, x, y, True=true, _STRICT_EXTRA=extra, /Normal 
                    1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                           x, y, True=true, _STRICT_EXTRA=extra, /Normal
                ENDCASE
            ENDELSE
         ENDIF ELSE BEGIN
            IF alphaImage THEN BEGIN
               outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
                    TV=1, ALPHABGPOSITION=alphapos)
               TV, outImage, x, y, True=3, _STRICT_EXTRA=extra, /Device
            ENDIF ELSE BEGIN
               CASE scale OF
                   0: TV, image, x, y, True=true, _STRICT_EXTRA=extra, /Device
                   1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                           x, y, True=true, _STRICT_EXTRA=extra, /Device
                ENDCASE
            ENDELSE
         ENDELSE
       ENDIF ELSE BEGIN
         IF N_Params() EQ 2 THEN BEGIN
            IF Keyword_Set(normal) THEN BEGIN
                IF alphaImage THEN BEGIN
                   outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
                       TV=1, ALPHABGPOSITION=alphapos)
                   TV, outImage, x,  True=3, _STRICT_EXTRA=extra, /Normal
                ENDIF ELSE BEGIN
                   CASE scale OF 
                        0: TV, image, x, True=true, _STRICT_EXTRA=extra, /Normal
                        1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                                x, True=true, _STRICT_EXTRA=extra, /Normal
                   ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                   outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
                        TV=1, ALPHABGPOSITION=alphapos)
                   TV, outImage, x,  True=3, _STRICT_EXTRA=extra, /Device
                ENDIF ELSE BEGIN
                   CASE scale OF 
                        0: TV, image, x, True=true, _STRICT_EXTRA=extra, /Device
                        1: TV, BytScl(image, Top=top, Max=max, Min=min) + bottom, $
                                x, True=true, _STRICT_EXTRA=extra, /Device
                   ENDCASE
                ENDELSE
             ENDELSE
         ENDIF
       ENDELSE
       GoTo, restoreDecomposed
    
    ENDIF
    
    ; Maintain aspect ratio (ratio of height to width)?
    IF KEYWORD_SET(keep_aspect) THEN BEGIN
    
       ; Find aspect ratio of image.
       ratio = FLOAT(imgYsize) / imgXSize
    
       ; Find the proposed size of the image in pixels without aspect
       ; considerations.
       xpixSize = (position(2) - position(0)) * !D.X_VSize
       ypixSize = (position(3) - position(1)) * !D.Y_VSize
    
       ; Try to fit the image width. If you can't maintain
       ; the aspect ratio, fit the image height.
       trialX = xpixSize
       trialY = trialX * ratio
       IF trialY GT ypixSize THEN BEGIN
          trialY = ypixSize
          trialX = trialY / ratio
       ENDIF
    
       ; Recalculate the position of the image in the window.
       position[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + position[0]
       position[2] = position[0] + (trialX/FLOAT(!D.X_VSize))
       position[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + position[1]
       position[3] = position[1] + (trialY/FLOAT(!D.Y_VSize))
    
    ENDIF
    
    ; Set the output position.
    oposition = position
    
    ; Calculate the image size and start locations.
    xsize = Ceil((position[2] - position[0]) * !D.X_VSIZE)
    ysize = Ceil((position[3] - position[1]) * !D.Y_VSIZE)
    xstart = Round(position[0] * !D.X_VSIZE)
    ystart = Round(position[1] * !D.Y_VSIZE)
    
    ; Display the image. Sizing different for scalable pixels devices.
    IF (!D.Flags AND 1) NE 0 THEN BEGIN
    
       ; Need a gray-scale color table if this is a true
       ; color image.
       IF true GT 0 THEN LOADCT, 0, /Silent
       IF alphaImage THEN BEGIN
           outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
           TV, outImage, xstart, ystart, XSIZE=xsize, YSIZE=ysize, _STRICT_EXTRA=extra, True=3
       ENDIF ELSE BEGIN
           CASE scale OF 
                0: TV, image, xstart, ystart, XSIZE=xsize, $
                        YSIZE=ysize, _STRICT_EXTRA=extra, True=true
                1: TV, BytScl(image, Top=(top-bottom), Max=max, Min=min) + $
                         bottom, xstart, ystart, XSIZE=xsize, YSIZE=ysize, $
                         _STRICT_EXTRA=extra, True=true
            ENDCASE
       ENDELSE
    ENDIF ELSE BEGIN ; All other devices.
    
       CASE true OF
          0: BEGIN
             CASE scale OF
                 0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                        MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra
                 1: TV, BYTSCL( FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), $
                        INTERP=interp, MINUS_ONE=minusOne), Top=top, Max=max, Min=min) $
                        + bottom, ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra
             ENDCASE
             END
          1: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=1
                        1: TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, $
                                Max=max, Min=min) + bottom, ROUND(xstart), ROUND(ystart), $
                                _STRICT_EXTRA=extra, True=1
                     ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 1, r, g, b, _EXTRA=extra)   
                ENDELSE                
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
          2: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=2
                        1: TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, Max=max, $
                                Min=min) + bottom, ROUND(xstart), ROUND(ystart), $
                                _STRICT_EXTRA=extra, True=2
                    ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 2, r, g, b, _EXTRA=extra)
                ENDELSE                
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
          3: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    TV, FSC_Resize_Image(outImage, xsize, ysize, INTERP=interp, $
                       MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    CASE scale OF
                        0: TV, FSC_Resize_Image(image, xsize, ysize, INTERP=interp, $
                                MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=3
                        1: TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), $
                                INTERP=interp, MINUS_ONE=minusOne), Top=top-bottom, Max=max, $
                                Min=min) + bottom, ROUND(xstart), ROUND(ystart), $
                                _STRICT_EXTRA=extra, True=3
                    ENDCASE
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    outImage = cgImage_Prepare_Alpha(image, position, alphaBackgroundImage)
                    image2d = Color_Quan(outImage, 3, r, g, b, _EXTRA=extra)               
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 3, r, g, b, _EXTRA=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, xsize, ysize, INTERP=0, $
                   MINUS_ONE=minusOne), xstart, ystart, _STRICT_EXTRA=extra, True=0
             ENDELSE
      ENDCASE
    ENDELSE
        
    ; Restore Decomposed state if necessary.
    RestoreDecomposed:
    
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
     
    CASE StrUpCase(!D.NAME) OF
       'X': BEGIN
          IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'WIN': BEGIN
          IF thisRelease GE 5.2 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'MAC': BEGIN
          IF thisRelease GE 5.2 THEN BEGIN
             Device, Decomposed=thisDecomposed
    
             ; Here is a hack that fixes a longstanding Mac problem with
             ; color tables after changing the decomposed state.
             TV, [0]
          ENDIF
          ENDCASE
       'Z': BEGIN
          IF thisRelease GE 6.4 THEN Device, Decomposed=thisDecomposed
          ENDCASE
       'PS': BEGIN
          IF thisRelease GE 7.1 THEN BEGIN
              Device, DECOMPOSED=thisDecomposed
              IF N_Elements(r) NE 0 THEN TVLCT, r, g, b
          ENDIF
          ENDCASE
       ELSE:
    ENDCASE

    ; Set up common block parameters, but only if device supports windows.
    ; And only if the QUIET flag is not turned on.
    IF ~Keyword_Set(quiet) THEN BEGIN
        IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
            _cgimage_xsize = imgXsize
            _cgimage_ysize = imgYsize
            _cgimage_winID = !D.Window
            _cgimage_winxsize = !D.X_Size
            _cgimage_winysize = !D.Y_Size
            _cgimage_position = position
            _cgimage_current = 1
        ENDIF 
    ENDIF
    
    ; Save plot system variables.
    bangp = !P
    bangx = !X
    bangy = !Y
     
    ; Need a data range?
    IF N_Elements(plotxrange) EQ 0 THEN plotxrange = [0, imgXsize]
    IF N_Elements(plotyrange) EQ 0 THEN plotyrange = [0, imgYsize]
    
    ; If the user wanted axes, draw them now.
    IF axes THEN BEGIN
    
        cgPLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
            XSTYLE=1, YSTYLE=1, POSITION=position, AXISCOLOR=acolor, $
            XTITLE=plotxtitle, YTITLE=plotytitle, TITLE=title, CHARSIZE=charsize, $
            FONT=font, _STRICT_EXTRA=axkeywords
            
    ENDIF ELSE BEGIN
    
        ; If you are saving the data coordinate space, draw invisible axes.
        IF Keyword_Set(save) THEN BEGIN
            PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
                XSTYLE=5, YSTYLE=5, POSITION=position, _STRICT_EXTRA=axkeywords
        ENDIF
    
    ENDELSE

    ; Clean up after yourself.
    IF (!D.Name NE 'Z') THEN BEGIN
        TVLCT, rr, gg, bb
        ; If you loaded a color palette, restore the before color vectors.
        IF N_Elements(p_red) NE 0 THEN TVLCT, p_red, p_grn, p_blu
    ENDIF
    IF ~Keyword_Set(save) THEN BEGIN
        !P = bangp
        !X = bangx
        !Y = bangy
    ENDIF

    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti

END