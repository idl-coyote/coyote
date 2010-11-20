;+
; NAME:
;     TVSCALE
;
; PURPOSE:
;     This purpose of TVSCALE is to create a device-independent TVSCL command
;     with the power and functionality to be used in sophisticated graphics
;     programs, as well as at the IDL command line. It can be thought of as 
;     a "smart" TVSCL command.
;
;     Use the TOP and BOTTOM keywords to define a particular set of
;     number to scale the data to. The algorithm used is this:
;
;         TV, BytScl(image, TOP=top-bottom) + bottom
;
;     Note that if you scale the image between 100 and 200, that
;     there are 101 possible pixel values. So the proper way to
;     load colors would be like this:
;
;       LoadCT, NColors=101, Bottom=100
;       TVSCALE, image, Top=200, Bottom=100
;
;     Alternatively, you could use the NCOLORS keyword:
;
;       LoadCT, NColors=100, Bottom=100
;       TVSCALE, image, NColors=100, Bottom=100
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;     Graphics display.
;
; CALLING SEQUENCE:
;
;     TVSCALE, image
;
; INPUTS:
;     image:    A 2D or 3D image array. It does not have to be byte data.
;
;       x  :    The X position of the lower-left corner of the image.
;               This parameter is only recognized if the TV keyword is set.
;               If the Y position is not used, X is taken to be the image
;               "position" in the window. See the TV command documenation
;               for details.
;
;       y  :    The Y position of the lower-left corner of the image.
;               This parameter is only recognized if the TVSCL keyword is set.
;
; KEYWORD PARAMETERS:
;
;     ACOLOR:   Set this keyword to the axis color. If a byte or integer value,
;               it will assume you are using INDEXED color mode. If a long integer
;               is will assume you are using DECOMPOSED color mode. If a string,
;               is will pass the string color name along to FSC_COLOR for processing.
;
;     AXES:     Set this keyword to draw a set of axes around the image. Setting this
;               keyword also sets SAVE=1, unless told otherwise.
;
;     AXKEYWORDS:   An IDL structure variable of PLOT keywords as structure fields
;               and keyword values as the values of the fields. Pass directly to the
;               PLOT command that draws the axes for the image. Ignored unless the
;               AXES keyword is set. For example,
;
;               TVScale, image, /AXES, AXKEYWORDS={CHARSIZE:1.5, XTITLE:'Distance (mm)', $
;                    YTITLE:'Signal', COLOR:FSC_Color('dodger blue')}
;
;     BACKGROUND:   This keyword specifies the background color. Note that
;               the keyword ONLY has effect if the ERASE keyword is also
;               set or !P.MULTI is set to multiple plots and TVIMAGE is
;               used to place the *first* plot. [Note change, setting this keyword
;               automatically sets ERASE=1.] Can be a string (e.g., 'ivory'), or
;               a 24-bit value that can be decomposed into a color, or an 8-bit
;               index number into the current color table. 
;
;               If you are in indexed color mode, the background color index
;               must be outside the range of image color indices, or you will
;               see the background color in your image output. For example, you
;               should NOT use code like this while in indexed color mode:
;               
;                  TVImage, image, Background=FSC_Color('gray')
;                  
;                Since FSC_Color will load a gray color in index 81, which is inside
;                the indices assigned to the image. To do this correctly in indexed color
;                mode, do this:
;                
;                   TVImage, image, Background='gray'
;                   
;                Or, this:
;                
;                    TVImage, Bytscl(image, Top=253), Background=FSC_Color('gray',254)
;
;     BOTTOM:   The image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of BOTTOM is 0 by default.
;
;     ERASE:    If this keyword is set an ERASE command is issued
;               before the image is displayed. ERASE and BACKGROUND 
;               should only be used on 24-bit devices that support windows! 
;
;     _EXTRA:   This keyword picks up any TV keywords you wish to use.
;
;     HALF_HALF: Obsolete and unused.
;
;     KEEP_ASPECT_RATIO: Normally, the image will be resized to fit the
;               specified position in the window. If you prefer, you can
;               force the image to maintain its aspect ratio in the window
;               (although not its natural size) by setting this keyword.
;               The image width is fitted first. If, after setting the
;               image width, the image height is too big for the window,
;               then the image height is fitted into the window. The
;               appropriate values of the POSITION keyword are honored
;               during this fitting process. Once a fit is made, the
;               POSITION coordiates are re-calculated to center the image
;               in the window. You can recover these new position coordinates
;               as the output from the POSITION keyword.
;
;     MARGIN:   A single value, expressed as a normalized coordinate, that
;               can easily be used to calculate a position in the window.
;               The margin is used to calculate a POSITION that gives
;               the image an equal margin around the edge of the window.
;               The margin must be a number in the range 0.0 to 0.333. This
;               keyword is ignored if the POSITION or OVERPLOT keywords are
;               used. It is also ignormed when TVImage is executed in a
;               multi-plot window, EXCEPT if it's value is zero. In this
;               special case, the image will be drawn into its position in
;               the multi-plot window with no margins whatsoever. (The
;               default is to have a slight margin about the image to separate
;               it from other images or graphics.
;
;     MAXVALUE: The data is linearly scaled between the MIN and MAX values,
;               if they are provided. MAX is set to MAX(image) by default.
;
;     MINVALUE: The data is linearly scaled between the MIN and MAX values,
;               if they are provided. MIN is set to MIN(image) by default.
;
;     MINUS_ONE: The value of this keyword is passed along to the FSC_Resize_Image
;               command. It prevents FSC_Resize_Image from adding an extra row and
;               column to the resulting array, which can be a problem with
;               small image arrays. By default, set to 1.
;
;     MULTIMARGIN: Sometimes, when displaying multiple images with !P.Multi, you
;               want the images to be slightly smaller than the position created
;               by !P.Multi so you can add, for example, a colorbar or an annotation
;               to the image. This keyword can be used to adjust the image position
;               by a small margin. A four-element array, the margins apply to the 
;               [bottom, left, top, right] of the image position. So, to
;               leave room at the top of an image for a color bar, you might
;               type this:
;               
;                  TVImage, image, MultiMargin=[0, 0, 4, 0]
;                  
;               This keyword applies *only* to images displayed with !P.Multi, and if
;               passed a scalar value, will use the same value for all four positions.
;               
;     NCOLORS:  If this keyword is supplied, the TOP keyword is ignored and
;               the TOP keyword is set equal to BOTTOM + NCOLORS - 1. This
;               keyword is provided to make TVSCALE easier to use with the
;               color-loading programs such as LOADCT:
;
;                  LoadCT, 5, NColors=100, Bottom=100
;                  TVScale, image, NColors=100, Bottom=100
;
;     NOINTERPOLATION: Setting this keyword disables the default bilinear
;               interpolation done to the image when it is resized. Nearest
;               neighbor interpolation is done instead. This is preferred
;               when you do not wish to change the pixel values of the image.
;
;     NORMAL:   Setting this keyword means image position coordinates x and y
;               are interpreted as being in normalized coordinates. This keyword
;               is only valid if the TVSCL keyword is set.
;
;     OVERPLOT: Setting this keyword causes the POSITION keyword to be ignored
;               and the image is positioned in the location established by the
;               last graphics command. For example:
;
;                    Plot, Findgen(11), Position=[0.1, 0.3, 0.8, 0.95]
;                    TVScale, image, /Overplot
;
;     POSITION: The location of the image in the output window. This is
;               a four-element floating array of normalized coordinates of
;               the type given by !P.POSITION or the POSITION keyword to
;               other IDL graphics commands. The form is [x0, y0, x1, y1].
;               The default is [0.0, 0.0, 1.0, 1.0]. Note that this keyword is ALSO
;               an output keyword. That is to say, upon return from TVSCALE
;               this keyword (if passed by reference) contains the actual
;               position in the window where the image was displayed. This
;               may be different from the input values if the KEEP_ASPECT_RATIO
;               keyword is set, or if you are using TVSCALE with the POSITION
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
;               !P.MULTI. But in this special case, TVSCALE will use the POSITION
;               coordinates to calculate an image position in the actual position
;               calculated for the image by !P.MULTI. The main purpose of this
;               functionality is to allow the user to display images along with
;               colorbars when using !P.MULTI. See the example below.
;
;    QUIET:     There are situations when you would prefer that TVIMAGE does not
;               advertise itself by filling out the FSC_$TVIMAGE common block. For
;               example, if you are using TVIMAGE to draw a color bar, it would
;               not be necessary. Setting this keyword means that TVIMAGE just
;               goes quietly about it's business without bothering anyone else.
;
;    SAVE:      Set this to cause a data coordinate system to be established
;               for the image. The XRANGE and YRANGE keyword values will be used
;               to establish a data coordinate system coincident with the final
;               image position. 
;
;     TOP:      The image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of TOP is !D.Table_Size by default.
;
;     TVSCL:    Setting this keyword makes the TVIMAGE command work much
;               like the TVSCL command, although better. That is to say, it
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
;               are ignored.
;
;      XRANGE:  If the AXES keyword is set, this keyword is a two-element vector
;               giving the X axis range. By default, [0, size of image in X].
;
;      YRANGE:  If the AXES keyword is set, this keyword is a two-element vector
;               giving the Y axis range. By default, [0, size of image in Y].
;
; OUTPUTS:
;     None.
;
; SIDE EFFECTS:
;     Unless the KEEP_ASPECT_RATIO keyword is set, the displayed image
;     may not have the same aspect ratio as the input data set.
;
; RESTRICTIONS:
;     If the POSITION keyword and the KEEP_ASPECT_RATIO keyword are
;     used together, there is an excellent chance the POSITION
;     parameters will change. If the POSITION is passed in as a
;     variable, the new positions will be returned in the same variable
;     as an output parameter.
;
;     If a 24-bit image is displayed on an 8-bit display, the
;     24-bit image must be converted to an 8-bit image and the
;     appropriate color table vectors. This is done with the COLOR_QUAN
;     function. The TVSCALE command will load the color table vectors
;     and set the NOINTERPOLATION keyword if this is done. Note that the
;     resulting color table vectors are normally incompatible with other
;     IDL-supplied color tables. Hence, other graphics windows open at
;     the time the image is display are likely to look strange.
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
;        thisPosition = [0.1, 0.1, 0.9, 0.9]
;        TVSCALE, image, POSITION=thisPosition, /KEEP_ASPECT_RATIO
;        CONTOUR, image, POSITION=thisPosition, /NOERASE, XSTYLE=1, $
;            YSTYLE=1, XRANGE=[0,360], YRANGE=[0,360], NLEVELS=10
;
;     To display four image in a window with associated color bars:
;
;     !P.Multi=[0,2,2]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 0
;     TVSCALE, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 2
;     TVSCALE, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 3
;     TVSCALE, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 5
;     TVSCALE, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     !P.Multi =0
;
; MODIFICATION HISTORY:
;      Written by:     David Fanning, 27 May 1999 from TVIMAGE code.
;      Added MIN, MAX, and NCOLORS keywords 28 May 1999. DWF.
;      Added the OVERPLOT keyword to allow plotting on POSITION coordinates
;         estabished by the preceding graphics command. 11 Oct 99. DWF.
;      Added NOINTERPOLATION keyword so that nearest neighbor interpolation
;         is performed rather than bilinear. 3 Dec 99. DWF
;      Brought the TVSCALE code up to date with TVIMAGE code. 3 April 2000. DWF.
;      Brought the TVSCALE code up to date with TVIMAGE code. 6 May 2000. DWF.
;      Change MIN and MAX keywords to MINVALUE and MAXVALUE to prevent
;         ambiguous keyword errors. 27 July 2000. DWF.
;      Brought up to date with changes in TVImage. 23 Sept 2000. DWF.
;      Added fix for brain-dead Macs from Ben Tupper that restores Macs ability to display images. 8 June 2001. DWF.
;      Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;      Brought up to date with changes to TVImage as of 22 April 2007. DWF.
;      Updated the program for the 24-bit Z-buffer in IDL 6.4. 11 June 2007. DWF.
;      Added new POSITION keyword functionality for !P.MULTI display. 9 Sept 2007. DWF.
;      Bit one too many times. Added _STRICT_EXTRA keywords for all _EXTRA keywords. 1 Feb 2008. DWF.
;      Added FSC_$TVIMAGE common block for interactive interaction with TVINFO. 16 March 2008. DWF.
;      Added keywords to allow axes to be drawn around the image. 18 March 2008. DWF.
;      Added QUIET keyword to allow by-passing of FSC_$TVIMAGE common block updating. 21 March 2008. DWF.
;      Made BACKGROUND and ERASE valid keywords only on 24-bit devices. Ignored on others. 28 May 2008. DWF.
;      Cannot make color work in device independent way for axes, unless I handle axis color directly. To this
;          end, I have added an ACOLOR keyword. 16 June 2008. DWF.
;      Added BREWER keyword so I can specify Brewer colors with BACKGROUND and ACOLOR keywords. 16 June 2008. DWF.
;      Added back missing HALF_HALF and OVERPLOT keywords. 17 October 2008. DWF.
;      Added the ability to display transparent images. 13 May 2009. DWF.
;      Modified to work with 24-bit color PostScript in IDL 7.1. 24 May 2009. DWF.
;      Added MULTIMARGIN keyword to allow position adjustment when plotting with 
;          !P.Multi. 7 July 2009. DWF.
;      Fixed a problem in which displaying an image with !P.MULTI turned on, switched the
;          color of the output window. If this happens to you, set the BACKGROUND keyword
;          to the color you want to have in the window. 4 January 2010. DWF.
;      Some LINUX distributions cannot both get the current color decomposition state and
;           set the state to another value on the same DEVICE command. I have changed all such 
;           occurances to two commands. One gets the current state, the other sets it. 11 Oct 2010. DWF.
;      Added SAVE keyword. 30 Oct 2010. DWF.
;      If the AXES keyword is set, but no MARGIN or POSITION keyword is set, and the command
;           is not doing a multiplot, then a Margin of 0.1 is used so image axes are shown.
;           30 Oct 2010. DWF.
;      Removed TVSCALE_ERROR routine in favor of ERROR_MESSAGE, since other Coyote Library
;           routines are already used in the program. 1 Nov 2010. DWF.
;       The SAVE keyword now always establishes a data coordinate system for the image
;           if this keyword is set, using the values of XRANGE and YRANGE. The data 
;           coordinate system is coincident with the file position of the image. 11 Nov 2010. DWF.
;       Added the WHITE keyword. 12 Nov 2010. DWF.
;       Modified how the ERASE keyword works. Now images only erase the background when this
;            keyword is set and !P.MULTI[0] is set to 0. 12 Nov 2010. DWF.
;       Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;       Made changes that supports the BACKGROUND color in PostScript. Requires the program
;           PS_BACKGROUND from the Coyote Library. 17 November 2010. DWF.
;       If the BACKGROUND color is set, then ERASEIT=1 automatically. 17 November 2010. DWF.
;       BACKGROUND color changes affected multi-plots. Fixed 18 Nov 2010. DWF.
;       BACKGROUND color changes affected display in indexed color. Fixed 18 Nov 2010. DWF.
;       Removed TVIMAGE_CONGRID in favor of FSC_RESIZE_IMAGE, which always does the interpolation
;            with centered pixels, and allows nearest neightbor resampling of true-color images.
;            20 November 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO TVSCALE, image, x, y, $
   ACOLOR=acolor, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BOTTOM=bottom, $
   BREWER=brewer, $ ; Obsolete and not used
   ERASE=eraseit, $
   HALF_HALF=half_half, $ ; Obsolete and not used
   KEEP_ASPECT_RATIO=keep, $
   MARGIN=margin, $
   MAXValue=max, $
   MINUS_ONE=minusOne, $
   MINValue=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOINTERPOLATION=nointerp, $
   NORMAL=normal, $
   POSITION=position, $
   OVERPLOT=overplot, $
   QUIET=quiet, $
   SAVE=save, $
   TOP=top, $
   TVSCL=tvscl, $
   WHITE=white, $
   XRANGE=plotxrange, $
   YRANGE=plotyrange, $
   _EXTRA=extra

   ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       RETURN
    ENDIF
    
     ; Set up a common block as input to TVINFO.
     COMMON FSC_$TVIMAGE, _tvimage_xsize, _tvimage_ysize, $
                          _tvimage_winxsize, _tvimage_winysize, $
                          _tvimage_position, _tvimage_winID, $
                          _tvimage_current
    
    ; Which release of IDL is this?    
    thisRelease = Float(!Version.Release)
    
    ; If the background color is specified, then ERASEIT should be automatically set.
    IF N_Elements(background) NE 0 THEN eraseit = 1

    ; Doing multiple plots?   
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    
    ; Check for image and keyword parameters.
    IF N_Elements(image) EQ 0 THEN MESSAGE, 'You must pass a valid image argument.'
    IF Keyword_Set(axis) THEN axes = 1
    axes = Keyword_Set(axes)
    
    ; Axes automatically sets SAVE, unless told not to.
    IF axes THEN IF N_Elements(save) EQ 0 THEN save = 1

    ; If axes are set and MARGIN and POSITION are NOT set and you are NOT
    ; doing multiplots, then set a normal "plot" margin.
    IF Keyword_Set(axes) AND ((N_Elements(margin) EQ 0) AND (N_Elements(position) EQ 0) $
        AND (multi EQ 0)) THEN margin = 0.075
    
    interp = 1.0 - Keyword_Set(nointerp)
    minusOne = Keyword_Set(minusOne)

    ; Check the drawing colors for background and axes.
    IF Keyword_Set(white) THEN BEGIN
       IF N_Elements(acolor) EQ 0 THEN acolor = 'black'
       background = 'white'
       eraseit = 1
    ENDIF
    IF N_Elements(background) EQ 0 THEN background = !P.Background
    IF Size(background, /TNAME) EQ 'STRING' THEN BEGIN
        IF StrUpCase(background) EQ 'WHITE' THEN BEGIN
           IF N_Elements(acolor) EQ 0 THEN acolor = 'black 
        ENDIF 
    ENDIF
    IF N_Elements(acolor) EQ 0 THEN acolor = !P.Color
    noerase = Keyword_Set(noerase) ; Don't change, used in PS output.

    ; Before you do anything, get the current color table vectors
    ; so they can be restored later.
    TVLCT, rr, gg, bb, /Get
    
    ; Do you need to erase the window before image display?
    IF Keyword_Set(eraseit) AND (!P.MULTI[0] EQ 0) THEN BEGIN
         IF (!D.Flags AND 256) NE 0 THEN BEGIN
            FSC_Erase, background
         ENDIF ELSE BEGIN
            IF (!D.NAME EQ 'Z') THEN BEGIN
                FSC_Erase, background
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
    ENDIF
    
    ; Make sure the multimargin has four elements.
    IF N_Elements(multimargin) EQ 0 THEN multimargin = [0., 0., 0., 0.] 
    IF N_Elements(multimargin) EQ 1 THEN multimargin = [multimargin, multimargin, multimargin, multimargin]
    IF N_Elements(multimargin) NE 4 THEN Message, 'The keyword MULTIMARGIN must be a four-element array.'
    
    ; Check image size..
    s = Size(image)
    IF s[0] LT 2 OR s[0] GT 3 THEN $
       MESSAGE, 'Argument does not appear to be an image. Returning...'
    alphaImage = 0
    
    ; Allow 24-bit images and 2D images that are sent in as 3D
    ; arrays where one dimension is a 1. 
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
    
    ; Prepare an alpha image, if needed.
    IF alphaImage THEN BEGIN
       index = Where(Size(image,/DIMENSIONS) EQ 4)
       CASE index OF
            0: aImage = Transpose(image, [1,2,0])
            1: aImage = Transpose(image, [0,2,1])
            ELSE: aImage = image
       ENDCASE
       
       ; Separate the alpha channel.
       alpha_channel = aImage[*,*,3]
                    
       ; Some alpha channels are screwy. Just ignore those.
       IF MIN(alpha_channel) EQ MAX(alpha_channel) THEN BEGIN
             outimage = aImage[*,*,0:2]
       ENDIF ELSE BEGIN
             alpha_channel = Scale_Vector(alpha_channel, 0.0, 1.0)
             foregndImage = aImage[*,*,0:2]
             ss = Size(foregndImage, /DIMENSIONS)
             backgndImage = BytArr(ss[0], ss[1], 3) + 255B
             alpha = Rebin(alpha_channel, ss[0], ss[1], ss[2])
             outImage = foregndImage*alpha + (1 - alpha)*backgndImage
       ENDELSE
    ENDIF

   ; Check for TOP and BOTTOM keywords.
   IF N_Elements(top) EQ 0 THEN top = !D.Table_Size
   IF N_Elements(bottom) EQ 0 THEN bottom = 0B
   IF N_Elements(ncolors) NE 0 THEN top = (bottom + ncolors - 1) < 255
   IF N_Elements(max) EQ 0 THEN max = Max(image)
   IF N_Elements(min) EQ 0 THEN min = Min(image)
    
    
    ; If a window is not open, open one, otherwise in X devices you get incorrect
    ; window size information the first time you call TVIMAGE.
    IF (!D.FLAGS AND 256) NE 0 THEN IF !D.Window EQ -1 THEN Window
    
    ; Check for position and overplot keywords.    
    IF N_Elements(position) EQ 0 THEN BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1) THEN BEGIN
           IF Size(background, /TNAME) EQ 'STRING' THEN background = FSC_Color(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
             XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], NOERASE=tempNoErase
          TVLCT, rr, gg, bb
          position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
      ENDIF ELSE BEGIN
         IF Keyword_Set(overplot) THEN BEGIN
            position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
         ENDIF ELSE position = [0.0, 0.0, 1.0, 1.0]
      ENDELSE
    ENDIF ELSE BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1)THEN BEGIN
    
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = FSC_Color(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
              XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], NOERASE=tempNoErase
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
    
    ; Check for TVSCL keyword. If present, then act like a TVSCL command.
    IF Keyword_Set(tvscl) THEN BEGIN
    
       IF N_Params() GE 3 OR N_Params() EQ 1 THEN BEGIN
         IF N_Elements(x) EQ 0 THEN x = 0
         IF N_Elements(y) EQ 0 THEN y = 0
         IF Keyword_Set(normal) THEN BEGIN
            IF alphaImage THEN BEGIN
               TV, BytScl(outImage, Top=!D.Table_Size-1, Max=max, Min=min), x, y, True=3, _STRICT_EXTRA=extra, /Normal
            ENDIF ELSE BEGIN
               TV, BytScl(image, Top=!D.Table_Size-1, Max=max, Min=min), x, y, True=true, _STRICT_EXTRA=extra, /Normal
            ENDELSE
         ENDIF ELSE BEGIN
            IF alphaImage THEN BEGIN
               TV, BytScl(outImage, Top=!D.Table_Size-1, Max=max, Min=min), x, y, True=3, _STRICT_EXTRA=extra, /Device
            ENDIF ELSE BEGIN
               TV, BytScl(image, Top=!D.Table_Size-1, Max=max, Min=min), x, y, True=true, _STRICT_EXTRA=extra, /Device
            ENDELSE
         ENDELSE
       ENDIF ELSE BEGIN
            IF Keyword_Set(normal) THEN BEGIN
                IF alphaImage THEN BEGIN
                   TV, BytScl(outImage, Top=!D.Table_Size-1, Max=max, Min=min), x, True=3, _STRICT_EXTRA=extra, /Normal
                ENDIF ELSE BEGIN
                   TV, BytScl(image, Top=!D.Table_Size-1, Max=max, Min=min), x, True=true, _STRICT_EXTRA=extra, /Normal
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                   TV, BytScl(outImage, Top=!D.Table_Size-1, Max=max, Min=min), x, True=3, _STRICT_EXTRA=extra, /Device
                ENDIF ELSE BEGIN
                   TV, BytScl(image, Top=!D.Table_Size-1, Max=max, Min=min), x, True=true, _STRICT_EXTRA=extra, /Device
                ENDELSE
             ENDELSE
       ENDELSE
       GoTo, restoreDecomposed
    
    ENDIF
    
    ; Maintain aspect ratio (ratio of height to width)?
    IF KEYWORD_SET(keep) THEN BEGIN
    
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
    
    ; Calculate the image size and start locations.
    xsize = Ceil((position[2] - position[0]) * !D.X_VSIZE)
    ysize = Ceil((position[3] - position[1]) * !D.Y_VSIZE)
    xstart = Round(position[0] * !D.X_VSIZE)
    ystart = Round(position[1] * !D.Y_VSIZE)
    
    ; Display the image. Sizing different for scalable pixels devices
    IF (!D.Flags AND 1) NE 0 THEN BEGIN
    
       ; Need a gray-scale color table if this is a true
       ; color image.
       IF true GT 0 THEN LOADCT, 0, /Silent
       IF alphaImage THEN BEGIN
           TV, BytScl(outImage, Top=(top-bottom), Max=max, Min=min) + bottom, xstart, $
              ystart, XSIZE=xsize, YSIZE=ysize, _STRICT_EXTRA=extra, True=3
       ENDIF ELSE BEGIN
           TV, BytScl(image, Top=(top-bottom), Max=max, Min=min) + bottom, xstart, $
              ystart, XSIZE=xsize, YSIZE=ysize, _STRICT_EXTRA=extra, True=true
       ENDELSE
    ENDIF ELSE BEGIN ; All other devices.
    
       CASE true OF
    
          0: TV, BYTSCL( FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra
    
          1: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(FSC_Resize_Image(outImage, CEIL(xsize), CEIL(ysize),INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=1
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 1, r, g, b, _EXTRA=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne), ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=0
             ENDELSE
    
          2: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(FSC_Resize_Image(outImage, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=2
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 2, r, g, b, _Extra=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne), ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=0
             ENDELSE
    
          3: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(FSC_Resize_Image(outImage, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(FSC_Resize_Image(image, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 3, r, g, b, _Extra=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, FSC_Resize_Image(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne), ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=0
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
    ; Only if the QUIET keyword is not set.
    IF ~Keyword_Set(quiet) THEN BEGIN
        IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
            _tvimage_xsize = imgXsize
            _tvimage_ysize = imgYsize
            _tvimage_winID = !D.Window
            _tvimage_winxsize = !D.X_Size
            _tvimage_winysize = !D.Y_Size
            _tvimage_position = position
            _tvimage_current = 1
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
    
        ; If axes color is a string, convert it.
        IF Size(acolor, /TNAME) EQ 'STRING' THEN acolor = FSC_COLOR(acolor)    
        PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
            XSTYLE=1, YSTYLE=1, POSITION=position, COLOR=acolor, _STRICT_EXTRA=axkeywords
            
    ENDIF ELSE BEGIN
    
        ; If you are saving the data coordinate space, draw invisible axes.
        IF Keyword_Set(save) THEN BEGIN
            PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
                XSTYLE=5, YSTYLE=5, POSITION=position, _STRICT_EXTRA=axkeywords
        ENDIF
    
    ENDELSE

    ; Clean up after yourself.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
    IF ~Keyword_Set(save) THEN BEGIN
        !P = bangp
        !X = bangx
        !Y = bangy
    ENDIF


END