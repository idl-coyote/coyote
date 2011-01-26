;+
; NAME:
;     TVIMAGE
;
; PURPOSE:
;     This purpose of TVIMAGE is to create a device-independent TV command
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
;       Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;     Graphics display.
;
; CALLING SEQUENCE:
;
;     TVIMAGE, image
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
;     ACOLOR:   This keyword has been depreciated in favor of the COLOR keyword.
;               
;     ALPHABACKGROUNDIMAGE: Normally, when a image with an alpha channel is displayed,
;               the image is blended with the image currently in the display window.
;               This means, the program has to obtain that background image. This is not a 
;               problem on devices (e.g., WIN, X, Z) that allow this kind of operation,
;               but it is on devices (e.g., the PostScript device, PS) that do not.
;               To get around this problem, you can pass the background image to the
;               TVImage program, along with the alpha channel image you wish to display
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
;               TVImage, image, /AXES, AXKEYWORDS={TICKLEN:-0.025}
;               
;               The axis color, range, title, font and character size must be 
;               set with TVIMAGE keywords ACOLOR, [XY]RANGE, [XY]TITLE, FONT, and
;               CHARSIZE.
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
;     BOTTOM:   IF SCALE=1, the image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of BOTTOM is 0 by default.
;
;     BREWER:   Obsolete and not used.
;     
;     COLOR:    Set this keyword to the axis color. If a byte or integer value,
;               it will assume you are using INDEXED color mode. If a long integer
;               is will assume you are using DECOMPOSED color mode. If a string,
;               is will pass the string color name along to FSC_COLOR for processing.
;     
;     CHARSIZE: Sets the character size. Used only if the AXES keyword is also set.
;
;     ERASE:    If this keyword is set an ERASE command is issued
;               before the image is displayed. 
;               
;     FONT:     Set this to the type of font wanted on axis annotation. By default
;               FONT = !P.Font. Used only if the AXES keyword is also set.
;
;     _EXTRA:   This keyword picks up any TV keywords you wish to use.
;
;     HALF_HALF: Obsolete and not used. Image resizing is always done
;               as if CONGRID was called with CENTER=1. This prevents
;               image pixels from changing locations in the output image.
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
;               used. It is also ignored when TVImage is executed in a
;               multi-plot window, EXCEPT if it's value is zero. In this
;               special case, the image will be drawn into its position in
;               the multi-plot window with no margins whatsoever. (The
;               default is to have a slight margin about the image to separate
;               it from other images or graphics.
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
;                  TVImage, image, MultiMargin=[0, 0, 4, 0]
;                  
;               This keyword applies *only* to images displayed with !P.Multi, and if
;               passed a scalar value, will use the same value for all four positions.
;               
;     MINUS_ONE: The value of this keyword is passed along to the FSC_RESIZE_IMAGE
;               command. It prevents FSC_RESIZE_IMAGE from adding an extra row and
;               column to the resulting array, which can be a problem with
;               small image arrays. This keyword is set to 1 by default.
;
;     NCOLORS:  If this keyword is supplied, the TOP keyword is ignored and
;               the TOP keyword is set equal to  NCOLORS - 1. This
;               keyword is provided to make TVIMAGE easier to use with the
;               color-loading programs such as LOADCT:
;
;                  LoadCT, 5, NColors=100, Bottom=100
;                  TVImage, image, NColors=100, Bottom=100
;                  
;               Setting this keyword to a value implies SCALE=1.
;
;     NOINTERPOLATION: Setting this keyword disables the default bilinear
;               interpolation done to the image when it is resized. Nearest
;               neighbor interpolation is done instead. This is preferred
;               when you do not wish to change the pixel values of the image.
;               This keyword must be set, for example, when you are displaying
;               GIF files that come with their own non-IDL color table vectors.
;
;     NORMAL:   Setting this keyword means image position coordinates x and y
;               are interpreted as being in normalized coordinates. This keyword
;               is only valid if the TV keyword is set.
;
;     OVERPLOT: Setting this keyword causes the POSITION keyword to be ignored
;               and the image is positioned in the location established by the
;               last graphics command. For example:
;
;                    Plot, Findgen(11), Position=[0.1, 0.3, 0.8, 0.95]
;                    TVImage, image, /Overplot
;
;     POSITION: The location of the image in the output window. This is
;               a four-element floating array of normalized coordinates of
;               the type given by !P.POSITION or the POSITION keyword to
;               other IDL graphics commands. The form is [x0, y0, x1, y1].
;               The default is [0.0, 0.0, 1.0, 1.0]. Note that this keyword is ALSO
;               an output keyword. That is to say, upon return from TVIMAGE
;               this keyword (if passed by reference) contains the actual
;               position in the window where the image was displayed. This
;               may be different from the input values if the KEEP_ASPECT_RATIO
;               keyword is set, or if you are using TVIMAGE with the POSITION
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
;               !P.MULTI. But in this special case, TVIMAGE will use the POSITION
;               coordinates to calculate an image position in the actual position
;               calculated for the image by !P.MULTI. The main purpose of this
;               functionality is to allow the user to display images along with
;               colorbars when using !P.MULTI. See the example below.
;
;    QUIET:      There are situations when you would prefer that TVIMAGE does not
;                advertise itself by filling out the FSC_$TVIMAGE common block. For
;                example, if you are using TVIMAGE to draw a color bar, it would
;                not be necessary. Setting this keyword means that TVIMAGE just
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
;    TV:        Setting this keyword makes the TVIMAGE command work much
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
;      WHITE:   A shorthand way of setting the ERASE keyword to 1 and the BACKGROUND
;               keyword to "white. Also sets the ACOLOR keyword to "black," unless
;               it is already set to something else.
;               
;      WINDOW:  Set this keyword to add the command to an FSC_Window application.
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
;     function. The TVIMAGE command will load the color table vectors
;     and set the NOINTERPOLATION keyword if this is done. Note that the
;     resulting color table vectors are normally incompatible with other
;     IDL-supplied color tables. Hence, other graphics windows open at
;     the time the image is display are likely to look strange.
;
;     Other programs from Coyote Library are required.
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
;        TVIMAGE, image, POSITION=thisPosition, /KEEP_ASPECT_RATIO
;        CONTOUR, image, POSITION=thisPosition, /NOERASE, XSTYLE=1, $
;            YSTYLE=1, XRANGE=[0,360], YRANGE=[0,360], NLEVELS=10
;
;     To display four images in a window without spacing between them:
;
;     !P.Multi=[0,2,2]
;     TVImage, image, Margin=0
;     TVImage, image, Margin=0
;     TVImage, image, Margin=0
;     TVImage, image, Margin=0
;     !P.Multi = 0
;
;     To display four image in a window with associated color bars:
;
;     !P.Multi=[0,2,2]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 0
;     TVImage, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 2
;     TVImage, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 3
;     TVImage, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 5
;     TVImage, image, Position=p
;     FSC_Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     !P.Multi =0
;
; MODIFICATION HISTORY:
;      Written by:     David Fanning, 20 NOV 1996.
;      Fixed a small bug with the resizing of the image. 17 Feb 1997. DWF.
;      Removed BOTTOM and NCOLORS keywords. This reflects my growing belief
;         that this program should act more like TV and less like a "color
;         aware" application. I leave "color awareness" to the program
;         using TVIMAGE. Added 24-bit image capability. 15 April 1997. DWF.
;      Fixed a small bug that prevented this program from working in the
;          Z-buffer. 17 April 1997. DWF.
;      Fixed a subtle bug that caused me to think I was going crazy!
;          Lession learned: Be sure you know the *current* graphics
;          window! 17 April 1997. DWF.
;      Added support for the PRINTER device. 25 June 1997. DWF.
;      Extensive modifications. 27 Oct 1997. DWF
;          1) Removed PRINTER support, which didn't work as expected.
;          2) Modified Keep_Aspect_Ratio code to work with POSITION keyword.
;          3) Added check for window-able devices (!D.Flags AND 256).
;          4) Modified PostScript color handling.
;      Craig Markwart points out that Congrid adds an extra row and column
;          onto an array. When viewing small images (e.g., 20x20) this can be
;          a problem. Added a Minus_One keyword whose value can be passed
;          along to the Congrid keyword of the same name. 28 Oct 1997. DWF
;      Changed default POSITION to fill entire window. 30 July 1998. DWF.
;      Made sure color decomposition is OFF for 2D images. 6 Aug 1998. DWF.
;      Added limited PRINTER portrait mode support. The correct aspect ratio
;          of the image is always maintained when outputting to the
;          PRINTER device and POSITION coordinates are ignored. 6 Aug 1998. DWF
;      Removed 6 August 98 fixes (Device, Decomposed=0) after realizing that
;          they interfere with operation in the Z-graphics buffer. 9 Oct 1998. DWF
;      Added a MARGIN keyword. 18 Oct 1998. DWF.
;      Re-established Device, Decomposed=0 keyword for devices that
;         support it. 18 Oct 1998. DWF.
;      Added support for the !P.Multi system variable. 3 March 99. DWF
;      Added DEVICE, DECOMPOSED=1 command for all 24-bit images. 2 April 99. DWF.
;      Added ability to preserve DECOMPOSED state for IDL 5.2 and higher. 4 April 99. DWF.
;      Added TV keyword to allow TVIMAGE to work like the TV command. 11 May 99. DWF.
;      Added the OVERPLOT keyword to allow plotting on POSITION coordinates
;         estabished by the preceding graphics command. 11 Oct 99. DWF.
;      Added automatic recognition of !P.Multi. Setting MULTI keyword is no
;         longer required. 18 Nov 99. DWF.
;      Added NOINTERPOLATION keyword so that nearest neighbor interpolation
;         is performed rather than bilinear. 3 Dec 99. DWF
;      Changed ON_ERROR condition from 1 to 2. 19 Dec 99. DWF.
;      Added Craig Markwardt's CMCongrid program and removed RSI's. 24 Feb 2000. DWF.
;      Added HALF_HALF keyword to support CMCONGRID. 24 Feb 2000. DWF.
;      Fixed a small problem with image start position by adding ROUND function. 19 March 2000. DWF.
;      Updated the PRINTER device code to take advantage of available keywords. 2 April 2000. DWF.
;      Reorganized the code to handle 24-bit images on 8-bit displays better. 2 April 2000. DWF.
;      Added BACKGROUND keyword. 20 April 2000. DWF.
;      Fixed a small problem in where the ERASE was occuring. 6 May 2000. DWF.
;      Rearranged the PLOT part of code to occur before decomposition state
;         is changed to fix Background color bug in multiple plots. 23 Sept 2000. DWF.
;      Removed MULTI keyword, which is no longer needed. 23 Sept 2000. DWF.
;      Fixed a small problem with handling images that are slices from 3D image cubes. 5 Oct 2000. DWF.
;      Added fix for brain-dead Macs from Ben Tupper that restores Macs ability to
;         display images. 8 June 2001. DWF.
;      Fixed small problem with multiple plots and map projections. 29 June 2003. DWF.
;      Converted all array subscripts to square brackets. 29 June 2003. DWF.
;      Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;      Small modification at suggestion of Karsten Rodenacker to increase size of
;         images in !P.MULTI mode. 8 December 2004. DWF.
;      Minor modifications on Karsten Rodenacker's own account concerning margination
;         and TV behaviour. 8 December 2004. KaRo
;      There was a small inconsistency in how the image was resized for PostScript as
;         opposed to the display, which could occasionally result in a small black line
;         to the right of the image. This is now handled consistently. 3 January 2007. DWF.
;      Made a small change to CMCONGRID to permit nearest-neighbor interpolation for 3D arrays.
;         Previously, any 24-bit image was interpolated, no matter the setting of the NOINTERP
;         keyword. 22 April 2007. DWF.
;      Updated the program for the 24-bit Z-buffer in IDL 6.4. 11 June 2007. DWF.
;      Added new POSITION keyword functionality for !P.MULTI display. 9 Sept 2007. DWF.
;      Bit one too many times. Added _STRICT_EXTRA keywords for all _EXTRA keywords. 1 Feb 2008. DWF.
;      Added FSC_$TVIMAGE common block for interactive interaction with TVINFO. 16 March 2008. DWF.
;      Added SCALE keyword. 18 March 2008. DWF.
;      Added keywords to allow axes to be drawn around the image. 18 March 2008. DWF.
;      Added QUIET keyword to allow by-passing of FSC_$TVIMAGE common block updating. 21 March 2008. DWF.
;      Made BACKGROUND and ERASE valid keywords only on 24-bit devices. Ignored on others. 28 May 2008. DWF.
;      Cannot make color work in device independent way for axes, unless I handle axis color directly. To this
;          end, I have added an ACOLOR keyword. 16 June 2008. DWF.
;      Added BREWER keyword so I can specify Brewer colors with BACKGROUND and ACOLOR keywords. 16 June 2008. DWF.
;      Fixed a problem with the BACKGROUND keyword and multiple plots. 16 March 2009. DWF.
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
;       Added the SAVE keyword to save the data coordinate system established by adding axes
;           to the image. 29 Oct 2010. DWF.
;       If the AXES keyword is set, but no MARGIN or POSITION keyword is set, and the command
;           is not doing a multiplot, then a Margin of 0.1 is used so image axes are shown.
;           30 Oct 2010. DWF.
;       Changed the way alpha channel blending works. The alpha channel image is now blended
;           with the background image in the display, if it can be obtained for a partical graphics
;           device, or a background image can be passed to the program via the AlphaBackgroundImage
;           keyword, or failing that, the alpha channel image is blended with a white background.
;           1 November 2010. DWF.
;       Removed TVIMAGE_ERROR routine in favor of ERROR_MESSAGE, since other Coyote Library
;           routines are already used in the program. 1 Nov 2010. DWF.
;       Forgot to set the SET_PIXEL_DEPTH keyword in Z-Buffer to 24. Screwed up alpha channel
;           display. 2 November 2010. DWF.
;       Small error in alpha channel processing when images are the same size. Reorganized
;           the code to avoid duplication. 5 November 2010. DWF.
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
;       Alpha images can only be produced in IDL 6.5 or higher. Issue warning. 17 November 2010. DWF.
;       BACKGROUND color changes affected multi-plots. Fixed 18 Nov 2010. DWF.
;       BACKGROUND color changes affected display in indexed color. Fixed 18 Nov 2010. DWF.
;       Removed TVIMAGE_CONGRID in favor of FSC_RESIZE_IMAGE, which always does the interpolation
;            with centered pixels, and allows nearest neightbor resampling of true-color images.
;            20 November 2010. DWF.
;       Incorporated TVSCALE functionality into TVIMAGE. 22 November 2010. DWF.
;       Problem fixed when displaying alpha image when POSITION and ALPHABACKGROUND keywords used
;            simultaneously. 8 Dec 2010. DWF.
;       More sophisticated selection of axis color. 5 Jan 2011. DWF.
;       The fix on 8 Dec 2010 was causing problems with positioning of normal alpha images.
;            I have now solved the original problem with a new ALPHABGPOSITION keyword, while
;            restoring functionality that was lost in the 8 Dec 2010 fix. 10 January 2011. DWF.
;        Added XTITLE and YTITLE keywords to add titles to image axes. 10 January 2011. DWF.
;        Added FONT, CHARSIZE, and TITLE keywords. 11 Jan 2011. DWF.
;        Depreciated ACOLOR keyword in favor of new COLOR keyword. 11 Jan 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2011, by Fanning Software Consulting, Inc.                           ;
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
FUNCTION TVIMAGE_PREPARE_ALPHA, image, position, alphaBackgroundImage, $
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
           alphaBackgroundImage = TVRead()
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
        TVImage, bImage, /NOINTERP
    ENDIF ELSE BEGIN
        TVImage, bImage, Position=alphapos, /NOINTERP
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



PRO TVIMAGE, image, x, y, $
   ACOLOR=acolorname, $
   ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
   ALPHABGPOSITION=alphapos, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BREWER=brewer, $ ; Obsolete and not used.
   BOTTOM=bottom, $
   COLOR=color, $
   ERASE=eraseit, $
   HALF_HALF=half_half, $ ; Obsolete and not used.
   KEEP_ASPECT_RATIO=keep, $
   MARGIN=margin, $
   MAXVALUE=max, $
   MINUS_ONE=minusOne, $
   MINVALUE=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOINTERPOLATION=nointerp, $
   NORMAL=normal, $
   POSITION=position, $
   OVERPLOT=overplot, $
   QUIET=quiet, $
   SAVE=save, $
   SCALE=scale, $
   TOP=top, $
   TV=tv, $
   WHITE=white, $
   WINDOW=window, $
   XRANGE=plotxrange, $
   XTITLE=plotxtitle, $
   YRANGE=plotyrange, $
   YTITLE=plotytitle, $
   _EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: TVIMAGE, image'
        RETURN
    ENDIF
    
    ; Set up a common block as input to TVINFO.
    COMMON FSC_$TVIMAGE, _tvimage_xsize, _tvimage_ysize, $
                         _tvimage_winxsize, _tvimage_winysize, $
                         _tvimage_position, _tvimage_winID, $
                         _tvimage_current
    
    ; Add the command to FSC_Window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        FSC_Window, 'TVImage', image, x, y, $
           ACOLOR=acolorname, $
           ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
           ALPHABGPOSITION=alphapos, $
           AXIS=axis, $
           AXES=axes, $
           AXKEYWORDS=axkeywords, $
           BACKGROUND=background, $
           BREWER=brewer, $ ; Obsolete and not used.
           BOTTOM=bottom, $
           COLOR=color, $
           ERASE=eraseit, $
           HALF_HALF=half_half, $ ; Obsolete and not used.
           KEEP_ASPECT_RATIO=keep, $
           MARGIN=margin, $
           MAXVALUE=max, $
           MINUS_ONE=minusOne, $
           MINVALUE=min, $
           MULTIMARGIN=multimargin, $
           NCOLORS=ncolors, $
           NOINTERPOLATION=nointerp, $
           NORMAL=normal, $
           POSITION=position, $
           OVERPLOT=overplot, $
           QUIET=quiet, $
           SAVE=save, $
           SCALE=scale, $
           TOP=top, $
           TV=tv, $
           WHITE=white, $
           XRANGE=plotxrange, $
           XTITLE=plotxtitle, $
           YRANGE=plotyrange, $
           YTITLE=plotytitle, $
           _EXTRA=extra
             RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Depreciated ACOLOR keyword in favor of COLOR keyword.
    IF N_Elements(acolorname) EQ 0 THEN IF N_Elements(color) NE 0 THEN acolorname = color
    
    ; Which release of IDL is this?
    thisRelease = Float(!Version.Release)
    
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = FSC_DefCharSize(FONT=font)
    
    ; If the background color is specified, then ERASEIT should be automatically set.
    IF N_Elements(background) NE 0 THEN eraseit = 1
    
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
    interp = 1.0 - Keyword_Set(nointerp)
    IF N_Elements(minusOne) EQ 0 THEN minusOne = 1
    minusOne = Keyword_Set(minusOne)
    
    ; Check the drawing colors for background and axes.
    IF Keyword_Set(white) THEN BEGIN
       IF N_Elements(acolorname) EQ 0 THEN acolorname = 'black'
       background = 'white'
       eraseit = 1
    ENDIF
    
    IF N_Elements(background) EQ 0 THEN background = !P.Background
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
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN acolorname = 'BLACK'
                    IF (Total(pixel) EQ 0) THEN acolorname = 'WHITE'
                    IF N_Elements(acolorname) EQ 0 THEN acolorname = 'OPPOSITE'
                ENDIF ELSE acolorname = 'OPPOSITE'
           ENDELSE
     ENDIF
    IF N_Elements(acolorname) EQ 0 THEN acolor = !P.Color ELSE acolor = acolorname
    IF Size(acolor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN acolor = Byte(color)
    IF Size(acolor, /TYPE) LE 2 THEN acolor = StrTrim(Fix(acolor),2)
 
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
    ; window size information the first time you call TVIMAGE.
    IF (!D.FLAGS AND 256) NE 0 THEN IF (!D.Window EQ -1) THEN Window
    
    ; Check for position and overplot keywords.
    IF N_Elements(position) EQ 0 THEN BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1) THEN BEGIN
          ; Draw the invisible plot to get plot position.
          IF Size(background, /TNAME) EQ 'STRING' THEN background = FSC_Color(background)
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
             XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]], NOERASE=tempNoErase
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
    
    ; Check for TV keyword. If present, then act like a TV command.
    IF Keyword_Set(tv) THEN BEGIN
    
       IF N_Params() GE 3 OR N_Params() EQ 1 THEN BEGIN
         IF N_Elements(x) EQ 0 THEN x = 0
         IF N_Elements(y) EQ 0 THEN y = 0
         IF Keyword_Set(normal) THEN BEGIN
            IF alphaImage THEN BEGIN
               outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
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
               outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
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
                   outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
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
                   outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage, $
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
    
    ; Display the image. Sizing different for scalable pixels devices.
    IF (!D.Flags AND 1) NE 0 THEN BEGIN
    
       ; Need a gray-scale color table if this is a true
       ; color image.
       IF true GT 0 THEN LOADCT, 0, /Silent
       IF alphaImage THEN BEGIN
           outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
                    outImage = TVImage_Prepare_Alpha(image, position, alphaBackgroundImage)
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
    
        FSC_PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
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
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
    IF ~Keyword_Set(save) THEN BEGIN
        !P = bangp
        !X = bangx
        !Y = bangy
    ENDIF

END
