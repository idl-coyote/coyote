;+
; NAME:
;     TVSCALE
;
; PURPOSE:
;     This purpose of TVSCALE is to enable the TVSCL command in IDL
;     to be a completely device-independent and color-decomposition-
;     state independent command. On 24-bit displays color decomposition
;     is always turned off for 8-bit images and on for 24-bit images.
;     The color decomposition state is restored for those versions of
;     IDL that support it (> 5.2). Moreover, TVSCALE adds features
;     that TVSCL lacks. For example, images can be positioned in windows
;     using the POSITION keyword like other IDL graphics commands.
;     TVSCALE also supports the !P.MULTI system variable, unlike the
;     TVSCL command. TVSCALE was written to work especially well in
;     resizeable graphics windows. Note that if you wish to preserve
;     the aspect ratio of images in resizeable windows, you should set
;     the KEEP_ASPECT_RATIO keyword, described below. TVSCALE works
;     equally well on the display, in the PostScript device, and in
;     the Printer and Z-Graphics Buffer devices. The TRUE keyword is
;     set automatically to the correct value for 24-bit images, so you
;     don't need to specify it when using TVSCALE. In addition, you can
;     use the TOP and BOTTOM keywords to define a particular set of
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
;     AXES:     Set this keyword to draw a set of axes around the image.
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
;               used to place the *first* plot. Can be a string (e.g., 'ivory'), or
;               a 24-bit value that can be decomposed into a color, or an 8-bit
;               index number into the current color table. ERASE and BACKGROUND
;               should only be used on 24-bit devices that support windows!
;
;     BOTTOM:   The image is scaled so that all displayed pixels have values
;               greater than or equal to BOTTOM and less than or equal to TOP.
;               The value of BOTTOM is 0 by default.
;
;     ERASE:    If this keyword is set an ERASE command is issued
;               before the image is displayed. ERASE and BACKGROUND 
;               should only be used on 24-bit devices that support windows! 
;               The keyword is ignored on 8-bit devices or devices that do
;               not support windows.
;
;     _EXTRA:   This keyword picks up any TV keywords you wish to use.
;
;     HALF_HALF: If set, will tell CONGRID to extrapolate a *half* row
;               and column on either side, rather than the default of
;               one full row/column at the ends of the array.  If you
;               are interpolating images with few rows, then the
;               output will be more consistent with this technique.
;               This keyword is intended as a replacement for
;               MINUS_ONE, and both keywords probably should not be
;               used in the same call to CONGRID.
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
;     MINUS_ONE: The value of this keyword is passed along to the CONGRID
;               command. It prevents CONGRID from adding an extra row and
;               column to the resulting array, which can be a problem with
;               small image arrays.
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
;     Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 2
;     TVSCALE, image, Position=p
;     Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 3
;     TVSCALE, image, Position=p
;     Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
;     p = [0.02, 0.3, 0.98, 0.98]
;     LoadCT, 5
;     TVSCALE, image, Position=p
;     Colorbar, Position=[p[0], p[1]-0.1, p[2], p[1]-0.05]
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
;-
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
;
; NAME:
;  TVSCALE_CONGRID
;
; PURPOSE:
;       Shrink or expand the size of an array by an arbitrary amount.
;       This IDL procedure simulates the action of the VAX/VMS
;       CONGRID/CONGRIDI function.
;
;  This function is similar to "REBIN" in that it can resize a
;       one, two, or three dimensional array.   "REBIN", however,
;       requires that the new array size must be an integer multiple
;       of the original size.   CONGRID will resize an array to any
;       arbitrary size (REBIN is somewhat faster, however).
;       REBIN averages multiple points when shrinking an array,
;       while CONGRID just resamples the array.
;
; CATEGORY:
;       Array Manipulation.
;
; CALLING SEQUENCE:
;  array = CONGRID(array, x, y, z)
;
; INPUTS:
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
; OPTIONAL INPUTS:
;       y:      The new Y dimension of the resized array.   If the original
;               array has only 1 dimension then y is ignored.   If the
;               original array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array.   If the original
;               array has only 1 or 2 dimensions then z is ignored.   If the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORD PARAMETERS:
;       INTERP: If set, causes linear interpolation to be used.
;               Otherwise, the nearest-neighbor method is used.
;
;       CUBIC:  If set, uses "Cubic convolution" interpolation.  A more
;               accurate, but more time-consuming, form of interpolation.
;               CUBIC has no effect when used with 3 dimensional arrays.
;
;       MINUS_ONE:
;               If set, will prevent CONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   For example,
;               If the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               If MINUS_ONE is present (AND IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and
;               (j-1)/(y-1).
;
;       HALF_HALF:
;               If set, will tell CONGRID to extrapolate a *half* row
;               and column on either side, rather than the default of
;               one full row/column at the ends of the array.  If you
;               are interpolating images with few rows, then the
;               output will be more consistent with this technique.
;               This keyword is intended as a replacement for
;               MINUS_ONE, and both keywords probably should not be
;               used in the same call to CONGRID.
;
; OUTPUTS:
;  The returned array has the same number of dimensions as the original
;       array and is of the same data type.   The returned array will have
;       the dimensions (x), (x, y), or (x, y, z) depending on how many
;       dimensions the input array had.
;
; PROCEDURE:
;       IF the input array has three dimensions, or if INTERP is set,
;       then the IDL interpolate function is used to interpolate the
;       data values.
;       If the input array has two dimensions, and INTERP is NOT set,
;       then the IDL POLY_2D function is used for nearest neighbor sampling.
;       If the input array has one dimension, and INTERP is NOT set,
;       then nearest neighbor sampling is used.
;
; EXAMPLE:
;       ; vol is a 3-D array with the dimensions (80, 100, 57)
;       ; Resize vol to be a (90, 90, 80) array
;       vol = CONGRID(vol, 90, 90, 80)
;
; MODIFICATION HISTORY:
;       DMS, Sept. 1988.
;       DMS, Added the MINUS_ONE keyword, Sept. 1992.
;  Daniel Carr. Re-wrote to handle one and three dimensional arrays
;                    using INTERPOLATE function.
;  DMS, RSI, Nov, 1993.  Added CUBIC keyword.
;       Craig Markwardt, Dec, 1997.  Added halfhalf keyword to
;                        more evenly distribute "dead" pixel row
;       Use uniformly spaced grid points for half_half W. Landsman   Feb. 2000
;              (and slightly modified by C. Markwardt 14 Feb 2000)
;  DWF, FSC, 22 Apr 2007. Modified the program so that 3D arrays use nearest-neighbor
;       interpolation unless the INTERP keyword is set.
;  DWF, FSC, 22 Apr 2007. Function renamed from CMCONGRID to TVSCALE_CONGRID on
;       recommendation of Craig Markwardt as he wants no part of this. :-)


FUNCTION TVSCALE_CONGRID, arr, x, y, z, Interp=int, Minus_One=m1, Cubic = cubic, $
                    Half_Half=hh

ON_ERROR, 2    ;Return to caller if error
s = Size(arr)

IF ((s[0] EQ 0) OR (s[0] GT 3)) THEN $
   Message, 'Array must have 1, 2, or 3 dimensions.'

;  Supply defaults = no interpolate, and no minus_one.
if n_elements(int) le 0 then int = 0 else int = keyword_set(int)
if n_elements(m1) le 0 then m1 = 0 else m1 = keyword_set(m1)

; Compute offsets pixel offsets for half_half
halfx = 0.0 & halfy = 0.0 & halfz = 0.0
if keyword_set(hh) then begin
    if s[0] GE 1 then halfx = -0.5 + (float(s[1])/x)
    if s[0] GE 2 then halfy = -0.5 + (float(s[2])/y)
    if s[0] GE 3 then halfz = -0.5 + (float(s[3])/z)
endif
cub = KEYWORD_SET(cubic)
if cub THEN int = 1  ;Cubic implies interpolate


CASE s[0] OF
   1: BEGIN          ; *** ONE DIMENSIONAL ARRAY
   srx = float(s[1] - m1)/(x-m1) * findgen(x) + halfx
      IF int THEN $
         RETURN, INTERPOLATE(arr, srx, CUBIC = cub) ELSE $
         RETURN, arr(ROUND(srx))
   ENDCASE
   2: BEGIN ; *** TWO DIMENSIONAL ARRAY
   IF int THEN BEGIN
     srx = float(s[1] - m1) / (x-m1) * findgen(x) + halfx
     sry = float(s[2] - m1) / (y-m1) * findgen(y) + halfy
          RETURN, INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cub)
   ENDIF ELSE $
     RETURN, POLY_2D(arr, $
      [[0,0],[(s[1]-m1)/float(x-m1),0]], $ ;Use poly_2d
      [[0,(s[2]-m1)/float(y-m1)],[0,0]],int,x,y)

   ENDCASE
   3: BEGIN ; *** THREE DIMENSIONAL ARRAY
   srx = float(s[1] - m1) / (x-m1) * findgen(x) + halfx
   sry = float(s[2] - m1) / (y-m1) * findgen(y) + halfy
   srz = float(s[3] - m1) / (z-m1) * findgen(z) + halfz
   IF int THEN BEGIN
      RETURN, interpolate(arr, srx, sry, srz, /grid)
   ENDIF ELSE BEGIN
      RETURN, interpolate(arr, round(srx), round(sry), round(srz), /grid)
   ENDELSE
   ENDCASE
ENDCASE

RETURN, arr_r
END
;--------------------------------------------------------------------------


FUNCTION TVSCALE_ERROR, theMessage, Traceback=traceback, NoName=noName, _Extra=extra

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage, _Extra=extra)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
   Print, ''
ENDIF

RETURN, answer
END ;--------------------------------------------------------------------------------------------------


PRO TVSCALE, image, x, y, $
   ACOLOR=acolor, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BOTTOM=bottom, $
   BREWER=brewer, $
   ERASE=eraseit, $
   HALF_HALF=half_half, $
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
   SCALE=scale, $
   TOP=top, $
   TVSCL=tvscl, $
   XRANGE=plotxrange, $
   YRANGE=plotyrange, $
   _EXTRA=extra
   
   
   

   ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = TVSCALE_ERROR(Traceback=1, /Error)
       RETURN
    ENDIF
    
     ; Set up a common block as input to TVINFO.
     COMMON FSC_$TVIMAGE, _tvimage_xsize, _tvimage_ysize, $
                          _tvimage_winxsize, _tvimage_winysize, $
                          _tvimage_position, _tvimage_winID, $
                          _tvimage_current
    
    ; Check for image and keyword parameters.
    IF N_Elements(image) EQ 0 THEN MESSAGE, 'You must pass a valid image argument.', /NoName
    IF Keyword_Set(axis) THEN axes = 1
    axes = Keyword_Set(axes)
    interp = 1.0 - Keyword_Set(nointerp)
    half_half = Keyword_Set(half_half)
    minusOne = Keyword_Set(minusOne)
    IF N_Elements(background) EQ 0 THEN background = !P.Background
    IF Keyword_Set(eraseit) THEN BEGIN
         IF (!D.Flags AND 256) NE 0 THEN BEGIN
            Device, Get_Visual_Depth=theDepth
            IF theDepth GE 24 THEN BEGIN
               varType = Size(background, /TNAME)
               CASE 1 OF
                  varType EQ 'BYTE' OR varType EQ 'INT': BEGIN
                     TVLCT, r, g, b, /GET
                     Erase, Color=background
                     TVLCT, r, g, b
                     END
                  varType EQ 'LONG': BEGIN
                     Device, Decomposed=1, Get_Decomposed=theState
                     Erase, Color=background
                     Device, Decomposed=theState
                     END
                  varType EQ 'STRING': BEGIN
                     Device, Decomposed=1, Get_Decomposed=theState
                     Erase, Color=FSC_Color(background)
                     Device, Decomposed=theState
                     END
                  ELSE: BEGIN
                     TVLCT, r, g, b, /GET
                     Erase, Color=background
                     TVLCT, r, g, b
                     END
               ENDCASE
            ENDIF
         ENDIF ELSE BEGIN
            IF (!D.NAME EQ 'Z') THEN Erase
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
       MESSAGE, 'Argument does not appear to be an image. Returning...', /NoName
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
             MESSAGE, 'Argument does not appear to be a 24-bit image. Returning...', /NoName
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
    
    ; Which release of IDL is this?    
    thisRelease = Float(!Version.Release)
    
    ; Doing multiple plots?   
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    
    ; Check for position and overplot keywords.    
    IF N_Elements(position) EQ 0 THEN BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1) THEN BEGIN
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
             XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]]
          position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
      ENDIF ELSE BEGIN
         IF Keyword_Set(overplot) THEN BEGIN
            position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
         ENDIF ELSE position = [0.0, 0.0, 1.0, 1.0]
      ENDELSE
    ENDIF ELSE BEGIN
       IF Keyword_Set(multi) AND (Keyword_Set(overplot) NE 1)THEN BEGIN
    
          ; Draw the invisible plot to get plot position.
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, Background=background, $
              XMargin=multimargin[[1,3]], YMargin=multimargin[[0,2]]
    
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
    
          0: TV, BYTSCL( TVSCALE_CONGRID(image, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra
    
          1: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(outImage, CEIL(xsize), CEIL(ysize), 3, INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(image, 3, CEIL(xsize), CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=1
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 1, r, g, b, _EXTRA=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, TVSCALE_CONGRID(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne, HALF_HALF=half_half), ROUND(xstart), $
                   ROUND(ystart), _STRICT_EXTRA=extra, True=0
             ENDELSE
    
          2: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(outImage, CEIL(xsize), CEIL(ysize), 3, INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(image, CEIL(xsize), 3, CEIL(ysize), INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=2
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 2, r, g, b, _Extra=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, TVSCALE_CONGRID(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne, HALF_HALF=half_half), ROUND(xstart), $
                   ROUND(ystart), _STRICT_EXTRA=extra, True=0
             ENDELSE
    
          3: IF thisDepth GT 8 THEN BEGIN
                IF alphaImage THEN BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(outImage, CEIL(xsize), CEIL(ysize), 3, INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDIF ELSE BEGIN
                    TV, BYTSCL(TVSCALE_CONGRID(image, CEIL(xsize), CEIL(ysize), 3, INTERP=interp, $
                       MINUS_ONE=minusOne, HALF_HALF=half_half), Top=top-bottom, Max=max, Min=min) + bottom, $
                       ROUND(xstart), ROUND(ystart), _STRICT_EXTRA=extra, True=3
                ENDELSE
             ENDIF ELSE BEGIN
                IF alphaImage THEN BEGIN
                    image2d = Color_Quan(outImage, 3, r, g, b, _Extra=extra)
                ENDIF ELSE BEGIN
                    image2d = Color_Quan(image, 3, r, g, b, _Extra=extra)
                ENDELSE
                TVLCT, r, g, b
                TV, TVSCALE_CONGRID(image2d, CEIL(xsize), CEIL(ysize), INTERP=0, $
                   MINUS_ONE=minusOne, HALF_HALF=half_half), ROUND(xstart), $
                   ROUND(ystart), _STRICT_EXTRA=extra, True=0
             ENDELSE
    
       ENDCASE
    ENDELSE
 
    ; Restore Decomposed state if necessary.
    RestoreDecomposed:
    
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

    ; If the user wanted axes, draw them now.
    IF axes THEN BEGIN
    
         ; Save plot system variables and current color table.
        bangp = !P
        bangx = !X
        bangy = !Y
        TVLCT, r, g, b, /GET
    
        ; If axes color is string, convert it.
        IF Size(acolor, /TNAME) EQ 'STRING' THEN acolor = FSC_COLOR(acolor, BREWER=brewer)    
    
        ; Need a data range?
        IF N_Elements(plotxrange) EQ 0 THEN plotxrange = [0, imgXsize]
        IF N_Elements(plotyrange) EQ 0 THEN plotyrange = [0, imgYsize]
        PLOT, [0], /NODATA, /NOERASE, XRANGE=plotxrange, YRANGE=plotyrange, $
            XSTYLE=1, YSTYLE=1, POSITION=position, COLOR=acolor, _STRICT_EXTRA=axkeywords
            
        ; Clean up after yourself.
        TVLCT, r, g, b
        !P = bangp
        !X = bangx
        !Y = bangy
    ENDIF
    

END