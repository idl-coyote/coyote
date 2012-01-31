;+
; NAME:
;   PLOTIMAGE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Displays an image via a "PLOT"-like interface.
;
; CALLING SEQUENCE:
;   PLOTIMAGE, img, [xrange=xrange,] [yrange=yrange,] ...
;
; DESCRIPTION:
;
;   PLOTIMAGE displays an image (or slice of an image) on the current
;   graphics device.  The syntax is very similar to the PLOT command,
;   in the sense that an XRANGE and YRANGE for the plot can be
;   specified.  
;
;   PLOTIMAGE keeps separate the notions of the image coordinate
;   system and the displayed coordinate system, which allows any input
;   image to be "cropped," "zoomed," or "flipped."
;
;   PLOTIMAGE allows the user to express image extents in physical
;   units rather than pixel units.
;
;   The image coordinate system specifies the physical coordinates of
;   original image data, IMG.  The image is considered to be a 2D
;   array (IMG = ARRAY(NX,NY)), where the values are attached to the
;   midpoint of each geometric pixel.  The image has NX columns and NY
;   rows.  Physical coordinates are attached to each pixel by using
;   the IMGXRANGE and IMGYRANGE keywords.  The IMGXRANGE keyword is a
;   two-element array specifying the "left" and "right" boundaries of
;   the image pixels in physical units; the IMGYRANGE keyword
;   specifies the "top" and "bottom" boundaries of the image.  This is
;   illustrated in Figure 1 for a simplified case.
;
;                                   ___
;         +-----------+-----------+  ^  IMGYRANGE[1]
;         |           |           |  |
;         | IMG[0,1]  | IMG[1,1]  |  |
;         |     +     |     +     |  |
;         |           |           |  |
;         |           |           |  |
;         +-----------+-----------+  |
;         |           |           |  |
;         | IMG[0,0]  | IMG[1,0]  |  |
;         |     +     |     +     |  |
;         |           |           |  |
;         |           |           |  v
;         +-----------+-----------+ ___ IMGYRANGE[0]
;        |                         |
;        |<----------------------->|
;        IMGXRANGE[0]   IMGXRANGE[1]
;
;      Figure 1.  Simplified example of a 2x2 input image,
;      demonstrating that IMG[*,*] values refer to the pixel
;      mid-points, and that IMGXRANGE and IMGYRANGE ranges specify the
;      physical coordinates of the outer edges of the image extent in
;      X and Y, respectively.
; 
;
;   The displayed plot coordinate system is entirely independent of
;   the native image coordinates.  Users can set up the plot scale
;   using any combination of {X,Y}RANGE, {X,Y}STYLE and/or {X,Y}LOG,
;   as they would for any IDL plot, using physical units.  The input
;   image will then be overlayed on this coordinate system.
;
;   If the displayed plot coordinates are narrower than the native
;   image coordinates, then the displayed portion of the image will be
;   cropped to fit.  If the displayed coordinates are wider than the
;   native image coordinates, then the image will be displayed with
;   blank spaces on either side (see Figure 2).  A mirror "flip" is
;   also possible in X and/or Y, if XRANGE or YRANGE are specified in
;   reverse order.
;                                                 ___
;      +---------------------------------------+   ^
;      |            ___                        |   |
;      |             ^  +---------------+      |   |
;      |             |  |               |      |   |
;      |             |  |               |      |   |
;      |    IMGYRANGE|  |     IMG       |      |   | YRANGE
;      |             |  |               |      |   |
;      |             v  |               |      |   |
;      |            ___ +---------------+      |   |
;      |               |<-- IMGXRANGE -->|     |   |
;      |                                       |   v
;      +---------------------------------------+  ___
;     |<-------------   XRANGE   -------------->|
;
;     Figure 2.  Example of an image whose native image coordinates
;     are embedded in a wider plot display range.
;
;   The standard [XY]STYLE keywords can be used to style either axis.
;   However at the very least [XY]STYLE=1 is always implied, i.e. the
;   plot limits exactly obey the [XY]RANGE keyword values.
;
;   If XLOG or YLOG are set, then the image is assumed to be sampled
;   on a logarithmic grid, and logarithmic axes are displayed
;   accordingly.  PLOTIMAGE does not attempt to resample the image
;   from linear scale to logarithmic scale, or reverse.
;
;   Psuedocolor images may be of any type, but must rescaled to a byte
;   range by using the RANGE keyword.  By default the color range used
;   in the rescaling operation is 0 to !D.N_COLORS - 3B.  The extra
;   two color values are reserved for the background and default pen
;   colors.  This behavior can be adjusted by specifying the BOTTOM
;   and/or NCOLORS keywords.
;
;   Truecolor images must always be of type BYTE and one of their
;   dimensions must have 3 elements, corresponding to the three color
;   planes of the image.
;
;
; INPUTS:
;
;   IMG - Array to be displayed.  For single-plane images (i.e.,
;         pseudocolor), the image must be two dimensional and of any
;         real numeric type.  For images that are not of BYTE type,
;         the RANGE keyword must be supplied, and then PLOTIMAGE will
;         rescale the image values to a byte range.
;
;         An image declared as ARRAY(NX,NY) will be NX pixels in the
;         x-direction and NY pixels in the y-direction.  The image is
;         resampled to fill the desired display region (and optionally
;         smoothed).
;
;         For three-plane images (i.e., truecolor) the image must be
;         of type BYTE.  One of the dimensions of the array must have
;         three elements.  Hence it must be one of BYTARR(NX, NY, 3),
;         BYTARR(NX, 3, NY) or BYTARR(3, NX, NY).  The 3-element
;         dimension is recognized automatically.
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   IMGXRANGE, IMGYRANGE - Each is a two component vector that
;                          describes the X and Y position of the outer
;                          edges of the first and last pixels.
;                          Default: IMGXRANGE = [0,NX]
;                                   IMGYRANGE = [0,NY]
;
;   XRANGE, YRANGE - Each is a two component vector that specifies the
;                    X and Y plot ranges, respectively.  These values
;                    are not required to coincide with IMG[XY]RANGE.
;                    Default: XRANGE=IMGXRANGE
;                             YRANGE=IMGYRANGE
;
;   POSITION - Position of the inner plot window in the standard
;              graphics keyword format.  Overrides PANEL and SUBPANEL.
;
;   INTERP - if set, interpolate (smooth) the image before displaying.
;            This keyword applies to the screen displays.  For printed
;            images that are coarser than MIN_DPI, the image is
;            implicitly interpolated regardless of INTERP.
;
;   PRESERVE_ASPECT - if set, preserve the aspect ratio of the
;                     original image (in pixels).  The result will be
;                     the largest image that fits in the display
;                     region while maintaining approximately square
;                     pixels.  However, PIXEL_ASPECT_RATIO overrides
;                     PRESERVE_ASPECT.  The POSITION keyword will be
;                     reset upon output to the ultimate image
;                     position.
;                     DEFAULT: not set (image will fill POSITION rectangle)
;
;   PIXEL_ASPECT_RATIO - The ratio of width to height for each pixel.
;                        If specified, then the image will be scaled
;                        so that each pixel has the specified aspect
;                        ratio.  If not specified, then the image will
;                        be scaled independently in X and Y in order
;                        to fill the POSITION rectangle.  NOTE: If you
;                        want to change the overall image aspect
;                        ratio, then use the POSITION keyword.
;                  DEFAULT: undefined (image will fill POSITION rectangle)
;
;   MIN_DPI - if printing, the minimum dot-per-inch pixel resolution
;             for the resulting image.  Output images that would be
;             coarser than this value are resampled to have a
;             resolution of at least MIN_DPI, and smoothed.  Some
;             common resolutions are: screen, 90 dpi; dot matrix, 72
;             dpi; laser printer 300-600 dpi.  Note that large values
;             of MIN_DPI will produce very large output files.
;             Default: 0 (i.e., the output image will not be smoothed)
;
;   RANGE - a two element vector.  If the image is single plane (i.e.,
;           pseudocolor) the input image can be of any real numeric
;           type, and then must be rescaled into byte range with this
;           keyword.  In contrast, truecolor images must always be of
;           type BYTE.  Values are scaled into byte range with the
;           following statement:
;              RESULT = BYTSCL(INPUT, MIN=RANGE(0), MAX=RANGE(1), $
;                              TOP=NCOLORS-1) + BOTTOM
;           so that pixels with an intensity RANGE(0) are set to
;           BOTTOM; those with RANGE(1) are set to the maximum color.
;           Default: no range scaling occurs (and the image must hence
;                    be of type BYTE -- otherwise an error occurs)
;
;   NCOLORS - number of color table values be used in the byte
;             rescaling operation.
;             Default: !D.N_COLORS - BOTTOM - 1 (for default pen color)
;
;   BOTTOM - bottom-most value of the color table to be used in the
;            byte rescaling operation.
;            Default: 1 (for default background color)
;
;   NOERASE - If set, the display is not erased before graphics
;             operations.
;
;   NODATA - If set, the image is not actually displayed, but
;            coordinate axes may be drawn.
;
;   NOAXES - An attempt is made to render the image without coordinate
;            axes.  However, it's usually more straightforward to set 
;            XSTYLE=4 or YSTYLE=4, which is the standard IDL way to
;            disable coordinate axes.
;
;   ORDER - same interpretation as the !ORDER system variable; 
;           if ORDER=0, then the first pixel is drawn in the lower
;           left corner; if ORDER=1, then the first pixel is drawn in
;           the upper left corner.
;           Default: 0
;
;
;   PANEL, SUBPANEL - An alternate way to more precisely specify the
;                     plot and annotation positions.  See SUBCELL.
;
;   PLOTIMAGE will pass other keywords directly to the PLOT command
;   used for generating the plot axes.  XSTYLE=1 and YSTYLE=1 are
;   enforced.
;
; OUTPUTS:
;   NONE
;
; PROCEDURE:
;
; EXAMPLE:
;
;   This example constructs an image whose values are found by
;       z(x,y) = cos(x) * sin(y)
;   and x and y are in the range [-2,2] and [4,8], respectively.
;   The image is then plotted, with appropriate axes.
;
;   x = findgen(20)/5. - 2. + .1   ; 0.1 = half-pixel
;   y = findgen(20)/5. + 4. + .1
;   zz = cos(x) # sin(y)
;   imgxrange = [-2.,2.]           ; extend to pixel edges
;   imgyrange = [4.,8.]
;   plotimage, bytscl(zz), imgxrange=imgxrange, imgyrange=imgyrange
;
;   This second example plots the same image, but with a plot range
;   much larger than the image's.
;
;   xr=[-10.,10]
;   yr=[-10.,10]
;   plotimage, bytscl(zz), imgxrange=imgxrange, imgyrange=imgyrange, $
;      xrange=xr, yrange=yr
;
; SEE ALSO:
;
;   OPLOTIMAGE, BYTSCL
;
; EXTERNAL SUBROUTINES:
;
;   SUBCELL, DEFSUBCELL
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Correct various one-off problems, 02 Feb 1999, CM
;   Made self-contained with some pre-processing, 17 Oct 1999, CM
;   Corrected bug in newly introduced CONGRID functions, 18 Oct 1999, CM
;   Correct behavior with no POSITION keyword, 17 Nov 1999, CM
;   Simplified axis plotting, 17 Nov 1999, CM
;   Use _EXTRA keyword in first PLOT, but with blank TITLEs, 11 Jan
;     2000, CM
;   Correct implementation of X/YSTYLE in first PLOT, 11 Feb 2000, CM
;   Correct CONGRID implementation (small effect when enlarging most
;     images), 14 Feb 2000, CM
;   Major changes: 19 Apr 2000
;      - now handle decomposed color, automatic color mapping via
;        RANGE, and 24-bit multiplane images
;      - new PRESERVE_ASPECT keyword to keep square pixels
;      - removed legacy TVIMAGE code
;      - smoothing is more configurable, esp. for printers, but is not
;        done by default; more printers are supported
;   Corrected INTERPOLATE behavior (thanks to Liam Gumley
;     <Liam.Gumley@ssec.wisc.edu>), other minor tweaks, CM 20 Apr 2000
;   Added ability to use PRESERVE_ASPECT with POSITION, PANEL or
;     SUBPANEL keywords CM 20 Oct 2000
;   Oops, a typo is now fixed, CM 23 Oct 2000
;   Add fix for MacIntoshes and DECOMPOSED color, Tupper, 02 Aug 2001
;   Better behavior with fractional pixels (ie, when the image pixels
;     are very large compared to the screen), 23 Aug 2001
;   Add support for Z buffer, CM, 20 Oct 2002
;   Memory conservation: use REVERSE() to reverse IMG; rewrote
;     PLOTIMAGE_RESAMP to rescale entire image instead of each color plane
;     separately.  Jeff Guerber, 30 July 2003
;   Add PIXEL_ASPECT_RATIO keyword, 22-25 Nov 2005
;   Check for the case of an 1xNXxNY 3D image and treat it as a 2D
;     image.  The "1" dimension can be anywhere, CM, 03 Sep 2006
;   Add the ORDER keyword parameter, CM, 20 Mar 2007
;   Enable XLOG and YLOG keywords, for logarithmic axes;
;     doesn't actually resample the image from linear<->log, CM
;     21 Jan 2009
;   Documentation, CM, 21 Jan 2009
;   Allow reverse color scale, CM, 13 Nov 2010
;
;   $Id: plotimage.pro,v 1.15 2010/11/13 09:53:39 cmarkwar Exp $
;
;-
; Copyright (C) 1997-2001,2003,2005,2006,2007,2009,2010 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
;%insert HERE
;%include subcell.pro
;%include defsubcell.pro

; Utility routine to resample an image
;
;  IMAGE - image data ARRAY(NX,NY,BDEPTH)
;  NX,NY - original X,Y image size
;  BDEPTH- original image depth
;  NEWX, NEWY- desired X,Y image size
;  INTERP - if set, then use bilinear interpolation, otherwise nearest neighbor
function plotimage_resamp, image, nx, ny, bdepth, newx, newy, interp=interp

  ;; Sometimes the final dimension is lost.  Put it back
  image = reform(image, nx, ny, bdepth, /overwrite)

  ;; Correct interpolation
  srx = float(nx)/newx * findgen(newx) - 0.5 + 0.5*(float(nx)/newx)
  sry = float(ny)/newy * findgen(newy) - 0.5 + 0.5*(float(ny)/newy)
  srz = indgen(bdepth)
  if keyword_set(interp) then $
    return, interpolate(image, srx, sry, srz, /grid)

  ;; Simple nearest neighbor interpolation
  return, interpolate(image, round(srx), round(sry), srz, /grid)
end

pro plotimage_pos, xrange0, imgxrange0, imgxsize, xreverse, srcxpix, imgxpanel, $
                   logscale=logscale, $
                   quiet=quiet, status=status, pixtolerance=pixtolerance

  if keyword_set(logscale) then begin
     if min(xrange0) LE 0 OR min(imgxrange0) LE 0 then $
        message, ('ERROR: if XLOG or YLOG is set, then the image boundary cannot '+$
                  'cross or touch zero.  Did you forget to set IMGXRANGE or IMGYRANGE?')
     xrange    = alog10(xrange0)
     imgxrange = alog10(imgxrange0)
  endif else begin
     xrange    = xrange0
     imgxrange = imgxrange0
  endelse

  if n_elements(pixtolerance) EQ 0 then pixtolerance = 1.e-2
  status = 0
  ;; Decide if image must be reversed
  xreverse = 0
  if double(xrange(1)-xrange(0))*(imgxrange(1)-imgxrange(0)) LT 0 then begin
      xreverse = 1
      imgxrange = [imgxrange(1), imgxrange(0)]
  endif

  srcxpix  = [ 0L, imgxsize-1 ]
  ;; Size of one x pix
  dx = double(imgxrange(1) - imgxrange(0)) / imgxsize

  if min(xrange) GE max(imgxrange) OR max(xrange) LE min(imgxrange) then begin
      message, 'WARNING: No image data in specified plot RANGE.', /info, $
        noprint=keyword_set(quiet)
      return
  endif

  ;; Case where xrange cuts off image at left
  if (xrange(0) - imgxrange(0))/dx GT 0 then begin
      offset = double(xrange(0)-imgxrange(0))/dx
      if abs(offset-round(offset)) LT pixtolerance then $
        offset = round(offset)
      srcxpix(0) = floor(offset)
      froffset = offset - floor(offset)
      if abs(froffset) GT pixtolerance then begin
          xrange = double(xrange)
          xrange(0) = imgxrange(0) +dx*srcxpix(0)
      endif
  endif

  ;; Case where xrange cuts off image at right
  if (xrange(1) - imgxrange(1))/dx LT 0 then begin
      offset = double(xrange(1)-imgxrange(0))/dx
      if abs(offset-round(offset)) LT pixtolerance then $
        offset = round(offset)
      srcxpix(1) = ceil(offset) - 1
      froffset = offset - ceil(offset)
      if abs(froffset) GT pixtolerance then begin
          xrange = double(xrange)
          srcxpix(1) = srcxpix(1) < (imgxsize-1)
          xrange(1) = imgxrange(0) + dx*(srcxpix(1)+1)
      endif
  endif

  imgxpanel = [0., 1.]
  if (xrange(0) - imgxrange(0))/dx LT 0 then $
    imgxpanel(0) = (imgxrange(0) - xrange(0))/(xrange(1)-xrange(0))
  if (xrange(1) - imgxrange(1))/dx GT 0 then $
    imgxpanel(1) = (imgxrange(1) - xrange(0))/(xrange(1)-xrange(0))

  status = 1
  return
end

;; Main program
pro plotimage, img0, xrange=xrange0, yrange=yrange0, $
               imgxrange=imgxrange0, imgyrange=imgyrange0, $
               xlog=xlog, ylog=ylog, $
               position=position, panel=panel, subpanel=subpanel, $
               xstyle=xstyle, ystyle=ystyle, title=title, $
               interp=interp0, quiet=quiet, dither=dither, $
               preserve_aspect=paspect, pixel_aspect_ratio=asprat, $
               min_dpi=min_dpi, order=order, $
               ncolors=ncolors0, bottom=bottom0, range=range, $
               noerase=noerase0, nodata=nodata, noaxes=noaxes, $
               pixtolerance=pixtolerance, _EXTRA=extra

  ;; Return to user when an error is encountered
  on_error, 2

  ;; Usage message
  if n_params() EQ 0 then begin
      message, 'PLOTIMAGE, image, xrange=, yrange=, imgxrange=, imgyrange=,..', $
               /info
      return
  endif

  ;; Must have a byte-scaled image already
  imgsize  = size(img0)

  ;; Make sure windowing exists (borrowed from IMDISP)
  if ((!d.flags and 256) ne 0) and (!d.window lt 0) then begin
      window, /free, /pixmap
      wdelete, !d.window
  endif

  ;; Parameter checking
  if n_elements(ystyle) EQ 0 then ystyle = 0L
  if n_elements(xstyle) EQ 0 then xstyle = 0L
  if keyword_set(nodata) then mynodata = 1 else mynodata = 0
  if n_elements(pixtolerance) EQ 0 then pixtolerance = 1.e-2
  if n_elements(title) EQ 0 then title = ''
  if n_elements(min_dpi) EQ 0 then min_dpi = 0
  interp = keyword_set(interp0)
  noerase = keyword_set(noerase0)
  imgpanel = [0., 0., 1., 1.]

  ;; Default handling of color table stuff
  if n_elements(bottom0) EQ 0 then bottom0 = 1B
  bottom = byte(bottom0(0)) < 255B
  dncolors = min([!d.n_colors, !d.table_size, 256])
  if n_elements(ncolors0) EQ 0 then ncolors0 = dncolors - 1 - bottom
  ;; Make sure color table values are in bounds
  ncolors = floor(ncolors0(0)) < 256
  if bottom + ncolors GT 256 then ncolors = 256 - bottom

  ;; Image size and dimensions
  nimgdims  = imgsize(0)
  imgtype   = imgsize(nimgdims+1)
  if nimgdims LT 2 OR nimgdims GT 3 then begin
      message, 'ERROR: image must have 2 or 3 dimensions'
  endif

  if nimgdims EQ 2 then begin
      ;; Two dimensional image is pseudo color
      img = img0

      ONE_CHANNEL_IMAGE:
      imgxsize = imgsize(1)
      imgysize = imgsize(2)
      bdepth = 1

      if imgtype NE 1 then begin
          if n_elements(range) LT 2 then $
            message, 'ERROR: non-byte image must be scaled with RANGE keyword'
          if range(0) LE range(1) then begin
              img = bytscl(img, min=range(0), max=range(1), top=ncolors-1B) $
                + bottom
          endif else begin
              ;; Reverse color scheme
              img = bytscl(img, min=range(1), max=range(0), top=ncolors-1B)
              img = ncolors-1B-img + bottom
          endelse
      endif
      img = reform(img, imgxsize, imgysize, bdepth, /overwrite)
  endif else begin
      wh = where(imgsize(1:3) EQ 1, ct)
      if ct GT 0 then begin
          imgxsize = 1
          imgysize = 1
          j = 0
          for i = 1, 3 do if imgsize(i) NE 1 then begin
              if j EQ 0 then imgxsize = imgsize(i) else imgysize = imgsize(i)
              j = j + 1
          endif
          img = reform(img0, imgxsize, imgysize)
          imgsize = size(img)

          goto, ONE_CHANNEL_IMAGE
      endif else begin
          ;; Three dimensional image has three planes
          wh = where(imgsize(1:3) EQ 3, ct)
          if imgtype NE 1 then $
            message, 'ERROR: true color image must of type byte'
          if ct EQ 0 then $
            message, ('ERROR: True color image must have 3 elements '+$
                      'in one of its dimensions')
          truedim = wh(0)
          
          ;; Shuffle the data so planes are interleaved ...
          case truedim of
              0: img = transpose(img0, [1,2,0]) ;; ... from pixels interleaved
              1: img = transpose(img0, [0,2,1]) ;; ... from rows interleaved
              2: img = img0                 ;; ... by straight copying
          end

          imgsize = size(img)
          imgxsize = imgsize(1)
          imgysize = imgsize(2)
          bdepth = imgsize(3)

      endelse

  endelse

  ;; By default, we have no info about the image, and display the
  ;; whole thing
  if n_elements(imgxrange0) LT 2 then imgxrange = [ 0., imgxsize ] $
  else imgxrange = 0. + imgxrange0(0:1)
  if n_elements(xrange0) LT 2 then xrange = imgxrange $
  else xrange = 0. + xrange0(0:1)

  status = 0
  plotimage_pos, xrange, imgxrange, imgxsize, xreverse, srcxpix, imgxpanel, $
    quiet=keyword_set(quiet), status=status, pixtolerance=pixtolerance, $
    logscale=xlog
  if status EQ 0 then mynodata = 1 $
  else imgpanel([0,2]) = imgxpanel

  ;; By default, we have no info about the image, and display the
  ;; whole thing
  if n_elements(imgyrange0) LT 2 then imgyrange = [ 0., imgysize ] $
  else imgyrange = 0. + imgyrange0(0:1)
  if n_elements(yrange0) LT 2 then yrange = imgyrange $
  else yrange = 0. + yrange0(0:1)
  if keyword_set(order) then yrange = [yrange(1), yrange(0)]

  status = 0
  plotimage_pos, yrange, imgyrange, imgysize, yreverse, srcypix, imgypanel, $
    quiet=keyword_set(quiet), status=status, pixtolerance=pixtolerance, $
    logscale=ylog
  if status EQ 0 then mynodata = 1 $
  else imgpanel([1,3]) = imgypanel

  ;; Dimensions of output image in pixels
  nx = srcxpix(1)-srcxpix(0)+1
  ny = srcypix(1)-srcypix(0)+1

  ;; Create a coordinate system by plotting with no data or axes
  if n_elements(position) EQ 0 AND n_elements(panel) EQ 0 AND $
    n_elements(subpanel) EQ 0 then begin

      ;; If PANEL/SUBPANEL is not given, then plot once to set up
      ;; axes, despite NOAXES
      plot, xrange, yrange, noerase=noerase, /nodata, $
        xstyle=xstyle OR 5, ystyle=xstyle OR 5, xlog=xlog, ylog=ylog, $
        xrange=xrange, yrange=yrange, xtitle='', ytitle='', title='', $
        _EXTRA=extra

      ;; Retrieve axis settings
      xwindow = !x.window
      ywindow = !y.window

      subpanel1 = [xwindow(0), ywindow(0), xwindow(1), ywindow(1)]
      imgposition = subcell(imgpanel, subpanel1)
      position = subpanel1

  endif else begin

      ;; Construct the plot size from panel info.  Default is full-screen
      if NOT keyword_set(noerase) then erase
      if n_elements(position) GE 4 then begin
          imgposition = subcell(imgpanel, position)
      endif else begin
          if n_elements(panel) LT 4 then panel = [0.0,0.0,1.0,1.0]
          if n_elements(subpanel) LT 4 then subpanel = [-1., -1, -1, -1]
          subpanel = defsubcell(subpanel)

          imgposition = subcell(subcell(imgpanel, subpanel), panel)
          position = subcell(subpanel, panel)
      endelse

      xwindow = position([0,2])
      ywindow = position([1,3])

  endelse

  ;; If the aspect is to be preserved then we need to recompute the
  ;; position after considering the image size.  Since we have already
  ;; computed the outer envelope of the image from either the POSITION
  ;; or PANEL, or from the plot window itself, we can now go to the
  ;; logic which estimates the aspect-corrected size.

  if (keyword_set(paspect) OR n_elements(asprat) GT 0) AND $
    nx GT 0 AND ny GT 0 then begin

      if n_elements(asprat) EQ 0 then asprat1 = 1.0 $
      else asprat1 = asprat(0) + 0.

      ;; If we are preserving the aspect, then re-plot after scaling
      ;; the POSITION

      imgaspect = float(ny)/float(nx)/asprat1
      dispaspect = (ywindow(1)-ywindow(0))*!d.y_vsize $
        / ((xwindow(1)-xwindow(0))*!d.x_vsize)

      ;; Compute the new image dimensions
      if imgaspect GT dispaspect then begin
          x0 = total(xwindow)/2
          dx = (ywindow(1)-ywindow(0))*!d.y_vsize/(imgaspect*!d.x_vsize)
          xwindow = x0 + dx*[-0.5,0.5]
      endif else begin
          y0 = total(ywindow)/2
          dy = (xwindow(1)-xwindow(0))*!d.x_vsize*imgaspect/!d.y_vsize
          ywindow = y0 + dy*[-0.5,0.5]
      endelse

      subpanel1 = [xwindow(0), ywindow(0), xwindow(1), ywindow(1)]
      imgposition = subcell(imgpanel, subpanel1)
      position = subpanel1

      ;; Replot to regain coordinate system
      plot, xrange, yrange, /noerase, /nodata, $
        xstyle=xstyle OR 5, ystyle=xstyle OR 5, xlog=xlog, ylog=ylog, $
        xrange=xrange, yrange=yrange, xtitle='', ytitle='', title='', $
        position=position, _EXTRA=extra

  endif

  ;; Draw the image data
  if NOT keyword_set(mynodata) then begin

      ;; Reverse X- or Y- directions if necessary
      if xreverse then $
        srcxpix = imgxsize - 1 - [srcxpix(1), srcxpix(0)]
      if yreverse then $
        srcypix = imgysize - 1 - [srcypix(1), srcypix(0)]

      ;; Extract relevant image elements
      img = (temporary(img))(srcxpix(0):srcxpix(1), srcypix(0):srcypix(1),*)
      img = reform(img, nx, ny, bdepth, /overwrite)

      ;; Complete the extraction, if reversed
      if xreverse then begin
          img = reverse(img, 1, /overwrite)
          img = reform(img, nx, ny, bdepth, /overwrite)
      endif
      if yreverse then begin
          img = reverse(img, 2, /overwrite)
          img = reform(img, nx, ny, bdepth, /overwrite)
      endif

      ;; Compute the image position on screen in pixels
      x0 = round(imgposition(0) * !d.x_vsize)
      y0 = round(imgposition(1) * !d.y_vsize)
      dx = round((imgposition(2) - imgposition(0)) * !d.x_vsize) > 1
      dy = round((imgposition(3) - imgposition(1)) * !d.y_vsize) > 1

      ;; Decide which output type
      windowing = (!d.name EQ 'WIN') OR (!d.name EQ 'MAC') OR (!d.name EQ 'X')
      printing = (!d.name EQ 'PRINTER') OR (!d.flags AND 1) NE 0

      ;; Decide whether to resample the image
      rescaling = (windowing OR (!d.name EQ 'Z')) $
        AND ((dx NE nx) OR (dy NE ny))

      ;; If printing, and the printed resolution of the image will be
      ;; too coarse, then we should resample and interpolate
      dpi = min([nx*!d.x_px_cm/dx, ny*!d.y_px_cm/dy]*2.54) ; d.p.i. of image
      dxsize = dx & dysize = dy
      if printing AND (dpi LT min_dpi(0)) then begin
          dx = round(min_dpi(0)*dx/(2.54*!d.x_px_cm)) > nx
          dy = round(min_dpi(0)*dy/(2.54*!d.y_px_cm)) > ny
          interp = 1
          rescaling = 1
      endif

      ;; Rescale the image if needed
      if rescaling then begin
          img = plotimage_resamp(temporary(img), nx, ny, bdepth, $
            dx, dy, interp=interp)
          img = reform(img, dx, dy, bdepth, /overwrite)
      endif

      ;; Generic printer device
      if !d.name EQ 'PRINTER' then begin
          if bdepth EQ 3 then begin
              device, /true_color
              tv, img, x0, y0, xsize=dxsize, ysize=dysize, true=3
          endif else begin
              device, /index_color
              tv, img, x0, y0, xsize=dxsize, ysize=dysize
          endelse
          goto, DONE_IMG
      endif

      ;; Devices with scalable pixels
      if (!d.flags AND 1) NE 0 then begin
          if bdepth EQ 3 then begin
              tvlct, r, g, b, /get
              loadct, 0, /silent
              tv, img, x0, y0, xsize=dxsize, ysize=dysize, true=3
              tvlct, r, g, b
          endif else begin
              tv, img, x0, y0, xsize=dxsize, ysize=dysize
          endelse
          goto, DONE_IMG
      endif

      ;; Get visual depth (in bytes) and decomposed state
      decomposed0 = 0
      vdepth = 1
      version = float(!version.release)
      if windowing then begin

          ;; Visual depth
          if version GE 5.1 then begin
              device, get_visual_depth=vdepth
              vdepth = vdepth / 8
          endif else begin
              if !d.n_colors GT 256 then vdepth = 3
          endelse

          ;; Decomposed state
          if vdepth GT 1 then begin
              if version GE 5.2 then device, get_decomposed=decomposed0
              if bdepth EQ 3 then    device, decomposed=1 $
              else                   device, decomposed=0
          endif
      endif

      ;; If visual is 8-bit but image is 24-bit, then quantize
      if vdepth LE 1 AND bdepth EQ 3 then begin
          img = color_quan(temporary(img), 3, r, g, b, colors=ncolors-1, $
                           dither=keyword_set(dither)) + bottom
          tvlct, r, g, b, bottom
          bdepth = 1
      endif

      ;; Put the image
      if bdepth EQ 3 then tv, img, x0, y0, true=3 $
      else                tv, img, x0, y0

      ;; Restore the decomposed state
      if windowing then begin
          if vdepth GT 1 then device, decomposed=decomposed0
          ;; Tupper supplies following work-around for MacIntoshes
          if (!d.name EQ 'MAC') then tv, [0], -1, -1
      endif
  endif

  ;; Plot the axes if requested
  DONE_IMG:
  if NOT keyword_set(noaxes) then begin
      if n_elements(xrange) EQ 0 then begin
          if n_elements(imgxrange) GT 1 then xrange=imgxrange $
          else xrange = [0L, imgxsize]
      endif
      if n_elements(yrange) EQ 0 then begin
          if n_elements(imgyrange) GT 1 then yrange=imgyrange $
          else yrange = [0L, imgysize]
      endif

      plot, xrange, yrange, /noerase, /nodata, /normal, $
        xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
        xstyle=xstyle OR 1, ystyle=ystyle OR 1, title=title, $
        position=position, _EXTRA=extra
  endif

  return
end

