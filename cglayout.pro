; docformat = 'rst'
;
; NAME:
;   cgLayout
;
; PURPOSE:
;   The purpose of this program is to return the normalized position coordinates for 
;   a line plot, contour plot, or image plot with a specific "layout" in the current
;   graphics window. A "layout" has a specified grid of columns and rows organized 
;   inside a graphics display window. This is similar to the positions calculated by 
;   !P.Multi, although a great deal more flexible. 
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this program is to return the normalized position coordinates for 
; a line plot, contour plot, or image plot with a specific "layout" in the current
; graphics window. A "layout" has a specified grid of columns and rows organized 
; inside a graphics display window. This is similar to the positions calculated by 
; !P.Multi, although a great deal more flexible. Of course, with additional flexibility 
; and power comes more responsibility. You will be required to use the NoErase keyword
; correctly and you will be responsible for setting the character size on your plots.
; These jobs are normally handled by !P.Multi. Some users will find this liberating, some 
; will find it a pain in the keister. There is always !P.Multi to go back to.
; 
; A grid position is a combination of the number of columns and rows desired, plus
; the application of inside and outside margins, as well as a desired aspect ratio.
; Margins are specified in character size units. One unit in the X direction is
; given as !D.X_CH_SIZE pixels. One unit in the Y direction is given as !D.Y_CH_SIZE
; pixels. The outside margins locate the grid inside the graphics display window.
; (These are equivalent to !X.OMargin and !Y.OMargin system variable when displaying
; a grid with !P.Multi, for example.) Inside margins use the same units, but are
; used to modify the initial grid positions in the window. (These are equivalent
; to using the XMargin and YMargin keywords on a plot command.) For example, inside margins
; might be used to leave room inside a larger position for color bars or other annotations
; you wish to put on a graphics display. The aspect ratio modifies the grid position
; after the outside and inside margins have been applied to create the final grid position,
; which will be centered in its initial grid position.
;
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;    This function returns the normalized position coordinates for a particular
;    location in a grid of locations (if the layout parameter is a three-element array),
;    or a 4-by-(ncols*nrows) array of grid positions with one position for each location
;    in the ncols by nrows grid of locations (if the layout parameter is a two-element array).
;    The return variable is suitable for passing to the POSITION keyword of an IDL graphics
;    command (e.g., cgPlot, cgContour, cgImage, etc.). The grid positions are organized in
;    row order, starting with the grid location in the upper-left of the graphihcs display window.
;    
; :Params:
; 
;    layout: in, required, type=integer
;         This parameter sets up the grid layout for the current graphics window. A grid
;         is organized by columns and rows, with the first grid position located in the upper-left
;         corner of the current graphics display window and proceeding in row order. This parameter
;         is either a two-element vector, giving the number of columns and number of rows,
;         respectively (e.g., [ncols,nrows]), or it is a three-element vector, giving, in addition, the
;         specific grid location for which a position is required (e.g, [ncols, nrows, gridPosition]).
;         Grid positions start at 1 with the first grid in the upper left corner of the graphics display 
;         window and proceed in row order, sequentually, until the last grid position, which is equal 
;         to the number of columns times the number of rows.
;       
; :Keywords:
; 
;    aspect: in, optional, type=float
;         This kewyord allows you to specify a specific aspect ratio for the return
;         positions. The aspect ratio is calculated as YDimension/XDimension. So, for
;         example, if you wish the positions to be twice as wide as they are high, you
;         would set the Aspect keyword to 1.0/2.0 or 0.5. If you wish your positions to
;         have a square aspect ratio, you would set the Aspect keyword to 1.0.
;         
;    ixmargin: in, optional, type=integer
;         This keyword is a two-element vector that sets the right and left, respectively, inside
;         X margin for the grid position. Units are multiples of !D.X_CH_SIZE. Default = [0,0].
;         
;    iymargin: in, optional, type=integer
;         This keyword is a two-element vector that sets the bottom and top, respectively, inside
;         Y margin for the grid position. Units are multiples of !D.Y_CH_SIZE. Default = [0,0].
;         
;    oxmargin: in, optional, type=integer
;         This keyword is a two-element vector that sets the right and left, respectively, inside
;         X margin for the grid position. The default OXMargins are suitable for displaying line
;         plots. If you are displaying image plots, you may wish to make the OXMargins the same on
;         both sides of the graphics display window (e.g, OXMargin=[5,5]). Units are multiples of 
;         !D.X_CH_SIZE. Default = [10,4].
;         
;    oymargin: in, optional, type=integer
;         This keyword is a two-element vector that sets the bottom and top, respectively, inside
;         Y margin for the grid position. The default OYMargins are suitable for displaying line
;         plots. There is a little additional room at the top of the plot in the defaults for adding 
;         a title to a multiple plot set-up. Units are multiples of !D.Y_CH_SIZE. Default = [6,8].
;         
;    xgap: in, optional, type=integer, default=14
;         This keywords sets the distance between plots in the X dimension. Units are multiples
;         of !D.X_CH_SIZE.
;         
;    ygap: in, optional, type=integer, default=8
;         This keywords sets the distance between plots in the Y dimension. Units are multiples
;         of !D.Y_CH_SIZE.
;    
; :Examples:
;    Here is how to use this program to display line plots::
;    
;        cgDisplay, WID=0
;        pos = cgLayout([2,2])
;        FOR j=0,3 DO BEGIN
;          cgPlot, cgDemoData(17), NoErase=j NE 0, Position=pos[*,j], Title='Plot ' + StrTrim(j+1,2)
;        ENDFOR
;        cgText, 0.5, 0.925, /Normal, 'Example Plot Layout', Alignment=0.5, Charsize=cgDefCharsize()*1.25
;       
;    Here is how to use this program to display contour plots or images with colorbars::
;    
;        cgDisplay, WID=1
;        cgLoadCT, 22, /Brewer, /Reverse
;        pos = cgLayout([2,2], OXMargin=[5,5], OYMargin=[5,12], XGap=3, YGap=10)
;        FOR j=0,3 DO BEGIN
;          p = pos[*,j]
;          cgImage, cgDemoData(18), NoErase=j NE 0, Position=p
;          cgColorBar, position=[p[0], p[3]+0.05, p[2], p[3]+0.1]
;        ENDFOR
;        cgText, 0.5, 0.925, /Normal, 'Example Image Layout', Alignment=0.5, Charsize=cgDefCharsize()*1.25
;        
;    .. image:: cglayout.png
;    
;    Here is how to display square plots in a PostScript file::
;    
;        PS_Start, 'cglayout_example.ps'
;        cgDisplay
;        pos = cgLayout([2,2], Aspect=1.0)
;        FOR j=0,3 DO BEGIN
;          cgPlot, cgDemoData(17), NoErase=j NE 0, Position=pos[*,j], Title='Plot ' + StrTrim(j+1,2)
;        ENDFOR
;        cgText, 0.5, 0.925, /Normal, 'Example Plot Layout', Alignment=0.5, Charsize=cgDefCharsize()*1.25
;        PS_End
;        
;    Here is how to draw the third plot in a 3 column by 2 row layout::
;    
;        cgDisplay, 800, 600, WID=3
;        cgPlot, cgDemoData(17), Position=cgLayout([3,2,3])
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
;    Change History::
;       Written, 19 December 2012 by David W. Fanning, from suggestions from Matthew Argall.
;
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
FUNCTION cgLayout, layout, $
   ASPECT=aspect, $
   IXMARGIN=ixMargin, $
   IYMARGIN=iyMargin, $
   OXMARGIN=oxMargin, $
   OYMARGIN=oyMargin, $
   XGAP=xgap, $
   YGAP=ygap
   
   Compile_Opt idl2
   
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = Error_Message()
      RETURN, -1
   ENDIF
   
   ; Check parameters.
   IF N_Elements(layout) EQ 0 THEN BEGIN
      Print, 'Syntax: pos = cgLocator([2,3,1])'
      RETURN, -1
   ENDIF
   IF N_Elements(layout) EQ 3 THEN layoutIndex = layout[2]-1
   
   ; Check keywords.
   area = Keyword_Set(area)
   IF N_Elements(ixmargin) EQ 0 THEN ixmargin = [0,0]
   IF N_Elements(iymargin) EQ 0 THEN iymargin = [0,0]
   IF N_Elements(oxmargin) EQ 0 THEN oxmargin = [10,4]
   IF N_Elements(oymargin) EQ 0 THEN oymargin = [6,8]
   IF N_Elements(xgap) EQ 0 THEN xgap = 14
   IF N_Elements(ygap) EQ 0 THEN ygap = 8
 
   ; Get the size of the window. If the current device supports windows,
   ; a window has to be open.
   IF ((!D.FLAGS AND 256) NE 0) AND !D.Window LT 0 THEN BEGIN
        createdWindow = 1
        Window, /Pixmap
   ENDIF ELSE createdWindow = 0
   xsize = Double(!D.X_Size)
   ysize = Double(!D.Y_Size)
   IF createdWindow THEN WDelete, !D.Window
   
   ; Number of columns and rows.
   IF N_Elements(layout) LT 2 THEN Message, 'Layout parameter must be a 2- or 3-element vector.'
   ncols = layout[0]
   nrows = layout[1]
   
   ; Set up the inside and outside margins and the gaps between positions.
   xomargin = oxmargin * !D.X_CH_SIZE
   yomargin = oymargin * !D.Y_CH_SIZE
   ximargin = ixmargin * !D.X_CH_SIZE / xsize
   yimargin = iymargin * !D.Y_CH_SIZE / ysize
   gapx = xgap * !D.X_CH_SIZE
   gapy = ygap * !D.Y_CH_SIZE
   
   ; Calculate the window or drawing area inside the graphics window.
   winarea = [ xomargin[0], yomargin[0], xsize - xomargin[1], ysize - yomargin[1] ]   
   
   ; Calculate the plot width and height.
   plot_width  = (winarea[2] - winarea[0] - gapx*(ncols-1)) / ncols
   plot_height = (winarea[3] - winarea[1] - gapy*(nrows-1)) / nrows
   
   ; Calculate the plot areas inside the drawing area.
   plot_areas = FltArr(4, ncols, nrows)
   FOR j=0,nrows-1 DO BEGIN
      FOR k=0,ncols-1 DO BEGIN
         plot_areas[0,k,j] = winarea[0] + (plot_width + gapx) * k  ; x0
         plot_areas[2,k,j] = plot_areas[0,k,j] + plot_width        ; x1
         plot_areas[1,k,j] = winarea[3] - (plot_height + gapy) * j ; y0
         plot_areas[3,k,j] = plot_areas[1,k,j] - plot_height       ; y1
      ENDFOR
   ENDFOR
   
   ; Normalize the plot areas.
   plot_areas[[0,2],*,*] = plot_areas[[0,2],*,*] / xsize
   plot_areas[[1,3],*,*] = plot_areas[[1,3],*,*] / ysize
   
   ; Calculate the plot positions. These are the plot areas with the
   ; inside margins subtracted.
   positions = FltArr(4, ncols, nrows)
   positions[0,*,*] = plot_areas[0,*,*] + ximargin[0]
   positions[2,*,*] = plot_areas[2,*,*] - ximargin[1]
   positions[3,*,*] = plot_areas[1,*,*] + yimargin[0]
   positions[1,*,*] = plot_areas[3,*,*] - yimargin[1]
   
   ; Reform the positions into a 4 by ncols*rows array.   
   positions = Reform(positions, 4, ncols*nrows)
   
   ; Did the user ask for an aspect ratio?
   IF N_Elements(aspect) NE 0 THEN BEGIN
   
      ; Make sure aspect is not 0.
      IF aspect[0] EQ 0 THEN Message, 'The aspect ratio cannot be zero.'
   
      ; Calculate the same aspect ratio for each of the positions.
      FOR j=0,ncols*nrows-1 DO BEGIN
      
          p = positions[*,j]
          xpixSize = (p[2] - p[0]) * xsize
          ypixSize = (p[3] - p[1]) * ysize
          ratio = aspect[0]
          
          ; Try to fit the width. If you can't maintain
          ; the aspect ratio, fit the height.
          trialX = xpixSize
          trialY = trialX * ratio
          IF trialY GT ypixSize THEN BEGIN
             trialY = ypixSize
             trialX = trialY / ratio
          ENDIF
          
          ; Recalculate the position of the plot in the window.
          p[0] = (((xpixSize - trialX) / 2.0) / xsize) + p[0]
          p[2] = p[0] + (trialX/Float(xsize))
          p[1] = (((ypixSize - trialY) / 2.0) / ysize)  + p[1]
          p[3] = p[1] + (trialY/Float(ysize))
          
          positions[*,j] = p
        ENDFOR
   ENDIF
   
   ; If you have a layout index use that to return a specific
   ; position. Otherwise, return all the positions calculated
   ; for the window.
   IF N_Elements(layoutIndex) EQ 0 THEN BEGIN
      RETURN, positions
   ENDIF ELSE BEGIN
      RETURN, positions[*,layoutIndex]
   ENDELSE
   
END      
      