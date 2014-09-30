; docformat = 'rst'
;
; NAME:
;   cgZPlot
;
; PURPOSE:
;   This program creates a "zoomable" line plot in an interactive window. The user can
;   zoom into or out of the plot. Once a plot is zoomed, the user can then pan the plot
;   in both the X and Y directions. See the operating instructions for how to interact
;   with the line plot.
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
;+
;  This program creates a "zoomable" line plot in an interactive window. The user can
;  zoom into or out of the plot. Once a plot is zoomed, the user can then pan the plot
;  in both the X and Y directions. See the operating instructions for how to interact
;  with the line plot.
;   
; .. image:: cgzplot.png
; 
;  Operating Instructions--
;  
;  Use the LEFT mouse button to zoom the plot and the RIGHT mouse button to pan the plot.
;  
;  If you click and drag inside the plot axes, you will create a rubber band box. Select the
;  portion of data you wish to zoom into. The zoom will occur in both the X and Y directions.
;  If you wish to zoom the plot all the way back out, simply click and release the LEFT mouse
;  button inside the plot axes without moving the mouse.
;  
;  Once you are zoomed into a plot, you can adjust the zoom by clicking the LEFT mouse button
;  outside the plot axes. If you click the mouse below the plot, you will cause the X axis to
;  zoom out of the plot by the zoomFactor amount (normally 5% of the current range of the axis).
;  If you wish to zoom the X axis into the plot, simply click above in the region of the window
;  above the plot. Click below the plot to zoom out, above the plot to zoom in. Similarly, you 
;  can adjust the zoom on the Y axis. Clicking to the left of the plot zooms the Y axis out, 
;  while clicking to the right of the plot zooms the Y axis in.
;  
;  If you are zoomed into the plot, you can pan to different locations in the plot by using
;  the RIGHT mouse button. Hold and drag the RIGHT mouse button inside the plot axes. The
;  entire plot will pan in both the X and Y directions.
;  
;  File output requires that ImageMagick and GhostScript be installed on your machine. Note
;  that exact axis scaling is always in effect.
;
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;         
; :Keywords:
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. 
;     err_clip: in, optional, type=boolean, default=1
;        Set this keyword to cause error bars to be clipped to the borders of the plot.
;        The default is to clip the error bars to the extent of the zoomed plot.
;     err_color: in, optional, type=varies
;        The color error bars should be drawn in. The default is to use the `Color` keyword.
;     err_thick:, in, optional, type=integer
;        The thickness of the line for drawing the error bars. By default, !P.Thick.
;     err_width: in, optional, type=float
;        The width of the end lines on error bars in normalized coordinates. By default, the
;        width is one percent of the width of the axes length in the appropriate dimension.
;     err_xhigh: in, optional
;         he high error values that should be added to the independent or X data values.
;     err_xlow: in, optional
;        The low error values that should be subtracted from the independent or X data values.
;     err_yhigh: in, optional
;        The high error values that should be added to the dependent or Y data values.
;     err_ylow: in, optional
;        The low error values that should be subtracted from the dependent or Y data values.
;     label: in, optional, type=string
;        A label is similar to a plot title, but it is aligned to the left edge
;        of the plot and is written in hardware fonts. Use of the label keyword
;        will suppress the plot title.
;     legends: in, optional, type=object
;        A single cgLegendItem object, or an array of cgLegendItem objects that will be
;        drawn on the plot as a legend.
;     max_value: in, optional, type=float
;        Set this keyword to the maximum value to plot. Any values greater than this 
;        value are treated as missing.
;     min_value: in, optional, type=float
;        Set this keyword to the minimu value to plot. Any values smaller than this 
;        value are treated as missing.
;     oplots: in, optional, type=object
;        A single cgOverPlot object, or an array of cgOverPlot objects that will be
;        overplot on the axes set up by the original data.
;     parent: in, optional, type=long
;        The identifer of the parent widget for this program's draw widget. If not
;        provided, the program will create it's own top-level base widget as a parent.
;     symcolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the symbol color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     xlog: in, optional, type=boolean, default=0
;        Set this keyword to use a logarithmic X axis
;     xrange: in, optional, type=double
;        Set this keyword to a two-element array giving the X data range of the plot.
;     xsize: in, optional, type=int, default=640
;        The X size of the program's draw widget.
;     ylog: in, optional, type=boolean, default=0
;        Set this keyword to use a logarithmic Y axis
;     ynozero: in, optional, type=boolean, default=0
;        Set this keyword to use allow the Y axis to start at a value other than zero.
;     yrange: in, optional, type=double
;        Set this keyword to a two-element array giving the Y data range of the plot.
;     ysize: in, optional, type=int, default=512
;        The Y size of the program's draw widget.
;     zoomfactor: in, optional, type=float
;        Set this keyword to a number between 0.01 and 0.25. This affects the amount
;        of zooming when the X axis and Y axis are zoomed with the LEFT mouse button.
;        The default value is 0.05 or five percent of the current axis range on each
;        end of the axis, resulting in a 10 percent change in the axis length.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is 
;        allowed in the program. Note that this is not the same as saying it is a good
;        idea to use every one of the these keywords. Use good judgment.
;        
; :Examples:
;    Use as you would use the cgPlot command::
;       cgZPlot, cgDemoData(1)
;       cgZPlot, cgDemoData(1), Aspect=1.0
;       cgZPlot, cgDemoData(1), Color='olive', AxisColor='red', Thick=2
;       cgZPlot, cgDemoData(1), Color='blue', SymColor='red', PSym=-16
;
;     Example using error bars::
;       data = Congrid(cgDemoData(1), 15)
;       seed = -5L
;       time = cgScaleVector(Findgen(N_Elements(data)), 1, 9)
;       high_yerror = RandomU(seed, N_Elements(data)) * 5 > 0.5
;       low_yerror = RandomU(seed, N_Elements(data)) * 4 > 0.25
;       high_xerror = RandomU(seed, N_Elements(data)) * 0.75 > 0.1
;       low_xerror = RandomU(seed, N_Elements(data))  * 0.75 > 0.1
;       xtitle = 'Time'
;       ytitle = 'Signal Strength'
;       title = 'Error Bar Plot'
;       position = [0.125, 0.125, 0.9, 0.925]
;       thick = (!D.Name EQ 'PS') ? 3 : 1
;       cgZPlot, time, data, Color='red5', PSym=-16, $
;           SymSize=1.0, Thick=thick, Title=title, XTitle=xtitle, YTitle=ytitle, $
;           Position=position, YStyle=1, $
;           ERR_XLow=low_xerror, ERR_XHigh=high_xerror, ERR_CLIP=1, $
;           ERR_YLow=low_yerror, ERR_YHigh=high_yerror, ERR_Color='blu5'
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
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Separated the object code (cgZPlot__Define) from this driver code for easier inheritance. 14 June 2012. DWF.
;        Added LABEL keyword. 12 July 2012. DWF.
;-
PRO cgZPlot, x, y, $
    ASPECT=aspect, $
    DRAWID=drawid, $
    ERR_CLIP=err_clip, $
    ERR_COLOR=err_color, $
    ERR_THICK=err_thick, $
    ERR_WIDTH=err_width, $
    ERR_XHIGH=err_xhigh, $
    ERR_XLOW=err_xlow, $
    ERR_YHIGH=err_yhigh, $
    ERR_YLOW=err_ylow, $
    LABEL=label, $
    LEGENDS=legends, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    OPLOTS=oplots, $
    PARENT=parent, $
    SYMCOLOR=symcolor, $
    XLOG=xlog, $
    XRANGE=xrange, $
    XSIZE=xsize, $
    YLOG=ylog, $
    YRANGE=yrange, $
    YNOZERO=ynozero, $
    YSIZE=ysize, $
    ZOOMFACTOR=zoomfactor, $
    _REF_EXTRA=extra

    Compile_Opt idl2
   
    ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       dep = x
       indep = Findgen(N_Elements(dep))
       ENDCASE
    
       2: BEGIN
       dep = y
       indep = x
       ENDCASE
       
       ELSE: BEGIN
           Print, 'Syntax: cgZPlot, x, y'
           RETURN
           END
    
    ENDCASE

    thisObject = Obj_New('cgZPlot', indep, dep, $
        ASPECT=aspect, $
        DRAWID=drawid, $
        ERR_CLIP=err_clip, $
        ERR_COLOR=err_color, $
        ERR_THICK=err_thick, $
        ERR_WIDTH=err_width, $
        ERR_XHIGH=err_xhigh, $
        ERR_XLOW=err_xlow, $
        ERR_YHIGH=err_yhigh, $
        ERR_YLOW=err_ylow, $
        LABEL=label, $
        LEGENDS=legends, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        OPLOTS=oplots, $
        PARENT=parent, $
        SYMCOLOR=symcolor, $
        XLOG=xlog, $
        XRANGE=xrange, $
        XSIZE=xsize, $
        YLOG=ylog, $
        YRANGE=yrange, $
        YNOZERO=ynozero, $
        YSIZE=ysize, $
        ZOOMFACTOR=zoomfactor, $
        _EXTRA=extra)
    
END