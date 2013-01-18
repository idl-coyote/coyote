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
;    label: in, optional, type=string
;         A label is similar to a plot title, but it is aligned to the left edge
;         of the plot and is written in hardware fonts. Use of the label keyword
;         will suppress the plot title.
;    legends: in, optional, type=object
;         A single cgLegendItem object, or an array of cgLegendItem objects that will be
;         drawn on the plot as a legend.
;    object: out, optional, type=objref
;         The object reference to the underlying object.
;    oplots: in, optional, type=object
;         A single cgOverPlot object, or an array of cgOverPlot objects that will be
;         overplot on the axes set up by the original data.
;    parent: in, optional, type=long
;         The identifer of the parent widget for this program's draw widget. If not
;         provided, the program will create it's own top-level base widget as the parent widget.
;    xsize: in, optional, type=int, default=640
;         The X size of the program's draw widget.
;    ysize: in, optional, type=int, default=512
;         The Y size of the program's draw widget.
;    zoomfactor: in, optional, type=float
;         Set this keyword to a number between 0.01 and 0.25. This affects the amount
;         of zooming when the X axis and Y axis are zoomed with the LEFT mouse button.
;         The default value is 0.05 or five percent of the current axis range on each
;         end of the axis, resulting in a 10 percent change in the axis length.
;    _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is 
;        allowed in the program.
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
    LABEL=label, $
    LEGENDS=legends, $
    OBJECT=thisObject, $
    OPLOTS=oplots, $
    PARENT=parent, $
    XSIZE=xsize, $
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
       LABEL=label, $
       LEGENDS=legends, $
       OPLOTS=oplots, $
       PARENT=parent, $
       XSIZE=xsize, $
       YSIZE=ysize, $
       ZOOMFACTOR=zoomfactor, $
       _EXTRA=extra)
    
END