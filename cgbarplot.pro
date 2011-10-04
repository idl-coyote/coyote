; docformat = 'rst'
;
; NAME:
;   cgBarPlot
;
; PURPOSE:
;   Provides a device-independent and color-model-independent way to create a bar
;   plot or to overplot on an existing bar plot.
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
; :Description:
;   Provides a device-independent and color-model-independent way to create a bar
;   plot or to overplot on an existing bar plot.
;
; :Categories:
;    Graphics
;    
; :Params:
;    values: in, required
;         A vector containing the values to be represented by bars. Each element in
;         the vector will be represented by a bar. 
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;         Set this keyword to add the command to a cgWindow resizeable graphics window.
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     barnames: in, optional, type=string, default=""
;        A string array, containing one string label per bar. If the bars are vertical, 
;        the labels are placed beneath them.  If horizontal (rotated) bars are specified, 
;        the labels are placed to the left of the bars.
;     baroffset: in, optional, type=float, default=barspace/barwidth*1.5
;        A scalar that specifies the offset to be applied to the first bar, in units of 
;        "nominal bar width".  This keyword allows, for example, different groups of bars 
;        to be overplotted on the same graph.  
;     barspace: in, optional, type=float, default=barwidth*0.20
;        A scalar that specifies, in units of "nominal bar width", the spacing between bars.  
;        For example, if BARSPACE is 1.0, then all bars will have one bar-width of space 
;        between them. If not specified, the bars are spaced apart by 20% of the bar width.
;     barwidth: in, optional, type=float, default=barwidth*0.20
;         A floating-point value that specifies the width of the bars in units of "nominal bar 
;         width".  The nominal bar width is computed so that all the bars (and the space 
;         between them, set by default to 20% of the width of the bars) will fill the
;         available space (optionally controlled with the BASERANGE keyword).
;     baselines: in, optional, type=float, default=0.0
;         A vector, the same size as VALUES, that contains the base value associated 
;         with each bar.  If not specified, a base value of zero is used for all bars.
;     baserange: in, optional, type=float, default=1.0
;         A floating-point scalar in the range 0.0 to 1.0, that determines the fraction of 
;         the total available plotting area (in the direction perpendicular to the bars) to 
;         be used. If not specified, the full available area is used.
;     colors: in, optional, type=varies
;         A vector of color values, the same size as VALUES, containing either the color
;         names, 24-bit color values, or color index numbers (byte values) of the colors to
;         be used for the bars. If not specified, the colors are selected based on the 
;         available colors in the current color table.
;     layout: in, optional, type=intarr(3)
;         This keyword specifies a grid with a graphics window and determines where the
;         graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;         The grid is determined by the number of columns (ncolumns) by the number of 
;         rows (nrows). The location of the graphic is determined by the third number. The
;         grid numbering starts in the upper left (1) and goes sequentually by column and then
;         by row.
;     noerase: in, optional, type=boolean, default=0
;         Set this keyword to draw the plot without erasing the display first.
;     outline: in, optional, type=boolean, default=0
;         Set this keyword to draw an outline around each bar in the OPLOTCOLORS.
;     oplotcolors: in, optional, type=varies, default='charcoal'
;         A vector of color values, similar to colors for overplot outlines on the bars.
;         If a scalar value (e.g., "charcoal") the same value is used for all outlines.
;     overplot: in, optional, type=boolean, default=0
;         Set this keyword if you wish to overplot data on an already exisiting set of
;         axes. It is like calling the IDL OPLOT command.
;     position: in, optional, type=fltarr(4)
;         The usual four-element normalized position vector for the Plot comamnd. 
;     rotate: in, optional, type=boolean, default=0
;         If set, this keyword indicates that horizontal rather than vertical bars should 
;         be drawn.  The bases of horizontal bars are on the left, "Y" axis and the bars 
;         extend to the right.
;     title: in, optional, type=string, default=""
;         The title of the plot, if supplied.
;     window: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the plot in a resizable graphics window.
;     xtitle: in, optional, type=string, default=""
;         The X title of the plot, if supplied.
;     ytitle: in, optional, type=string, default=""
;         The Y title of the plot, if supplied.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot command is allowed in the program.
;          
; :Examples:
;    To create bar plots of four different bar classes. Example uses AL_Legend from
;    the NASA Astronomy Library. Comment out if not available.::
;       
;    ; Create data.
;    seed = -5L
;    data1 = RandomU(seed, 5) * 10.0
;    data2 = RandomU(seed, 5) * 7.5
;    data3 = RandomU(seed, 5) * 7.5
;    data4 = RandomU(seed, 5) * 10.0
;    
;    ; Display as four different plots.
;    !P.Multi=[0,2,2]
;    cgDisplay, WID=0
;    cgBarPlot, data1, /OUTLINE, OPLOTCOLOR='charcoal', YRANGE=[0, 12], COLORS='red', XTITLE='Class 1'
;    cgBarPlot, data2, /OUTLINE, OPLOTCOLOR='charcoal', YRANGE=[0, 12], COLORS='blue', XTITLE='Class 2'
;    cgBarPlot, data3, /OUTLINE, OPLOTCOLOR='charcoal', YRANGE=[0, 12], COLORS='gold', XTITLE='Class 3'
;    cgBarPlot, data4, /OUTLINE, OPLOTCOLOR='charcoal', YRANGE=[0, 12], COLORS='dark green', XTITLE='Class 4'
;    !P.Multi=0
;
;    ; Display as accumulated bar plots.
;    cgDisplay, WID=1
;    barnames = ['Exp 1', 'Exp 2', 'Exp 3', 'Exp 4', 'Exp 5']
;    cgBarPlot, data1, YRANGE=[0, 35], COLORS='red', BARNAMES=barnames
;    cgBarplot, data2, /OVERPLOT, BASELINE=data1, COLORS='blue'
;    cgBarplot, data3, /OVERPLOT, BASELINE=data1+data2, COLORS='gold'
;    cgBarplot, data4, /OVERPLOT, BASELINE=data1+data2+data3, COLORS='dark green'
;    colors = ['red', 'blue', 'gold', 'dark green']
;    items = ['Class 1', 'Class 2', 'Class 3', 'Class 4']
;    Al_Legend, items, /FILL, PSYM=Replicate(15,4), COLORS=colors, SYMSIZE=Replicate(1.75,4), $
;       POSITION=[0.20, 0.92], /NORMAL, CHARSIZE=cgDefCharSize()
;
;    ; Display all on same plot.
;    cgDisplay, WID=2
;    cgBarPlot, data1, YRANGE=[0, 12], BAROFFSET=2.5, BASERANGE=0.20, COLORS='red'
;    cgBarplot, data2, /OVERPLOT, BAROFFSET=9.5, BASERANGE=0.20, COLORS='blue'
;    cgBarplot, data3, /OVERPLOT, BAROFFSET=17.0, BASERANGE=0.20, COLORS='gold'
;    cgBarplot, data4, /OVERPLOT, BAROFFSET=24.5, BASERANGE=0.20, COLORS='dark green'
;    colors = ['red', 'blue', 'gold', 'dark green']
;    items = ['Class 1', 'Class 2', 'Class 3', 'Class 4']
;    Al_Legend, items, /FILL, PSYM=Replicate(15,4), COLORS=colors, SYMSIZE=Replicate(1.75,4), $
;       POSITION=[0.45, 0.92], /NORMAL, CHARSIZE=cgDefCharSize()

; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 3 October 2011. DWF. Written as a straight-forward translation of
;            the IDL Bar_Plot program. In retrospect, maybe not the best idea, as I 
;            don't think the Bar_Plot program actually works and the interface is clunky.
;            But, this is a start. The interface may change to something more elegant over
;            time.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgBarPlot, values, $
    ADDCMD=addcmd, $
    AXISCOLOR=saxiscolor, $
    BACKGROUND=sbackground, $
    BARNAMES=barnamesIn, $
    BAROFFSET=baroffsetIn, $
    BARSPACE=barspaceIn, $
    BARWIDTH=barwidthIn, $
    BASELINES=baselines, $
    BASERANGE=baserange, $
    COLORS=scolors, $
    LAYOUT=layout, $
    NOERASE=noerase, $
    OUTLINE=outline, $
    OPLOTCOLORS=oplotcolors, $
    OVERPLOT=overplot, $
    POSITION=position, $
    RANGE=range, $
    ROTATE=rotate,  $
    TITLE=title,  $
    WINDOW=window, $
    XTITLE=xtitle, $
    YTITLE=ytitle, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = Error_Message()
        IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF (N_Elements(values) EQ 0) THEN BEGIN
        Print, 'USE SYNTAX: cgBarPlot, values'
        RETURN
    ENDIF
    
    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        ; Special treatment for overplotting or adding a command.
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgBarPlot', values, $
                ADDCMD=1, $
                AXISCOLOR=saxiscolor, $
                BACKGROUND=sbackground, $
                BARNAMES=barnamesIn, $
                BAROFFSET=baroffsetIn, $
                BARSPACE=barspaceIn, $
                BARWIDTH=barwidthIn, $
                BASELINES=baselines, $
                BASERANGE=baserange, $
                COLORS=scolors, $
                LAYOUT=layout, $
                NOERASE=noerase, $
                OPLOTCOLORS=oplotcolors, $
                OUTLINE=outline, $
                OVERPLOT=overplot, $
                POSITION=position, $
                RANGE=range, $
                ROTATE=rotate,  $
                TITLE=title,  $
                XTITLE=xtitle, $
                YTITLE=ytitle, $
                _REF_EXTRA=extra
             RETURN
       ENDIF
        
        ; Open a new window or replace the current commands, as required.
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
            cgWindow, 'cgBarPlot', values, $
                AXISCOLOR=saxiscolor, $
                BACKGROUND=sbackground, $
                BARNAMES=barnamesIn, $
                BAROFFSET=baroffsetIn, $
                BARSPACE=barspaceIn, $
                BARWIDTH=barwidthIn, $
                BASELINES=baselines, $
                BASERANGE=baserange, $
                COLORS=scolors, $
                LAYOUT=layout, $
                NOERASE=noerase, $
                OPLOTCOLORS=oplotcolors, $
                OUTLINE=outline, $
                OVERPLOT=overplot, $
                POSITION=position, $
                RANGE=range, $
                ROTATE=rotate,  $
                REPLACECMD=replaceCmd, $
                TITLE=title,  $
                XTITLE=xtitle, $
                YTITLE=ytitle, $
                _REF_EXTRA=extra
            
         RETURN
    ENDIF
    
    ; The number of bars to draw.
    nbars = N_Elements(values)

    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /GET
    
    ; Going to do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
    ENDIF

    ; Check the keywords.
    IF N_Elements(sbackground) EQ 0 THEN BEGIN
        IF Keyword_Set(overplot) || Keyword_Set(noerase) THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                background = 'WHITE' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF (!D.Window LT 0) &&  Keyword_Set(noerase) THEN BEGIN
                        Window
                        IF ~Keyword_Set(traditional) THEN cgErase, 'WHITE'
                    ENDIF
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN background = 'WHITE'
                    IF (Total(pixel) EQ 0) THEN background = 'BLACK'
                    IF N_Elements(background) EQ 0 THEN background = 'OPPOSITE'
                ENDIF ELSE background = 'OPPOSITE'
           ENDELSE
        ENDIF ELSE background = 'WHITE' 
    ENDIF ELSE background = sbackground
    IF Size(background, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN background = Byte(background)
    IF Size(background, /TYPE) LE 2 THEN background = StrTrim(Fix(background),2)
    
    ; Choose an axis color.
    IF N_Elements(saxisColor) EQ 0 AND N_Elements(saxescolor) NE 0 THEN saxiscolor = saxescolor
    IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN saxisColor = 'BLACK'
       ENDIF
       IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                IF StrUpCase(background) EQ 'BLACK' THEN background = 'WHITE'
                saxisColor = 'BLACK' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF !D.Window LT 0 THEN Window
                    IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) OR (StrUpCase(background) EQ 'WHITE') THEN saxisColor = 'BLACK'
                    IF (Total(pixel) EQ 0) OR (StrUpCase(background) EQ 'BLACK') THEN saxisColor = 'WHITE'
                    IF N_Elements(saxisColor) EQ 0 THEN saxisColor = 'OPPOSITE'
                ENDIF ELSE saxisColor = 'OPPOSITE'
          ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(saxisColor) EQ 0 THEN axisColor = !P.Color ELSE axisColor = saxisColor
    IF Size(axisColor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN axisColor = Byte(axisColor)
    IF Size(axisColor, /TYPE) LE 2 THEN axisColor = StrTrim(Fix(axisColor),2)
    
    ; If colors are identical, do something about it.
    IF ColorsAreIdentical(background, axiscolor) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
           IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        axiscolor = 'OPPOSITE'
    ENDIF
    
    ; Check other keywords.
    addcmd = Keyword_Set(addcmd)
    barnames = (N_Elements(barnamesIn) EQ 0) ? StrArr(nbars) + ' ' : barnamesIn
    IF N_Elements(barnames) NE nbars THEN Message, 'Number of bar names does not equal the number of bars.'
    barspace = (N_Elements(barspaceIn) EQ 0) ? 0.2 : Float(barspaceIn)
    barwidth = (N_Elements(barwidthIn) EQ 0) ? 1.0 - barspace - (barspace / nbars) : barwidthIn    
    baroffset = (N_Elements(baroffsetIn) EQ 0) ? barspace/barwidth*1.5 : baroffsetIn
    IF N_Elements(baselines) EQ 0 THEN baselines = IntArr(nbars)
    IF N_Elements(baserange) EQ 0 THEN baserange = 0.95
    IF N_Elements(scolors) EQ 0 THEN BEGIN
        scolors=String(Byte((256.0/nbars)*(Indgen(nbars)+0.5)), FORMAT='(i0)')
    ENDIF
    IF Size(scolors, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN colors = Byte(sColors)
    IF Size(scolors, /TYPE) LE 2 THEN colors = String(Fix(sColors), Format='(I0)')
    IF N_Elements(colors) EQ 0 THEN colors = scolors
    IF N_Elements(colors) EQ 1 THEN colors = Replicate(colors, nbars)
    IF N_Elements(colors) NE nbars THEN $
       Message, 'There is a mismatch between the number of bars and number of bar colors.'
    outline = Keyword_Set(outline)
    overplot = Keyword_Set(overplot)
    IF N_Elements(oplotcolors) EQ 0 THEN oplotColors = Replicate('charcoal', nbars)
    IF N_Elements(oplotColors) EQ 1 THEN oplotColors = Replicate(oplotColors, nbars)
    IF N_Elements(oplotColors) NE nbars THEN $
       Message, 'There is a mismatch between the number of bars and number of overplot colors.'
    noerase = Keyword_set(noerase)
    rotate = Keyword_Set(rotate)
    IF N_Elements(title) EQ 0 THEN title = ""
    window = Keyword_Set(window)
    IF N_Elements(xtitle) EQ 0 THEN xtitle = ""
    IF N_Elements(ytitle) EQ 0 THEN ytitle = ""
    
    
    ; Find the min and max of the data range. Comparing baselines and values.
    mnB = Min(baselines, MAX=mxB, /NAN)
    mnV = Min(values, MAX=mxV, /NAN)
    IF N_Elements(range) EQ 0 THEN range=[mnB < mnV, mxB > (mxV + 0.1*mxV)]      
    
    ; Do you want horizontal bars, rather than vertical?
    IF (rotate) THEN BEGIN                
       xrange = range             ;Or, use range specIFied
       yrange = [0, N_Elements(values)]        ;Axis perpend. to bars
       yticks = 1                    ;Suppress ticks in plot
       ytickname = strarr(2)+' '
       xticks = 0
       xtickname = strarr(1)+''
    ENDIF ELSE BEGIN                   ;Vertical bars
       yrange = range 
       xrange = [0, N_Elements(values)]                 ;Axis perpend. to bars
       xticks = 1                    ;Suppress ticks in plot
       xtickname = strarr(2)+' '
       yticks = 0
       ytickname = strarr(1)+''
    ENDELSE
  IF (overplot EQ 0) THEN BEGIN              ;Create new plot, no data
        cgPlot,[values],/nodata,title=title,xtitle=xtitle,ytitle=ytitle, $
           noerase=overplot,xrange=xrange,yrange=yrange,xticks=xticks, $
           xtickname=xtickname,yticks=yticks,ytickname=ytickname, $
           xstyle=1,ystyle=1,/data,background=background, position=position, $
           axiscolor=axiscolor,_strict_extra=extra
    ENDIF
    IF (rotate) THEN BEGIN               ; Horizontal bars
       base_win = !y.window              ; Window range in Y
       scal_fact = !x.s                  ; Scaling factors
       tick_scal_fact = !y.s             ; Tick scaling factors
    ENDIF ELSE BEGIN                     ; Vertical bars
       base_win = !x.window              ; Window range in X
       scal_fact = !y.s                  ; Scaling factors
       tick_scal_fact = !x.s             ; Tick scaling factors
    ENDELSE
    winrange = baserange*(base_win[1]-base_win[0])   ; Normal. window range
    barsize = barwidth*winrange/nbars                ; Normal. bar width
    winoffset = base_win[0]+(baroffset*barsize)      ; Normal. first offset
    bases = scal_fact[0]+(scal_fact[1]*baselines)    ; Baselines, in normal coordinates
    normal = scal_fact[0]+(scal_fact[1]*values)      ; Values, in normal coordinates
    barstart = Lindgen(nbars)*(barsize+barspace*(winrange/nbars)) ; Coor. at left edges
    tickv = winoffset+barstart+(0.5*barsize)         ; Tick coordinates. (centered)
    
    FOR i=0,nbars-1 do BEGIN               ; Draw the bars
       width = winoffset+[barstart[i],barstart[i], $     ; Compute bar width
         (barstart[i]+barsize),(barstart[i]+barsize)]
       length = [baselines[i], baselines[i]+values[i], baselines[i]+values[i], baselines[i]]
       IF (rotate) THEN BEGIN              ; Horizontal bars
          xy = Convert_Coord(length, [0,1,1,0], /DATA, /TO_NORMAL)  ; Compute bar length
          length=Transpose(xy[0,*])
          x = length                       ; X-axis is "length" axis
          y = width                        ; Y-axis is "width" axis
       ENDIF ELSE BEGIN                    ; Vertical bars
          xy = Convert_Coord([0,1,1,0], length, /DATA, /TO_NORMAL)  ; Compute bar length
          length=Transpose(xy[1,*])
          x = width                        ; X-axis is "width" axis
          y = length                       ; Y-axis is "length" axis
       ENDELSE
       cgColorFill, x, y, COLOR=colors[i], /NORMAL                    ; Polyfill with color
       IF (outline) THEN cgPlots, x, y, /NORMAL, COLOR=OplotColors[i] ; Outline using !p.color
    ENDFOR
    
    tickv = (tickv-tick_scal_fact[0])/tick_scal_fact[1]  ; Locations of the ticks
    IF (rotate) THEN BEGIN                 ; Label the bars (Y-axis)
      cgAxis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickv,ytickname=barnames, $
        yticklen=0.0
    ENDIF ELSE BEGIN                       ; Label the bars (X-axis)
      cgAxis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv,xtickname=barnames, $
        xticklen=0.0
    ENDELSE

END