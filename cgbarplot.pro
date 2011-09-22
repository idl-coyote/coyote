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
    baroffset = (N_Elements(baroffsetIn) EQ 0) ? barspace / barwidth : baroffsetIn
    IF N_Elements(baselines) EQ 0 THEN baselines = IntArr(nbars)
    IF N_Elements(baserange) EQ 0 THEN baserange = 0.95
    IF N_Elements(scolors) EQ 0 THEN BEGIN
        scolors=String(Byte((256.0/nbars)*(Indgen(nbars)+0.5)), FORMAT='(i0)')
    ENDIF
    IF Size(scolors, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN colors = Byte(sColors)
    IF Size(scolors, /TYPE) LE 2 THEN colors = String(Fix(sColors), Format='(I0)')
    IF N_Elements(colors) EQ 0 THEN colors = scolors
    outline = Keyword_Set(outline)
    overplot = Keyword_Set(overplot)
    IF N_Elements(oplotcolors) EQ 0 THEN oplotColors = Replicate('charcoal', nbars)
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
    IF (rotate) THEN BEGIN                 ;Horizontal bars
       base_win = !y.window                  ;Window range in Y
       scal_fact = !x.s                  ;Scaling factors
       tick_scal_fact = !y.s                 ;Tick scaling factors
    ENDIF ELSE BEGIN                   ;Vertical bars
       base_win = !x.window                  ;Window range in X
       scal_fact = !y.s                  ;Scaling factors
       tick_scal_fact = !x.s                 ;Tick scaling factors
    ENDELSE
    winrange = baserange*(base_win[1]-base_win[0])       ;Normal. window range
    barsize = barwidth*winrange/nbars            ;Normal. bar width
    winoffset = base_win[0]+(baroffset*barsize)      ;Normal. first offset
    bases = scal_fact[0]+(scal_fact[1]*baselines)    ;Baselines, in normal coor.
    normal = scal_fact[0]+(scal_fact[1]*values)      ;Values, in normal coor.
    barstart = Lindgen(nbars)*(barsize+barspace*(winrange/nbars)) ;Coor. at left edges
    tickv = winoffset+barstart+(0.5*barsize)         ;Tick coor. (centered)
    
    FOR i=0,nbars-1 do BEGIN               ;Draw the bars
       width = winoffset+[barstart[i],barstart[i], $     ;Compute bar width
         (barstart[i]+barsize),(barstart[i]+barsize)]
       length = [bases[i],normal[i],normal[i],bases[i]]  ;Compute bar length
       IF (rotate) THEN BEGIN              ;Horizontal bars
          x = length                     ;X-axis is "length" axis
          y = width                      ;Y-axis is "width" axis
       ENDIF ELSE BEGIN                ;Vertical bars
          x = width                      ;X-axis is "width" axis
          y = length                     ;Y-axis is "length" axis
       ENDELSE
       cgColorFill, x, y, COLOR=colors[i], /NORMAL        ;Polyfill with color
       IF (outline) THEN cgPlots, x, y, /NORMAL        ;Outline using !p.color
    ENDFOR
    
    tickv = (tickv-tick_scal_fact[0])/tick_scal_fact[1]  ;Locations of the ticks
    IF (rotate) THEN BEGIN                 ;Label the bars (Y-axis)
      cgAxis,yaxis=0,ystyle=1,yticks=(nbars-1),ytickv=tickv,ytickname=barnames, $
        yticklen=0.0
    ENDIF ELSE BEGIN                       ;Label the bars (X-axis)
      cgAxis,xaxis=0,xstyle=1,xticks=(nbars-1),xtickv=tickv,xtickname=barnames, $
        xticklen=0.0
    ENDELSE

END