PRO cgDotPlot, labels, values, $
    ADDCMD=addcmd, $
    AXISCOLOR=saxiscolor, $
    BACKGROUND=sbackground, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    FONT=font, $
    LABELCHARSIZE=labelcharsize, $
    LABELCOLOR=slabelcolor, $
    NODATA=nodata, $
    NOERASE=noerase, $
    OUTPUT=output, $
    POSITION=position, $
    PSYM=psymIn, $
    PLOTFILLCOLOR=splotFillColor, $
    SYMSIZE=symsize, $
    TITLE=title, $
    XCHARSIZE=xcharsize, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTITLE=xtitle, $
    WINDOW=window
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        IF (N_Elements(output) NE 0) THEN PS_END, /NOFIX
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgDotPlot, labels, values'
        RETURN
    ENDIF
    
    ; Need labels and values.
    IF Size(labels, /TNAME) NE 'STRING' THEN Message, 'The LABELS parameter must be a string array.'
    IF N_Elements(values) NE N_Elements(labels) THEN Message, 'The number of labels must match the number of values provided.'    
    
    ; Check to see if psymIn is a string. If so, covert it here.
    IF N_Elements(psymIn) NE 0 THEN BEGIN
        IF Size(psymIn, /TNAME) EQ 'STRING' THEN BEGIN
              names = cgSymCat(/Names) 
              index = Where(STRUPCASE(StrCompress(names, /REMOVE_ALL)) EQ STRUPCASE(StrCompress(psymIN, /REMOVE_ALL)), count)
              IF count GT 0 THEN psym = index[0] ELSE Message, 'Cannot resolve the PSYM value: ' + psymIn
        ENDIF ELSE psym = psymIn
    ENDIF
    
    ; Pay attention to !P.NoErase in setting the NOERASE kewyord. 
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; Special treatment for overplotting or adding a command.
        IF Keyword_Set(addcmd) THEN BEGIN
        cgWindow, 'cgDotPlot', labels, values, $
            AXISCOLOR=saxiscolor, $
            BACKGROUND=sbackground, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            FONT=font, $
            LABELCHARSIZE=labelcharsize, $
            LABELCOLOR=slabelcolor, $
            NODATA=nodata, $
            NOERASE=noerase, $
            OUTPUT=output, $
            POSITION=position, $
            PSYM=psymIn, $
            PLOTFILLCOLOR=splotFillColor, $
            SYMSIZE=symsize, $
            TITLE=title, $
            XCHARSIZE=xcharsize, $
            XRANGE=xrange, $
            XSTYLE=xstyle, $
            XTITLE=xtitle, $
            ADDCMD=1
         RETURN
       ENDIF
        
        ; Open a new window or replace the current commands, as required.
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgDotPlot', labels, values, $
            AXISCOLOR=saxiscolor, $
            BACKGROUND=sbackground, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            FONT=font, $
            LABELCHARSIZE=labelcharsize, $
            LABELCOLOR=slabelcolor, $
            NODATA=nodata, $
            NOERASE=noerase, $
            OUTPUT=output, $
            POSITION=position, $
            PSYM=psymIn, $
            PLOTFILLCOLOR=splotFillColor, $
            SYMSIZE=symsize, $
            TITLE=title, $
            XCHARSIZE=xcharsize, $
            XRANGE=xrange, $
            XSTYLE=xstyle, $
            XTITLE=xtitle, $
            REPLACECMD=replaceCmd
         RETURN
    ENDIF
    
    ; Are we doing some kind of output?
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
       ; If the output string has a dot character, then this must be a
       ; filename, and we will determine the type of file from the filename extension.
       IF StrPos(output, '.') NE -1 THEN BEGIN
             root_name = cgRootName(output, DIRECTORY=theDir, EXTENSION=ext)
             IF theDir EQ "" THEN CD, CURRENT=theDir
             outfilename = output
             outputSelection = StrUpCase(ext)
       ENDIF
       
       ; Need to have a filename and extention at this point.
       IF N_Elements(outfilename) EQ 0 THEN Message, 'Output filename  must have an file type extension.'
       IF N_Elements(outputSelection) EQ 0 THEN Message, 'An output filename extension is required.'
       
       typeOfOutput = ['PS','EPS','PDF','BMP','GIF','JPEG','JPG','PNG','TIFF', 'TIF']
       void = Where(typeOfOutput EQ outputSelection, count)
       IF count EQ 0 THEN Message, 'Cannot find ' + outputSelection + ' in allowed output types.'
       
       ; Set things up.
       CASE outputSelection OF
          'PS': BEGIN
              ext = '.ps'
              delete_ps = 0
              END    
          'EPS': BEGIN
              ext = '.eps'
              encapsulated = 1
              delete_ps = 0
              END
          'PDF': BEGIN
              ext = '.pdf'
              pdf_flag = 1
              delete_ps = 1
              END     
          'BMP': BEGIN
              ext = '.bmp'
              bmp_flag = 1
              delete_ps = 1
              END      
          'GIF': BEGIN
              ext = '.gif'
              gif_flag = 1
              delete_ps = 1
              END
          'JPEG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END      
          'JPG': BEGIN
              ext = '.jpg'
              jpeg_flag = 1
              delete_ps = 1
              END
          'PNG': BEGIN
              ext = '.png'
              png_flag = 1
              delete_ps = 1
              END      
          'TIFF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END
          'TIF': BEGIN
              ext = '.tif'
              tiff_flag = 1
              delete_ps = 1
              END    
       ENDCASE
       
       ; We need to know the root name of the file, because we have to make a PostScript
       ; file of the same name. At least we do if the type is not PS or EPS.
       IF (outputSelection NE 'PS') && (outputSelection NE 'EPS') THEN BEGIN
           root_name = cgRootName(outfilename, DIRECTORY=theDir)
           IF theDir EQ "" THEN CD, CURRENT=theDir
           ps_filename = Filepath(ROOT_DIR=theDir, root_name + '.ps')
       ENDIF ELSE ps_filename = outfilename
       
       ; Get the output default values.
       cgWindow_GetDefs, $
         PS_Charsize = ps_charsize, $          ; The PostScript character size.
         PS_FONT = ps_font, $                  ; Select the font for PostScript output.
         PS_Decomposed = ps_decomposed, $      ; Sets the PostScript color mode.
         PS_Delete = ps_delete, $              ; Delete PS file when making IM raster.
         PS_Metric = ps_metric, $              ; Select metric measurements in PostScript output.
         PS_Scale_factor = ps_scale_factor, $  ; Select the scale factor for PostScript output.
         PS_TT_Font = ps_tt_font               ; Select the true-type font to use for PostScript output.   
       
       ; Set up the PostScript device.
       PS_Start, $
          CHARSIZE=ps_charsize, $
          DECOMPOSED=ps_decomposed, $
          FILENAME=ps_filename, $
          FONT=ps_font , $
          ENCAPSULATED=encapsulated, $
          METRIC=ps_metric, $
          SCALE_FACTOR=ps_scale_factor, $
          TT_FONT=ps_tt_font, $
          QUIET=1
    
    ENDIF
    
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /GET
    
    ; Going to do this in decomposed color, if possible.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    ; Check keywords.
    IF N_Elements(title) EQ 0 THEN title = ""
    IF N_Elements(xtitle) EQ 0 THEN xtitle = ""
    title = cgCheckForSymbols(title)
    xtitle = cgCheckForSymbols(xtitle)
    FOR j=0,N_Elements(labels)-1 DO labels[j] = cgCheckForSymbols(labels[j])
    background = cgDefaultColor(sbackground, /BACKGROUND, MODE=currentState)
    axisColor = cgDefaultColor(saxisColor, DEFAULT='opposite', MODE=currentState)
    color = cgDefaultColor(sColor, DEFAULT=axisColor, MODE=currentState)
    plotfillcolor = cgDefaultColor(splotfillcolor, DEFAULT='blk1', MODE=currentState)
    labelcolor = cgDefaultColor(slabelcolor, DEFAULT=axisColor, MODE=currentState)
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(labelcharsize) EQ 0 THEN labelcharsize = charsize
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
    IF N_Elements(psym) EQ 0 THEN psym = 9 ; Open circle
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
         IF ~noerase THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
;                IF Keyword_Set(overplot) NE 1 THEN BEGIN
;                
;                    ; Save the current system variables. Will need to restore later.
;                    bangx = !X
;                    bangy = !Y
;                    bangp = !P
;                    
;                    ; Draw the plot that doesn't draw anything.
;                    Plot, [1], POSITION=position, CHARSIZE=charsize, /NODATA, $
;                        FONT=font, _STRICT_EXTRA=extra  
;                    
;                    ; Save the "after plot" system variables. Will use later. 
;                    afterx = !X
;                    aftery = !Y
;                    afterp = !P     
;                    
;                    ; Draw the background color and set the variables you will need later.
;                    PS_Background, background
;                    psnodraw = 1
;                    tempNoErase = 1
;                    
;                    ; Restore the original system variables so that it is as if you didn't
;                    ; draw the invisible plot.
;                    !X = bangx
;                    !Y = bangy
;                    !P = bangp
;                
;                ENDIF
            ENDIF ELSE tempNoErase = noerase
        ENDIF ELSE tempNoErase = noerase
     ENDIF ELSE tempNoErase = noerase
 
    ; Load the drawing colors. If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN axiscolor = cgColor(axiscolor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
    IF Size(plotfillcolor, /TNAME) EQ 'STRING' THEN plotfillcolor = cgColor(plotfillcolor)
    IF Size(symcolor, /TNAME) EQ 'STRING' THEN symcolor = cgColor(symcolor)
    IF N_Elements(xstyle) EQ 0 THEN xstyle = 0
    
    ; Calculate the maximum length of the labels in normalized coordinates.
    maxStrLen = Max(StrLen(labels))
    maxLength = maxStrLen * Float(!D.X_CH_SIZE) / !D.X_SIZE
    
    ; Calculate a default plot position.
    IF N_Elements(position) EQ 0 && (Total(!P.Multi) LE 0) && (Total(!P.Position) EQ 0.0) THEN BEGIN
        position = [0.125+maxLength, 0.125, 0.9, 0.9]
    ENDIF
    
    ; Draw the plot axes.
    numLabels = N_Elements(labels)
    IF ((!D.Flags AND 256) NE 0) && (!P.Multi[0] EQ 0) && ~noerase THEN cgErase, background
    Plot, values, Indgen(numLabels)+1, /NoData, COLOR=axiscolor, $
      YRANGE=[0,numlabels+1], YSTYLE=9+4, YMINOR=1, YTICKS=numLabels+1, $
      POSITION=position, BACKGROUND=background, YTICKLEN=-0.025, XTICKLEN=-0.025, $
      XSTYLE=xstyle+8+4, XRANGE=xrange, NOERASE=noerase
    p = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
    PolyFill, [p[0],p[0],p[2],p[2],p[0]], [p[1],p[3],p[3],p[1],p[1]], /Normal, $
      Color=plotfillcolor
    Plot, values, Indgen(numLabels)+1, /NoData, COLOR=cgColor('gray'), $
      YRANGE=[0,numlabels+1], YSTYLE=9, YMINOR=1, YTICKS=numLabels+1, $
      POSITION=position, YTICKLEN=1, XTICKLEN=1, XGRIDSTYLE=2, YGRIDSTYLE=2, $
      XSTYLE=xstyle+8, /NoErase, XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', XMINOR=1, XRANGE=xrange
    
    Plot, values, Indgen(numLabels)+1, /NoData, COLOR=axiscolor, $
      YRANGE=[0,numlabels+1], YSTYLE=9, YMINOR=1, YTICKS=numLabels+1, $
      POSITION=position, YTICKLEN=-0.025, XTICKLEN=-0.025, $
      XSTYLE=xstyle+8, /NoErase, YTICKFORMAT='(A1)', XRANGE=xrange, TITLE=title, XTITLE=xtitle
    Axis, YAXIS=1, COLOR=axiscolor, YMINOR=1, YTICKFORMAT='(A1)', YTICKLEN=0.0001, $
         YRANGE=[0,numlabels+1], YSTYLE=1
    Axis, XAXIS=1, COLOR=axiscolor, XRANGE=xrange, XTICKFORMAT='(A1)', XSTYLE=xstyle
    OPlot, values, Indgen(numLabels)+1, PSYM=cgSymCat(psym), COLOR=color, SYMSIZE=symsize
    
    FOR j=0,N_Elements(labels)-1 DO BEGIN
       xloc = !X.Window[0] - 0.025
       yloc = !Y.Window[0] + ((j+1) * ((!Y.Window[1]-!Y.Window[0])/(N_Elements(labels)+1)))
       skosh = 0.005
       XYOUTS, xloc, yloc-skosh, labels[j], /Normal, $
          FONT=font, Charsize=labelCharsize, Alignment=1.0, COLOR=cgColor(labelColor)
    ENDFOR
    
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
    
    ; Are we producing output? If so, we need to clean up here.
    IF (N_Elements(output) NE 0) && (output NE "") THEN BEGIN
    
       ; Get the output default values.
       cgWindow_GetDefs, $
           IM_Density = im_density, $                      ; Sets the density parameter on ImageMagick convert command.
           IM_Options = im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
           IM_Resize = im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
           IM_Transparent = im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
           IM_Width = im_width, $                          ; Sets the width of raster file output created with ImageMagick.
           PDF_Unix_Convert_Cmd = pdf_unix_convert_cmd, $  ; Command to convert PS to PDF.
           PDF_Path = pdf_path                             ; The path to the Ghostscript conversion command.
    
        ; Close the PostScript file and create whatever output is needed.
        PS_END, DELETE_PS=delete_ps, $
             ALLOW_TRANSPARENT=im_transparent, $
             BMP=bmp_flag, $
             DENSITY=im_density, $
             GIF=gif_flag, $
             GS_PATH=pdf_path, $
             IM_OPTIONS=im_options, $
             JPEG=jpeg_flag, $
             PDF=pdf_flag, $
             PNG=png_flag, $
             RESIZE=im_resize, $
             TIFF=tiff_flag, $
             UNIX_CONVERT_CMD=pdf_unix_convert_cmd, $
             WIDTH=im_width

         basename = File_Basename(outfilename)
         dirname = File_Dirname(outfilename)
         IF dirname EQ "." THEN CD, CURRENT=dirname
         Print, 'Output File: ' + Filepath(ROOT_DIR=dirname, basename)
    ENDIF
    
    ; Restore the decomposed color state if you can.
    SetDecomposedState, currentState
    
    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
        
END

labels = [ 'Exxon Mobil', $
           'Wal-Mart', $
           'Chevron', $           
           'ConocoPhillips', $
           'General Electric', $
           'General Motors', $
           'Ford Motor', $
           'At&T', $
           'Hewlett-Packard', $
           'Bank of America' ]
           
values = [ 100, 105, 125, 142, 170, 193, 247, 325, 367, 418]

cgDisplay, WID=0, Title='Default Values'
cgDotPlot, labels, values
cgDisplay, WID=1, Title='Optional Values'
cgDotPlot, labels, values, symsize=1.5, psym=16, $
   Title='Fortune 500 Companies', XTitle='Millions in Revenue', $
   Color='red', AXISCOLOR='navy',plotfillcolor='rose', XRANGE=[80,450], XSTYLE=1
  
 ; Resizeable graphics window. 
 cgDotPlot, labels, values, symsize=1.5, psym=16, $
   Title='Fortune 500 Companies', XTitle='Millions in Revenue', $
   Color='red', AXISCOLOR='navy',plotfillcolor='rose', XRANGE=[80,450], XSTYLE=1, $
   /Window
   
   
;cgDotPlot, labels, values, output='cgdotplot_test.png'

PS_Start, 'test.ps'
cgDisplay, WID=2, 1000, 500
cgDotPlot, labels, values, Title='RMSE', position=[0.25, 0.15, 0.45, 0.9]
cgDotPlot, Replicate("",N_Elements(labels)), values, TITLE='RMSE A', position=[0.50, 0.15, 0.70, 0.9], /Noerase
cgDotPlot, Replicate("",N_Elements(labels)), values, TITLE='R$\up2$', position=[0.75, 0.15, 0.95, 0.9], /Noerase
PS_END, /png
END

           