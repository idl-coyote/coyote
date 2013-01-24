; docformat = 'rst'
;
; NAME:
;   cgDotPlot
;
; PURPOSE:
;   The purpose of cgDotPlot is to create a "dot plot" of the sort described on this web page:
;   http://peltiertech.com/Utility/DotPlotUtility.html.
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
; The purpose of cgDotPlot is to create a "dot plot" of the sort described on this 
; `web page <http://peltiertech.com/Utility/DotPlotUtility.html>`.
;
;  .. image:: cgdotplot.png
;
; :Categories:
;    Graphics
;    
; :Params:
;    labels: in, required, type=strarr
;         A vector of string labels to be plotted along the left edge of the dot plot.
;    values: in, required
;         A vector of values associated with each label, representing the values to be 
;         plotted on the dot plot.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     axiscolor: in, optional, type=string/integer, default='opposite'
;        If this keyword is a string, the name of the axis color. 
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     background: in, optional, type=string/integer, default='WHITE'
;        If this keyword is a string, the name of the background color. 
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     charsize: in, optional, type=float
;        The character size for axes annotations. Uses cgDefCharSize to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string/integer, default='BLACK'
;        If this keyword is a string, the name of the data color. 
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;     font: in, optional, type=integer, default=!P.Font
;        The type of font desired for axis annotation.
;     labelcharsize: in, optional, type=float
;        The character size of the labels. The default is to use `Charsize`.
;     labelcolor: in, optional, type=string
;        The name of the color the labels should be drawn in. The default is the
;        `AxisColor`.
;     nogrid: in, optional, type=boolean, default=0
;        Set this keyword to eliminate the background grid from the plot.
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to draw the plot without erasing the display first.
;     output: in, optional, type=string, default=""
;        Set this keyword to the name of an output file. The type of file is determined
;        from the file's extension, as listed below. Normally, the file name is given in
;        all lowercase letters::
;            'ps'   - PostScript file
;            'eps'  - Encapsulated PostScript file
;            'pdf'  - PDF file
;            'bmp'  - BMP raster file
;            'gif'  - GIF raster file
;            'jpg' - JPEG raster file
;            'png'  - PNG raster file
;            'tif' - TIFF raster file
;        All raster file output is created through PostScript intermediate files (the
;        PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;        to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command. Output parameters can be set with `cgWindow_SetDefs`.
;     overplot: in, optional, type=boolean, default=0
;        Set this keyword if you wish to overplot data on an already exisiting set of
;        cgDotPlot axes. Note that labels will have to be passed, but they will not be
;        drawn again in the overplotting.
;     position: in, optional, type=vector
;        The usual four-element position vector for the Plot comamnd. Only monitored and
;        possibly set if the `Aspect` keyword is used.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. This may also be set to the
;        "name" of a symbol, such as returned from Print, cgSymCat(/Names).
;     plotfillcolor: in, optional, type=string, default='BLK1'
;        The name of the color that fills the area inside the axes on the plot.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     title: in, optional, type=string
;         The title of the plot.
;     window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in a current cgWindow or to
;        create a new cgWindow for displaying this command.
;     xcharsize: in, optional, type=float
;        The character size of the annotations on the X axis of the plot.
;     xgridstyle: in, optional, type=integer, default=1
;        The X line style of the grid lines used to draw the grid on the axis. By default, dots.
;     xrange: in, optional
;        A two-element array giving the X range of the plot.
;     xstyle: in, optional
;        A two-element array giving the X style of the plot. Normally, this is
;        set to 1 to give exact axis ranges. Otherwise, not used.
;     xtitle: in, optional, type=string
;         The X title of the plot.
;     ygridstyle: in, optional, type=integer, default=1
;        The Y line style of the grid lines used to draw the grid on the axis. By default, dots.
;
; :Examples:
;    There are several examples in a main-level program at the end of this program file.
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning 
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 12 November 2012, by David W. Fanning and donated to the IDL community 
;            by Marta Yebra of CSIRO, Australia.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgDotPlot, labels, values, $
    ADDCMD=addcmd, $
    AXISCOLOR=saxiscolor, $
    BACKGROUND=sbackground, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    FONT=font, $
    LABELCHARSIZE=labelcharsize, $
    LABELCOLOR=slabelcolor, $
    NOGRID=nogrid, $
    NOERASE=noerase, $
    OUTPUT=output, $
    OVERPLOT=overplot, $
    POSITION=position, $
    PSYM=psymIn, $
    PLOTFILLCOLOR=splotFillColor, $
    SYMSIZE=symsize, $
    TITLE=title, $
    XCHARSIZE=xcharsize, $
    XGRIDSTYLE=xgridstyle, $
    XRANGE=xrange, $
    XSTYLE=xstyle, $
    XTITLE=xtitle, $
    YGRIDSTYLE=ygridstyle, $
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
        IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgDotPlot', labels, values, $
                AXISCOLOR=saxiscolor, $
                BACKGROUND=sbackground, $
                CHARSIZE=charsize, $
                COLOR=scolor, $
                FONT=font, $
                LABELCHARSIZE=labelcharsize, $
                LABELCOLOR=slabelcolor, $
                NOGRID=nogrid, $
                NOERASE=noerase, $
                OUTPUT=output, $
                OVERPLOT=overplot, $
                POSITION=position, $
                PSYM=psymIn, $
                PLOTFILLCOLOR=splotFillColor, $
                SYMSIZE=symsize, $
                TITLE=title, $
                XCHARSIZE=xcharsize, $
                XGRIDSTYLE=xgridstyle, $
                XRANGE=xrange, $
                XSTYLE=xstyle, $
                XTITLE=xtitle, $
                YGRIDSTYLE=ygridstyle, $
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
            NOGRID=nogrid, $
            NOERASE=noerase, $
            OUTPUT=output, $
            OVERPLOT=overplot, $
            POSITION=position, $
            PSYM=psymIn, $
            PLOTFILLCOLOR=splotFillColor, $
            SYMSIZE=symsize, $
            TITLE=title, $
            XCHARSIZE=xcharsize, $
            XGRIDSTYLE=xgridstyle, $
            XRANGE=xrange, $
            XSTYLE=xstyle, $
            XTITLE=xtitle, $
            YGRIDSTYLE=ygridstyle, $
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
    background = cgDefaultColor(sbackground, DEFAULT='white', /BACKGROUND)
    axisColor = cgDefaultColor(saxisColor, DEFAULT='black')
    color = cgDefaultColor(sColor, DEFAULT='black')
    plotfillcolor = cgDefaultColor(splotfillcolor, DEFAULT='blk1')
    labelcolor = cgDefaultColor(slabelcolor, DEFAULT=axisColor)
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    IF N_Elements(labelcharsize) EQ 0 THEN labelcharsize = charsize
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
    IF N_Elements(psym) EQ 0 THEN psym = 9 ; Open circle
    IF N_Elements(xgridstyle) EQ 0 THEN xgridstyle = 1 ; Dots
    IF N_Elements(ygridstyle) EQ 0 THEN ygridstyle = 1 ; Dots
    
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
    
    ; Draw the plot axes. The first plot should not be seen, and it just to
    ; establish plot location.
    numLabels = N_Elements(labels)
    IF ~Keyword_Set(overplot) THEN BEGIN
        Plot, values, Indgen(numLabels)+1, /NoData, COLOR=axiscolor, $
          YRANGE=[0,numlabels+1], YSTYLE=9+4, YMINOR=1, YTICKS=numLabels+1, $
          POSITION=position, BACKGROUND=background, YTICKLEN=-0.025, XTICKLEN=-0.025, $
          XSTYLE=xstyle+8+4, XRANGE=xrange, NOERASE=noerase
    ENDIF
    
    ; Fill the inside of the plot with a color.
    p = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
    
    IF ~Keyword_Set(overplot) THEN BEGIN
        PolyFill, [p[0],p[0],p[2],p[2],p[0]], [p[1],p[3],p[3],p[1],p[1]], /Normal, $
          Color=plotfillcolor
    ENDIF
    
    ; Add grid lines to the plot, if needed.
    IF ~Keyword_Set(nogrid) && ~Keyword_Set(overplot) THEN BEGIN      
        Plot, values, Indgen(numLabels)+1, /NoData, COLOR=cgColor('gray'), $
          YRANGE=[0,numlabels+1], YSTYLE=9, YMINOR=1, YTICKS=numLabels+1, $
          POSITION=p, YTICKLEN=1, XTICKLEN=1, XGRIDSTYLE=xgridstyle, YGRIDSTYLE=ygridstyle, $
          XSTYLE=xstyle+8, NoErase=1, XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', XMINOR=1, $
          XRANGE=xrange
    ENDIF
    
    ; Draw the actual plot with normal annotations.
    IF Keyword_Set(overplot) THEN BEGIN
       OPlot, values, Indgen(numLabels)+1, PSYM=cgSymCat(psym), COLOR=color, SYMSIZE=symsize 
    ENDIF ELSE BEGIN
        Plot, values, Indgen(numLabels)+1, /NoData, COLOR=axiscolor, $
          YRANGE=[0,numlabels+1], YSTYLE=9, YMINOR=1, YTICKS=numLabels+1, $
          POSITION=p, YTICKLEN=-0.025, XTICKLEN=-0.025, $
          XSTYLE=xstyle+8, NoErase=1, YTICKFORMAT='(A1)', XRANGE=xrange, TITLE=title, XTITLE=xtitle
        Axis, YAXIS=1, COLOR=axiscolor, YMINOR=1, YTICKFORMAT='(A1)', YTICKLEN=0.0001, $
             YRANGE=[0,numlabels+1], YSTYLE=1
        Axis, XAXIS=1, COLOR=axiscolor, XRANGE=xrange, XTICKFORMAT='(A1)', XSTYLE=xstyle
        OPlot, values, Indgen(numLabels)+1, PSYM=cgSymCat(psym), COLOR=color, SYMSIZE=symsize
    ENDELSE
    
    ; Add the plot labels.
    IF ~Keyword_Set(overplot) THEN BEGIN
      FOR j=0,N_Elements(labels)-1 DO BEGIN
         xloc = !X.Window[0] - 0.025
         yloc = !Y.Window[0] + ((j+1) * ((!Y.Window[1]-!Y.Window[0])/(N_Elements(labels)+1)))
         skosh = 0.01
         XYOUTS, xloc, yloc-skosh, labels[j], /Normal, $
            FONT=font, Charsize=labelCharsize, Alignment=1.0, COLOR=cgColor(labelColor)
      ENDFOR
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

           