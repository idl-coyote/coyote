; docformat = 'rst'
;
; NAME:
;   cgScatter2D
;
; PURPOSE:
;   The purpose of cgScatter2d is to create a two-dimensional scatter plot with the 
;   option of drawing a correlation coefficient on the plot.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of cgScatter2d is to create a two-dimensional scatter plot with the 
; option of drawing a correlation coefficient on the plot.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;   
; :Params:
; 
;     x: in, required
;        The variable along the X or horizontal dimension.
;     y: in, required
;        The varaiable along the Y or vertical dimension.
;        
; :Keywords:
; 
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     aspect: in, optional, type=float, default=none
;        Set this keyword to a floating point ratio that represents the aspect ratio 
;        (ysize/xsize) of the resulting plot. The plot position may change as a result
;        of setting this keyword. Note that `Aspect` cannot be used when plotting with
;        !P.MULTI.
;     axiscolor: in, optional, type=string, default='opposite'
;        The name of the axis color. May be specified as a color table index number, as well.
;     axescolor: in, optional, type=string
;        Provisions for bad spellers.
;     background: in, optional, type=string, default='background'
;        The name of the background color. May be specified as a color table index number, as well.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;        The character size for axes annotations. Uses cgDefCharSize to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     color: in, optional, type=string, default='black'
;        The name of the data or symbol color. May be specified as a color table index or color triple, as well.
;        May be a vector of the same length as the input data vectors.
;     coefficient: out, optional, type=double
;        The Pearson correlation coefficient of the two data sets. Calculated with the IDL routine CORRELATE.
;     fcharsize: in, optional, type=float
;        The character size of the fit parameters that are written on the plot. This keyword is only
;        in effect if the `Fit` keyword is set. The default is the same as `Charsize`.
;     fcolor: in, optional, type=string, default='red'
;        The name of the color for the fitting line though the data
;     fthick: in, optional, type=integer, default=1
;        The thickness of the fitting line.
;     fillcolor: in, optional, type=string
;        If this keyword is set to the name of a color, that color will be used to "fill"
;        the area of the plot enclosed by the axes. Unfortunately, at this time, the `FillColor`
;        keyword can NOT be used with multiple plots, just single plots.
;     fit: in, optional, type=boolean, default=1
;        If this keyword is set to 1 (the default), a straight line is fit through the data
;        with the IDL routine LINFIT. If this keyword is set, the Pearson correlation coeffcient
;        and the equation of the fitted line is displayed on the scatter plot, unless the `NoDisplay`
;        keyword is set.
;     font: in, optional, type=integer, default=!P.Font
;        The type of font desired for axis annotation.
;     gcolor: in, optional, type=string, default='gray'
;        The name of the grid color. May be specified as a color table index number, as well.
;     glinestyle: in, optional, type=integer, default=1
;        The grid linestyle. Dotted by default. An integer from 0 to 5. See the IDL LineStyle 
;        graphics keyword documentation.
;     grid: in, optional, type=boolean, default=0
;        Set this keword to 1 to draw a grid on the plot.
;     isotropic: in, optional, type=boolean, default=0
;        A short-hand way of setting the `Aspect` keyword to 1.
;     layout: in, optional, type=intarr(3)
;        This keyword specifies a grid with a graphics window and determines where the
;        graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;        The grid is determined by the number of columns (ncolumns) by the number of 
;        rows (nrows). The location of the graphic is determined by the third number. The
;        grid numbering starts in the upper left (1) and goes sequentually by column and then
;        by row.
;     nodisplay, in, optional, type=boolean default=0
;        If this keyword is set, then the Pearson correlation coefficient and the equation
;        of the fitting line is not displayed on the plot. This keyword is only considered if
;        the `Fit` keyword is set.
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to draw the plot without erasing the display first.
;     outfilename: in, optional, type=string
;        If the `Output` keyword is set, the user will be asked to supply an output
;        filename, unless this keyword is set to a non-null string. In that case, the
;        value of this keyword will be used as the filename and there will be no dialog
;        presented to the user.
;     output: in, optional, type=string, default=""
;        Set this keyword to the type of output desired. Possible values are these::
;            
;            'PS'   - PostScript file
;            'EPS'  - Encapsulated PostScript file
;            'PDF'  - PDF file
;            'BMP'  - BMP raster file
;            'GIF'  - GIF raster file
;            'JPEG' - JPEG raster file
;            'PNG'  - PNG raster file
;            'TIFF' - TIFF raster file
;            
;        Or, you can simply set this keyword to the name of the output file, and the type of
;        file desired will be determined by the file extension. If you use this option, the
;        user will not be prompted to supply the name of the output file.  
;            
;        All raster file output is created through PostScript intermediate files (the
;        PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;        to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command. Output parameters can be set with cgWindow_SetDefs.
;     overplot: in, optional, type=boolean, default=0
;        Set this keyword if you wish to overplot data on an already exisiting set of
;        axes. It is like calling the IDL OPLOT command.
;     params: out, optional, type=double
;        The output line fitting parameters [intercept, slope].
;     position: in, optional, type=vector
;        The usual four-element position vector for the Plot comamnd. Only monitored and
;        possibly changed if the `Aspect` keyword is used.
;     psym: in, optional, type=integer, default=2
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 1 and 46.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     title: in, optional, type=string, default=""
;        The title of the plot.
;     traditional: in, optional, type=boolean, default=0
;        If this keyword is set, the traditional color scheme of a black background for
;        graphics windows on the display is used and PostScript files always use a white background.
;     window: in, optional, type=boolean, default=0
;        Set this keyword to replace all the commands in a current cgWindow or to
;        create a new cgWindow for displaying this command.
;     xrange: in, optional, type=float
;        The X range of the plot.
;     xstyle: in, optional, type=integer, default=0
;        The value sent to the XStyle keyword for the Plot command.
;     xticklen: in, optional, type=float
;        The X tick length. Will be set to 1.0 if the `Grid` keyword is set.
;     xtitle: in, optional, type=string', default=""
;        The X title of the plot.
;     yrange: in, optional, type=float
;        The Y range of the plot.
;     ystyle: in, optional, type=integer, default=0
;        The value sent to the YStyle keyword for the Plot command.
;     yticklen: in, optional, type=float
;        THe Y tick length. Will be set to 1.0 if the `Grid` keyword is set.
;     ytitle: in, optional, type=string', default=""
;        The Y title of the plot.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot command is allowed in the program.
;        
; :Examples:
;    Use to compare two data sets::
;       data_1 = cgDemoData(1)+ RandomU(seed, 101) * 10
;       data_2 = cgDemoData(1)+ RandomU(seed, 101) * 10
;       cgScatter2D, data_1, data_2 
;       
;    Add a grid to the plot::
;       cgScatter2D, data_1, data_2, SymColor='navy', FillColor='rose', /Grid
;       
;    Output the plot to a PNG file::
;       cgScatter2D, data_1, data_2,  SymColor='navy', /Grid, FillColor='rose', Output='scatter.png'
;    
;    Display the plot in a resizeable graphics window::
;       cgScatter2D, data_1, data_2,  SymColor='navy', /Grid, FillColor='rose', /Window
;    
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
;        Written, 12 January 2012. DWF.
;        Removed an extra COLOR keyword and changed an OPLOT command to a PLOTS command
;           to allow a vector of colors to be used for the scatter points.
;         Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgScatter2D, x, y, $
    AddCmd=addcmd, $
    Aspect=aspect, $
    AxisColor=saxiscolor, $
    AxesColor=saxescolor, $
    Background=sbackground, $
    Charsize=charsize, $
    Color=scolor, $
    Coefficient=coefficient, $
    FCharsize=fcharsize, $
    FColor=sfcolor, $
    FThick=fthick, $
    FillColor=sfillcolor, $
    Fit=fit, $
    Font=font, $
    GColor=sgcolor, $
    GLinestyle=glinestyle, $
    Grid=grid, $
    Isotropic=isotropic, $
    Layout=layout, $
    NoDisplay=nodisplay, $
    NoErase=noerase, $
    OutFilename=outfilename, $
    Output=output, $
    Overplot=overplot, $
    Params=params, $
    Position=position, $
    PSym=psym, $
    SymSize=symsize, $
    Title=title, $
    Traditional=traditional, $
    Window=window, $
    XRange=xrange, $
    XStyle=xstyle, $
    XTickLen=xticklen, $
    XTitle=xtitle, $
    YRange=yrange, $
    YStyle=ystyle, $
    YTicklen=yticklen, $
    YTitle=ytitle, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        IF (N_Elements(output) NE 0) THEN PS_END, /NOFIX
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgScatter2D, x, y'
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
        cgWindow, 'cgScatter2D', x, y, $
            Aspect=aspect, $
            AxisColor=saxiscolor, $
            AxesColor=saxescolor, $
            Background=sbackground, $
            Charsize=charsize, $
            Color=scolor, $
            Coefficient=coefficient, $
            FCharsize=fcharsize, $
            FColor=sfcolor, $
            FThick=fthick, $
            FillColor=sfillcolor, $
            Fit=fit, $
            Font=font, $
            GColor=sgcolor, $
            GLinestyle=glinestyle, $
            Grid=grid, $
            Isotropic=isotropic, $
            Layout=layout, $
            NoDisplay=nodisplay, $
            NoErase=noerase, $
            OutFilename=outfilename, $
            Output=output, $
            Overplot=overplot, $
            Params=params, $
            Position=position, $
            PSym=psym, $
            SymSize=symsize, $
            Title=title, $
            Traditional=traditional, $
            XRange=xrange, $
            XStyle=xstyle, $
            XTickLen=xticklen, $
            XTitle=xtitle, $
            YRange=yrange, $
            YStyle=ystyle, $
            YTicklen=yticklen, $
            YTitle=ytitle, $
            ADDCMD=1, $
            _Extra=extra
             RETURN
       ENDIF
        
        ; Open a new window or replace the current commands, as required.
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        cgWindow, 'cgScatter2D', x, y, $
            Aspect=aspect, $
            AxisColor=saxiscolor, $
            AxesColor=saxescolor, $
            Background=sbackground, $
            Charsize=charsize, $
            Color=scolor, $
            Coefficient=coefficient, $
            FCharsize=fcharsize, $
            FColor=sfcolor, $
            FThick=fthick, $
            FillColor=sfillcolor, $
            Fit=fit, $
            Font=font, $
            GColor=sgcolor, $
            GLinestyle=glinestyle, $
            Grid=grid, $
            Isotropic=isotropic, $
            Layout=layout, $
            NoDisplay=nodisplay, $
            NoErase=noerase, $
            OutFilename=outfilename, $
            Output=output, $
            Overplot=overplot, $
            Params=params, $
            Position=position, $
            PSym=psym, $
            SymSize=symsize, $
            Title=title, $
            Traditional=traditional, $
            XRange=xrange, $
            XStyle=xstyle, $
            XTitle=xtitle, $
            XTickLen=xticklen, $
            YRange=yrange, $
            YStyle=ystyle, $
            YTicklen=yticklen, $
            YTitle=ytitle, $
            REPLACECMD=replaceCmd, $
            _Extra=extra
            
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
    
       IF N_Elements(outputSelection) EQ 0 THEN outputSelection = StrUpCase(output)
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
       
       ; Do you need a filename?
       IF ( (N_Elements(outfilename) EQ 0) || (outfilename EQ "") ) THEN BEGIN 
            filename = 'cgplot' + ext
            outfilename = cgPickfile(FILE=filename, TITLE='Select Output File Name...', $
                FILTER=ext, /WRITE)
            IF outfilename EQ "" THEN RETURN
       ENDIF
       
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

    ; Check the color keywords.
    traditional = Keyword_Set(traditional)
    background = cgDefaultColor(sbackground, /BACKGROUND, TRADITIONAL=traditional)
    IF (N_Elements(saxisColor) EQ 0) && (N_Elements(saxesColor) NE 0) THEN saxisColor = saxesColor
    axisColor = cgDefaultColor(saxisColor, TRADITIONAL=traditional)
    color = cgDefaultColor(sColor, DEFAULT=axisColor, TRADITIONAL=traditional)
    fcolor = cgDefaultColor(sfColor, DEFAULT='red', TRADITIONAL=traditional)
    gcolor = cgDefaultColor(sgColor, DEFAULT='gray', TRADITIONAL=traditional)
    
    ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font)
    
    ; Other keywords.
    SetDefaultValue, fit, 1
    SetDefaultValue, fcharsize, charsize
    SetDefaultValue, fthick, 1
    IF !D.Name EQ 'PS' THEN fthick = 3*fthick
    SetDefaultValue, grid, 0, /Boolean
    SetDefaultValue, glinestyle, 1
    IF grid THEN BEGIN
        xticklen = 1
        yticklen = 1
    ENDIF
    SetDefaultValue, symsize, 1.0
    IF Keyword_Set(isotropic) THEN aspect = 1.0
    IF N_Elements(psym) EQ 0 THEN psym = 2 ELSE psym = 1 > psym 
    IF N_Elements(xrange) EQ 0 THEN BEGIN
        range = Max(x) - Min(x)
        IF Min(x) NE 0 THEN minx = Min(x)-(range*0.05) ELSE minx = 0.0
        xrange = [minx, Max(x)+(range*0.05)]
    ENDIF
    IF N_Elements(yrange) EQ 0 THEN BEGIN
        range = Max(y) - Min(y)
        IF Min(y) NE 0 THEN miny = Min(y)-(range*0.05) ELSE miny = 0.0
        yrange = [miny, Max(y)+(range*0.05)]
    ENDIF
    SetDefaultValue, xstyle, 0
    SetDefaultValue, ystyle, 0
    
    IF (N_Elements(aspect) NE 0) AND (Total(!P.MULTI) EQ 0) THEN BEGIN
    
        ; If position is set, then fit the plot into those bounds.
        IF (N_Elements(position) GT 0) THEN BEGIN
          trial_position = Aspect(aspect, margin=0.)
          trial_width = trial_position[2]-trial_position[0]
          trial_height = trial_position[3]-trial_position[1]
          pos_width = position[2]-position[0]
          pos_height = position[3]-position[1]

          ; Same logic as cgImage: try to fit image width, then if you can't get the right aspect
          ; ratio, fit the image height instead.
          fit_ratio = pos_width / trial_width
          IF trial_height * fit_ratio GT pos_height THEN $
             fit_ratio = pos_height / trial_height

          ; new width and height
          trial_width *= fit_ratio
          trial_height *= fit_ratio

          ; calculate position vector based on trial_width and trial_height
          position[0] += 0.5*(pos_width - trial_width)
          position[2] -= 0.5*(pos_width - trial_width)
          position[1] += 0.5*(pos_height - trial_height)
          position[3] -= 0.5*(pos_height - trial_height)
        ENDIF ELSE position=Aspect(aspect)   ; if position isn't set, just use output of Aspect
        
    ENDIF
    
    ; If we don't have a position yet, and we are not doing multiple plots, define a position.
    IF (N_Elements(position) EQ 0) && (Total(!P.Multi) EQ 0) THEN position = [0.15, 0.125, 0.9, 0.9]
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
         IF ~noerase THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                IF Keyword_Set(overplot) NE 1 THEN BEGIN
                
                    ; Save the current system variables. Will need to restore later.
                    bangx = !X
                    bangy = !Y
                    bangp = !P
                    
                    ; Draw the plot that doesn't draw anything.
                    Plot, x, y, POSITION=position, CHARSIZE=charsize, /NODATA, $
                        FONT=font, XRANGE=xrange, YRANGE=yrange, _STRICT_EXTRA=extra  
                    
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
                
                ENDIF
            ENDIF ELSE tempNoErase = noerase
        ENDIF ELSE tempNoErase = noerase
     ENDIF ELSE tempNoErase = noerase
 
    
    ; Load the drawing colors. If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN axiscolor = cgColor(axiscolor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    IF Size(gcolor, /TNAME) EQ 'STRING' THEN gcolor = cgColor(gcolor)
    IF Size(fcolor, /TNAME) EQ 'STRING' THEN fcolor = cgColor(fcolor)
    IF Size(background, /TNAME) EQ 'STRING' THEN background = cgColor(background)
    
    ; Do you need a plot fill color?
    IF (N_Elements(sfillColor) NE 0) && (Total(!P.Multi) EQ 0) THEN BEGIN
        p = position
        fillColor = cgColor(sfillColor)
        IF !D.Name NE 'PS' THEN cgErase, Color=background
        PolyFill, [p[0], p[0], p[2], p[2], p[0]], /Normal, $
                  [p[1], p[3], p[3], p[1], p[1]], Color=fillcolor
        tempNoErase = 1
    ENDIF ELSE BEGIN
         IF (N_Elements(sfillColor) NE 0) && (Total(!P.Multi) NE 0) THEN BEGIN
             Message, 'Cannot use the FillColor keyword with multiple plots.', /Informational
         ENDIF
    ENDELSE
    
    ; Check for symbols in titles.
    IF N_Elements(title) NE 0 THEN title = cgCheckForSymbols(title)
    IF N_Elements(xtitle) NE 0 THEN xtitle = cgCheckForSymbols(xtitle)
    IF N_Elements(ytitle) NE 0 THEN ytitle = cgCheckForSymbols(ytitle)    

    ; Draw the plot.
    IF Keyword_Set(overplot) THEN BEGIN
       IF psym LE 0 THEN OPlot, dep, indep, COLOR=color, _EXTRA=extra
    ENDIF ELSE BEGIN
      IF grid THEN BEGIN
          bangxgrid = !X
          bangygrid = !Y
          bangpgrid = !P
          Plot, x, y, BACKGROUND=background, COLOR=gcolor, CHARSIZE=charsize, $
                POSITION=position, /NODATA, NOERASE=tempNoErase, FONT=font, $
                XTICKLEN=xticklen, YTICKLEN=yticklen, XRANGE=xrange, YRANGE=yrange, $
                XGRIDSTYLE=glinestyle, YGRIDSTYLE=glinestyle, XSTYLE=4 AND xstyle, YSTYLE=4 AND ystyle, $
                _STRICT_EXTRA=extra, XTickFormat='(A1)', YTickFormat='(A1)', $
                XTITLE="", YTITLE="", TITLE=""
           
          aftergridx = !X
          aftergridy = !Y
          aftergridp = !P
          !X = bangxgrid
          !Y = bangygrid    
          !P = bangpgrid 
          Plot, x, y, COLOR=axiscolor, CHARSIZE=charsize, $
                POSITION=position, /NODATA, NOERASE=(!P.Multi[0] GT 0) ? 0 : 1, FONT=font, $
                XRANGE=xrange, YRANGE=yrange, $
                XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra, $
                XTITLE=xtitle, YTITLE=ytitle, TITLE=title
          IF !P.Multi[0] EQ 0 THEN BEGIN
             !X = aftergridx
             !Y = aftergridy
             !P = aftergridp
          ENDIF
      ENDIF ELSE BEGIN
          Plot, x, y, BACKGROUND=background, COLOR=axiscolor, CHARSIZE=charsize, $
                POSITION=position, /NODATA, NOERASE=tempNoErase, FONT=font, $
                XTICKLEN=xticklen, YTICKLEN=yticklen, XRANGE=xrange, YRANGE=yrange, $
                XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra, $
                XTITLE=xtitle, YTITLE=ytitle, TITLE=title
      ENDELSE
    ENDELSE
    IF Abs(psym) GT 1 THEN BEGIN
        PlotS, x, y, COLOR=color, PSYM=cgSymCat(Abs(psym), _Extra=extra), $
           SYMSIZE=symsize, _EXTRA=extra
    ENDIF 
    
    ; Calculate the Pearson correlation coefficient.
    coefficient = Correlate(x, y, /DOUBLE)
    
    ; Add the fitting line, if needed.
    IF fit THEN BEGIN
       params = LinFit(x, y, /Double, YFIT=yfit)
       OPlot, x, yfit, COLOR=fcolor, THICK=fthick
       
       IF coefficient GE 0 THEN BEGIN
          yloc = !Y.Window[1] - 0.05
          xloc = !X.Window[0] + 0.05
          alignment = 0.0
          y1loc = yloc - ( 2.0 * (!D.Y_CH_SIZE / Float(!D.Y_Size) ) )
       ENDIF ELSE BEGIN
          yloc = !Y.Window[1] - 0.05
          xloc = (!X.Window[1]-!X.Window[0]) * (3.0/5.0) + !X.Window[0]
          alignment = 0.0
          y1loc = yloc - (2.0 * (!D.Y_CH_SIZE / Float(!D.Y_Size)))
       ENDELSE
       
       ; Write the correlation coefficient and the fitting equation on the display,
       ; if allowed to.
       IF Keyword_Set(nodisplay) EQ 0 THEN BEGIN
           cgText, xloc, yloc, /Normal, 'R = ' + String(coefficient, Format='(F0.3)'), $
               Charsize=fcharsize
           cgText, xloc, y1loc, /Normal, 'y = ' + String(params[1], Format='(F0.2)') $
               + 'x + ' + String(params[0], Format='(F0.2)'), Charsize=fcharsize
       ENDIF
    ENDIF
         
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
    
    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti
    
END
    