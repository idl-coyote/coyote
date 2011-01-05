; docformat = 'rst'
;
; NAME:
;   FSC_Contour
;
; PURPOSE:
;   The purpose of FSC_Contour is to create a wrapper for the traditional IDL graphics
;   command, Contour. The Contour command has a number of deficiencies that make it
;   difficult to use in a modern computing environment. FSC_Contour corrects these
;   deficiencies and allows the user to produce traditional contour plots in a device
;   and machine independent manner.
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
; :Description:
;   The purpose of FSC_Contour is to create a wrapper for the traditional IDL graphics
;   command, Contour. The Contour command has a number of deficiencies that make it
;   difficult to use in a modern computing environment. FSC_Contour corrects these
;   deficiencies and allows the user to produce traditional contour plots in a device
;   and machine independent manner.
;
; :Categories:
;    Graphics
;    
; :Params:
;    data: in, required, type=any
;         A one- or two-dimensional array containing the values that make 
;         up the contour surface.
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates for
;         the contour surface.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates for
;         the contour surface.
;       
; :Keywords:
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, optional, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     c_colors: in, optional, type=integer/string vector
;        Set to the index values of the contour colors or to named colors. Must contain
;        the same number of colors as the number of requested contour levels.
;     c_labels: in, optional, type=integer vector
;        A vector that specifies which contour levels to label. If used, the LABEL
;        keyword is ignored.
;     cell_fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created using the "cell fill" method.
;        This keyword should always be set if displaying filled contours on map projections
;        or if missing data is present in the data you are contouring.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, same as AXISCOLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created.
;     irregular: in, optional, type=boolean
;        If this keyword is set, the data, x, and y input parameters are taken to be
;        irregularly gridded data, the the data is gridded for use in the contour plot
;        using the Triangulate and Trigrid method. The resolution of the gridded output
;        is set by the RESOLUTION keyword.
;     label: in, optional, type=integer, default=1
;        An number that tells how to label contour levels. A 0 means
;        no contour levels are labelled. A 1 (the default) means all contour levels are
;        labelled. A 2 means label every 2nd contour level is labelled. A 3 means every 
;        3rd contour level is labelled, and so on.
;     levels: in, optional, type=any
;         A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;         NLEVELS is used to construct N equally-spaced contour levels.
;     missingvalue: in, optional, type=any
;        Use this keyword to identify any missing data in the input data values.
;     nlevels: in, optional, type=integer, default=6
;        If the Contour plot LEVELS keyword is not used, this keyword will produce this
;        number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;        this keyword actually works!
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to prevent the window from erasing the contents before displaying
;        the contour plot.
;     overplot: in, optional, type=boolean
;        Set this keyword to overplot the contours onto a previously established
;        data coordinate system.
;     resolution: in, optional, type=integer array, default=[41\,41]
;        If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;        in a two element integer array of the final gridded data that is sent to the 
;        contour plot.
;     traditional: in, optional, type=boolean, default=0
;         If this keyword is set, the traditional color scheme of a black background for
;         graphics windows on the display is used and PostScript files always use a white background.
;     window: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the plot in a resizable graphics window.
;     xstyle: in, optional, type=integer, default=1
;        If unused in the program, set to 1 to force exact axis scaling.
;     ystyle: in, optional, type=integer, default=1
;        If unused in the program, set to 1 to force exact axis scaling.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDL Contour command is allowed in the program.
;
; :Examples:
;    Use as you would use the IDL CONTOUR command::
;       data = dist(51)
;       FSC_Contour, data
;       LoadCT, 33
;       FSC_Contour, data, /FILL
;       FSC_Contour, data, /OVERPLOT
;       
;       See http://www.dfanning.com/graphics_tips/fsc_contour.html for additional examples.
;
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
;        Written, 11 November 2010. DWF.
;        Restored the CELL_FILL keyword, which had been accidentally removed in
;           the earlier version. 12 November 2010. DWF.
;        Add the ability to specify the contour colors as color names. 16 November 2010. DWF.
;        Now setting decomposition state by calling SetDecomposedState. 16 November 2010. DWF.
;        Final color table restoration skipped in Z-graphics buffer. 17 November 2010. DWF.
;        Background keyword now applies in PostScript file as well. 17 November 2010. DWF.
;        Many changes after BACKGROUND changes to get !P.MULTI working again! 18 November 2010. DWF.
;        Fixed a small problem with the OVERPLOT keyword. 18 Nov 2010. DWF.
;        Changes so that color variables don't change type. 23 Nov 2010. DWF.
;        Added WINDOW keyword to allow graphic to be displayed in a resizable graphics window. 8 Dec 2010. DWF
;        Modifications to allow FSC_Contour to be drop-in replacement for old Contour commands in 
;            indexed color mode. 24 Dec 2010. DWF.
;        Previous changes introduced problems with OVERPLOT that have now been fixed. 28 Dec 2010. DWF.
;        Set NOERASE keyword from !P.NoErase system variable when appropriate. 28 Dec 2010. DWF.
;        Additional problems with NOERASE discovered and solved. 29 Dec 2010. DWF.
;        Change to DECOMPOSED color was using incorrect color tables. 29 Dec 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.
;        Still working on getting contour colors to work in decomposed color mode in all 
;             circumstances. 2 Jan 2011. DWF.
;        Fixed problem with FILL when no contour colors (C_COLORS) are specified. 3 Jan 2011. DWF.
;        Fixed a problem that preventing output keyword (e.g., PATH_INFO) from being returned properly. 
;             3 Jan 2011. DWF.
;        Fixed a problem calculating NLEVELS when LEVELS keyword was used instead. 3 Jan 2011. DWF.
;        TVLCT commands protected from NULL device. 4 Jan 2011. DWF.
;         
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Contour, data, x, y, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    C_COLORS=c_colors, $
    C_LABELS=c_labels, $
    CELL_FILL=cell_fill, $
    COLOR=scolor, $
    FILL=fill, $
    IRREGULAR=irregular, $
    LABEL=label, $
    LEVELS=levels, $
    NLEVELS=nlevels, $
    NOERASE=noerase, $
    MISSINGVALUE=missingvalue, $
    OVERPLOT=overplot, $
    RESOLUTION=resolution, $
    TRADITIONAL=traditional, $
    WINDOW=window, $
    XSTYLE=xstyle, $
    YSTYLE=ystyle, $
    _REF_EXTRA=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) AND (Keyword_Set(overplot) EQ 0) THEN BEGIN
    
        FSC_Window, 'FSC_Contour', data, x, y, $
            AXISCOLOR=saxiscolor, $
            AXESCOLOR=saxescolor, $
            BACKGROUND=sbackground, $
            C_COLORS=c_colors, $
            C_LABELS=c_labels, $
            CELL_FILL=cell_fill, $
            COLOR=scolor, $
            FILL=fill, $
            IRREGULAR=irregular, $
            LABEL=label, $
            LEVELS=levels, $
            NLEVELS=nlevels, $
            NOERASE=noerase, $
            MISSINGVALUE=missingvalue, $
            OVERPLOT=overplot, $
            RESOLUTION=resolution, $
            TRADITIONAL=traditional, $
            XSTYLE=xstyle, $
            YSTYLE=ystyle, $
            _Extra=extra
            
         RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Elements(data) EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: FSC_Contour, data, x, y, NLEVELS=10'
        RETURN
    ENDIF
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)
    
    ndims = Size(data, /N_DIMENSIONS)
    s = Size(data, /DIMENSIONS)
    CASE ndims OF
        1: BEGIN
           IF N_Elements(x) EQ 0 THEN xgrid = Indgen(s[0]) ELSE xgrid = x
           IF N_Elements(y) EQ 0 THEN ygrid = Indgen(s[0]) ELSE ygrid = y
           END
        2: BEGIN
           IF N_Elements(x) EQ 0 THEN xgrid = Indgen(s[0]) ELSE xgrid = x
           IF N_Elements(y) EQ 0 THEN ygrid = Indgen(s[1]) ELSE ygrid = y
           END
        ELSE: Message, 'Contour data must be 1D or 2D.'
    ENDCASE
    
    ; Get the current color table vectors.
    IF (!D.Name NE 'NULL') THEN TVLCT, rr, gg, bb, /GET
    
    ; Check the keywords.
    IF N_Elements(sbackground) EQ 0 THEN BEGIN
        IF Keyword_Set(overplot) || Keyword_Set(noerase) THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                background = 'WHITE' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF (!D.Window LT 0) &&  Keyword_Set(noerase) THEN BEGIN
                        Window
                        IF ~Keyword_Set(traditional) THEN FSC_Erase, 'WHITE'
                    ENDIF
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN background = 'WHITE'
                    IF (Total(pixel) EQ 0) THEN background = 'BLACK'
                    IF N_Elements(background) EQ 0 THEN background = 'OPPOSITE'
                ENDIF ELSE background = 'OPPOSITE'
           ENDELSE
        ENDIF ELSE BEGIN
           IF Keyword_Set(traditional) THEN BEGIN
              IF ((!D.Flags AND 256) NE 0) THEN background = 'BLACK' ELSE background = 'WHITE'
           ENDIF ELSE background = 'WHITE' 
        ENDELSE
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
                    IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN FSC_Erase, background
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) OR (background EQ 'WHITE') THEN saxisColor = 'BLACK'
                    IF (Total(pixel) EQ 0) OR (background EQ 'BLACK') THEN saxisColor = 'WHITE'
                    IF N_Elements(saxisColor) EQ 0 THEN saxisColor = 'OPPOSITE'
                ENDIF ELSE saxisColor = 'OPPOSITE'
          ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(saxisColor) EQ 0 THEN axisColor = !P.Color ELSE axisColor = saxisColor
    IF Size(saxisColor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN saxisColor = Byte(saxisColor)
    IF Size(axisColor, /TYPE) LE 2 THEN axisColor = StrTrim(Fix(axisColor),2)
    
    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN sColor = 'BLACK'
       ENDIF
       IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                IF StrUpCase(background) EQ 'BLACK' THEN background = 'WHITE'
                sColor = 'BLACK' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF !D.Window LT 0 THEN Window
                    IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN FSC_Erase, background
                    pixel = TVRead(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) OR (background EQ 'WHITE') THEN sColor = 'BLACK'
                    IF (Total(pixel) EQ 0) OR (background EQ 'BLACK') THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN sColor = 'OPPOSITE'
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(sColor) EQ 0 THEN color = !P.Color ELSE  color = sColor
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)
    
    ; If color is the same as background, do something.
    IF ColorsAreIdentical(background, color) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
            IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN FSC_Erase, background
        ENDIF
        color = 'OPPOSITE'
    ENDIF
    IF ColorsAreIdentical(background, axiscolor) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
            IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN FSC_Erase, background
        ENDIF
        axiscolor = 'OPPOSITE'
    ENDIF
    
    fill = Keyword_Set(fill)
    irregular = Keyword_Set(irregular)
    IF N_Elements(label) EQ 0 THEN label = 1
    IF N_Elements(resolution) EQ 0 THEN resolution=[41,41]
    IF N_Elements(nlevels) EQ 0 THEN BEGIN
        IF N_Elements(levels) EQ 0 THEN nlevels = 6 ELSE nlevels = N_Elements(levels)
    ENDIF    
    IF N_Elements(xstyle) EQ 0 THEN xstyle=1
    IF N_Elements(ystyle) EQ 0 THEN ystyle=1
    IF N_Elements(missingvalue) NE 0 THEN BEGIN
        IF  (Size(data, /TNAME) NE 'FLOAT') $
        AND (Size(data, /TNAME) NE 'DOUBLE') $
        THEN contourData = Float(data) ELSE contourData = data
        missingIndices = Where(contourData EQ missingValue[0], missingCount)
        IF missingCount GT 0 THEN BEGIN
            contourData[missingIndices] = !Values.F_NAN
            IF Keyword_Set(fill) THEN BEGIN
               fill = 0
               cell_fill = 1
            ENDIF
        ENDIF
    ENDIF ELSE contourData = data
    
    ; Handle gridding of irregular data.
    IF irregular THEN BEGIN
        Triangulate, xgrid, ygrid, triangles
        contourData = Trigrid(xgrid, ygrid, contourData, triangles, $
            NX=resolution[0], NY=resolution[1], $
            XGRID=xgrid, YGRID=ygrid, MISSING=!Values.F_NAN)
        IF Keyword_Set(fill) THEN BEGIN
           fill = 0
           cell_fill = 1
        ENDIF
     ENDIF
  
    ; Do you need to calculate levels.
    IF N_Elements(levels) EQ 0 THEN BEGIN
        minData = Min(contourData, /NAN)
        maxData = Max(contourData, /NAN)
        levels = ((maxData - minData) / Float(nlevels)) * Indgen(nlevels) + minData
    ENDIF
    
    ; Need to make sure contour colors are integers if they are indices. 
    IF N_Elements(c_colors) NE 0 THEN BEGIN
        IF Size(c_colors, /TNAME) NE 'STRING' THEN BEGIN
           IF (Size(c_colors, /TYPE) EQ 3) && (Max(c_colors) LE 255) THEN c_colors = Fix(c_colors)
        ENDIF
    ENDIF ELSE BEGIN
        IF Keyword_Set(fill) THEN BEGIN
            IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
            rrr = Congrid(rrr, nlevels)
            ggg = Congrid(ggg, nlevels)
            bbb = Congrid(bbb, nlevels)
            IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, 1
            c_colors = StrTrim(Indgen(nlevels)+1,2)
        ENDIF ELSE BEGIN
            c_colors = Replicate(color, nlevels)
        ENDELSE
        IF Size(c_colors, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN c_colors = Byte(c_colors)
        IF Size(c_colors, /TYPE) LE 2 THEN c_colors = StrTrim(Fix(c_colors),2)
    ENDELSE
    ; Set up the appropriate contour labeling. Only can do if C_LABELS not passed in.
    IF N_Elements(c_labels) EQ 0 THEN BEGIN
        indices = Indgen(N_Elements(levels))
        IF label EQ 0 THEN BEGIN
           c_labels = Replicate(0,N_Elements(levels))
        ENDIF ELSE c_labels = Reverse((indices MOD label) EQ 0)
    ENDIF

    ; Going to have to do all of this in indexed color.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    
    ; Load the drawing colors, if needed.
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN axiscolor = FSC_Color(axiscolor)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = FSC_Color(color)
    IF Size(background, /TNAME) EQ 'STRING' THEN background = FSC_Color(background)
    IF (Size(c_colors, /TYPE) LE 2) AND (Size(c_colors, /TYPE) NE 0) THEN c_colors = StrTrim(Fix(c_colors),2)
    IF Size(c_colors, /TNAME) EQ 'STRING' THEN c_colors = FSC_Color(c_colors)
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
       IF Keyword_Set(noerase) EQ 0 THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                IF Keyword_Set(overplot) NE 1 THEN BEGIN

                    ; Save the current system variables. Will need to restore later.
                    bangx = !X
                    bangy = !Y
                    bangp = !P
 
                    ; Make sure axis are turned off. I don't really want to draw anything,
                    ; just advance !P.MULTI or "erase" the display for the next plot.
                    IF BitGet(xstyle, 2) NE 1 THEN xxstyle = xstyle + 4 ELSE xxstyle = xstyle
                    IF BitGet(ystyle, 2) NE 1 THEN yystyle = xstyle + 4 ELSE yystyle = ystyle
                    
                    ; Draw the plot that doesn't draw anything.
                     Contour, contourData, xgrid, ygrid, COLOR=axiscolor, $
                        BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=xstyle, $
                        _STRICT_EXTRA=extra, /NODATA
                    
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
     
    ; Storing these system variable is *required* to make !P.MULTI work correctly
    ; when doing filled contours. Do not delete!
    bangx = !X
    bangy = !Y
    bangp = !P
    
    ; If you are not overploting, draw the contour plot now. Only the axes are
    ; drawn here, no data.
    IF Keyword_Set(overplot) EQ 0 THEN BEGIN
    
        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, $
            BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
            _STRICT_EXTRA=extra, /NODATA, NOERASE=tempNoErase
                    
    ENDIF
    
    ; This is where we actually draw the data.
    Contour, contourData, xgrid, ygrid, FILL=fill, CELL_FILL=cell_fill, COLOR=color, $
        LEVELS=levels, C_Labels=c_labels, C_COLORS=c_colors, $
        XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra, /OVERPLOT
        
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        !X = afterX
        !Y = afterY
        !P = afterP
    ENDIF
        
    ; If you filled the contour plot, you will need to repair the axes. We have
    ; to be careful that we don't advance a !P.MULTI plot when we do this. Thus,
    ; we have to take care with the system variables again.
    IF Keyword_Set(fill) OR Keyword_Set(cell_fill) THEN BEGIN
        
        ; Get the current system variables, so we can restore them later.
        newx = !X
        newy = !y
        newP = !P
        
        ; Set the system variables to their original values.
        !X = bangx
        !Y = bangy
        !P = bangp
        
        ; Repair the plot.
        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, C_COLORS=c_colors, $
           BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
           _STRICT_EXTRA=extra, /NODATA, NOERASE=1
          
        ; Restore the system variables so that it appears we didn't do what we just did.
        !X = newx 
        !y = newy 
        !P = newP

    ENDIF
    
    ; Restore the decomposed color state if you can.
    SetDecomposedState, currentState
    
    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') AND (!D.Name NE 'NULL') THEN TVLCT, rr, gg, bb
     
END
    