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
;     axiscolor: in, optional, type=string/integer, default=Same as 'color'
;        If this keyword is a string, the name of the axis color. By default, same as 'color'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, optional, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     c_labels: in, optional, type=integer vector
;        A vector that specifies which contour levels to label. If used, the SKIPLABEL
;        keyword is ignored.
;     cell_fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created using the "cell fill" method.
;        This keyword should always be set if displaying filled contours on map projections
;        or if missing data is present in the data you are contouring.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     fill: in, optional, type=boolean, default=0
;        Set to indicate filled contours should be created.
;     irregular: in, optional, type=boolean
;        If this keyword is set, the data, x, and y input parameters are taken to be
;        irregularly gridded data, the the data is gridded for use in the contour plot
;        using the Triangulate and Trigrid method. The resolution of the gridded output
;        is set by the RESOLUTION keyword.
;     levels: in, optional, type=any
;         A vector of data levels to contour. If used, NLEVELS is ignored. If missing, 
;         NLEVELS is used to construct N equally-spaced contour levels.
;     missingvalue: in, optional, type=any
;        Use this keyword to identify any missing data in the input data values.
;     nlevels: in, optional, type=integer, default=6
;        If the Contour plot LEVELS keyword is not used, this keyword will produce this
;        number of equally spaced contour intervals. Unlike the Contour NLEVELS keyword,
;        this keyword actually works!
;     overplot: in, optional, type=boolean
;        Set this keyword to overplot the contours onto a previously established
;        data coordinate system.
;     resolution: in, optional, type=integer array, default=[41\,41]
;        If the IRREGULAR keyword is set, this keyword specifies the X and Y resolution
;        in a two element integer array of the final gridded data that is sent to the 
;        contour plot.
;     skiplabel: in, optional, type=integer, default=1
;        An number that tells how many labels to skip while labelling contours. A 1 means
;        all contour levels are labelled. A 2 indices every other contour level is labelled.
;        A 3, every third level is labelled, and so on. Set to 0 to prevent level labelling.
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
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO FSC_Contour, data, x, y, $
    AXISCOLOR=axiscolor, $
    AXESCOLOR=axescolor, $
    BACKGROUND=background, $
    C_LABELS=c_labels, $
    CELL_FILL=cell_fill, $
    COLOR=color, $
    FILL=fill, $
    IRREGULAR=irregular, $
    LEVELS=levels, $
    NLEVELS=nlevels, $
    MISSINGVALUE=missingvalue, $
    OVERPLOT=overplot, $
    RESOLUTION=resolution, $
    SKIPLABEL=skiplabel, $
    XSTYLE=xstyle, $
    YSTYLE=ystyle, $
    _Extra=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Elements(data) EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: FSC_Contour, data, x, y, NLEVELS=10'
        RETURN
    ENDIF
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
    TVLCT, rr, gg, bb, /GET
    
    ; Check the keywords.
    IF N_Elements(background) EQ 0 THEN background = 'white'
    IF N_Elements(color) EQ 0 THEN color = 'black'
    IF (N_Elements(axescolor) EQ 0) AND (N_Elements(axiscolor) EQ 0) THEN BEGIN
       axiscolor = color
    ENDIF
    IF N_Elements(axescolor) NE 0 THEN axiscolor = axescolor
    fill = Keyword_Set(fill)
    irregular = Keyword_Set(irregular)
    IF N_Elements(skiplabel) EQ 0 THEN skiplabel = 1
    IF N_Elements(resolution) EQ 0 THEN resolution=[41,41]
    IF (N_Elements(nlevels) EQ 0) AND (N_Elements(levels) EQ 0) THEN nlevels = 6
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
    
    ; Set up the appropriate contour labeling. Only can do if C_LABELS not passed in.
    IF N_Elements(c_labels) EQ 0 THEN BEGIN
        indices = Indgen(N_Elements(levels))
        IF skiplabel EQ 0 THEN BEGIN
           c_labels = Replicate(0,N_Elements(levels))
        ENDIF ELSE c_labels = Reverse((indices MOD skiplabel) EQ 0)
    ENDIF
        
    ; Load the drawing colors, if needed.
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN $
        axiscolor = FSC_Color(axiscolor, DECOMPOSED=0, 254)
    IF Size(color, /TNAME) EQ 'STRING' THEN $
        color = FSC_Color(color, DECOMPOSED=0, 253)
    IF Size(background, /TNAME) EQ 'STRING' THEN $
        background = FSC_Color(background, DECOMPOSED=0, 252)
    
    ; Going to have to do all of this in decomposed color.
    currentState = DecomposedColor()
    Device, Decomposed=0
    
    ; Draw the contour plot.
    IF ~Keyword_Set(overplot) THEN BEGIN
        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, $
            BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
            _STRICT_EXTRA=extra, /NODATA, OVERPLOT=0
    ENDIF
    Contour, contourData, xgrid, ygrid, FILL=fill, CELL_FILL=cell_fill, COLOR=color, $
        BACKGROUND=background, LEVELS=levels, C_Labels=c_labels, C_COLORS=c_colors, $
        XSTYLE=xstyle, YSTYLE=ystyle, _STRICT_EXTRA=extra, /OVERPLOT
        
    ; If you filled the contour plot, you will need to repair the axes.
    IF Keyword_Set(fill) OR Keyword_Set(cell_fill) THEN BEGIN
        Contour, contourData, xgrid, ygrid, COLOR=axiscolor, C_COLORS=c_colors, $
          BACKGROUND=background, LEVELS=levels, XSTYLE=xstyle, YSTYLE=ystyle, $
          _STRICT_EXTRA=extra, /NODATA, /NOERASE, OVERPLOT=0
    ENDIF
    
    ; Restore the decomposed color state if you can.
    IF currentState THEN Device, Decomposed=1
    
    ; Restore the color table.
    TVLCT, rr, gg, bb
    
END
    