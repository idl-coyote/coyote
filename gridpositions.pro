;+
; NAME:
;       GRIDPOSITIONS
;
; PURPOSE:
;
;       Sets up a column-row grid in the current graphics window in a fashion
;       similar to !P.MULTI, except that the grid can be confined to a portion
;       of the window, leaving room for color bars and other annotations.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Graphics
;
; CALLING SEQUENCE:
;
;       positions = GridPositions(columns, rows)
;
; INPUT_PARAMETERS:
;
;       columns:         The number of columns in the grid.
;
;       rows             The number of rows in the grid.
;
; OUTPUT_PARAMETERS:
;
;      positions:        A 4xN array, where N=(columns*rows), giving the positions of the grid in
;                        normalized coordinates.
;
; KEYWORDS:
;
;     INCHES:           If calculating the position in the PostScript device, indicates whether 
;                       the window size is given in centimeters (the default) or in inches. Set
;                       this keyword to 1 to indicate inches. Ignored for other devices.
;
;     LANDSCAPE:        If calculating the position in the PostScript device, set this keyword to
;                       indicate a landscript page. Ignored for other devices.
;
;     ORDER:            If this keyword is set to 0, the positions are calculated in row order. If
;                       set to 1, the positions are calculated in column order.
;
;     XEXTENT:          A one or two element array, with values from 0 to 1, giving the extent of
;                       the grid in the X direction. For example XEXTENT=[.2, .8] will position
;                       the X portion of the grid from 0.2 to 0.8 (normalized coordinates) in the
;                       window. If a scalar value, the XEXTENT will be assumed to be [0, value].
;
;     XMARGIN:          A one or two element array, giving the plot margin outside the grid position.
;                       That is to say, the normal grid position will be reduced by this margin. This
;                       corresponds to a plot margin, and as such is expressed in character units. See
;                       the on-line help for graphics keywords for additional information. If a scalar,
;                       the margin is taken from both the left and right sides of the position. In other
;                       words, XMARGIN=[value, value]. By default, [0.05,0.05], which will cause grid 
;                       positions to have just a bit of space between them.
;
;     XSIZE:            The X size of the window the positions are being calculated for. By
;                       default, !D.X_SIZE.
;
;     XEXTENT:          A one or two element array, with values from 0 to 1, giving the extent of
;                       the grid in the X direction. For example XEXTENT=[.2, .8] will position
;                       the X portion of the grid from 0.2 to 0.8 (normalized coordinates) in the
;                       window. If a scalar value, the XEXTENT will be assumed to be [0, value].
;
;     XMARGIN:          A one or two element array, giving the plot margin outside the grid position.
;                       That is to say, the normal grid position will be reduced by this margin. This
;                       corresponds to a plot margin, and as such is expressed in character units. See
;                       the on-line help for graphics keywords for additional information. If a scalar,
;                       the margin is taken from both the left and right sides of the position. In other
;                       words, XMARGIN=[value, value]. By default, [0,0], which will cause grid 
;                       positions to abut one another.
;
;     XSIZE:            The X size of the window the positions are being calculated for. By
;                       default, !D.X_SIZE.
;
;     YEXTENT:          A one or two element array, with values from 0 to 1, giving the extent of
;                       the grid in the X direction. For example YEXTENT=[.2, .8] will position
;                       the Y portion of the grid from 0.2 to 0.8 (normalized coordinates) in the
;                       window. If a scalar value, the YEXTENT will be assumed to be [0, value].
;
;     YMARGIN:          A one or two element array, giving the plot margin outside the grid position.
;                       That is to say, the normal grid position will be reduced by this margin. This
;                       corresponds to a plot margin, and as such is expressed in character units. See
;                       the on-line help for graphics keywords for additional information. If a scalar,
;                       the margin is taken from both the bottom and top sides of the position. In other
;                       words, YMARGIN=[value, value]. By default, [0,0], which will cause grid 
;                       positions to abut one another.
;
;     YSIZE:            The Y size of the window the positions are being calculated for. By
;                       default, !D.Y_SIZE.
;
; EXAMPLE:
;
;       To display four images, scaled differently, in the center of the display, with room
;       for a color bar:
;
;           positions = GridPositions(2, 2, YEXTENT=[0.15,0.85], XEXTENT=[0.2,0.8])
;           image = LoadData(11)
;           Erase, COLOR=FSC_Color('rose')
;           CTLoad, 25, /Brewer
;           TVImage, image, POSITION=positions[*,0]
;           TVImage, BytScl(Sobel(image)), POSITION=positions[*,1]
;           TVImage, BytScl(Hist_Equal(image)), POSITION=positions[*,2]
;           TVImage, BytScl(Median(image,7)), POSITION=positions[*,3]
;
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 17 March 2009.
;-
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
Function GridPositions, columns, rows, $
    INCHES=inches, $
    LANDSCAPE=landscape, $
    ORDER=order, $
    XEXTENT=xextent, $
    XMARGIN=xmargin, $
    XSIZE=xsize, $
    YEXTENT=yextent, $
    YMARGIN=ymargin, $
    YSIZE=ysize
    
    Compile_Opt idl2
    
    ; Return to caller with an error.
    On_Error, 2
    
    ; Check positional and keyword parameters.
    inches = Keyword_Set(inches)
    landscape = Keyword_Set(landscape)
    order = Keyword_Set(order)
    IF N_Elements(xsize) EQ 0 THEN xsize = !D.X_Size
    IF N_Elements(ysize) EQ 0 THEN ysize = !D.Y_Size
    IF N_Elements(xextent) EQ 0 THEN xextent = [0.0, 1.0]
    IF N_Elements(xextent) EQ 1 THEN xextent = [0, xextent]
    xextent = 0.0 > xextent < 1.0
    IF N_Elements(yextent) EQ 0 THEN yextent = [0.0, 1.0]
    IF N_Elements(yextent) EQ 1 THEN yextent = [0, yextent]
    yextent = 0.0 > yextent < 1.0
    IF N_Elements(columns) EQ 0 THEN columns = 2
    IF N_Elements(rows) EQ 0 THEN rows = 2
    IF N_Elements(xmargin) EQ 0 THEN xmargin = [0,0]
    IF N_Elements(xmargin) EQ 0 THEN xmargin = [xmargin, xmargin]
    IF N_Elements(ymargin) EQ 0 THEN ymargin = [0,0]
    IF N_Elements(ymargin) EQ 0 THEN ymargin = [ymargin, ymargin]
    
   
    ; Calculate window size.
    xs = xsize * (xextent[1] - xextent[0])
    ys = ysize * (yextent[1] - yextent[0])
    IF xs LE 0.0 THEN Message, 'X Extent results in a size of 0 length.'
    IF ys LE 0.0 THEN Message, 'Y Extent results in a size of 0 length.'
    
    ; Set up appropriately for the right device.
    CASE !D.NAME OF
        'PS': BEGIN
              Device, XSIZE=xs, YSIZE=ys, INCHES=inches, PORTRAIT=1-landscape, LANDSCAPE=landscape
              END
              
        'Z':  BEGIN
              Device, SET_RESOLUTION=[xs, ys]
              END
              
        ELSE: BEGIN
              thisWindow = !D.Window
              Window, /FREE, /PIXMAP, XSIZE=xs, YSIZE=ys
              END
    ENDCASE

    numpos = columns*rows
    thePositions = FltArr(4, numpos)
    !P.MULTI = [0, columns, rows, 0, order]
    FOR j=0,numpos-1 DO BEGIN
        Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, XMargin=xmargin, YMargin=ymargin
              thePositions[*,j] = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]] 
    ENDFOR
    !P.MULTI =0
    thePositions[[0,2], *] = thePositions[[0,2], *] *  (xextent[1] - xextent[0]) + xextent[0]
    thePositions[[1,3], *] = thePositions[[1,3], *] *  (yextent[1] - yextent[0]) + yextent[0]
    
    ; Clean up
    CASE !D.NAME OF
        'PS': Device, XSIZE=xsize, YSIZE=ysize, INCHES=inches, PORTRAIT=1-landscape, LANDSCAPE=landscape          
        'Z':       
        ELSE: BEGIN
              WDelete, !D.Window
              IF thisWindow NE -1 THEN WSet, thisWindow
              END
    ENDCASE
    
    RETURN, thePositions
    
END