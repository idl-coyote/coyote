; docformat = 'rst'
;
; NAME:
;   cgDCBar
;
; PURPOSE:
;  The purpose of this routine is to add a discrete color bar to
;  a graphics plot. A "discrete" color bar is one with a handful
;  of colors. Labels are centered beneath or beside the color fields.
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
; The purpose of this routine is to add a discrete color bar to
; a graphics plot. A "discrete" color bar is one with a handful
; of colors. Labels are centered beneath or beside the color fields.
; 
; .. image:: cgdcbar.png
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    colors, in, required
;       A vector of "colors" to be represented in the color bar. The vector can be a 
;       vector of color "names" that are known to cgColor. Or, it can be a vector of 
;       24-bit color values that can be decomposed into color triples. Or, it can be a 
;       vector of byte or integer values that can be used as indices into the current 
;       color table. If both colors and NCOLORS (see below) are undefined, a 10-element 
;       color table will be loaded and used.
;       
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    barcolor: in, optional, type=string
;       This is the name of a color known to cgCOLOR that can be used to draw the color 
;       bar outlines. By default, the same as specified with the COLOR keyword.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    color: in, optional, type=string, default="opposite"
;        The name of the color to use for color bar annotations. 
;    filename: in, optional, type=string
;        The name of a color table file that can be read by cgCOLOR. This allows you to 
;        specify your own color names for your own colors.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar tries to "fit" itself to the normalized
;       coordinates of the last graphics command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot. Because there are so many
;       ways this colorbar can be displayed, the "fit" may not always be a good one.
;       If you are fitting to an image, be sure to set the SAVE keyword on cgImage
;       to establish a data coordinate system.
;    font: in, optional, type=integer, default=!P.Font
;       Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;    labels: in, optional, type=string
;       An array of string labels that should annotate each color. Must be the same length
;       as the colors vector. Colors are labelled consecutively by default.
;    ncolors: in, optional, type=integer, default=256
;       An alternative way to specify the colors in the color bar is by
;       using the NCOLORS and BOTTOM keywords to locate the colors in the
;       current color table. The NCOLORS and BOTTOM keywords have the same
;       meaning as in the LOADCT, XLOADCT, XCOLORS, or cgCOLORBAR programs.
;    position: in, optional, type=float          
;       A four-element array of normalized coordinates in the same
;       form as the POSITION keyword on a plot. Default is [0.88, 0.10, 0.95, 0.90] 
;       for a vertical bar and [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;       See the FIT keyword, also.
;    right: in, optional, type=boolean, default=0   
;       This puts the title on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    rotate: in, optional, type=float, default=0.0
;       Set this keyword to a value that will rotate the label text.
;       Positive values between 0 and 180 degrees rotate in a counter-clockwise
;       sense. Negative values between 0 and 180 degress rotate in a 
;       clockwise sense.
;    spacing: in, optional, type=float, default=1.0
;       When labels are rotated, it is a little difficult to determine where,
;       exactly, they should be located. This keyword gives the user some control
;       over this location. The location "spacer" is multiplied by this amount. 
;       So, for example, to move the labels a little further away from the color bar, 
;       make this number greater than 1 (e.g, 1.25). To move the labels a little closer, 
;       use a number less than 1 (e.g, 0.75).
;    tcharsize: in, optional, type=float
;       The character size of the title. By default, set to cgDefCharsize().
;    treverse: in, optional, type=boolean, default=0
;       Set this keyword to reverse the direction of the title on a vertical color bar.
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    vertical: in, optional, type=boolean, default=0
;       Setting this keyword give a vertical color bar. The default is a horizontal color bar.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;    
; :Examples:
;       To display a 12 color horizontal color bar, labels with a three-letter
;       month abbreviation::
;       
;          cgDisplay
;          cgLoadCT, 5, NCOLORS=12, BOTTOM=1
;          cgDCBar, NCOLORS=12, BOTTOM=1, LABELS=cgMonths(/Abbreviation)
;       
;       To load a 5 color vertical color bar, with the labels rotated 45 degrees::
;
;          cgDisplay
;          labels = StrArr(5) 
;          FOR j=0,4 DO labels[j] = 'City ' + StrTrim(j+1,2)
;          colors = ['dodger blue', 'yellow', 'forest green', 'purple', 'tan']
;          cgDCBar, colors, LABELS=labels, ROTATE=45, /VERTICAL
;
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;       Written by: David W. Fanning, 15 March 2009.
;       Modification to code to avoid changing the colors vectors. 15 March 2009.
;       Added FONT keyword. 1 April 2009. DWF.
;       Code modified to support 24-bit PostScript printers. 23 September 2009. DWF.
;       Fixed a problem with determining visual depth in Z-buffer. 15 January 2010. DWF.
;       Added SPACING keyword and changed the default spacing on horizontal color bars slightly.  23 Apr 2010. DWF.
;       Modified the spacing of the labels on the color bar, specifically for the PostScript device. 3 November 2010. DWF.
;       Added Window and AddCmd keywords. 28 Jan 2011. DWF.
;       Added a Right keyword and changed the title spacing a little bit for aesthetic reasons. 2 July 2011. DWF.
;       Fixed a problem with assigning the color with the COLOR keyword in the Z-buffer. 30 Aug 2011. DWF.
;       The default BOTTOM keyword value was incorrect. Fixed in this version. 5 December 2011. DWF.
;       Modified to use cgDefaultColor for default color selection. 24 Dec 2011. DWF.
;       Previous change incorrectly implemented for PS device. Fixed. 29 Dec 2011. DWF.
;       Added CHARPERCENT, FIT, and TREVERSE keywords. Cleaned up documentation. 20 March 2012. DWF.
;       Added the ability to use escape characters in plot titles to specify cgSymbol symbols. 27 July 2012. DWF.
;       
; :Copyright:
;     Copyright (c) 2009-2012, Fanning Software Consulting, Inc.
;-
PRO cgDCBar, colors, $
    ADDCMD=addcmd, $
    BARCOLOR=barcolor, $
    BOTTOM=bottom, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILENAME=file, $
    FIT=fit, $
    FONT=font, $
    LABELS=labels, $
    NCOLORS=ncolors, $
    POSITION=position, $
    RIGHT=right, $
    ROTATE=rotate, $
    SPACING=spacing, $
    TCHARSIZE=tcharsize, $
    TREVERSE=treverse, $
    TITLE=title, $
    VERTICAL=vertical, $
    WINDOW=window

    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF 
    
    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(window) OR Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        IF Keyword_Set(addcmd) THEN window = 0
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgDCBar', colors, $
            BARCOLOR=barcolor, $
            BOTTOM=bottom, $
            CHARPERCENT=charpercent, $
            CHARSIZE=charsize, $
            COLOR=color, $
            FILENAME=file, $
            FIT=fit, $
            FONT=font, $
            LABELS=labels, $
            NCOLORS=ncolors, $
            POSITION=position, $
            RIGHT=right, $
            ROTATE=rotate, $
            SPACING=spacing, $
            TCHARSIZE=tcharsize, $
            TREVERSE=treverse, $
            TITLE=title, $
            VERTICAL=vertical, $
            REPLACECMD=Keyword_Set(window), $
            ADDCMD=Keyword_Set(addcmd)
         RETURN
    ENDIF

    ; Check parameters and keywords.
    IF (N_Elements(charPercent) EQ 0) $
        THEN charPercent = 0.85 $
        ELSE charPercent = 0.0 > charPercent < 1.0
    IF N_Elements(bottom) EQ 0 THEN bottom = 0
    SetDefaultValue, charsize, cgDefCharsize() * charPercent
    IF N_Elements(colors) EQ 0 THEN BEGIN
        IF N_Elements(ncolors) EQ 0 THEN BEGIN
            cgLoadCT, 25, /Brewer, NCOLORS=10, BOTTOM=245
            ncolors = 10
            bottom = 245
        ENDIF
        IF N_Elements(bottom) EQ 0 THEN bottom = 255 - ncolors
        ncolors = (bottom + ncolors) - bottom
        TVLCT, r, g, b, /Get
        colors = Indgen(ncolors) + bottom
    ENDIF ELSE ncolors = N_Elements(colors)
    cbar_colors = colors
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(labels) EQ 0 THEN labels = StrTrim(SIndgen(ncolors)+1,2)
    IF N_Elements(labels) NE N_Elements(colors) THEN $
        Message, 'LABELS vector must be the same length as the COLORS vector.'
    IF N_Elements(rotate) EQ 0 THEN rotate = 0
    rotate = (-180) > rotate < 180 ; Restrict to -180 to 180 degrees.
    IF N_Elements(spacing) EQ 0 THEN spacing = 1.0
    IF N_Elements(title) EQ 0 THEN title = "" ELSE title = cgCheckForSymbols(title)
    IF N_Elements(tcharsize) EQ 0 THEN tcharsize = cgDefCharsize()
    treverse = Keyword_Set(treverse)
    vertical = Keyword_Set(vertical)
    IF N_Elements(position) EQ 0 THEN BEGIN
        IF vertical THEN BEGIN
           position = [0.85, 0.1, 0.90, 0.9]
        ENDIF ELSE BEGIN
           position = [0.1, 0.88, 0.9, 0.93]
        ENDELSE
    ENDIF
    
    ; If needed create a window first, so the drawing
    ; colors are correct for the window you want to draw into.
    IF ((!D.Flags AND 256) NE 0) && (!D.Window LT 0) THEN cgDisplay
 
    ; Save the orginal color table so it can be restored later.
    TVLCT, rr, gg, bb, /Get
    
    ; I would prefer to draw in 24-bit color if I can, since this way I can
    ; avoid loading colors into the color table. I'll have to see where I am to
    ; see if I can do this in 24-bit color.
    SetDecomposedState, 1, CURRENT=currentState
    
    cbar_colors = cgDefaultColor(cbar_colors)
    color = cgDefaultColor(color, DEFAULT='opposite')
    barcolor = cgDefaultColor(color, DEFAULT=color)
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color, FILE=file)
    IF Size(barcolor, /TNAME) EQ 'STRING' THEN barcolor = cgColor(barcolor, FILE=file)
    IF Size(cbar_colors, /TNAME) EQ 'STRING' THEN cbar_colors = cgColor(cbar_colors, FILE=file)
    
    
    ; Draw horizontal color bar.
    IF ~vertical THEN BEGIN

        ; Are we trying to fit the color bar?
        IF Keyword_Set(fit) THEN BEGIN
            position[[0,2]] = !X.Window
            distance = (position[3] - position[1])
            position[1] = !Y.Window[1] + ((4*!D.Y_CH_SIZE*charsize) / !D.Y_Size)
            position[3] = position[1] + distance
            IF position[3] GT 1.0 THEN BEGIN
              diff = position[3]-1.0
              position[3] = 1.0 - 0.015
              IF (position[3] - position[1]) LT 0.015 THEN position[1] = (position[3] < position[1])-0.015
            ENDIF
        ENDIF
       
        ; Calculate positions for the rectangles of the bar.
        x0 = position[0]
        step = (position[2]-position[0])/ncolors
        x1 = x0 + step
        y0 = position[1]
        y1 = position[3]
        
        ; Draw each rectangle.
        FOR j=0,ncolors-1 DO BEGIN
            x = [x0, x1, x1, x0, x0]
            y = [y0, y0, y1, y1, y0]
            Polyfill, x, y, /NORMAL, Color=cbar_colors[j]
            PlotS, x, y, /NORMAL, COLOR=barcolor
            x0 = x1
            x1 = x0 + step
        ENDFOR
        
        ; Add the annotations of the bar.
        chardist = !D.Y_CH_SIZE / Float(!D.Y_Size) * $
            ((StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? (0.9 * spacing) : (1.5 * spacing))
        IF !D.Name EQ 'PS' THEN chardist = !D.Y_CH_SIZE / Float(!D.Y_Size) * (0.75 * spacing)
        step = (position[2]-position[0])/ncolors
        x = position[0] + step/2.
        y = y0 - (chardist * ((rotate NE 0) ? 1 : 2))
        CASE 1 OF
           (rotate EQ 0): alignment = 0.5
           (rotate GT 0) AND (rotate LE 180): alignment = 1.0
           (rotate LT 0) AND (rotate GT -180):alignment = 0.0
           ELSE: alignment = 0.5
        ENDCASE
        FOR j=0,N_Elements(labels)-1 DO BEGIN
            XYOutS, x, y, /NORMAL, StrTrim(labels[j],2), COLOR=color, $
                ORIENTATION=rotate, ALIGNMENT=alignment, CHARSIZE=charsize, FONT=font
            x = x + step
        ENDFOR
        XYOutS, (position[2]-position[0])/2.0 + position[0], y1+chardist, title, $
            COLOR=color, /NORMAL, ALIGNMENT=0.5, CHARSIZE=tcharsize, FONT=font
            
    ENDIF ELSE BEGIN ; Draw the vertical color bar.

        ; Are we trying to fit the color bar?
        IF Keyword_Set(fit) THEN BEGIN
            position[[1,3]] = !Y.Window
            distance = position[2] - position[0]
            IF Keyword_Set(right) THEN BEGIN
                position[0] = !X.Window[1] + ((2*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDIF ELSE BEGIN
                position[0] = !X.Window[1] + ((4*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDELSE
            position[2] = position[0] + distance
            IF position[2] GT 1.0 THEN BEGIN
              diff = position[2]-1.0
              position[2] = 1.0 - 0.015
              IF (position[2] - position[0]) LT 0.015 THEN position[0] = (position[2] < position[0])-0.015
            ENDIF
        ENDIF

        ; Draw each rectangle.
        x0 = position[0]
        x1 = position[2]
        step = (position[3]-position[1])/ncolors
        y0 = position[1]
        y1 = y0 + step
        FOR j=0,ncolors-1 DO BEGIN
            x = [x0, x1, x1, x0, x0]
            y = [y0, y0, y1, y1, y0]
            Polyfill, x, y, /NORMAL, Color=cbar_colors[j]
            PlotS, x, y, /NORMAL, COLOR=barcolor
            y0 = y1
            y1 = y0 + step
        ENDFOR

        ; Add the annotations of the bar.
        chardist = !D.Y_CH_SIZE / Float(!D.Y_Size) * $
            ((StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? (0.75 * spacing) : (1.25 * spacing))
        step = (position[3]-position[1])/ncolors
        y = position[1] + step/2. - (!D.Y_CH_Size / Float(!D.Y_Size) * 0.5)
        CASE 1 OF
           (rotate EQ 0): x = x1 + chardist*1
           (rotate GT 0): x = x1 + chardist*1.5
           (rotate LT 0): x = x1 + chardist*1
            ELSE: x = x1 + chardist
        ENDCASE
        CASE 1 OF
           (rotate EQ 90):  BEGIN
                alignment = 0.5
                x = x1 + chardist*2.5
            END
           (rotate EQ -90): alignment = 0.5
           ELSE: alignment = 0.0
        ENDCASE
        FOR j=0,N_Elements(labels)-1 DO BEGIN
            XYOutS, x, y, /NORMAL, StrTrim(labels[j],2), COLOR=color, $
                ORIENTATION=rotate, ALIGNMENT=alignment, CHARSIZE=charsize, FONT=font
            y = y + step
        ENDFOR
        IF Keyword_Set(right) THEN BEGIN
        
           IF treverse THEN BEGIN
             spacing = (rotate NE 0) ? !D.X_CH_SIZE*4.5/Float(!D.X_Size) : !D.X_CH_SIZE*4.0/Float(!D.X_Size)
           ENDIF ELSE BEGIN
              spacing = (rotate NE 0) ? !D.X_CH_SIZE*3.5/Float(!D.X_Size) : !D.X_CH_SIZE*3.0/Float(!D.X_Size)
           ENDELSE
           rotateFactor = Cos(Abs(rotate)*!DtoR) > 0.75
           xstart = x1 + (spacing + ( Max(StrLen(labels)) * (rotateFactor * !D.X_CH_SIZE ))/Float(!D.X_Size))
           IF treverse THEN BEGIN
               XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
                   title, COLOR=color, /NORMAL, ALIGNMENT=0.5, $
                   ORIENTATION=(x GE 0.5) ? 90 : -90, CHARSIZE=tcharsize, FONT=font
           ENDIF ELSE BEGIN
               XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
                   title, COLOR=color, /NORMAL, ALIGNMENT=0.5, $
                   ORIENTATION=(x GE 0.5) ? -90 : 90, CHARSIZE=tcharsize, FONT=font
           ENDELSE
        ENDIF ELSE BEGIN
           IF treverse THEN BEGIN
               IF treverse THEN BEGIN
                  xstart = x0 - (chardist * ((x0 LT 0.5) ? 1.5 : 1.0))               
               ENDIF ELSE BEGIN
                  xstart = x0 - (chardist * ((x0 LT 0.5) ? 2 : 1.5))
               ENDELSE
               XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
                   title, COLOR=color, /NORMAL, ALIGNMENT=0.5, $
                   ORIENTATION=(x GE 0.5) ? 90 : -90, CHARSIZE=tcharsize, FONT=font  
           ENDIF ELSE BEGIN
               xstart = x0 - (chardist * ((x0 LT 0.5) ? 1.5 : 2))
               XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
                   title, COLOR=color, /NORMAL, ALIGNMENT=0.5, $
                   ORIENTATION=(x GE 0.5) ? -90 : 90, CHARSIZE=tcharsize, FONT=font  
           ENDELSE      
        ENDELSE
    ENDELSE
    
    ; Restore the orginal color table.
    TVLCT, rr, gg, bb
    
    ; Clean up.
    SetDecomposedState, currentState
END
