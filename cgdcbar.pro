;+
; NAME:
;   cgDCBar
;
; PURPOSE:
;
;       The purpose of this routine is to add a discrete color bar to
;       a graphics plot. A "discrete" color bar is one with a handful
;       of colors. Labels are centered beneath or beside the color fields.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, Widgets.
;
; CALLING SEQUENCE:
;
;       cgDCBar
;
; INPUTS:
;
;       colors:       A vector of "colors" to be represented in the color bar. The
;                     vector can be a vector of color "names" that are known to cgCOLOR.
;                     Or, it can be a vector of 24-bit color values that can be decomposed
;                     into color triples. Or, it can be a vector of byte or integer values
;                     that can be used as indices into the current color table. If both colors
;                     and NCOLORS (see below) are undefined, a 10-element color table will be
;                     loaded and used.
;
; KEYWORD PARAMETERS:
;
;       ADDCMD:       Set this keyword to add the cgDCBar command to the current cgWindow
;                     command list. 
;               
;       BARCOLOR:     This is the name of a color known to cgCOLOR that can be
;                     used to draw the color bar outlines. By default, the same as
;                     specified with the COLOR keyword.
;                     
;       BOTTOM:       The lowest color index of the colors to be loaded in
;                     the color bar. Used in conjunction with NCOLORS when the colors
;                     argument is not used.
;                     
;       CHARSIZE:     The size of the color bar annotations. By default, !P.Charsize.
;
;       COLOR:        This is the name of a color known to cgCOLOR that can be
;                     used to draw the color bar annotations (labels and title).
;                     By default, "Black" for the PostScript device, and "White" 
;                     for all other devices. If unspecified, the "color" of the
;                     pixel in the upper-right corner of the display will be used
;                     to choose either "black" or "white".
;                     
;       FILE:         The name of a color table file that can be read by cgCOLOR.
;                     This allows you to specify your own color names for your own colors.
;                     
;       FONT:         Set this keyword to the type of font desired for labels. Similar to !P.FONT.
;                     The default is to use the !P.FONT value.
;
;       LABELS:       The labels that should annotate each color. Must be the same length
;                     as the colors vector. Colors are labelled consecutively by default.
;
;       NCOLORS:      An alternative way to specify the colors in the color bar is by
;                     using the NCOLORS and BOTTOM keywords to locate the colors in the
;                     current color table. The NCOLORS and BOTTOM keywords have the same
;                     meaning as in the LOADCT, XLOADCT, XCOLORS, or cgCOLORBAR programs.
;
;       POSITION:     A four-element array of normalized coordinates in the same
;                     form as the POSITION keyword on a plot. Default is
;                     [0.85, 0.1, 0.90, 0.9] for a vertical color bar and
;                     [0.1, 0.88, 0.9, 0.93] for a horizontal color bar.
;                  
;       RIGHT:        Set this keyword to put the title to the right of a vertical
;                     color bar. The default is to put the title to the left of a
;                     vertical color bar. The keyword has no effect for a horizontal
;                     color bar.
;
;       ROTATE:       Set this keyword to a value that will rotate the label text.
;                     Positive values between 0 and 180 degrees rotate in a counter-clockwise
;                     sense. Negative values between 0 and 180 degress rotate in a 
;                     clockwise sense.
;                     
;       SPACING:      When labels are rotated, it is a little difficult to determine where,
;                     exactly, they should be located. This keyword gives the user some control
;                     over this location. The default spacing is 1.0. The location "spacer" is
;                     multiplied by this amount. So, for example, to move the labels a little
;                     further away from the color bar, make this number greater than 1 (e.g, 1.25).
;                     To move the labels a little closer, use a number less than 1 (e.g, 0.75).
;
;       TITLE:        This is title for the color bar. The default is to have no title.
;
;       TCHARSIZE:    The size of the title. By default, same as CHARSIZE.
;
;       VERTICAL:     Setting this keyword give a vertical color bar. The default
;                     is a horizontal color bar.
;                     
;       WINDOW:       Set this keyword to add the color bar to an cgWindow display.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Color bar is drawn in the current graphics window.
;
; REQUIRED PROGRAMS:
;
;       The number of programs are required from the Coyote Library.
;       
;          http://www.dfanning.com/documents/programs.html
;          
;       Known to be among these are the following.
;
;          http://www.dfanning.com/programs/color24.pro
;          http://www.dfanning.com/programs/cgLoadCT.pro
;          http://www.dfanning.com/programs/error_message.pro
;          http://www.dfanning.com/programs/cgcolor.pro
;
; EXAMPLE:
;
;       To display a 12 color horizontal color bar, labels with a three-letter
;       month abbreviation.
;       
;          IDL> Window
;          IDL> LoadCT, 5, NCOLORS=12, BOTTOM=1
;          IDL> cgDCBar, NCOLORS=12, BOTTOM=1, LABELS=theMonths(/Abbreviation)
;       
;       To load a 5 color vertical color bar, with the labels rotated 45 degrees.
;
;          IDL> Window
;          IDL> labels = StrArr(5) 
;          IDL> FOR j=0,4 DO labels[j] = 'City ' + StrTrim(j+1,2)
;          IDL> colors = ['dodger blue', 'yellow', 'forest green', 'purple', 'tan']
;          IDL> cgDCBar, colors, LABELS=labels, ROTATE=45, /VERTICAL
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 15 March 2009.
;       Modification to code to avoid changing the colors vectors. 15 March 2009.
;       Added FONT keyword. 1 April 2009. DWF.
;       Code modified to support 24-bit PostScript printers. 23 September 2009. DWF.
;       Fixed a problem with determining visual depth in Z-buffer. 15 January 2010. DWF.
;       Added SPACING keyword and changed the default spacing on horizontal color bars slightly. 
;           23 Apr 2010. DWF.
;       Modified the spacing of the labels on the color bar, specifically for the 
;           PostScript device. 3 November 2010. DWF.
;       Added Window and AddCmd keywords. 28 Jan 2011. DWF.
;       Added a Right keyword and changed the title spacing a little bit for
;           aesthetic reasons. 2 July 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2009-2010, by Fanning Software Consulting, Inc.                           ;
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
PRO cgDCBar, colors, $
    ADDCMD=addcmd, $
    BARCOLOR=barcolor, $
    BOTTOM=bottom, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILE=file, $
    FONT=font, $
    LABELS=labels, $
    NCOLORS=ncolors, $
    POSITION=position, $
    RIGHT=right, $
    ROTATE=rotate, $
    SPACING=spacing, $
    TITLE=title, $
    TCHARSIZE=tcharsize, $
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
            CHARSIZE=charsize, $
            COLOR=color, $
            FILE=file, $
            FONT=font, $
            LABELS=labels, $
            NCOLORS=ncolors, $
            POSITION=position, $
            RIGHT=right, $
            ROTATE=rotate, $
            SPACING=spacing, $
            TITLE=title, $
            TCHARSIZE=tcharsize, $
            VERTICAL=vertical, $
            REPLACECMD=Keyword_Set(window), $
            ADDCMD=Keyword_Set(addcmd)
         RETURN
    ENDIF

    ; Check parameters and keywords.
    IF (!D.Name EQ 'PS') AND N_Elements(color) EQ 0 THEN BEGIN
        color = 'black'
    ENDIF ELSE BEGIN
        IF (!D.Window GE 0) AND ((!D.Flags AND 1) EQ 0) THEN BEGIN
            pixel = cgSnapshot(!D.X_Size-1, !D.Y_Size-1, 1, 1)
            IF N_ELEMENTS(color) EQ 0 THEN BEGIN
                color = 'white'
                IF Total(pixel) EQ 765 THEN color = 'black'
                IF Total(pixel) EQ 0 THEN color = 'opposite'
            ENDIF 
        ENDIF ELSE color = 'opposite'
    ENDELSE
    IF N_Elements(barcolor) EQ 0 THEN   barcolor = color
    IF N_Elements(charsize) EQ 0 THEN charsize = !P.Charsize
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
    IF N_Elements(bottom) EQ 0 THEN bottom = 255 - ncolors
    IF N_Elements(font) EQ 0 THEN font = !P.Font
    IF N_Elements(labels) EQ 0 THEN labels = StrTrim(SIndgen(ncolors)+1,2)
    IF N_Elements(labels) NE N_Elements(colors) THEN $
        Message, 'LABELS vector must be the same length as the COLORS vector.'
    IF N_Elements(rotate) EQ 0 THEN rotate = 0
    rotate = (-180) > rotate < 180 ; Restrict to -180 to 180 degrees.
    IF N_Elements(spacing) EQ 0 THEN spacing = 1.0
    IF N_Elements(title) EQ 0 THEN title = ""
    IF N_Elements(tcharsize) EQ 0 THEN tcharsize = charsize
    vertical = Keyword_Set(vertical)
    IF N_Elements(position) EQ 0 THEN BEGIN
        IF vertical THEN BEGIN
           position = [0.85, 0.1, 0.90, 0.9]
        ENDIF ELSE BEGIN
           position = [0.1, 0.88, 0.9, 0.93]
        ENDELSE
    ENDIF
    
    ; Save the orginal color table so it can be restored later.
    TVLCT, rr, gg, bb, /Get
    
    ; I would prefer to draw in 24-bit color if I can, since this way I can
    ; avoid loading colors into the color table. I'll have to see where I am to
    ; see if I can do this in 24-bit color.
    CASE !D.Name OF
        'X': BEGIN
             Device, Get_Visual_Depth=theDepth
             IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
             Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
             END
        'WIN': BEGIN
             Device, Get_Visual_Depth=theDepth
             IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
             Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
             END
        'Z': BEGIN
             Device, Get_Pixel_Depth=theDepth
             IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
             Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
             END
        'PS': BEGIN
             IF Float(!Version.Release) GE 7.1 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
             IF supportsTrueColor THEN BEGIN
                Device, DECOMPOSED=1
                theState = 1
                ENDIF
             END
        ELSE: BEGIN
              supportsTrueColor = 0
             END
    ENDCASE
    
    ; Set up the colors for drawing. All 24-bit if it supports true color.
    ; Otherwise load colors for 8-bit support.
    IF supportsTrueColor THEN BEGIN
        CASE Size(cbar_colors, /TNAME) OF
            'STRING': BEGIN
                cbar_colors = cgColor(cbar_colors, DECOMPOSED=1, FILE=file)
                END
            'INT': BEGIN
                 TVLCT, r, g, b, /GET
                 temp = LonArr(ncolors)
                 FOR j=0,ncolors-1 DO BEGIN
                    temp[j] = Color24([r[cbar_colors[j]], g[cbar_colors[j]], b[cbar_colors[j]]])
                 ENDFOR
                 cbar_colors = Temporary(temp)
                 END
            'ULONG':
            'LONG': BEGIN 
            
                    ; If the state is NOT using decomposed color, these are probably
                    ; color index numbers, rather than long integers to be decomposed.
                    IF theState EQ 0 THEN BEGIN
                         TVLCT, r, g, b, /GET
                         temp = LonArr(ncolors)
                         FOR j=0,ncolors-1 DO BEGIN
                            temp[j] = Color24([r[cbar_colors[j]], g[cbar_colors[j]], b[cbar_colors[j]]])
                         ENDFOR
                         cbar_colors = Temporary(temp)
                    ENDIF
                    
                    ; If the maximum value of these long integers in not over 255, then
                    ; we can be pretty sure these are color index numbers. At least I'm
                    ; going to treat them that way for now and see what kind of trouble I
                    ; get in.
                    IF Max(cbar_colors) LE 255 THEN BEGIN
                         TVLCT, r, g, b, /GET
                         temp = LonArr(ncolors)
                         FOR j=0,ncolors-1 DO BEGIN
                            temp[j] = Color24([r[cbar_colors[j]], g[cbar_colors[j]], b[cbar_colors[j]]])
                         ENDFOR
                         cbar_colors = Temporary(temp)
                    ENDIF
                    END
            'BYTE': BEGIN
                 TVLCT, r, g, b, /GET
                 temp = LonArr(ncolors)
                 FOR j=0,ncolors-1 DO BEGIN
                    temp[j] = Color24([r[cbar_colors[j]], g[cbar_colors[j]], b[cbar_colors[j]]])
                 ENDFOR
                 cbar_colors = Temporary(temp)
                 END
            ELSE: BEGIN
                 TVLCT, r, g, b, /GET
                 temp = LonArr(ncolors)
                 FOR j=0,ncolors-1 DO BEGIN
                    temp[j] = Color24([r[cbar_colors[j]], g[cbar_colors[j]], b[cbar_colors[j]]])
                 ENDFOR
                 cbar_colors = Temporary(temp)
                 END
        ENDCASE
    ENDIF ELSE BEGIN
    
        CASE Size(cbar_colors, /TNAME) OF
            'STRING': BEGIN
                cbar_colors = cgColor(cbar_colors, DECOMPOSED=0, FILE=file)
                END
             'LONG': BEGIN

                ; If the maximum value of these long integers in not over 255, then
                ; we can be pretty sure these are color index numbers. At least I'm
                ; going to treat them that way for now and see what kind of trouble I
                ; get into.
                IF Max(cbar_colors) GT 255 THEN BEGIN
                    r = cbar_colors AND '0000FF'xL
                    g = ISHFT(cbar_colors AND '00FF00'xL, -8)
                    b = ISHFT(cbar_colors AND 'FF0000'xL, -16)
                    TVLCT, r, g, b, bottom
                    cbar_colors = Indgen(ncolors) + bottom
                ENDIF
                END
             ELSE: 
        ENDCASE
    
    ENDELSE
    
    ; Draw horizontal color bar.
    IF ~vertical THEN BEGIN
    
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
            PlotS, x, y, /NORMAL, COLOR=cgColor(barcolor, FILE=file)
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
            XYOutS, x, y, /NORMAL, StrTrim(labels[j],2), COLOR=cgColor(color, FILE=file), $
                ORIENTATION=rotate, ALIGNMENT=alignment, CHARSIZE=charsize, FONT=font
            x = x + step
        ENDFOR
        XYOutS, (position[2]-position[0])/2.0 + position[0], y1+chardist, title, $
            COLOR=cgColor(color, FILE=file), /NORMAL, ALIGNMENT=0.5, CHARSIZE=tcharsize, FONT=font
            
    ENDIF ELSE BEGIN ; Draw the vertical color bar.

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
            PlotS, x, y, /NORMAL, COLOR=cgColor(barcolor, FILE=file)
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
            XYOutS, x, y, /NORMAL, StrTrim(labels[j],2), COLOR=cgColor(color, FILE=file), $
                ORIENTATION=rotate, ALIGNMENT=alignment, CHARSIZE=charsize, FONT=font
            y = y + step
        ENDFOR
        IF Keyword_Set(right) THEN BEGIN
        
           spacing = (rotate NE 0) ? !D.X_CH_SIZE*3.5/Float(!D.X_Size) : !D.X_CH_SIZE*2/Float(!D.X_Size)
           rotateFactor = Cos(Abs(rotate)*!DtoR) > 0.75
           xstart = x1 + (spacing + ( Max(StrLen(labels)) * (rotateFactor * !D.X_CH_SIZE ))/Float(!D.X_Size))
           XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
               title, COLOR=cgColor(color, FILE=file), /NORMAL, ALIGNMENT=0.5, $
               ORIENTATION=(x GE 0.5) ? -90 : 90, CHARSIZE=tcharsize, FONT=font
        ENDIF ELSE BEGIN
           xstart = x0 - (chardist * ((x0 LT 0.5) ? 1.5 : 2))
           XYOutS, xstart, (position[3]-position[1])/2.0 + position[1], $
               title, COLOR=cgColor(color, FILE=file), /NORMAL, ALIGNMENT=0.5, $
               ORIENTATION=(x GE 0.5) ? -90 : 90, CHARSIZE=tcharsize, FONT=font        
        ENDELSE
    ENDELSE
    
    ; Restore the orginal color table.
    TVLCT, rr, gg, bb
    
    ; Clean up.
    CASE !D.Name OF
        'X': Device, DECOMPOSED=theState
        'WIN': Device, DECOMPOSED=theState
        'Z': Device, DECOMPOSED=theState
        'PS': IF supportsTrueColor THEN DEVICE, DECOMPOSED=0
        ELSE: 
    ENDCASE
END
