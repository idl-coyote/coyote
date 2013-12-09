; docformat = 'rst'
;
; NAME:
;   cgLegendItem__Define
;
; PURPOSE:
;   The purpose of this program is to a create simple legend object that can be drawn on
;   a data plot.
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
;+
; The purpose of this program is to create a simple legend object that can be drawn on
; a data plot. The user can include any number of legend "items" in the final legend.
;        
; :Categories:
;    Graphics
;    
; :Examples:
;    A plot with a simple legend::
;        cgDisplay, 800, 450
;        cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.7, 0.9]
;        cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;        legendObj = Obj_New('cgLegendItem', SymColor=['red7', 'blu7'], $
;            PSym=[6,15], Symsize=1.5, Location=[0.725, 0.9], Titles=['May 27', 'June 27'], $
;            Length=0.075, /Box, VSpace=2.75, /Background, BG_Color='rose', /Draw)
;        Obj_Destroy, legendObj
;
;    Same as the previous example, but in a resizeable graphics window::
;        cgWindow, WXSize=800, WYSize=450
;        cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.7, 0.9], /AddCmd
;        cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7', /AddCmd
;        legendObj = Obj_New('cgLegendItem', SymColor=['red7', 'blu7'], $
;            PSym=[6,15], Symsize=1.5, Location=[0.725, 0.9], Titles=['May 27', 'June 27'], $
;            Length=0.075, /Box, VSpace=2.75, /Background, BG_Color='rose', /AddCmd)
;           
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written 18 July 2012. DWF.
;        Adapted to accept multiple legend elements. Legend elements are stacked vertically. 
;           Each legend element can be given its own title, color, symbol symbol color, and 
;           linestyle. Legend elements are offset  by 1.3*!D.Y_CH_Size/!D.YSize.  A single 
;           symbol can now be drawn in the center of the line instead of one at each end 
;           point with the CENTER_SYM keyword. 04/25/2013, Matthew Argall.
;        Many changes to make this work like a simple, but useful, legend-drawing program. Now
;           called by the wrapper cgLegend. 5 Dec 2013. DWF.
;           
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;
;-

;+--------------------------------------------------------------------------
; This is the initialization method for the object. It creates an instance of
; the object.
;
; :Keywords:
;     addcmd: in, optional, type=boolean,default=0
;        If this keyword is set, the object is added to the resizeable graphics
;        window, cgWindow. The DRAW method of the object is called in cgWindow.
;     alignment: in, optional, type=integer, default=0
;        This keyword affects the alignment of the legend box with respect to the `Location` point.
;        There are eight possible values (0 to 7) that assign the location point to be one of the
;        four corners of the legend box, or the middle of one of the four sides of the legend box.
;        The values are specified as follows::
;           0 - Location specifies the upper left corner of the legend box.
;           1 - Location specifies the upper right corner of the legend box.
;           2 - Location specifies the lower right corner of the legend box.
;           3 - Location specifies the lower left corner of the legend box.
;           4 - Location specifies the top side of the legend box (centered horizontally).
;           5 - Location specifies the bottom side of the legend box (centered horizontally).
;           6 - Location specifies the left side of the legend box (centered vertically).
;           7 - Location specifies the right side of the legend box (centered vertically).
;     background: out, optional, type=boolean, default=0
;        Set this keyword to draw a colored background for the legend.
;     bg_color: out, optional, type=string, default="white"
;        The name of the background color.
;     box: in, optional, type=boolean, default=0
;        Set this keyword to draw a box around the legend items.
;     bx_color: in, optional, type=varies, default="black"
;        The color of the box drawn around the legend items.
;     bx_thick: in, optional, type=float
;        The thickness of the line used to draw the box around the legend items.
;        If not set, use !P.Thick at drawing time.
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line. The default
;        is to draw a symbol at each endpoint of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     colors: in, optional, type=varies, default="black"
;        The name of the data color. This is the color of each data line. May be an array.
;     data: in, optional, type=boolean, default=0
;        If set the values specified by the `Location` keyword are taken to be in data
;        coordinate space.
;     draw: in, optional, type=boolean, default=0
;        Set this keyword to immediately draw the object as soon as it has been initialized.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyles: in, optional, type=integer
;        The line style for drawing each line. May be an array.
;     location: in, optional, type=fltarr
;        A two-element vector giving the X and Y location of upper-left corner of the legend
;        (or legend box, if the `Box` keyword is set) in normalized coordinates. If the `Data`
;        keyword is set, the locations are taken to be in data coordinate space.
;     psyms: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46. May be an array.
;     symcolors: in, optional, type=varies
;        The name of the symbol color. By default, the same as the `COLOR` keyword. May be an array.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float
;        The thickness of the symbol. If not set, use !P.Thick at drawing time.
;     tcolors: in, optional, type=varies, default="black"
;        The `Title` color. May be an array.
;     thick: in, optional, type=float
;        The thickness of the line. If not set, use !P.Thick at drawing time.
;     titles: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine if the legend should be drawn (visible=1), or
;        if the legend should not be drawn (visible=0).
;     vspace: in, optional, type=float, default=1.5
;         A scale factor for vertical spacing between legend items. This number is multiplied by
;         `Charsize` to determine vertical spacing.
;     window: in, optional, type=boolean, default=0
;         If this keyword is set, the object replaces any commands in a current
;         cgWindow or it opens a new cgWindow and adds itself to it.
;---------------------------------------------------------------------------
FUNCTION cgLegendItem::INIT, $
    ADDCMD=addcmd, $
    ALIGNMENT=alignment, $
    BACKGROUND=background, $
    BG_COLOR=bg_color, $
    BOX=box, $
    BX_COLOR=bx_color, $
    BX_THICK=bx_thick, $
    CENTER_SYM=center_sym, $
    CHARSIZE=charsize, $
    COLORS=colors, $
    DATA=data, $
    DRAW=draw, $
    HARDWARE=hardware, $
    LENGTH=length, $
    LINESTYLES=linestyles, $
    LOCATION=location, $
    PSYMS=psyms, $
    SYMCOLORS=symcolors, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    TCOLORS=tcolors, $
    THICK=thick, $
    TITLES=titles, $
    TT_FONT=tt_font, $
    VISIBLE=visible, $
    VSPACE=vspace, $
    WINDOW=window
    
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
    ; How many items are we drawing?
    nlegends = N_Elements(titles)
    IF nlegends EQ 0 THEN BEGIN
        titles = 'Plot Item'
        nlegends = 1
    ENDIF
    
    ; Define default parameters.
    SetDefaultValue, alignment, 0
    SetDefaultValue, background, 0, /Boolean
    SetDefaultValue, bg_color, 'white'
    SetDefaultValue, box, 0, /Boolean
    SetDefaultValue, bx_color, 'black'
    SetDefaultValue, center_sym, /BOOLEAN
    IF N_Elements(charsize)  EQ 0 THEN charsize = cgDefCharsize()
    CASE N_Elements(colors) OF
        0: colors = Replicate('black', nlegends)
        1: colors = Replicate(colors, nlegends)
        ELSE: IF N_Elements(colors) NE nlegends THEN BEGIN
            Message, 'Number of COLORS elements not equal to the number of legend items.
        END
    ENDCASE
    SetDefaultValue, data, 0, /Boolean
    CASE N_Elements(linestyles) OF
        0: linestyles = Replicate(0, nlegends)
        1: linestyles = Replicate(linestyles, nlegends)
        ELSE: IF N_Elements(linestyles) NE nlegends THEN BEGIN
            Message, 'Number of LINESTYLES elements not equal to the number of legend items.
        END
    ENDCASE
    CASE N_Elements(psyms) OF
        0: psyms = Replicate(0, nlegends)
        1: psyms = Replicate(psyms, nlegends)
        ELSE: IF N_Elements(psyms) NE nlegends THEN BEGIN
            Message, 'Number of PSYMS elements not equal to the number of legend items.
        END
    ENDCASE
    SetDefaultValue, hardware, 0
    SetDefaultValue, length, 0.075
    SetDefaultValue, location, [0.1, 0.95]
    CASE N_Elements(symcolors) OF
        0: symcolors = colors
        1: symcolors = Replicate(symcolors, nlegends)
        ELSE: IF N_Elements(symcolors) NE nlegends THEN BEGIN
            Message, 'Number of SYMCOLORS elements not equal to the number of legend items.
        END
    ENDCASE
    SetDefaultValue, symsize, 1.0
    CASE N_Elements(tcolors) OF
        0: tcolors = Replicate('black', nlegends)
        1: tcolors = Replicate(tcolors, nlegends)
        ELSE: IF N_Elements(tcolors) NE nlegends THEN BEGIN
            Message, 'Number of TCOLORS elements not equal to the number of legend items.
        END
    ENDCASE
    SetDefaultValue, visible, 1
    SetDefaultValue, vspace, 1.5
    
    ; Set the appropriate alignment keywords.
    alignment = 0 > alignment < 7
    CASE alignment OF
        0: BEGIN
           align_hcenter = 0
           align_vcenter = 0
           align_right = 0
           align_bottom = 0
           END
        1: BEGIN
           align_hcenter = 0
           align_vcenter = 0
           align_right = 1
           align_bottom = 0
           END
        2: BEGIN
           align_hcenter = 0
           align_vcenter = 0
           align_right = 1
           align_bottom = 1
           END
        3: BEGIN
           align_hcenter = 0
           align_vcenter = 0
           align_right = 0
           align_bottom = 1
           END
        4: BEGIN
           align_hcenter = 1
           align_vcenter = 0
           align_right = 0
           align_bottom = 0
           END
        5: BEGIN
           align_hcenter = 1
           align_vcenter = 0
           align_right = 0
           align_bottom = 1
           END
        6: BEGIN
           align_hcenter = 0
           align_vcenter = 1
           align_right = 0
           align_bottom = 0
           END
        7: BEGIN
           align_hcenter = 0
           align_vcenter = 1
           align_right = 1
           align_bottom = 0
           END
    ENDCASE
    
    ; Populate the object.
    self.alignment = alignment
    self.align_bottom = Keyword_Set(align_bottom)
    self.align_hcenter = Keyword_Set(align_hcenter)
    self.align_vcenter = Keyword_Set(align_vcenter)
    self.align_right = Keyword_Set(align_right)
    self.background = background
    self.bg_color = Ptr_New(bg_color)
    self.box = Keyword_Set(box)
    self.bx_color = Ptr_New(bx_color)
    IF N_Elements(bx_thick) NE 0 THEN self.bx_thick = bx_thick
    self.center_sym = Keyword_Set(center_sym)
    self.charsize = charsize
    self.colors = Ptr_New(colors)
    self.data = Keyword_Set(data)
    IF N_Elements(tt_font) NE 0 THEN self.tt_font = Ptr_New(tt_font) ELSE self.tt_font = Ptr_New(/ALLOCATE_HEAP)
    self.hardware = hardware
    self.length = length
    self.linestyles = Ptr_New(linestyles)
    self.location = location
    self.psyms = Ptr_New(psyms)
    self.symcolors = Ptr_New(symcolors)
    self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    self.tcolors = Ptr_New(tcolors)
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    self.titles = Ptr_New(titles)
    self.visible = Keyword_Set(visible)
    self.vspace = vspace
    self.wid = -1
    
    ; Need to add this command to a resizeable cgWindow?
    IF Keyword_Set(window) THEN self -> AddCmd, /REPLACE, /DESTROY_OBJECT
    IF Keyword_Set(addcmd) THEN self -> AddCmd, /DESTROY_OBJECT
    
    ; Need immediate draw?
    IF Keyword_Set(draw) THEN self -> Draw
    
    RETURN, 1
END


;+--------------------------------------------------------------------------
;   Adds the object as a command (the DRAW method is called) in a cgWindow
;   resizeable graphics window. If there is no current cgWindow, one is
;   created. The object is destroyed when the cgWindow is destroyed.
;
; :Keywords:
;     destroy_object: in, optional, type=boolean, default=0
;        Set this keyword to allow the cgWindow to destroy the object when finished with it.
;     method: in, optional, type='string', default='draw'
;        The object method to add to the cgWindow.
;     replace: in, optional, type=boolean, default=0
;        If this keyword is set, object DRAW method replaces any commands in the
;        current graphics window.
;---------------------------------------------------------------------------
PRO cgLegendItem::AddCmd, REPLACE=replace, METHOD=method, DESTROY_OBJECT=destroy_object

    IF N_Elements(method) EQ 0 THEN method = 'DRAW'
    
    ; Which method are we adding?
    currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
    
    ; Do we have a window to add it to?
    IF (wincnt EQ 0) && ( (!D.Flags AND 256) NE 0) THEN cgWindow
    
    ; Which method are we adding?
    currentWindow = cgQuery(/CURRENT, COUNT=wincnt)

    ; Add (or replace) the command to the window.
    IF wincnt GT 0 THEN BEGIN
        IF Keyword_Set(replace) THEN BEGIN
            cgWindow, method, self, /Method, WDestroyObjects=Keyword_Set(destroy_object), /ReplaceCmd ; Replace all commands in the window
        ENDIF ELSE BEGIN
            cgWindow, method, self, /Method, WDestroyObjects=Keyword_Set(destroy_object), /AddCmd     ; Add this command to the window.
        ENDELSE
        
        ; Get the window ID of the window we put this in and store it.
        wid = cgQuery(/Current)
        self.wid = wid
    ENDIF
END


;+--------------------------------------------------------------------------
; This method calculates the size of the box needed for the legend.
;---------------------------------------------------------------------------
PRO cgLegendItem::CalculateBoxSize

    ; Set up thickness values, if needed.
    IF self.bx_thick EQ 0 THEN thick = !P.Thick ELSE thick = self.bx_thick
    IF self.symthick EQ 0 THEN symthick = !P.Thick ELSE symthick = self.symthick
    IF self.thick EQ 0 THEN thick = !P.Thick ELSE thick = self.thick
    
    ; Do we need to convert location to normalized coordinate space?
    IF self.data THEN BEGIN
        location = Convert_Coord(self.location, /Data, /TO_Normal)
    ENDIF ELSE location = self.location
    
    ; If you are drawing a box, then the position refers to the box position, not
    ; the first legend item.
    IF self.box THEN BEGIN
        bx0 = location[0]
        by1 = location[1]
        x0 = bx0 + (2.25 * Float(!D.X_CH_Size)/!D.X_Size)
        x1 = x0 + self.length
        y = by1 - (1.75 * Float(!D.Y_CH_Size)/!D.Y_Size)
        y_offset = self.vspace * Float(!D.Y_CH_Size)/!D.Y_Size
    ENDIF ELSE BEGIN
        x0 = location[0]
        x1 = x0 + self.length
        y = location[1]
        y_offset = self.vspace * Float(!D.Y_CH_Size)/!D.Y_Size
        bx0 = x0 - (2 * Float(!D.X_CH_Size)/!D.X_Size)
        by1 = y + (2 * Float(!D.Y_CH_Size)/!D.Y_Size)
    ENDELSE
    
    ; We want to keep track of the item width as we draw it.
    IF Ptr_Valid(self.width) THEN BEGIN
        Ptr_Free, self.width
        self.width = Ptr_New(/Allocate_Heap)
    ENDIF ELSE self.width = Ptr_New(/Allocate_Heap)
    itemWidth = FltArr(N_Elements(*self.titles))

    IF !D.Name EQ 'PS' THEN BEGIN
        xx = 0.1
        yy=1.25
        xsize = !D.X_Size
        ysize = !D.Y_Size
    ENDIF ELSE BEGIN
        xsize = !D.X_Size
        ysize = !D.Y_Size
        currentID = !D.Window
        
        ; This has to be WINDOW, rather than cgDisplay because cgDisplay means
        ; something different in cgWindows.
        Window, XSIZE=xsize, YSIZE=30, /Pixmap, /Free
        pixID = !D.Window
        xx = 0.5
        yy=0.5
    ENDELSE
    
    ; For each legend item.
    FOR j = 0, N_Elements(*self.titles) - 1 DO BEGIN
    
        ; Need to draw a line?
        IF x0 NE x1 THEN BEGIN
            itemWidth[j] = x1-x0
        ENDIF
        
        ; Need to draw symbols?
        IF (*self.psyms)[j] NE 0 THEN BEGIN
            ; Center the symbol on the line?
            IF self.center_sym THEN BEGIN
                x2 = x0 + (x1 - x0) / 2.0
            ENDIF
            itemWidth[j] = itemWidth[j] + (self.symsize * (Float(!D.X_CH_Size)/xsize))
        ENDIF
        
        ; Draw the title.
        IF self.hardware THEN BEGIN
            thisFont = !P.Font
            !P.Font = (!D.Name EQ 'PS') ? 1 : 0
        ENDIF
        by0 =  y-(0.5*!D.Y_CH_SIZE/ysize)-(j*y_offset)
        cgText, xx, yy, Alignment=0.5, /Normal, (*self.titles)[j],  $
            TT_FONT=*self.tt_font, CHARSIZE=self.charsize, FONT=!P.Font, WIDTH=width, CHARTHICK=thick
        itemWidth[j] = itemWidth[j] + width
        IF self.hardware THEN !P.Font = thisFont
        
    ENDFOR
    *self.width = itemWidth
    
    ; Cleanup
    IF N_Elements(pixID) NE 0 THEN BEGIN
        WDelete, pixID
        IF currentID GE 0 THEN WSet, currentID
    ENDIF
    
    ; Calculate the end position of the legend box.
    len = Max(itemWidth)
    bx1 = x0 + len + (2.0 * Float(!D.X_CH_Size)/xsize)
    by0 = by0 - Float(!D.Y_CH_Size)/ysize
    xlength = bx1 - bx0
    ylength = by1 - by0

    ; There are 8 possible positions for the legend box, depending on the values of certain keywords,
    ; with respect to the location specified.
    CASE 1 OF
        ~self.align_bottom && ~self.align_vcenter && ~self.align_hcenter && ~self.align_right: ; Location is upper-left corner.
        
        ~self.align_bottom && self.align_right && ~self.align_vcenter: BEGIN   ; Location is upper-right corner.
            ; Translate X coordinates to the left by length of box.
            bx0 = bx0 - xlength
            bx1 = bx1 - xlength
        END
        
        self.align_bottom && self.align_right && ~self.align_vcenter: BEGIN     ; Location is bottom-right corner.
            ; Translate X coordinates to the left by length of box.
            ; Translate Y coordinates up by length of box.
            bx0 = bx0 - xlength
            bx1 = bx1 - xlength
            by0 = by0 + ylength
            by1 = by1 + ylength
        END
        
        self.align_bottom && ~self.align_right  && ~self.align_vcenter  && ~self.align_hcenter: BEGIN    ; Location is bottom-left corner.
            ; Translate Y coordinates up by length of box.
            by0 = by0 + ylength
            by1 = by1 + ylength
        END
        
        ~self.align_bottom && self.align_hcenter && ~self.align_right: BEGIN   ; Location is top center.
            ; Move to left by half an x length.
            bx0 = bx0 - (xlength/2.0)
            bx1 = bx1 - (xlength/2.0)
        END
        
        self.align_bottom && self.align_hcenter && ~self.align_right: BEGIN    ; Location is bottom center.
            ; Move to left by half an x length.
            ; Translate Y coordinates up by length of box.
            bx0 = bx0 - (xlength/2.0)
            bx1 = bx1 - (xlength/2.0)
            by0 = by0 + ylength
            by1 = by1 + ylength
        END
        
        self.align_vcenter && ~self.align_right  && ~self.align_bottom: BEGIN    ; Location is left center.
            ; Translate Y coordinates up by half the length of box
            by0 = by0 + (ylength/2.0)
            by1 = by1 + (ylength/2.0)
        END
        
        self.align_vcenter && self.align_right  && ~self.align_bottom: BEGIN     ; Location is right center.
            ; Translate X coordinates to the left by length of box.
            bx0 = bx0 - xlength
            bx1 = bx1 - xlength
            ; Translate Y coordinates up by half the length of box
            by0 = by0 + (ylength/2.0)
            by1 = by1 + (ylength/2.0)
        END
        
        ELSE: Message, 'Confusing use of alignment keywords. Continuing...', /Informational
        
    ENDCASE
    
    self.bx_pos = [bx0, by0, bx1, by1]

END


;+--------------------------------------------------------------------------
; This method draws the legend item or items in a graphics window.
;---------------------------------------------------------------------------
PRO cgLegendItem::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        IF N_Elements(incomingColorState) THEN cgSetColorState, incomingColorState
        RETURN
    ENDIF
    
    ; If this is not visible, return now.
    IF ~self.visible THEN RETURN
    
    ; Set the current graphics window.
    IF self.wid EQ -1 THEN wid = !D.Window ELSE wid = self.wid
    IF (!D.Flags AND 256) NE 0 THEN WSet, wid
    
    ; We want to draw in decomposed color, if possible.
    cgSetColorState, 1, Current=incomingColorState
    
    self -> CalculateBoxSize
    
    ; Do we need to draw a background? If so, we have a problem. We can't figure out where
    ; to draw the background "box" until after we have actually drawn the legend in the window
    ; because we can't apriori determine the width of each item. We are going to use the AL_LEGEND
    ; practice of drawing the legend twice, but the first time, we will draw everything in the background
    ; color, hoping we don't see a "flash" of drawing. If we have box coordinates already, we don't have to
    ; do this again.
    IF self.background && (Total(self.bx_pos) EQ 0.0) THEN BEGIN
        background_color = cgColor('background')
        bg_color = *self.bg_color
        bx_color = background_color
        colors = Replicate(background_color, N_Elements(*self.colors))
        symcolors = Replicate(background_color, N_Elements(*self.symcolors))
        tcolors = Replicate(background_color, N_Elements(*self.tcolors))
        drawAgain = 1
    ENDIF ELSE BEGIN
        bg_color = *self.bg_color
        bx_color = *self.bx_color
        colors = *self.colors
        symcolors = *self.symcolors
        tcolors = *self.tcolors
        drawAgain = 0
        
        ; Draw the background box, if needed.
        IF self.background THEN cgColorFill, Position=self.bx_pos, COLOR=bg_color
    ENDELSE
    
    ; Set up thickness values, if needed.
    IF self.bx_thick EQ 0 THEN thick = !P.Thick ELSE thick = self.bx_thick
    IF self.symthick EQ 0 THEN symthick = !P.Thick ELSE symthick = self.symthick
    IF self.thick EQ 0 THEN thick = !P.Thick ELSE thick = self.thick
    
    ; Do we need to convert location to normalized coordinate space?
    IF self.data THEN BEGIN
        location = Convert_Coord(self.location, /Data, /TO_Normal)
    ENDIF ELSE location = self.location
    
    ; If you are drawing a box, then the position refers to the box position, not 
    ; the first legend item.
    IF self.box THEN BEGIN
        bx0 = self.bx_pos[0]
        by1 = self.bx_pos[3]
        x0 = bx0 + (2.25 * Float(!D.X_CH_Size)/!D.X_Size)
        x1 = x0 + self.length
        y = by1 - (1.75 * Float(!D.Y_CH_Size)/!D.Y_Size)
        y_offset = self.vspace * Float(!D.Y_CH_Size)/!D.Y_Size
    ENDIF ELSE BEGIN
        x0 = self.bx_pos[0] + (2 * Float(!D.X_CH_Size)/!D.X_Size)
        x1 = x0 + self.length
        y = self.bx_pos[3] -(2 * Float(!D.Y_CH_Size)/!D.Y_Size)
        y_offset = self.vspace * Float(!D.Y_CH_Size)/!D.Y_Size
        bx0 = self.bx_pos[0] 
        by1 = self.bx_pos[3] 
    ENDELSE
    
    ; For each legend item.
    FOR j = 0, N_Elements(*self.titles) - 1 DO BEGIN
        
        ; Need to draw a line?
        IF x0 NE x1 THEN BEGIN
           cgPlotS, [x0,x1], [y,y]-(j*y_offset), COLOR=colors[j], $
                    LINESTYLE=(*self.linestyles)[j], THICK=thick, /NORMAL
        ENDIF
    
        ; Need to draw symbols?
        IF (*self.psyms)[j] NE 0 THEN BEGIN
            ; Center the symbol on the line?
            IF self.center_sym THEN BEGIN
                x2 = x0 + (x1 - x0) / 2.0
               cgPlotS, [x2,x2], [y,y]-(j*y_offset), PSYM=(*self.psyms)[j], THICK=symthick, $
                        SYMSIZE=self.symsize, SYMCOLOR=symcolors[j], /NORMAL
            ENDIF ELSE BEGIN
               cgPlotS, [x0,x1], [y,y]-(j*y_offset), PSYM=(*self.psyms)[j], THICK=symthick, $
                        SYMSIZE=self.symsize, SYMCOLOR=symcolors[j], /NORMAL
            ENDELSE
        ENDIF
    
        ; Draw the title.
        IF self.hardware THEN BEGIN
            thisFont = !P.Font
            !P.Font = (!D.Name EQ 'PS') ? 1 : 0
        ENDIF
        cgText, x1+(2.0*!D.X_CH_SIZE/!D.X_Size), y-(0.5*!D.Y_CH_SIZE/!D.Y_Size)-(j*y_offset),$
            /NORMAL, ALIGNMENT=0.0, (*self.titles)[j], COLOR=tcolors[j], $
            TT_FONT=*self.tt_font, CHARSIZE=self.charsize, FONT=!P.Font, WIDTH=width, CHARTHICK=thick
        IF self.hardware THEN !P.Font = thisFont
        
    ENDFOR
    
    ; Calculate the end position of the legend box.
     bx0 = self.bx_pos[0]
     bx1 = self.bx_pos[2]
     by0 = self.bx_pos[1]
     by1 = self.bx_pos[3]
    
    ; Draw the box, if needed.
    IF self.box THEN BEGIN
        cgPlots, [bx0, bx0, bx1, bx1, bx0], [by1, by0, by0, by1, by1], /Normal, $
            Color=bx_color, Thick=bx_thick
     ENDIF
    
     ; We have to calculate the box each time, or else this won't work in resizeable
     ; graphics windows.
     self.bx_pos = FltArr(4)

    ; Restore starting color state.
    cgSetColorState, incomingColorState
    
END


;+--------------------------------------------------------------------------
; This method obtains properties from the object.
;
; :Keywords:
;     alignment: out, optional, type=integer
;        The current alignment of the legend box.
;     background: out, optional, type=boolean, default=0
;        Set this keyword to draw a colored background for the legend.
;     bg_color: out, optional, type=string, default="white"
;        The name of the background color.
;     box: out, optional, type=boolean, default=0
;        Set to 1 if a box is drawn around the legend items.
;     bx_color: out, optional
;        The color of the box drawn around the legend items.
;     bx_thick: out, optional, type=float
;        The thickness of the line used to draw the box around the legend items.
;     center_sym: out, optional, type=boolean
;        This keyword is set if symbols are placed in the center of the line.
;     charsize: out, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     colors: out, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     data: out, optional, type=boolean
;        Indicates if the `Location` is in data coordinates.
;     hardware: out, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: out, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyles: out, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: out, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;     psyms: out, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolors: out, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLORS` keyword.
;     symsize: out, optional, type=float, default=1.0
;        The symbol size.
;     symthick: out, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolors: out, optional, type=string/strarr
;        The `Titles` color. Set by default to `Colors`.
;     thick: out, optional, type=float, default=1.0
;        The thickness of the line.
;     titles: out, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: out, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: out, optional, type=boolean, default=1
;        The current visibility of the legend.
;     vspace: out, optional, type=float, default=1.5
;        A scale factor for vertical spacing between legend items. This number is multiplied by
;        `Charsize` to determine vertical spacing.
;---------------------------------------------------------------------------
PRO cgLegendItem::GetProperty, $
   ALIGNMENT=alignment, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   BOX=box, $
   BX_COLOR=bx_color, $
   BX_THICK=bx_thick, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   COLORS=colors, $
   DATA=data, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLES=linestyles, $
   LOCATION=location, $
   PSYMS=psyms, $
   SYMCOLORS=symcolors, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TCOLORS=tcolors, $
   THICK=thick, $
   TITLES=titles, $
   TT_FONT=tt_font, $
   VISIBLE=visible, $
   VSPACE=vspace

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF Arg_Present(alignment) THEN alignment = self.alignment
    IF Arg_Present(background) THEN background = self.background
    IF Arg_Present(bg_color) THEN bg_color = *self.bg_color
    IF Arg_Present(box) THEN box = self.box
    IF Arg_Present(bx_color) THEN bx_color = *self.bx_color
    IF Arg_Present(bx_thick) THEN bx_thick = self.bx_thick
    IF Arg_Present(center_sym) THEN center_sym = self.center_sym
    IF Arg_Present(charsize) THEN charsize = self.charsize
    IF Arg_Present(colors) THEN colors = *self.colors
    IF Arg_Present(data) THEN data = self.data
    IF Arg_Present(hardware) THEN hardware = self.hardware
    IF Arg_Present(length) THEN length = self.length
    IF Arg_Present(linestyles) THEN linestyles = *self.linestyles
    IF Arg_Present(location) THEN location = self.location
    IF Arg_Present(psyms) THEN psyms = *self.psyms
    IF Arg_Present(symcolors) THEN symcolors = *self.symcolors
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(symthick) THEN symthick = self.symthick
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(tcolors) THEN tcolors = *self.tcolors
    IF Arg_Present(titles) THEN titles = *self.titles
    IF Arg_Present(tt_font) THEN tt_font = *self.tt_font
    IF Arg_Present(visible) THEN visible = self.visible
    IF Arg_Present(vspace) THEN vspace = self.vspace

END


;+--------------------------------------------------------------------------
; This method sets properties of the object.
; 
; :Keywords:
;     alignment: in, optional, type=integer, default=0
;        This keyword affects the alignment of the legend box with respect to the `Location` point.
;        There are eight possible values (0 to 7) that assign the location point to be one of the
;        four corners of the legend box, or the middle of one of the four sides of the legend box.
;        The values are specified as follows::
;           0 - Location specifies the upper left corner of the legend box.
;           1 - Location specifies the upper right corner of the legend box.
;           2 - Location specifies the lower right corner of the legend box.
;           3 - Location specifies the lower left corner of the legend box.
;           4 - Location specifies the top side of the legend box (centered horizontally).
;           5 - Location specifies the bottom side of the legend box (centered horizontally).
;           6 - Location specifies the left side of the legend box (centered vertically).
;           7 - Location specifies the right side of the legend box (centered vertically).
;     background: in, optional, type=boolean, default=0
;        Set this keyword to draw a colored background for the legend.
;     bg_color: in, optional, type=string, default="white"
;        The name of the background color.
;     box: in, optional, type=boolean, default=0
;        Set this keyword to draw a box around the legend items.
;     bx_color: in, optional, type=varies, default="black"
;        The color of the box drawn around the legend items.
;     bx_thick: in, optional, type=float
;        The thickness of the line used to draw the box around the legend items.
;        If not set, use !P.Thick at drawing time.
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     colors: in, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     data: in, optional, type=boolean, default=0
;        If set the values specified by the `Location` keyword are taken to be in data
;        coordinate space.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyles: in, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: in, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95].
;     psyms: in, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolors: in, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolors: in, optional, type=string/strarr
;        The `Titles` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     titles: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;     vspace: in, optional, type=float, default=1.5
;        A scale factor for vertical spacing between legend items. This number is multiplied by
;        `Charsize` to determine vertical spacing.
;---------------------------------------------------------------------------
PRO cgLegendItem::SetProperty, $
   ALIGNMENT=alignment, $ $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   BOX=box, $
   BX_COLOR=bx_color, $
   BX_THICK=bx_thick, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   COLORS=colors, $
   DATA=data, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLES=linestyles, $
   LOCATION=location, $
   PSYMS=psyms, $
   SYMCOLORS=symcolors, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TCOLORS=tcolors, $
   THICK=thick, $
   TITLES=titles, $
   TT_FONT=tt_font, $
   VISIBLE=visible, $
   VSPACE=vspace


    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    ; Set the appropriate alignment keywords.
    IF N_Elements(alignment) NE 0 THEN BEGIN
        alignment = 0 > alignment < 7
        CASE alignment OF
            1: BEGIN
                align_hcenter = 0
                align_vcenter = 0
                align_right = 0
                align_bottom = 0
            END
            1: BEGIN
                align_hcenter = 0
                align_vcenter = 0
                align_right = 1
                align_bottom = 0
            END
            2: BEGIN
                align_hcenter = 0
                align_vcenter = 0
                align_right = 1
                align_bottom = 1
            END
            3: BEGIN
                align_hcenter = 0
                align_vcenter = 0
                align_right = 0
                align_bottom = 1
            END
            4: BEGIN
                align_hcenter = 1
                align_vcenter = 0
                align_right = 0
                align_bottom = 0
            END
            5: BEGIN
                align_hcenter = 1
                align_vcenter = 0
                align_right = 0
                align_bottom = 1
            END
            6: BEGIN
                align_hcenter = 0
                align_vcenter = 1
                align_right = 0
                align_bottom = 0
            END
            7: BEGIN
                align_hcenter = 0
                align_vcenter = 1
                align_right = 1
                align_bottom = 0
            END
        ENDCASE
        self.alignment = alignment
    ENDIF
    
    IF N_Elements(align_bottom) NE 0 THEN self.align_bottom = Keyword_Set(align_bottom)
    IF N_Elements(align_hcenter) NE 0 THEN self.align_hcenter = Keyword_Set(align_hcenter)
    IF N_Elements(align_vcenter) NE 0 THEN self.align_vcenter = Keyword_Set(align_vcenter)
    IF N_Elements(align_right) NE 0 THEN self.align_right = Keyword_Set(align_right)
    IF N_Elements(background) NE 0 THEN self.background = Keyword_Set(background)
    IF N_Elements(bg_color) NE 0 THEN self.bg_color = bg_color
    IF N_Elements(box) NE 0 THEN self.box = Keyword_Set(box)
    IF N_Elements(bx_color) NE 0 THEN self.bx_color = bx_color
    IF N_Elements(bx_thick) NE 0 THEN self.bx_thick = bx_thick
    IF N_Elements(center_sym) NE 0 THEN self.center_sym = center_sym
    IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
    IF N_Elements(colors) NE 0 THEN *self.colors = colors
    IF N_Elements(data) NE 0 THEN self.data = Keyword_Set(data)
    IF N_Elements(hardware) NE 0 THEN self.hardware = Keyword_Set(hardware)
    IF N_Elements(length) NE 0 THEN self.length = length
    IF N_Elements(linestyles) NE 0 THEN *self.linestyles = linestyles
    IF N_Elements(location) NE 0 THEN self.location = location
    IF N_Elements(psyms) NE 0 THEN *self.psyms = psyms
    IF N_Elements(symcolors) NE 0 THEN *self.symcolors = symcolors
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(tcolors) NE 0 THEN *self.tcolors = tcolors
    IF N_Elements(titles) NE 0 THEN *self.titles = titles
    IF N_Elements(tt_font) NE 0 THEN *self.tt_font = tt_font
    IF N_Elements(visible) NE 0 THEN self.visible = Keyword_Set(visible)
    IF N_Elements(vspace) NE 0 THEN *self.vspace = vspace
   
END


;+--------------------------------------------------------------------------
; This method destroys anything the object uses that retains memory space.
;---------------------------------------------------------------------------
PRO cgLegendItem::CLEANUP

   Ptr_Free, self.bg_color
   Ptr_Free, self.colors
   Ptr_Free, self.linestyles
   Ptr_Free, self.psyms
   Ptr_Free, self.symcolors
   Ptr_Free, self.tcolors
   Ptr_Free, self.titles
   Ptr_Free, self.tt_font
   Ptr_Free, self.width
   
END


;+--------------------------------------------------------------------------
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;---------------------------------------------------------------------------
PRO cgLegendItem__Define, class

    Compile_Opt idl2

    class = { cgLegendItem, $
              INHERITS IDL_Object, $
              alignment: 0L, $
              align_hcenter: 0, $
              align_vcenter: 0, $
              align_bottom: 0, $
              align_right: 0, $
              background: 0, $              ; A flag that indicates a background should be drawn.
              bg_color: Ptr_New(), $        ; The color of the background.
              box: 0, $                     ; A flag that indicates a box should be drawn around legend.
              bx_color: Ptr_New(), $        ; The box color.
              bx_pos: FltArr(4), $          ; The position of the legend box in the window.
              bx_thick: 0.0, $              ; The thickness of the box line.
              charsize: 0.0, $              ; The character size of the legend titles.
              center_sym: 0, $              ; A flag indicating the symbol should be centered on the line.
              colors: Ptr_New(), $          ; The color of the lines connecting symbols.
              data: 0, $                    ; A flag that indicates the location of in data coordinate space.
              tt_font: Ptr_New(), $         ; The name of a true-type font to use for the title.
              hardware: 0, $                ; A flag indicating hardware fonts.
              length: 0.0, $                ; The length of the lines connecting legend symbols.
              linestyles: Ptr_New(), $      ; The linestyle of the lines connecting legend symbols.
              location: FltArr(2), $        ; The location of the upper left corner of the legend item.
              psyms: Ptr_New(), $           ; The number of the symbol to use on the legend item.
              symcolors: Ptr_New(), $       ; The color of the symbols used in the legend items.
              symsize: "", $                ; The size of the symbols.
              symthick: 0.0, $              ; The thickness of the lines used to draw symbols
              thick: 0.0, $                 ; The thickness of the lines connecting symbols.
              tcolors: Ptr_New(), $         ; The color of the title for the legend item.
              titles: Ptr_New(), $          ; The text of the title for the legend item.
              visible: 0,  $                ; A flag to indicate if the legend should be drawn.
              vspace: 0.0, $                ; The vertical spacing between legend items.
              wid: 0L, $                    ; The window index number of a cgWindow if added to it.
              width: Ptr_New() $            ; The width of each legend item.
            }
END