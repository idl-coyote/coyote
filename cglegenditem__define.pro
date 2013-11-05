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
;   The purpose of this program is to create a simple legend object that can be drawn on
;   a data plot. Perhaps later such objects can be collected into a more sophisticated
;   "legend" object.
;        
; :Categories:
;    Graphics
;    
; :Examples:
;    A plot with a simple legend::
;       cgDisplay, 800, 450
;       legendItem1 = Obj_New('cgLegendItem', SymColor='red7', PSym=6, Symsize=1.5, $
;           Location=[0.825, 0.875], Title='May 27', /Hardware, Length=0.05)
;       legendItem2 = Obj_New('cgLegendItem', SymColor='blu7', PSym=15, Symsize=1.5, $
;           Location=[0.825, 0.835], Title='June 27', /Hardware, Length=0.05)
;       cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.8, 0.9], $
;          Legends=[legendItem1,legendItem2], Label='First Experiment'
;       cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;       Obj_Destroy, [legendItem1,legendItem2]
;
;    Same as the previous example, but with a single legend object and with the symbol
;    centered on the line::
;       cgDisplay, 800, 450
;       legendItem = Obj_New('cgLegendItem', SymColor=['red7', 'blu7'], $
;           PSym=[6,15], Symsize=1.5, Location=[0.825, 0.875], Title=['May 27', 'Jun 27'], $
;           /CENTER_SYM, /Hardware, Length=0.05)
;       cgPlot, cgDemoData(17), PSym=-6, SymColor='red7', Position=[0.15, 0.15, 0.8, 0.9], $
;          Legends=legendItem, Label='First Experiment'
;       cgOPlot, cgDemoData(17), PSym=-15, SymColor='blu7'
;       Obj_Destroy, legendItem
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
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;     
; :History:
;     Change History::
;        Written 18 July 2012. DWF.
;        Adapted to accept multiple legend elements. Legend elements are stacked vertically. 
;           Each legend element can be given its own title, color, symbol symbol color, and 
;           linestyle. Legend elements are offset  by 1.3*!D.Y_CH_Size/!D.YSize.  A single 
;           symbol can now be drawn in the center of the line instead of one at each end 
;           point with the CENTER_SYM keyword. 04/25/2013, Matthew Argall.
;-

;+
; This method draws the legend item in a graphics window.
;-
PRO cgLegendItem::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        RETURN
    ENDIF
    
    ; If this is not visible, return now.
    IF ~self.visible THEN RETURN
    
    x0 = self.location[0]
    x1 = x0 + self.length
    y = self.location[1]
    y_offset = 1.3 * !D.Y_CH_Size/!D.Y_Size

    ;For each legend element
    FOR j = 0, N_Elements(*self.title) - 1 DO BEGIN
        
        ; Need to draw a line?
        IF x0 NE x1 THEN BEGIN
           cgPlotS, [x0,x1], [y,y]-(j*y_offset), COLOR=(*self.color)[j], $
                    LINESTYLE=(*self.lineStyle)[j], THICK=self.thick, /NORMAL
        ENDIF
    
        ; Need to draw symbols?
        IF (*self.psym)[j] NE 0 THEN BEGIN
            ; Center the symbol on the line?
            IF self.center_sym THEN BEGIN
                x2 = x0 + (x1 - x0) / 2.0
               cgPlotS, [x2,x2], [y,y]-(j*y_offset), PSYM=(*self.psym)[j], $
                        SYMSIZE=self.symsize, SYMCOLOR=(*self.symColor)[j], /NORMAL
            ENDIF ELSE BEGIN
               cgPlotS, [x0,x1], [y,y]-(j*y_offset), PSYM=(*self.psym)[j], $
                        SYMSIZE=self.symsize, SYMCOLOR=(*self.symColor)[j], /NORMAL
            ENDELSE
        ENDIF
    
        ; Draw the title.
        IF self.hardware THEN BEGIN
            thisFont = !P.Font
            !P.Font = (!D.Name EQ 'PS') ? 1 : 0
        ENDIF
        cgText, x1+(2.0*!D.X_CH_SIZE/!D.X_Size), y-(0.5*!D.Y_CH_SIZE/!D.Y_Size)-(j*y_offset),$
            /NORMAL, ALIGNMENT=0.0, (*self.title)[j], COLOR=(*self.tcolor)[j], $
            TT_FONT=*self.tt_font, CHARSIZE=self.charsize, FONT=!P.Font
        IF self.hardware THEN !P.Font = thisFont
        
    ENDFOR
    
END


;+
; This method obtains properties from the object.
;
; :Keywords:
;     center_sym: out, optional, type=boolean
;        This keyword is set if symbols are placed in the center of the line.
;     charsize: out, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: out, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: out, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: out, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: out, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: out, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;     psym: out, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: out, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: out, optional, type=float, default=1.0
;        The symbol size.
;     symthick: out, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: out, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: out, optional, type=float, default=1.0
;        The thickness of the line.
;     title: out, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: out, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: out, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
PRO cgLegendItem::GetProperty, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   COLOR=color, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
   VISIBLE=visible

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF Arg_Present(center_sym) THEN center_sym = self.center_sym
    IF Arg_Present(charsize) THEN charsize = self.charsize
    IF Arg_Present(color) THEN color = *self.color
    IF Arg_Present(hardware) THEN hardware = self.hardware
    IF Arg_Present(length) THEN length = self.length
    IF Arg_Present(linestyle) THEN linestyle = *self.linestyle
    IF Arg_Present(location) THEN location = self.location
    IF Arg_Present(psym) THEN psym = *self.psym
    IF Arg_Present(symcolor) THEN symcolor = *self.symcolor
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(symthick) THEN symthick = self.symthick
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(tcolor) THEN tcolor = *self.tcolor
    IF Arg_Present(title) THEN title = *self.title
    IF Arg_Present(tt_font) THEN tt_font = *self.tt_font
    IF Arg_Present(visible) THEN visible = self.visible

END


;+
; This method sets properties of the object.
; 
; :Keywords:
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: in, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95].
;     psym: in, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: in, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
PRO cgLegendItem::SetProperty, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   COLOR=color, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
   VISIBLE=visible

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    IF N_Elements(center_sym) NE 0 THEN self.center_sym = center_sym
    IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
    IF N_Elements(color) NE 0 THEN *self.color = color
    IF N_Elements(hardware) NE 0 THEN self.hardware = hardware
    IF N_Elements(length) NE 0 THEN self.length = length
    IF N_Elements(linestyle) NE 0 THEN *self.linestyle = linestyle
    IF N_Elements(location) NE 0 THEN self.location = location
    IF N_Elements(psym) NE 0 THEN *self.psym = psym
    IF N_Elements(symcolor) NE 0 THEN *self.symcolor = symcolor
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(tcolor) NE 0 THEN *self.tcolor = tcolor
    IF N_Elements(title) NE 0 THEN *self.title = title
    IF N_Elements(tt_font) NE 0 THEN *self.tt_font = tt_font
    IF N_Elements(visible) NE 0 THEN self.visible = visible
    
END


;+
; This method destroys anything the object uses that retains memory space.
;-
PRO cgLegendItem::CLEANUP

   Ptr_Free, self.color
   Ptr_Free, self.linestyle
   Ptr_Free, self.psym
   Ptr_Free, self.symcolor
   Ptr_Free, self.tcolor
   Ptr_Free, self.title
   Ptr_Free, self.tt_font
   
END


;+
; This method creates an instance of the object.
;
; :Keywords:
;     center_sym: in, optional, type=boolean
;        Set this keyword to place a single symbol in the center of the line. The default
;        is to draw a symbol at each endpoint of the line.
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string/strarr
;        The name of the data color. This is the color of each data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer/intarr
;        The line style for drawing each line.
;     location: in, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95].
;     psym: in, optional, type=integer/intarr
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/strarr
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: in, optional, type=string/strarr
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string/strarr, default='Plot Item'
;        The "title" or text for each legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
FUNCTION cgLegendItem::INIT, $
   CENTER_SYM=center_sym, $
   CHARSIZE=charsize, $
   COLOR=color, $
   HARDWARE=hardware, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   TCOLOR=tcolor, $
   THICK=thick, $
   TITLE=title, $
   TT_FONT=tt_font, $
   VISIBLE=visible

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, 0
    ENDIF
    
    nlegends = N_Elements(title)
    IF nlegends EQ 0 THEN BEGIN
        title = 'Plot Item'
        nlegends = 1
    ENDIF 
    
    ; Define default parameters.
    SetDefaultValue, center_sym, /BOOLEAN
    IF N_Elements(charsize)  EQ 0 THEN charsize = cgDefCharsize()
    IF N_Elements(color)     EQ 0 THEN color = Replicate('black', nlegends)
    IF N_Elements(linestyle) EQ 0 THEN linestyle = Replicate(0, nlegends)
    IF N_Elements(psym)      EQ 0 THEN psym = Replicate(0, nlegends)
    SetDefaultValue, hardware, 0
    SetDefaultValue, length, 0.075
    SetDefaultValue, location, [0.1, 0.95]
    SetDefaultValue, symcolor, color
    SetDefaultValue, symsize, 1.0
    SetDefaultValue, symthick, 1.0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, tcolor, color
    SetDefaultValue, visible, 1
    
    ; Populate the object.
    self.center_sym = center_sym
    self.charsize = charsize
    self.color = Ptr_New(color)
    IF N_Elements(tt_font) NE 0 THEN self.tt_font = Ptr_New(tt_font) ELSE self.tt_font = Ptr_New(/ALLOCATE_HEAP)
    self.hardware = hardware
    self.length = length
    self.linestyle = Ptr_New(linestyle)
    self.location = location
    self.psym = Ptr_New(psym)
    self.symcolor = Ptr_New(symcolor)
    self.symsize = symsize
    self.symthick = symthick
    self.tcolor = Ptr_New(tcolor)
    self.thick = thick
    self.title = Ptr_New(title)
    self.visible = visible

    RETURN, 1
END

;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO cgLegendItem__Define, class

    Compile_Opt idl2

    class = { cgLegendItem, $
              INHERITS IDL_Object, $
              charsize: 0.0, $
              center_sym: 0, $
              color: Ptr_New(), $           ;""
              tt_font: Ptr_New(), $
              hardware: 0, $
              length: 0.0, $
              linestyle: Ptr_New(), $       ;""
              location: FltArr(2), $
              psym: Ptr_New(), $            ;0L
              symcolor: Ptr_New(), $        ;""
              symsize: "", $
              symthick: 0.0, $
              thick: 0.0, $
              tcolor: Ptr_New(), $          ;""
              title: Ptr_New(), $           ;""
              visible: 0 $
            }
END