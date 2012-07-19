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
;-

;+
; This method draws the legend item in a graphics window.
;-
PRO cgLegendItem::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        IF N_Elements(thisFont) NE 0 THEN !P.Font = thisFont
        RETURN
    ENDIF
    
    ; If this is not visible, return now.
    IF ~self.visible THEN RETURN
    
    x0 = self.location[0]
    x1 = x0 + self.length
    y = self.location[1]
    
    ; Need to draw a line?
    IF x0 NE x1 THEN BEGIN
       cgPlotS, [x0,x1], [y,y], COLOR=self.color, LINESTYLE=self.lineStyle, THICK=self.thick, /NORMAL
    ENDIF
    
    ; Need to draw symbols?
    IF self.psym NE 0 THEN BEGIN
       cgPlotS, [x0,x1], [y,y], PSYM=self.psym, SYMSIZE=self.symsize, SYMCOLOR=self.symColor, /NORMAL
    ENDIF
    
    ; Draw the title.
    IF self.hardware THEN BEGIN
        thisFont = !P.Font
        !P.Font = (!D.Name EQ 'PS') ? 1 : 0
    ENDIF
    cgText, x1+(2.0*!D.X_CH_SIZE/!D.X_Size), y-(0.5*!D.Y_CH_SIZE/!D.Y_Size),$
        /NORMAL, ALIGNMENT=0.0, self.title, COLOR=self.tcolor, $
        TT_FONT=*self.tt_font, CHARSIZE=self.charsize, FONT=!P.Font
    IF self.hardware THEN !P.Font = thisFont
    
END


;+
; This method obtains properties from the object.
;
; :Keywords:
;     charsize: out, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: out, optional, type=string
;        The name of the data color. This is the color of the data line.
;     hardware: out, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: out, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: out, optional, type=integer
;        The line style for drawing the line.
;     location: out, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;     psym: out, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: out, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: out, optional, type=float, default=1.0
;        The symbol size.
;     symthick: out, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: out, optional, type=string
;        The `Title` color. Set by default to `Color`.
;     thick: out, optional, type=float, default=1.0
;        The thickness of the line.
;     title: out, optional, type=string, default='Plot Item'
;        The "title" or text for the legend item.
;     tt_font: out, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: out, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
PRO cgLegendItem::GetProperty, $
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
        void = Error_Message()
        RETURN
    ENDIF

    IF Arg_Present(charsize) THEN charsize = self.charsize
    IF Arg_Present(color) THEN color = self.color
    IF Arg_Present(hardware) THEN hardware = self.hardware
    IF Arg_Present(length) THEN length = self.length
    IF Arg_Present(linestyle) THEN linestyle = self.linestyle
    IF Arg_Present(location) THEN location = self.location
    IF Arg_Present(psym) THEN psym = self.psym
    IF Arg_Present(symcolor) THEN symcolor = self.symcolor
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(symthick) THEN symthick = self.symthick
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(tcolor) THEN tcolor = self.tcolor
    IF Arg_Present(title) THEN title = self.title
    IF Arg_Present(tt_font) THEN tt_font = *self.tt_font
    IF Arg_Present(visible) THEN visible = self.visible

END


;+
; This method sets properties of the object.
; 
; :Keywords:
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string
;        The name of the data color. This is the color of the data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer
;        The line style for drawing the line.
;     location: in, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95].
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: in, optional, type=string
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string, default='Plot Item'
;        The "title" or text for the legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
PRO cgLegendItem::SetProperty, $
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
        void = Error_Message()
        RETURN
    ENDIF

    IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(hardware) NE 0 THEN self.hardware = hardware
    IF N_Elements(length) NE 0 THEN self.length = length
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(location) NE 0 THEN self.location = location
    IF N_Elements(psym) NE 0 THEN self.psym = psym
    IF N_Elements(symcolor) NE 0 THEN self.symcolor = symcolor
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(tcolor) NE 0 THEN self.tcolor = tcolor
    IF N_Elements(title) NE 0 THEN self.title = title
    IF N_Elements(tt_font) NE 0 THEN *self.tt_font = tt_font
    IF N_Elements(visible) NE 0 THEN self.visible = visible
    
END


;+
; This method destroys anything the object uses that retains memory space.
;-
PRO cgLegendItem::CLEANUP
   Ptr_Free, self.tt_font
END


;+
; This method creates an instance of the object.
; 
; :Keywords:
;     charsize: in, optional, type=float
;        The character size for the legend text. Uses cgDefCharsize by default.
;        Ignored if using hardware fonts on the display.
;     color: in, optional, type=string
;        The name of the data color. This is the color of the data line.
;     hardware: in, optional, type=boolean
;        Set this keyword if you want to output the legend text in a hardware font.
;     length: in, optional, type=float, default=0.075
;        The length of the line connecting symbols in the legend, in normalized
;        coordinates (0 to 1 in the graphics window). Set this equal to 0.0 if
;        you wish to only plot symbols in the legend.
;     linestyle: in, optional, type=integer
;        The line style for drawing the line.
;     location: in, optional, type=fltarr
;        The location of the upper-left corner of the legend item,
;        in normalized coordinates (0 to 1 in the graphics window).
;        The default is [0.1, 0.95].
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     tcolor: in, optional, type=string
;        The `Title` color. Set by default to `Color`.
;     thick: in, optional, type=float, default=1.0
;        The thickness of the line.
;     title: in, optional, type=string, default='Plot Item'
;        The "title" or text for the legend item.
;     tt_font: in, optional, type=string
;        The name of a true-type font to use for the legend text.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
FUNCTION cgLegendItem::INIT, $
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
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Define default parameters.
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharsize()
    SetDefaultValue, color, 'black'
    SetDefaultValue, hardware, 0
    SetDefaultValue, length, 0.075
    SetDefaultValue, linestyle, 0
    SetDefaultValue, location, [0.1, 0.95]
    SetDefaultValue, psym, 0
    SetDefaultValue, symcolor, color
    SetDefaultValue, symsize, 1.0
    SetDefaultValue, symthick, 1.0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, tcolor, color
    SetDefaultValue, title, 'Plot Item'
    SetDefaultValue, visible, 1
    
    ; Populate the object.
    self.charsize = charsize
    self.color = color
    IF N_Elements(tt_font) NE 0 THEN self.tt_font = Ptr_New(tt_font) ELSE self.tt_font = Ptr_New(/ALLOCATE_HEAP)
    self.hardware = hardware
    self.length = length
    self.linestyle = linestyle
    self.location = location
    self.psym = psym
    self.symcolor = symcolor
    self.symsize = symsize
    self.symthick = symthick
    self.tcolor = tcolor
    self.thick = thick
    self.title = title
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

    class = { CGLEGENDITEM, $
              INHERITS IDL_Object, $
              charsize: 0.0, $
              color: "", $
              tt_font: Ptr_New(), $
              hardware: 0, $
              length: 0.0, $
              linestyle: 0L, $
              location: FltArr(2), $
              psym: 0L, $ 
              symcolor: "", $
              symsize: "", $
              symthick: 0.0, $
              thick: 0.0, $
              tcolor: "", $
              title: "", $
              visible: 0 $
            }
END