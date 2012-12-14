; docformat = 'rst'
;
; NAME:
;   cgOverPlot__Define
;
; PURPOSE:
;   The purpose of this program is to create a data object that can be plotted or drawn on
;   a set of axes set up by another plotting command.
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
; The purpose of this program is to create a data object that can be plotted or drawn on
; a set of axes set up by another plotting command.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Use, for example, with the cgPlot command::
;       oplotObj1 = Obj_New('cgOverPlot', cgDemoData(17), Color='red', PSYM=-1, LINESTYLE=2)
;       oplotObj2 = Obj_New('cgOverPlot', cgDemoData(17), Color='blue', PSYM=-2, LINESTYLE=4)
;       cgPlot, cgDemoData(17), Color='purple', PSYM=-4, OPLOTS=[oplotObj1, oplotObj2]
;       Obj_Destroy, [oplotObj1, oplotObj2]
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
;        Written, 18 July  2012. DWF.
;        The THICK keyword was not being set in the INIT method. Fixed. 2 Nov 2012. DWF.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-

;+
; The initialization method of the object. Called automatically when the object
; is created.
; 
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     color: in, optional, type=string, default='opposite'
;        The name of the data color. This is the color of the data line.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the data is drawn as soon as the object is created.
;     linestyle: in, optional, type=integer, default=0
;        The line style for drawing the line.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     skip: in, optional, type=integer, default=1
;        The number of data points to skip when the line is drawn. The default is to
;        not skip any data points, but to plot them all.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     thick: in, optional, type=float, default = 1.0
;        The thickness of the line.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
FUNCTION cgOverPlot::INIT, x, y, $
    ADDCMD=addcmd, $
    COLOR=color, $
    DRAW=draw, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SKIP=skip, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick, $
    VISIBLE=visible
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN, 0
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgOverPlot, x, y'
        RETURN, 0
    ENDIF
    
    ; Adding this object to a window?
    IF Keyword_Set(addcmd) THEN BEGIN
        cgControl, Execute=0
        cgWindow, 'Draw', self, ADDCMD=1
       ENDIF
     
    ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       dep = x
       indep = Findgen(N_Elements(dep))
       ENDCASE
    
       2: BEGIN
       dep = y
       indep = x
       ENDCASE
    
    ENDCASE
    
    ; If either of these input vectors are scalars, make them vectors.
    IF N_Elements(dep) EQ 1 THEN dep = [dep]
    IF N_Elements(indep) EQ 1 THEN indep = [indep]
    
    ; Default colors.
    SetDecomposedState, 1, CURRENTSTATE=currentState
    color = cgDefaultColor(color, DEFAULT="opposite")
    symcolor = cgDefaultColor(symcolor, DEFAULT=color)

    ; Other parameter defaults.
    IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
    IF N_Elements(psym) EQ 0 THEN psym = 0
    IF N_Elements(thick) EQ 0 THEN thick = 1.0
    IF N_Elements(skip) EQ 0 THEN skip = 1
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
    IF N_Elements(symthick) EQ 0 THEN symthick = 1.0
    IF N_Elements(visible) EQ 0 THEN visible = 1
    
    ; Load the object.
    self.dep = Ptr_New(dep)
    self.indep = Ptr_New(indep)
    self.color = color
    self.linestyle = linestyle    
    self.psym = psym
    self.thick = thick
    self.skip = 1
    self.symcolor = symcolor
    self.symsize = symsize
    self.symthick = symthick
    self.xrange = [Min(indep), Max(indep)]
    self.yrange = [Min(dep), Max(dep)]
    self.visible = visible
    
    IF Keyword_Set(draw) THEN self -> Draw
    IF Keyword_Set(addcmd) THEN cgControl, Execute=1
    
    RETURN, 1
 
END

;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO cgOverPlot::CLEANUP

    Ptr_Free, self.dep
    Ptr_Free, self.indep
    
END



;+
; This method draws the overplot object. It assumes a set of axes has been
; established by some other graphics command.
;
; :Keywords:
;     skip: in, optional, type=integer
;         Set this keyword to the number of data points to skip when drawing the
;         data vectors. Similar to the SKIP parameter, but this keyword gives you
;         the opportunity to temporarily over-ride the value of the SKIP parameter.
;-
PRO cgOverPlot::Draw, SKIP=skip

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; If the plot is not visible, return now.
    IF ~self.visible THEN RETURN
    
    ; Are we skipping data points?
    IF N_Elements(skip) NE 0 THEN skipCount = skip
    IF N_Elements(skipCount) EQ 0 THEN skipCount = self.skip

    ; If you aren't skipping, draw all the data.
    IF skipCount EQ 1 THEN BEGIN
        asymbol = cgSymCat(self.psym, THICK=self.symThick, COLOR=self.symcolor)
        IF self.psym LT 0 THEN asymbol = -asymbol
        OPlot, *self.indep, *self.dep, COLOR=cgColor(self.color), LINESTYLE=self.linestyle, $
            PSYM=asymbol, SYMSIZE=self.symsize, THICK=self.thick
    ENDIF ELSE BEGIN
        count = N_Elements(*self.dep)-1
        asymbol = cgSymCat(self.psym, THICK=self.symThick, COLOR=self.symcolor)
        IF self.psym LT 0 THEN asymbol = -asymbol 
        OPlot, (*self.indep)[0:count:skipCount], (*self.dep)[0:count:skipCount], $
            COLOR=cgColor(self.color), LINESTYLE=self.linestyle, $
            PSYM=asymbol, SYMSIZE=self.symsize, THICK=self.thick
    ENDELSE
    
END


;+
; This method obtains the current properties of the object. 
; 
; :Keywords:
;     color: out, optional, type=string
;        The name of the data color. This is the color of the data line.
;     dep: out, optional, type=varies
;        The current dependent data of the object.
;     draw: out, optional, type=boolean
;        If this keyword is set, the data is drawn as soon as the object is created.
;     indep: out, optional, type=varies
;        The current independent data of the object.
;     linestyle: out, optional, type=integer
;        The line style for drawing the line.
;     psym: out, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     skip: out, optional, type=integer, default=1
;        The number of data points to skip when the line is drawn. 
;     symcolor: out, optional, type=string
;        The name of the symbol color. 
;     symsize: out, optional, type=float
;        The symbol size.
;     symthick: out, optional, type=float
;        The thickness of the symbol.
;     thick: out, optional, type=float
;        The thickness of the line.
;     xrange: out, optional, type=fltarr
;        The range of the dependent data.
;     yrange: out, optional, type=fltarr
;        The range of the independent data.
;     visible: out, optional, type=boolean
;        The visibility of the objects plot line.
;-
PRO cgOverPlot::GetProperty, $
    COLOR=color, $
    DEP=dep, $
    DRAW=draw, $
    INDEP=indep, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SKIP=skip, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    VISIBLE=visible

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    IF Arg_Present(color) THEN color = self.color
    IF Arg_Present(dep) THEN dep = *self.dep
    IF Arg_Present(indep) THEN indep = *self.indep
    IF Arg_Present(linestyle) THEN linestyle = self.linestyle
    IF Arg_Present(psym) THEN psym = self.psym
    IF Arg_Present(skip) THEN skip = self.skip
    IF Arg_Present(symcolor) THEN symcolor = self.symcolor
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(symthick) THEN symthick = self.symthick
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(xrange) THEN xrange = self.xrange
    IF Arg_Present(yrange) THEN yrange = self.yrange
    IF Arg_Present(visible) THEN visible = self.visible
    
END
    

;+
; This method sets the properties of the object.
;
; :Keywords:
;     color: in, optional, type=string
;        The name of the data color. This is the color of the data line.
;     dep: in, optional, type=varies
;        A vector of dependent data.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the data is drawn as soon as the properties are set.
;     indep: in, optional, type=varies
;        A vector of independent data.
;     linestyle: in, optional, type=integer
;        The line style for drawing the line.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     skip: in, optional, type=integer, default=1
;        The number of data points to skip when the line is drawn. The default is to
;        not skip any data points, but to plot them all.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     thick: in, optional, type=float, default = 1.0
;        The thickness of the line.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
PRO cgOverPlot::SetProperty, $
    COLOR=color, $
    DEP=dep, $
    DRAW=draw, $
    INDEP=indep, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SKIP=skip, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick, $
    VISIBLE=visible

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(dep) NE 0 THEN *self.dep = dep
    IF N_Elements(indep) NE 0 THEN *self.indep = indep
    self.yrange = [Min(*self.dep), Max(*self.dep)]
    self.xrange = [Min(*self.indep), Max(*self.indep)]
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(psym) NE 0 THEN self.psym = psym
    IF N_Elements(skip) NE 0 THEN self.skip = skip
    IF N_Elements(symcolor) NE 0 THEN self.symcolor = symcolor
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(visible) NE 0 THEN self.visible = visible
    
    ; Need to draw?
    IF Keyword_Set(draw) THEN self -> Draw
     
END

;+
; The class definition module for the object.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO cgOverPlot__Define, class

    class = { CGOVERPLOT, $
              INHERITS IDL_OBJECT, $ ; To allow IDL 8 dot referencing of properties.
              dep: Ptr_New(), $      ; The dependent data.
              indep: Ptr_New(), $    ; The independent data.
              color: "", $           ; The color of the overplotted line.
              linestyle: 0L, $       ; The linestyle of the overplotted line.
              psym: 0L, $            ; The symbol of the overplotted line.
              symcolor: "", $        ; The symbol color.
              symthick: 0L, $        ; The thickness of the line symbol.
              symsize: 0L, $         ; The size of the line symbol.
              thick: 0L, $, $        ; The thickness of the overplotted line.
              skip: 0L, $            ; The number of data points to skip in the drawing.
              visible: 0L, $         ; A flag that indicates if the plot should be drawn.
              xrange: FltArr(2), $   ; The X range of the data.
              yrange: FltArr(2) $    ; The Y range of the data.
            }
            
END