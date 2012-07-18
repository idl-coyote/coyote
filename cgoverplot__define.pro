PRO cgOverPlot::Draw, SKIP=skip

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; If you aren't skipping, draw all the data.
    IF N_Elements(skip) EQ 0 THEN BEGIN
        asymbol = cgSymCat(self.psym, THICK=self.symThick, COLOR=self.symcolor)
        OPlot, *self.indep, *self.dep, COLOR=cgColor(self.color), LINESTYLE=self.linestyle, $
            PSYM=asymbol, SYMSIZE=self.symsize, THICK=self.thick
    ENDIF ELSE BEGIN
        count = N_Elements(*self.dep)-1
        asymbol = cgSymCat(self.psym, THICK=self.symThick, COLOR=self.symcolor)
        OPlot, (*self.indep)[0:count:skip], (*self.dep)[0:count:skip], COLOR=cgColor(self.color), LINESTYLE=self.linestyle, $
            PSYM=asymbol, SYMSIZE=self.symsize, THICK=self.thick
    ENDELSE
    
END

PRO cgOverPlot::GetProperty, $
    COLOR=color, $
    DEP=dep, $
    DRAW=draw, $
    INDEP=indep, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick, $
    XRANGE=xrange, $
    YRANGE=yrange

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
    IF Arg_Present(symcolor) THEN symcolor = self.symcolor
    IF Arg_Present(symsize) THEN symsize = self.symsize
    IF Arg_Present(symthick) THEN symthick = self.symthick
    IF Arg_Present(thick) THEN thick = self.thick
    IF Arg_Present(xrange) THEN xrange = self.xrange
    IF Arg_Present(yrange) THEN yrange = self.yrange
    
END
    
PRO cgOverPlot::SetProperty, $
    COLOR=color, $
    DEP=dep, $
    DRAW=draw, $
    INDEP=indep, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick

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
    IF N_Elements(symcolor) NE 0 THEN self.symcolor = symcolor
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(symthick) NE 0 THEN self.symthick = symthick
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    
    ; Need to draw?
    IF Keyword_Set(draw) THEN self -> Draw
     
END


PRO cgOverPlot::Cleanup

    Ptr_Free, self.dep
    Ptr_Free, self.indep
    
END

FUNCTION cgOverPlot::Init, x, y, $
    ADDCMD=addcmd, $
    COLOR=color, $
    DRAW=draw, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick
    
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
    color = cgDefaultColor(color, DEFAULT="opposite", MODE=currentState)
    symcolor = cgDefaultColor(symcolor, DEFAULT=color, MODE=currentState)

    ; Other parameter defaults.
    IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
    IF N_Elements(psym) EQ 0 THEN psym = 0
    IF N_Elements(thick) EQ 0 THEN thick = 1.0
    IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
    IF N_Elements(symthick) EQ 0 THEN symthick = 1.0
    
    ; Load the object.
    self.dep = Ptr_New(dep)
    self.indep = Ptr_New(indep)
    self.color = color
    self.linestyle = linestyle    
    self.psym = psym
    self.symcolor = symcolor
    self.symsize = symsize
    self.symthick = symthick
    self.xrange = [Min(indep), Max(indep)]
    self.yrange = [Min(dep), Max(dep)]
    
    IF Keyword_Set(draw) THEN self -> Draw
    IF Keyword_Set(addcmd) THEN cgControl, Execute=1
    
    RETURN, 1
 
END

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
              xrange: FltArr(2), $   ; The X range of the data.
              yrange: FltArr(2) $    ; The Y range of the data.
            }
            
END