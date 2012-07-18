PRO cgLegendItem::Draw

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
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
    cgText, x1+(2.0*!D.X_CH_SIZE/!D.X_Size), y-(0.5*!D.Y_CH_SIZE/!D.Y_Size),$
        /NORMAL, ALIGNMENT=0.0, self.title, COLOR=self.tcolor, $
        FONT=*self.font, CHARSIZE=self.charsize
END


PRO cgLegendItem::GetProperty, $
   CHARSIZE=charsize, $
   COLOR=color, $
   FONT=font, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   THICK=thick, $
   TCOLOR=tcolor, $
   TITLE=title

    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF

    IF Arg_Present(charsize) THEN charsize = self.charsize
    IF Arg_Present(color) THEN color = self.color
    IF Arg_Present(font) THEN font = *self.font
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
    IF Arg_Present(visible) THEN visible = self.visible

END


PRO cgLegendItem::SetProperty, $
   CHARSIZE=charsize, $
   COLOR=color, $
   FONT=font, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   THICK=thick, $
   TCOLOR=tcolor, $
   TITLE=title, $
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
    IF N_Elements(font) NE 0 THEN *self.font = font
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
    IF N_Elements(visible) NE 0 THEN self.visible = visible
    
END


PRO cgLegendItem::CLEANUP
   Ptr_Free, self.font
END


FUNCTION cgLegendItem::INIT, $
   CHARSIZE=charsize, $
   COLOR=color, $
   FONT=font, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   LOCATION=location, $
   PSYM=psym, $
   SYMCOLOR=symcolor, $
   SYMSIZE=symsize, $
   SYMTHICK=symthick, $
   THICK=thick, $
   TCOLOR=tcolor, $
   TITLE=title, $
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
    IF N_Elements(font) NE 0 THEN self.font = Ptr_New(font) ELSE self.font = Ptr_New(/ALLOCATE_HEAP)
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


PRO cgLegendItem__Define, class

    Compile_Opt idl2

    class = { CGLEGENDITEM, $
              INHERITS IDL_Object, $
              charsize: 0.0, $
              color: "", $
              font: Ptr_New(), $
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