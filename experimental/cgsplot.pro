;
; :Description:
;     This method evaluates a keyword structure and creates a new keyword list
;     with the evaluated values.
;
FUNCTION cgsPlot::EvaluateKeywords, keywords, SUCCESS=success

  ; Error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /CANCEL
     success = 0
     void = Error_Message()
     IF N_Elements(keywords) NE 0 THEN RETURN, keywords ELSE RETURN, 0
  ENDIF
  
  Compile_Opt idl2
  
  ; Assume success.
  success = 1
  
  ; Need to have keywords.
  IF N_Elements(keywords) EQ 0 THEN $
      Message, 'A keyword structure must be passed to the method.'

  ; Get the keyword names.
  tags = Tag_Names(keywords)
  
  ; Find the keywords to be evaluated in this keyword structure.
  evalTags = Tag_Names(*self.altps_keywords)
  theseTags = Ptr_New(/Allocate_Heap)
  FOR j=0,N_Elements(evalTags)-1 DO BEGIN
     index = Where(tags EQ evalTags[j], count)
     IF (count EQ 1) THEN BEGIN
        IF N_Elements(*theseTags) NE 0 THEN *theseTags = [*theseTags, index] ELSE *theseTags = [index] 
     ENDIF ELSE IF count GT 1 THEN Message, 'Ambiguous keywords to evaluate.'
  ENDFOR
  
  ; Evaluate the keywords and, if necessary, replace the keyword value
  ; with the PostScript alternative value.
  FOR j=0,N_Elements(tags)-1 DO BEGIN
    index = Where(*theseTags EQ j, count)
    IF count EQ 0 THEN BEGIN
      IF N_Elements(mkeys) EQ 0 THEN BEGIN
        mkeys = Create_Struct(tags[j], keywords.(j)) 
      ENDIF ELSE BEGIN
        mkeys = Create_Struct(mkeys, tags[j], keywords.(j))
      ENDELSE
    ENDIF ELSE BEGIN
    
      ; If this is PostScript, use the alternative keyword value.
      tagValue = (!D.Name EQ 'PS') ? (*self.altps_keywords).(index) : keywords.(j)
      IF N_Elements(mkeys) EQ 0 THEN BEGIN
        mkeys = Create_Struct(tags[j], tagValue)
      ENDIF ELSE BEGIN
        mkeys = Create_Struct(mkeys, tags[j], tagValue)
      ENDELSE
    ENDELSE
    
  ENDFOR
 
  Ptr_Free, theseTags
  RETURN, mkeys
END ;----------------------------------------------------------------------------------------------------------------


PRO cgsPlot::SetData, x, y, NODRAW=nodraw

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    CASE N_Params() OF
    
       0: Message, 'Must pass at least one positional parameter.'
      
       1: BEGIN
       indep = x
       dep = Findgen(N_Elements(x))
       ENDCASE
    
       2: BEGIN
       indep = y
       dep = x
       ENDCASE
    
    ENDCASE
    
    ; Load the values.
    IF Ptr_Valid(self.dep) THEN *self.dep = dep ELSE self.dep = Ptr_New(dep)
    IF Ptr_Valid(self.indep) THEN *self.indep = indep ELSE self.indep = Ptr_New(indep)

    ; Update the display, if you can. If you belong to a window, ask the window
    ; to draw itself.
    IF ~Keyword_Set(nodraw) THEN BEGIN
        IF Obj_Valid(self.winObject) THEN BEGIN
            self.winObject -> ExecuteCommands
        ENDIF ELSE BEGIN
            self -> Draw
        ENDELSE
     ENDIF

END ; ----------------------------------------------------------------------  


PRO cgsPlot::Save, filename, RESOLUTION=resolution

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN filename = 'cgsplot.png'
    
    rootName = FSC_Base_Filename(filename, DIRECTORY=dirName, EXTENSION=ext)
    ext = StrUpCase(ext)
    CASE 1 OF
        (ext EQ 'BMP'): type = 'BMP'
        (ext EQ 'JPEG') || (ext EQ 'JPG'): type = 'JPEG'
        (ext EQ 'PNG'): type = 'PNG'
        (ext EQ 'TIF') || (ext EQ 'TIFF'): type = 'TIFF'
        (ext EQ 'GIF'): type = 'GIF'
        ELSE: Message, 'File extension type not yet implement or known.'
    ENDCASE
    
    ; Need a resolution?
    IF N_Elements(resolution) EQ 0 THEN BEGIN
        IF !D.Window LT 0 THEN BEGIN
            resolution = [640, 512] 
        ENDIF ELSE BEGIN
            resolution = [!D.X_Size, !D.Y_Size]
        ENDELSE
    ENDIF
    
    ; Get the global CGS defaults.
    cgWindow_GetDefs, $
       ; ImageMagick Properties.
       IM_Transparent = d_im_transparent, $              ; Sets the "alpha" keyword on ImageMagick convert command.
       IM_Density = d_im_density, $                      ; Sets the density parameter on ImageMagick convert command.
       IM_Raster = d_im_raster, $                        ; Create raster files via ImageMagick.
       IM_Resize = d_im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
       IM_Options = d_im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
       
       ; PostScript properties.
       PS_Decomposed = d_ps_decomposed, $                ; Set the PS color decomposition state.
       PS_Delete = d_ps_delete, $                        ; Delete PS file when making IM raster.
       PS_Metric = d_ps_metric, $                        ; Select metric measurements in PostScript output.
       PS_Encapsulated = d_ps_encapsulated, $            ; Create Encapsulated PostScript output.    
       PS_FONT = d_ps_font, $                            ; Select the font for PostScript output.
       PS_CHARSIZE = d_ps_charsize, $                    ; Select the character size for PostScript output.
       PS_QUIET = d_ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
       PS_SCALE_FACTOR = d_ps_scale_factor, $            ; Select the scale factor for PostScript output.
       PS_TT_FONT = d_ps_tt_font                         ; Select the true-type font to use for PostScript output.
    
    ; What kind of raster file.
    rasterType = d_im_raster
    CASE rasterType OF
    
        ; Normal raster.
        0: BEGIN
        
           ; Set up a Z-buffer for display.
           thisDevice = !D.Name
           Set_Plot, 'Z'
           SetDecomposedState, 1, CURRENT=currentState
           cgDisplay, resolution[0], resolution[1]
           self -> Draw
           void = cgSnapshot(TYPE=type, FILENAME=Filepath(ROOT_Dir=dirName, rootName), /NODIALOG)
           SetDecomposedState, currentState
           Set_Plot, thisDevice
           END
           
        ; Raster via ImageMagick.
        1: BEGIN
        
           ; Create a PostScript file first.
           thisname = Filepath(ROOT_Dir=dirName, rootName + '.ps')
           PS_Start, FILENAME=thisname, $
                DECOMPOSED=d_ps_decomposed, $
                METRIC=d_ps_metric, $
                SCALE_FACTOR=d_ps_scale_factor, $
                CHARSIZE=d_ps_charsize, $
                FONT=d_ps_font, $
                QUIET=d_ps_quiet, $
                TT_FONT=d_ps_tt_font
           cgDisplay, resolution[0], resolution[1]
           
           ; Draw the graphics.
           self -> Draw
           
           ; Close the file and convert to proper file type.
           CASE type OF
                'BMP':  PS_END, /BMP, DELETE_PS=d_ps_delete, $
                            ALLOW_TRANSPARENT=D_IM_TRANSPARENT, $
                            DENSITY=d__density, RESIZE=d_im_resize, $
                            IM_OPTIONS=d_im_options
                'GIF':  PS_END, /GIF, DELETE_PS=d_ps_delete, $
                            ALLOW_TRANSPARENT=d_im_transparent, $
                            DENSITY=d__density, RESIZE=d_im_resize, $
                            IM_OPTIONS=d_im_options
                'JPEG': PS_END, /JPEG, DELETE_PS=d_ps_delete, $
                            ALLOW_TRANSPARENT=d_im_transparent, $
                            DENSITY=d__density, RESIZE=d_im_resize, $
                            IM_OPTIONS=d_im_options
                'PNG':  PS_END, /PNG,  DELETE_PS=d_ps_delete, $
                            ALLOW_TRANSPARENT=d_im_transparent, $
                            DENSITY=d__density, RESIZE=d_im_resize, $
                            IM_OPTIONS=d_im_options
                'TIFF': PS_END, /TIFF, DELETE_PS=d_ps_delete, $
                            ALLOW_TRANSPARENT=d_im_transparent, $
                            DENSITY=d__density, RESIZE=d_im_resize, $
                            IM_OPTIONS=d_im_options
           ENDCASE
        
           END
    
    ENDCASE
    
END ; ----------------------------------------------------------------------  


PRO cgsPlot::PS, filename, GUI=gui

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN filename = 'cgsplot.ps'
    
    ; Get the global CGS PostScript defaults.
    cgWindow_GetDefs, $
       ; PostScript properties.
       PS_Decomposed = d_ps_decomposed, $                ; Set the PS color decomposition state.
       PS_Delete = d_ps_delete, $                        ; Delete PS file when making IM raster.
       PS_Metric = d_ps_metric, $                        ; Select metric measurements in PostScript output.
       PS_Encapsulated = d_ps_encapsulated, $            ; Create Encapsulated PostScript output.    
       PS_FONT = d_ps_font, $                            ; Select the font for PostScript output.
       PS_CHARSIZE = d_ps_charsize, $                    ; Select the character size for PostScript output.
       PS_QUIET = d_ps_quiet, $                          ; Select the QUIET keyword for PS_Start.
       PS_SCALE_FACTOR = d_ps_scale_factor, $            ; Select the scale factor for PostScript output.
       PS_TT_FONT = d_ps_tt_font                         ; Select the true-type font to use for PostScript output.

    ; Allow the user to configure the PostScript file.
    PS_Start, GUI=Keyword_Set(gui), CANCEL=cancelled, FILENAME=filename, $
                DECOMPOSED=d_ps_decomposed, $
                METRIC=d_ps_metric, $
                SCALE_FACTOR=d_ps_scale_factor, $
                CHARSIZE=d_ps_charsize, $
                FONT=d_ps_font, $
                QUIET=d_ps_quiet, $
                TT_FONT=d_ps_tt_font
    IF cancelled THEN RETURN
    
    ; Execute the graphics commands.
    self -> Draw
    
    ; Clean up.
    PS_End
    
    
END ; ----------------------------------------------------------------------  


PRO cgsPlot::Draw, SUCCESS=success

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        Print, 'Problem executing cgsPlot command:'
        Print, '   ' + !Error_State.MSG
        void = Error_Message()
        success = 0
        RETURN
    ENDIF
    
    ; Assume success
    success = 1
    
    ; If the update flag is turned on, draw the plot. If this object
    ; carries a valid WinObject, then it should ask the window to draw
    ; its commands.
    IF self.update THEN BEGIN
    
        ; Get the keywords.
        keywords = self -> GetKeywords()
        
        ; Do we have alternative parameters?
        IF Ptr_Valid(self.altps_params) THEN BEGIN
        
            altTags = Tag_Names(*self.altps_params)
            IF (Where(altTags EQ 'P1') NE -1) THEN BEGIN
                IF (!D.Name EQ 'PS') $
                    THEN p1 = (*self.altps_params).p1 $
                    ELSE IF Ptr_Valid(self.indep) THEN p1 = *self.indep
            ENDIF ELSE IF Ptr_Valid(self.indep) THEN p1 = *self.indep
            IF (Where(altTags EQ 'P2') NE -1) THEN BEGIN
                IF (!D.Name EQ 'PS') $
                    THEN p2 = (*self.altps_params).p2 $
                    ELSE IF Ptr_Valid(self.dep) THEN p2 = *self.dep
            ENDIF ELSE IF Ptr_Valid(self.dep) THEN p2 = *self.dep
        
        ENDIF ELSE BEGIN
            IF Ptr_Valid(self.indep) THEN p1 = *self.indep
            IF Ptr_Valid(self.dep) THEN p2 = *self.dep
        ENDELSE

        ; Set the current window.
        IF Obj_Valid(self.winObject) THEN self.winObject -> SetWindow

        cgPlot, p1, p2, _EXTRA=keywords
        
    ENDIF
    
END ; ----------------------------------------------------------------------  


FUNCTION cgsPlot::GetKeywords

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, {background:'white', color:'black'}
    ENDIF
    
    keywordStruct = { $
        AXISCOLOR: self.axiscolor, $
        ISOTROPIC: self.isotropic, $
        LINESTYLE=self.linestyle, $
        NSUM: self.nsum, $
        POLAR: self.polar, $
        SYMCOLOR: self.symcolor, $
        XLOG: self.xlog, $
        YLOG: self.ylog, $
        YNOZERO: self.ynozero }
        
    IF N_Elements(*self.aspect) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'aspect', *self.aspect)
    ENDIF
    IF N_Elements(*self.layout) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'layout', *self.layout)
    ENDIF
    IF N_Elements(*self.max_value) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'max_value', *self.max_value)
    ENDIF
    IF N_Elements(*self.min_value) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'min_value', *self.min_value)
    ENDIF
        
    ; Get superclass keywords.
    superKeywords = self -> cgsGraphic::GetKeywords()
    keywordStruct = Create_Struct(keywordStruct, superKeywords)
    
    ; Do you have any alternative PostScript keywords?
    IF Ptr_Valid(self.altps_keywords) THEN BEGIN
        ps_keywords = self -> EvaluateKeywords(keywordStruct, SUCCESS=success)
        IF success THEN keywordStruct = Temporary(ps_keywords)
    ENDIF
        
    RETURN, keywordStruct
    
END ; ----------------------------------------------------------------------  


PRO cgsPlot::GetProperty, $
    ASPECT=aspect, $
    AXISCOLOR=axiscolor, $
    ISOTROPIC=isotropic, $
    LAYOUT=layout, $
    LINESTYLE=linestyle, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    NSUM=nsum, $
    POLAR=polar, $
    SYMCOLOR=symcolor, $
    UPDATE=update, $
    WINOBJECT=winobject, $
    XLOG=xlog, $
    YLOG=ylog, $
    YNOZERO=ynozero, $
    _EXTRA=extraKeywords

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get superclass keywords.
    IF N_Elements(extraKeywords) NE 0 THEN self -> cgsGraphic::SetProperty, _STRICT_REF_EXTRA=extraKeywords

    ; Get the properties of the object.
    IF Arg_Present(aspect) THEN IF N_Elements(*self.aspect) NE 0 THEN aspect = *self.aspect
    IF Arg_Present(axiscolor) THEN axiscolor = self.axiscolor
    IF Arg_Present(isotropic) THEN isotropic = self.isotropic
    IF Arg_Present(layout) THEN IF N_Elements(*self.layout) NE 0 THEN layout = *self.layout
    IF Arg_Present(linestyle) THEN linesytle = self.linestyle
    IF Arg_Present(max_value) THEN IF N_Elements(*self.max_value) NE 0 THEN max_value = *self.max_value
    IF Arg_Present(min_value) THEN IF N_Elements(*self.min_value) NE 0 THEN min_value = *self.min_value
    IF Arg_Present(nsum) THEN nsum = self.nsum
    IF Arg_Present(polar) THEN polar = self.polar
    IF Arg_Present(symcolor) THEN symcolor = self.symcolor
    IF Arg_Present(winObject) THEN winObject = self.winObject
    IF Arg_Present(xlog) THEN xlog = self.xlog
    IF Arg_Present(ylog) THEN ylog = self.ylog
    IF Arg_Present(ynozero) THEN ynozero = self.ynozero
    
END ; ----------------------------------------------------------------------  


PRO cgsPlot::Set, _EXTRA=extraKeywords
    
    self -> SetProperty, _EXTRA=extraKeywords

END ; ----------------------------------------------------------------------  


PRO cgsPlot::SetProperty, $
    AltPS_Keywords=altps_Keywords, $ 
    AltPS_Params=altps_Params, $     
    ASPECT=aspect, $
    AXISCOLOR=axiscolor, $
    ISOTROPIC=isotropic, $
    LAYOUT=layout, $
    LINESTYLE=linestyle, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    NODRAW=nodraw, $
    NSUM=nsum, $
    POLAR=polar, $
    SYMCOLOR=symcolor, $
    UPDATE=update, $
    WINOBJECT=winObject, $
    XLOG=xlog, $
    YLOG=ylog, $
    YNOZERO=ynozero, $
    _EXTRA=extraKeywords
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set superclass keywords.
    IF N_Elements(extraKeywords) NE 0 THEN BEGIN
        self -> cgsGraphic::SetProperty, AltPS_Keywords=altps_Keywords, _STRICT_EXTRA=extraKeywords
    ENDIF
    
    ; Set the properties of the object.
    IF N_Elements(altps_Params) NE 0 THEN self.altps_Params = Ptr_New(altps_Params)
    IF (N_Elements(aspect) NE 0) THEN *self.aspect = aspect 
    IF N_Elements(axiscolor) NE 0 THEN self.axiscolor = axiscolor
    IF N_Elements(isotropic) NE 0 THEN self.isotropic = Keyword_Set(isotropic)
    IF (N_Elements(layout) NE 0) THEN *self.layout = layout
    IF (N_Elements(linestyle) NE 0) THEN *self.linestyle = linestyle
    IF (N_Elements(max_value) NE 0) THEN *self.max_value = max_value
    IF (N_Elements(min_value) NE 0) THEN *self.min_value = min_value
    IF N_Elements(nsum) NE 0 THEN self.nsum = nsum
    IF N_Elements(update) NE 0 THEN self.update = Keyword_Set(update)  
    IF N_Elements(polar) NE 0 THEN self.polar = Keyword_Set(polar)
    IF N_Elements(symcolor) NE 0 THEN self.symcolor = symcolor
    IF N_Elements(winObject) NE 0 THEN self.winObject = winObject
    IF N_Elements(xlog) NE 0 THEN self.xlog = Keyword_Set(xlog)
    IF N_Elements(ylog) NE 0 THEN self.ylog = Keyword_Set(ylog)
    IF N_Elements(ynozero) NE 0 THEN self.ynozero = Keyword_Set(ynozero)
        
    ; Update the display, if you can. If you belong to a window, ask the window
    ; to draw itself.
    IF ~Keyword_Set(nodraw) THEN BEGIN
        IF Obj_Valid(self.winObject) THEN BEGIN
            self.winObject -> ExecuteCommands
        ENDIF ELSE BEGIN
            self -> Draw
        ENDELSE
     ENDIF
END ; ----------------------------------------------------------------------  


PRO cgsPlot::CLEANUP

    Ptr_Free, self.indep
    Ptr_Free, self.dep
    Ptr_Free, self.aspect
    Ptr_Free, self.layout
    Ptr_Free, self.max_value
    Ptr_Free, self.min_value
    Ptr_Free, self.altps_params
    self -> cgsGraphic::CLEANUP
  
END ; ----------------------------------------------------------------------  


FUNCTION cgsPlot::INIT, x, y, $
    ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
    ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
    ASPECT=aspect, $
    AXISCOLOR=axiscolor, $
    ISOTROPIC=isotropic, $
    LAYOUT=layout, $
    LINESTYLE=linestyle, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    NSUM=nsum, $
    POLAR=polar, $
    SYMCOLOR=symcolor, $
    UPDATE=update, $
    XLOG=xlog, $
    YLOG=ylog, $
    YNOZERO=ynozero, $
    _EXTRA=extraKeywords
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF
 
    ; Initialize sub-class objects.
    ok = self -> cgsGraphic::INIT(_EXTRA=extraKeywords, ALTPS_KEYWORDS=altps_keywords) 
    IF ~ok THEN Message, 'Cannot initialize cgsGraphic superclass object.'
        
     ; Sort out which is the dependent and which is independent data.
    CASE N_Params() OF
      
       1: BEGIN
       self.indep = Ptr_New(x)
       self.dep = Ptr_New(Findgen(N_Elements(x)))
       ENDCASE
    
       2: BEGIN
       self.indep = Ptr_New(y)
       self.dep = Ptr_New(x)
       ENDCASE
    
    ENDCASE
    
    ; Set properties.
    IF N_Elements(update) EQ 0 THEN update = 1
    IF N_Elements(axisColor) EQ 0 AND N_Elements(axescolor) NE 0 THEN axiscolor = axescolor
    IF N_Elements(axiscolor) EQ 0 THEN axiscolor = 'black'
    IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
    IF N_Elements(symcolor) EQ 0 THEN symcolor = self.color
    
    ; Initialize all keyword pointers.
    self.aspect = Ptr_New(/Allocate_Heap)
    self.layout = Ptr_New(/Allocate_Heap)
    self.max_value = Ptr_New(/Allocate_Heap)
    self.min_value = Ptr_New(/Allocate_Heap)
    
    ; Set the properties of the object.
    self -> SetProperty, $
        ALTPS_PARAMS=altps_params, $
        ASPECT=aspect, $
        AXISCOLOR=axiscolor, $
        ISOTROPIC=isotropic, $
        LAYOUT=layout, $
        LINESTYLE=linestyle, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        NSUM=nsum, $
        POLAR=polar, $
        SYMCOLOR=symcolor, $
        UPDATE=update, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        _EXTRA=extraKeywords
        
    RETURN, 1
    
END ; ----------------------------------------------------------------------  

PRO cgsPlot__DEFINE, class

         class = { CGSPLOT, $
                   INHERITS cgsGraphic, $
                   altps_params: Ptr_New(), $
                   indep: Ptr_New(), $
                   dep: Ptr_New(), $
                   aspect: Ptr_New(), $
                   axiscolor: "", $
                   isotropic: 0B, $
                   layout: Ptr_New(), $
                   linestyle: 0L, $
                   max_value: Ptr_New(), $
                   min_Value: Ptr_New(), $
                   nsum: 0L, $
                   update: 0B, $
                   polar: 0B, $
                   symcolor: "", $
                   winObject: Obj_New(), $
                   xlog: 0B, $
                   ylog: 0B, $
                   ynozero: 0B }
                   
END ; ----------------------------------------------------------------------


FUNCTION cgsPlot, x, y, $
    ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
    ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
    ASPECT=aspect, $
    AXISCOLOR=axiscolor, $
    CURRENT=current, $              ; If set, add the command to the current graphics window.
    ISOTROPIC=isotropic, $
    LAYOUT=layout, $
    LINESTYLE=linestyle, $
    MAX_VALUE=max_value, $
    MIN_VALUE=min_value, $
    NOWINDOW=nowindow, $
    NSUM=nsum, $
    POLAR=polar, $
    SYMCOLOR=symcolor, $
    UPDATE=update, $
    XLOG=xlog, $
    YLOG=ylog, $
    YNOZERO=ynozero, $
    _EXTRA=extraKeywords 
    
     ; Sort out which is the dependent and which is independent data.
     ; This is required because otherwise if I get just a single parameter
     ; it is passed to the object, but the second parameter is also passed,
     ; but as an undefined variable parameter.
    CASE N_Params() OF
      
       1: BEGIN
       indep = x
       dep = Findgen(N_Elements(x))
       ENDCASE
    
       2: BEGIN
       indep = y
       dep = x
       ENDCASE
    
    ENDCASE

        
    plotObject = Obj_New('cgsPlot', indep, dep, $
        ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
        ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
        ASPECT=aspect, $
        AXISCOLOR=axiscolor, $
        ISOTROPIC=isotropic, $
        LAYOUT=layout, $
        LINESTYLE=linestyle, $
        MAX_VALUE=max_value, $
        MIN_VALUE=min_value, $
        NODRAW=1, $
        NSUM=nsum, $
        POLAR=polar, $
        SYMCOLOR=symcolor, $
        UPDATE=update, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        _EXTRA=extraKeywords)
        
    ; Out of here if you failed to get a valid object.
    IF ~Obj_Valid(plotObject) THEN RETURN, Obj_New()
        
    plotObject -> GetProperty, Layout=t
    
    ; If the CURRENT keyword is set, check to see if there is a current
    ; graphics window you can add this to. Otherwise, create a new window.
    ; If NOWINDOW is set, skip this part and just draw yourself.
    IF ~Keyword_Set(nowindow) && Obj_Valid(plotObject) THEN BEGIN
        winObject = cgsQuery(/CURRENT, COUNT=count)
        IF count EQ 0 THEN BEGIN
            winObject = cgsWindow() 
        ENDIF ELSE BEGIN
            IF ~Keyword_Set(current) THEN winObject = cgsWindow() 
        ENDELSE
        plotObject -> SetProperty, WinObject=winObject
        winObject -> AddCmd, plotObject
    ENDIF ELSE BEGIN
        IF Obj_Valid(plotObject) THEN plotObject -> Draw
    ENDELSE

    ; Return the object, if you have a valid one.    
    IF Obj_Valid(plotObject) THEN BEGIN
        RETURN, plotObject
    ENDIF ELSE BEGIN
        RETURN, Obj_New()
    ENDELSE
    
END