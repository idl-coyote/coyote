;
; :Description:
;     This method evaluates a keyword structure and creates a new keyword list
;     with the evaluated values.
;
FUNCTION cgsContour::EvaluateKeywords, keywords, SUCCESS=success

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


PRO cgsContour::SetData, x, y, NODRAW=nodraw

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


PRO cgsContour::Save, filename, RESOLUTION=resolution

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN filename = 'cgscontour.png'
    
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


PRO cgsContour::PS, filename, GUI=gui

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


PRO cgsContour::Draw, SUCCESS=success

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        Print, 'Problem executing cgsContour command:'
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
                    ELSE IF Ptr_Valid(self.array) THEN p1 = *self.array
            ENDIF ELSE IF Ptr_Valid(self.array) THEN p1 = *self.array
            IF (Where(altTags EQ 'P2') NE -1) THEN BEGIN
                IF (!D.Name EQ 'PS') $
                    THEN p2 = (*self.altps_params).p2 $
                    ELSE IF Ptr_Valid(self.x) THEN p2 = *self.x
            ENDIF ELSE IF Ptr_Valid(self.x) THEN p2 = *self.x
            IF (Where(altTags EQ 'P3') NE -1) THEN BEGIN
                IF (!D.Name EQ 'PS') $
                    THEN p3 = (*self.altps_params).p3 $
                    ELSE IF Ptr_Valid(self.y) THEN p3 = *self.y
            ENDIF ELSE IF Ptr_Valid(self.y) THEN p3 = *self.y
        
        ENDIF ELSE BEGIN
            IF Ptr_Valid(self.array) THEN p1 = *self.array
            IF Ptr_Valid(self.x) THEN p2 = *self.x
            IF Ptr_Valid(self.y) THEN p3 = *self.y
        ENDELSE

        ; Set the current window.
        IF Obj_Valid(self.winObject) THEN self.winObject -> SetWindow

        cgContour, p1, p2, p3, _EXTRA=keywords
        
    ENDIF
    
END ; ----------------------------------------------------------------------  


FUNCTION cgsContour::GetKeywords

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, {background:'white', color:'black'}
    ENDIF
    
   keywordStruct = { $
        AXISCOLOR: self.axiscolor, $
        CELL_FILL: self.cell_fill, $
        FILL: self.fill, $
        IRREGULAR: self.irregular, $
        LABEL: self.label, $
        ONIMAGE: self.onImage, $
        OVERPLOT: self.overplot, $
        TRADITIONAL: self.traditional, $
        XLOG: self.xlog, $
        YLOG: self.ylog, $
        YNOZERO: self.ynozero }
        
    IF N_Elements(*self.c_colors) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'c_colors', *self.c_colors)
    ENDIF
    IF N_Elements(*self.c_labels) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'c_labels', *self.c_labels)
    ENDIF
    IF N_Elements(*self.c_charsize) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'c_charsize', *self.c_charsize)
    ENDIF
    IF N_Elements(*self.layout) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'layout', *self.layout)
    ENDIF
    IF N_Elements(*self.levels) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'levels', *self.levels)
    ENDIF
    IF N_Elements(*self.missingvalue) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'missingvalue', *self.missingvalue)
    ENDIF
    IF N_Elements(*self.nlevels) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'nlevels', *self.nlevels)
    ENDIF
    IF N_Elements(*self.resolution) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'resolution', *self.resolution)
    ENDIF
    IF N_Elements(*self.palette) NE 0 THEN BEGIN
       keywordStruct = Create_Struct(keywordStruct, 'palette', *self.palette)
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


PRO cgsContour::GetProperty, $
        AXISCOLOR=axisColor, $
        C_CHARSIZE=c_charsize, $
        C_COLORS=c_colors, $
        C_LABELS=c_labels, $
        CELL_FILL=cell_fill, $
        FILL=fill, $
        IRREGULAR=irregular, $
        LABEL=label, $
        LAYOUT=layout, $
        LEVELS=levels, $
        MISSINGVALUE=missingvalue, $
        NLEVELS=nlevels, $
        NODRAW=nodraw, $
        ONIMAGE=onImage, $
        OVERPLOT=overplot, $
        PALETTE=palette, $
        RESOLUTION=resolution, $
        TRADITIONAL=traditional, $
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
    IF Arg_Present(axiscolor) THEN axiscolor = self.axiscolor
    IF Arg_Present(c_colors) THEN IF N_Elements(*self.c_colors) NE 0 THEN c_colors = *self.c_colors
    IF Arg_Present(c_labels) THEN IF N_Elements(*self.c_labels) NE 0 THEN c_labels = *self.c_labels
    IF Arg_Present(c_charsize) THEN IF N_Elements(*self.c_charsize) NE 0 THEN c_charsize = *self.c_charsize
    IF Arg_Present(cell_fill) THEN cell_fill = self.cell_fill
    IF Arg_Present(fill) THEN fill = self.fill
    IF Arg_Present(irregular) THEN irregular = self.irregular
    IF Arg_Present(label) THEN label = self.label
    IF Arg_Present(layout) THEN IF N_Elements(*self.layout) NE 0 THEN layout = *self.layout
    IF Arg_Present(levels) THEN IF N_Elements(*self.levels) NE 0 THEN levels = *self.levels
    IF Arg_Present(missingvalue) THEN IF N_Elements(*self.missingvalue) NE 0 THEN missingvalue = *self.missingvalue
    IF Arg_Present(nlevels) THEN nlevels = self.nlevels
    IF Arg_Present(onimage) THEN onimage = self.onimage
    IF Arg_Present(overplot) THEN overplot = self.overplot
    IF Arg_Present(palette) THEN IF N_Elements(*self.palette) NE 0 THEN palette = *self.palette
    IF Arg_Present(resolution) THEN resolution = self.resolution
    IF Arg_Present(traditional) THEN traditional = self.traditional
    IF Arg_Present(winObject) THEN winObject = self.winObject
    IF Arg_Present(xlog) THEN xlog = self.xlog
    IF Arg_Present(ylog) THEN ylog = self.ylog
    IF Arg_Present(ynozero) THEN ynozero = self.ynozero
    
END ; ----------------------------------------------------------------------  


PRO cgsContour::Set, _EXTRA=extraKeywords
    
    self -> SetProperty, _EXTRA=extraKeywords

END ; ----------------------------------------------------------------------  


PRO cgsContour::SetProperty, $
        AltPS_Keywords=altps_Keywords, $ 
        ALTPS_PARAMS=altps_params, $
        ASPECT=aspect, $
        AXISCOLOR=axisColor, $
        C_CHARSIZE=c_charsize, $
        C_COLORS=c_colors, $
        C_LABELS=c_labels, $
        CELL_FILL=cell_fill, $
        FILL=fill, $
        IRREGULAR=irregular, $
        LABEL=label, $
        LAYOUT=layout, $
        LEVELS=levels, $
        NLEVELS=nlevels, $
        NODRAW=nodraw, $
        MISSINGVALUE=missingvalue, $
        ONIMAGE=onImage, $
        OVERPLOT=overplot, $
        PALETTE=palette, $
        RESOLUTION=resolution, $
        TRADITIONAL=traditional, $
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
    
    ; Set superclass keywords.
    IF N_Elements(extraKeywords) NE 0 THEN BEGIN
        self -> cgsGraphic::SetProperty, AltPS_Keywords=altps_Keywords, _STRICT_EXTRA=extraKeywords
    ENDIF
    
    ; Set the properties of the object.
    IF N_Elements(altps_Params) NE 0 THEN self.altps_Params = Ptr_New(altps_Params)
    IF N_Elements(aspect) NE 0 THEN self.position = Aspect(aspect) 
    IF N_Elements(axiscolor) NE 0 THEN self.axiscolor = axiscolor
    IF N_Elements(c_charsize) NE 0 THEN *self.c_charsize = c_charsize
    IF N_Elements(c_colors) NE 0 THEN *self.c_colors = c_colors 
    IF N_Elements(c_labels) NE 0 THEN *self.c_labels = c_labels 
    IF N_Elements(cell_fill) NE 0 THEN self.cell_fill = Keyword_Set(cell_fill)
    IF N_Elements(fill) NE 0 THEN self.fill = Keyword_Set(fill)
    IF N_Elements(irregular) NE 0 THEN self.irregular = Keyword_Set(irregular)
    IF N_Elements(label) NE 0 THEN self.label = label
    IF (N_Elements(layout) NE 0) THEN *self.layout = layout 
    IF N_Elements(levels) NE 0 THEN *self.levels = levels 
    IF N_Elements(nlevels) NE 0 THEN *self.nlevels = nlevels
    IF N_Elements(missingvalue) NE 0 THEN *self.missingvalue = missingvalue 
    IF N_Elements(onimage) NE 0 THEN self.onimage = Keyword_Set(onimage)
    IF N_Elements(overplot) NE 0 THEN self.overplot = Keyword_Set(overplot)
    IF (N_Elements(palette) NE 0) THEN *self.palette = palette
    IF (N_Elements(resolution) NE 0) THEN *self.resolution = resolution
    IF N_Elements(update) NE 0 THEN self.update = Keyword_Set(update)  
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


PRO cgsContour::CLEANUP

    Ptr_Free, self.array
    Ptr_Free, self.x
    Ptr_Free, self.y
    Ptr_Free, self.aspect
    Ptr_Free, self.c_colors
    Ptr_Free, self.c_labels
    Ptr_Free, self.c_charsize
    Ptr_Free, self.layout
    Ptr_Free, self.levels
    Ptr_Free, self.missingvalue
    Ptr_Free, self.nlevels
    Ptr_Free, self.resolution
    Ptr_Free, self.palette
    Ptr_Free, self.altps_params
    self -> cgsGraphic::CLEANUP
  
END ; ----------------------------------------------------------------------  


FUNCTION cgsContour::INIT, array, x, y, $
    ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
    ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
    ASPECT=aspect, $
    AXISCOLOR=axisColor, $
    C_CHARSIZE=c_charsize, $
    C_COLORS=c_colors, $
    C_LABELS=c_labels, $
    CELL_FILL=cell_fill, $
    FILL=fill, $
    IRREGULAR=irregular, $
    LABEL=label, $
    LAYOUT=layout, $
    LEVELS=levels, $
    NLEVELS=nlevels, $
    NOWINDOW=nowindow, $
    MISSINGVALUE=missingvalue, $
    ONIMAGE=onImage, $
    OVERPLOT=overplot, $
    PALETTE=palette, $
    RESOLUTION=resolution, $
    TRADITIONAL=traditional, $
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
        
    ; Check parameters.
    IF N_Elements(array) EQ 0 THEN array = cgDemoData(2)
    ndims = Size(array, /N_DIMENSIONS)
    IF ndims NE 2 THEN Message, 'Input data must be a 2D array.'
    s = Size(array, /DIMENSIONS)
    IF N_Elements(x) EQ 0 THEN x = Indgen(s[0])
    IF N_Elements(y) EQ 0 THEN y = Indgen(s[1])
    
    ; Set properties.
    IF N_Elements(update) EQ 0 THEN update = 1
    IF N_Elements(axiscolor) EQ 0 THEN axiscolor = 'opposite'


    ; Initialize all keyword pointers.
    self.c_colors = Ptr_New(/Allocate_Heap)
    self.c_labels = Ptr_New(/Allocate_Heap)
    self.c_charsize = Ptr_New(/Allocate_Heap)
    self.layout = Ptr_New(/Allocate_Heap)
    self.levels = Ptr_New(/Allocate_Heap)
    self.nlevels = Ptr_New(/Allocate_Heap)
    self.resolution = Ptr_New(/Allocate_Heap)
    self.missingvalue = Ptr_New(/Allocate_Heap)
    self.palette = Ptr_New(/Allocate_Heap)
    
    
    self -> SetProperty, $
        ALTPS_PARAMS=altps_params, $
        ASPECT=aspect, $
        AXISCOLOR=axisColor, $
        C_CHARSIZE=c_charsize, $
        C_COLORS=c_colors, $
        C_LABELS=c_labels, $
        CELL_FILL=cell_fill, $
        FILL=fill, $
        IRREGULAR=irregular, $
        LABEL=label, $
        LAYOUT=layout, $
        LEVELS=levels, $
        NLEVELS=nlevels, $
        NOWINDOW=nowindow, $
        MISSINGVALUE=missingvalue, $
        ONIMAGE=onImage, $
        OVERPLOT=overplot, $
        PALETTE=palette, $
        RESOLUTION=resolution, $
        TRADITIONAL=traditional, $
        UPDATE=update, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        _EXTRA=extraKeywords
        
    RETURN, 1
    
END ; ----------------------------------------------------------------------  

PRO cgsContour__DEFINE, class

         class = { CGSCONTOUR, $
                   INHERITS cgsGraphic, $
                   altps_params: Ptr_New(), $
                   array: Ptr_New(), $
                   x: Ptr_New(), $
                   y: Ptr_New(), $
                   axiscolor: "", $
                   c_charsize: Ptr_New(), $
                   c_colors: Ptr_New(), $
                   c_labels: Ptr_New(), $
                   cell_fill: 0B, $
                   fill: 0B, $
                   irregular: 0B, $
                   label: 0, $
                   layout: Ptr_New(), $
                   levels: Ptr_New(), $
                   missingvalue: Ptr_New(), $
                   nlevels: Ptr_New(), $
                   onimage: 0B, $
                   overplot: 0B, $
                   palette: Ptr_New(), $
                   resolution: Ptr_New(), $
                   traditional: 0B, $
                   update: 0B, $
                   winObject: Obj_New(), $
                   xlog: 0B, $
                   ylog: 0B, $
                   ynozero: 0B }
                   
END ; ----------------------------------------------------------------------


FUNCTION cgsContour, array, x, y, $
    ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
    ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
    ASPECT=aspect, $
    AXISCOLOR=axisColor, $
    C_CHARSIZE=c_charsize, $
    C_COLORS=c_colors, $
    C_LABELS=c_labels, $
    CELL_FILL=cell_fill, $
    FILL=fill, $
    IRREGULAR=irregular, $
    LABEL=label, $
    LAYOUT=layout, $
    LEVELS=levels, $
    NLEVELS=nlevels, $
    NOWINDOW=nowindow, $
    MISSINGVALUE=missingvalue, $
    ONIMAGE=onImage, $
    OVERPLOT=overplot, $
    PALETTE=palette, $
    RESOLUTION=resolution, $
    TRADITIONAL=traditional, $
    UPDATE=update, $
    XLOG=xlog, $
    YLOG=ylog, $
    YNOZERO=ynozero, $
    _EXTRA=extraKeywords 
        
    contourObject = Obj_New('cgsContour', array, x, y, $
        ALTPS_KEYWORDS=altps_Keywords, $ ; A structure of PostScript alternative keywords and values.
        ALTPS_PARAMS=altps_Params, $     ; A structure of PostScript alternative parameters and values. 
        ASPECT=aspect, $
        AXISCOLOR=axisColor, $
        C_CHARSIZE=c_charsize, $
        C_COLORS=c_colors, $
        C_LABELS=c_labels, $
        CELL_FILL=cell_fill, $
        FILL=fill, $
        IRREGULAR=irregular, $
        LABEL=label, $
        LAYOUT=layout, $
        LEVELS=levels, $
        NLEVELS=nlevels, $
        NOWINDOW=nowindow, $
        MISSINGVALUE=missingvalue, $
        ONIMAGE=onImage, $
        OVERPLOT=overplot, $
        PALETTE=palette, $
        RESOLUTION=resolution, $
        TRADITIONAL=traditional, $
        UPDATE=update, $
        XLOG=xlog, $
        YLOG=ylog, $
        YNOZERO=ynozero, $
        _EXTRA=extraKeywords)
        
    ; Out of here is you don't get a valid contour object.
    IF Obj_Valid(contourObject) EQ 0 THEN RETURN, Obj_New()
        
    contourObject -> GetProperty, Layout=t
    
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
        contourObject -> SetProperty, WinObject=winObject
        winObject -> AddCmd, contourObject
    ENDIF ELSE BEGIN
        IF Obj_Valid(contourObject) THEN contourObject -> Draw
    ENDELSE

    ; Return the object, if it is valid.    
    IF Obj_Valid(contourObject) THEN BEGIN
        RETURN, contourObject
    ENDIF ELSE BEGIN
        RETURN, Obj_New()
    ENDELSE
    
END