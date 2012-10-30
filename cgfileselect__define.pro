;+
; This method returns the object properties.
;-
PRO cgFileSelect::GetProperty, $
   FILENAME=filename, $
   LASTDIR=lastdir, $
   PARENT=parent, $
   TLB=tlb, $
   UNAME=uname, $
   UVALUE=uvalue
   
   IF Arg_Present(filename) THEN filename = self.filename
   IF Arg_Present(lastdir) THEN lastdir = self.lastdir
   IF Arg_Present(parent) THEN parent = self.parent
   IF Arg_Present(tlb) THEN tlb = self.tlb
   IF Arg_Present(uname) THEN uname = self.uname
   IF Arg_Present(uvalue) THEN IF Ptr_Valid(self.uvalue) THEN uvalue = *self.uvalue

END

;+
; This method sets the object properties.
;-
PRO cgFileSelect::SetProperty, $
   FILENAME=filename, $
   LASTDIR=lastdir, $
   UVALUE=uvalue
   
   IF N_Elements(filename) NE 0 THEN BEGIN
        self.filename = filename
        Widget_Control, self.textID, Set_Value=filename
   ENDIF
   IF N_Elements(lastdir) NE 0 THEN self.lastdir = lastdir
   IF N_Elements(uvalue) NE 0 THEN IF Ptr_Valid(self.uvalue) $
       THEN *self.uvalue = uvalue $
       ELSE self.uvalue = Ptr_New(uvalue)
       
END

;+
; The class clean-up method.
;-
PRO cgFileSelect::CLEANUP

    Ptr_Free, self.uvalue

END


;+
; The class initialization method.
; 
; :Params:
;     parent: in, optional
;          The identifier of the parent widget. If not used, a top-level base widget
;          will be created to hold the contents of the program.
;          
; :Keywords:
;     
;-
FUNCTION cgFileSelect::INIT, parent, $
   FILENAME=filename, $
   FRAME=frame, $
   LABELSIZE=labelsize, $
   NAMESIZE=namesize, $
   TITLE=title, $
   UNAME=uname, $
   UVALUE=uvalue
    
   Compile_Opt idl2
    
   ; Error handling for the program module.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       void = Error_Message()
       RETURN, 0
   ENDIF
    
   ; Need a parent?
   IF N_Elements(parent) EQ 0 THEN BEGIN
        parent = Widget_Base()
        createdParent = 1
   ENDIF ELSE createdParent = 0
   
   ; Default keyword values.
   SetDefaultValue, title, 'Filename: '
   SetDefaultValue, filename, ""
   SetDefaultValue, labelsize, !D.X_CH_SIZE * StrLen(title)
   SetDefaultValue, namesize, 0
   SetDefaultValue, uname, 'cgFileSelect'
   
   IF filename EQ "" THEN BEGIN
      CD, CURRENT=lastDir 
   ENDIF ELSE BEGIN
      root = cgRootName(filename, DIRECTORY=lastDir)
   ENDELSE
   
   ; Create the widgets for the program.
   tlb = Widget_Base(parent, ROW=1, UVALUE=self, EVENT_PRO='cgFileSelect_Events', FRAME=frame)
   labelID = Widget_Label(tlb, Value=title, SCR_XSIZE=labelsize)
   IF namesize NE 0 THEN BEGIN
      textID = Widget_Label(tlb, Value=filename, /SUNKEN_FRAME, SCR_XSIZE=namesize)
   ENDIF ELSE BEGIN
      textID = Widget_Label(tlb, Value=filename, /SUNKEN_FRAME, DYNAMIC_RESIZE=1)
   ENDELSE
   button = Widget_Button(tlb, Value='Browse', UVALUE=self)
   
   ; Need to realize the widgets?
   IF createdParent THEN BEGIN
       cgCenterTLB, parent
       Widget_Control, parent, /REALIZE
   ENDIF
   
   ; Populate the object.
   self.filename = filename
   self.lastDir = lastDir
   self.parent = parent
   self.tlb = tlb
   self.textID = textID
   self.labelID = labelID
   self.uname = uname
   IF N_Elements(uvalue) NE 0 THEN self.uvalue = Ptr_New(uvalue)   
    
    
   ; Need to run the program?
   IF createdParent THEN XManager, 'cgfileselect', parent, /No_Block
   
   RETURN, 1
END


;+
; The class event handler. All program widget events come here to be processed.
;
; :Params:
;     event: in, required
;         The event structure from the widget causing the event.
;+
PRO cgFileSelect_Events, event

    Widget_Control, event.id, GET_UVALUE=self
    self -> GetProperty, LASTDIR=lastDir, FILENAME=filename, PARENT=parent, TLB=tlb
    filename = Dialog_Pickfile(PATH=lastDir, /WRITE, Title='Select File...', $
       FILE=cgRootName(filename))
    IF filename NE "" THEN BEGIN
       rootname = cgRootName(filename, DIRECTORY=lastDir)
       self -> SetProperty, FILENAME=filename, LASTDIR=lastDir
    ENDIF
        
END


;+
; The class definition module for the cgFileSelect object.
; 
; :Params:
;     class: out, optional, type=structure
;         The structure definition for the cgFileSelect object class.
;-
PRO cgFileSelect__Define, class

    class = { cgFileSelect, $
              filename: "", $
              labelID: 0L, $
              lastDir: "", $
              parent: 0L, $
              tlb: 0L, $
              textID: 0L, $
              uname: "", $
              uvalue: Ptr_New() $
            }

END