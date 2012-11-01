; docformat = 'rst'
;
; NAME:
;   cgProgressBar
;
; PURPOSE:
;   This program is used to draw a progress bar.
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
; This program is used to draw a progress bar on the display.
; 
; .. image:: cgprogressbar.png
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    To display a progress bar in a program with a FOR loop::
;       
;          cgProgressBar = Obj_New("CGPROGRESSBAR", /Cancel)
;          cgProgressBar -> Start
;          FOR j=0,9 DO BEGIN
;             IF cgProgressBar -> CheckCancel() THEN BEGIN
;                ok = Dialog_Message('The user cancelled operation.')
;                RETURN
;             ENDIF
;             Wait, 0.5  ; Would probably be doing something ELSE here!
;             cgProgressBar -> Update, (j+1)*10
;          ENDFOR
;          cgProgressBar -> Destroy
;    
;    An additional example can be found my compiling the program and typing
;    this command::
;    
;       IDL> ProgressBar_Example
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
;    Change History::
;       Written by:  David W. Fanning, 27 September 2012. Inspired by Ronn Kling's program krProgressBar.
;         
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
;
;+
; Checks the cancel button to see if it has been selected.
; 
; :Returns:
;    Returns a 1 if the user clicked the Cancel button and a 0 otherwise.
; 
;+
; The initialization routine for the cgPROGRESSBAR object class.
;
; :Keywords:
;    cancelbutton: in, optional, type=boolean, default=0
;       Set this keyword if you wish to have a CANCEL button on the 
;       progress bar. If a CANCEL button is present, the user is reponsible
;       for checking if the user has canceled while the progress bar is running.
;    group_leader: in, optional, type=long
;       The identifier of a group leader widget for the progress bar. If the group
;       leader dies, the progress bar will be destroyed. 
;    nocancel: in, optional, type=boolean, default=1
;        A depreciated keyword, added for compatibility with the old Progressbar code.
;        If set, sets the CancelButton keyword to 0.
;    percent: in, optional, type=float, default=0.0
;        The initial percentage completion of the progress bar when it first
;        appears on the display. Used only if the `Start` keyword is also set.
;    text: in, optional, type=string
;        The text that appears in a label widget above the progress bar. If not
;        supplied, no label widget is created.
;    title: in, optional, type=string
;        The text that appears as the window title of the progress bar. By default, 
;        "Operation in Progress...".
;    xoffset: in, optional, type=int
;        The X offset, in pixels, from the top-left corner of the display. If
;        not provided, the progress bar is centered in the window.
;    xsize: in, optional, type=int, default=250
;        The X size, in pixels, of the progress bar.
;    yoffset: in, optional, type=int
;        The Y offset, in pixels, from the top-left corner of the display. If
;        not provided, the progress bar is centered in the window.
;    ysize: in, optional, type=int, default=25
;        The Y size, in pixels, of the progress bar.       
;-
FUNCTION cgProgressBar::INIT, $
    CANCELBUTTON=cancelbutton, $ ; Set this keyword if you desire a CANCEL button on progress bar.
    GROUP_LEADER=group_leader, $ ; The identifier of the group leader widget.
    NOCANCEL=nocancel, $         ; Depreciated keyword.
    PERCENT=percent, $           ; Initial percent of the progress bar. (Only recognized if START used.)
    START=start, $               ; Set this keyword if you wish to call the START method from INIT.
    TEXT=text, $                 ; The message text to be written over the progress bar.
    TITLE=title, $               ; The title of the top-level base widget.
    XOFFSET=xoffset, $           ; The X offset of the progress bar.
    XSIZE=xsize, $               ; The X size of the progress bar.
    YOFFSET=yoffset, $           ; The Y offset of the progress bar.
    YSIZE=ysize                  ; The Y size of the progress bar.

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Check keywords.
    IF N_Elements(nocancel) NE 0 THEN cancelbutton = 1 - Keyword_Set(nocancel)
    SetDefaultValue, cancelbutton, 0, /Boolean
    SetDefaultValue, percent, 0.0
    SetDefaultValue, start, 0, /Boolean
    SetDefaultValue, text, ""
    SetDefaultValue, title, "Operation in Progress..."
    SetDefaultValue, xsize, 250
    SetDefaultValue, ysize, 25
    
    ; Progress bar should be centered unless told otherwise.
    Device, Get_Screen_Size=screenSize
    IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
    IF N_Elements(xoffset) EQ 0 THEN xoffset = (screenSize[0]/2 - xsize/2)
    IF N_Elements(yoffset) EQ 0 THEN yoffset = (screenSize[1]/2 - ysize/2)

    ; The progress must be between 0.0 and 100.0.
    percent = 0.0 > percent < 100.0
    
    ; Create the widgets for the program.
    self.tlb = Widget_Base(Column=1, Group_Leader=group_leader, $
        Floating=N_Elements(group_leader) GT 0, $
        XOffset=xoffset, YOffset=yoffset, Title=title, $
        Map=0, Base_Align_Center=1, TLB_Frame_Attr=11)
    IF text NE "" THEN BEGIN
       self.labelID = Widget_Label(self.tlb, Value=text, /Dynamic_Resize)
    ENDIF
    self.drawID = Widget_Draw(self.tlb, XSize=xsize, YSize=ysize, $
        Graphics_Level=2, Renderer=1, Retain=2)
    IF cancelbutton THEN BEGIN
       self.cancelID = Widget_Button(self.tlb, Value='Cancel')
    ENDIF
    
    ; Create needed objects.
    self.view = Obj_New('IDLgrView', ViewPlane_Rect=[0,0,xsize,ysize], $
        Color=[255, 255, 255])
    self.model = Obj_New('IDLgrModel')
    self.view -> Add, self.model
    
    ; Store sizes.
    self.xsize = xsize
    self.ysize = ysize
    
    ; Create the background image for the progress bar.
    self.image = self -> CreateImage()
    self.model -> Add, self.image
    
    ; Create the initial mask for the image and update it with the percent.
    mask = BytArr(4, xsize, ysize)
    mask[3,*,*] = 255B
    self.mask = Obj_New('IDLgrImage', mask, Blend_Function=[3,4], /No_Copy)
    self -> UpdateMask, percent
    self.model -> Add, self.mask
    
    ; Prepare to overlay percentage as text.
    percentString = String(percent, Format='(I0)') + ' %'
    self.text = Obj_New('IDLgrText', percentString, Alignment=0.5, $
        Location=[xsize/2,ysize/2,0.1], Vertical_Alignment=0.5, $
        Color=[255,255,255])
    self.model -> Add, self.text
    
    ; Start it up?
    IF start THEN self -> Start, percent

    RETURN, 1

END


;+
; The clean-up routine for the object.
;-
PRO cgProgressBar::CLEANUP

   IF Widget_Info(self.tlb, /VALID_ID) THEN Widget_Control, self.tlb, /Destroy
   Obj_Destroy, self.image
   Obj_Destroy, self.mask
   Obj_Destroy, self.text
   Obj_Destroy, self.view
   Obj_Destroy, self.model
   Obj_Destroy, self.window
END


;+
; Checks to see if a Cancel button has been clicked during the on-going operation.
; An optional response can be issued, if so.
; 
; :Keywords:
;    message: in, optional, type=string
;       If the RESPOND keyword is set, this is the message set in the blocking
;       dialog widget. By default: "Current Operation Cancelled by User".
;    respond: in, optional, type=boolean, default=0
;       If this keyword is set, the program responds to a positive cancel flag
;       by destroying the progress bar and setting a blocking dialog widget
;       for the user. The keyword is ignored if the cancel flag is zero.
;-
FUNCTION cgProgressBar::CheckCancel, $
    MESSAGE=message, $
    RESPOND=respond

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Assume no cancel operation.
    cancelFlag = 0
  
   ; Check for a Cancel event, if there is one.
   IF Widget_Info(self.cancelID, /Valid_ID) THEN BEGIN
      event = Widget_Event(self.cancelID, /NoWait)
      name = Tag_Names(event, /Structure_Name)
      IF name EQ 'WIDGET_BUTTON' THEN cancelFlag = 1
   ENDIF
   
   ; Need a response to the cancel flag?
   IF cancelFlag && Keyword_Set(respond) THEN BEGIN
       self -> Destroy
       IF N_Elements(message) EQ 0 THEN message = "Current Operation Cancelled by User"
       void = Dialog_Message(message)
   ENDIF

   RETURN, cancelFlag
END


;+
; Create the object image for the program. Taken from Ronn Kling's
; "really cool" background image in krProgressBar.
;-
FUNCTION cgProgressBar::CreateImage

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, -1
    ENDIF

    ; RGB vectors set up by Ronn Kling.
    red = Bytscl([Intarr(193), Indgen(63)])
    grn = Bytscl([Intarr(96) , Indgen(256-96)])
    blu = Bytscl([Intarr(188), Intarr(256-188)+188])
    
    data = [228,228,238,248,248,238,228,217,197,186,165,155,145,145,145, $
            145,155,166,176,186,186,186,176,166,124]
    IF self.ysize NE 25 THEN data = Congrid(data, self.ysize)
    
    ; Arrange the data in the form of an image.
    data = Rebin(Reform(data, 1, self.ysize), self.xsize, self.ysize)
    img = BytArr(3, self.xsize, self.ysize)
    img[0,*,*] = red[data]
    img[1,*,*] = grn[data]
    img[2,*,*] = blu[data]
    
    ; Create an object image.
    image = Obj_New('IDLgrImage', img, Interpolate=1, /No_Copy)
    
    RETURN, image

END

;+
; Destroys the progress bar.
;-
PRO cgProgressBar::Destroy

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
   IF Widget_Info(self.tlb, /VALID_ID) THEN Widget_Control, self.tlb, /DESTROY
   Obj_Destroy, self

END


;+
; Updates the progress bar.
; 
; :Params:
;     percent: in, required, type=float
;         The percent the progress bar has been completed. A number between
;         0 and 100. Forced into this range.
;-
PRO cgProgressBar::Update, percent

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Nothing to do if percent equals zero.
    IF (N_Elements(percent) EQ 0) || (percent EQ 0.0) THEN RETURN
    
    ; Percent must be between 0 and 100.
    percent = 0.0 > percent < 100.0
    
    ; Update the mask.
    self -> UpdateMask, percent
    
    ; Update the text object.
    newText = String(percent, Format='(I0)') + ' %'
    self.text -> SetProperty, String=newText
    
    ; Draw the progress bar.
    self.window -> Draw, self.view
    
END


;+
; Update the image mask to reflect the correct percentage.
; 
; :Params:
;     percent: in, required, type=float
;         The percent the progress bar has been completed. A number between
;         0 and 100. Forced into this range.
;-
PRO cgProgressBar::UpdateMask, percent

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Nothing to do if percent equals zero.
    IF (N_Elements(percent) EQ 0) || (percent EQ 0.0) THEN RETURN
    
    ; Percent must be between 0 and 100.
    percent = 0.0 > percent < 100.0

    ; Get the current maps
    self.mask -> GetProperty, Data=data

    ; Modify the mask by the percent needed and update the alpha channel.
    newMask = BytArr(self.xsize, self.ysize) + 255B
    x = Reverse(Bytscl(Findgen((self.xsize*(percent/100.))>1)))
    y = Bytarr(self.ysize) + 1B
    newMask[0,0] = x # y
    data[3, *, *] = newMask
    
    ; Set it back.
    self.mask -> SetProperty, Data=data
     
END


;+
; Starts the progress bar by realizing the progress bar on the display.
;
; :Params:
;    percent: in, optional, type=float
;       Starts the progress bar with this percentage completion. A number
;       between 0.0 and 100.0.
;-
PRO cgProgressBar::Start, percent

    Compile_Opt idl2
    
    ; Standard error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Check parameters.
    IF N_Elements(percent) EQ 0 THEN percent = 0.0
    
    ; Realize the widget.
    Widget_Control, self.tlb, /Realize, Map=1

    ; Get the window object reference number of the draw widget.
    Widget_Control, self.drawID, Get_Value=window
    self.window = window
    
    ; Draw the bar.
    self.window -> Draw, self.view

    ; Do you supply a starting percent?
    IF N_Elements(percent) NE 0 THEN self -> Update, (0.0 > percent < 100.0)

END

;+
; The event handler for the progress bar example program.
;
; :Params:
;     event: in, required, type=structure
;        The event structure.
;-
PRO Progressbar_Example_Event, event

    ; Respond to program button events.

    Widget_Control, event.id, Get_Value=buttonValue
    
    CASE buttonValue OF
    
       'Start Simple Loop': BEGIN
    
          ; Create the progress bar.
          progressbar = Obj_New('cgprogressbar', /Start)
    
          ; Start the loop.
          count = 0
          FOR j=0, 1000 DO BEGIN
    
              IF j MOD 10 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
                progressbar -> Update, (count * 1.0)
                count = count + 1
              ENDIF
    
              Wait, 0.01 ; This is where you would do something useful.
          ENDFOR
    
          ; Destroy the progress bar when you are finished with it.
          progressbar -> Destroy
          ENDCASE
    
    
       'Start Cancel Loop': BEGIN
    
         ; Create the progress bar.
          progressbar = Obj_New('cgprogressbar', /Cancel)
    
          ; Place the progress bar on the display.
          progressbar -> Start
    
          ; Start the loop.
          count = 0
          FOR j=0, 1000 DO BEGIN
    
              IF j MOD 10 EQ 0 THEN BEGIN ; Update the progess bar every 100 times through loop.
    
                ; Did the user try to cancel the progress bar?
                IF progressbar->CheckCancel() THEN BEGIN
                   ok = Dialog_Message('User cancelled operation.') ; Other cleanup, etc. here.
                   progressbar -> Destroy ; Destroy the progress bar.
                   RETURN
                ENDIF
    
                ; If user didn't cancel, update the progress bar. Update value
                ; must be between 0 and 100.
                progressbar -> Update, (count * 1.0)
                count = count + 1
              ENDIF
    
              Wait, 0.01 ; This is where you would do something useful.
          ENDFOR
    
          ; Destroy the progress bar when you are finished with it.
          progressbar -> Destroy
          ENDCASE
    
       'Quit': Widget_Control, event.top, /Destroy
    
    ENDCASE

END


;+
; This is an example program that illustrates how the cgProgressBar
; object can be used.
;-
PRO Progressbar_Example
    tlb = Widget_Base(Column=1, Xoffset=200, Yoffset=200)
    button = Widget_Button(tlb, Value='Start Simple Loop')
    button = Widget_Button(tlb, Value='Start Cancel Loop')
    quiter = Widget_Button(tlb, Value='Quit')
    Widget_Control, tlb, /Realize
    XManager, 'progressbar_example', tlb, /No_Block
END


;+
; The class definition module for the cgPROGRESSBAR object class.
;
; :Params:
;     class: out, optional, type=structure
;        The object class definition as a structure. Occasionally useful.
;-
PRO cgProgressBar__Define, class

   class = { cgPROGRESSBAR, $
             INHERITS IDL_OBJECT, $
             image: Obj_New(), $
             mask: Obj_New(), $
             text: Obj_New(), $
             view: Obj_New(), $
             model: Obj_New(), $
             window: Obj_New(), $
             xsize: 0L, $
             ysize: 0L, $
             tlb: 0L, $
             drawID: 0L, $
             cancelID: 0L, $
             labelID: 0L $
            }
END