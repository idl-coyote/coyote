; docformat = 'rst'
;
; NAME:
;   cgSurface
;
; PURPOSE:
;   The purpose of cgSurface is to create a window where a surface is displayed. Surfaces
;   can be wire-framed, shaded surfaces, and surfaces with texture maps draped on top of
;   them, among other types of surfaces. LEFT mouse button rotates the surface, MIDDLE
;   mouse button zooms out from the surface, RIGHT mouse button zoom into the surface. 
;   Clicking on the surface axes will allow the user to move or translate the surface, and 
;   clicking on the plot title will allow the user to move the title.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   The purpose of cgSurface is to create a window where a surface is displayed. Surfaces
;   can be wire-framed, shaded surfaces, and surfaces with texture maps draped on top of
;   them, among other types of surfaces. LEFT mouse button rotates the surface, MIDDLE
;   mouse button zooms out from the surface, RIGHT mouse button zoom into the surface. 
;   Clicking on the surface axes will allow the user to move or translate the surface, and 
;   clicking on the plot title will allow the user to move the title.
;
; .. image:: cgsurface.png
; 
; :Categories:
;    Graphics
;    
; :Examples:
;    Use as you would use the IDL SURFACE of SHADE_SURF command::
;       data = Dist(200)
;       LoadCT, 33
;       cgSurface, data
;       cgSurface, data, /Elevation_Shading
;       cgSurface, data, /Shaded
;       cgSurface, data, /Shaded, Texture_Image=cgDemoData(16) 
;       
;       Setting up the initial surface rotation.
;       IDL> T3D, /RESET, ROTATE=[0, 0, 30]
;       IDL> T3D, ROTATE=[-90, 0, 0]
;       IDL> T3D, ROTATE=[0, 30, 0]
;       IDL> T3D, ROTATE=[30, 0, 0]
;       IDL> cgSurface, cgDemoData(2), Transform=!P.T
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
;        Completely re-written, 26 November 2010 from old cgSURFACE program. DWF.
;        Added ability to translate the surface by clicking on an axis. 28 Nov 2010. DWF.
;        Fixed a problem with light controls in which the light controls didn't show the
;            current light color. 28 Nov 2010. DWF.
;        I was ANDing [XYZ]Style keywords with 8 instead of 4 for hidded axes. Fixed. 4 Jan 2011. DWF.
;        Added Axes ON/OFF button. 4 Jan 2011. DWF.
;        Rotation is throwing underflow warnings, so switched to code that surpress 
;            these warnings. 26 Aug 2011. DWF
;        Added TRANSFORM keyword to allow the initial surface to be rotated to user 
;            specifications. 26 Sept 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2010-2011, Fanning Software Consulting, Inc.
;-

;+
; Controls light intensity by handling selection events from the Intensity Value widget.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO CW_Light_Control_Intensity_Events, event

    ; Handles selection events from the Intensity Value widget.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, infoCarrier, Set_UValue=info, /No_Copy
    ENDIF

    ; Get the info carrier.
    parent = Widget_Info(event.id, /Parent)
    infoCarrier = Widget_Info(parent, Find_by_UName='CW_LIGHT_CARRIER')
    Widget_Control, infoCarrier, Get_UValue=info, /No_Copy
    
    ; Get the new intensity value.
    info.theIntensity = *event.selection
    
    ; Change the intensity of the light.
    info.theLight->SetProperty, Intensity=info.theIntensity
    
    ; Prepare to send an event that notifies the program.
    event_pro = info.event_pro
    tlb = info.tlb
    top = event.top
    parent = info.parent
    name = info.name
    intensity = info.theIntensity
    color = info.theColor
    hide = info.theHide
    Widget_Control, infoCarrier, Set_UValue=info, /No_Copy
    
    ; Send the event.
    IF event_pro NE "" THEN BEGIN
       eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
          NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
       Widget_Control, parent, Send_Event=eventStruct
    ENDIF
    
END ;------------------------------------------------------------------------------



;+
; Controls light properties such as color, whether the light is on or off, etc.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO CW_Light_Control_Events, event

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, infoCarrier, Set_UValue=info, /No_Copy
    ENDIF

    ; Get the info structure.
    infoCarrier = Widget_Info(event.handler, Find_By_UName='CW_LIGHT_CARRIER')
    Widget_Control, infoCarrier, Get_UValue=info, /No_Copy
    
    ; What kind of event is this? Branch appropriately.
    Widget_Control, event.id, Get_UValue=thisEvent
    CASE thisEvent OF
    
       'COLOR': BEGIN
          TVLCT, info.color, info.index
          DEVICE, Decomposed=0, Get_Decomposed=theDecomposedState
          setcolor_title = Widget_Info(event.id, /UNAME)
          thisColor = PickColor(CURRENTCOLOR=info.color, Group_Leader=event.top, Title=setcolor_title)
          thisColor = Reform(thisColor, 3, 1)
          info.theLight->SetProperty, Color=thisColor
          DEVICE, Decomposed=theDecomposedState
          info.color = thisColor
          END
    
       'RESET': BEGIN
          info.theColor = info.origColor
          info.theIntensity = info.origIntensity
          info.theHide = info.origHide
          info.color = info.origColor
          info.intensityID->SetSelection, info.origIntensity
          IF info.origHide THEN BEGIN
             Widget_Control, info.onButtonID, Set_Button=0
             Widget_Control, info.offButtonID, Set_Button=1
          ENDIF ELSE BEGIN
             Widget_Control, info.onButtonID, Set_Button=1
             Widget_Control, info.offButtonID, Set_Button=0
          ENDELSE
    
          info.theLight->SetProperty, Intensity=info.origIntensity, $
             Color=info.origColor, Hide=info.origHide
    
          END
    
       'ON': BEGIN
          info.theHide = 0
          info.theLight->SetProperty, Hide=0
          END
    
       'OFF': BEGIN
          info.theHide = 1
          info.theLight->SetProperty, Hide=1
          END
    
    ENDCASE
    
    ; Prepare to send an event if requested.
    event_pro = info.event_pro
    tlb = info.tlb
    top = event.top
    parent = info.parent
    name = info.name
    intensity = info.theIntensity
    color = info.theColor
    hide = info.theHide
    Widget_Control, infoCarrier, Set_UValue=info, /No_Copy
    
    ; Send the event.
    IF event_pro NE "" THEN BEGIN
       eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
          NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
       Widget_Control, parent, Send_Event=eventStruct
    ENDIF
    
END ;------------------------------------------------------------------------------



;+
; This is a compound widget that allows the user to manipulate various
; properties of a light object via a graphical user interface.
; 
; :Params:
;    parent: in, required
;       The parent widget of this compound widget.
;    thelight: in, required, type=object
;       An object reference to a particular light object.
;       
; :Keywords:
;   color: in, optional, type=bytarr
;      A color triple representing the color of the light.
;   event_pro: in, optional, type=string
;      The name of an event handler that will handle events for this widget.
;   index: in, optional, type=integer
;      A color table index number. If the `Color` keyword is not used, the color
;      will be obtained from the colors loaded in the current color table at this
;      color table index number.
;   labelsize: in, optional, type=integer
;      The size of the label widget used in the compound widget in pixels.
;      If not specified, the "natural" size of the label widget is used.
;   name: in, optional, type=string, default=""
;      A name for this widget. Provided to help identify the widget in
;      event handlers.
;   setcolor_name: in, optional, type=string, default=""
;      This keyword sets the UNAME property of the Set Color button in the interface.
;      It's purpose is to help you identify that button in event handlers.
;   uvalue: in, optional
;       A container to store any IDL variable needed by the user of this program.
;-
FUNCTION CW_Light_Control, parent, theLight, $
  Color=color, $
  Event_Pro=event_pro, $
  Index=index, $
  LabelSize=labelsize, $
  Name=name, $
  SetColor_Name=setColor_name, $
  UValue=uvalue

; This is a compound widget that allows one to manipulate various
; properties of light objects.

    On_Error, 2
    
    ; Check parameters. Define defaults if necessary.
    IF N_Elements(parent) EQ 0 THEN Message, 'Parent widget parameter is required 1st parameter.'
    IF (N_Elements(theLight) EQ 0) OR (Size(theLight, /TName) NE 'OBJREF') THEN $
       Message, 'Light Object Reference is required 2nd parameter.'
    IF N_Elements(uvalue) EQ 0 THEN uvalue = "LIGHT_CONTROL"
    IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
    IF N_Elements(index) EQ 0 THEN index =  !D.Table_Size-2
    IF N_Elements(setcolor_name) EQ 0 THEN setcolor_name = ""
    IF N_Elements(color) EQ 0 THEN BEGIN
       TVLCT, r, g, b, /Get
       color = Reform([r[index], g[index], b[index]], 1, 3)
    ENDIF ELSE color = Reform(color, 1, 3)
    TVLCT, color, index
    
    ; Set the light properties.
    theLight->GetProperty, Intensity=theIntensity, Hide=theHide, Color=theColor
    IF N_Elements(name) EQ 0 THEN name = 'Light'
    
    ; Create the widgets.
    tlb = Widget_Base(parent, Row=1, Base_Align_Center=1, $
       Event_Pro='CW_Light_Control_Events')
    IF N_Elements(labelsize) NE 0 THEN $
       labelID = Widget_Label(tlb, Value=name + ': ', UNAME='CW_LIGHT_CARRIER', XSize=labelsize) ELSE $
       labelID = Widget_Label(tlb, Value=name + ': ', UNAME='CW_LIGHT_CARRIER')
    exBaseID = Widget_Base(tlb, Row=1, /Exclusive, /Frame)
    onButtonID = Widget_Button(exBaseID, Value='On', UValue='ON')
    offButtonID = Widget_Button(exBaseID, Value='Off', UValue='OFF')
    IF theHide THEN Widget_Control, offbuttonID, /Set_Button ELSE $
       Widget_Control, onbuttonID, /Set_Button
    intensityValues = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    intensityID = FSC_Droplist(tlb, Title='Intensity:', Value = intensityValues, $
       Event_Pro='CW_Light_Control_Intensity_Events', Format='(F3.1)', Spaces=[1,1])
    intensityID->SetSelection, theIntensity
    colorID = Widget_Button(tlb, Value='Set Color', UValue='COLOR', UNAME=setcolor_name)
    resetID= Widget_Button(tlb, Value='Reset', UValue='RESET')
    Widget_Control, tlb, /Realize
    
    ; Create info structure with information to run the program. Store it.
    info = {theLight:theLight, name:name, theIntensity:theIntensity, theHide:theHide, color:color, $
            theColor:theColor, Event_Pro:event_pro, origIntensity:theIntensity, index:index, $
            origColor:theColor, origHide:theHide, tlb:tlb, parent:parent, intensityID:intensityID, $
            onButtonID:onButtonID, offButtonID:offButtonID}
    Widget_Control, labelID, Set_UValue=info, /No_Copy
    
    RETURN, tlb
    
END ;------------------------------------------------------------------------------



;+
; An event handler that destroys the light controller in the program.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Light_Done, event
    Widget_Control, event.top, /Destroy
END ;--------------------------------------------------------------------



;+
; An event handler that renders the light controller's graphics.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Light_Controls_Event, event
    Widget_Control, event.top, Get_UValue=info
    info.theWindow->Draw, info.theView
END
;-------------------------------------------------------------------------



;+
; An event handler that creates the graphical user interface for the
; light controller.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Light_Controls, event

    ; Place the light control beside the current widget program.
    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
    xpos = sizes[0] + offsets[0] + 10
    ypos = offsets[1] + 100
    
    ; Lights only make sense with a solid surface.
    info.thisSurface->SetProperty, Style=2, Shading=1
    info.thisWindow->Draw, info.thisView
    
    ; Create widgets.
    tlb = Widget_Base(Title='cgSurface Light Controls', Column=1, Group_Leader=event.top, $
       UValue={theView:info.thisView, theWindow:info.thisWindow}, XOffset=xpos, YOffset=ypos)
    dummy = CW_Light_Control(tlb, Name='Non-Rotating Light', info.nonRotatingLight, LabelSize=130, $
       Event_Pro='cgSurface_Light_Controls_Event', Index=!D.Table_Size-18, Color=[255,255,255], $
       SetColor_Name='Color for Non-Rotating Light')
    dummy = CW_Light_Control(tlb, Name='Rotating Light', info.rotatingLight, LabelSize=130, $
       Event_Pro='cgSurface_Light_Controls_Event', Index=!D.Table_Size-19, Color=[255,255,255], $
       SetColor_Name='Color for Rotating Light')
    dummy = CW_Light_Control(tlb, Name='Fill Light', info.fillLight, LabelSize=130, $
       Event_Pro='cgSurface_Light_Controls_Event', Index=!D.Table_Size-20, Color=[255,255,255], $
       SetColor_Name='Color for Fill Light')
    dummy = CW_Light_Control(tlb, Name='Ambient Light', info.ambientLight, LabelSize=130, $
       Event_Pro='cgSurface_Light_Controls_Event', Index=!D.Table_Size-21, Color=[255,255,255], $
       SetColor_Name='Color for Ambient Light')
    quit = Widget_Button(tlb, Value='Done', Event_Pro='cgSurface_Light_Done')
    
    Widget_Control, tlb, /Realize
    
    XManager, 'cgSurface_Light_Controls', tlb, /No_Block, Event_Handler='cgSurface_Light_Controls_Event'
    Widget_Control, event.top, Set_UValue=info, /No_Copy

END
;-------------------------------------------------------------------------


;+
; An event handler that turns the surface axes on or off.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Axes_OnOff, event

    ; This event handler turns the surface axes on or off.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=uvalue
    Widget_Control, event.id, Set_Value=uvalue, Set_UValue=buttonValue
    
    CASE buttonValue OF
    
       'Turn Axes ON': BEGIN
            info.xaxis -> SetProperty, HIDE=0
            info.yaxis -> SetProperty, HIDE=0
            info.zaxis -> SetProperty, HIDE=0         
        END
        
       ; Not at all sure why this works!
       'Turn Axes OFF': BEGIN
            info.xaxis -> SetProperty, HIDE=1
            info.yaxis -> SetProperty, HIDE=1
            info.zaxis -> SetProperty, HIDE=1         
        END
    
    ENDCASE
    
    ; Draw the graphic display.
    info.thisWindow -> Draw, info.thisView
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ;------------------------------------------------------------------------------


;+
; An event handler that turns the bottom color on or off.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Bottom_OnOff, event

    ; This event handler turns the bottom color on or off.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=uvalue
    Widget_Control, event.id, Set_Value=uvalue, Set_UValue=buttonValue
    
    CASE buttonValue OF
    
       'Bottom Color ON': info.thisSurface -> SetProperty, Bottom=info.bottom
    
       ; Not at all sure why this works!
       'Bottom Color OFF': info.thisSurface -> SetProperty, Bottom=info.bottomOffPtr
    
    ENDCASE
    
    ; Draw the graphic display.
    info.thisWindow -> Draw, info.thisView
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ;------------------------------------------------------------------------------


;+
; An event handler that changes the color tables for elevation shading.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Change_Colors, event

    ; This event handler changes color tables for elevation shading.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=buttonUValue
    CASE StrUpCase(buttonValue) OF
        'TITLE COLOR': BEGIN
            title = 'Set Title Color'
            color = cgPickColorName(buttonUValue, TITLE=title, GROUP_LEADER=event.top)
            info.tcolor = cgColor(color, /Triple, /Row)
            info.plottitle -> SetProperty, COLOR=info.tcolor
            END
        'SURFACE COLOR': BEGIN
            title = 'Set Surface Color'
            color = cgPickColorName(buttonUValue, TITLE=title, GROUP_LEADER=event.top)
            info.color = cgColor(color, /Triple, /Row)
            info.thisSurface -> SetProperty, COLOR=info.color
            END
        'BACKGROUND COLOR': BEGIN
            title = 'Set Background Color'
            color = cgPickColorName(buttonUValue, TITLE=title, GROUP_LEADER=event.top)
            info.background = cgColor(color, /Triple, /Row)
            info.thisView -> SetProperty, COLOR=info.background
            END
        'AXIS COLOR': BEGIN
            title = 'Set Axis Color'
            color = cgPickColorName(buttonUValue, TITLE=title, GROUP_LEADER=event.top)
            info.axiscolor = cgColor(color, /Triple, /Row)
            info.xaxis -> SetProperty, COLOR=info.axiscolor
            info.yaxis -> SetProperty, COLOR=info.axiscolor
            info.zaxis -> SetProperty, COLOR=info.axiscolor
            END
        'BOTTOM COLOR': BEGIN
            title = 'Set Bottom Color'
            color = cgPickColorName(buttonUValue, TITLE=title, GROUP_LEADER=event.top)
            info.bottom = cgColor(color, /Triple, /Row)
            info.thisSurface -> SetProperty, BOTTOM=info.bottom
            END
    ENDCASE
    
    ; Set the user value to new color name.
    Widget_Control, event.id, SET_UVALUE=color
    
    ; Draw the surface.
    info.thisWindow -> Draw, info.thisView
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


;+
; An event handler for draw events such as expose events and trackball
; events. The trackball uses the IDL-supplied TRACKBALL_DEFINE.PRO.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Draw_Events, event

    ; Draw widget events handled here: expose events and trackball
    ; events. The trackball uses RSI-supplied TRACKBALL_DEFINE.PRO.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
    thisEvent = drawTypes(event.type)
    
    CASE thisEvent OF
    
       'EXPOSE':  BEGIN
           ; Expose events are NOT blocked by modal widgets, thus, it is possible
           ; to get an expose event here when it is not expected. This will cause
           ; an error, since info will be undefined. Check for this condition before
           ; processing.
           IF N_Elements(info) EQ 0 THEN RETURN
           END
    
       'PRESS': BEGIN
       
           item = info.thisWindow->Select(info.thisView, [event.x, event.y])
           IF Obj_Valid(item[0]) THEN BEGIN
               IF Obj_Class(item[0]) EQ 'IDLGRTEXT' THEN BEGIN
                   Widget_Control, event.id, /CLEAR_EVENTS
                   Widget_Control, event.id, EVENT_PRO='cgSURFACE_MOVE_TITLE'
                   info.xstart = event.x
                   info.ystart = event.y
                   info.selectedItem = item[0]
                   Widget_Control, event.top, Set_UValue=info, /No_Copy
                   Widget_Control, event.id, DRAW_MOTION_EVENTS=1
                   RETURN
               ENDIF
               IF Obj_Class(item[0]) EQ 'IDLGRAXIS' THEN BEGIN
                   Widget_Control, event.id, /CLEAR_EVENTS
                   Widget_Control, event.id, EVENT_PRO='cgSURFACE_MOVE_SURFACE'
                   info.xstart = event.x
                   info.ystart = event.y
                   info.selectedItem = item[0]
                   Widget_Control, event.top, Set_UValue=info, /No_Copy
                   Widget_Control, event.id, DRAW_MOTION_EVENTS=1
                   RETURN
               ENDIF
           ENDIF
    
           ; Zoom out on middle, zoom in on right, rotate on left.
           possibleButtons = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
           thisButton = possibleButtons(event.press)
    
           CASE thisButton OF
    
             'RIGHT': BEGIN
                      info.thisView->GetProperty, Viewplane_Rect=thisRect
                      thisRect(0) = (thisRect(0) + 0.05) < thisRect(2)
                      thisRect(1) = (thisRect(1) + 0.05) < thisRect(3)
                      thisRect(2) = (thisRect(2) - 0.1) > thisRect(0)
                      thisRect(3) = (thisRect(3) - 0.1) > thisRect(1)
                      info.thisView->SetProperty, Viewplane_Rect=thisRect
                      END
    
             'MIDDLE': BEGIN
                      info.thisView->GetProperty, Viewplane_Rect=thisRect
                      thisRect(0) = thisRect(0) - 0.05
                      thisRect(1) = thisRect(1) - 0.05
                      thisRect(2) = thisRect(2) + 0.1
                      thisRect(3) = thisRect(3) + 0.1
                      info.thisView->SetProperty, Viewplane_Rect=thisRect
                      END
    
             'LEFT':  BEGIN
                      Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
                      info.thisWindow->SetProperty, Quality=info.dragQuality ; Set Drag Quality.
                      END
    
             ELSE:
           ENDCASE
    
           END
       'RELEASE': BEGIN
           Widget_Control, event.id, Draw_Motion_Events=0 ; Motion events OFF.
           info.thisWindow->SetProperty, Quality=2 ; Drag Quality to High.
           END
       'MOTION': BEGIN ; Trackball events
           END
           
       ELSE: ; Fall though, don't care.
    
    ENDCASE
    
    ; Does the trackball need updating? If so, update.
    needUpdate = info.thisTrackball -> Update(event, Transform=thisTransform)
    IF needUpdate THEN BEGIN
       info.thisModel->GetProperty, Transform=modelTransform
       info.thisModel->SetProperty, Transform=modelTransform # thisTransform
    ENDIF
    
    ; Draw the view. If this program STILL throws floating point exceptions,
    ; comment this line out and uncomment the code below it. Dishonest as
    ; all get out, but it works fine. :-)
    ;info.thisWindow->Draw, info.thisView
    currentExcept = !Except
    !Except = 0
    info.thisWindow -> Draw, info.thisView
    dummy = Check_Math()
    !Except = currentExcept
    
    ; Put the info structure back.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


;+
; An event handler changing the colors used in elevation shading.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Elevation_Colors, event

    ; This event handler changes color tables for elevation shading.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    ; What kind of event is this? Could be from Change Colors button
    ; or from XCOLORS itself.
    thisEvent = Tag_Names(event, /Structure_Name)
    CASE thisEvent OF
    
       "WIDGET_BUTTON": BEGIN
          IF info.colortable EQ -1 THEN BEGIN
            TVLCT, info.r, info.g, info.b
          ENDIF ELSE BEGIN
            cgLoadCT, info.colortable, BREWER=info.brewer, REVERSE=info.reverse
          ENDELSE
          XColors, Group_Leader=event.top, NotifyID=[event.id, event.top], $
             Title="cgSurface Elevation Shading Colors", BREWER=info.brewer, $
             INDEX=info.colortable, REVERSE=info.reverse
          ENDCASE
    
       "XCOLORS_LOAD": BEGIN
          info.r = event.r
          info.g = event.g
          info.b = event.b
          info.colortable = event.index
          info.brewer = event.brewer
          info.reverse = event.reversed
          IF Obj_Valid(info.colorPalette) THEN info.colorPalette->SetProperty, $
             Red=event.r, Green=event.g, Blue=event.b
          ENDCASE
    
    ENDCASE
    
    ; Draw the graphic display.
    info.thisWindow -> Draw, info.thisView
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ;------------------------------------------------------------------------------


;+
; An event handler to set up elevation shading for the surface.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Elevation_Shading, event

    ; This event handler sets up elevation shading for the surface.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=uvalue
    Widget_Control, event.id, Set_Value=uvalue, Set_UValue=buttonValue
    
    CASE buttonValue OF
    
       'Elevation Shading ON': BEGIN
          s = Size(info.data, /Dimensions)
          info.zAxis->GetProperty, CRange=zrange
          info.thisSurface->SetProperty, Palette=info.colorPalette, $
             Vert_Colors=Reform(BytScl(info.data, /NAN, Min=Min(zrange), $
             Max=Max(zrange)), s[0]*s[1]), Bottom=info.bottomOffPtr, Specular=""
          Widget_Control, info.bottomID, Set_Value='Bottom Color ON' 
          Widget_Control, info.bottomID, Set_UValue='Bottom Color OFF'
          ENDCASE
    
       'Elevation Shading OFF': BEGIN
          info.thisSurface->SetProperty, Palette=Obj_New(), Vert_Colors=0, $
            Bottom=info.bottom, SPECULAR=info.specularColor
          Widget_Control, info.bottomID, Set_Value='Bottom Color OFF'
          Widget_Control, info.bottomID, Set_UValue='Bottom Color ON'
          ENDCASE
    
    ENDCASE
    
    ; Draw the graphic display.
    info.thisWindow->Draw, info.thisView
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


;+
; An event handler to destroy the GUI for this program.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.
   Widget_Control, event.top, /Destroy
   
END ;-----------------------------------------------------------------------------------------


;+
; An event handler to allow the surface to move in the graphics window.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Move_Surface, event

    ; This event handler moves the surface.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
    thisEvent = drawTypes(event.type)
    CASE thisEvent OF
    
        'RELEASE': BEGIN
            Widget_Control, event.id, /CLEAR_EVENTS
            Widget_Control, event.id, EVENT_PRO='cgSURFACE_DRAW_EVENTS'
            Widget_Control, event.id, DRAW_MOTION_EVENTS=0
            info.xstart = -1
            info.ystart = -1
            info.selectedItem = Obj_New()
            END
            
        'MOTION': BEGIN
            delta_x = (event.x - info.xstart) / Float(info.xsize) 
            delta_y = (event.y - info.ystart) / Float(info.ysize) 
            info.thisModel -> Translate, 2*delta_x, 2*delta_y, 0
            info.thisWindow -> Draw, info.thisView
            info.xstart = event.x
            info.ystart = event.y
            END
    ENDCASE
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;------------------------------------------------------------------------------


;+
; An event handler to allow the title to move in the graphics window.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Move_Title, event

    ; This event handler moves the surface title.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
    thisEvent = drawTypes(event.type)
    CASE thisEvent OF
    
        'RELEASE': BEGIN
            Widget_Control, event.id, /CLEAR_EVENTS
            Widget_Control, event.id, EVENT_PRO='cgSURFACE_DRAW_EVENTS'
            Widget_Control, event.id, DRAW_MOTION_EVENTS=0
            info.xstart = -1
            info.ystart = -1
            info.selectedItem = Obj_New()
            END
            
        'MOTION': BEGIN
            delta_x = (event.x - info.xstart) / Float(info.xsize) 
            delta_y = (event.y - info.ystart) / Float(info.ysize) 
            info.textModel -> Translate, 2*delta_x, 2*delta_y, 0
            info.thisWindow -> Draw, info.thisView
            info.xstart = event.x
            info.ystart = event.y
            END
    ENDCASE
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;------------------------------------------------------------------------------


;+
; An event handler to allow the user to save the graphics window
; in a variety of output formats, including raster formats and
; PostScript.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Output, event

   ; This event handler creates GIF and JPEG files.

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    ; Get a snapshop of window contents. (TVRD equivalent.)
    info.thisWindow->GetProperty, Image_Data=snapshot
    
    ; What kind of file is wanted?
    Widget_Control, event.id, GET_UValue=whichFileType
    CASE whichFileType OF
    
       'GIF': BEGIN
    
             ; Because we are using RGB color for this model, we have
             ; a 3-m-n array. Use Color_Quan to create a 2D image and
             ; appropriate color tables for the GIF file.
    
          image2D = Color_Quan(snapshot, 1, r, g, b)
          filename = Dialog_Pickfile(/Write, File='cgsurface.gif')
          IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
          END
    
       'JPEG': BEGIN
    
          filename = Dialog_Pickfile(/Write, File='cgsurface.jpg')
          IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100
          END
    
    
       'TIFF': BEGIN
    
          filename = Dialog_Pickfile(/Write, File='cgsurface.tif')
          IF filename NE '' THEN BEGIN
    
             ; TIFF files should have their Y direction reversed for
             ; compatibility with most other software.
    
             Write_TIFF, filename, Reverse(snapshot,3)
          ENDIF
          END
    
       'BMP': BEGIN
          filename = Dialog_Pickfile(/Write, File='cgsurface.bmp')
          IF filename NE '' THEN Write_BMP, filename, snapshot
          END
    
       'PNG': BEGIN
          filename = Dialog_Pickfile(/Write, File='cgsurface.png')
          IF filename NE '' THEN Write_PNG, filename, snapshot
          END
    
       'PS': BEGIN
          filename = Dialog_Pickfile(/Write, File='cgsurface.ps')
          IF filename NE '' THEN BEGIN
             resolution = [2.54, 2.54]/ 600; 600 pixels per inch
             viewDimensions = [info.xsize, info.ysize] / 100.0 ; 100 pixels in size = 1 inch
             clipboard = Obj_New('IDLgrClipboard', Dimensions=viewDimensions, Resolution=resolution, Unit=1)
             clipboard->Draw, info.thisView, /Postscript, Filename=filename, /Vector
             Obj_Destroy, clipboard
          ENDIF
          END
    
    ENDCASE
    
        ;Put the info structure back.
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ;------------------------------------------------------------------------------


;+
; An event handler to allow the user to change various surface properties.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Properties, event

    ; Event handler to set program properties.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    ; What property is wanted?
    Widget_Control, event.id, Get_UValue=newProperty
    CASE newProperty OF
    
       'ORIGINAL_T3D': info.thisModel->SetProperty, Transform=info.origTransform
       
       'DRAG_LOW': BEGIN
          info.dragQuality = 0
          Widget_Control, info.dragLowID, Sensitive=0
          Widget_Control, info.dragMedID, Sensitive=1
          Widget_Control, info.dragHighID, Sensitive=1
          END
    
       'DRAG_MEDIUM': BEGIN
          info.dragQuality = 1
          Widget_Control, info.dragMedID, Sensitive=0
          Widget_Control, info.dragLowID, Sensitive=1
          Widget_Control, info.dragHighID, Sensitive=1
          END
    
       'DRAG_HIGH': BEGIN
          info.dragQuality = 2
          Widget_Control, info.dragMedID, Sensitive=1
          Widget_Control, info.dragLowID, Sensitive=1
          Widget_Control, info.dragHighID, Sensitive=0
          END
    
    ENDCASE
    
    ; Redraw the graphic.
    info.thisWindow->Draw, info.thisView
    
    ;Put the info structure back.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


;+
; An event handler to respond to window resizing events.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Resize, event

    ; The only events generated by this simple program are resize
    ; events, which are handled here.

    ; Get the info structure.
    Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget. This is the proper way to do this
    ; in object graphics, but it does not always work in UNIX
    ; versions of IDL. If it doesn't work for you, comment the
    ; first line out and try the second. The second line is more
    ; portable, but not exactly the proper "object" way. :-(
    info.thisWindow->SetProperty, Dimension=[event.x, event.y]
    ;Widget_Control, info.drawID, Draw_XSize=event.x, Draw_YSize=event.y
    
    ; Store the new size.
    info.xsize = event.x
    info.ysize = event.y

    ; Redisplay the graphic.
    info.thisWindow->Draw, info.thisView

    ; Update the trackball objects location in the center of the window.
    info.thisTrackball->Reset, [event.x/2, event.y/2], (event.y/2) < (event.x/2)

    ; Put the info structure back.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



;+
; An event handler to turn the surface skirt on and off.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Skirt_OnOff, event

    ; This event handler turns the skirt on or off.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=uvalue
    Widget_Control, event.id, Set_Value=uvalue, Set_UValue=buttonValue
    
    CASE buttonValue OF
    
       'Turn Skirt ON':  info.thisSurface -> SetProperty, SHOW_SKIRT=1
       'Turn Skirt OFF': info.thisSurface -> SetProperty, SHOW_SKIRT=0
    
    ENDCASE
    
    ; Draw the graphic display.
    info.thisWindow -> Draw, info.thisView
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    
END ;------------------------------------------------------------------------------


;+
; An event handler to respond to events from the Style menu, changing
; style properties of the surface.
; 
; :Params:
;    event: in, required
;       The event structure from the graphical user interface of the program.
;-
PRO cgSurface_Style, event

     ; Event handler to select surface style.

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
    ENDIF

    Widget_Control, event.top, Get_UValue=info, /No_Copy
    
    ; Make sure lights are turned on.
    info.nonRotatingLight->SetProperty, Hide=0
    info.rotatingLight->SetProperty, Hide=0
    info.fillLight->SetProperty, Hide=0
    info.ambientLight->SetProperty, Hide=0
    info.thisSurface->SetProperty, Color=info.color
    
    ; What style is wanted?
    Widget_Control, event.id, Get_UValue=newStyle
    CASE newStyle OF
    
       'DOTS': info.thisSurface->SetProperty, Style=0
       'MESH': info.thisSurface->SetProperty, Style=1
       'SOLID': info.thisSurface->SetProperty, Style=2, Shading=1
       'XPARALLEL': info.thisSurface->SetProperty, Style=3
       'YPARALLEL': info.thisSurface->SetProperty, Style=4
       'WIRELEGO': info.thisSurface->SetProperty, Style=5
       'SOLIDLEGO': info.thisSurface->SetProperty, Style=6
       'HIDDEN': BEGIN
           Widget_Control, event.id, Get_Value=buttonValue
           IF buttonValue EQ 'Hidden Lines OFF' THEN BEGIN
              setting = 0
              hlvalue = 'Hidden Lines ON'
           ENDIF ELSE BEGIN
              setting = 1
              hlvalue = 'Hidden Lines OFF'
           ENDELSE
           Widget_Control, event.id, Set_Value=hlvalue
           info.thisSurface->SetProperty, Hidden_Lines=setting
           ENDCASE
    
    ENDCASE
    
    ; Redraw the graphic.
    info.thisWindow->Draw, info.thisView
    
    ; Put the info structure back.
    Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


;+
; The cleanup routine for the program. Cleans everything up when the widget dies.
; 
; :Params:
;    tlb: in, required
;       The identifier of the widget that just died.
;-
PRO cgSurface_Cleanup, tlb

    ; Come here when program dies. Free all created objects.
    Widget_Control, tlb, Get_UValue=info
    IF N_Elements(info) NE 0 THEN BEGIN
        Ptr_Free, info.bottomOffPtr
        Obj_Destroy, info.thisContainer
    ENDIF
    
END ;------------------------------------------------------------------------------


;+
; A function for calculating the correct surface aspect ratio. A position
; in the window with this aspect ratio is returned.
; 
; :Returns:
;    A four-element POSITION array, giving the position in the window for
;    a plot with this aspect ratio.
; 
; :Params:
;    aspectRatio: in, optional, type=float, default=1.0
;       The desired aspect ratio of the surface.
;       
; :Keywords:
;    margin: in, optional, type=float, default=0.0
;       The desired margin around the edges of the window.
;    windowaspect: in, optional, type=float
;       The aspect ratio of the window the graphics are to be displayed in.
;-
FUNCTION cgSurface_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

    ; This function calculates the correct aspect ratio for display.
    
    ON_ERROR, 2
    
    ; Check for aspect ratio parameter and possibilities.
    IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0
    
    IF aspectRatio EQ 0 THEN BEGIN
       MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
       aspectRatio = 1.0
    ENDIF
    
    s = SIZE(aspectRatio)
    IF s(s(0)+1) NE 4 THEN $
       MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational
    
    ; Check for margins.
    IF N_ELEMENTS(margin) EQ 0 THEN margin = 0
    
    ; Error checking.
    IF margin LT 0 OR margin GE 0.5 THEN $
       MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'
    
    ; Calculate the aspect ratio of the current window.
    IF N_Elements(wAspectRatio) EQ 0 THEN wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE
    
    ; Calculate normalized positions in window.
    IF (aspectRatio LE wAspectRatio) THEN BEGIN
       xstart = margin
       ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
       xend = 1.0 - margin
       yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
    ENDIF ELSE BEGIN
       xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
       ystart = margin
       xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
       yend = 1.0 - margin
    ENDELSE
    
    ; Return the position in the window.
    RETURN, [xstart, ystart, xend, yend]
        
END ;-----------------------------------------------------------------------------------------


;+
;   The purpose of cgSurface is to create a window where a surface is displayed. Surfaces
;   can be wire-framed, shaded surfaces, and surfaces with texture maps draped on top of
;   them, among other types of surfaces. LEFT mouse button rotates the surface, MIDDLE
;   mouse button zooms out from the surface, RIGHT mouse button zoom into the surface. 
;   Clicking on the surface axes will allow the user to move or translate the surface, and 
;   clicking on the plot title will allow the user to move the title.
;
; :Params:
;    data: in, required, type=any
;         A two-dimensional array of data to be displayed.
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates of the
;         surface grid.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates of the
;         surface grid.
;       
; :Keywords:
;     axiscolor: in, optional, type=string, default='black'
;        The name of the axis color. By default, 'black'..
;     background: in, optional, type=string, default='white'
;        The name of the background color. By default, 'white'.
;     block: in, optional, type=boolean, default=0
;         Set this keyword to make the program a blocking widget program.
;     bottom: in, optional, type=string, default=COLOR
;        The name of the bottom color. By default, same as COLOR.
;     brewer: in, optional, type=boolean, default=0
;        Set this keyword to indicate that the colortable (CTABLE) is
;        to use Brewer color tables rather than IDL standard color tables.
;     charsize: in, optional, type=float, default=1.0
;        The character size of the surface annotation. This value is multiplied
;        times a 12 point character size.
;     color: in, optional, type=string, default='blu6'
;        The name of the data color. 
;     constrain_aspect: in, optional, type=boolean, default=0
;        Set this keyword to maintain the aspect ratio of the Y size
;        of the data to the Y size of the data. The default is to let the
;        sizes conform to a unit square.
;     ctable: in, optional, type=integer
;        The color table to use with the surface. The default is to use the
;        current color table.
;     elevation_shading: in, optional, type=boolean, default=0
;        Set this keyword to put elevation shading into effect for the surface.
;     font: in, optional, type=string, default="Helvetica"
;        The name of the true-type font desired for axis annotation and the plot title. 
;        If undefined, the default is "Helvetica".
;     hidden_lines: in, optional, type=boolean, default=1
;        Set this keyword to turn hidden line removal on for the surface. Turned 
;        ON by default.
;     group_leader: in, optional, type=long
;         Set this keyword to the identifier of a widget that will serve as the
;         group leader for this widget program. When the group leader dies, this
;         program will die, too.
;     reverse: in, optional, type=boolean, default=0
;        Set this keyword to reverse the color table set by CTABLE.
;     shaded: in, optional, type=boolean, default=0
;        Set this keyword if you wish to display a shaded surface. The is the same as setting STYLE=2.
;     skirt: in, optional, type=any
;         Set this keyword to a Z value where a skirt will be drawn for the surface.
;     style: in, optional, type=integer, default=1
;         Sets the style of the surface::
;         
;             0 - Dot surface
;             1 - Wire mesh (the default)
;             2 - Shaded surface
;             3 - Parallel X lines
;             4 - Parallel Y line
;             5 - Wire mesh lego style
;             6 - Solid lego style
;             
;     texture_image: in, optional, type=byte
;         Set this keyword to a 2D or true-color image that will be overlaid on the surface
;         as a texture map. If a 2D image is passed, the colortable specified with CTABLE will
;         be used to construct a true-color image for the texture map.
;     tcharsize: in, optional, type=float
;         The title character size. By default 1.25 times the `Charsize`.
;     tcolor: in, optional, type=string
;         The name of the title color. By default, the same as `AxisColor`.
;     title: in, optional, type=string
;        The title of the plot. It will be written "flat to the screen", rather than rotated.
;     transform: in, optional, type=4x4 double array
;         A homogeneous transformation matrix to be applied to the initial surface. Such a 
;         transformation matrix can be obtained, for example, with the T3D procedure.
;     xoffset: in, optional, type=integer, default=50
;         The number of pixels the surface window should be offset in the X direction
;         from the upper-left corner of the display.
;     xrange: in, optional, type=float
;         The X data range of the data. Normally, just chosen from the data itself.
;     xsize: in, optional, type=interger, default=640
;         The X size of the initial surface window. By default, 640 pixels.
;     xstyle: in, hidden
;         The normal XSTYLE keyword.
;     xtitle: in, optional, type=string
;         The text for the X axis of the surface plot.
;     yoffset: in, optional, type=integer, default=25
;         The number of pixels the surface window should be offset in the Y direction
;         from the upper-left corner of the display.
;     yrange: in, optional, type=float
;         The Y data range of the data. Normally, just chosen from the data itself.
;     ysize: in, optional, type=integer, default=512
;         The Y size of the initial surface window. By default, 640 pixels.
;     ystyle: in, hidden
;         The normal YSTYLE keyword.
;     ytitle: in, optional, type=string
;         The text for the Y axis of the surface plot.
;     zrange: in, optional, type=float
;         The Z data range of the data. Normally, just chosen from the data itself.
;     zscale: in, optional, type=float, default=1.0
;          A number between 0.001 and 1.0 that will "scale" the Z axis height. Default is 1.0.
;     zstyle: in, hidden
;         The normal ZSTYLE keyword.
;     ztitle: in, optional, type=string
;         The text for the Z axis of the surface plot.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDLgrSurface object is allowed in the program.
;-
PRO cgSurface, data, x, y, $
    Axiscolor=axiscolorName, $
    Background=backgroundName, $
    Block=block, $
    Bottom=bottomName, $
    Brewer=brewer, $
    Charsize=charsize, $
    Color=colorName, $
    Constrain_Aspect=constrain_aspect, $
    CTable=colortable, $
    Elevation_Shading=elevation_shading, $
    Font=font, $
    Hidden_Lines=hidden_lines, $
    Group_Leader=groupLeader, $
    Reverse=reverse, $
    Shaded=shaded, $
    Skirt=skirt, $
    Style=style, $
    Texture_Image=texture_image, $
    Title=plotTitleText, $
    TCharsize=tcharsize, $
    TColor=tcolorName, $
    Transform=transform, $
    XOffset=xoffset, $
    XRange=xrange_u, $
    XSize=xsize, $
    XStyle=xstyle, $
    XTitle=xtitleText, $
    YOffset=yoffset, $
    YRange=yrange_u, $
    YSize=ysize, $
    YStyle=ystyle, $
    YTitle=ytitleText, $
    ZRange=zrange_u, $
    ZScale=zscale, $
    ZStyle=zstyle, $
    ZTitle=ztitleText, $
    _Extra=extra

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
  
    ; Did the user pass parameters?
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgSurface, data, x, y'
        RETURN
    ENDIF
    
    ; We can only do this on devices that support windows.
    IF (!D.Flags AND 256) EQ 0 THEN $
        Message, 'This program only works on devices that support windows.'
        
    ; We are going to do this in decomposed color mode.
    SetDecomposedState, 1, CurrentState=currentDecomposedState
      
    ; Check parameters.
    IF N_Elements(data) EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgSurface, data, x, y'
        Print, 'Using example data.'
        data = cgDemoData(2)
    ENDIF
    
    ; Get the current color table vectors. May need them later.
    TVLCT, rr, gg, bb, /GET
    
    ndims = Size(data, /N_DIMENSIONS)
    IF ndims NE 2 THEN Message, 'Data must be 2D.'
    s = Size(data, /DIMENSIONS)
    IF N_Elements(x) EQ 0 THEN x = Findgen(s[0])
    IF N_Elements(y) EQ 0 THEN y = Findgen(s[1])
    
    ; Check keywords.
    IF N_Elements(axisColorName) EQ 0 THEN axisColorName = 'black' 
    IF N_Elements(backgroundName) EQ 0 THEN backgroundName = 'white' 
    IF N_Elements(tcolorName) EQ 0 THEN tcolorName = axisColorName 
    IF N_Elements(colorName) EQ 0 THEN colorName = 'blu6' 
    IF N_Elements(bottomName) EQ 0 THEN bottomName = 'dark gray' 
    IF N_Elements(colortable) EQ 0 THEN BEGIN
        colors = Transpose([[rr],[gg], [bb]])
        colortable = -1
    ENDIF ELSE BEGIN
        cgLoadCT, colortable, Reverse=Keyword_Set(reverse), Brewer=Keyword_Set(brewer), $
            RGB_TABLE=colors, /ROW
    ENDELSE
    
    ; Create a color palette for use later.
    colorPalette = Obj_New("IDLgrPalette", colors[0,*], colors[1,*], colors[2,*])

    block = Keyword_Set(block)
    brewer = Keyword_Set(brewer)
    IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
    constrain_aspect = Keyword_Set(constrain_aspect)
    elevation_shading = Keyword_Set(elevation_shading)
    IF N_Elements(font) EQ 0 THEN font = 'Helvetica'
    IF N_Elements(hidden_lines) EQ 0 THEN hidden_lines = 1
    hidden_lines = Keyword_Set(hidden_lines)
    reverse = Keyword_Set(reverse)
    IF N_Elements(xoffset) EQ 0 THEN xoffset = 50
    IF N_Elements(yoffset) EQ 0 THEN yoffset = 25
    IF Keyword_Set(shaded) THEN style = 2
    IF N_Elements(style) EQ 0 THEN style = 1
    IF N_Elements(tcharsize) EQ 0 THEN tcharsize = 1.25
    IF N_Elements(zscale) EQ 0 THEN zscale = 1.0
    zscale = 0.001 > zscale < 1.0
    shading = 1 ; Always do Gouraud shading when style=2.
    bottomOffPtr = Ptr_New(/ALLOCATE_HEAP) ; Pointer to turn bottom colors off.
    specularColor = [200,200,200]
    IF N_Elements(xstyle) EQ 0 THEN xstyle = 0
    IF N_Elements(ystyle) EQ 0 THEN ystyle = 0
    IF N_Elements(zstyle) EQ 0 THEN zstyle = 0
    IF N_Elements(xsize) EQ 0 THEN xsize = 640
    IF N_Elements(ysize) EQ 0 THEN ysize = 512
    IF N_Elements(xtitleText) EQ 0 THEN xtitleText = ""
    IF N_Elements(ytitleText) EQ 0 THEN ytitleText = ""
    IF N_Elements(ztitleText) EQ 0 THEN ztitleText = ""
    IF N_Elements(plotTitleText) EQ 0 THEN plotTitleText = ""
    
    ; If the colors are strings, they need to be converted to row vectors.
    ; If they are LONGS, they need to be decomposed to a row vector.
    ; If they are NOT longs, they must be indices into the color table.
    IF Size(axiscolorName, /TNAME) EQ 'STRING' THEN axiscolor = cgColor(axiscolorName, /Triple, /Row)
    IF N_Elements(axiscolor) NE 3 THEN BEGIN
        IF Size(axiscolor, /TNAME) EQ 'LONG' THEN BEGIN
            axiscolor = [Byte(axiscolor), Byte(axiscolor,1), Byte(axiscolor,2)]
        ENDIF ELSE BEGIN
            axiscolor = [rr[0>axiscolor<255],gg[0>axiscolor<255],bb[0>axiscolor<255]]
        ENDELSE
    ENDIF
    IF Size(backgroundName, /TNAME) EQ 'STRING' THEN background = cgColor(backgroundName, /Triple, /Row)
    IF N_Elements(background) NE 3 THEN BEGIN
        IF Size(background, /TNAME) EQ 'LONG' THEN BEGIN
            background = [Byte(background), Byte(background,1), Byte(background,2)]
        ENDIF ELSE BEGIN
            background = [rr[0>background<255],gg[0>background<255],bb[0>background<255]]
        ENDELSE
    ENDIF
    IF Size(bottomName, /TNAME) EQ 'STRING' THEN bottom = cgColor(bottomName, /Triple, /Row)
    IF N_Elements(bottom) NE 3 THEN BEGIN
        IF Size(bottom, /TNAME) EQ 'LONG' THEN BEGIN
            bottom = [Byte(bottom), Byte(bottom,1), Byte(bottom,2)]
        ENDIF ELSE BEGIN
            bottom = [rr[0>bottom<255],gg[0>bottom<255],bb[0>bottom<255]]
        ENDELSE
    ENDIF
    IF Size(colorName, /TNAME) EQ 'STRING' THEN color = cgColor(colorName, /Triple, /Row)
    IF N_Elements(color) NE 3 THEN BEGIN
        IF Size(color, /TNAME) EQ 'LONG' THEN BEGIN
            color = [Byte(color), Byte(color,1), Byte(color,2)]
        ENDIF ELSE BEGIN
            color = [rr[0>color<255],gg[0>color<255],bb[0>color<255]]
        ENDELSE
    ENDIF
    IF Size(tcolorName, /TNAME) EQ 'STRING' THEN tcolor = cgColor(tcolorName, /Triple, /Row)
    IF N_Elements(tcolor) NE 3 THEN BEGIN
        IF Size(tcolor, /TNAME) EQ 'LONG' THEN BEGIN
            tcolor = [Byte(tcolor), Byte(tcolor,1), Byte(tcolor,2)]
        ENDIF ELSE BEGIN
            tcolor = [rr[0>tcolor<255],gg[0>tcolor<255],bb[0>tcolor<255]]
        ENDELSE
    ENDIF
    
    ; Should we constrain the aspect ratio of the surface?
    IF constrain_aspect THEN BEGIN
    
       ; I want the surface data to have the same aspect ratio as 
       ; the data itself in the X and Y directions.
       s = Size(data, /DIMENSIONS)
       surfaceAspect = Float(s[1]) / s[0]
       windowAspect = Float(ysize) / xsize
       pos = cgSurface_Aspect(surfaceAspect, WindowAspect=windowAspect, Margin=0)
       pos = [pos[0], pos[2], pos[1], pos[3], 0.0, 1.0] - 0.5
    
    ENDIF ELSE pos = [0, 1, 0, 1, 0, 1] - 0.5
    
    ; Do you have a texture image?
    IF N_Elements(texture_image) NE 0 THEN BEGIN
    
        ; Create a texture image object.
        ndims = Size(texture_image, /N_DIMENSIONS)
        IF ndims LT 2 OR ndims GT 3 THEN Message, 'Texture image must be a 2D or 3D array.'
        type = Size(texture_image, /TYPE)
        IF type GT 1 THEN Message, 'Texture image must be a byte array.'
        IF ndims EQ 2 THEN BEGIN
            textureImage = Obj_New('IDLgrImage', texture_image, PALETTE=colorPalette)
        ENDIF ELSE BEGIN
            textureImage = Obj_New('IDLgrImage', texture_image)
        ENDELSE
        
        ; Set up texture coordinates.
        imageDims = Image_Dimensions(data, XSize=ixsize, YSize=iysize, TrueIndex=trueindex) 
        texcoords = FltArr(2, ixsize, iysize)
        texcoords[0,*,*] = (Findgen(ixsize)#Replicate(1,iysize)) / (ixsize-1)
        texcoords[1,*,*] = (Replicate(1,iysize)#Findgen(ixsize)) / (ixsize-1)
       
        ; Need white surface.
        color = [255, 255, 255]
        
        ; Need shaded surface.
        style = 2
    ENDIF
    
    ; Create a view. The coodinate system is chosen so that (0,0,0) is in the
    ; center of the window. This will make rotations easier.
    IF plotTitleText EQ "" THEN BEGIN
       viewrect = [-1.0,-1.0,1.9,1.9]
    ENDIF ELSE BEGIN
       viewrect = [-1.0,-1.0,1.9,2.0]
    ENDELSE
    thisView = OBJ_NEW('IDLgrView', Color=background, Viewplane_Rect=viewrect)
    
    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.
    thisModel = OBJ_NEW('IDLgrModel')
    thisView->Add, thisModel
    
    ; Create a separate model for the title that doesn't rotate.
    textModel = Obj_New('IDLgrModel')
    thisView->Add, textModel
    
    ; Create helper objects. First, create title objects
    ; for the axes and plot. Color them green.
    xTitle = Obj_New('IDLgrText', xtitleText, Color=axisColor, /Enable_Formatting)
    yTitle = Obj_New('IDLgrText', ytitleText, Color=axisColor, /Enable_Formatting)
    zTitle = Obj_New('IDLgrText', ztitleText, Color=axisColor, /Enable_Formatting)
    
    ; Create font objects.
    axisFont  = Obj_New('IDLgrFont', font, Size=12*charsize)
    titleFont = Obj_New('IDLgrFont', font, Size=12*tcharsize)
    
    ; Create a plot title object. I am going to place the title
    ; centered in X and towards the top of the viewplane rectangle.
    plotTitle = Obj_New('IDLgrText', plotTitleText, Color=tcolor, /Enable_Formatting, $
       Alignment=0.5, Location=[0.0, 0.75, 0.0], Font=titleFont)
    textModel->Add, plotTitle
    
    ; Create a trackball for surface rotations. Center it in
    ; the graphics window. Give it a 300 pixel diameter.
    thisTrackball = OBJ_NEW('Trackball', [xsize/2,ysize/2], 300)
        
    ; Create a surface object. 
    IF elevation_shading THEN BEGIN
        thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
           Color=color, _Strict_Extra=extra, Style=style, $
           Shading=shading, Hidden_Lines=hidden_lines)
    ENDIF ELSE BEGIN
        thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
           Color=color, _Strict_Extra=extra, Style=style, $
           Shading=shading, Hidden_Lines=hidden_lines, BOTTOM=bottom, $
           SPECULAR=specularColor)    
    ENDELSE
    
    ; Do you have a texture image?
    IF N_Elements(texture_image) GT 0 THEN BEGIN
        thisSurface -> SetProperty, Texture_Map=textureImage, Texture_Coord=texcoords
    ENDIF
    
    ; Did you want a skirt?
    IF N_Elements(skirt) NE 0 THEN BEGIN
        thisSurface -> SetProperty, SKIRT=skirt, /SHOW_SKIRT
    ENDIF
    
    ; Get the data ranges of the surface. Use the ranges from the surface,
    ; unless the user specified something else.
    thisSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange
    IF N_Elements(xrange_u) NE 0 THEN xrange = xrange_u
    IF N_Elements(yrange_u) NE 0 THEN yrange = yrange_u
    IF N_Elements(zrange_u) NE 0 THEN zrange = zrange_u
    
    ; Create axes objects for the surface. Axes are created after the surface 
    ; so the range can be set correctly. Note how I set the font.
    xAxis = Obj_New("IDLgrAxis", 0, Color=axisColor, Ticklen=0.1, $
       Minor=4, Title=xtitle, Range=xrange, Exact=(xstyle AND 1), Hide=(xstyle AND 4))
    xAxis->GetProperty, Ticktext=xAxisText
    xAxisText->SetProperty, Font=axisFont
    
    yAxis = Obj_New("IDLgrAxis", 1, Color=axisColor, Ticklen=0.1, $
       Minor=4, Title=ytitle, Range=yrange, Exact=(ystyle AND 1), Hide=(ystyle AND 4))
    yAxis->GetProperty, Ticktext=yAxisText
    yAxisText->SetProperty, Font=axisFont
    
    zAxis = Obj_New("IDLgrAxis", 2, Color=axisColor, Ticklen=0.1, $
       Minor=4, Title=ztitle, Range=zrange, Exact=(zstyle AND 1), Hide=(zstyle AND 4))
    zAxis->GetProperty, Ticktext=zAxisText
    zAxisText->SetProperty, Font=axisFont
    
    ; The axes may not use exact axis scaling, so the ranges may
    ; have changed from what they were originally set to. Get
    ; and update the range variables.
    xAxis->GetProperty, CRange=xrange
    yAxis->GetProperty, CRange=yrange
    zAxis->GetProperty, CRange=zrange
    
    ; If you want elevation shading, have to set the colors up now.
    IF elevation_shading THEN BEGIN
       s = Size(data, /Dimensions)
       thisSurface->SetProperty, Vert_Colors=Reform(BytScl(data, /NAN, Min=Min(zrange), Max=Max(zrange)), $
          s[0]*s[1]), Palette=colorPalette
    ENDIF
    
    ; Set scaling parameters for the surface and axes so that everything
    ; is scaled into the range -0.5 to 0.5. We do this so that when the
    ; surface is rotated we don't have to worry about translations. In
    ; other words, the rotations occur about the point (0,0,0).
    xs = FSC_Normalize(xrange, Position=[pos[0], pos[1]])
    ys = FSC_Normalize(yrange, Position=[pos[2], pos[3]])
    zs = FSC_Normalize(zrange, Position=[pos[4], pos[5]] * zscale)
    
    ; Scale the axes and place them in the coordinate space.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.
    xAxis->SetProperty, Location=[9999.0, pos[2],  pos[4]*zscale], XCoord_Conv=xs
    yAxis->SetProperty, Location=[pos[0], 9999.0,  pos[4]*zscale], YCoord_Conv=ys
    zAxis->SetProperty, Location=[pos[0],  pos[3], 9999.0], ZCoord_Conv=zs
    
    ; Scale the surface.
    thisSurface->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
    
    ; Add the surface and axes objects to the model.
    thisModel->Add, thisSurface
    thisModel->Add, xAxis
    thisModel->Add, yAxis
    thisModel->Add, zAxis
    
    ; Rotate the surface model to the standard surface view or 
    ; apply a transformation matrix, if you have one.
    IF N_Elements(transform) NE 0 THEN BEGIN
      thisModel -> SetProperty, Transform=transform
    ENDIF ELSE BEGIN
      thisModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
      thisModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
      thisModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.
    ENDELSE
    
    ; Create some lights to view the surface. Surfaces will look
    ; best if there is some ambient lighting to illuminate them
    ; uniformly, and some positional lights to give the surface
    ; definition. We will create three positional lights: one,
    ; non-rotating light will provide overhead definition. Two
    ; rotating lights will provide specific surface definition.
    ; Lights should be turned off or hidden if elevation shading
    ; is in effect.
    
    ; First create the ambient light. Don't turn it on too much,
    ; or the surface will appear washed out.
    ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2)
    thisModel->Add, ambientLight
    
    ; Shaded surfaces will not look shaded unless there is a
    ; positional light source to give the surface edges definition.
    ; This light will rotate with the surface.
    rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $
        Location=[xrange[1], yrange[1], 4*zrange[1]], $
        Direction=[xrange[0], yrange[0], zrange[0]])
    thisModel->Add, rotatingLight
    
    ; Create a fill light source so you can see the underside
    ; of the surface. Otherwise, just the top surface will be visible.
    ; This light will also rotate with the surface.
    fillLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $
       Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
       Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])
    thisModel->Add, fillLight
    
    ; Create a non-rotating overhead side light.
    nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.8, $
        Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
        Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])
    nonrotatingModel = Obj_New('IDLgrModel')
    nonrotatingModel->Add, nonrotatingLight
    
    ; Be sure to add the non-rotating model to the view, or it won't be visualized.
    thisView->Add, nonrotatingModel
    
    ; Scale the light sources.
    rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
    fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
    nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
    
    ; Rotate the non-rotating model to the standard surface view or apply
    ; the transformation matrix, if you have one.
    IF N_Elements(transform) NE 0 THEN BEGIN
      nonrotatingModel -> SetProperty, Transform=transform
    ENDIF ELSE BEGIN
      nonrotatingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
      nonrotatingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
      nonrotatingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.
    ENDELSE
    
    ; Check for availability of GIF files.
    thisVersion = Float(!Version.Release)
    IF (thisVersion LT 5.4) OR (thisVersion GT 6.3) THEN haveGif = 1 ELSE haveGIF = 0
    
    ; Create the widgets to view the surface. 
    ; Button events are on to enable trackball movement.
    tlb = Widget_Base(Title='Resizeable Surface Window', Column=1, $
       TLB_Size_Events=1, MBar=menubase, XOFFSET=xoffset, YOFFSET=yoffset)
    
    ; Sigh...Rendering throws a LOT of floating point exception errors, especially
    ; when asking IDL to to the retaining for backing store. You can solve the problem
    ; in one of two ways. Do your own backing store by turning EXPOSE events on, which
    ; has its own problems when working with blocking widgets, or do the rendering
    ; in software. The code exists here for you to choose your own poison. :-(
    ;drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, $
    ;   Event_Pro='cgSurface_Draw_Events', Button_Events=1, Retain=2)
    ;drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, $
    ;   Event_Pro='cgSurface_Draw_Events', Button_Events=1, Expose_Events=1)
    drawID = Widget_Draw(tlb, XSize=xsize, YSize=ysize, Graphics_Level=2, $
       Event_Pro='cgSurface_Draw_Events', Button_Events=1, Retain=1, Renderer=1)
    
    ; Create FILE menu buttons.
    filer = Widget_Button(menubase, Value='File', /Menu)
    
    ; Create OUTPUT menu buttons for formatted output files. Use GIF
    ; files if available.
    output = Widget_Button(filer, Value='Save As...', /Menu)
    button = Widget_Button(output, Value='PostScript File', $
       UValue='PS', Event_Pro='cgSurface_Output')
    button = Widget_Button(output, Value='BMP File', $
       UValue='BMP', Event_Pro='cgSurface_Output')
    IF havegif THEN gif = Widget_Button(output, Value='GIF File', $
       UValue='GIF', Event_Pro='cgSurface_Output')
    button = Widget_Button(output, Value='JPEG File', $
       UValue='JPEG', Event_Pro='cgSurface_Output')
    button = Widget_Button(output, Value='PNG File', $
       UValue='PNG', Event_Pro='cgSurface_Output')
    button = Widget_Button(output, Value='TIFF File', $
       UValue='TIFF', Event_Pro='cgSurface_Output')
    
    quitter = Widget_Button(filer, /Separator, Value='Exit', $
       Event_Pro='cgSurface_Exit')
    
    ; Create STYLE menu buttons for surface style.
    style = Widget_Button(menubase, Value='Style', /Menu)
    dummy = Widget_Button(style, Value='Dot Surface', $
       Event_Pro='cgSurface_Style', UValue='DOTS')
    dummy = Widget_Button(style, Value='Wire Mesh', $
       Event_Pro='cgSurface_Style', UValue='MESH')
    dummy = Widget_Button(style, Value='Solid', $
       Event_Pro='cgSurface_Style', UValue='SOLID')
    dummy = Widget_Button(style, Value='Parallel X Lines', $
       Event_Pro='cgSurface_Style', UValue='XPARALLEL')
    dummy = Widget_Button(style, Value='Parallel Y Lines', $
       Event_Pro='cgSurface_Style', UValue='YPARALLEL')
    dummy = Widget_Button(style, Value='Wire Mesh Lego', $
       Event_Pro='cgSurface_Style', UValue='WIRELEGO')
    dummy = Widget_Button(style, Value='Solid Lego', $
       Event_Pro='cgSurface_Style', UValue='SOLIDLEGO')
    IF hidden_lines THEN hlValue = 'Hidden Lines OFF' ELSE hlValue='Hidden Lines ON'
    dummy = Widget_Button(style, Value=hlvalue, $
       Event_Pro='cgSurface_Style', UValue='HIDDEN', /Separator)
    
    IF elevation_shading THEN BEGIN
       elevationID = Widget_Button(style, Value='Elevation Shading OFF', $
          /Separator, UValue='Elevation Shading ON', $
          Event_Pro='cgSurface_Elevation_Shading')
          thisSurface -> SetProperty, Bottom=bottomOffPtr
    ENDIF ELSE BEGIN
       elevationID = Widget_Button(style, Value='Elevation Shading ON', $
          /Separator, UValue='Elevation Shading OFF', $
          Event_Pro='cgSurface_Elevation_Shading')
    ENDELSE
   
    IF elevation_shading THEN BEGIN
       bottomID = Widget_Button(style, Value='Bottom Color ON', $
          /Separator, UValue='Bottom Color OFF', $
          Event_Pro='cgSurface_Bottom_OnOff')
          thisSurface -> SetProperty, Bottom=bottomOffPtr
    ENDIF ELSE BEGIN
       bottomID = Widget_Button(style, Value='Bottom Color OFF', $
          /Separator, UValue='Bottom Color ON', $
          Event_Pro='cgSurface_Bottom_OnOff')
    ENDELSE
    
    void = Widget_Button(style, Value='Turn Axes OFF', $
          /Separator, UValue='Turn Axes ON', $
          Event_Pro='cgSurface_Axes_OnOff')
    
    IF N_Elements(skirt) GT 0 THEN BEGIN
        skirtID = Widget_Button(style, Value='Turn Skirt OFF', $
          /Separator, UValue='Turn Skirt ON', $
          Event_Pro='cgSurface_Skirt_OnOff')
    ENDIF
    ; Create PROPERTIES menu buttons for surface properties.
    properties = Widget_Button(menubase, Value='Properties', /Menu, $
       Event_Pro='cgSurface_Properties')
    
    ; Background Color
    colorID = Widget_Button(properties, Value='Colors', /Menu, $
        Event_Pro='cgSurface_Change_Colors')
    dummy = Widget_Button(colorID, Value='Surface Color', UValue=colorName)
    dummy = Widget_Button(colorID, Value='Bottom Color', UValue=bottomName)
    dummy = Widget_Button(colorID, Value='Axis Color', UValue=axiscolorName)
    dummy = Widget_Button(colorID, Value='Background Color', UValue=backgroundName)
    dummy = Widget_Button(colorID, Value='Title Color', UValue=tColorName)
    
    colorsID = Widget_Button(colorID, Value='Elevation Color Table', $
       Event_Pro='cgSurface_Elevation_Colors', /Separator)
     
    ; Original Axis rotation.
    dummy = Widget_Button(properties, Value='Original Rotation', /Separator, $
       Event_Pro='cgSurface_Properties', UValue='ORIGINAL_T3D')
    
    ; Drag Quality.
    dragID = Widget_Button(properties, Value='Drag Quality', /Separator, /Menu)
       dragLowID = Widget_Button(dragID, Value='Low', $
          Event_Pro='cgSurface_Properties', UValue='DRAG_LOW')
       dragMedID = Widget_Button(dragID, Value='Medium', $
          Event_Pro='cgSurface_Properties', UValue='DRAG_MEDIUM')
       dragHighID = Widget_Button(dragID, Value='High', $
          Event_Pro='cgSurface_Properties', UValue='DRAG_HIGH')
    Widget_Control, dragHighID, Sensitive=0
    
    ; Light controller.
    lightID = Widget_Button(properties, Value='Light Controls...', $
       /Separator, Event_Pro='cgSurface_Light_Controls')
    
    ; Draw the widgets.
    Widget_Control, tlb, /Realize
    
   ; Get the window destination object. The view will
   ; be drawn when the window is exposed.
    Widget_Control, drawID, Get_Value=thisWindow
    thisWindow -> Draw, thisView
    
    ; Create a container object to hold all the other
    ; objects. This will make it easy to free all the
    ; objects when we are finished with the program.
    thisContainer = Obj_New('IDL_Container')
    
    ; Add created objects to the container.
    thisContainer->Add, thisView
    thisContainer->Add, thisTrackball
    thisContainer->Add, xTitle
    thisContainer->Add, yTitle
    thisContainer->Add, zTitle
    thisContainer->Add, xAxis
    thisContainer->Add, yAxis
    thisContainer->Add, zAxis
    thisContainer->Add, thisSurface
    thisContainer->Add, nonRotatingModel
    thisContainer->Add, thisModel
    thisContainer->Add, plotTitle
    thisContainer->Add, axisFont
    thisContainer->Add, titleFont
    thisContainer->Add, colorPalette
    IF Obj_Valid(textureImage) THEN thisContainer->Add, textureImage
    
    ; Get the current transformation matrix, so it can be restored.
    thisModel->GetProperty, Transform=origTransform
    
    ; Create an INFO structure to hold needed program information.
    info = { origTransform:origTransform, $       ; The transformation matrix.
             thisContainer:thisContainer, $       ; The object container.
             thisWindow:thisWindow, $             ; The window object.
             thisSurface:thisSurface, $           ; The surface object.
             thisTrackball:thisTrackball, $       ; The trackball object.
             thisModel:thisModel, $               ; The model object.
             textModel:textModel, $               ; The model holding the instructions.
             xAxis:xAxis, $                       ; The X Axis object.
             yAxis:yAxis, $                       ; The Y Axis object.
             zAxis:zAxis, $                       ; The Z Axis object.
             xTitle:xTitle, $                     ; The X Title object.
             yTitle:yTitle, $                     ; The Y Title object.
             zTitle:zTitle, $                     ; The Z Title object.
             xsize:xsize, $
             ysize:ysize, $
             xstart:-1, $
             ystart:-1, $
             selectedItem:Obj_New(), $
             specularColor:specularColor, $
             bottomOffPtr:bottomOffPtr, $         ; The pointer to turn the bottom color off.
             nonRotatingLight:nonRotatingLight, $ ; The non-rotating light object.
             rotatingLight:rotatingLight, $       ; The rotating light object.
             fillLight:fillLight, $               ; The fill light object.
             ambientLight:ambientLight, $         ; The ambient light object.
             colorPalette:colorPalette, $         ; The surface color palette.
             colorsID:colorsID, $                 ; The color button for the texture map.
             drawID:drawID, $                     ; The widget identifier of the draw widget.
             colortable:colortable, $             ; The current color table.
             brewer:brewer, $
             reverse:reverse, $
             r:rr, $                               ; The R values of the current color table.
             g:gg, $                               ; The G values of the current color table.
             b:bb, $                               ; The B values of the current color table.
             data:data, $                         ; The original 2D data set.
             elevation_shading:elevation_shading, $               ; An elevation shading flag.
             elevationID:elevationID, $           ; The ID of the Elevation Shading button.
             bottomID:bottomID, $
             lightID:lightID, $                   ; The light control button ID.
             plotTitle:plotTitle, $               ; The plot title object.
             dragLowID:dragLowID, $               ; ID of Drag Quality Low button.
             dragMedID:dragMedID, $               ; ID of Drag Quality Medium button.
             dragHighID:dragHighID, $             ; ID of Drag Quality High button.
             dragQuality:2, $                     ; The current drag quality.
             color:color, $
             axiscolor:axiscolor, $
             background:background, $
             bottom:bottom, $
             thisView:thisView }                  ; The view object.
    
    ; Store the info structure in the UValue of the TLB.
    Widget_Control, tlb, Set_UValue=info, /No_Copy
    
    ; Call XManager. Set a cleanup routine so the objects
    ; can be freed upon exit from this program.
    XManager, 'cgsurface', tlb, Cleanup='cgSurface_Cleanup', $
       No_Block=(1 - Keyword_Set(block)), $
       Event_Handler='cgSurface_Resize', Group_Leader=groupLeader
    
END ;-----------------------------------------------------------------------------------------
   
