;+
; NAME:
;       XSURFACE
;
; PURPOSE:
;
;       The purpose of this program is to demonstrate how to
;       create a surface with axes and a title in the new IDL 5
;       object graphics.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Widgets, Object Graphics.
;
; CALLING SEQUENCE:
;
;       XSURFACE, data, x, y
;
; REQUIRED INPUTS:
;       None. Fake data will be used if no data is supplied in call.
;
; OPTIONAL INPUTS
;
;       data: A 2D array of surface data.
;
;       x: A vector of X data values.
;
;       y: A vector of Y data values.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       COLORTABLE: Set this keyword to a number between 0 and 40 to select one
;       of the pre-selected IDL color tables for elevation shading.
;
;       ELEVATION_SHADING: Set this keyword to put elevation shading into effect.
;
;       EXACT: Set this keyword to a one-, two-,or three-element array to set exact axis
;       scaling for the X, Y, and Z axes, respectively. If Exact is a one-element array,
;       all three axes are set to the same value. For example, to set the X axis to
;       exact scaling and the Y and Z axes to normal scaling, type:
;
;           IDL> FSC_Surface, Exact=[1,0,0]
;
;       _EXTRA: This keyword collects otherwise undefined keywords that are
;        passed to the IDLgrSURFACE initialization routine.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       LANDSCAPE: Set this keyword if you are printing in landscape mode. The
;       default is Portrait mode. The Landscape keyword on the PRINTER object
;       is set, but not all printers will honor this keyword setting. If yours
;       does not, set Landscape mode in the Printer Setup dialog.
;
;       SHADED: Set this keyword to set up a shaded surface plot rather than a wire
;       mesh surface, which is the default.
;
;       TITLE:  A string used as the title of the plot.
;
;       XTITLE: A string used as the X title of the plot.
;
;       YTITLE: A string used as the Y title of the plot.
;
;       ZTITLE: A string used as the Z title of the plot.


; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; DEPENDENCIES:
;       This program requires the following additional files from the Coyote Library:
;
;          error_message.pro
;          fsc_droplist.pro
;          fsc_normalize.pro
;          getcolor.pro
;          loaddata.pro
;          pickcolor.pro
;          xcolors.pro
;
; EXAMPLE:
;       To use this program with your data, type:
;
;        IDL> XSurface, data
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 8 June 97.
;       Made axis scaling more robust. 17 Sept 97. DWF.
;       Minor modifications to incorporate better understanding
;          of how objects work. 4 Oct 97. DWF.
;       Fixed error cleaning up all of my created objects. 12 Feb 98. DWF.
;       Changed IDLgrContainer to IDL_Container to fix 5.1 problems. 20 May 98. DWF.
;       Fixed mis-spelling of HELVETICA14. 29 June 98. DWF.
;       Added the EXACT keyword to the X and Y axes to force axis ranging. 27 July 98. DWF
;       Added the ability to select rendering "drag" quality for faster operation. 22 Aug 98. DWF.
;       Added ability to get non-exact axis scaling. 12 May 99. DWF.
;       Improved documentation and readability of code. 12 May 99. DWF.
;       Added VECTOR and LANDSCAPE keywords and improved printing capability. 16 Feb 2000. DWF.
;       Added different lights and a Light Controller option. 28 April 2000. DWF.
;       Added elevation shading. 8 May 2000. DWF.
;       Removed VECTOR keyword. Replaced with VECTOR/BITMAP/COLOR Print buttons. 8 May 2000. DWF.
;       Added HIDDEN_LINE keyword. 8 May 2000. DWF.
;       Renamed NORMALIZE to FSC_NORMALIZE to avoid numerous naming conflicts. 17 October 2008. DWF.
;       Made the draw widget render in software to eliminate a window read-through problem
;          (http://www.dfanning.com/ographics_tips/snapshot.html). 30 December 2008. DWF.
;       
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
PRO CW_Light_Control_Intensity_Events, event

; Handles selection events from the Intensity Value widget.

   ; Get the info carrier.

parent = Widget_Info(event.handler, /Parent)
infoCarrier = Widget_Info(parent, Find_by_UName='CW_LIGHT_CARRIER')
Widget_Control, infoCarrier, Get_UValue=info, /No_Copy

   ; Get the new intensity value.

info.theIntensity = *event.selection

   ; Change the intensity of the light.

info.theLight->SetProperty, Intensity=info.theIntensity

   ; Prepare to send an event that notifies the Simple_Surface program.

event_pro = info.event_pro
tlb = info.tlb
top = event.top
parent = info.parent
name = info.name
intensity = info.theIntensity
color = info.theColor
hide = info.theHide
Widget_Control, infoCarrier, Set_UValue=info, /No_Copy

IF event_pro NE "" THEN BEGIN
   eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
      NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
   Widget_Control, parent, Send_Event=eventStruct
ENDIF
END ;------------------------------------------------------------------------------



PRO CW_Light_Control_Events, event

   ; Get the info structure.

infoCarrier = Widget_Info(event.handler, Find_By_UName='CW_LIGHT_CARRIER')
Widget_Control, infoCarrier, Get_UValue=info, /No_Copy

   ; What kind of event is this? Branch appropriately.

Widget_Control, event.id, Get_UValue=thisEvent
CASE thisEvent OF

   'COLOR': BEGIN
      TVLCT, info.color, info.index
      DEVICE, Decomposed=0, Get_Decomposed=theDecomposedState
      thisColor = PickColor(Group_Leader=event.top, info.index)
      thisColor = Reform(thisColor, 3, 1)
      info.theLight->SetProperty, Color=thisColor
      info.theColor = thisColor
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

   ; Send an event if requested.

event_pro = info.event_pro
tlb = info.tlb
top = event.top
parent = info.parent
name = info.name
intensity = info.theIntensity
color = info.theColor
hide = info.theHide
Widget_Control, infoCarrier, Set_UValue=info, /No_Copy

IF event_pro NE "" THEN BEGIN
   eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
      NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
   Widget_Control, parent, Send_Event=eventStruct
ENDIF

END ;------------------------------------------------------------------------------



FUNCTION CW_Light_Control, parent, theLight, Name=name, UValue=uvalue, Event_Pro=event_pro, $
   LabelSize=labelsize, Index=index, Color=color

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
colorID = Widget_Button(tlb, Value='Set Color', UValue='COLOR')
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



PRO XSurface_Elevation_Colors, event

; This event handler changes color tables for elevation shading.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF

   "WIDGET_BUTTON": BEGIN
      TVLCT, info.r, info.g, info.b
      XColors, Group_Leader=event.top, NotifyID=[event.id, event.top], $
         Title="XSurface Elevation Shading Colors"
      ENDCASE

   "XCOLORS_LOAD": BEGIN
      info.r = event.r
      info.g = event.g
      info.b = event.b
      info.colortable = event.index
      IF Obj_Valid(info.thisPalette) THEN info.thisPalette->SetProperty, $
         Red=event.r, Green=event.g, Blue=event.b
      ENDCASE

ENDCASE

   ; Draw the graphic display.

info.thisWindow->Draw, info.thisView
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------




PRO XSurface_Elevation_Shading, event

; This event handler sets up elevation shading for the surface.

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=uvalue
Widget_Control, event.id, Set_Value=uvalue, Set_UValue=buttonValue

CASE buttonValue OF
   'Elevation Shading ON': BEGIN
      s = Size(info.data, /Dimensions)
      info.thisSurface->SetProperty, Palette=info.thisPalette, $
         Vert_Colors=Reform(BytScl(info.data), s[0]*s[1])
      Widget_Control, info.colorsID, Sensitive = 1
      ENDCASE

   'Elevation Shading OFF': BEGIN
      info.thisSurface->SetProperty, Palette=Obj_New(), Vert_Colors=0
      Widget_Control, info.colorsID, Sensitive = 0

         ; Make sure lights are turned on.

      info.nonRotatingLight->SetProperty, Hide=0
      info.rotatingLight->SetProperty, Hide=0
      info.fillLight->SetProperty, Hide=0
      info.ambientLight->SetProperty, Hide=0

      ENDCASE

ENDCASE

   ; Draw the graphic display.

info.thisWindow->Draw, info.thisView
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------



FUNCTION XSurface_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

; This function calculates the correct aspect ratios for printing.

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

IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15

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

position = [xstart, ystart, xend, yend]

RETURN, position
END
;-------------------------------------------------------------------------



Pro XSurface_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO XSurface_Draw_Events, event

     ; Draw widget events handled here: expose events and trackball
     ; events. The trackball uses RSI-supplied TRACKBALL_DEFINE.PRO
     ; from the IDL50/examples/object directory.

Widget_Control, event.top, Get_UValue=info, /No_Copy

drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = drawTypes(event.type)

CASE thisEvent OF

   'EXPOSE':  ; Nothing required except to draw the view.
   'PRESS': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
       info.thisWindow->SetProperty, Quality=info.dragQuality ; Drag Quality to Low.
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'RELEASE': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=0 ; Motion events OFF.
       info.thisWindow->SetProperty, Quality=2 ; Drag Quality to High.
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'MOTION': BEGIN ; Trackball events
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   ELSE:

ENDCASE

    ; Draw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XSurface_Style, event

     ; Event handler to select surface style.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Make sure elevation shading is turned off.

info.nonRotatingLight->SetProperty, Hide=0
info.rotatingLight->SetProperty, Hide=0
info.fillLight->SetProperty, Hide=0
info.ambientLight->SetProperty, Hide=0
info.thisSurface->SetProperty, Color=info.surfColor

    ; What style is wanted?

Widget_Control, event.id, Get_UValue=newStyle
CASE newStyle OF

   'DOTS': info.thisSurface->SetProperty, Style=0
   'MESH': info.thisSurface->SetProperty, Style=1
   'SOLID': info.thisSurface->SetProperty, Style=2, Shading=1
   'XPARALLEL': info.thisSurface->SetProperty, Style=3
   'YPARALLEL': info.thisSurface->SetProperty, Style=4
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

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XSurface_Properties, event

     ; Event handler to set graphic properties.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What property is wanted?

Widget_Control, event.id, Get_UValue=newProperty
CASE newProperty OF

   'ORIGINAL_T3D': info.thisModel->SetProperty, Transform=info.origTransform

       ; Surface color.

   'SURFACE_COLOR': BEGIN
      DEVICE, Decomposed=0, Get_Decomposed=theDecomposedState
      TVLCT, Reform(info.surfColor, 1, 3), info.surfIndex
      thisColor = PickColor(Group_Leader=event.top, info.surfIndex, $
         Cancel=cancelled, Title='Pick Surface Color...')
      IF NOT cancelled THEN BEGIn
         thisColor = Reform(thisColor, 3, 1)
         info.thisSurface->SetProperty, Color=thisColor
         info.surfColor = thisColor
      ENDIF
      DEVICE, Decomposed=theDecomposedState
      END

       ; Background color.

   'BBLACK': info.thisView->SetProperty, Color=[0,0,0]
   'BWHITE': info.thisView->SetProperty, Color=[255,255,255]
   'BCHARCOAL': info.thisView->SetProperty, Color=[80,80,80]
   'BGRAY': info.thisView->SetProperty, Color=[135, 135, 135]

       ; Axes colors.

   'ABLACK': BEGIN
      info.xAxis->SetProperty, Color=[0,0,0]
      info.yAxis->SetProperty, Color=[0,0,0]
      info.zAxis->SetProperty, Color=[0,0,0]
      END
   'AWHITE': BEGIN
      info.xAxis->SetProperty, Color=[255,255,255]
      info.yAxis->SetProperty, Color=[255,255,255]
      info.zAxis->SetProperty, Color=[255,255,255]
      END
   'AGREEN': BEGIN
      info.xAxis->SetProperty, Color=[0,255,0]
      info.yAxis->SetProperty, Color=[0,255,0]
      info.zAxis->SetProperty, Color=[0,255,0]
      END
   'AYELLOW': BEGIN
      info.xAxis->SetProperty, Color=[255,255,0]
      info.yAxis->SetProperty, Color=[255,255,0]
      info.zAxis->SetProperty, Color=[255,255,0]
      END
   'ANAVY': BEGIN
      info.xAxis->SetProperty, Color=[0, 0, 115]
      info.yAxis->SetProperty, Color=[0, 0, 115]
      info.zAxis->SetProperty, Color=[0, 0, 115]
      END

       ; Title colors.

   'TBLACK': info.plotTitle->SetProperty, Color=[0,0,0]
   'TWHITE': info.plotTitle->SetProperty, Color=[255,255,255]
   'TGREEN': info.plotTitle->SetProperty, Color=[0,255,0]
   'TYELLOW': info.plotTitle->SetProperty, Color=[255,255,0]
   'TNAVY': info.plotTitle->SetProperty, Color=[0,0,115]

      ; Color schemes.

   'B/W': BEGIN
      info.thisView->SetProperty, Color=[255,255,255]
      info.thisSurface->SetProperty, Color=[0,0,0]
      info.surfColor = [0,0,0]
      info.xAxis->SetProperty, Color=[0,0,0]
      info.yAxis->SetProperty, Color=[0,0,0]
      info.zAxis->SetProperty, Color=[0,0,0]
      info.plotTitle->SetProperty, Color=[0,0,0]
      END
   'W/B': BEGIN
      info.thisView->SetProperty, Color=[0,0,0]
      info.thisSurface->SetProperty, Color=[255,255,255]
      info.surfColor = [255,255,255]
      info.xAxis->SetProperty, Color=[255,255,255]
      info.yAxis->SetProperty, Color=[255,255,255]
      info.zAxis->SetProperty, Color=[255,255,255]
      info.plotTitle->SetProperty, Color=[255,255,255]
      END
   'ORIGINAL_COLORS': BEGIN
      info.thisView->SetProperty, Color=[80,80,80]
      info.thisSurface->SetProperty, Color=[255,255,255]
      info.surfColor = [255,255,0]
      info.xAxis->SetProperty, Color=[0,255,0]
      info.yAxis->SetProperty, Color=[0,255,0]
      info.zAxis->SetProperty, Color=[0,255,0]
      info.plotTitle->SetProperty, Color=[0,255,0]
      END

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
END
;-------------------------------------------------------------------



PRO XSurface_Output, event

   ; This event handler creates GIF and JPEG files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

info.thisWindow->GetProperty, Image_Data=snapshot

   ; JPEG or GIF file wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='idl.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='idl.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------


PRO XSurface_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;------------------------------------------------------------------------



PRO XSurface_Light_Done, event
Widget_Control, event.top, /Destroy
END ;--------------------------------------------------------------------



PRO XSurface_Light_Controls_Event, event
Widget_Control, event.top, Get_UValue=info
info.theWindow->Draw, info.theView
END
;-------------------------------------------------------------------------



PRO XSurface_Light_Controls, event

   ; Place the light control beside the current widget program.

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
xpos = sizes[0] + offsets[0] + 10
ypos = offsets[1] + 100

   ; Lights only make sense with a solid surface.

info.thisSurface->SetProperty, Style=2, Shading=1
info.thisWindow->Draw, info.thisView

   ; Create widgets.

tlb = Widget_Base(Title='XSurface Light Controls', Column=1, Group_Leader=event.top, $
   UValue={theView:info.thisView, theWindow:info.thisWindow}, XOffset=xpos, YOffset=ypos)
dummy = CW_Light_Control(tlb, Name='Non-Rotating Light', info.nonRotatingLight, LabelSize=130, $
   Event_Pro='XSurface_Light_Controls_Event', Index=!D.Table_Size-18, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Rotating Light', info.rotatingLight, LabelSize=130, $
   Event_Pro='XSurface_Light_Controls_Event', Index=!D.Table_Size-19, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Fill Light', info.fillLight, LabelSize=130, $
   Event_Pro='XSurface_Light_Controls_Event', Index=!D.Table_Size-20, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Ambient Light', info.ambientLight, LabelSize=130, $
   Event_Pro='XSurface_Light_Controls_Event', Index=!D.Table_Size-21, Color=[255,255,255])
quit = Widget_Button(tlb, Value='Done', Event_Pro='XSurface_Light_Done')

Widget_Control, tlb, /Realize

XManager, 'XSurface_Light_Controls', tlb, /No_Block, Event_Handler='XSurface_Light_Controls_Event'
Widget_Control, event.top, Set_UValue=info, /No_Copy

END
;-------------------------------------------------------------------------



PRO XSurface_Printing, event

   ; Printer output handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What kind of printing?

Widget_Control, event.id, Get_UValue=printType

   ; Does the user really want to print?

print_it = Dialog_PrinterSetup(info.thisPrinter)
IF NOT print_it THEN BEGIN
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

IF printType NE 'COLOR' THEN BEGIN

   ; Find out the current colors of all the objects.

   info.thisView->GetProperty, Color=backgroundColor
   info.thisSurface->GetProperty, Color=surfaceColor
   info.xAxis->GetProperty, Color=axisColor
   info.yAxis->GetProperty, Color=axisColor
   info.zAxis->GetProperty, Color=axisColor
   info.thisPalette->GetProperty, Red=r, Green=g, Blue=b

   ; Change colors to black and white for printing.

   info.thisView->SetProperty, Color=[255,255,255]
   info.thisSurface->SetProperty, Color=[0,0,0]
   info.xAxis->SetProperty, Color=[0,0,0]
   info.yAxis->SetProperty, Color=[0,0,0]
   info.zAxis->SetProperty, Color=[0,0,0]
   info.plotTitle->SetProperty, Color=[0,0,0]
   info.thisPalette->LoadCT, 0

ENDIF

   ; I want the output on the page to have the same aspect ratio
   ; (ratio of height to width) as I see in the display window.

info.thisWindow->GetProperty, Dimensions=wdims
info.thisPrinter->GetProperty, Dimensions=pdims
plotAspect = Float(wdims[1]) / wdims[0]
windowAspect = Float(pdims[1]) / pdims[0]
position = XSurface_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0)
info.thisView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
   Location=[position[0], position[1]], Units=3

   ; Print it. May take a little time. Alert the user.

Widget_Control, Hourglass=1
IF printType EQ 'VECTOR' THEN BEGIN
   info.thisPrinter->Draw, info.thisView, Vector=1
ENDIF ELSE BEGIN
   info.thisPrinter->Draw, info.thisView
ENDELSE
info.thisPrinter->NewDocument
Widget_Control, Hourglass=0

IF printType NE 'COLOR' THEN BEGIN

   ; Put everything back the way it was.

   info.thisView->SetProperty, Color=backgroundColor, Dimensions=[0,0], Location=[0,0]
   info.thisSurface->SetProperty, Color=surfaceColor
   info.xAxis->SetProperty, Color=axisColor
   info.yAxis->SetProperty, Color=axisColor
   info.zAxis->SetProperty, Color=axisColor
   info.plotTitle->SetProperty, Color=axisColor
   info.thisPalette->SetProperty, Red=r, Green=g, Blue=b

ENDIF

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XSurface_Resize, event

     ; The only events generated by this simple program are resize
     ; events, which are handled here.

     ; Get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget.

info.thisWindow->SetProperty, Dimension=[event.x, event.y]

    ; Redisplay the graphic.

info.thisWindow->Draw, info.thisView

    ; Update the trackball objects location in the center of the
    ; window.

info.thisTrackball->Reset, [event.x/2, event.y/2], $
    (event.y/2) < (event.x/2)

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO XSurface, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Title=plotTitle, $
   Group_Leader=groupLeader, Exact=exact, $
   Landscape=landscape, Elevation_Shading=elevation, $
   Colortable=colortable, Shaded=shaded, Hidden_Lines=hidden_lines

   ; New printer functionality requires IDL 5.3 or higher.

IF Float(!Version.Release) LT 5.3 THEN BEGIN
   ok = Dialog_Message('Program functionality requires IDL 5.3 or higher. Returning...')
   RETURN
ENDIF

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
IF N_Elements(plotTitle) EQ 0 THEN plotTitle=''
IF N_Elements(colortable) EQ 0 THEN colortable = 4 ELSE colortable = 0 > colortable < 40
hidden_lines = Keyword_Set(hidden_lines)
elevation = Keyword_Set(elevation)
landscape = Keyword_Set(landscape)
IF Keyword_Set(shaded) THEN BEGIN
   shading = 1
   style = 2
ENDIF ELSE BEGIN
   shading = 0
   style = 1
ENDELSE
CASE N_Elements(exact) OF
   0: exact = [0,0,0]
   1: exact = Replicate(exact, 3)
   2: exact = [exact, 0]
   3:
   ELSE: BEGIN
      ok = Dialog_Message('Exact keyword contains too many elements. Returning...')
      RETURN
      ENDCASE
ENDCASE
   ; Save color table vectors.

thisDevice = !D.Name
Set_Plot, 'Z'
LoadCT, colortable, /Silent
TVLCT, r, g, b, /Get
Set_Plot, thisDevice

    ; Need some data.

Catch, error
IF error NE 0 THEN BEGIN  ; Can't find LoadData.
   data = DIST(41)
   x = Findgen(41)
   y = Findgen(41)
   IF !Error NE -154 THEN Print, !Err_String ELSE Print, 'Skipping LOADDATA call.'
ENDIF

IF N_Elements(data) EQ 0 THEN BEGIN
   data = LoadData(2)
ENDIF

s = Size(data)

IF s(0) NE 2 THEN Message,'Must pass 2D argument. Using fake data.'
IF N_Elements(x) EQ 0 THEN x = Findgen(s(1))
IF N_Elements(y) EQ 0 THEN y = Findgen(s(2))


Catch, /Cancel

    ; Create a view. Use RGB color. Charcoal background.
    ; The coodinate system is chosen so that (0,0,0) is in the
    ; center of the window. This will make rotations easier.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[-1.2,-1.1,2.3,2.3])

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Create a separate model for the title that doesn't rotate.

textModel = Obj_New('IDLgrModel')
thisView->Add, textModel

    ; Create helper objects. First, create title objects
    ; for the axes and plot. Color them green.

xTitle = Obj_New('IDLgrText', xtitle, Color=[0,255,0])
yTitle = Obj_New('IDLgrText', ytitle, Color=[0,255,0])
zTitle = Obj_New('IDLgrText', ztitle, Color=[0,255,0])

    ; Create font objects.

helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=14)

    ; Create a plot title object. I am going to place the title
    ; centered in X and towards the top of the viewplane rectangle.

plotTitle = Obj_New('IDLgrText', plotTitle, Color=[0,255,0], $
   Alignment=0.5, Location=[0.0, 1.05, 0.0], Font=helvetica14pt)
textModel->Add, plotTitle

    ; Create a trackball for surface rotations. Center it in
    ; the 400-by-400 window. Give it a 200 pixel diameter.

thisTrackball = OBJ_NEW('Trackball', [200, 200], 200)

   ; Create a palette for the surface.

thisPalette = Obj_New("IDLgrPalette")
thisPalette->LoadCT, colortable

    ; Create a surface object. Make it white.

IF elevation THEN BEGIN
   thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
      Color=[255,255,255], _Extra=extra, Style=style, $
      Shading=shading, Hidden_Lines=hidden_lines)
   s = Size(data, /Dimensions)
   thisSurface->SetProperty, Vert_Colors=Reform(BytScl(data), s[0]*s[1]), Palette=thisPalette
ENDIF ELSE BEGIN
   thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
      Color=[255,255,255], _Extra=extra, Style=style, $
      Shading=shading, Hidden_Lines=hidden_lines)
ENDELSE

    ; Get the data ranges of the surface.

thisSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange

    ; Create axes objects for the surface. Color them green.
    ; Axes are created after the surface so the range can be
    ; set correctly. Note how I set the font to 10 pt helvetica.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=xtitle, Range=xrange, Exact=exact[0])
xAxis->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica10pt

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ytitle, Range=yrange, Exact=exact[1])
yAxis->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ztitle, Range=zrange, Exact=exact[2])
zAxis->GetProperty, Ticktext=zAxisText
zAxisText->SetProperty, Font=helvetica10pt

    ; The axes may not use exact axis scaling, so the ranges may
    ; have changed from what they were originally set to. Get
    ; and update the range variables.

xAxis->GetProperty, CRange=xrange
yAxis->GetProperty, CRange=yrange
zAxis->GetProperty, CRange=zrange

    ; Set scaling parameters for the surface and axes so that everything
    ; is scaled into the range -0.5 to 0.5. We do this so that when the
    ; surface is rotated we don't have to worry about translations. In
    ; other words, the rotations occur about the point (0,0,0).

xs = FSC_Normalize(xrange, Position=[-0.5,0.5])
ys = FSC_Normalize(yrange, Position=[-0.5,0.5])
zs = FSC_Normalize(zrange, Position=[-0.5,0.5])

    ; Scale the axes and place them in the coordinate space.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.

xAxis->SetProperty, Location=[9999.0, -0.5, -0.5], XCoord_Conv=xs
yAxis->SetProperty, Location=[-0.5, 9999.0, -0.5], YCoord_Conv=ys
zAxis->SetProperty, Location=[-0.5,  0.5, 9999.0], ZCoord_Conv=zs

    ; Scale the surface.

thisSurface->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Add the surface and axes objects to the model.

thisModel->Add, thisSurface
thisModel->Add, xAxis
thisModel->Add, yAxis
thisModel->Add, zAxis

    ; Rotate the surface model to the standard surface view.

thisModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
thisModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
thisModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

; Create some lights to view the surface. Surfaces will look
; best if there is some ambient lighting to illuminate them
; uniformly, and some positional lights to give the surface
; definition. We will create three positional lights: one,
; non-rotating light will provide overhead definition. Two
; rotating lights will provide specific surface definition.

    ; First create the ambient light. Don't turn it on too much,
    ; or the surface will appear washed out.

ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2, Hide=Keyword_Set(elevation))
thisModel->Add, ambientLight

    ; Create a non-rotating overhead side light.

nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.8, $
    Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
    Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]], $
    Hide=Keyword_Set(elevation))
nonrotatingModel = Obj_New('IDLgrModel')
nonrotatingModel->Add, nonrotatingLight
thisView->Add, nonrotatingModel

    ; Rotate the non-rotating model to the standard surface view.

nonrotatingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
nonrotatingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
nonrotatingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

    ; Shaded surfaces will not look shaded unless there is a
    ; positional light source to give the surface edges definition.

rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $
    Location=[xrange[1], yrange[1], 4*zrange[1]], $
    Direction=[xrange[0], yrange[0], zrange[0]], Hide=Keyword_Set(elevation))
thisModel->Add, rotatingLight

    ; Create a fill light source so you can see the underside
    ; of the surface. Otherwise, just the top surface will be visible.
    ; This light will also rotate with the surface.

fillLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $
   Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
   Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]], $
   Hide=Keyword_Set(elevation))
thisModel->Add, fillLight

    ; Scale the light sources.

rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Resizeable Window Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='XSurface_Draw_Events', Button_Events=1, $
   Renderer=1)

    ; Create FILE menu buttons for printing and exiting.

filer = Widget_Button(menubase, Value='File', /Menu)

   ; Create OUTPUT menu buttons for formatted output files.

output = Widget_Button(filer, Value='Save As...', /Menu)
gif = Widget_Button(output, Value='GIF File', $
   UValue='GIF', Event_Pro='XSurface_Output')
jpeg = Widget_Button(output, Value='JPEG File', $
   UValue='JPEG', Event_Pro='XSurface_Output')

pnt = Widget_Button(filer, Value='Print', /Separator, $
   Event_Pro='XSurface_Printing', /Menu)
dummy = Widget_Button(pnt, Value='Vector Output (faster BW)', UValue='VECTOR')
dummy = Widget_Button(pnt, Value='Bitmap Output (slower BW)', UValue='BITMAP')
dummy = Widget_Button(pnt, Value='Full Color Printing (slower)', UValue='COLOR')
quitter = Widget_Button(filer, /Separator, Value='Exit', $
   Event_Pro='XSurface_Exit')

   ; Create STYLE menu buttons for surface style.

style = Widget_Button(menubase, Value='Style', /Menu)
dummy = Widget_Button(style, Value='Dot Surface', $
   Event_Pro='XSurface_Style', UValue='DOTS')
dummy = Widget_Button(style, Value='Wire Mesh', $
   Event_Pro='XSurface_Style', UValue='MESH')
dummy = Widget_Button(style, Value='Solid', $
   Event_Pro='XSurface_Style', UValue='SOLID')
dummy = Widget_Button(style, Value='Parallel X Lines', $
   Event_Pro='XSurface_Style', UValue='XPARALLEL')
dummy = Widget_Button(style, Value='Parallel Y Lines', $
   Event_Pro='XSurface_Style', UValue='YPARALLEL')
IF hidden_lines THEN hlValue = 'Hidden Lines OFF' ELSE hlValue='Hidden Lines ON'
dummy = Widget_Button(style, Value=hlvalue, $
   Event_Pro='XSurface_Style', UValue='HIDDEN', /Separator)

IF elevation THEN BEGIN
   elevationID = Widget_Button(style, Value='Elevation Shading OFF', $
      /Separator, UValue='Elevation Shading ON', $
      Event_Pro='XSurface_Elevation_Shading')
ENDIF ELSE BEGIN
   elevationID = Widget_Button(style, Value='Elevation Shading ON', $
      /Separator, UValue='Elevation Shading OFF', $
      Event_Pro='XSurface_Elevation_Shading')
ENDELSE
colorsID = Widget_Button(style, Value='Elevation Colors...', $
   Event_Pro='XSurface_Elevation_Colors')
IF elevation EQ 0 THEN Widget_Control, colorsID, Sensitive = 0

   ; Create PROPERTIES menu buttons for surface properties.

properties = Widget_Button(menubase, Value='Properties', /Menu, $
   Event_Pro='XSurface_Properties')

   ; Surface Color

scolorID = Widget_Button(properties, Value='Surface Color...', $
   UVALUE='SURFACE_COLOR')

   ; Background Color

bcolor = Widget_Button(properties, Value='Background Color', /Menu)
dummy = Widget_Button(bcolor, Value='Black', $
   Event_Pro='XSurface_Properties', UValue='BBLACK')
dummy = Widget_Button(bcolor, Value='White', $
   Event_Pro='XSurface_Properties', UValue='BWHITE')
dummy = Widget_Button(bcolor, Value='Charcoal', $
   Event_Pro='XSurface_Properties', UValue='BCHARCOAL')
dummy = Widget_Button(bcolor, Value='Gray', $
   Event_Pro='XSurface_Properties', UValue='BGRAY')

   ; Axes Color

acolor = Widget_Button(properties, Value='Axes Color', /Menu)
dummy = Widget_Button(acolor, Value='Black', $
   Event_Pro='XSurface_Properties', UValue='ABLACK')
dummy = Widget_Button(acolor, Value='White', $
   Event_Pro='XSurface_Properties', UValue='AWHITE')
dummy = Widget_Button(acolor, Value='Yellow', $
   Event_Pro='XSurface_Properties', UValue='AYELLOW')
dummy = Widget_Button(acolor, Value='Green', $
   Event_Pro='XSurface_Properties', UValue='AGREEN')
dummy = Widget_Button(acolor, Value='Navy Blue', $
   Event_Pro='XSurface_Properties', UValue='ANAVY')

   ; Title Color

tcolor = Widget_Button(properties, Value='Title Color', /Menu)
dummy = Widget_Button(tcolor, Value='Black', $
   Event_Pro='XSurface_Properties', UValue='TBLACK')
dummy = Widget_Button(tcolor, Value='White', $
   Event_Pro='XSurface_Properties', UValue='TWHITE')
dummy = Widget_Button(tcolor, Value='Yellow', $
   Event_Pro='XSurface_Properties', UValue='TYELLOW')
dummy = Widget_Button(tcolor, Value='Green', $
   Event_Pro='XSurface_Properties', UValue='TGREEN')
dummy = Widget_Button(tcolor, Value='Navy Blue', $
   Event_Pro='XSurface_Properties', UValue='TNAVY')

   ; Color Schemes.

dummy = Widget_Button(properties, Value='Black on White', /Separator, $
   Event_Pro='XSurface_Properties', UValue='B/W')
dummy = Widget_Button(properties, Value='White on Black', $
   Event_Pro='XSurface_Properties', UValue='W/B')
dummy = Widget_Button(properties, Value='Original Colors', $
   Event_Pro='XSurface_Properties', UValue='ORIGINAL_COLORS')

   ; Original Axis rotation.

dummy = Widget_Button(properties, Value='Original Rotation', /Separator, $
   Event_Pro='XSurface_Properties', UValue='ORIGINAL_T3D')

   ; Drag Quality.

dragID = Widget_Button(properties, Value='Drag Quality', /Separator, /Menu)
   dragLowID = Widget_Button(dragID, Value='Low', $
      Event_Pro='XSurface_Properties', UValue='DRAG_LOW')
   dragMedID = Widget_Button(dragID, Value='Medium', $
      Event_Pro='XSurface_Properties', UValue='DRAG_MEDIUM')
   dragHighID = Widget_Button(dragID, Value='High', $
      Event_Pro='XSurface_Properties', UValue='DRAG_HIGH')
Widget_Control, dragHighID, Sensitive=0

lightID = Widget_Button(properties, Value='Light Controls...', $
   /Separator, Event_Pro='XSurface_Light_Controls')

Widget_Control, tlb, /Realize

    ; Get the window destination object. The view will
    ; be drawn when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter', Landscape=landscape)

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')

   ; Add created objects to the container.

thisContainer->Add, thisView
thisContainer->Add, thisPrinter
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
thisContainer->Add, helvetica10pt
thisContainer->Add, helvetica14pt
thisContainer->Add, thisPalette

   ; Get the current transformation matrix, so it can be restored.

thisModel->GetProperty, Transform=origTransform

   ; Create an INFO structure to hold needed program information.

info = { origTransform:origTransform, $       ; The transformation matrix.
         thisContainer:thisContainer, $       ; The object container.
         thisWindow:thisWindow, $             ; The window object.
         thisPrinter:thisPrinter, $           ; The printer object.
         thisSurface:thisSurface, $           ; The surface object.
         thisTrackball:thisTrackball, $       ; The trackball object.
         thisModel:thisModel, $               ; The model object.
         textModel:textModel, $               ; The model holding the instructions.
         xAxis:xAxis, $                       ; The X Axis object.
         yAxis:yAxis, $                       ; The Y Axis object.
         zAxis:zAxis, $                       ; The Z Axis object.
         nonRotatingLight:nonRotatingLight, $ ; The non-rotating light object.
         rotatingLight:rotatingLight, $       ; The rotating light object.
         fillLight:fillLight, $               ; The fill light object.
         ambientLight:ambientLight, $         ; The ambient light object.
         thisPalette:thisPalette, $           ; The surface color palette.
         colorsID:colorsID, $                 ; The color button for the texture map.
         colortable:colortable, $             ; The current color table.
         r:r, $                               ; The R values of the current color table.
         g:g, $                               ; The G values of the current color table.
         b:b, $                               ; The B values of the current color table.
         data:data, $                         ; The original 2D data set.
         elevation:elevation, $               ; An elevation shading flag.
         elevationID:elevationID, $           ; The ID of the Elevation Shading button.
         scolorID:scolorID, $                 ; The surface color button ID.
         lightID:lightID, $                   ; The light control button ID.
         plotTitle:plotTitle, $               ; The plot title object.
         dragLowID:dragLowID, $               ; ID of Drag Quality Low button.
         dragMedID:dragMedID, $               ; ID of Drag Quality Medium button.
         dragHighID:dragHighID, $             ; ID of Drag Quality High button.
         dragQuality:2, $                     ; The current drag quality.
         surfIndex:!D.Table_Size-22, $        ; The surface color index.
         surfColor:[255,255,255], $           ; The surface color.
         landscape:landscape, $               ; Flag to indicate landscape printing.
         thisView:thisView }                  ; The view object.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'xsurface', tlb, Cleanup='XSurface_Cleanup', /No_Block, $
   Event_Handler='XSurface_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------