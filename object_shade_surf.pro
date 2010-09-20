
;+
; NAME:
;       OBJECT_SHADE_SURF
;
; PURPOSE:
;
;       This program shows you the correct way to write an
;       elevation-shaded surface in object graphics. This would
;       be the equivalent of these direct graphics commands:
;
;           Surface, data, Shades=BytScl(data)
;           Shade_Surf, data, Shades=BytScl(data)
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       2642 Bradbury Court
;       Fort Collins, CO 80521 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Object Graphics
;
; CALLING SEQUENCE:
;       OBJECT_SHADE_SURF, data, x, y
;
; INPUTS:
;       data: The 2D surface data.
;       x:    A vector of X values, corresponding to the X values of data.
;       y:    A vector of Y values, corresponding to the Y values of data.
;
; KEYWORD PARAMETERS:
;       STYLE: Set equal to 1 for a wire-frame surface. Set equal to 2 for
;       a solid surface (the default).
;
; COMMON BLOCKS:
;       None.
;
; EXAMPLE:
;       OBJECT_SHADE_SURF
;
; MODIFICATION HISTORY:
;       Written by:  David Fanning, November 1998.
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

Pro Object_Shade_Surf_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Object_Shade_Surf_Draw_Events, event

     ; Draw widget events handled here: expose events and trackball
     ; events. The trackball uses RSI-supplied TRACKBALL_DEFINE.PRO
     ; from the IDL50/examples/object directory.

Widget_Control, event.top, Get_UValue=info, /No_Copy

drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = drawTypes(event.type)
dragQuality = 0
CASE thisEvent OF

   'EXPOSE':  ; Nothing required except to draw the view.
   'PRESS': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
       info.thisWindow->SetProperty, Quality=dragQuality ; Drag Quality to Low.
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




PRO Object_Shade_Surf_Resize, event

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



PRO Object_Shade_Surf_Style, event

     ; Change the style of the surface

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value=thisButton

CASE thisButton OF

   'Wire Frame':  info.thisSurface->SetProperty, Style=1
   'Solid': info.thisSurface->SetProperty, Style=2

ENDCASE

    ; Draw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------




PRO Object_Shade_Surf_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------




PRO Object_Shade_Surf, data, x, y, Style=style

    ; Check for parameters.

IF N_Elements(data) EQ 0 THEN data = Dist(40)
s = Size(data, /Dimensions)
IF N_Elements(x) EQ 0 THEN x = Findgen(s[0])
IF N_Elements(y) EQ 0 THEN y = Findgen(s[1])
IF N_Elements(style) EQ 0 THEN style = 2

    ; Create a view. Use RGB color. Charcoal background.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[-1.0,-1.0,2.0,2.0])

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Create an surface object shaded by elevation. Use Color Table 5.
    ; Turn Gouraud shading on, but DON'T use a light source, as this will
    ; modulate the shading parameters. Notice that adding a Palette object
    ; to a surface object is NOT documented.

numVerts = s[0]*s[1]
thisPalette=Obj_New('IDLgrPalette')
thisPalette->LoadCT, 5

thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, Style=style, Shading=1, $
   Vert_Colors=Reform(BytScl(data), numVerts), Palette=thisPalette)

    ; Create axes objects for the surface. Color them green.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, /Exact)

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, /Exact)

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
   Minor=4)

    ; Add the surface and axes objects to the model.

thisModel->Add, thisSurface
thisModel->Add, xAxis
thisModel->Add, yAxis
thisModel->Add, zAxis

    ; Create a trackball for surface rotations. Center it in
    ; the window.

thisTrackball = OBJ_NEW('Trackball', [200, 200], 200)

    ; Get the data ranges for the surface.

thisSurface->GetProperty,XRange=xrange,YRange=yrange,ZRange=zrange

    ; Set scaling parameters for the surface and axes so that everything
    ; is scaled into the range -0.5 to 0.5. We do this so that when the
    ; surface is rotated we don't have to worry about translations. In
    ; other words, the rotations occur about the point (0,0,0).

xs = FSC_Normalize(xrange, Position=[-0.5,0.5])
ys = FSC_Normalize(yrange, Position=[-0.5,0.5])
zs = FSC_Normalize(zrange, Position=[-0.5,0.5])

    ; Set the range, location, and scaling factors for the axes.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.

xAxis->SetProperty, Range=xrange, Location=[9999.0, -0.5, -0.5], $
    XCoord_Conv=xs
yAxis->SetProperty, Range=yrange, Location=[-0.5, 9999.0, -0.5], $
    YCoord_Conv=ys
zAxis->SetProperty, Range=zrange, Location=[-0.5, 0.5, 9999.0], $
    ZCoord_Conv=zs

    ; Scale the surface.

thisSurface->SetProperty,XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Rotate the surface model to the standard surface view.

thisModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
thisModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
thisModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Elevation-Shaded Surface: Objects', Column=1, $
   MBar=menubase, TLB_Size_Events=1)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Object_Shade_Surf_Draw_Events', Button_Events=1)

    ; Create FILE menu.

filer = Widget_Button(menubase, Value='File', /Menu)
quitter = Widget_Button(filer, Value='Exit', $
   Event_Pro='Object_Shade_Surf_Exit')

    ; Create STYLE menu.

styleID = Widget_Button(menubase, Value='Style', /Menu, $
   Event_Pro='Object_Shade_Surf_Style')
button = Widget_Button(styleID, Value='Wire Frame')
button = Widget_Button(styleID, Value='Solid')

Widget_Control, tlb, /Realize

    ; Get the window destination object. The view will
    ; be drawn when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')

   ; Add created objects to the container.

thisContainer->Add, thisView
thisContainer->Add, thisTrackball
thisContainer->Add, xAxis
thisContainer->Add, yAxis
thisContainer->Add, zAxis
thisContainer->Add, thisSurface
thisContainer->Add, thisModel
thisContainer->Add, thisPalette

   ; Get the current transformation matrix, so it can be restored.

thisModel->GetProperty, Transform=origTransform

   ; Create an INFO structure to hold needed program information.

info = { origTransform:origTransform, $ ; The transformation matrix.
         thisContainer:thisContainer, $ ; The object container.
         thisWindow:thisWindow, $       ; The window object.
         thisSurface:thisSurface, $     ; The surface object.
         thisTrackball:thisTrackball, $ ; The trackball object.
         thisModel:thisModel, $         ; The model object.
         xAxis:xAxis, $                 ; The X Axis object.
         yAxis:yAxis, $                 ; The Y Axis object.
         zAxis:zAxis, $                 ; The Z Axis object.
         thisView:thisView }            ; The view object.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'Object_Shade_Surf', tlb, $
   Cleanup='Object_Shade_Surf_Cleanup', $
   Group_Leader=groupLeader, /No_Block, $
   Event_Handler='Object_Shade_Surf_Resize'

END
;-------------------------------------------------------------------