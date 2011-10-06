;+
; NAME:
;       SELECT_OBJECTS
;
; PURPOSE:
;       The purpose of this program is to demonstrate how to select
;       and move objects in an object graphics window. Once the objects
;       appear in the window, use your mouse to select the objects and
;       move them in the window. The window is resizeable.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;       Object Graphics.
;
; CALLING SEQUENCE:
;       SELECT_OBJECTS
;
; REQUIRED INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       Requires VCOLORBAR from the Coyote Library:
;           http://www.idlcoyote.com/programs/vcolorbar__define.pro.
;
; EXAMPLE:
;       Select_Objects
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 21 September 98.
;       Added the ability to shrink and expand the objects. 27 Sept 98. DWF.
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
PRO Select_Button_Events, event

; This event handler responds to draw widget events.

   ; Get the info structure.

Widget_Control, event.top, Get_UValue=info

   ; What kind of an event is this?

possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE']
possibleButtons = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
thisEvent = possibleEvents(event.type)
thisButton = possibleButtons(event.press)

   ; Do the right thing.

CASE thisEvent OF

   'EXPOSE': info.thisWindow->Draw, info.thisView

   'DOWN': BEGIN

         ; Has the user selected an object?

      item = info.thisWindow->Select(info.thisView, [event.x, event.y])

         ; Get the first (closest) object in the list of selected objects.

      IF Obj_Valid(item[0]) THEN BEGIN

         item = item[0]

         CASE thisButton OF

               ; Move the selected object.

            'LEFT': BEGIN

               ; Turn motion events ON. Record starting coordinates.

            Widget_Control, info.drawID, Draw_Motion_Events=1
            info.xstart = event.x
            info.ystart = event.y

               ; Save the selected item. Get its name. Store it.

            info.selectedItem = item
            info.selectedItem->GetProperty, Name=selectedName
            info.selectedName = selectedName

            Print, 'Object Selected: ', selectedName
            ENDCASE

               ; Shrink the selected object.

            'RIGHT': BEGIN
               item->Scale, 0.95, 0.95, 1.00
               info.thisWindow->Draw, info.thisView
               ENDCASE

               ; Expand the selected object.

            'MIDDLE': BEGIN
               item->Scale, 1.05, 1.05, 1.00
               info.thisWindow->Draw, info.thisView
               ENDCASE

            ENDCASE
      ENDIF ELSE Print, 'No Object Selected.'
         ENDCASE

   'UP': BEGIN

         ; Turn motion events OFF. Reinitialize selected object fields.

      Widget_Control, info.drawID, Draw_Motion_Events=0, /Clear_Events
      info.selectedName = ''
      info.selectedItem = Obj_New()
      END

   'MOTION': BEGIN

            ; Calculate movement in normalized coordinates. Update start coords.

         deltax = (event.x - info.xstart) / Float(info.xsize)
         deltay = (event.y - info.ystart) / Float(info.ysize)
         info.xstart = event.x
         info.ystart = event.y

            ; Translate the selected model.

         info.selectedItem->Translate, deltax, deltay, 0

            ; Draw the view.

         info.thisWindow->Draw, info.thisView

      END

  ELSE:

ENDCASE

   ; Store info structure.

Widget_Control, event.top, Set_UValue=info
END
;------------------------------------------------------------------------



PRO Select_Cleanup, tlb

; This procedure cleans up all the persistent objects.

Widget_Control, tlb, Get_UValue=info
Obj_Destroy, info.thisContainer
END
;------------------------------------------------------------------------



PRO Select_Resize, event

; This procedure resizes the graphics window.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Resize the draw widget.

Widget_Control, info.drawID, Draw_XSize=event.x, Draw_YSize=event.y

   ; Update the size fields in the info structure.

info.xsize = event.x
info.ysize = event.y

   ; Redisplay the graphic.

info.thisWindow->Draw, info.thisView

Widget_Control, event.top, Set_UValue=info, /No_Copy

END
;------------------------------------------------------------------------



PRO Select_Objects

; This is an example program that shows you how to select
; and move objects in an object graphics window.

   ; Load an image data file.

filename = FilePath(SubDirectory=['examples', 'data'], 'worldelv.dat')
image = BytArr(360,360)
OpenR, lun, filename, /Get_Lun
ReadU, lun, image
Free_Lun, lun

   ; Create an object graphics display window.

tlb = Widget_Base(Title='Object Selection Example', TLB_Size_Events=1, $
   Base_Align_Center=1, Column=1)
drawID = Widget_Draw(tlb, Button_Events=1, Expose_Events=1, $
   Retain=0, Graphics_Level=2, XSize=400, YSize=400, $
   Event_Pro='Select_Button_Events')
labelID = Widget_Label(tlb, $
   Value='Left Button: Drag.  Middle Button: Expand.  Right Button: Contract.')

   ; Realize the widgets. Get window object.

Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=thisWindow

   ; Create the coordinate system for the window.

thisView = Obj_New('IDLgrView', Viewplane_Rect=[0,0,1,1], Color=[100,100,100])

   ; Create a color palette for the image and colorbar.

thisPalette = Obj_New('IDLgrPalette')
thisPalette->LoadCT, 5

   ; Create objects to be selected and moved in the window.

thisImage = Obj_New('IDLgrImage', image, Palette=thisPalette)
thisImage->GetProperty, XRange=xrange, YRange=yrange
xs = FSC_Normalize(xrange, Position=[0.2, 0.9])
ys = FSC_Normalize(yrange, Position=[0.1, 0.8])
thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

thisColorbar = Obj_New('VColorbar', Palette=thisPalette, $
   Position=[0.10, 0.10, 0.15, 0.80], Color=[255,255,255], $
   Title='Color Range')

thisTitle = Obj_New('IDLgrText', 'World Elevation Data Set', $
   Location=[0.5, 0.9], Alignment=0.5, Color=[255,255,255])

   ; The easiest way to move objects in a window is to put
   ; each object to be moved in its own model. The models
   ; are easy to translate and move.

   ; Create a model for each of the objects to be moved.
   ; Make each model a selection target.

imageModel = Obj_New('IDLgrModel', Name='IMAGE', Select_Target=1)
   imageModel->Add, thisImage
colorbarModel = Obj_New('IDLgrModel', Name='COLORBAR', Select_Target=1)
   colorbarModel->Add, thisColorbar
titleModel = Obj_New('IDLgrModel', Name='TITLE', Select_Target=1)
   titleModel->Add, thisTitle

   ; Add the models to the view.

thisView->Add, imageModel
thisView->Add, colorbarModel
thisView->Add, titleModel

   ; Create a container for later cleanup.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisView
thisContainer->Add, thisWindow
thisContainer->Add, thisPalette
thisContainer->Add, imageModel
thisContainer->Add, colorbarModel
thisContainer->Add, titleModel

   ; Select an ARROW cursor.

thisWindow->SetCurrentCursor, 'Arrow'

   ; Create info structure to hold program information.

info = { thisContainer:thisContainer, $    ; The graphics container.
         thisWindow:thisWindow, $          ; The window object.
         thisView:thisView, $              ; The view to be displayed in the window.
         xstart:0, $                       ; The starting x coordinate in a drag action.
         ystart:0, $                       ; The starting y coordinate in a drag action.
         drawID:drawID, $                  ; The draw widget ID.
         xsize:400, $                      ; The current X size of the draw widget.
         ysize:400, $                      ; The current Y size of the draw widget.
         selectedItem:Obj_New(),$          ; The selected object. Currently NULL.
         selectedName:'' $                 ; The name of the selected object.
       }

   ; Store the info structure in the UVALUE of the TLB.

Widget_Control, tlb, Set_UValue=info
XManager, 'select', tlb, Cleanup='Select_Cleanup', /No_Block, $
   Event_Handler='Select_Resize'
END
