;+
; NAME:
;       OWINDOW
;
; PURPOSE:
;       The purpose of this program is to create an object window.
;       I use it mostly when I am creating and testing object graphics
;       programs, but it is also a nice template for larger object
;       graphics programs. The window is resizeable and it destroys
;       its objects when it is destroyed.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Widgets, IDL 5 Object Graphics.
;
; CALLING SEQUENCE:
;       thisWindow = OWindow(thisView)
;
; REQUIRED INPUTS:
;       None. A default view object is created with a gray background and
;          a viewplane rectangle defined as [0,0,1,1].
;
; OPTIONAL INPUTS:
;
;       thisView: A view or scene object that you wish to be displayed
;          in the window.
;
; RETURN VALUE:
;       thisWindow: The window graphics object associated with this window.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;          is destroyed, this program will be destroyed.
;
;       TITLE: A string used as the title of the graphics window.
;
;       XSIZE: The X Size of the graphics window in device coordinates. The
;          default value is 300.
;
;       YSIZE: The Y Size of the graphics window in device coordinates. The
;          default value is 300.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;       To display a view object in this window, type:
;
;          IDL> thisWindow = OWindow(thisView)
;
;       Later, after you have modified the view object, you can type:
;
;          IDL> thisWindow->Draw, modifiedView
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 19 June 97.
;       Set RETAIN=1 on draw widget. 6 Oct 97. DWF.
;       Changed discredited IDLgrContainer to IDL_Container. 29 JUN 98. DWF.
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


PRO OWindow_Cleanup, tlb

    ; This is the clean up routine called when the TLB dies.

Widget_Control, tlb, Get_UValue=info, /No_Copy
IF N_Elements(info) GT 0 THEN Obj_Destroy, info.thisContainer
END
;------------------------------------------------------------------------



PRO OWindow_Quit, event

    ; Quit the program via the QUIT button.

Widget_Control, event.top, /Destroy
END
;------------------------------------------------------------------------



PRO OWindow_Expose_Events, event

    ; Handle window expose events here.

Widget_Control, event.top, Get_UValue=info, /No_Copy
info.thisWindow->Draw, info.thisView
Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;------------------------------------------------------------------------



PRO OWindow_Event, event

    ; Handle window resize events here.

Widget_Control, event.top, Get_UValue=info, /No_Copy
info.thisWindow->SetProperty, Dimension=[event.x, event.y]
info.thisWindow->Draw, info.thisView
Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------------



FUNCTION OWindow, thisView, XSize=xsize, YSize=ysize, Title=title, $
    Group_Leader=group

    ; Check defaults. Define values if necessary.

IF N_Params() EQ 0 THEN thisView = Obj_New('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[0,0,1,1])
IF N_Elements(xsize) EQ 0 THEN xsize = 400
IF N_Elements(ysize) EQ 0 THEN ysize = 400
IF N_Elements(title) EQ 0 THEN title = ''

    ; Creat the widgets for this program.

tlb = Widget_Base(Column=1, Title=title, TLB_Size_Events=1, $
    MBar=menubase)
drawID = Widget_Draw(tlb, XSize=xsize, YSize=ysize, Graphics_Level=2, $
   Expose_Events=1, Event_Pro='OWindow_Expose_Events', Retain=0)

filer = Widget_Button(menubase, Value='File')
quitter = Widget_Button(filer, Value='Quit', Event_Pro='OWindow_Quit')

    ; Realize the widgets. Get the object window.

Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=thisWindow

    ; Store objects in a container for easy clean up.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisWindow
thisContainer->Add, thisView

    ; Create info structure for program information.

info = { thisWindow:thisWindow, $
         thisContainer:thisContainer, $
         thisView:thisView }

Widget_Control, tlb, Set_UValue=info, /No_Copy

XManager, 'owindow', tlb, /No_Block, Cleanup='OWindow_Cleanup', $
    Group_Leader=group

    ; Return the window object so you can put something else
    ; in this window if you like.

RETURN, thisWindow
END