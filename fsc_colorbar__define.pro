;+
; NAME:
;       FSC_COLORBAR__DEFINE
;       Note: The name of the routine has been changed from COLORBAR__DEFINE
;       on 25 Sept 2010 to avoid conflicts with an IDL 8.0 routine of the
;       same name. See the article "IDL 8 Name Conflicts" here:
;       
;           http://www.idlcoyote.com/ng_tips/idl8_name_conflicts.html
;
; PURPOSE:
;       The purpose of this routine is to implement a FSC_COLORBAR object
;       class. The ColorBar is rendered in the direct graphics system.
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
;       Graphics.
;
; CALLING SEQUENCE:
;       colorbar = Obj_New("FSC_COLORBAR")
;
; INPUTS:
;       All inputs to the program are via keyword parameters.
;
; KEYWORD PARAMETERS:
;
;   Background: Background color. This is the color with which the colorbar is
;               erased. The default color is !P.Background.
;   Bottom: Bottom color index of colors allocated to colorbar.
;   Charsize: Character size of annotation. Default is 1.0.
;   Color: Color of annotation and outline. Default is !P.Color.
;   Font: Font to use for annotation. Default is -1, Hershey fonts.
;   Format: Format of annotation. Default is "(F8.2)".
;   Major: The number of major tick intervals. Default is 5.
;   Minor: The number of minor tick intervals. Default is 2.
;   MinusOne: Set this keyword to choose MinusOne keyword on the Congrid command
;               that resizes the colorbar into the window.
;   NColors: The number of colors allocated to colorbar. Default is (256 <
;            !D.N_Colors).
;   Neighbor: Set to indicate Nearest Neighbor sampling for Congrid. Default is
;             0 (Bilinear).
;   Position: The position of colorbar in normalized coordinates. Default for a
;             horizontal colorbar is [0.15, 0.88, 0.85, 0.95]. Default for a
;             vertical colorbar is [0.88, 0.15, 0.95, 0.85]. These defaults are
;             designed for a 400 by 400 window.
;   Range: The data range on colorbar. Default is [0, 255].
;   TickLen: The length of tick marks. Default is -0.1
;   TickV:   Locations for the tick marks in data units. This is the same as
;            the [XY]TickV keyword. Default is to do what IDL would do
;            normally.
;   Vertical: Set this keyword if you want a vertical colorbar. Default is
;             horizontal.
;   XEraseBox: A five-element vector of X points (normalized) for erasing the
;              colorbar plot. Normally this keyword will not have to be used.
;              The program uses the plot REGION for erasing. But larger
;              character sizes can result in annotation going outside the
;              region enclosed by the plot. If that is the case, then use this
;              keyword along with YEraseBox to specify a larger-than-normal
;              erasure area. The points are sent to the POLYFILL command for
;              erasing.
;
;                 POLYFILL, xEraseBox, yEraseBox, /Normal, Color=background
;
;   YEraseBox: A five-element vector of Y points (normalized) for erasing the
;              colorbar plot.
;
; OBJECT METHODS:
;
;   Clamp: This procedure method allows the color bar range to be "clamped"
;          to a particular data range.
;
;   Draw: This procedure method draws the colorbar in the display window. The
;         ERASE keyword to this method will erase the current colorbar (by
;         calling the ERASE method) before drawing the colorbar in the display
;         window.
;
;               colorbar->Draw
;
;   Erase: This procedure method erases the colorbar object in the window. It
;          accomplishes this by performing a POLYFILL in the background color.
;          This method is primarily useful for interactive graphics display
;          devices.
;               colorbar->Erase
;
;   GetProperty: This procedure method allows one to obtain the current state
;                of the object via the keyword parameters listed above.
;
;               colorbar->GetProperty, Range=currentRange, Title=currentTitle
;               Print, currentRange, currentTitle
;
;   SetProperty: This procedure method allows one to set the properties of the
;                colorbar object via the keywords described above. In addition,
;                a DRAW and ERASE keyword are provided so that the colorbar can
;                be immediately drawn when the new property is set.
;
;               colorbar->SetProperty, Range=[500, 15000], /Erase, /Draw
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       The display window is not erased first.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;       To create a colorbar, use it, then destroy it, type:
;
;       colorbar = Obj_New("FSC_COLORBAR", Title='Colorbar Values', Range=[0,1000],$
;                  Format='(I4)')
;       Window
;       LoadCT, 5
;       colorbar->Draw
;       colorbar->SetProperty, Range=[0,500], /Erase, /Draw
;       Obj_Destroy, colorbar
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, Fanning Software Consulting,
;                   26 November 1998.
;       Added Horizontal keyword to SetProperty method and fixed problem in
;       going from Vertical to Horizontal color bars. 29 Nov 1998. DWF.
;       Added LoadCT method and current color table index to object.
;             6 December 1998.
;       Fixed a bug dealing with nearest neighbor resampling. 30 Mar 1999. DWF.
;       Fixed a bug with how NCOLORS and BOTTOM keywords interacted.
;             29 Aug 1999. DWF.
;       10 Oct 99. Modified the program so that current plot and map coordinates
;                are saved and restored after the colorbar is drawn. DWF.
;       26 May 2000 Added {XY}TICKV capability to the draw method. This
;                required adding TickV to the object data structure, and to the
;                INIT, GetProperty and SetProperty methods.
;                Changed default tick length to -0.1. DWF (and Jack Saba)
;       18 Nov 2001. Added Clamp method. DWF.
;       25 September 2010. Renamed to FSC_Colorbar__Define to avoid conflict with a
;                Colorbar__Define program introduced with IDL 8.0. DWF.
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
PRO FSC_Colorbar::Clamp, datarange, Draw=draw

; This method clamps the data to a particular data range.

self->GetProperty, Range=currentRange

thisclamp = Bytscl(datarange, Max=currentRange[1], Min=currentRange[0])
lowval = thisclamp[0]
highval = thisclamp[1]
IF self.vertical THEN bar = BytScl(REPLICATE(1B,10) # BINDGEN(self.ncolors), Min=lowval, Max=highval) ELSE $
   bar = BytScl(BIndgen(self.ncolors) # Replicate(1B, 20), Min=lowval, Max=highval)
*self.bar = bar
IF Keyword_Set(draw) THEN self->Draw, /Erase
END
;-------------------------------------------------------------------------


PRO FSC_Colorbar::GetProperty, $

; The GetProperty method of the FSC_COLORBAR object class. All properties are
; obtained via the following keywords:

   Background=background, $ ; Background color. Default is !P.Background.
   Bottom=bottom, $         ; Bottom color index of colors allocated to
                            ; colorbar.
   Charsize=charsize, $     ; Character size of annotation. Default is 1.0.
   Color=color, $           ; Color of annotation and outline. Default is
                            ; !P.Color
   Font=font, $             ; Font to use for annotation. Default is -1,
                            ; Hershey fonts.
   Format=format, $         ; Format of annotation. Default is F8.2.
   Index=index, $           ; The color table index. Default is to use none.
   Major=major, $           ; The number of major tick intervals. Default is 5.
   Minor=minor, $           ; The number of minor tick intervals. Default is 2.
   MinusOne=minusone, $     ; Set this keyword to choose MinusOne keyword on
                            ; Congrid command.
   NColors=ncolors, $       ; The number of colors allocated to colorbar.
                            ; Default is (256 < !D.N_Colors).
   Neighbor=neighbor, $     ; Set to indicate Nearest Neighbor sampling for
                            ; Congrid. Default is 0 (Bilinear).
   Position=position, $     ; The position of colorbar in normalized
                            ; coordinates.
   Range=range, $           ; The data range on colorbar. Default is [0, 255].
   TickLen=ticklen, $       ; The length of tick marks. Default is -0.1.
   TickV=tickv, $           ; Locations for tick marks in color index units.
   Title=title, $           ; The title of the color bar. Default is a null
                            ; string.
   Vertical=vertical, $     ; Set this keyword if you want a vertical colorbar.
                            ; Default is horizontal.
   XEraseBox=xerasebox, $   ; A five-element vector of X points (normalized)
                            ; for erasing the colorbar plot.
   YEraseBox=yerasebox      ; A five-element vector of Y points (normalized)
                            ; for erasing the colorbar plot.

   ; Error handling.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in GETPROPERTY method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::GetProperty Method: ' + !Error_State.Msg
   RETURN
ENDIF

   ; Return value of self object.

bottom = self.bottom
charsize = self.charsize
color = self.color
background = self.background
font = self.font
format = self.format
index = self.index
range = self.range
major = self.major
minor = self.minor
minusone = self.minusone
ncolors = self.ncolors
IF self.neighbor EQ 0 THEN neighbor = 1 ELSE neighbor = 0
position = self.position
title = self.title
ticklen = self.ticklen
tickv   = *(self.tickv)
vertical = self.vertical
xerasebox = self.xerasebox
yerasebox = self.yerasebox

END
;-----------------------------------------------------------------



PRO FSC_Colorbar::SetProperty, $

; The SetProperty method of the FSC_COLORBAR object class. All properties are
; set via the following keywords:

   Background=background, $ ; Background color. Default is !P.Background.
   Bottom=bottom, $         ; Bottom color index of colors allocated to
                            ; colorbar.
   Charsize=charsize, $     ; Character size of annotation. Default is 1.0.
   Color=color, $           ; Color of annotation and outline. Default is
                            ; !P.Color
   Draw=draw, $             ; Set this keyword to call DRAW method after
                            ; setting property.
   Erase=erase, $           ; Set this keyword to set ERASE keyword on DRAW
                            ; method call.
   Font=font, $             ; Font to use for annotation. Default is -1,
                            ; Hershey fonts.
   Format=format, $         ; Format of annotation. Default is F8.2.
   Horizontal=horizontal, $ ; Set this keyword to go from a vertical colorbar
                            ; to horizontal.
   Index=index, $           ; The color table index. Default is 0.
   Major=major, $           ; The number of major tick intervals. Default is 5.
   Minor=minor, $           ; The number of minor tick intervals. Default is 2.
   MinusOne=minusone, $     ; Set this keyword to choose MinusOne keyword on
                            ; Congrid command.
   NColors=ncolors, $       ; The number of colors allocated to colorbar.
                            ; Default is (256 < !D.N_Colors).
   Neighbor=neighbor, $     ; Set to indicate Nearest Neighbor sampling for
                            ; Congrid. Default is 0 (Bilinear).
   Position=position, $     ; The position of colorbar in normalized
                            ; coordinates.
   Range=range, $           ; The data range on colorbar. Default is [0, 255].
   TickLen=ticklen, $       ; The length of tick marks. Default is -0.1.
   TickV=tickv, $           ; Locations for tick marks in color index units.
   Title=title, $           ; The title of the color bar. Default is a null
                            ; string.
   Vertical=vertical, $     ; Set this keyword if you want a vertical colorbar.
                            ; Default is horizontal.
   XEraseBox=xerasebox, $   ; A five-element vector of X points (normalized)
                            ; for erasing the colorbar plot.
   YEraseBox=yerasebox      ; A five-element vector of Y points (normalized)
                            ; for erasing the colorbar plot.

   ; Error handling.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in SETPROPERTY method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::SetProperty Method: ' + !Error_State.Msg
   RETURN
ENDIF

   ; Set property if requested.

IF N_Elements(bottom) NE 0 THEN  self.bottom = bottom
IF N_Elements(charsize) NE 0 THEN  self.charsize = charsize
IF N_Elements(color) NE 0 THEN  self.color = color
IF N_Elements(background) NE 0 THEN  self.background = background
IF N_Elements(font) NE 0 THEN  self.font = font
IF N_Elements(format) NE 0 THEN  self.format = format
IF N_Elements(range) NE 0 THEN  self.range = range
IF N_Elements(major) NE 0 THEN  self.major = major
IF N_Elements(minor) NE 0 THEN  self.minor = minor
IF N_Elements(minusone) NE 0 THEN  self.minusone = minusone
IF N_Elements(ncolors) NE 0 THEN  self.ncolors = ncolors
IF N_Elements(index) NE 0 THEN  BEGIN
   self.index = index
   LoadCT, index, NColors=self.ncolors, Bottom=self.bottom
ENDIF
IF N_Elements(neighbor) NE 0 THEN  BEGIN
   self.neighbor = neighbor
   IF self.neighbor EQ 0 THEN self.neighbor = 1 ELSE self.neighbor = 0
ENDIF
IF N_Elements(position) NE 0 THEN  self.position = position
IF N_Elements(ticklen) NE 0 THEN  self.ticklen = ticklen
IF N_Elements(tickv) NE 0 THEN  *(self.tickv) = tickv
IF N_Elements(title) NE 0 THEN  self.title = title
IF N_Elements(xerasebox) NE 0 THEN  self.xerasebox = xerasebox
IF N_Elements(yerasebox) NE 0 THEN  self.yerasebox = yerasebox
IF Keyword_Set(vertical) THEN BEGIN
   *self.bar = Replicate(1B, 20) # BIndgen(self.ncolors)
   IF N_Elements(position) EQ 0 THEN self.position = [0.88, 0.15, 0.95, 0.85]
  self.vertical = 1
ENDIF
IF Keyword_Set(horizontal) THEN BEGIN
   *self.bar =  BIndgen(self.ncolors) # Replicate(1B, 20)
   IF N_Elements(position) EQ 0 THEN self.position = [0.15, 0.88, 0.85, 0.95]
   self.vertical = 0
ENDIF

   ; Scale the color bar.

*self.bar = BytScl(*self.bar, Top=self.ncolors-1) + self.bottom

   ; Draw the bar?

IF Keyword_Set(draw) THEN self->Draw, Erase=Keyword_Set(erase)
END
;-----------------------------------------------------------------



PRO FSC_Colorbar::LoadCT, index, Draw=draw, Erase=erase

; The LoadCT method of the FSC_COLORBAR object class.

   ; Error handling.

Catch, error
;error=0
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in LoadCT method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::LoadCT Method: ' + !Error_State.Msg
   RETURN
ENDIF

IF N_Elements(index) EQ 0 THEN index = 0

LoadCT, index, NColors=self.ncolors, Bottom=self.bottom, /Silent
self.index = index

IF Keyword_Set(draw) THEN self->Draw, Erase=Keyword_Set(erase)
END
;-----------------------------------------------------------------




PRO FSC_Colorbar::Erase

; The Erase method of the FSC_COLORBAR object class.

   ; Error handling.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in ERASE method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::Erase Method: ' + !Error_State.Msg
   RETURN
ENDIF

   ; Valid erase boxes?

IF Total(self.xerasebox) EQ 0 OR Total(self.yerasebox) EQ 0 THEN BEGIN
   ok = Dialog_Message('No colorbar exists to erase. Returning...')
   RETURN
ENDIF

PolyFill, self.xerasebox, self.yerasebox, /Normal, Color=self.background

END
;-----------------------------------------------------------------




PRO FSC_Colorbar::Draw, Erase=erase

; The Draw method of the FSC_COLORBAR object class.

   ; Error handling.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in DRAW method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::Draw Method: ' + !Error_State.Msg
   RETURN
ENDIF

   ; Save the current plot state.

bang_p = !P
bang_x = !X
bang_Y = !Y
bang_Z = !Z
bang_Map = !Map

   ; Must turn off decomposed color on devices that support it.

CASE !D.Name OF
   'MAC': Device, Decomposed=0
   'WIN': Device, Decomposed=0
   'X':   Device, Decomposed=0
   ELSE:
ENDCASE

   ; Get starting locations.

xstart = self.position(0)
ystart = self.position(1)

   ; Get the size of the bar.

xsize = (self.position[2] - self.position[0])
ysize = (self.position[3] - self.position[1])

   ; Need to erase the display first?

IF Keyword_Set(erase) THEN self->Erase

   ; Display the color bar in the window. Sizing is different for
   ; devices with scalable pixels.

scalablePixels = (!D.Flags AND 1) NE 0

IF scalablePixels THEN BEGIN

   TV, *self.bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal

ENDIF ELSE BEGIN

   bar = CONGRID(*self.bar, Ceil(xsize*!D.X_VSize), Ceil(ysize*!D.Y_VSize), $
       Interp=self.neighbor, Minus_One=self.minusone)
   TV, bar, xstart, ystart, /Normal

ENDELSE

   ; Annotate the color bar.

IF self.vertical THEN BEGIN

   Plot, [self.range[0], self.range[1]], [self.range[0], self.range[1]], $
      /NoData, XTicks=1, YTitle=self.title, Font=self.font, YMinor=self.minor, $
      Position=self.position, YTicks=self.major, XStyle=1, YStyle=1, $
      Color=self.color, YCharsize=self.charsize, /NoErase, $
      YTickformat=self.format, XTickformat='(A1)', YTicklen=self.ticklen, $
      YRange=self.range, YTickV=*(self.tickv)

ENDIF ELSE BEGIN

   Plot, [self.range[0], self.range[1]], [self.range[0], self.range[1]], $
      /NoData, XTicks=self.major, Font=self.font, XMinor=self.minor, $
      Position=self.position, YTicks=1, XStyle=1, YStyle=1, XTitle=self.title, $
      Color=self.color, XCharsize=self.charsize, /NoErase, YTickformat='(A1)', $
      XTickformat=self.format, XTicklen=self.ticklen, XRange=self.range, $
      XTickV=*(self.tickv)

ENDELSE

   ; Update the erase boxes.

self.xerasebox = [ !X.Region[0], !X.Region[0], !X.Region[1], !X.Region[1], $
                   !X.Region[0] ]
self.yerasebox = [ !Y.Region[0], !Y.Region[1], !Y.Region[1], !Y.Region[0], $
                   !Y.Region[0] ]

   ; Restore the previous plot and map system variables.

!P = bang_p
!X = bang_x
!Y = bang_y
!Z = bang_z
!Map = bang_map

END
;-----------------------------------------------------------------



PRO FSC_Colorbar::CleanUp

; The CleanUp method of the FSC_COLORBAR object class.

Ptr_Free, self.bar
Ptr_Free, self.tickv
END
;-----------------------------------------------------------------



FUNCTION FSC_Colorbar::Init, $

; The INIT method of the FSC_COLORBAR object class.

   Background=background, $ ; Background color. Default is !P.Background.
   Bottom=bottom, $         ; Bottom color index of colors allocated to
                            ; colorbar.
   Charsize=charsize, $     ; Character size of annotation. Default is 1.0.
   Color=color, $           ; Color of annotation and outline. Default is
                            ; !P.Color
   Font=font, $             ; Font to use for annotation. Default is -1,
                            ; Hershey fonts.
   Format=format, $         ; Format of annotation. Default is F8.2.
   Index=index, $           ; The color table index. Default is to use whatever
                            ; is loaded.
   Major=major, $           ; The number of major tick intervals. Default is 5.
   Minor=minor, $           ; The number of minor tick intervals. Default is 2.
   MinusOne=minusone, $     ; Set this keyword to choose MinusOne keyword on
                            ; Congrid command.
   NColors=ncolors, $       ; The number of colors allocated to colorbar.
                            ; Default is (256 < !D.N_Colors).
   Neighbor=neighbor, $     ; Set to indicate Nearest Neighbor sampling for
                            ; Congrid. Default is 0 (Bilinear).
   Position=position, $     ; The position of colorbar in normalized
                            ; coordinates.
   Range=range, $           ; The data range on colorbar. Default is [0, 255].
   TickLen=ticklen, $       ; The length of tick marks. Default is -0.1.
   TickV=tickv, $           ; Locations for tick marks in color index units.
   Title=title, $           ; The title of the color bar. Default is a null
                            ; string.
   Vertical=vertical, $     ; Set this keyword if you want a vertical colorbar.
                            ; Default is horizontal.
   XEraseBox=xerasebox, $   ; A five-element vector of X points (normalized)
                            ; for erasing the colorbar plot.
   YEraseBox=yerasebox      ; A five-element vector of Y points (normalized)
                            ; for erasing the colorbar plot.

   ; Error handling.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, Cancel=1
   ok = Dialog_Message('Error in INIT method. Returning...')
   Print, ''
   Print, 'FSC_Colorbar::INIT Method: ' + !Error_State.Msg
   RETURN, 0
ENDIF

   ; Check keywords. Define default values if unsupplied.

IF N_Elements(bottom) EQ 0 THEN bottom = 0
IF N_Elements(ncolors) EQ 0 THEN ncolors = (256-bottom) < (!D.N_Colors - bottom)
IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
IF N_Elements(font) EQ 0 THEN font = -1
IF N_Elements(format) EQ 0 THEN format = '(F8.2)'
IF N_Elements(color) EQ 0 THEN color = !P.Color
IF N_Elements(index) EQ 0 THEN index = -1
IF index GE 0 THEN LoadCT, index, NColors=ncolors, Bottom=bottom, /Silent
IF N_Elements(background) EQ 0 THEN background = !P.Background
IF N_Elements(range) EQ 0 THEN range = [0, 255]
IF N_Elements(major) EQ 0 THEN major = 5
IF N_Elements(minor) EQ 0 THEN minor = 2
IF N_Elements(ticklen) EQ 0 THEN ticklen = -0.1
IF N_Elements(tickv) EQ 0 THEN tickv = 0
IF N_Elements(title) EQ 0 THEN title = ''
IF N_Elements(xerasebox) EQ 0 THEN xerasebox = FltArr(5)
IF N_Elements(yerasebox) EQ 0 THEN yerasebox = FltArr(5)
vertical = Keyword_Set(vertical)
neighbor = Keyword_Set(neighbor)
IF neighbor EQ 0 THEN neighbor = 1 ELSE neighbor = 0
minusone = Keyword_Set(minusone)

   ; Create the colorbar image and default positions.

IF vertical THEN BEGIN
   bar = Replicate(1B, 20) # BIndgen(ncolors)
   IF N_Elements(position) EQ 0 THEN position = [0.88, 0.15, 0.95, 0.85]
ENDIF ELSE BEGIN
   bar = BIndgen(ncolors) # Replicate(1B, 20)
   IF N_Elements(position) EQ 0 THEN position = [0.15, 0.88, 0.85, 0.95]
ENDELSE

   ; Scale the color bar.

bar = BytScl(bar, Top=ncolors-1) + bottom

   ; Fill out this particular instance of the FSC_COLORBAR object.

self.bar = Ptr_New(bar)
self.position = Float(position)
self.vertical = vertical
self.bottom = bottom
self.ncolors = ncolors
self.font = font
self.format = format
self.index = index
self.range = range
self.title = title
self.ticklen = ticklen
self.tickv = Ptr_New(tickv)
self.neighbor = neighbor
self.major = major
self.minor = minor
self.minusone = minusone
self.charsize = charsize
self.color = color
self.background = background
self.xerasebox = xerasebox
self.yerasebox = yerasebox

RETURN, 1
END
;-----------------------------------------------------------------



PRO FSC_Colorbar__Define

; Structure definition of the FSC_COLORBAR object class.

struct = { FSC_COLORBAR, $ ; The FSC_COLORBAR object class definition.
   bar:Ptr_New(), $        ; The colorbar image.
   position:FltArr(4), $   ; Position in the window (normalized coords).
   vertical:0, $           ; Flag for vertical colorbar. Default is 0.
   bottom:0, $             ; The bottom color index. Default is 0.
   ncolors:0, $            ; The number of colors in the colorbar. Default is
                           ;  (256 < !D.N_Colors).
   major:0, $              ; The number of major tick intervals. (Same as
                           ;  XTICKS.) Default is 5.
   minor:0, $              ; The number of minor tick intervals. Default is 2.
   font:0, $               ; The font to use. Default is -1, Hershey fonts.
   format:'', $            ; The format of the annotations. Default is F8.2.
   index:0, $              ; The current color table index.
   range:FltArr(2), $      ; The colorbar data range. Default is [0,255].
   title:'', $             ; The title of the colorbar. Default is null string.
   ticklen:0.0, $          ; The length of tick marks. Default is -0.1.
   tickv: Ptr_New(), $     ; Values for the tick marks.
   neighbor:0, $           ; A flag to indicate nearest neighbor sampling.
                           ;  Default is 0 (bilinear).
   minusone:0, $           ; A flag to indicate MinusOne keyword set on
                           ;  Congrid. Default is 0.
   charsize:0.0, $         ; The character size of the annotation. Default is
                           ;  1.0.
   color:0, $              ; The color of annotation and colorbar outline.
                           ;  Default is !P.Color.
   background:0, $         ; The background or "erase" color. Default is
                           ;  !P.Background.
   xerasebox:FltArr(5), $  ; A vector of X points for erasing.
   yerasebox:FltArr(5) $   ; A vector of Y points for erasing.
   }

END
;--------------------------------------------------------------------------
