;+
; NAME:
;       CLIPBOARD
;
; PURPOSE:
;
;       The purpose of this program is to copy the contents of a
;       graphics window to the clipboard for subsequent pasting into
;       applications such as Photoshop or Powerpoint.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;      Graphics.
;
; CALLING SEQUENCE:
;
;      CLIPBOARD, window_index
;
; OPTIONAL INPUTS:
;
;       window_index:    The window index number of the graphics window to
;                        copy. If absent, the current graphics window is used
;                        by default.
;
; KEYWORDS:
;
;       All COLOR_QUAN keywords are allowed. In particular, if you are
;       taking snapshots of line plots with few colors in them, you may
;       get better results by calling the program with the CUBE=6 keyword
;       set. Otherwise, white colors can sometimes be a bit gray.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; DEPENDENCIES:
;
;       Uses the IDLgrClipboard object introduced in IDL 5.2(?).
;
; PROCEDURE:
;
;       Copies the window contents to a clipboard object.
;
; EXAMPLE:
;
;        IDL> Window
;        IDL> Plot, Findgen(11)
;        IDL> CLIPBOARD
;
; RESTRICTIONS:
;
;       May not work for all applications. Applications tested successfully
;       include: Framemaker, Powerpoint, Photoshop, Excel, Microsoft Word.
;       Converts 24-bit images to 2D images with color tables.
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 24 October 2001.
;       Added _EXTRA keyword to pass COLOR_QUAN keywords along. 28 Oct 2002. DWF.
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
PRO Clipboard, windowIndex, _Extra=extra

; This procedure copies the window identified by the
; window index number (or the current window if an index
; number is not provided) to the clipboard.

IF N_Elements(windowIndex) EQ 0 THEN windowIndex = !D.Window

   ; Is this a valid window?

IF windowIndex LT 0 THEN BEGIN
   ok = Dialog_Message('No current window to copy. Returning...')
   RETURN
ENDIF

   ; Catch window setting errors.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message('Specified window is unavailable: ' + $
      StrTrim(windowIndex, 2) + '. Returning...')
   WSet, thisWindow
   RETURN
ENDIF

   ; Set active window.

thisWindow = !D.Window
WSet, windowIndex
Catch, /Cancel

   ; Take a snapshot of window. Pay attention to visual depth.

Device, Get_Visual_Depth=thisDepth
IF thisDepth GT 8 THEN BEGIN
   snapshot = TVRD(True=1)
   snapshot = Color_Quan(snapshot, 1, r, g, b, _Extra=extra)
ENDIF ELSE BEGIN
   snapshot = TVRD()
   TVLCT, r, g, b, /Get
ENDELSE
s = Size(snapshot, /Dimensions)

   ; Create an object graphics image and hierarchy.

palette = Obj_New('IDLgrPalette', r, g, b)
image = Obj_New('IDLgrImage', snapshot, Palette=palette)
model = Obj_New('IDLgrModel')
model->Add, image
thisView = Obj_New('IDLgrView', ViewPlane_Rect=[0,0,s[0],s[1]])
thisView->Add, model

   ; Create a clipboard

theClipboard = Obj_New('IDLgrClipboard', Color_Model=1, $
   Dimensions=[s[0], s[1]], N_Colors=!D.Table_Size, $
   Resolution=[1.0/!D.X_PX_CM, 1.0/!D.Y_PX_CM], $
   Palette=palette)

   ; Copy the snapshot to the clipboard.

theClipboard->Draw, thisView

   ; Destroy the objects.

Obj_Destroy, palette
Obj_Destroy, model
Obj_Destroy, thisView
Obj_Destroy, theClipboard

   ; Restore the current window.

IF thisWindow NE -1 THEN WSet, thisWindow
END
