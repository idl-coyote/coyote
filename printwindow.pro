;+
; NAME:
;   PRINTWINDOW
;
;    This program sends the contents of the specified
;    window to the default printer. The current window
;    is used if a window index number is not provided.
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
;  Graphics
;
; CALLING SEQUENCE:
;
;  IDL> PrintWindow, wid
;
; OPTIONAL POSITIONAL PARAMETERS:
;
;   WID       The window index number of the window to send to the
;             printer. !D.Window used by default.
;
; KEYWORD PARAMETERS:
;
;   LANDSCAPE  If this keyword is set, the output is in Landscape
;              mode. Otherwise, Portrait mode is used.
;
;   PAGESIZE: Set this keyword to a string indicating the type
;             of PostScript page size you want. Current values are "LETTER",
;             "LEGAL", and "A4". Default is "LETTER".
;
;   RGB_ERROR: Some printers (particularly attached to LINUX machines) cannot
;              load a 24-bit image. You get this error message:
;
;                 %Can't set RGB color on an indexed destination.
;
;              If this happens to you, set this keyword and the 24-bit image will
;              be made into a 2D image with color table vectors. Colors are not
;              quaranteed to be accurate with this method, but in practice it is
;              not usually too bad.
;
;              IDL> PrintWindow, /RGB_Error
;
; MODIFICATION HISTORY:
;
; Written by David W. Fanning based on previous PRINT_IT program. 29 July 2000.
; Added RGB_Error keyword. 2 Nov 2004. DWF.
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


FUNCTION PRINTWINDOW_PSWINDOW_ASPECT, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

ON_ERROR, 1

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
END ; ----------------------------------------------------------------------------------



FUNCTION PRINTWINDOW_PSWINDOW, LANDSCAPE=landscape, CM=cm, MARGIN=margin, $
   PageSize=pagesize, Printer=printer, Fudge=fudge, XFudge=xfudge, YFudge=yfudge

   ; Set up default values.

landscape = Keyword_Set(landscape)
cm = Keyword_Set(cm)
printer = Keyword_Set(printer)
inches = 1

   ; Set up printer fudge factors, if necessary.

IF N_Elements(fudge) NE 0 THEN BEGIN
   xfudge = fudge
   yfudge = fudge
ENDIF
IF N_Elements(xfudge) EQ 0 THEN xfudge = 0.0
IF N_Elements(yfudge) EQ 0 THEN yfudge = 0.0

   ; Get the page size.

IF N_Elements(pagesize) EQ 0 THEN pagesize = 'LETTER' $
   ELSE pagesize = StrUpCase(pagesize)
CASE pagesize OF
   'LETTER': BEGIN
      shortside = 8.5
      longside = 11.0
      ENDCASE
   'LEGAL': BEGIN
      shortside = 8.5
      longside = 14.0
      ENDCASE
    'A4': BEGIN
      shortside = 8.27
      longside = 11.7
      ENDCASE
    ELSE: BEGIN
      Message, 'Unknown page size. Using LETTER...', /Informational
      shortside = 8.5
      longside = 11.0
      ENDCASE
ENDCASE

   ; Need measurements in centimeters?

IF KEYWORD_SET(cm) THEN BEGIN
      shortside = shortside * 2.54
      longside = longside * 2.54
      inches = 0
ENDIF

   ; Determine the margin of the window on the page.

IF N_ELEMENTS(margin) EQ 0 THEN margin=0.15

   ; Get the aspect ratio of the current display window. Aspect ratio
   ; is ratio of xsize/ysize.

aspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Get the aspect ratio of the page.

IF Keyword_Set(landscape) THEN pAspectRatio = shortside / longside $
   ELSE pAspectRatio = longside / shortside

   ; Get the position on the page for this window.

pos = PRINTWINDOW_PSWindow_Aspect(aspectRatio, Margin=margin, WindowAspect=pAspectRatio)

   ; Convert normalized position coordinates to size units.

IF KEYWORD_SET(landscape) THEN BEGIN
   IF printer THEN BEGIN
      xsize = (pos[2] - pos[0]) * longside
      ysize = (pos[3] - pos[1]) * shortside
      yoffset = pos[1] * shortside - yfudge
      xoffset = pos[0] * longside - xfudge
      landscape = 1
      portrait = 0
   ENDIF ELSE BEGIN
      xsize = (pos[2] - pos[0]) * longside
      ysize = (pos[3] - pos[1]) * shortside
      xoffset = pos[1] * shortside
      yoffset = longside - (pos[0] * longside)
      landscape = 1
      portrait = 0
   ENDELSE
ENDIF ELSE BEGIN
   xsize = (pos[2] - pos[0]) * shortside
   ysize = (pos[3] - pos[1]) * longside
   IF printer THEN BEGIN
      xoffset = pos[0] * shortside - xfudge
      yoffset = pos[1] * longside - yfudge
   ENDIF ELSE BEGIN
      xoffset = pos[0] * shortside
      yoffset = pos[1] * longside
   ENDELSE
   landscape = 0
   portrait = 1
ENDELSE

   ; Return the proper DEVICE data structure.

RETURN, {PSWINDOW_STRUCT, XSIZE:xsize, YSIZE:ysize, $
   XOFFSET:xoffset, YOFFSET:yoffset, INCHES:inches, $
   PORTRAIT:portrait, LANDSCAPE:landscape}

END ;--------------------------------------------------------------------------------------



FUNCTION PrintWindow_Error, theMessage, Traceback=traceback, NoName=noName

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from Error_Message:'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END ;-----------------------------------------------------------------------------------


PRO PrintWindow, wid, Landscape=landscape, PageSize=pageSize, RGB_Error=rgb_error

   ; Check parameters.

IF N_Params() EQ 0 THEN wid = !D.Window
landscape = Keyword_Set(landscape)
IF N_Elements(pagesize) EQ 0 THEN pagesize = 'LETTER' $
   ELSE pagesize = StrUpCase(pagesize)
CASE pagesize OF
   'LETTER': BEGIN
      shortside = 8.5
      longside = 11.0
      ENDCASE
   'LEGAL': BEGIN
      shortside = 8.5
      longside = 14.0
      ENDCASE
    'A4': BEGIN
      shortside = 8.27
      longside = 11.7
      ENDCASE
    ELSE: BEGIN
      Message, 'Unknown page size. Using LETTER...', /Informational
      shortside = 8.5
      longside = 11.0
      ENDCASE
ENDCASE

   ; Are we running on a 24-bit device?

Catch, theError
IF theError NE 0 THEN BEGIN
   theDepth = 8
   GOTO, testDepth
ENDIF
Device, Get_Visual_Depth=theDepth
testDepth:
IF theDepth GT 8 THEN truecolor = 1 ELSE truecolor = 0

   ; Main error handler.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = PrintWindow_Error(!Error_State.Msg)
   RETURN
ENDIF

   ; Valid window ID?

IF wid LT 0 THEN Message, 'No graphics window currently open.', /NoName

   ; Get information about printer.

ok = Dialog_PrinterSetup()
IF NOT ok THEN RETURN

   ; Make the window current. Get contents.

thisWindow = !D.Window
WSet, wid
contents = TVRD(True=truecolor)

   ; Need a window on printer with same aspect ratio as current window.
   ; Your fudge factor (to account for offsets calculated from the printable
   ; edge of the page rather than the real edge of the page) may be different.

keywords = PRINTWINDOW_PSWindow(/Printer, Landscape=landscape, Fudge=0.25, $
   PageSize=pagesize, Margin=0.075)

IF Keyword_Set(RGB_Error) AND (truecolor GT 0) THEN BEGIN
   image2D = Color_Quan(contents, truecolor, r, g, b, Colors=256, Dither=1)
   TVLCT, r, g, b
   contents = image2D
   truecolor = 0
ENDIF

   ; Change the current device to PRINTER. Copy color table.

thisDevice = !D.Name

Set_Plot, 'PRINTER', /Copy

   ; Reset the PRINTER for proper calculations. Landscape mode
   ; has to be set separately from sizes. I don't know why.

Device, Scale_Factor=1, Portrait=1
Device, Landscape = keywords.landscape

   ; Configure the printer.

Device, _Extra=keywords

   ; Print it. Most UNIX printers can't do color.

TV, contents, True=truecolor, XSize=keywords.xsize, $
   YSize=keywords.ysize, Inches=keywords.inches

Device, /Close_Document

   ; Clean up.

Set_Plot, thisDevice
WSet, thisWindow

END
