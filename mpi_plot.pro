;+
; NAME:
;       MPI_PLOT
;
; PURPOSE:
;
;       This program is a simple wrapper for the IDL PLOT command. The
;       main purpose of the program is to demonstrate one way the
;       MPI_PLOTCONFIG program can be used to update plot parameters.
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
;       Graphics
;
; CALLING SEQUENCE:
;
;       MPI_Plot, x, y
;       MPI_Plot, xx, yy, /Overplot
;
; INPUT PARAMETERS:
;
;       x - The independent data. If y is not present, x is taken to be the dependent data.
;
;       y - The dependent data. The vectors x and y must be the same length.
;
;       xs - The independent data to overplot. If yy is not present, xx is taken to be the dependent data.
;
;       yy - The dependent data to overplot. The vectors xx and yy must be the same length.
;
; INPUT KEYWORDS:
;
;       BACKGROUND - The name of a background color. (See below for a list of color names.)
;         By default on 24-bit systems: 'IVORY'. Uses 'GRAY' on 8-bit systems.
;
;       COLOR - The name of the plot color. (See below for a list of color names.)
;         By default on 24-bit systems: 'SADDLE BROWN'. Uses 'GREEN' on 8-bit systems.
;         When OVERPLOTing, use the COLOR keyword to specify the color of the overplot.
;
;       DATACOLOR - The name of the data color. By default the same as the COLOR keyword.
;         (See below for a list of color names.) When OVERPLOTing, use the COLOR keyword
;         to specify the color of the overplot.
;
;       OVERPLOT - Set this keyword to overplot data into the MPI_PLOT window.
;         If multiple windows are on the display, select the one to overplot into
;         by selecting it with the cursor.
;
;       PSYM - The plot symbol value. By default, 18. Possible values are:
;         0 - Dot
;         1 - Filled Circle
;         2 - Filled Upward Triangle
;         3 - Filled Downward Triangle
;         4 - Filled Diamond
;         5 - Filled Square
;         6 - Open Circle
;         7 - Open Upward Triangle
;         8 - Open Downward Triangle
;         9 - Open Diamond
;        10 - Open Square
;        11 - Plus Sign
;        12 - X
;        13 - Star
;        14 - Filed Rightfacing Triangle
;        15 - Filled Leftfacing Triangle
;        16 - Open Rightfacing Triangle
;        17 - Open Leftfacing Triangle
;        18 - No Symbol (the default).
;
;       TITLE - The title of the plot. By default, a null string.
;
;       XLOG = Set this keyword to use logarithmic axis styling on the X axis.
;
;       XTITLE - The title of the X axis of the plot. By default, a null string.
;
;       YLOG = Set this keyword to use logarithmic axis styling on the Y axis.
;
;       YTITLE - The title of the Y axis of the plot. By default, a null string.
;
;       In addition, any keyword appropriate for the MPI_PLOTCONFIG object program can be used.
;       Among those keywords, are these most popular ones:
;
;       CHARSIZE - The character size of the plot. By default, 1.0.
;
;       CHARTHICK - The character thickness of the plot. By default, 1.0.
;
;       FONT - The type of plot font: -1=Hershey, 0=Hardware, 1=True-Type. By default, !P.FONT.
;
;       LINESTYLE - The plot linestyle. By default, 0. Possible values are:
;         0 - Solid Line
;         1 - Dotted
;         2 - Dashed
;         3 - Dash Dot
;         4 - Dash Dot Dot
;         5 - Long Dash
;         6 - No Line
;
;       POSITION - The position of the plot in the plot window in normalized coordinates. By default, [0.20, 0.15, 0.95, 0.95].
;
;       SYMSIZE - The plot symbol size. By default, 1.0.
;
;       THICK - The plot line thickness. By default, 1.0.
;
;       TICKLEN - The plot tick length. By default, 0.02.
;
; COLOR NAMES:
;
;        The following color names can be used for BACKGROUND, COLOR and DATACOLOR keywords:
;
;           White, Snow, Ivory, Light Yellow, Cornsilk, Beige, Seashell, Linen, Antique White,
;           Papaya, Almond, Bisque, Moccasin, Wheat, Burlywood, Tan, Light Gray, Lavender,
;           Medium Gray, Gray, Slate Gray, Dark Gray , Charcoal, Black, Light Cyan, Powder Blue,
;           Sky Blue, Steel Blue, Dodger Blue, Royal Blue, Blue, Navy, Honeydew, Pale Green,
;           Aquamarine, Spring Green, Cyan, Turquoise, Sea Green, Forest Green, Green Yellow,
;           Chartreuse, Lawn Green, Green, Lime Green, Olive Drab, Olive, Dark Green, Pale Goldenrod,
;           Khaki, Dark Khaki, Yellow, Gold, Goldenrod, Dark Goldenrod, Saddle Brown, Rose,
;           Pink, Rosy Brown, Sandy Brown, Peru, Indian Red, Chocolate, Sienna, Dark Salmon,
;           Salmon, Light Salmon, Orange, Coral, Light Coral, Firebrick, Brown, Hot Pink,
;           Deep Pink, Magenta, Tomato, Orange Red, Red, Violet Red, Maroon, Thistle, Plum,
;           Violet, Orchid, Medium Orchid, Dark Orchid, Blue Violet, and Purple.
;
; REQUIRED PROGRAMS:
;
;        The following programs are required to reside in your !PATH. They can be
;        obtained from the Coyote Library:
;
;                     http://www.dfanning.com/programs/adjustposition.pro
;                     http://www.dfanning.com/programs/cw_drawcolor.pro
;                     http://www.dfanning.com/programs/cw_spacer.pro
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/fsc_color.pro
;                     http://www.dfanning.com/programs/fsc_droplist.pro
;                     http://www.dfanning.com/programs/fsc_field.pro
;                     http://www.dfanning.com/programs/fsc_fileselect.pro
;                     http://www.dfanning.com/programs/fsc_inputfield.pro
;                     http://www.dfanning.com/programs/fsc_psconfig__define.pro
;                     http://www.dfanning.com/programs/mpi_plotconfig__define.pro
;                     http://www.dfanning.com/programs/mpi_axis.pro
;                     http://www.dfanning.com/programs/mpi_axis__define.pro
;                     http://www.dfanning.com/programs/pickcolorname.pro
;                     http://www.dfanning.com/programs/psconfig.pro
;                     http://www.dfanning.com/programs/pswindow.pro
;                     http://www.dfanning.com/programs/tvread.pro
;
;         All these programs can be obtained at once by downloading the MPI_PLOT zip file:
;
;                     http://www.dfanning.com/programs/mpi_plot.zip
;
;
; COMMON BLOCK:
;
;       The addition of the OVERPLOT keyword required a COMMON block named MPI_PLOT_COMMMON
;       to store the program information pointer. This pointer is loaded in the COMMON block
;       when the keyboard focus changes. Thus, to overplot into an MPI_PLOT window, first
;       select the window with the cursor.
;
; RESTRICTIONS
;
;       Colors will be loaded in the color table.
;
; EXAMPLE:
;
;       x = Findgen(11) & y = Findgen(11)
;       MPI_PLOT, x, y
;       MPT_PLOT, Reverse(x), y, /Overplot, Linestyle=2
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, March 2001, and offered to the IDL user
;          community by the Max-Plank Institute of Meteorology in  Hamburg, Germany.
;       Added OVERPLOT keyword and made numerous general improvements. 21 November 2001. DWF
;       Removed restriction for only one copy of MPI_PLOT on display at once. 25 November 2001. DWF.
;       Added XLOG and YLOG keywords. 7 May 2002. DWF.
;       PostScript configuration now opens up with a plot window the same aspect
;          ratio as the MPI_PLOT window. 21 August 2002. DWF.
;       Fixed a problem in which PSCONFIG was called as a blocking widget rather than as a
;          modal widget, as required. 11 March 2003. DWF.
;       Made a change to the GUI method that fixes a problem I have been having
;          on some Linux machines in widgets not always showing up. 15 July 2003. DWF.
;       Fixed a problem when ploting vectors with more that 32K elements. 7 March 2006. DWF.
;       Fixed a problem with initial display on Macs. 20 May 2009. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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

PRO MPI_Overplot__Define

   struct = { MPI_OVERPLOT, $      ; The overplot structure.
              dep: Ptr_New(), $    ; The dependent data.
              indep: Ptr_New(), $  ; The independent data.
              extra: Ptr_New(), $  ; Any extra keywords.
              color: "", $         ; The name of the overplot color.
              psym:0L $            ; The symbol used for overplotting.
            }
END; -------------------------------------------------------------------------



PRO MPI_Plot_Clear_Overplots, event

; This procedure clears any overplots from the program.

   ; Get info structure.

Widget_Control, event.top, Get_UValue=infoptr

   ; Free the overplot pointer.

IF Ptr_Valid((*infoptr).oplotinfo) THEN BEGIN
   FOR j=0,N_Elements(*(*infoptr).oplotinfo)-1 DO BEGIN
      struct = (*(*infoptr).oplotinfo)[j]
      Ptr_Free, struct.dep
      Ptr_Free, struct.indep
      Ptr_Free, struct.extra
   ENDFOR
   Ptr_Free, (*infoptr).oplotinfo
ENDIF ELSE Ptr_Free, (*infoptr).oplotinfo

   ; Turn overplot button off.

Widget_Control, (*infoptr).clearoplotID, Sensitive=0

   ; Redraw the plot.

MPI_Plot_DrawThePlot, infoptr

END; -------------------------------------------------------------------------


PRO MPI_Plot_DrawThePlot, infoptr, Overplot=overplot

; This procedure draws the plot and overplot on the
; output device.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /cancel
   ok = Error_Message(/Traceback)
ENDIF

   ; Set current graphics window for devices that support it.

IF (!D.Flags AND 256) NE 0 THEN WSet, (*infoptr).wid

   ; Restore plotting and axis system variables. Required for overplotting.

!X = (*infoptr).x
!Y = (*infoptr).y
!P = (*infoptr).p

   ; Draw the plot.

IF Keyword_Set(overplot) EQ 0 THEN BEGIN
   plotkeywords = (*infoptr).plotconfig->GetKeywords()
   IF (!D.Flags AND 256) EQ 0 THEN BEGIN
      PolyFill, [0, 0, 1, 1, 0], [0, 1, 1, 0, 0], /Normal, Color=plotkeywords.background
      Plot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords, /NoData, /NoErase
   ENDIF ELSE BEGIN
      Plot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords, /NoData
   ENDELSE
   plotkeywords.color = plotkeywords.datacolor
   OPlot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords
ENDIF

   ; Draw any overplots.

IF Ptr_Valid((*infoptr).oplotinfo) THEN BEGIN
   FOR j=0,N_Elements(*(*infoptr).oplotinfo)-1 DO BEGIN
      struct = (*(*infoptr).oplotinfo)[j]
      OPlot, *struct.indep, *struct.dep, _Extra=*struct.extra, $
         Color=FSC_Color(struct.color, !D.Table_Size-4+j), $
         PSym=(*infoptr).plotconfig->Symbol(struct.psym)
   ENDFOR
ENDIF

   ; Update the system variables.

(*infoptr).x = !X
(*infoptr).y = !Y
(*infoptr).p = !P

END; -------------------------------------------------------------------------




PRO MPI_Plot_PostScript, event

; This event handler creates PostScript output.

   ; Get info structure.

Widget_Control, event.top, Get_UValue=infoptr

   ; Make the display window current.

WSet, (*infoptr).wid

keywords = PSWindow()
pskeywords = PSConfig(Cancel=cancelled, _Extra=keywords, Group_Leader=event.top)
IF cancelled THEN RETURN

   ; Configure the PostScript device.

thisDevice = !D.Name
Set_Plot, 'PS'
Device, _Extra=pskeywords

   ; Display the grapics. Background color will have to be done
   ; separately. And if color is turned off, we have have to do
   ; things differntly, too.

plotkeywords = (*infoptr).plotconfig->GetKeywords()

IF pskeywords.color EQ 1 THEN BEGIN

   PolyFill, [0, 0, 1, 1, 0], [0, 1, 1, 0, 0], /Normal, Color=plotkeywords.background
   Plot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords, /NoData, /NoErase
   plotkeywords.color = plotkeywords.datacolor
   OPlot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords

ENDIF ELSE BEGIN

   plotkeywords.color = !P.Color
   plotkeywords.datacolor = !P.Color
   plotkeywords.background = !P.Background
   Plot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords, /NoData
   OPlot, (*infoptr).indep, (*infoptr).dep, _Extra=plotkeywords

ENDELSE

   ; Any overplotting to do?

IF Ptr_Valid((*infoptr).oplotinfo) THEN BEGIN
   FOR j=0,N_Elements(*(*infoptr).oplotinfo)-1 DO BEGIN
      struct = (*(*infoptr).oplotinfo)[j]
      OPlot, *struct.indep, *struct.dep, _Extra=*struct.extra, $
         Color=FSC_Color(struct.color, !D.Table_Size-4+j), $
         PSym=(*infoptr).plotconfig->Symbol(struct.psym)
   ENDFOR
ENDIF

   ; Close the PostScript file and clean up.

Device, /Close_File
Set_Plot, thisDevice

END; -------------------------------------------------------------------------



PRO MPI_Plot_Print, event

; This event handler executes the command in the Printer device.

   ; Set up the printer.

ok = Dialog_PrinterSetup()
IF NOT ok THEN RETURN

   ; Get info structure and printer orientation.

Widget_Control, event.top, Get_UValue=infoptr
Widget_Control, event.id, Get_UValue=orientation

   ; Save the current graphics device.

thisDevice = !D.Name

   ; Make the display window current.

WSet, (*infoptr).wid

   ; With printers, you have to load your drawing colors
   ; *before* you enter the PRINTER device and then copy
   ; them into the device. It's a pain in the ol' wazoo,
   ; but there is no help for it. :-(

plotkeywords = (*infoptr).plotconfig->GetKeywords()
TVLCT, FSC_Color(plotkeywords.name_background, /Triple), !P.Background
TVLCT, FSC_Color(plotkeywords.name_color, /Triple), !D.Table_Size-2
TVLCT, FSC_Color(plotkeywords.name_datacolor, /Triple), !D.Table_Size-3
IF Ptr_Valid((*infoptr).oplotinfo) THEN BEGIN
   FOR j=0,N_Elements(*(*infoptr).oplotinfo)-1 DO BEGIN
      struct = (*(*infoptr).oplotinfo)[j]
      TVLCT, FSC_Color(struct.color, /Triple),!D.Table_Size-4+j
   ENDFOR
ENDIF

   ; Set up the printer. You may have to adjust the fudge factors
   ; to account for the printable area offset.

CASE orientation OF
   'PORTRAIT': BEGIN
      keywords = PSWindow(/Printer, Fudge=0.25)
      Set_Plot, 'PRINTER', /Copy
      Device, Portrait=1
      ENDCASE
   'LANDSCAPE': BEGIN
      keywords = PSWindow(/Printer, /Landscape, Fudge=0.25)
      Set_Plot, 'PRINTER', /Copy
      Device, Landscape=1
      ENDCASE
ENDCASE
Device, _Extra=keywords

   ; Display the graphics.

MPI_Plot_DrawThePlot, infoptr

   ; Close the printer file and clean up.

Device, /Close_Document
Set_Plot, thisDevice

END ;----------------------------------------------------------------------------------------



PRO MPI_Plot_SaveAs, event

; Saves the current display window as output files.

   ; Get the info structure and the appropriate file extension.

Widget_Control, event.top, Get_UValue=infoptr
Widget_Control, event.id, Get_UValue=file_extension

   ; Base name for file output.

basename = 'mpi_plot'

   ; Take a snapshot of the display window and write the file.

WSet, (*infoptr).wid
CASE file_extension OF
   'BMP'  : image = TVREAD(Filename = basename, /BMP)
   'GIF'  : image = TVREAD(Filename = basename, /GIF)
   'PICT' : image = TVREAD(Filename = basename, /PICT)
   'JPG'  : image = TVREAD(Filename = basename, /JPEG)
   'TIF'  : image = TVREAD(Filename = basename, /TIFF)
   'PNG'  : image = TVREAD(Filename = basename, /PNG)
ENDCASE

END ;----------------------------------------------------------------------------------------



PRO MPI_Plot_Quit, event
Widget_Control, event.top, /Destroy
END ;-----------------------------------------------------------------------------



PRO MPI_Plot_Cleanup, tlb

   ; This is the cleanup routine for the program. All
   ; created objects and pointers must be destroyed.

Widget_Control, tlb, Get_UValue=infoptr
Obj_Destroy, (*infoptr).xaxis
Obj_Destroy, (*infoptr).yaxis
Obj_Destroy, (*infoptr).plotconfig
IF Ptr_Valid((*infoptr).oplotinfo) THEN BEGIN
   FOR j=0,N_Elements(*(*infoptr).oplotinfo)-1 DO BEGIN
      struct = (*(*infoptr).oplotinfo)[j]
      Ptr_Free, struct.dep
      Ptr_Free, struct.indep
      Ptr_Free, struct.extra
   ENDFOR
   Ptr_Free, (*infoptr).oplotinfo
ENDIF ELSE Ptr_Free, (*infoptr).oplotinfo
Ptr_Free, infoptr
END ;-----------------------------------------------------------------------------



PRO MPI_Plot_Configure_Button, event

   ; This event handler responds to requests to configure the
   ; plot parameters. Events will be sent to the buttonbase widget.

Widget_Control, event.top, Get_UValue=infoptr

   ; Call the GUI method on the Plot Configuration object.

Widget_Control, event.top, Update=0
(*infoptr).plotconfig->GUI, Group_Leader=event.top, All_Events=1, $
   NotifyID=[(*infoptr).buttonbase, event.top]
Widget_Control, event.top, Update=1

END ;-----------------------------------------------------------------------------



PRO MPI_Plot_Resize, event

thisEvent = Tag_Names(event, /Structure_Name)

IF thisEvent EQ 'WIDGET_BASE' THEN BEGIN

      ; This event handler responds to TLB re-size events.

   Widget_Control, event.top, Get_UValue=infoptr

      ; Make the display window the current graphics window.

   WSet, (*infoptr).wid

      ; Resize the draw widget.

   Widget_Control, (*infoptr).drawID, Draw_XSize=(*infoptr).xminsize > event.x, $
      Draw_YSize=(event.y - (*infoptr).ysize) > (*infoptr).yminsize

      ; Display the graphics.

   MPI_Plot_DrawThePlot, infoptr
   RETURN
ENDIF


IF thisEvent EQ 'WIDGET_KBRD_FOCUS' THEN BEGIN

   ; This event handler responds to TLB keyboard focus events.

   IF event.enter EQ 0 THEN RETURN

      ; Declare the common block with the info pointer.

   COMMON MPI_PLOT_COMMON, thePointer

   Widget_Control, event.top, Get_UValue=infoptr

      ; Make the display window the current graphics window.

   WSet, (*infoptr).wid

      ; Set the pointer to the common block.

   thePointer = infoptr
   RETURN
ENDIF

END ;-----------------------------------------------------------------------------



PRO MPI_Plot_Configuration_Events, event

   ; This event handler responds to events comimg from
   ; the Plot Configuration widget via the buttonbase widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /cancel
   ok = Error_Message(/Traceback)
ENDIF
Widget_Control, event.top, Get_UValue=infoptr

   ; Display the graphics.

MPI_Plot_DrawThePlot, infoptr

END ;-----------------------------------------------------------------------------



PRO MPI_Plot, x, y, _Extra=extra, XTitle=xtitle, YTitle=ytitle, Title=title, $
   Overplot=overplot, Color=color, DataColor=datacolor, PSym=psym, XLog=xlog, $
   YLog=ylog

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   RETURN
ENDIF

   ; Common block required for overplotting capability.

COMMON MPI_PLOT_COMMON, infoPtr

   ; Check color keywords. Differences for display depth
   ; and overplotting verses plotting.

Device, Get_Visual_Depth=theDepth
IF theDepth GT 8 THEN BEGIN
   IF Keyword_Set(overplot) THEN BEGIN
      IF N_Elements(color) EQ 0 THEN BEGIN
         IF Ptr_Valid(infoptr) THEN BEGIN
            plotkeywords = (*infoptr).plotconfig->GetKeywords()
            color = plotkeywords.name_datacolor
         ENDIF ELSE color = 'SADDLE BROWN'
       ENDIF
   ENDIF ELSE BEGIN
      IF N_Elements(background) EQ 0 THEN background = 'IVORY'
      IF N_Elements(color) EQ 0 THEN color = 'SADDLE BROWN'
      IF N_Elements(datacolor) EQ 0 THEN datacolor = color
   ENDELSE
ENDIF ELSE BEGIN
   IF Keyword_Set(overplot) THEN BEGIN
      IF N_Elements(color) EQ 0 THEN BEGIN
         IF Ptr_Valid(infoptr) THEN BEGIN
            plotkeywords = (*infoptr).plotconfig->GetKeywords()
            color = plotkeywords.name_datacolor
         ENDIF ELSE color = 'GREEN'
       ENDIF
   ENDIF ELSE BEGIN
      IF N_Elements(background) EQ 0 THEN background = 'GRAY'
      IF N_Elements(color) EQ 0 THEN color = 'GREEN'
      IF N_Elements(datacolor) EQ 0 THEN datacolor = color
   ENDELSE
ENDELSE

   ; Make sure color names are strings.

IF N_Elements(psym) EQ 0 THEN psym = 18 ; No symbol.

   ; Are you overplotting something?

IF Keyword_Set(overplot) EQ 0 THEN BEGIN

      ; Do we have the color right?

   IF Size(color, /TName) NE 'STRING' THEN Message, 'COLOR must be specified as color NAME.', /NoName

      ; Because of common block, only one MPI_PLOT at a time allowed.

;   IF XRegistered('mpi_plot') GT 0 THEN BEGIN
;      ok = Dialog_Message('Only one MPI_PLOT can be used at once. Try overplotting.')
;      RETURN
;   ENDIF

   infoPtr = Ptr_New()

ENDIF ELSE BEGIN

      ; Is there a current plot?

   IF Ptr_Valid(infoptr) EQ 0 THEN BEGIN
      ok = Dialog_Message('There is no current plot to overplot onto. Returning.')
      RETURN
   ENDIF

      ; Sort out the positional parameters.

   IF N_Elements(x) EQ 0 THEN BEGIN
      ok = Dialog_Message('Please supply data for overplotting.')
      RETURN
   ENDIF ELSE BEGIN
      IF N_Elements(y) EQ 0 THEN BEGIN
         dep = x
         indep = Lindgen(N_Elements(x))
      ENDIF ELSE BEGIN
         dep = y
         indep = x
      ENDELSE
   ENDELSE

      ; Create an overplot structure and add it to the info structure.

   struct = {MPI_OVERPLOT, Ptr_New(dep), Ptr_New(indep), Ptr_New(extra), color, psym}
   IF Ptr_Valid((*infoptr).oplotinfo) THEN $
      *(*infoptr).oplotinfo = [*((*infoptr).oplotinfo), struct] ELSE $
      (*infoptr).oplotinfo = Ptr_New([struct])

      ; Draw the plot.

   MPI_Plot_DrawthePlot, infoptr, /Overplot

      ; Allow the clear oplots button to be active.

   Widget_Control, (*infoptr).clearoplotID, Sensitive=1

   RETURN
ENDELSE

   ; Sort out the positional parameters for plotting.

IF N_Elements(x) EQ 0 THEN BEGIN
   x = Findgen(101)
   dep = Sin(x/5)/ Exp(x/50)
   Indep = Findgen(101)
ENDIF ELSE BEGIN
   IF N_Elements(y) EQ 0 THEN BEGIN
      dep = x
      indep = Indgen(N_Elements(x))
   ENDIF ELSE BEGIN
      dep = y
      indep = x
   ENDELSE
ENDELSE

   ; Check keywords.

IF N_Elements(title) EQ 0 THEN title=""
IF N_Elements(xtitle) EQ 0 THEN xtitle=""
IF N_Elements(ytitle) EQ 0 THEN ytitle=""
IF Size(background, /TName) NE 'STRING' THEN Message, 'BACKGROUND must be specified as color NAME.', /NoName
IF Size(color, /TName) NE 'STRING' THEN Message, 'COLOR must be specified as color NAME.', /NoName
IF Size(datacolor, /TName) NE 'STRING' THEN Message, 'DATACOLOR must be specified as color NAME.', /NoName

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.4 THEN haveGif = 1 ELSE haveGIF = 0

   ; Find the data axes ranges.

mindep = Min(dep, Max=maxdep)
minindep = Min(indep, Max=maxindep)

   ; Create the widgets.

tlb = Widget_Base(Column=1, TLB_Size_Events=1, Base_Align_Center=1, Title='MPI_Plot', KBRD_Focus_Events=1)
drawid = Widget_Draw(tlb, XSize=400, YSize=400)
buttonbase = Widget_Base(tlb, Row=1)

   ; Quit button.

button = Widget_Button(buttonbase, Value='Quit', Event_Pro='MPI_Plot_Quit', /Dynamic_Resize)

   ; Print button.

printID = Widget_Button(buttonbase, Value='Print', Event_Pro='MPI_Plot_Print', /Menu, /Dynamic_Resize)
   dummy = Widget_Button(printID, Value='Landscape Orientation', UVALUE='LANDSCAPE')
   dummy = Widget_Button(printID, Value='Portrait Orientation', UVALUE='PORTRAIT')

   ; Save As button.

saveID = Widget_Button(buttonbase, Value='Save As', Event_Pro='MPI_Plot_SaveAs', /Menu, /Dynamic_Resize)
dummy = Widget_Button(saveID, Value='BMP File', UValue='BMP')
IF havegif THEN dummy = Widget_Button(saveID, Value='GIF File', UValue='GIF')
dummy = Widget_Button(saveID, Value='PICT File', UValue='PICT')
dummy = Widget_Button(saveID, Value='PNG File', UValue='PNG')
dummy = Widget_Button(saveID, Value='JPEG File', UValue='JPG')
dummy = Widget_Button(saveID, Value='TIFF File', UValue='TIF')
psID = Widget_Button(saveID, Value="PostScript File", UValue='PS', $
   Event_Pro='MPI_Plot_Postscript')

   ; Clear overplots.

clearoplotID = Widget_Button(buttonbase, Value='Clear Overplots', $
  Event_Pro='MPI_Plot_Clear_Overplots', /Dynamic_Resize, Sensitive=0)

   ; Modify Plot button.

button = Widget_Button(buttonbase, Value='Modify Plot', $
  Event_Pro='MPI_Plot_Configure_Button', /Dynamic_Resize)

   ; Set up a widget to receive the Plot Configuration Events. The
   ; Buttonbase is not being used currently, so is a good choice.

Widget_Control, buttonBase, Event_Pro='MPI_Plot_Configuration_Events'

   ; Create the axis and plot configuration objects.

xaxis = Obj_New("MPI_Axis", Range=[minindep, maxindep],  /XAxis, _Extra=extra, Title=xtitle, $
   Log=Keyword_Set(xlog))
yaxis = Obj_New("MPI_Axis", Range=[mindep, maxdep], /YAxis, _Extra=extra, Title=ytitle, $
   Log=Keyword_Set(ylog))
plotconfig=Obj_New("MPI_PlotConfig", XAxis=xaxis, YAxis=yaxis, _Extra=extra, $
   Title=title, Color=color, DataColor=datacolor, Background=background, PSym=psym, $
   UseDataColor=1)

   ; Realize the program and make the display window the current graphics window.

Widget_Control, tlb, /Realize, Update=0
Widget_Control, drawid, Get_Value=wid

   ; Draw the plot.

WSet, wid
plotkeywords = plotconfig->GetKeywords()
Plot, indep, dep, _Extra=plotkeywords, /NoData
plotkeywords.color = plotkeywords.datacolor
OPlot, indep, dep, _Extra=plotkeywords

Widget_Control, tlb, Update=1

; Inquire about buttonbase sizes. Needed for resizing events.

geom = Widget_Info(buttonbase, /Geometry)
ysize = geom.scr_ysize + 10
xminsize = geom.scr_xsize
yminsize = (StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? 200 : 400

   ; Create an info structure and store it in the UValue of the TLB.

info = {xaxis:xaxis, yaxis:yaxis, wid:wid, dep:dep, indep:indep, plotconfig:plotconfig, $
   buttonbase:buttonbase, drawid:drawid, ysize:ysize, xminsize:xminsize, oplotinfo:Ptr_New(), $
   x:!X, y:!Y, p:!p, clearoplotID:clearoplotID, yminsize:yminsize}
infoPtr = Ptr_New(info, /No_Copy)
Widget_Control, tlb, set_uvalue=infoPtr

XManager, 'mpi_plot', tlb, /No_Block, Cleanup='MPI_Plot_Cleanup', $
   Event_Handler='MPI_Plot_Resize'
END
