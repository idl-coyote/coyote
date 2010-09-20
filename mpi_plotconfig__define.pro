;+
; NAME:
;       MPI_PLOTCONFIG__DEFINE
;
; PURPOSE:
;
;       This is a program for interactively adjusting and keeping track
;       of keywords appropriate for configuring the PLOT command.
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
;       plotConfigObj = Obj_New("MPI_PLOTCONFIG")
;
; INPUT PARAMETERS:
;
;       None.
;
; INPUT KEYWORDS (Sent to the INIT method. The same keywords can be set with the SETPROPERTY method of the object.):
;
;       BACKGROUND - The name of the background color. By default on 24-bit systems: 'IVORY'. ON 8-bit systems 'GRAY'.
;
;       CHARSIZE - The character size of the plot. By default, 1.0.
;
;       CHARTHICK - The character thickness of the plot. By default, 1.0.
;
;       COLOR - The name of the plot color. (This will be the axis color if DATACOLOR is also used.)
;         By default on 24-bit systems: 'SADDLE BROWN'. Uses 'GREEN' on 8-bit systems.
;
;       DATACOLOR - The name of the data color. (Requires use of USEDATACOLOR to be active.) By default
;         on 24-bit systems: 'NAVY'. Uses 'YELLOW' on 8-bit systems.
;
;       _EXTRA - Extra keywords to be passed to MPI_AXIS objects used internally.
;
;       FONT - The type of plot font: -1=Hershey, 0=Hardware, 1=True-Type. By default, !P.FONT.;
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
;       NOAXISINFO - Set this keyword to inhibit axis information on the GUI. By default, 0.
;
;       POSITION - The position of the plot in the plot window in normalized coordinates. By default, [0.20, 0.15, 0.95, 0.95].
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
;      SYMSIZE - The plot symbol size. By default, 1.0.
;
;      SUBTITLE - The plot subtitle. By default, "".
;
;      TITLE - The plot title. By default, "".
;
;      THICK - The plot line thickness. By default, 1.0.
;
;      TICKLEN - The plot tick length. By default, 0.02.
;
;      USEDATACOLOR - Set this keyword to return a DATACOLOR field in the keyword structure. By default, 0.
;
;      XAXIS - An MPI_AXIS object for the X axis. One is created by default, if not provided.
;
;      YAXIS - An MPI_AXIS object for the Y axis. One is created by default, if not provided.
;
; METHOD PROCEDURES:
;
;      GUI - This procedure method displays a graphical user interface that allows the user
;            to change plot configuration parameters. The following keywords can be used:
;
;            ALL_EVENTS - Set this keyword to have an event sent any time something in the
;               GUI changes. The default is to send an event only when the user hits the ACCEPT button.
;               Note that the NOTIFYID keyword must be used to generate events.
;
;            BLOCK - Set this keyword if you want to block the command line. The default is to NOT block the command line.
;
;            DEFAULTFONT - The name of a font to use as the default font.
;
;            GROUP_LEADER - The group leader for this GUI. If this keyword is used, the program will be
;               distroyed when the group leader is destroyed.
;
;            LABELDEFAULTSIZE - The default screen size for a label. All labels are offsets from this size. 55 by default.
;               The purpose of this keyword is to allow the user to modify the look of the GUI if different
;               fonts are used.
;
;            LABELFONT - The name of a font to use for program labels.
;
;            NOTIFYID - A two-element array containing the widget identifier and top-level base ID of a widget
;               designated to receive an event from this program. The event structure will be defined and sent
;               like this:
;
;                    Widget_Control, notifyid[0], Send_Event={ MPI_PLOTCONFIG_EVENT, $
;                                                              ID: notifyid[0], $
;                                                              TOP:notifyid[1], $
;                                                              HANDLER: 0L, $
;                                                              OBJECT: self }
;
;               Most event handlers will be written so that they will get the plot keywords
;               from the plot configuration object and draw the plot. A sample event handler might
;               look like this:
;
;                    PRO MPI_Plot_Configuration_Events, event
;                    Widget_Control, event.top, Get_UValue=info, /No_Copy
;                    WSet, info.wid
;                    plotkeywords = event.object->GetKeywords()
;                    Plot, info.indep, info.dep, _Extra=plotkeywords
;                    Widget_Control, event.top, Set_UValue=info, /No_Copy
;                    END
;
;            XLONGFORM - By default, the X axis information is displayed in "short" form, with only the
;               most relevant information readily available. Other axis information is accessed via buttons.
;               Set this keyword to display the X axis information in a "long" form, in which all the axis
;               information is immediately visible.
;
;            YLONGFORM - By default, the Y axis information is displayed in "short" form, with only the
;               most relevant information readily available. Other axis information is accessed via buttons.
;               Set this keyword to display the Y axis information in a "long" form, in which all the axis
;               information is immediately visible.
;
;      SETPROPERTY - This procedure can be used to set the properties of the plot
;               configuration object without using the graphical user interface. The
;               keywords are identical to those used in the INIT method, above.
;
; METHOD FUNCTIONS:
;
;      GETKEYWORDS - This function method contains no arguments or keywords. It returns a
;            structure, with fields equivalent to PLOT keywords. The idea is that these
;            keywords can be passed directly to the PLOT command using the keyword inheritance
;            mechanism via the _EXTRA keyword to the plot command. A possible sequence of commands
;            might look like this:
;
;               IDL> plotConfigObj = Obj_New("MPI_PLOTCONFIG")   ; Create the plot configuration object.
;               IDL> plotConfigObj->GUI, /Block                  ; Allow the user to configure the plot parameters.
;               IDL> plotKeywords = plotConfigObj->GetKeywords() ; Get the plot keywords.
;               IDL> Plot, x, y, _Extra=plotKeywords             ; Draw the plot in the way the user specified.
;               IDL> Obj_Destroy, plotConfigObj                  ; Destroy the object when finished with it.
;
; PROGRAM NOTES:
;
;      Color Names: Color names are those used with FSC_Color and PickColorName. See the
;         documentation for those programs for instuctions on loading your own colors.
;         To see the default colors and names, type this:
;
;                IDL> color = PickColorName('yellow')
;
;      Working with DataColor: Many people like to have the data color in a line plot
;         different from the axis color. This requires two commands in IDL: a PLOT command
;         with the NODATA keyword set, to draw in the axis color, followed by the OPLOT command,
;         with the data drawn in the data color. Unfortunately, IDL only has a single COLOR keyword
;         to represent both colors. So, you must be a bit resourceful to use this feature.
;
;         The proper sequence of commands to use this feature of the plot configuration object
;         will looks like this. First, initialize the object with the USEDATACOLOR keyword:
;
;             plotConfigObj = Obj_New("MPI_PLOTCONFIG", /UseDataColor) ; Use the DataColor option.
;
;         When you are ready to draw the plot, the keyword structure will have a new field named
;         DataColor. Since this keyword is not recognized by the PLOT command, it will be ignored
;         in the first PLOT command to draw the axes:
;
;             plotKeywords = plotConfigObj->GetKeywords() ; Get the plot keywords.
;             Plot, x, y, _Extra=plotKeywords, /NoData    ; Just draw the axes.
;
;         Next, change the color field to the datacolor field value, and overplot the
;         data onto the axes you just drew:
;
;             plotKeywords.color = PlotKeywords.datacolor
;             OPlot, x, y, _Extra=plotKeywords
;
;         You can see an example of how this is done in the heavily documented example program
;         MPI_PLOT, which you can use as a wrapper for the PLOT command with your own data, if you like.
;
;      Required Programs: The following programs are required to reside in your !PATH. They can be
;         obtained from the Coyote Library:
;
;                     http://www.dfanning.com/programs/adjustposition.pro
;                     http://www.dfanning.com/programs/cw_drawcolor.pro
;                     http://www.dfanning.com/programs/cw_spacer.pro
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/fsc_color.pro
;                     http://www.dfanning.com/programs/fsc_droplist.pro
;                     http://www.dfanning.com/programs/fsc_field.pro
;                     http://www.dfanning.com/programs/fsc_plotwindow.pro
;                     http://www.dfanning.com/programs/mpi_axis__define.pro
;                     http://www.dfanning.com/programs/pickcolorname.pro
;                     http://www.dfanning.com/programs/pswindow.pro
;                     http://www.dfanning.com/programs/tvread.pro
;
; EXAMPLE:
;
;       A heavily documented program, named MPI_PLOT, is supplied with this program.
;       This program not only explains how to use the MPI_PLOTCONFIG__DEFINE program,
;       it can be used as a wrapper program for the PLOT command that you can use with
;       your own data. The program can be downloaded here:
;
;                     http://www.dfanning.com/programs/mpi_plot.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, March 2001.
;       Made a change to the GUI method that fixes a problem I have been having
;          on some Linux machines in widgets not always showing up. 15 July 2003. DWF.
;       Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;       Fixed a problem when users tried to set PSYM to a negative value. 24 June 2008. DWF.
;-
;
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
PRO MPI_PlotConfig_CenterTLB, tlb

; Center the top-level base on the display.

screenSize = Get_Screen_Size()
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig_Error_Message, theMessage, Traceback=traceback, NoName=noName, _Extra=extra

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
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]


   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage, _Extra=extra)
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
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION MPI_PlotConfig::Symbol, number

   ; This method calculates a user-defined symbol for the PSYM keyword.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 18
ENDIF

   ; Error checking.

IF N_Elements(number) EQ 0 THEN RETURN, 18
IF (number GT 18) THEN Message, /NoName, 'Symbol number out of defined range. Returning NO SYMBOL.'
IF number LT 0 THEN BEGIN
    sign = -1 
    number = Abs(number)
ENDIF ELSE BEGIN
    sign = 1
ENDELSE

   ; Define helper variables for creating circles.

phi = Findgen(32) * (!PI * 2 / 32.)
phi = [ phi, phi(0) ]

   ; Use user defined symbol by default.

result = 8

CASE number OF

    0  : result = 3                                                ; Dot
    1  : UserSym, Cos(phi), Sin(phi), /Fill                        ; Filled circle.
    2  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], /Fill       ; Filled upward triangle.
    3  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ], /Fill        ; Filled downward triangle.
    4  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /Fill    ; Filled diamond.
    5  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], /Fill ; Filled square.
    6  : UserSym, cos(phi), sin(phi)                               ; Open circle.
    7  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ]              ; Open upward triangle.
    8  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ]               ; Open downward triangle.
    9  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ]           ; Open diamond.
   10  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ]        ; Open square.
   11  : result = 1                                                ; Plus.
   12  : result = 7                                                ; X.
   13  : result = 2                                                ; Star.
   14  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], /Fill         ; Filled rightfacing triangle.
   15  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], /Fill           ; Filled leftfacing triangle.
   16  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ]                ; Open rightfacing triangle.
   17  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ]                  ; Open leftfacing triangle.
   18  : result  = 0                                               ; No symbol at all.

ENDCASE

RETURN, result * sign
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::AxisObjectEvents, event

; This method responds to events from the axis objects, if they are
; associated with the plot object.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::SetPlotKeywords, plotkeywords

; This method sets the plot properties from a structure of keywords.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN
ENDIF

tagnames = Tag_Names(plotkeywords)
plotkeywords.color = plotkeywords.name_color
plotkeywords.background = plotkeywords.name_background
index = Where(tagnames EQ 'DATACOLOR', count)
IF count GT 0 THEN plotkeywords.datacolor = plotkeywords.name_datacolor

END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::Destroy

; This method destroys the top-level base of the compound widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN
ENDIF

IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, /Destroy
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::GetTLB

; This method returns the top-level base of the compound widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

RETURN, self.tlb
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::SendEvent

; This method sends an event to the widget to be notified, if one exists.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN
ENDIF

IF Total(self.notifyID) GT 0 THEN BEGIN

   thisEvent = { MPI_PLOTCONFIG_EVENT, $
                 ID: self.notifyID[0], $
                 TOP:self.notifyID[1], $
                 HANDLER: 0L, $
                 OBJECT: self }

   Widget_Control, self.notifyID[0], Send_Event=thisEvent
ENDIF

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::GetKeywords

; This method constructs a structure containing fields with values
; set to the current state of the object.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self->UpdateObject
struct = Create_Struct( $
   'background', FSC_Color(self.background, !P.Background), $
   'charthick', self.charthick, $
   'charsize', self.charsize, $
   'color', FSC_Color(self.color, !D.Table_Size-2), $
   'font', self.font, $
   'linestyle', self.linestyle, $
   'position', self.position, $
   'psym', self->Symbol(self.psym) * self.noline, $
   'subtitle', self.subtitle, $
   'symsize', self.symsize, $
   'thick', self.thick, $
   'ticklen', self.ticklen, $
   'title', self.title, $
   'name_color', self.color, $
   'name_background', self.background )

   ; Add axis color information, if required.

IF self.usedatacolor THEN $
   struct = Create_Struct(struct, 'datacolor', FSC_Color(self.dataColor, !D.Table_Size-3), $
      'name_datacolor', self.datacolor)

   ; Done unless axis information is required.

IF self.noaxisinfo THEN RETURN, struct

IF Obj_Valid(self.xaxis) THEN BEGIN
   xstruct = self.xaxis->GetKeywords()
   struct = Create_Struct(struct, xstruct)
ENDIF

IF Obj_Valid(self.yaxis) THEN BEGIN
   ystruct = self.yaxis->GetKeywords()
   struct = Create_Struct(struct, ystruct)
ENDIF

RETURN, struct
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::UpdateObject

; This method simply gathers all the information in the graphical
; user interface (that doesn't update itself) and updates the self
; object. If fields are undefined, they are set to their default values.

IF Obj_Valid(self.titleObj) NE 0 THEN BEGIN
   IF N_Elements(self.titleObj->Get_Value()) EQ 0 THEN BEGIN
      value = ""
      self.titleObj->Set_Value, value
   ENDIF ELSE value = self.titleObj->Get_Value()
self.title = value
ENDIF

IF Obj_Valid(self.subtitleObj) NE 0 THEN BEGIN
   IF N_Elements(self.subtitleObj->Get_Value()) EQ 0 THEN BEGIN
      value = ""
      self.subtitleObj->Set_Value, value
   ENDIF ELSE value = self.subtitleObj->Get_Value()
self.subtitle = value
ENDIF

IF Obj_Valid(self.thickObj) NE 0 THEN BEGIN
   IF Finite(self.thickObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.thickObj->Set_Value, value
   ENDIF ELSE value = self.thickObj->Get_Value()
self.thick = value
ENDIF

IF Obj_Valid(self.symsizeObj) NE 0 THEN BEGIN
   IF Finite(self.symsizeObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.symsizeObj->Set_Value, value
   ENDIF ELSE value = self.symsizeObj->Get_Value()
self.symsize = value
ENDIF

IF Obj_Valid(self.charthickObj) NE 0 THEN BEGIN
   IF Finite(self.charthickObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.charthickObj->Set_Value, value
   ENDIF ELSE value = self.charthickObj->Get_Value()
self.charthick = value
ENDIF

IF Obj_Valid(self.ticklenObj) NE 0 THEN BEGIN
   IF Finite(self.ticklenObj->Get_Value()) EQ 0 THEN BEGIN
      value = 0.020
      self.ticklenObj->Set_Value, value
   ENDIF ELSE value = self.ticklenObj->Get_Value()
self.ticklen = value
ENDIF

IF Obj_Valid(self.charsizeObj) NE 0 THEN BEGIN
   IF Finite(self.charsizeObj->Get_Value()) EQ 0 THEN BEGIN
      value = 1.0
      self.charsizeObj->Set_Value, value
   ENDIF ELSE value = self.charsizeObj->Get_Value()
self.charsize = value
ENDIF

END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::CheckEvent, event

; This event handler method checks to see if an event
; should be passed on.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Make sure all values are current by updating.

self->UpdateObject
IF self.all_events THEN self->SendEvent

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetPlotPosition, event

; This event handler method sets the plot position.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=tlbsizes
self.position = AdjustPosition(self.position, Group_Leader=event.top, Cancel=cancelled, $
   XOffset=offsets[0] + 0.7*tlbsizes[0], YOffset=offsets[1] + 0.4*tlbsizes[1])

IF cancelled THEN RETURN, 0 ELSE RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetTitle, event

; This event handler method sets the plot title.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.title = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetSubTitle, event

; This event handler method sets the plot subtitle.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.subtitle = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetFont, event

; This event handler method sets the plot font style.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.font = event.index - 1

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetLinestyle, event

; This event handler method sets the plot line style.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Can't have no symbols and no lines. If this is the
   ; case, set the symbol droplist to dots.

IF self.lineStyleID->GetSelection() EQ 'No Line' THEN BEGIN
   IF self.psymID->GetSelection() EQ 'No Symbol' THEN BEGIN
      self.psym = self.lastvalidsymbol
      self.psymID->SetIndex, self.lastvalidsymbol
      self.noline = 1
   ENDIF
   self.noline = 1
ENDIF ELSE BEGIN
   self.lastvalidline = event.index
   self.noline = -1
ENDELSE

self.linestyle = event.index

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetSymbol, event

; This event handler method sets the plot symbol type.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Can't have no symbols and no lines. If this is the
   ; case, set the line style droplist to solid lines.

IF self.psymID->GetSelection() EQ 'No Symbol' THEN BEGIN
   IF self.lineStyleID->GetSelection() EQ 'No Line' THEN BEGIN
      self.lineStyleID->SetIndex, self.lastvalidline
      self.linestyle = self.lastvalidline
      self.noline = 1
   ENDIF
ENDIF ELSE self.lastvalidsymbol = event.index

self.psym = event.index

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetBackgroundColor, event

; This event handler method sets the plot background color.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.background = event.color

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetColor, event

; This event handler method sets the plot color.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.color = event.color

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetDataColor, event

; This event handler method sets the axis color.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.datacolor = event.color

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetLineThick, event

; This event handler method sets the plot line thickness.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.thick = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetCharThick, event

; This event handler method sets the plot character thickness.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.charthick = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION MPI_PlotConfig::SetSymSize, event

; This event handler method sets the plot symbol size.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.symsize = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetCharSize, event

; This event handler method sets the plot symbol size.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.charsize = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SymbolWithLine, event

; This event handler method changes the psym field so that symbols are
; connected with lines.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

IF event.select EQ 1 THEN self.noline = -1 ELSE self.noline = 1

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::SetTickLen, event

; This event handler method sets the plot tick length.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self.ticklen = *event.value

RETURN, self->CheckEvent(event)
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::DismissDialog, event

; This event handler method destroys the widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

Widget_Control, self.tlb, /Destroy

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::AcceptDialog, event

; This event handler method updates the dialog and destroys the widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self->UpdateObject
IF Obj_Valid(self.xaxis) THEN self.xaxis->UpdateObject
IF Obj_Valid(self.yaxis) THEN self.yaxis->UpdateObject

self->SendEvent
self->Destroy

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::ApplyDialog, event

; This event handler method updates the dialog and destroys the widget.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

self->UpdateObject
IF Obj_Valid(self.xaxis) THEN self.xaxis->UpdateObject
IF Obj_Valid(self.yaxis) THEN self.yaxis->UpdateObject

self->SendEvent

RETURN, 0
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::GUI, $

; This method displays a graphical user interface for changing the values in the object.

   All_events=all_events, $             ; Set this keyword to get all of the events generated.
   Block=block, $                       ; Set this keyword if you want to block the command line.
   Defaultfont=defaultfont, $           ; The name of a font to use as the default font.
   Group_Leader=group_leader, $         ; The group leader for this GUI.
   LabelDefaultSize=labeldefaultsize, $ ; The default screen size for a label. All labels are offsets from this size. 55 by default.
   LabelFont=labelfont, $               ; The name of a font to use for program labels.
   NotifyID=notifyID, $                 ; The widget ID and top-level base to notify upon an event.
   XLongForm=xlongform, $               ; Set this keyword to display the X axis information in a long form.
   YLongForm=ylongform                  ; Set this keyword to display the Y axis information in a long form.


   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN
ENDIF

   ; Only one version of this program at a time.

IF XRegistered('mpi_plotconfig') GT 0 THEN RETURN

   ; Check keyword arguments.

self.all_events = Keyword_Set(all_events)
IF N_Elements(notifyID) NE 0 THEN self.notifyID = notifyID

   ; Set up the default label size.

IF N_Elements(labeldefaultsize) EQ 0 THEN labeldefaultsize = 55

   ; Set widget font names.

;thisOS = StrUpCase(!Version.os_family)
;CASE thisOS OF
;   'WINDOWS': BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = 'Times*Bold'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = 'MS Sans Serif*10'
;   END
;   'MACOS': BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = 'Times*Bold'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = 'Times*10'
;   END
;   ELSE: BEGIN
;      IF N_Elements(labelfont) EQ 0 THEN self.labelfont = 'Helvetica*12'
;      IF N_Elements(defaultfont) EQ 0 THEN self.defaultfont = 'Helvetica*12'
;   END
;ENDCASE
;Widget_Control, Default_Font=self.defaultfont

   ; Create the widget interface.

IF N_Elements(group_leader) EQ 0 THEN BEGIN
   tlb = Widget_Base(Column=1, Title='Plot Options...', $ ;Base_Align_Center=1
      Pro_Set_Value="MPI_PLOT_SET_VALUE", Func_Get_Value="MPI_PLOT_GET_VALUE", Map=0)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Column=1, Title='Plot Options...', $ ;Base_Align_Center=1, $
      Pro_Set_Value="MPI_PLOT_SET_VALUE", Func_Get_Value="MPI_PLOT_GET_VALUE", $
      Group_Leader=group_leader, Map=0)
ENDELSE

mainbase = Widget_Base(tlb, Column=1, Frame=1, UValue=self, Base_Align_Center=1)


   label = Widget_Label(mainbase, Value='Plot Options', Font=self.labelfont)

col1 = Widget_Base(mainbase, Column=1)
row1 = Widget_Base(col1, Row=1)

   titleID = FSC_Field(row1, LabelSize=labeldefaultsize, Title='Plot Title: ', $
      Label_Left=1, Value=self.title, XSize=40, Object=titleObj, $
      UValue={method:'SETTITLE', object:self}, $
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

    IF self.usedatacolor EQ 0 THEN colorID = CW_Drawcolor(row1, Color=self.color, LabelText='Plot Color: ', Title='Plot Color:', $
      UValue={method:'SETCOLOR', object:self}, Label_Left=1, LabelSize=labeldefaultsize + 12, Object=colorObj)

row2 = Widget_Base(col1, Row=1)

   subtitleID = FSC_Field(row2, LabelSize=labeldefaultsize, Title='Subtitle: ', $
      Label_Left=1, Value=self.subtitle, XSize=40, Object=subtitleObj, $
      UValue={method:'SETSUBTITLE', object:self}, $
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

    IF self.usedatacolor EQ 0 THEN bcolorID = CW_Drawcolor(row2, Color=self.background, LabelText='Background:', Title='Background Color', $
      UValue={method:'SETBACKGROUNDCOLOR', object:self}, Label_Left=1, LabelSize=labeldefaultsize+12, Object=bcolorObj)

   droprow = Widget_Base(mainbase, Row=1, Base_Align_Center=1)

   fonttype = ['Hershey', 'Hardware', 'True-Type']
   CASE self.font OF
      -1: fontIndex = 0
       0: fontIndex = 1
       1: fontIndex = 2
   ENDCASE
   fontID = FSC_Droplist(droprow, Value=fonttype, Index=fontIndex, $
      UValue={method:'SetFont', object:self}, Title='Font Type:', Space=[1,1])

   styles = ['Solid', 'Dotted', 'Dashed', 'Dash Dot', 'Dash Dot Dot', 'Long Dash', 'No Line']
   IF self.noline EQ 1 THEN self.linestyle = 6
   linestyleID = FSC_Droplist(droprow, Value=styles, Index=self.linestyle, $
      UValue={method:'SetLineStyle', object:self}, Title='Line Style:', Space=[1,1])

   symbols = ['Dot', 'Filled Circle', 'Filled Upward Triangle', 'Filled Downward Triangle', $
              'Filled Diamond', 'Filled Square', 'Open Circle', 'Open Upward Triangle', $
              'Open Downward Triangle', 'Open Diamond', 'Open Square', 'Plus Sign', 'X', $
              'Star', 'Filled Right Triangle', 'Filled Left Triangle', 'Open Right Triangle', $
              'Open Left Triangle', 'No Symbol']

   psymID = FSC_Droplist(droprow, Value=symbols, Index=0 > Abs(self.psym) < 19, $
      UValue={method:'SetSymbol', object:self}, Title='Symbol:', Space=[1,1])


   row4 = Widget_Base(mainbase, Row=1)
   column1 = Widget_Base(row4, Column=1)
   column2 = Widget_Base(row4, Column=1)
   column3 = Widget_Base(row4, Column=1)

   thickID = FSC_Field(column1, Value=Float(self.thick), Title='Line Thickness: ', $
      Label_Left=1, Object=thickObj, XSize=6, Decimal=3, $
      UValue={method:'SETLINETHICK', object:self}, LabelSize=labeldefaultsize*2, $   ; ++mgs/--mgs
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

   charthickID = FSC_Field(column1, Value=Float(self.charthick), Title='Character Thickness: ', $
      Label_Left=1, Object=charthickObj, XSize=6, Decimal=3, $
      UValue={method:'SETCHARTHICK', object:self}, LabelSize=labeldefaultsize*2, $    ; ++mgs/--mgs
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

   symsizeID = FSC_Field(column2, Value=Float(self.symsize), Title='Symbol Size: ', $
      Label_Left=1, Object=symsizeObj, XSize=6, Decimal=3, $
      UValue={method:'SETSYMSIZE', object:self}, LabelSize=labeldefaultsize+25, $    ; ++mgs/--mgs
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

   charsizeID = FSC_Field(column2, Value=Float(self.charsize), Title='Character Size: ', $
      Label_Left=1, Object=charsizeObj, XSize=6, Decimal=3, $
      UValue={method:'SETCHARSIZE', object:self}, LabelSize=labeldefaultsize+25, $      ; ++mgs/--mgs
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

   ticklenID = FSC_Field(column3, Value=Float(self.ticklen), Title='Tick Length: ', $
      Label_Left=1, Object=ticklenObj, XSize=6, Decimal=3, $
      UValue={method:'SETTICKLEN', object:self}, $
      /Cr_Only, Event_Pro="MPI_PlotConfig_Widget_Events")

   button = Widget_Button(column3, Value='Plot Position', $
      Event_Pro="MPI_PlotConfig_Widget_Events", UValue={method:'SETPLOTPOSITION', object:self})


   IF self.usedatacolor THEN BEGIN
      row5 = Widget_Base(mainbase, Row=1)
      colorID = CW_Drawcolor(row5, Color=self.color, LabelText='Axis Color: ', Title='Axis Color:', $
         UValue={method:'SETCOLOR', object:self}, Label_Left=1, Object=colorObj)
      bcolorID = CW_Drawcolor(row5, Color=self.background, LabelText='   Background Color: ', Title='Background Color', $
         UValue={method:'SETBACKGROUNDCOLOR', object:self}, Label_Left=1, Object=bcolorObj)
      acolorID = CW_Drawcolor(row5, Color=self.datacolor, LabelText='   Data Color: ', Title='Data Color', $
         UValue={method:'SETDATACOLOR', object:self}, Label_Left=1, Object=dcolorObj)
   ENDIF

   ; The X and Y axis information if required and available.

   IF self.noaxisinfo EQ 0 THEN BEGIN
      IF self.all_events THEN event_pro = 'MPI_PlotConfig_Widget_Events' ELSE event_pro="
      IF Obj_Valid(self.xaxis) THEN BEGIN
         xaxisID = self.xaxis->GUI(tlb, Short=1-Keyword_Set(xlongform), Event_Pro=event_pro)
         Widget_Control, xaxisID, Set_UValue={method:'AXISOBJECTEVENTS', object:self}
      ENDIF
      IF Obj_Valid(self.yaxis) THEN BEGIN
         yaxisID = self.yaxis->GUI(tlb, Short=1-Keyword_Set(ylongform), Event_Pro=event_pro)
         Widget_Control, yaxisID, Set_UValue={method:'AXISOBJECTEVENTS', object:self}
      ENDIF
   ENDIF

  buttonbase = Widget_Base(tlb, Row=1, Align_Center=1)
  button = Widget_Button(buttonbase, Value='Close', UValue={Object:self, Method:'DISMISSDIALOG'})
  IF Keyword_Set(block) THEN BEGIN
     button = Widget_Button(buttonbase, Value='Accept', UValue={Object:self, Method:'ACCEPTDIALOG'})
  ENDIF ELSE BEGIN
     button = Widget_Button(buttonbase, Value='Apply', UValue={Object:self, Method:'APPLYDIALOG'})
  ENDELSE

MPI_PlotConfig_CenterTLB, tlb

Widget_Control, tlb, /Realize


   ; Fill out object fields.

self.tlb = tlb
self.titleObj = titleObj
self.subtitleObj = subtitleObj
self.thickObj = thickObj
self.symsizeObj = symsizeObj
self.ticklenObj = ticklenObj
self.charthickObj = charthickObj
self.charsizeObj = charsizeObj
self.colorObj = colorObj
IF self.usedatacolor THEN self.dcolorObj = dcolorObj
self.bcolorObj = bcolorObj
self.lineStyleID = linestyleID
self.psymID = psymID

Widget_Control, tlb, Map=1
XManager, 'mpi_plotconfig', tlb, Event_Handler='MPI_PlotConfig_Widget_Events', No_Block = 1 - Keyword_Set(block)

END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig::SetProperty, $

; This method allows you to set the property of the object
; without going through the graphical user interface.

   Background=background, $     ; The name of the background color
   Charsize=charsize, $         ; The character size of the plot.
   Charthick=charthick, $       ; The character thickness of the plot.
   Color=color, $               ; The name of the plot color.
   Datacolor=datacolor, $       ; The name of the data color.
   Font=font, $                 ; The type of plot font: -1=Hershey, 0=Hardware, 1=True-Type.
   Linestyle=linestyle, $       ; The plot linestyle.
   NoAxisInfo=noaxisinfo, $     ; Set this keyword to inhibit axis information on the GUI.
   Position=position, $         ; The position of the plot in the plot window.
   PSym=psym, $                 ; The plot symbol value.
   Symsize=symsize, $           ; The plot symbol size.
   Subtitle=subtitle, $         ; The plot subtitle.
   Title=title, $               ; The plot title.
   Thick=thick, $               ; The plot line thickness.
   Ticklen=ticklen, $           ; The plot tick length.
   UseDataColor=usedatacolor, $ ; Set this keyword to return a DATACOLOR field in the keyword structure.
   XAxis=xaxis, $               ; An MPI_AXIS object for the X axis.
   YAxis=yaxis                  ; An MPI_AXIS object for the Y axis.


IF N_Elements(background) NE 0 THEN self.background = background
IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
IF N_Elements(charthick) NE 0 THEN self.charthick = charthick
IF N_Elements(color) NE 0 THEN self.color = color
IF N_Elements(datacolor) NE 0 THEN self.datacolor = datacolor
IF N_Elements(font) NE 0 THEN self.font = font
IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
IF N_Elements(noaxisinfo) NE 0 THEN self.noaxisinfo = Keyword_Set(noaxisinfo)
IF N_Elements(position) NE 0 THEN self.position = position
IF N_Elements(psym) NE 0 THEN self.psym = psym
IF self.psym LT 0 THEN BEGIN
   self.noline = -1
   self.psym = Abs(self.psym)
ENDIF ELSE IF self.psym EQ 18 THEN self.noline = -1 ELSE self.noline = 1
IF N_Elements(subtitle) NE 0 THEN self.subtitle = subtitle
IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
IF N_Elements(title) NE 0 THEN self.title = title
IF N_Elements(thick) NE 0 THEN self.thick = thick
IF N_Elements(ticklen) NE 0 THEN self.ticklen = ticklen
IF N_Elements(usedatacolor) NE 0 THEN self.usedatacolor = Keyword_Set(usedatacolor)
IF N_Elements(xaxis) NE 0 THEN BEGIN
   IF self.createaxis THEN Obj_Destroy, self.xaxis
   self.xaxis = xaxis
ENDIF
IF N_Elements(yaxis) NE 0 THEN BEGIN
   IF self.createaxis THEN Obj_Destroy, self.yaxis
   self.yaxis = yaxis
ENDIF

self->UpdateObject
END ;-----------------------------------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig::Init, $

; This is the initialization method for the MPI_PLOTCONFIG object.

   Background=background, $     ; The name of the background color
   Charsize=charsize, $         ; The character size of the plot.
   Charthick=charthick, $       ; The character thickness of the plot.
   Color=color, $               ; The name of the plot color.
   Datacolor=datacolor, $       ; The name of the data color. (Requires use of USEDATACOLOR to be active.)
   _Extra=extra, $              ; Extra keywords to be passed to MPI_AXIS objects.
   Font=font, $                 ; The type of plot font: -1=Hershey, 0=Hardware, 1=True-Type.
   Linestyle=linestyle, $       ; The plot linestyle.
   NoAxisInfo=noaxisinfo, $     ; Set this keyword to inhibit axis information on the GUI.
   Position=position, $         ; The position of the plot in the plot window.
   PSym=psym, $                 ; The plot symbol value.
   Symsize=symsize, $           ; The plot symbol size.
   Subtitle=subtitle, $         ; The plot subtitle.
   Title=title, $               ; The plot title.
   Thick=thick, $               ; The plot line thickness.
   Ticklen=ticklen, $           ; The plot tick length.
   UseDataColor=usedatacolor, $ ; Set this keyword to return a DATACOLOR field in the keyword structure.
   XAxis=xaxis, $               ; An MPI_AXIS object for the X axis.
   YAxis=yaxis                  ; An MPI_AXIS object for the Y axis.

; This method performs the initialization of the object.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = MPI_PlotConfig_Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Check for keywords and parameters.

usedatacolor = Keyword_Set(usedatacolor)
IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
IF N_Elements(charthick) EQ 0 THEN charthick = 1.0
IF N_Elements(font) EQ 0 THEN font = !P.Font
IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
IF N_Elements(position) EQ 0 THEN position = [0.20, 0.15, 0.95, 0.95]
IF N_Elements(psym) EQ 0 THEN psym = 18
IF N_Elements(subtitle) EQ 0 THEN subtitle = ""
IF N_Elements(symsize) EQ 0 THEN symsize = 1.0
IF N_Elements(title) EQ 0 THEN title = ""
IF N_Elements(thick) EQ 0 THEN thick = 1.0
IF N_Elements(ticklen) EQ 0 THEN ticklen = 0.02

   ; Use different colors in 8-bit environment.

Device, Get_Visual_Depth=theDepth
IF theDepth GT 8 THEN BEGIN
   IF N_Elements(background) EQ 0 THEN background = 'IVORY'
   IF N_Elements(color) EQ 0 THEN color = 'SADDLE BROWN' ELSE IF N_Elements(dataColor) EQ 0 THEN dataColor=color
   IF N_Elements(datacolor) EQ 0 THEN datacolor = 'NAVY'
ENDIF ELSE BEGIN
   IF N_Elements(background) EQ 0 THEN background = 'GRAY'
   IF N_Elements(color) EQ 0 THEN color = 'GREEN' ELSE IF N_Elements(dataColor) EQ 0 THEN dataColor=color
   IF N_Elements(datacolor) EQ 0 THEN datacolor = 'YELLOW'
ENDELSE

   ; Populate the object.

self.background = background
self.charsize = charsize
self.charthick = charthick
self.color = color
self.datacolor = datacolor
self.font = font
self.linestyle = linestyle
self.noaxisinfo = Keyword_Set(noaxisinfo)
self.position = position
self.psym = psym
IF self.psym LT 0 THEN BEGIN
   self.noline = -1
   self.psym = Abs(self.psym)
ENDIF ELSE IF self.psym EQ 18 THEN self.noline = -1 ELSE self.noline = 1
self.symsize = symsize
self.subtitle = subtitle
self.title = title
self.thick = thick
self.ticklen = ticklen
self.usedatacolor = usedatacolor

   ; Do we have to create the axis objects ourself?

IF Obj_Valid(xaxis) THEN BEGIN
   self.xaxis = xaxis
   self.createaxis = 0
ENDIF ELSE BEGIN
   self.xaxis = Obj_New('MPI_Axis', /XAxis, _Extra=extra)
   self.createaxis = 1
ENDELSE

IF Obj_Valid(yaxis) THEN BEGIN
   self.yaxis = yaxis
   self.createaxis = 0
ENDIF ELSE BEGIN
   self.yaxis = Obj_New('MPI_Axis', /YAxis, _Extra=extra)
   self.createaxis = 1
ENDELSE

RETURN, 1
END ;---------------------------------------------------------------------------------------------------



FUNCTION MPI_PlotConfig_GetValue, id

; The is the function that returns the "value" of the compound widget.
; In this case, the "value" is the current keyword structure.

child = Widget_Info(id, /Child)
Widget_Control, child, Get_UValue=self
self->UpdateObject
stateStruct = self->GetKeywords()
RETURN, stateStruct
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig_SetValue, value

; This is the procedure that sets the "value" of the compound widget.
; At the moment, I don't know what it would mean to "set" the value,
; so I am leaving this unimplemented.

ok = Dialog_Message('Setting the value of the compound widget is undefined. Returning...')
END ;-----------------------------------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig_Widget_Events, event

; The main event handler for the compound widget. It reacts
; to "messages" in the UValue of the text widget.
; The message indicates which object method to call. A message
; consists of an object method and the self object reference.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = Dialog_Message('Specified functionality currently  NOT IMPLEMENTED.')
   RETURN
ENDIF

Widget_Control, event.ID, Get_UValue=theMessage
event = Call_Method(theMessage.method, theMessage.object, event)

END ;-----------------------------------------------------------------------------------------------------------------------------




PRO MPI_PlotConfig::Cleanup

; The cleanup method.

Obj_Destroy, self.colorObj
Obj_Destroy, self.bcolorObj
IF self.usedatacolor THEN Obj_Destroy, self.dcolorObj

IF self.createaxis THEN BEGIN
   Obj_Destroy, self.xaxis
   Obj_Destroy, self.yaxis
ENDIF

END ;---------------------------------------------------------------------------------------------------



PRO MPI_PlotConfig__Define

struct = { MPI_PLOTCONFIG, $         ; The MPI_PLOTCONFIG object class.

   ; Properties of the PLOT command.

           datacolor: "", $          ; The name of the axis color.
           background: "", $         ; The name of the background color.
           charsize: 0.0, $          ; The plot charsize.
           charthick: 0.0, $         ; The thickness of the characters.
           color: "", $              ; The name of the plot color.
           font: 0, $                ; The plot font flag.
           linestyle: 0, $           ; The plot linestyle.
           position: FltArr(4), $    ; The position in the plot window.
           psym: 0L, $               ; The plot symbol.
           symsize: 0.0, $           ; The plot symbol size.
           subtitle: "", $           ; The plot subtitle.
           title: "", $              ; The plot title.
           thick: 0.0, $             ; The plot line thickness.
           ticklen: 0.0, $           ; The plot tick length.
           event_pro: "", $          ; The external event procedure.
           event_func: "", $         ; The external event function.

   ; Widget identifiers.

            tlb: 0L, $                 ; The top-level base widget identifier.
            titleObj: Obj_New(), $     ; The plot title field object identifier.
            subTitleObj: Obj_New(), $  ; The plot subtitle field object identifier.
            psymID: Obj_New(), $       ; The plot symbol droplist object identifier.
            linestyleID: Obj_New(), $  ; The line style droplist object identifier.
            thickObj: Obj_New(), $     ; The line thickness field object identifier.
            symsizeObj: Obj_New(), $   ; The plot symbol field object identifier.
            ticklenObj: Obj_New(), $   ; The tick length field object identifier.
            charthickObj: Obj_New(), $ ; The character thickness field object identifier.
            charsizeObj: Obj_New(), $  ; The character size field object identifier.
            colorObj: Obj_New(), $     ; The plot color object identifier.
            bcolorObj: Obj_New(), $    ; The plot background color object identifier.
            dcolorObj: Obj_New(), $    ; The plot data color object identifier.

   ; Other information.

            createaxis: 0L, $        ; A flag that indicates this program created the axis objects.
            lastvalidsymbol: 0L, $   ; The index of the last valid symbol.
            lastvalidline: 0L, $     ; The index of the last valid line style.
            xaxis: Obj_New(), $      ; The X AXIS object of class MPI_AXIS.
            yaxis: Obj_New(), $      ; The Y AXIS object of class MPI_AXIS.
            labelfont: "", $         ; The font name for the program labels.
            defaultfont: "", $       ; The default program font name.
            noline: 0L, $            ; A flag to tell if symbols should be conntect by lines (-1) on not (1).
            usedatacolor: 0L, $      ; A flag that indicates the user wants to include information on axis color.
            all_events: 0L, $        ; A flag that sends all events to the specified event handler.
            noaxisinfo: 0L, $        ; A flag to prevent axis information from being displayed or presented.
            notifyID: LonArr(2)  $   ; The widget ID and top-level base of the widget to notify.
           }
END ;---------------------------------------------------------------------------------------------------

