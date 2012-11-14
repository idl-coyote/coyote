;+
; NAME:
;   FSC_PSCONFIG__DEFINE
;
; PURPOSE:
;
;   The purpose of this program is to implement an object that
;   can keep track of--and allow the user to change--the current
;   configuration of the PostScript device.
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
;   General programming.
;
; DOCUMENTATION:
;
;   Complete documentation for the FSC_PSCONFIG object, including
;   keyword and method descriptions, and example programs using the object
;   can be found on the Coyote's Guide to IDL Programming web page:
;
;     http://www.idlcoyote.com/programs/docs/fsc_psconfig.html
;
;   Or, if you would prefer, you can download a self-contained PDF file:
;
;     http://www.idlcoyote.com/programs/docs/fsc_psconfig.pdf
;
; KEYWORDS:
;
;   Any keyword accepted by the FSC_PSCONFIG object can be used with
;   this program. Here are a few of the most popular keywords.
;
;   Bits_per_Pixel - The number of image bits saved for each image pixel: 2, 4, or 8. The default is 8.
;   Color - Set this keyword to select Color PostScript output. Turned on by default.
;   Decomposed - Set this keyword to 0 to select indexed color and to 1 to select decomposed color.
;   DefaultSetup - Set this keyword to the "name" of a default style. Current styles (you can easily
;     create and add your own to the source code) are the following:
;
;       "System (Portrait)" - The normal "default" system set-up. Also, "System".
;       "System (Landscape)" - The normal "default" landscape system set-up.
;       "Centered (Portrait)" - The window centered on the page. Also, "Center" or "Centered".
;       "Centered (Landscape)" - The window centered on the landscape page. Also, "Landscape".
;       "Square (Portrait)" - A square plot, centered on the page.
;       "Square (Landscape)" - A square plot, centered on the landscape page.
;       "Figure (Small)" - A small encapsulated figure size, centered on page. Also, "Encapsulated" or "Encapsulate".
;       "Figure (Large)" - A larger encapsulated figure size, centered on page. Also, "Figure".
;       "Color (Portrait)" - A "centered" plot, with color turned on. Also, "Color".
;       "Color (Landscape)" - A "centered" landscape plot, with color turned on.
;
;   Directory - Set this keyword to the name of the starting directory. The current directory is used by default.
;   Encapsulated - Set this keyword to select Encapsulated PostScript output. Turned off by default.
;   European - This keyword has been depreciated in favor of METRIC.
;   Filename - Set thie keyword to the name of the PostScript file. The default is "idl.ps".
;   Inches - Set this keyword to indicate sizes and offsets are in inches as opposed to centimeters. Set by Metric keyword by default.
;   Landscape - Set this keyword to select Landscape page output. Portrait page output is the default.
;   Language_Level - Set this keyword to select the Language Level interpreter. Default is 1.
;   Metric - Set this keyword to indicate metric mode (i.e., A4 page and centimeter units). Turned off by default.
;   PageType - Set this keyword to the "type" of page. Possible values are:
;       "Letter" - 8.5 by 11 inches. (Default, unless the Metric keyword is set.)
;       "Legal" - 8.5 by 14 inches.
;       "Ledger" - 11 by 17 inches.
;       "A4" - 21.0 by 29.7 centimeters. (Default, if the Metric keyword is set.)
;   XOffset - Set this keyword to the X Offset. Uses "System (Portrait)" defaults. (Note: offset calculated from lower-left corner of page.)
;   XSize - Set this keyword to the X size of the PostScript "window". Uses "System (Portrait)" defaults.
;   YOffset - Set this keyword to the Y Offset. Uses "System (Portrait)" defaults. (Note: offset calculated from lower-left corner of page.)
;   YSize - Set this keyword to the Y size of the PostScript "window". Uses "System (Portrait)" defaults.
;
;   In addition, the following keywords can be used:
;
;   CANCEL -- An output keyword that will be set to 1 if the user
;   chooses the Cancel button on the form. It will be 0 otherwise.
;
;   FONTINFO -- Set this keyword is you wish to have font information
;   appear on the form. The default is to not include font information.
;
;   FONTTYPE -- Set this keyword to a named variable that will indicate
;   the user's preference for font type. Values will be -1 (Hershey fonts),
;   0 (hardware fonts), and 1 (true-type fonts). This keyword will always
;   return -1 unless the FONTINFO keyword has also been set.
;
;   GROUP_LEADER -- Set this keyword to a widget identifier of the widget
;   you wish to be a group leader for this program.
;
; EXAMPLE:
;
;   A simple sequence of using the object would look something like this:
;
;     psObject = Obj_New("FSC_PSCONFIG")
;     psObject->GUI
;     psKeywords = psObject->GetKeywords()
;     thisDevice = !D.Name
;     Set_Plot, 'PS'
;     Device, _Extra=psKeywords
;     cgImage, image
;     Device, /Close_File
;     Set_Plot, thisDevice
;     Obj_Destroy, psObject
;
;  Note that the object can also be called from the PS_CONFIG interface:
;
;     psKeywords = PSConfig()
;
; OTHER PROGRAMS NEEDED:
;
;   The following programs are required to run this one:
;
;     fsc_droplist.pro
;     fsc_fileselect.pro
;     fsc_field.pro
;     fsc_plotwindow
;
; MODIFICATIONS:
;
;   Written by David W. Fanning, 31 January 2000.
;   Added capability to call GUI methods when the current graphics device
;      doesn't support windows. Device is restored when the GUI exits. 11 May 2000. DWF.
;   Changed the default value for the Color keyword to 1. 16 May 2000. DWF.
;   Fixed a bug where filename changed when switching Setups. 8 AUG 2000. DWF.
;   Fixed a bug when saving setup in Landscape mode. 8 AUG 2000. DWF.
;   Added the ability to Get and Set the object's name via the SetProperty
;      and a very abbreviated GetProperty method. Also added a GetName method. 26 SEP 2000. DWF.
;   Fixed a problem in which the proper configuration was not restored if in Landscape mode. 20 Nov 2000. DWF.
;   Made a number of modifications at the request of Martin Schultz. 4 Dec 2000. DWF.
;   Fixed a bug when setting file  and directory names with the SetProperty method. 18 Dec 2000. DWF.
;   Fixed a small problem in initializing the page size properly. 3 Jan 2001. DWF.
;   Corrected a problem that resulted from a change to FSC_DROPLIST. 6 Jan 2001. DWF.
;   Added the ability to restore the font type instead of always reverting to !P.Font. 7 Jan 2001. DWF.
;   Increased the length of the file/directory name fields. 7 Jan 2001. DWF.
;   Fixed another problem with Landscape mode interacting with A4 paper size. 7 Jan 2001. DWF.
;   Seems I only half fixed the previous problem. :-( 26 April 2001. DWF.
;   Forgot to update program to reflect change in FSC_FIELD. Fixed 26 April 2001. DWF.
;   Changed BOOKMAN keyword to BKMAN to avoid conflict with BOOKSTYLE keyword. 26 April 2001. DWF.
;   Modified the System Defaults to say "None" if none is used. Improved documentation. 10 September 2001. DWF.
;   Added the ability to specify a filename at the same time as a Default Setup. 10 September 2001. DWF.
;   Fixed a small problem in not setting new page sizes appropriately. 22 May 2002. DWF.
;   Fixed a problem that occurred when the Accept button was not named "Accept". 6 May 2003.DWF.
;   Whoops! I was a bit overly agressive on that last fix. :-( 17 July 2003. DWF.
;   Fixed a problem with setting page types when using the DEFAULTSETUP keyword. 31 July 2003. DWF.
;   Fixed a problem with turning encapsulation on in the GUI. Renamed ENCAPSULATE keyword ENCAPSULATED
;      to avoid obvious errors. 31 July 2003. DWF.
;   Removed obsolete STR_SEP and replaced with STRSPLIT. 27 Oct 2004. DWF.
;   Now honoring EUROPEAN keyword when setting system default setups in the INIT method. 12 Nov 2004. DWF.
;   Added CMYK output option 24 August 2007. Assumes LANGUAGE_LEVEL=2 printer. L. Anderson.
;   Fixed a problem with the filename on WINDOWS computers coming back with forward slashes instead of
;       backward slashes. 20 May 2008. DWF.
;   Modified the program to return as the default, ISOLATIN1=1. 18 July 2008. DWF.
;   Fixed a problem with filenames when a DEFAULTSETUP was used with it. 12 Nov 2008. DWF.
;   Changed default window size when LANDSCAPE keyword is set. 10 April 2009. DWF.
;   Changed the default window size for PORTRAIT mode to be a bit larger. 10 April 2009. DWF.
;   Updated for IDL 7.1 and 24-bit color PostScript support. 24 May 2009. DWF.
;   Added PAGETYPE field to returned structure to allow PostScript page type to be determined. 8 August 2009. DWF.
;   Fixed a problem with 24-bit color support that allowed only IDL 7 versions to work correctly. 20 Sept 2009. DWF.
;   Added a LANGUAGE_LEVEL keyword. 13 Dec 2010. DWF.
;   Added the FONTYPE value to the keyword return structure. 14 Dec 2010. DWF.
;   Modified the return structure to turn landscape mode off and set offsets to zero if in 
;        encapsulated mode. 19 Feb 2011. DWF.
;   Changes to handle inability to create raster files from PS encapsulated files in 
;        landscape mode. Also removed changes of 19 Feb 2011 as no longer needed. 26 Aug 2011. DWF.
;   The PAGETYPE was not getting set properly in the return keywords when the Metric 
;        option was selected on the GUI. 12 October 2011. DWF.
;   The program now remembers the last directory you used and will start in that
;       directory, unless told otherwise. 26 Oct 2011. DWF.
;   Parsing of full filename failing. Fixed 27 Oct 2011. DWF.
;-

;******************************************************************************************;
;  Copyright (c) 2008-2011, by Fanning Software Consulting, Inc.                           ;
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

FUNCTION FSC_PSCONFIG_RStrPos, Expr, SubStr, Pos

; This function finds the last occurance of a substring
; in a string. It was made obsolete in IDL 5.3. It is
; provided here for backwards compatibility. It normally
; resides in the IDL obsolete directory.

; PURPOSE:
;  This function finds the last occurrence of a substring within
;  an object string. If the substring is found in the expression,
;  RSTRPOS returns the character position of the match, otherwise
;  it returns -1.
;
; CATEGORY:
;  String processing.
;
; CALLING SEQUENCE:
;        Result = RSTRPOS(Expr, SubStr [, Pos])
;
; INPUTS:
;       Expr:  The expression string in which to search for the substring.
;  SubStr: The substring to search for.
;
; OPTIONAL INPUTS:
;  Pos:  The character position before which the search is begun.
;           If Pos is omitted, the search begins at the last character
;           of Expr.
;
; OUTPUTS:
;        Returns the position of the substring, or -1 if the
;   substring was not found within Expr.
;

  ON_ERROR, 2
  N = N_PARAMS()
  if (n lt 2) then message, 'Incorrect number of arguments.'

  ; Is expr an array or a scalar? In either case, make a result
  ; that matches.
  if (size(expr, /n_dimensions) eq 0) then result = 0 $
  else result = make_array(dimension=size(expr,/dimensions), /INT)

  RSubStr = STRING(REVERSE(BYTE(SubStr))) ; Reverse the substring

  for i = 0, n_elements(expr) - 1 do begin
    Len = STRLEN(Expr[i])
    IF (N_ELEMENTS(Pos) EQ 0) THEN Start=0 ELSE Start = Len - Pos

    RString = STRING(REVERSE(BYTE(Expr[i]))) ; Reverse the string

    SubPos = STRPOS(RString, RSubStr, Start)
    IF SubPos NE -1 THEN SubPos = Len - SubPos - STRLEN(SubStr)
    result[i] = SubPos
  endfor

  RETURN, result
END ;-------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG_Error_Message, theMessage, Traceback=traceback, $
   NoName=noName

; Handles program errors.

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string."
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   FOR j=0,N_Elements(traceback)-1 DO Print, traceback[j]
ENDIF

RETURN, answer
END ;---------------------------------------------------------------------------------------------



PRO FSC_PSCONFIG_Events, event

; This is the main event hander for the program. Its purpose
; is to dispatch events to the appropriate event handler
; methods by parsing the user value of the widget causing
; the event.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message()
   RETURN
ENDIF

   ; Get the instructions from the widget causing the event and
   ; act on them.

thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF

   'FSC_DROPLIST_EVENT' : BEGIN
         Widget_Control, event.id, Get_UValue=instructions
      ENDCASE

   'FSC_FIELD_EVENT': BEGIN
         Widget_Control, event.id, Get_UValue=instructions
      ENDCASE

   'WIDGET_BUTTON': BEGIN
         Widget_Control, event.id, Get_UValue=instructions
      ENDCASE

   'WIDGET_DRAW': BEGIN
         Widget_Control, event.id, Get_UValue=instructions
      ENDCASE

   ELSE: BEGIN ; FSC_PLOTWINDOW events.
         Widget_Control, event.id, Get_UValue=object
         instructions = object->GetUValue()
      ENDCASE

ENDCASE

   ; Turn the status light to OFF, if necessary.

IF instructions.method NE 'HELP' AND instructions.method NE 'REVERT' $
   AND instructions.method NE 'ACCEPT' THEN instructions.object->StatusLight, 0
IF instructions.method NE 'NULL' THEN $
   Call_Method, instructions.method, instructions.object, event

END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG_Normalize, range, Position=position

; The program creates a scaling and translating factor for
; positioning objects in the view coordinate system.

On_Error, 1
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
    position=Float(position)
range = Float(range)

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END ;-------------------------------------------------------------------------



PRO FSC_PSCONFIG_Help_Event, event

; Destroy the HELP widget window.

Widget_Control, event.top, /Destroy
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG_Restore_Device, id
Widget_Control, id, Get_UValue=thisDevice
Set_Plot, thisDevice
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Accept, event

; This method handles the event from the ACCEPT or APPLY buttons.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Get all the values from the form and load the appropriate
   ; self fields.

self.cancel = 0
self.pageTypeSet = self.pageTypeID->GetSelection()
self.landscapeSet = self.orientationID->GetIndex()
self.inchesSet = self.unitsID->GetIndex()
self.cmykSet = self.cmykID->GetIndex()
self.colorSet = self.colorID->GetIndex()
self.encapsulationSet = self.encapsulationID->GetIndex()
self.langLevelSet = self.langLevelID->GetSelection()
self.previewSet = self.previewID->GetIndex()
IF self.encapsulationSet EQ 0 THEN self.previewSet = 0
self.bitsSet = self.bitsID->GetSelection()
IF Float(!Version.Release) GE 7.1 THEN self.decomposedSet = self.decomposedID->GetIndex()

IF N_Elements(self.xsizeID->Get_Value()) EQ 0 THEN BEGIN
   IF self.inchesSet THEN xsize = 1.0 ELSE xsize = 2.54
   self.xsizeID->Set_Value, xsize
ENDIF ELSE xsize = self.xsizeID->Get_Value()
self.xsizeSet = xsize

IF N_Elements(self.ysizeID->Get_Value()) EQ 0 THEN BEGIN
   IF self.inchesSet THEN ysize = 1.0 ELSE ysize = 2.54
   self.ysizeID->Set_Value, ysize
ENDIF ELSE ysize = self.ysizeID->Get_Value()
self.ysizeSet = ysize

IF N_Elements(self.xoffsetID->Get_Value()) EQ 0 THEN BEGIN
   xoffset = 0.0
   self.xoffsetID->Set_Value, xoffset
ENDIF ELSE xoffset = self.xoffsetID->Get_Value()
self.xoffsetSet = xoffset

IF N_Elements(self.yoffsetID->Get_Value()) EQ 0 THEN BEGIN
   yoffset = 0.0
   self.yoffsetID->Set_Value, yoffset
ENDIF ELSE yoffset = self.yoffsetID->Get_Value()
self.yoffsetSet = yoffset

self.plotID->SetWindowLocation, xsize, ysize, xoffset, yoffset

   ; Get the font information if it is on the form.

IF self.fontInfo THEN BEGIN
   ;fonttype = self.fontTypeID->GetIndex()
   ;self.fonttypeSet = fonttype - 1
   self.fontsizeSet = self.fontsizeID->GetSelection()

   values = self.fontnameID->GetValues()
   selection = self.fontnameID->GetSelection()
   IF StrUpCase(selection) EQ 'AVANTGARDE' THEN self.AVANTGARDE = 1 ELSE self.AVANTGARDE = 0
   IF StrUpCase(selection) EQ 'BOOKMAN' THEN self.BOOKMAN = 1 ELSE self.BOOKMAN = 0
   IF StrUpCase(selection) EQ 'COURIER' THEN self.COURIER = 1 ELSE self.COURIER = 0
   IF StrUpCase(selection) EQ 'HELVETICA' THEN self.HELVETICA = 1 ELSE self.HELVETICA = 0
   IF StrUpCase(selection) EQ 'PALATINO' THEN self.PALATINO = 1 ELSE self.PALATINO = 0
   IF StrUpCase(selection) EQ 'SCHOOLBOOK' THEN self.SCHOOLBOOK = 1 ELSE self.SCHOOLBOOK = 0
   IF StrUpCase(selection) EQ 'SYMBOL' THEN self.SYMBOL = 1 ELSE self.SYMBOL = 0
   IF StrUpCase(selection) EQ 'TIMES' THEN self.TIMES = 1 ELSE self.TIMES = 0
   IF StrUpCase(selection) EQ 'ZAPFCHANCERY' THEN self.ZAPFCHANCERY = 1 ELSE self.ZAPFCHANCERY = 0
   IF StrUpCase(selection) EQ 'ZAPFDINGBATS' THEN self.ZAPFDINGBATS = 1 ELSE self.ZAPFDINGBATS = 0
   index = Where(StrUpCase(values) EQ StrUpCase(selection))
   self.fontnameSet = values[index[0]]

   self.isolatinSet = self.isolatinID->GetIndex()
   IF Obj_Valid(self.set_fontID) THEN self.set_fontSet = self.Set_FontID->Get_Value()
   self.truetypeSet = self.truetypeID->GetIndex()
   self.fontTypeSet = self.fontTypeID->GetIndex() - 1
ENDIF

   ; Get the filename information.

self.filenameID->GetProperty, File=filename, Directory=directory

; Save the name of the directory, if you can.
DEFSYSV, '!cgPostScript_LastDir', EXISTS=exists
IF exists THEN BEGIN
    IF File_Test(directory, /DIRECTORY) THEN !cgPostScript_LastDir = directory 
ENDIF ELSE BEGIN
    IF File_Test(directory, /DIRECTORY) THEN DEFSYSV, '!cgPostScript_LastDir', directory
ENDELSE
self.filenameSet = filename
self.directorySet = directory
self.fullFilenameSet=self.filenameID->GetFileName()

   ; If this is WINDOWS, then forward slashes need to be converted to backward slashes.
   
IF StrUpCase(!Version.OS_FAMILY) EQ 'WINDOWS' THEN BEGIN
   self.directorySet = StrJoin( StrSplit(self.directorySet, '/', /Regex, /Extract, /Preserve_Null), '\')
   self.fullFilenameSet = StrJoin( StrSplit(self.fullFilenameSet, '/', /Regex, /Extract, /Preserve_Null), '\')
ENDIF

   ; Set the status light if this is the APPLY button

Widget_Control, event.id, Get_UValue=buttonInfo
IF N_Elements(buttonInfo) NE 0 THEN $
   IF buttonInfo.method EQ 'APPLY' THEN self->StatusLight, 1

   ; Save this configuration.

self->SaveConfiguration

   ; Destroy the widget if this is a modal widget.

IF N_Elements(buttonInfo) NE 0 THEN $
   IF buttonInfo.method EQ 'ACCEPT' AND Widget_Info(self.tlb, /Valid_ID) THEN BEGIN
   Widget_Control, self.tlb, /Destroy
   self.noblock = 0
ENDIF

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Cancel, event

; If the cancel button was clicked, set the cancel flag to 1 and
; destroy the widget.

self.cancel = 1
self.noblock = 0
Widget_Control, event.top, /Destroy
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::ClearStyles, event

; This event handler method responds to the Clear button by setting
; turning all the font style buttons off..

self->UpDateFontStyle, /Clear
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Color, event

; This event handler method responds to changes in the Color droplist.


self.plotID->SetColor, event.index

; Set sensitivity of cmyk button

self.cmykID->Sensitive, event.index
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::CMYK, event

; This event handler method responds to changes in the CMYK droplist.

; If event.index is 1, set the Language Level to 2, otherwise to 1.
IF event.index EQ 1 $
    THEN self.langLevelID->SetSelection, 2 $
    ELSE self.langLevelID->SetSelection, 1

END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::Construct_Full_Filename

; This method returns the fully-qualified filename. Checks to be
; sure the last character in the directory name is not a directory
; specifier.

Catch, theError
IF theError NE 0 THEN BEGIN
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN, ""
ENDIF

   ; Get the correct directory separator.

CASE StrUpCase(!Version.OS_Family) OF
   'WINDOWS' : sep = '\'    ; PCs
   'MACOS'   : sep = ':'    ; Macintoshes
   'VMS'     : sep = ']'    ; VMS machines
   ELSE      : sep = '/'    ; Unix machines
ENDCASE

IF StrUpCase(!Version.OS_Family) NE "VMS" THEN BEGIN
   length = StrLen(self.directorySet)
   WHILE StrMid(self.directorySet, length-1, 1) EQ sep DO BEGIN
      self.directorySet = StrMid(self.directorySet, 0, length-1)
      length = StrLen(self.directorySet)
   ENDWHILE
ENDIF

RETURN, Filepath(Root_Dir=self.directorySet, self.filenameSet)
END ;-------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::DefaultList

; This method defines the default settings values in the Defaults droplist.
; To add your own defaults, start by adding the new default name here. Then
; add your default setup to the SetDefault method below.

defaultlist = [ 'None', $
                'System (Portrait)', $
                'System (Landscape)', $
                'Centered (Portrait)', $
                'Centered (Landscape)', $
                'Square (Portrait)', $
                'Square (Landscape)', $
                'Figure (Small)', $
                'Figure (Large)', $
                'Color (Portrait)', $
                'Color (Landscape)' ]

RETURN, defaultList
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Defaults, event

; This event handler method responds to changes in the Defaults droplist.

self->SetDefault, *event.selection
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Encapsulate, event

; This method responds to a change in the Encapsulation droplist widget.
IF event.index THEN BEGIN
    self.previewID->Sensitive, 1 
    self.landscapeSet = 0
    self.encapsulationSet = 1
    self->UpdateDisplay
    self.orientationID->Sensitive, 0
    
ENDIF ELSE BEGIN
    self.previewID->Sensitive, 0
    self.encapsulationSet = 0
    self->UpdateDisplay
    self.orientationID->Sensitive, 1
ENDELSE

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::FontType, event

; This event handler method responds to changes in the Font Type droplist.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Font styles, etc. can only be on if hardware or
   ; true-type fonts are selected.

CASE event.index OF

   0: BEGIN
   Widget_Control, self.fontnameID->GetID(), Sensitive=0
   Widget_Control, self.stylebaseID, Sensitive=0
   Widget_Control, self.clearbuttonID, Sensitive=0
   Widget_Control, self.fontSizeID->GetID(), Sensitive=0
   Widget_Control, self.truetypeID->GetID(), Sensitive=0
   Widget_Control, self.isolatinID->GetID(), Sensitive=0
   IF Obj_Valid(self.set_fontID) THEN Widget_Control, self.set_fontID->GetID(), Sensitive=0
   ENDCASE

   ELSE: BEGIN
   Widget_Control, self.fontnameID->GetID(), Sensitive=1
   Widget_Control, self.stylebaseID, Sensitive=1
   Widget_Control, self.clearbuttonID, Sensitive=1
   Widget_Control, self.fontSizeID->GetID(), Sensitive=1
   Widget_Control, self.truetypeID->GetID(), Sensitive=1
   Widget_Control, self.isolatinID->GetID(), Sensitive=1
   IF Obj_Valid(self.set_fontID) THEN Widget_Control, self.set_fontID->GetID(), Sensitive=1
   ENDCASE

ENDCASE

fontname = *event.selection
CASE StrUpCase(fontname) OF
   "TRUE-TYPE": BEGIN
      self.truetypeID->SetIndex, 1
      self.trueTypeSet = 1
      self.fontTypeSet = 1
   ENDCASE
   "HARDWARE" : BEGIN
      self.trueTypeSet = 0
      self.fontTypeSet = 0
      ENDCASE
   ELSE: BEGIN
      self.trueTypeSet = 0
      self.fontTypeSet = -1
      self.truetypeID->SetIndex, 0
   ENDCASE
ENDCASE

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::MetricStyle, event

; This event handler sets and unset the "Metric Style" button.

IF event.select EQ 1 THEN BEGIN

   Widget_Control, event.top, Update=0
   self.metric = 1
   IF self.inchesSet EQ 1 THEN BEGIN
      sizes = self->GetSizes()
      self.xsizeSet = sizes[0] * 2.54
      self.ysizeSet = sizes[1] * 2.54
      self.xoffsetSet = sizes[2] * 2.54
      self.yoffsetSet = sizes[3] * 2.54
   ENDIF
   self.inchesSet = 0
   self.pageTypeSet = 'A4'
   self.pageType = 'A4'
   self.plotID->SetUnits, 'Centimeters'
   Widget_Control, event.top, Update=1

ENDIF ELSE BEGIN

   Widget_Control, event.top, Update=0
   self.metric = 0
   IF self.inchesSet EQ 0 THEN BEGIN
      sizes = self->GetSizes()
      self.xsizeSet = sizes[0] / 2.54
      self.ysizeSet = sizes[1] / 2.54
      self.xoffsetSet = sizes[2] / 2.54
      self.yoffsetSet = sizes[3] / 2.54
   self.inchesSet = 1
   self.pageType = 'LETTER'
   self.pageTypeSet = 'LETTER'
   self.plotID->SetUnits, 'Inches'
   Widget_Control, event.top, Update=1

   ENDIF

ENDELSE
self->UpdateDisplay
self.plotID->CenterPlot
END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::GetKeywords, FontType=fonttype

; This method creates and returns a structure with the PostScript Device
; keywords as fields in the structure.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN, {Cancel:1}
ENDIF

  ; I am shielding the user from the strangeness of PostScript
  ; Landscape mode, in which the X and Y offsets are rotated
  ; 180 degrees. Thus, in the form, the offsets are calculated from
  ; the lower-left corner in both Landscape and Portrait mode.
  ; This means I have to monkey around to get the proper offsets
  ; if the form is in Landscape mode.

IF self.landscapeSet EQ 1 THEN BEGIN
    pageDims = self->PageDimensions()
       ; Switch x and y offsets
    yoffset = pageDims[1] - self.xoffsetSet
    xoffset = self.yoffsetSet
ENDIF ELSE BEGIN
    xoffset = self.xoffsetSet
    yoffset = self.yoffsetSet
ENDELSE


filename = self -> Construct_Full_Filename()

   ; Create the basic structure.

struct = { $
   bits_per_pixel: Fix(self.bitsSet), $
   cmyk: self.cmykSet, $
   color: self.colorSet, $
   encapsulated: self.encapsulationSet, $
   filename: filename, $
   font_size: Fix(self.fontSizeSet), $
   inches: self.inchesSet, $
   isolatin1: self.isolatinSet, $
   language_level: self.langLevelSet, $
   preview: self.previewSet, $
   tt_font: self.truetypeSet, $
   xoffset: xoffset, $
   xsize: self.xsizeSet, $
   yoffset: yoffset, $
   ysize: self.ysizeSet, $
   pagetype: StrUpCase(self.pageType), $
   fonttype: self.fonttypeSet}

   ; Addition of CMYK keyword implies LANGUAGE_LEVEL keyword set to 2.
   
IF self.cmykSet THEN struct.language_level = 2
   
   ; Can add DECOMPOSED keyword if IDL 7.1 and higher.
IF Float(!Version.Release) GE 7.1 THEN struct = Create_Struct(struct, 'decomposed', self.decomposedSet)

   ; Return the font type information to the user.

fonttype = self.fonttypeSet

   ; Depending upon which widgets are set, we have to add
   ; additional fields to the structure.

IF self.landscapeSet THEN BEGIN
   struct = Create_Struct(struct, 'landscape', 1)
   struct = Create_Struct(struct, 'portrait', 0)
ENDIF ELSE BEGIN
   struct = Create_Struct(struct, 'portrait', 1)
   struct = Create_Struct(struct, 'landscape', 0)
ENDELSE
IF self.set_fontSet NE "" THEN struct = Create_Struct(struct, 'set_font', self.set_font)

   ; What about font keywords?

IF self.avantgarde THEN struct = Create_Struct(struct, 'avantgarde', 1)
IF self.bookman THEN struct = Create_Struct(struct, 'bkman', 1)
IF self.courier THEN struct = Create_Struct(struct, 'courier', 1)
IF self.helvetica THEN struct = Create_Struct(struct, 'helvetica', 1)
IF self.palatino THEN struct = Create_Struct(struct, 'palatino', 1)
IF self.schoolbook THEN struct = Create_Struct(struct, 'schoolbook', 1)
IF self.symbol THEN struct = Create_Struct(struct, 'symbol', 1)
IF self.times THEN struct = Create_Struct(struct, 'times', 1)
IF self.zapfdingbats THEN struct = Create_Struct(struct, 'zapfdingbats', 1)
IF self.zapfchancery THEN struct = Create_Struct(struct, 'zapfchancery', 1)

   ; What about style keywords?

IF self.fontStyleSet[0] THEN struct = Create_Struct(struct, 'bold', 1) ELSE struct = Create_Struct(struct, 'bold', 0)
IF self.fontStyleSet[1] THEN struct = Create_Struct(struct, 'book', 1) ELSE struct = Create_Struct(struct, 'book', 0)
IF self.fontStyleSet[2] THEN struct = Create_Struct(struct, 'demi', 1) ELSE struct = Create_Struct(struct, 'demi', 0)
IF self.fontStyleSet[3] THEN struct = Create_Struct(struct, 'italic', 1) ELSE struct = Create_Struct(struct, 'italic', 0)
IF self.fontStyleSet[4] THEN struct = Create_Struct(struct, 'light', 1) ELSE struct = Create_Struct(struct, 'light', 0)
IF self.fontStyleSet[5] THEN struct = Create_Struct(struct, 'medium', 1) ELSE struct = Create_Struct(struct, 'medium', 0)
IF self.fontStyleSet[6] THEN struct = Create_Struct(struct, 'narrow', 1) ELSE struct = Create_Struct(struct, 'narrow', 0)
IF self.fontStyleSet[7] THEN struct = Create_Struct(struct, 'oblique', 1) ELSE struct = Create_Struct(struct, 'oblique', 0)

; If the user is doing encapsulated PostScript, then landscape mode must be off and
; offsets must be set to zero.
;IF struct.encapsulated THEN BEGIN
;
;    struct.portrait = 1
;    struct.landscape = 0
;    struct.xoffset = 0
;    struct.yoffset = 0
;
;ENDIF

   ; Return the keyword stucture.

RETURN, struct
END ;--------------------------------------------------------------------------------




FUNCTION FSC_PSConfig::GetName

; This method returns the "name" of the object.

RETURN, self.name
END ;--------------------------------------------------------------------------------



PRO FSC_PSConfig::GetProperty, Name=name

; This GetProperty method is set up only to be able to obtain
; the name of the object. This makes the object compatible with
; Martin Shultz's container object. All other keywords are swallowed
; silently. Note that the GetName method can also be used.

name = self.name
END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::GetSizes

RETURN, [self.xsizeID->Get_Value(), self.ysizeID->Get_Value(), $
   self.xoffsetID->Get_Value(), self.yoffsetID->Get_Value()]
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::GUI, Group_Leader=leader, NoBlock=noblock, Cancel=cancel, $
   FontInfo=fontinfo, Accept_Button_Name=acceptButtonName, FontType=fonttype

; This method displays the graphical user interface without font information.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Can't run this program in a device that doesn't support windows.

self.thisDevice = !D.Name
IF (!D.Flags AND 256) EQ 0 THEN BEGIN
   thisOS = StrUpCase(!Version.OS_Family)
   CASE thisOS OF
      'MACOS': Set_Plot, 'Mac'
      'WINDOWS': Set_Plot, 'Win'
      ELSE: Set_Plot, 'X'
   ENDCASE
ENDIF

   ; If the user wants font information, then call the GUIFont method.

IF Keyword_Set(fontinfo) THEN BEGIN
   self->GUIFont, Group_Leader=leader, NoBlock=noblock, Cancel=cancel, $
      Accept_Button_Name=acceptButtonName, FontType=fonttype
   RETURN
ENDIF

IF N_Elements(acceptButtonName) EQ 0 THEN BEGIN
   IF Keyword_Set(noblock) THEN BEGIN
      acceptButtonName = 'Apply'
   ENDIF ELSE BEGIN
      acceptButtonName = 'Accept'
   ENDELSE
ENDIF

   ; Only one GUI at a time with the same name.

IF XRegistered('fsc_psconfig_' + self.name) GT 0 THEN RETURN

   ;Set default values.

self.fontInfo = 0
CW_FIELD_SIZE = 5
IF Keyword_Set(noblock) THEN BEGIN
   cancelButVal = 'Dismiss'
ENDIF ELSE BEGIN
   cancelButVal = 'Cancel'
ENDELSE

self.fontTypeSet = !P.Font
IF Keyword_Set(noblock) THEN self.cancel = 0 ELSE self.cancel = 1

   ; Modal TLB if possible.

IF self.name NE "" THEN tlbTitle = self.name + ": Configure PostScript Device" ELSE $
   tlbTitle = "Configure PostScript Device"

IF N_Elements(leader) EQ 0 OR Keyword_Set(noblock) THEN BEGIN
   tlb = Widget_Base(Title=tlbTitle, Column=1, BASE_ALIGN_CENTER=1)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=tlbTitle, Column=1, $
      Group_Leader=leader, /Modal, BASE_ALIGN_CENTER=1)
ENDELSE

self.tlb = tlb

   ; Widget sub-bases.

   topBase = Widget_Base(tlb, Row=1, UValue=self.thisDevice, $
    Kill_Notify='FSC_PSConfig_Restore_Device', BASE_ALIGN_TOP=1)
    
      leftTopBase = Widget_Base(topBase, Column=1)
         bbbase = Widget_Base(leftTopBase)
         l = Widget_Label(bbbase, value=' Properties ', xoffset=5)
         lgeo    = widget_info(l, /geometry) 
         controlBase = Widget_Base(bbbase, Column=1, /Frame, YOffset=lgeo.ysize/2, YPAD=10, XPAD=10)
         l = Widget_Label(bbbase, value=' Properties ', xoffset=5)
         
      middleTopBase = Widget_Base(topBase, Column=1, BASE_ALIGN_LEFT=1)
   
   ; Create droplist widgets.

values = ['Letter', 'A4', 'Legal', 'Ledger']
self.pageTypeID = FSC_Droplist(controlBase, Value=values, Title='Page Size:', $
   UValue={Method:'PageSize', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.pageTypeID->SetSelection, self.pageTypeSet
self.pagetype = self.pageTypeSet

values = ['Portrait', 'Landscape']
self.orientationID = FSC_Droplist(controlBase, Value=values, Title='Orientation:', $
   UValue={Method:'Orientation', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.orientationID->SetIndex, self.landscapeSet
IF self.encapsulationSet THEN self.orientationID -> Sensitive, 0

values = ['Centimeters', 'Inches']
self.unitsID = FSC_Droplist(controlBase, Value=values, Title='Units:', $
   UValue={Method:'Units', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.unitsID->SetIndex, self.inchesSet

values = ['Off', 'On']
self.encapsulationID = FSC_Droplist(controlBase, Value=values, Title='Encapsulation:', $
   UValue={Method:'ENCAPSULATE', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.encapsulationID->SetIndex, self.encapsulationSet

values = ['None', 'Bitmap Preview', 'TIFF Preview']
self.previewID = FSC_Droplist(controlBase, Value=values, Title='Preview Mode:', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.previewID->SetIndex, self.previewSet

values = ['Off', 'On']
self.colorID = FSC_Droplist(controlBase, Value=values, Title='Color Output:', $
   UValue={Method:'COLOR', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.colorID->SetIndex, self.colorSet

values = ['Off', 'On']
self.cmykID = FSC_Droplist(controlBase, Value=values, Title = 'CMYK Output:', $
    UValue={Method:'CMYK', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.cmykID->SetIndex, self.cmykSet
IF ~self.colorset THEN self.cmykID->Sensitive, 0

values = ['1','2']
self.langlevelID = FSC_Droplist(controlBase, Value=values, Title = 'Language Level:', $
    UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.langlevelID->SetSelection, self.langlevelSet


values = ['8', '4', '2']
self.bitsID = FSC_Droplist(controlBase, Value=values, Title='Bits Per Image Pixel:', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.bitsID->SetSelection, self.bitsSet

IF Float(!Version.Release) GE 7.1 THEN BEGIN
    values = ['Off', 'On']
    self.decomposedID = FSC_Droplist(controlBase, Value=values, Title = 'Color Decomposition:', $
        UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
    self.decomposedID->SetIndex, self.decomposedSet
    self.bitsID->SetSelection, 8
    self.colorID->SetIndex, 1
    self.colorSet = 1
ENDIF

values = self->DefaultList()
self.defaultsID = FSC_Droplist(controlBase, Value=values, Title='Setups:', $
   UValue={Method:'DEFAULTS', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.defaultsID->SetSelection, self.defaultsSet

euroBase = Widget_Base(controlBase, row=1, /NonExclusive)
euroSetUPID = Widget_Button(euroBase, Value='Metric Style', UValue={Method:'METRICSTYLE', Object:self})
IF self.metric THEN Widget_Control, euroSetUpID, /Set_Button
IF self.inchesSet THEN unit = 'INCHES' ELSE unit = 'CENTIMETERS'

windowDims = self->PageDimensions()
shortside = Min(windowDims, Max=longside)
IF self.landscapeSet THEN BEGIN
   windowsize = [self.xoffsetSet/longside, self.yoffsetSet/shortside, $
      self.xsizeSet/longside + self.xoffsetSet/longside, self.ysizeSet/shortside + self.yoffsetSet/shortside]
ENDIF ELSE BEGIN
   windowsize = [self.xoffsetSet/shortside, self.yoffsetSet/longside, $
      self.xsizeSet/shortside + self.xoffsetSet/shortside, self.ysizeSet/longside + self.yoffsetSet/longside]
ENDELSE

   ; Create directions widgets.

directionBase = Widget_Base(middleTopBase, Column=1, SPACE=10)
bbbase = Widget_Base(directionBase)
l = Widget_Label(bbbase, value=' Directions ', xoffset=5)
lgeo    = widget_info(l, /geometry) 
labelBase = Widget_Base(bbbase, Column=1, /Frame, YOffset=lgeo.ysize/2, YPAD=10, XPAD=10, Base_Align_Center=1)
l = Widget_Label(bbbase, value=' Directions ', xoffset=5)
label = Widget_Label(labelBase, Value='Move or resize the PostScript "plot" on the', /Align_Left)
label = Widget_Label(labelBase, Value='page with left or right buttons. Click the middle', /Align_Left)
label = Widget_Label(labelBase, Value='button in the window to center plot on the page.', /Align_Left)

   ; Create size and offset windows.
   
sizebase = Widget_Base(middleTopBase, Column=1, SPACE=10)
bbbase = Widget_Base(sizebase)
l = Widget_Label(bbbase, value=' Page Size ', xoffset=5)
lgeo    = widget_info(l, /geometry) 
sizeitBase = Widget_Base(bbbase, Column=2, /Frame, YOffset=lgeo.ysize/2, YPAD=10, XPAD=10)
l = Widget_Label(bbbase, value=' Page Size ', xoffset=5)
xsizeID = FSC_Field(sizeitBase, Title=' X Size: ', Value=self.xsizeSet, Object=xsizeObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.xsizeID = xsizeObj
ysizeID = FSC_Field(sizeitBase, Title=' Y Size: ', Value=self.ysizeSet, Object=ysizeObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.ysizeID = ysizeObj

xoffsetID = FSC_Field(sizeitBase, Title=' X Offset: ', Value=self.xoffsetSet, Object=xoffsetObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.xoffsetID = xoffsetObj

yoffsetID = FSC_Field(sizeitBase, Title=' Y Offset: ', Value=self.yoffsetSet, Object=yoffsetObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.yoffsetID = yoffsetObj

   ; Set up TABing between fields.

self.xsizeID->SetTabNext, self.ysizeID->GetTextID()
self.ysizeID->SetTabNext, self.xoffsetID->GetTextID()
self.xoffsetID->SetTabNext, self.yoffsetID->GetTextID()
self.yoffsetID->SetTabNext, self.xsizeID->GetTextID()

   ; Create filename widgets.

fileBase = Widget_base(middleTopBase, Row=1)
         bbbase = Widget_Base(fileBase)
         l = Widget_Label(bbbase, value=' Filename ', xoffset=5)
         lgeo    = widget_info(l, /geometry) 
         filenameBase = Widget_Base(bbbase, Column=1, /Frame, YOffset=lgeo.ysize/2, YPAD=5)
         l = Widget_Label(bbbase, value=' Filename ', xoffset=5)

dummy = FSC_FileSelect(filenameBase, Filename=self.filenameSet, $
   Directory=self.directorySet, XSIZE=30, ObjectRef=filenameID)
self.filenameID = filenameID

geo = Widget_Info(filenameBase, /GEOMETRY)
Widget_Control, labelbase, XSIZE=geo.scr_xsize
Widget_Control, sizeitbase, XSIZE=geo.scr_xsize


   ; Create the plot window.
rightTopBase = Widget_Base(topBase, Column=1, Base_Align_Center=1)
IF StrUpCase(!VERSION.OS_FAMILY) EQ 'WINDOWS' THEN BEGIN
   bbbase = Widget_Base(rightTopBase)
   thisLabel = ' Interactive Plot Placement' 
   l = Widget_Label(bbbase, value=thisLabel, xoffset=5)
   lgeo    = widget_info(l, /geometry) 
   plotBase = Widget_Base(bbbase, Column=1, /Frame, YOffset=lgeo.ysize/2, YPAD=10, XPAD=10)
   l = Widget_Label(bbbase, value=thisLabel, xoffset=5)
   
self.plotID = FSC_PlotWindow(plotBase, Units=unit, PageSize=self.pagetypeSet, $
   Landscape=self.landscapeSet, WindowSize=windowsize, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'PLOTWINDOW', Object:self})

   ; Create bottom button widgets.

    actionBase = Widget_Base(rightTopBase, Row=1)
    cancel = Widget_Button(actionBase, Value=cancelButVal, UValue={Method:'CANCEL', Object:self})
    self.revertID = Widget_Button(actionBase, Value='Revert', UValue={Method:'REVERT', Object:self}, Sensitive=0)
    helper = Widget_Button(actionBase, Value='Help', UValue={Method:'HELP', Object:self})
    self.acceptID = Widget_Button(actionBase, Value=acceptButtonName, UValue={Method:'ACCEPT', Object:self})

ENDIF ELSE BEGIN

    plotbase = Widget_Base(rightTopBase, /Frame, XPAD=15, YPAD=15, COLUMN=1)
    self.plotID = FSC_PlotWindow(plotbase, Units=unit, PageSize=self.pagetypeSet, $
       Landscape=self.landscapeSet, WindowSize=windowsize, Event_Pro='FSC_PSCONFIG_Events', $
       UValue={Method:'PLOTWINDOW', Object:self})

   ; Create bottom button widgets.

    actionBase = Widget_Base(middleTopBase, Row=1)
    cancel = Widget_Button(actionBase, Value=cancelButVal, UValue={Method:'CANCEL', Object:self})
    self.revertID = Widget_Button(actionBase, Value='Revert', UValue={Method:'REVERT', Object:self}, Sensitive=0)
    helper = Widget_Button(actionBase, Value='Help', UValue={Method:'HELP', Object:self})
    self.acceptID = Widget_Button(actionBase, Value=acceptButtonName, UValue={Method:'ACCEPT', Object:self})
ENDELSE
 
   ; Size the widgets.

t = Widget_Info(leftTopBase, /Geometry)
targetSize = t.scr_xsize
self.pageTypeID->Resize, ParentSize=targetsize
self.orientationID->Resize, targetsize
self.unitsID->Resize, targetsize
self.encapsulationID->Resize, targetsize
self.langLevelID->Resize, targetsize
self.previewID->Resize, targetsize
self.cmykID->Resize, targetsize
self.colorID->Resize, targetsize
self.bitsID->Resize, targetsize
IF Obj_Valid(self.decomposedID) THEN self.decomposedID->Resize, targetsize
self.defaultsID->Resize, targetsize

IF self.encapsulationID->GetIndex() THEN self.previewID->Sensitive, 1 ELSE self.previewID->Sensitive, 0
self.plotID->SetColor, self.colorSet

   ; Center and realize TLB.

cgCenterTLB, tlb
Widget_Control, tlb, /Realize

self.noblock = Keyword_Set(noblock)

   ; Save the current configuration.

self->SaveConfiguration

XManager, 'fsc_psconfig_' + self.name, tlb, Event_Handler='FSC_PSCONFIG_EVENTS', $
  No_Block=Keyword_Set(noblock)
IF Keyword_Set(noblock) THEN cancel = 0 ELSE cancel = self.cancel
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::GUIFont, Cancel=cancel, Group_Leader=leader, NoBlock=noblock, $
   NoFont=nofont, Accept_Button_Name=acceptButtonName, FontType=fonttype

; This method displays the graphical user interface with font information
; in place.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Can't run this program in a device that doesn't support windows.

self.thisDevice = !D.Name
IF (!D.Flags AND 256) EQ 0 THEN BEGIN
   thisOS = StrUpCase(!Version.OS_Family)
   CASE thisOS OF
      'MACOS': Set_Plot, 'Mac'
      'WINDOWS': Set_Plot, 'Win'
      ELSE: Set_Plot, 'X'
   ENDCASE
ENDIF

   ; If the NoFont keyword is set, call the non-font GUI method.

IF Keyword_Set(nofont) THEN BEGIN
   self->Gui, Cancel=cancel, Group_Leader=leader, NoBlock=Keyword_Set(noblock), $
      Accept_Button_Name=acceptButtonName, FontType=fonttype
   RETURN
ENDIF

IF N_Elements(acceptButtonName) EQ 0 THEN BEGIN
   IF Keyword_Set(noblock) THEN BEGIN
      acceptButtonName = 'Apply'
   ENDIF ELSE BEGIN
      acceptButtonName = 'Accept'
   ENDELSE
ENDIF

   ; Only one GUI at a time with the same name.

IF XRegistered('fsc_psconfig_' + self.name) GT 0 THEN RETURN

   ;Set default values.

self.fontInfo = 1
CW_FIELD_SIZE = 5
IF Keyword_Set(noblock) THEN BEGIN
   cancelButVal = 'Dismiss'
ENDIF ELSE BEGIN
   cancelButVal = 'Cancel'
ENDELSE

IF N_Elements(fonttype) EQ 0 THEN fonttype = self.fontTypeSet
self.fontTypeSet = fonttype

IF self.fontTypeSet EQ 1 THEN self.trueTypeSet = 1
IF Keyword_Set(noblock) THEN self.cancel = 0 ELSE self.cancel = 1

IF self.name NE "" THEN tlbTitle = self.name + ": Configure PostScript Device" ELSE $
   tlbTitle = "Configure PostScript Device"

   ; Modal TLB if possible.

IF N_Elements(leader) EQ 0 OR Keyword_Set(noblock) THEN BEGIN
   tlb = Widget_Base(Title=tlbTitle, Column=1, Base_Align_Center=1)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=tlbTitle, Column=1, $
      Base_Align_Center=1, Group_Leader=leader, /Modal)
ENDELSE

self.tlb = tlb

   ; Sub-base hierarchy.

   topBase = Widget_Base(tlb, Row=1, UValue=self.thisDevice, Kill_Notify='FSC_PSConfig_Restore_Device')
      leftTopBase = Widget_Base(topBase, Column=1)
         controlBase = Widget_Base(leftTopBase, Column=1, /Frame)
         euroBase = Widget_Base(leftTopBase, row=1, /NonExclusive)
      rightTopBase = Widget_Base(topBase, Column=1)
         sizebase = Widget_Base(rightTopBase, Row=1, /Frame)
         fontbase = Widget_Base(rightTopBase, Column=1, Base_Align_Left=1, Frame=1)
            firstRow = Widget_Base(fontbase, Row=1)
            secondRow = Widget_Base(fontbase, Row=1)
            thirdRow = Widget_Base(fontbase, Row=1)
            fourthRow = Widget_Base(fontbase, Row=1)
      middleTopBase = Widget_Base(topBase, Column=1, Base_Align_Center=1)
   actionBase = Widget_Base(tlb, Row=1, Base_Align_Center=1)

   ; Create droplist widgets.

values = ['Letter', 'A4', 'Legal', 'Ledger']
self.pageTypeID = FSC_Droplist(controlBase, Value=values, Title='Page Size:', $
   UValue={Method:'PageSize', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.pageTypeID->SetSelection, self.pageTypeSet
self.pagetype = self.pageTypeSet

values = ['Portrait', 'Landscape']
self.orientationID = FSC_Droplist(controlBase, Value=values, Title='Orientation:', $
   UValue={Method:'Orientation', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.orientationID->SetIndex, self.landscapeSet

values = ['Centimeters', 'Inches']
self.unitsID = FSC_Droplist(controlBase, Value=values, Title='Units:', $
   UValue={Method:'Units', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.unitsID->SetIndex, self.inchesSet

values = ['Off', 'On']
self.encapsulationID = FSC_Droplist(controlBase, Value=values, Title='Encapsulation:', $
   UValue={Method:'ENCAPSULATE', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.encapsulationID->SetIndex, self.encapsulationSet

values = ['None','Bitmap Preview','TIFF Preview']
self.previewID = FSC_Droplist(controlBase, Value=values, Title='Preview Mode:', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.previewID->SetIndex, self.previewSet

values = ['Off', 'On']
self.colorID = FSC_Droplist(controlBase, Value=values, Title='Color Output:', $
   UValue={Method:'COLOR', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.colorID->SetIndex, self.colorSet

values = ['Off', 'On']
self.cmykID = FSC_Droplist(controlBase, Value=values, Title = 'CMYK Output', $
    UValue={Method:'CMYK', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.cmykID->SetIndex, self.cmykSet

values = ['1','2']
self.langlevelID = FSC_Droplist(controlBase, Value=values, Title = 'Language Level:', $
    UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.langlevelID->SetSelection, self.langlevelSet

values = ['8', '4', '2']
self.bitsID = FSC_Droplist(controlBase, Value=values, Title='Bits Per Image Pixel:', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.bitsID->SetSelection, self.bitsSet

IF Float(!Version.Release) GE 7.1 THEN BEGIN
    values = ['Off', 'On']
    self.decomposedID = FSC_Droplist(controlBase, Value=values, Title = 'Color Decomposition:', $
        UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
    self.decomposedID->SetIndex, self.decomposedSet
    self.bitsID->SetSelection, 8
    self.colorID->SetIndex, 1
    self.colorSet = 1
ENDIF

values = self->DefaultList()
self.defaultsID = FSC_Droplist(controlBase, Value=values, Title='Setups:', $
   UValue={Method:'DEFAULTS', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
self.defaultsID->SetSelection, self.defaultsSet

euroSetUpID = Widget_Button(euroBase, Value='Metric Style', UValue={Method:'METRICSTYLE', Object:self})
IF self.metric THEN Widget_Control, euroSetUpID, /Set_Button
IF self.inchesSet THEN unit = 'INCHES' ELSE unit = 'CENTIMETERS'

windowDims = self->PageDimensions()
shortside = Min(windowDims, Max=longside)
IF self.landscapeSet THEN BEGIN
   windowsize = [self.xoffsetSet/longside, self.yoffsetSet/shortside, $
      self.xsizeSet/longside + self.xoffsetSet/longside, self.ysizeSet/shortside + self.yoffsetSet/shortside]
ENDIF ELSE BEGIN
   windowsize = [self.xoffsetSet/shortside, self.yoffsetSet/longside, $
      self.xsizeSet/shortside + self.xoffsetSet/shortside, self.ysizeSet/longside + self.yoffsetSet/longside]
ENDELSE

   ; Create the plot window.

self.plotID = FSC_PlotWindow(middleTopBase, Units=unit, PageSize=self.pagetypeSet, $
   Landscape=self.landscapeSet, WindowSize=windowsize, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'PLOTWINDOW', Object:self})

   ; Create size and offset windows.

xsizeID = FSC_Field(sizebase, Title=' X Size: ', Value=self.xsizeSet, Object=xsizeObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.xsizeID = xsizeObj
ysizeID = FSC_Field(sizebase, Title=' Y Size: ', Value=self.ysizeSet, Object=ysizeObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.ysizeID = ysizeObj

xoffsetID = FSC_Field(sizebase, Title=' X Offset: ', Value=self.xoffsetSet, Object=xoffsetObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.xoffsetID = xoffsetObj

yoffsetID = FSC_Field(sizebase, Title=' Y Offset: ', Value=self.yoffsetSet, Object=yoffsetObj, $
   /Float, XSize=CW_FIELD_SIZE, Decimal=2, /CR_Only, Event_Pro='FSC_PSCONFIG_Events', $
   UValue={Method:'WINDOWSIZE', Object:self}, /Positive, /Label_Left)
self.yoffsetID = yoffsetObj

   ; Set up TABing between fields.

self.xsizeID->SetTabNext, self.ysizeID->GetTextID()
self.ysizeID->SetTabNext, self.xoffsetID->GetTextID()
self.xoffsetID->SetTabNext, self.yoffsetID->GetTextID()
self.yoffsetID->SetTabNext, self.xsizeID->GetTextID()

   ; Create the font widgets.

values = ['Vector (Hershey)', 'Hardware', 'True-Type']
self.fontTypeID = FSC_Droplist(firstRow, Value=values, Title='Font Type: ', $
   UValue={Method:'FONTTYPE', Object:self}, Event_Pro='FSC_PSCONFIG_Events') ;, UName='Font Type')
self.fontTypeID->SetIndex, (0 > (self.fonttypeSet + 1) < 2)

self.fontnameID = FSC_Droplist(firstrow, Value=*self.fontnames, Title='Font: ', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events') ;, UName='Font Name')
self.fontnameID->SetSelection, self.fontNameSet

values = ['Bold','Book', 'Demi', 'Italic', 'Light', 'Medium', 'Narrow', 'Oblique']
self.styleBaseID = Widget_Base(secondRow, Column=4, /Nonexclusive, /Frame)
FOR j=0,7 DO BEGIN
      self.fontStyleButtonID[j] = Widget_Button(self.styleBaseID, Value=values[j], $
         UValue={Method:'STYLEBUTTONS', Object:self}, Event_Pro='FSC_PSCONFIG_Events')
      Widget_Control, self.fontStylebuttonID[j], Set_Button=self.fontStyleSet[j]
ENDFOR
FOR j=0,7 DO IF self.fontStyleSet[j] THEN Widget_Control, self.fontStyleButtonID[j], Set_Button=1

clearBaseID = Widget_Base(secondRow)
self.clearButtonID = Widget_Button(clearBaseID, Value='Clear Font Styles', YOffset=15, XOffset=6, $
         UValue={Method:'CLEARSTYLES', Object:self}, Event_Pro='FSC_PSCONFIG_Events')

   ; More droplist widgets.

values = Indgen(31) + 6
self.fontSizeID = FSC_Droplist(thirdrow, Value=StrTrim(values,2), Title='Font Size: ', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events') ;, UName='Font Size')
self.fontSizeID->SetSelection, StrTrim(self.fontsizeSet,2)

self.truetypeID = FSC_Droplist(thirdrow, Value=['No', 'Yes'], Title='True-Type: ', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events') ;, UName='True-Type Fonts')
self.truetypeID->SetIndex, self.truetypeSet

self.isolatinID = FSC_Droplist(thirdrow, Value=['Off', 'On'], Title='ISOlatin1: ', $
   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events') ;, UName='Isolatin Encoding')
self.isolatinID->SetIndex, self.isolatinSet

;self.set_fontID = FSC_InputField(fourthRow, Value=self.set_fontSet, $
;   UValue={Method:'NULL', Object:self}, Event_Pro='FSC_PSCONFIG_Events', $
;   Title='Set_Font Name: ', XSize=35, /StringValue)

   ; The directions widgets.

labelBase = Widget_Base(middleTopBase, Column=1, Base_Align_Center=1)
label = Widget_Label(labelBase, Value='Directions')
label = Widget_Label(labelBase, Value='Move or resize the PostScript "plot" on the', /Align_Left)
label = Widget_Label(labelBase, Value='page with left or right buttons. Click the middle', /Align_Left)
label = Widget_Label(labelBase, Value='button in the window to center plot on the page.', /Align_Left)

   ; The file name widgets.

dummy = FSC_FileSelect(rightTopBase, Filename=self.filenameSet, $
   Directory=self.directorySet, XSIZE=40, ObjectRef=filenameID)
self.filenameID = filenameID

   ; The bottom button widgets.

cancel = Widget_Button(actionBase, Value=cancelButVal, UValue={Method:'CANCEL', Object:self})
self.revertID = Widget_Button(actionBase, Value='Revert', UValue={Method:'REVERT', Object:self}, Sensitive=0)
helper = Widget_Button(actionBase, Value='Help', UValue={Method:'HELP', Object:self})
self.acceptID = Widget_Button(actionBase, Value=acceptButtonName, UValue={Method:'ACCEPT', Object:self})

   ; Size the widgets for layout.

t = Widget_Info(leftTopBase, /Geometry)
targetSize = t.scr_xsize
self.pageTypeID->Resize, ParentSize=targetsize
self.orientationID->Resize, targetsize
self.unitsID->Resize, targetsize
self.encapsulationID->Resize, targetsize
self.previewID->Resize, targetsize
self.cmykID->Resize, targetsize
IF Obj_Valid(self.decomposedID) THEN self.decomposedID->Resize, targetsize
self.langlevelID->Resize, targetsize
self.colorID->Resize, targetsize
self.bitsID->Resize, targetsize
self.defaultsID->Resize, targetsize

t = Widget_Info(fontbase, /Geometry)
targetsize = t.scr_xsize / 2
self.fontTypeID->Resize, targetsize
self.fontnameID[0]->Resize, targetsize

targetsize = t.scr_xsize / 3
self.truetypeID->Resize, targetsize
self.isoLatinID->Resize, targetsize
self.fontSizeID->Resize, targetsize

targetsize = t.scr_xsize
IF Obj_Valid(self.set_fontID) THEN self.set_fontID->Resize, targetsize

   ; Some widgets need to be turned off.

IF self.encapsulationID->GetIndex() THEN self.previewID->Sensitive, 1 ELSE self.previewID->Sensitive, 0
self.plotID->SetColor, self.colorSet
IF self.fontTypeSet EQ -1 THEN BEGIN
   Widget_Control, self.fontnameID->GetID(), Sensitive=0
   Widget_Control, self.stylebaseID, Sensitive=0
   Widget_Control, self.clearbuttonID, Sensitive=0
   Widget_Control, self.fontSizeID->GetID(), Sensitive=0
   Widget_Control, self.truetypeID->GetID(), Sensitive=0
   Widget_Control, self.isolatinID->GetID(), Sensitive=0
   IF Obj_Valid(self.set_fontID) THEN Widget_Control, self.set_fontID->GetID(), Sensitive=0
ENDIF

   ; Center and realize the top-level base.

cgCenterTLB, tlb
Widget_Control, tlb, /Realize

self.noblock = Keyword_Set(noblock)

   ; Save the current configuration.

self->SaveConfiguration

XManager, 'fsc_psconfig_' + self.name, tlb, Event_Handler='FSC_PSCONFIG_EVENTS', $
  No_Block=Keyword_Set(noblock)
IF Keyword_Set(noblock) THEN cancel = 0 ELSE cancel = self.cancel
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Help, event

; This is the text that pops up in the HELP WINDOW when the
; Help button is selected. Feel free to change it to whatever
; you like. The "textsize" variable below the help text should
; be set to the number of lines of textual information.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

helptext = [ $
"                                                              ", $
"  The FSC_PSConfig object is a Open Source program written    ", $
"  by David Fanning of Fanning Software Consulting. Complete   ", $
"  program documenation is available on the FSC web page:      ", $
"                                                              ", $
"     http://www.idlcoyote.com/programs/docs/fsc_psconfig.html  ", $
"                                                              ", $
"  Other IDL programs, as well as many IDL programming tips    ", $
"  and techniques can be found on the Coyote's Guide to IDL    ", $
"  Programming web page. Please take a moment to look around.  ", $
"                                                              ", $
"     Fanning Software Consulting                              ", $
"     1645 Sheely Drive                                        ", $
"     Fort Collins, CO 80526                                   ", $
"     Phone: 970-221-0438   Fax: 970-221-4762                  ", $
"     E-Mail: david@idlcoyote.com                               ", $
"     IDL Book Orders: 1-888-461-0155                          ", $
"     Coyote's Guide: http://www.idlcoyote.com/                 ", $
"                                                              " ]
textsize = 19

   ; Only one help display at a time, please.

IF XRegistered('fsc_psconfig_help') GT 0 THEN RETURN

   ; Creat the widgets for the HELP WINDOW.

base = Widget_Base(Column=1, Group_Leader=self.acceptID)
textID = Widget_Text(base, Value=helptext, Scr_XSize=500, YSize=textsize)
IF NOT Widget_Info(self.tlb, /Modal) THEN button = Widget_Button(base, Value='Dismiss')
cgCenterTLB, base
Widget_Control, base, /Realize
XManager, 'fsc_psconfig_help', base, /Just_Reg
END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::PageDimensions

; This method sets up the page size. If you add a new page size,
; you must add its dimensions (in inches) to the CASE statement below.

; Letter 8.5 x 11.
; Ledger 11 x 17.
; Legal 8.5 x 14.
; A4 8.27 x 11.7.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN, [0,0]
ENDIF

CASE StrUpCase(self.pagetype) OF
   'LETTER': BEGIN
         shortside = 8.5
         longside = 11.0
      ENDCASE
   'LEDGER': BEGIN
         shortside = 11.0
         longside = 17.0
      ENDCASE
   'LEGAL': BEGIN
         shortside = 8.5
         longside = 14.0
      ENDCASE
   'A4': BEGIN
         shortside = 8.27
         longside = 11.70
      ENDCASE
;   ELSE: BEGIN
;         shortside = 8.5
;         longside = 11.0
;      ENDCASE
ENDCASE

IF self.inchesSet EQ 0 THEN BEGIN
   shortside = shortside * 2.54
   longside = longside * 2.54
ENDIF

RETURN, [shortside, longside]
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Orientation, event

; This event handler method responds to changes in the Orientation droplist.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

pagetype = self.pageTypeID->GetSelection()
Widget_Control, event.top, Update=0
self.plotID->SetPageSize, pagetype, Landscape=event.index, TLB=self.tlb
self.plotID->GetWindowLocation, xsize, ysize, xoffset, yoffset
self->UpdateSizes, xsize, ysize, xoffset, yoffset
self.landscapeSet = event.index
Widget_Control, event.top, Update=1
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::PageSize, event

; This event handler method responds to changes in the Page Size droplist.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Landscape is the default orientation for LEDGER output.

IF StrUpCase(*event.selection) EQ "LEDGER" THEN BEGIN
   landscapeValue = 1
   self.orientationID->SetIndex, 1
ENDIF ELSE landscapeValue = self.orientationID->GetIndex()

   ; Change the page size. Get the window location and update the display.

Widget_Control, event.top, Update=0
self.plotID->SetPageSize, *event.selection, Landscape=landscapeValue, TLB=self.tlb
self.plotID->GetWindowLocation, xsize, ysize, xoffset, yoffset
self->UpdateSizes, xsize, ysize, xoffset, yoffset
self.pagetype = *event.selection
Widget_Control, event.top, Update=1
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::PlotWindow, event

; The plot window moved or changed size. Update the size and offset widgets.

self->UpdateSizes, event.xsize, event.ysize, event.xoffset, event.yoffset
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Revert, event

; This method allows the user to revert to the previously
; saved configuration. All buttons, droplists, etc. are set
; to their previously saved values.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Reset the widget setting variables.

self.bitsSet = self.bitsRevert
self.defaultsSet = self.defaultsRevert
self.directorySet = self.directoryRevert
self.encapsulationSet = self.encapsulationRevert
self.filenameSet = self.filenameRevert
self.fontsizeSet = self.fontsizeRevert
self.fontstyleSet = self.fontstyleRevert
self.fonttypeSet = self.fonttypeRevert
self.fontnameSet = self.fontnameRevert
self.inchesSet = self.inchesRevert
self.isolatinSet = self.isolatinRevert
self.landscapeSet = self.landscapeRevert
self.langlevelSet = self.langlevelRevert
self.pagetypeSet = self.pagetypeRevert
self.previewSet = self.previewRevert
self.set_fontSet = self.set_fontRevert
self.truetypeSet = self.truetypeRevert
self.decomposedSet = self.decomposedRevert
self.cmykSet = self.cmykRevert
self.colorSet = self.colorRevert
self.xoffsetSet = self.xoffsetRevert
self.xsizeSet = self.xsizeRevert
self.yoffsetSet = self.yoffsetRevert
self.ysizeSet = self.ysizeRevert

IF Obj_Valid(self.plotID) THEN BEGIN
   IF self.inchesSet THEN plotUnits = 'INCHES' ELSE plotUnits = 'CENTIMETERS'
   self.plotID->SetUnits, plotUnits
ENDIF

   ; Turn off updating while you update the plot window.

Widget_Control, event.top, Update=0
self.plotID->SetPageSize, self.pagetypeSet, Landscape=self.landscapeSet, TLB=self.tlb
self.plotID->SetWindowLocation, self.xsizeSet, self.ysizeSet, self.xoffsetSet, self.yoffsetSet
self.plotID->SetColor, self.colorSet

   ; Make sure the Preview droplist is set correctly.

IF self.encapsulationSet THEN self.previewID->Sensitive, 1 ELSE self.previewID->Sensitive, 0

   ; The statuslight should be set to SAFE.

self->StatusLight, 1

   ; Update the display.

self->UpdateDisplay
Widget_Control, event.top, Update=1
self.plotID->Refresh

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::SaveConfiguration

; This method saves the current configuration. Reversion
; variables are set so the REVERT button can be used to
; return to the previously saved configuration.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

self.bitsRevert = self.bitsSet
self.cmykRevert = self.cmykSet
self.colorRevert = self.colorSet
self.defaultsRevert = self.defaultsSet
self.directoryRevert = self.directorySet
self.encapsulationRevert = self.encapsulationSet
self.filenameRevert = self.filenameSet
self.fontnameRevert = self.fontnameSet
self.fontsizeRevert = self.fontsizeSet
self.fontstyleRevert = self.fontstyleSet
self.fonttypeRevert = self.fonttypeSet
self.inchesRevert = self.inchesSet
self.isolatinRevert = self.isolatinSet
self.landscapeRevert = self.landscapeSet
self.langlevelRevert = self.langlevelSet
self.pagetypeRevert = self.pagetypeSet
self.previewRevert = self.previewSet
self.truetypeRevert = self.truetypeSet
self.decomposedRevert = self.decomposedSet
self.set_fontRevert = self.set_fontSet
self.xoffsetRevert = self.xoffSetSet
self.yoffsetRevert = self.yoffSetSet
self.xsizeRevert = self.xsizeSet
self.ysizeRevert = self.ysizeSet

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::SetDefault, defaultname

; This method implements the default setups. To add your own default
; setups, add the new default name to the list in the DefaultList method
; above. Then add the default settings in the CASE statement below.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Set the page type and page units here, based on EUROPEAN setting.

IF self.metric THEN BEGIN
   pagetype = 'A4'
   units = 0
ENDIF ELSE BEGIN
   pagetype = 'LETTER'
   units = 1
ENDELSE

   ; Get the file name.

IF Obj_Valid(self.filenameID) THEN BEGIN
   self.filenameID->GetProperty, Filename=defaultFileName
   self.filenameID->GetProperty, Directory=directoryName
   index = FSC_PSConfig_RStrPos( defaultFileName, "." )
   IF index GE 0 THEN defaultFileName = StrMid(defaultFileName, 0, index)
ENDIF ELSE BEGIN
   IF self.filenameSet NE "" THEN BEGIN
      IF N_Elements(directoryName) EQ 0 THEN BEGIN
            defaultFileName = cgRootName(self.filenameSet, EXTENSION=ext, DIRECTORY=directoryName)
            IF directoryName EQ "" THEN CD, Current=directoryName 
       ENDIF ELSE BEGIN
            defaultFileName = "idl"
            CD, Current=directoryName
       ENDELSE
   ENDIF ELSE BEGIN
        defaultFileName = "idl"
        CD, Current=directoryName
   ENDELSE
ENDELSE

   ; Get the names of the defaults on the default list.

defaultlist = self->DefaultList()

   ; If the defaultname variable is undefined, choose the first one
   ; in the list.

IF N_Elements(defaultname) EQ 0 THEN defaultname = defaultlist[0]
defaultname = String(defaultname)

   ; Define synonyms.

IF StrUpCase(defaultname) EQ 'SYSTEM' THEN defaultname = 'System (Portrait)'
IF StrUpCase(defaultname) EQ 'PORTRAIT' THEN defaultname = 'Centered (Portrait)'
IF StrUpCase(defaultname) EQ 'LANDSCAPE' THEN defaultname = 'Centered (Landscape)'
IF StrUpCase(defaultname) EQ 'CENTER' THEN defaultname = 'Centered (Portrait)'
IF StrUpCase(defaultname) EQ 'CENTERED' THEN defaultname = 'Centered (Portrait)'
IF StrUpCase(defaultname) EQ 'FIGURE' THEN defaultname = 'Figure (Large)'
IF StrUpCase(defaultname) EQ 'ENCAPSULATED' THEN defaultname = 'Figure (Small)'
IF StrUpCase(defaultname) EQ 'ENCAPSULATE' THEN defaultname = 'Figure (Small)'
IF StrUpCase(defaultname) EQ 'COLOR' THEN defaultname = 'Color (Portrait)'
IF StrUpCase(defaultname) EQ 'NONE' THEN defaultname = 'None'


index = Where(StrUpCase(defaultlist) EQ StrUpCase(defaultname), count)
IF count EQ 0 THEN BEGIN
   ok = Dialog_Message(['Default set-up "' + defaultname + '" is', $
      'unknown. Using system defaults.'])
   defaultname = 'System (Portrait)'
ENDIF

thisDefault = defaultlist[index[0]]

   ; The default setups. Add your setups to this CASE statement if you are
   ; adding a new default setup.

CASE thisDefault OF

   'System (Portrait)': BEGIN
         self.bitsSet = '8'                          ; Sets the Bits-per-Pixels keyword.
         self.colorSet = 0                           ; Sets the Color keyword.
         self.directorySet = directoryName           ; Sets the directory name of the PostScript file.
         self.encapsulationSet = 0                   ; Sets the Encapsulation keyword.
         self.filenameSet = defaultFilename + ".ps"  ; Sets the filename of the PostScript file.
         self.fonttypeSet = 2                        ; Sets the Font keyword.
         self.fontsizeSet = 12                       ; Sets the Font_Size keyword.
         self.fontStyleSet = Replicate(0, 8)         ; Sets the font style keywords in this order: Bold, Book, Demi, Italic, Light, Medium, Narrow, and Oblique.
         self.fontnameSet = "Helvetica"              ; Sets the PostScript font name. All PostScript font names acceptable.
         self.inchesSet = units                      ; Sets the Inches keyword.
         self.landscapeSet = 0                       ; Sets the Landscape keyword.
         self.langlevelSet = 1                       ; Set the Language Level keyword.
         self.isolatinSet = 1                        ; Sets the Isolatin1 keyword.
         self.pagetypeSet = pagetype                 ; Sets the page type. Possible values include "Letter", "A4", etc.
         self.pagetype = pagetype                    ; Sets the page type. Possible values include "Letter", "A4", etc.
         self.previewSet = 0                         ; Sets the Preview keyword.
         self.truetypeSet = 0                        ; Sets the TT_Font keyword.
         IF self.metric THEN self.xoffsetSet = 1.61 ELSE self.xoffsetSet = 0.75 ; Sets the XOffset keyword.
         IF self.metric THEN self.yoffsetSet = 14.65 ELSE self.yoffsetSet = 5.0 ; Sets the YOffset keyword.
         IF self.metric THEN self.xsizeSet = 17.80 ELSE self.xsizeSet = 7.0     ; Sets the XSize keyword.
         IF self.metric THEN self.ysizeSet = 12.70 ELSE self.ysizeSet = 5.0     ; Sets the YSize keyword.
         self.defaultsSet = 'System (Portrait)'
      ENDCASE

   'System (Landscape)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 0
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 1
         self.langlevelSet = 1                      
         self.isolatinSet = 1 
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 1.76 ELSE self.xoffsetSet = 0.75
         IF self.metric THEN self.yoffsetSet = 1.69 ELSE self.yoffsetSet = 0.75
         IF self.metric THEN self.xsizeSet = 26.25 ELSE self.xsizeSet = 9.5
         IF self.metric THEN self.ysizeSet = 17.63 ELSE self.ysizeSet = 7.0
         self.defaultsSet = 'System (Landscape)'
     ENDCASE

   'Centered (Portrait)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 0
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 3.51 ELSE self.xoffsetSet = 1.5
         IF self.metric THEN self.yoffsetSet = 3.68 ELSE self.yoffsetSet = 1.5
         IF self.metric THEN self.xsizeSet = 14.00 ELSE self.xsizeSet = 5.5
         IF self.metric THEN self.ysizeSet = 22.40 ELSE self.ysizeSet = 8.0
         self.defaultsSet = 'Centered (Portrait)'
      ENDCASE

   'Centered (Landscape)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 1
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 3.66 ELSE self.xoffsetSet = 1.5
         IF self.metric THEN self.yoffsetSet = 3.52 ELSE self.yoffsetSet = 1.5
         IF self.metric THEN self.xsizeSet = 22.43 ELSE self.xsizeSet = 8.0
         IF self.metric THEN self.ysizeSet = 13.97 ELSE self.ysizeSet = 5.5
         self.defaultsSet = 'Centered (Landscape)'
      ENDCASE

   'Square (Portrait)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 0
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 2.26 ELSE self.xoffsetSet = 1.0
         IF self.metric THEN self.yoffsetSet = 6.63 ELSE self.yoffsetSet = 2.25
         IF self.metric THEN self.xsizeSet = 16.50 ELSE self.xsizeSet = 6.5
         IF self.metric THEN self.ysizeSet = 16.50 ELSE self.ysizeSet = 6.5
         self.defaultsSet = 'Square (Portrait)'
      ENDCASE

   'Square (Landscape)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 1
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 6.63 ELSE self.xoffsetSet = 2.25
         IF self.metric THEN self.yoffsetSet = 2.26 ELSE self.yoffsetSet = 1.0
         IF self.metric THEN self.xsizeSet = 16.50 ELSE self.xsizeSet = 6.5
         IF self.metric THEN self.ysizeSet = 16.50 ELSE self.ysizeSet = 6.5
         self.defaultsSet = 'Square (Landscape)'
      ENDCASE

   'Figure (Small)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 1
         self.filenameSet = defaultFilename + ".eps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 0
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 2
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 6.06 ELSE self.xoffsetSet = 2.5
         IF self.metric THEN self.yoffsetSet = 11.71 ELSE self.yoffsetSet = 4.25
         IF self.metric THEN self.xsizeSet = 8.89 ELSE self.xsizeSet = 3.5
         IF self.metric THEN self.ysizeSet = 6.35 ELSE self.ysizeSet = 2.5
         self.defaultsSet = 'Figure (Small)'
      ENDCASE

   'Figure (Large)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 1
         self.filenameSet = defaultFilename + ".eps"
         self.fonttypeSet = 0
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 0
         self.isolatinSet = 1
         self.langlevelSet = 1                      
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 2
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 4.16 ELSE self.xoffsetSet = 1.75
         IF self.metric THEN self.yoffsetSet = 9.8 ELSE self.yoffsetSet = 3.5
         IF self.metric THEN self.xsizeSet = 12.70 ELSE self.xsizeSet = 5.0
         IF self.metric THEN self.ysizeSet = 10.16 ELSE self.ysizeSet = 4.0
         self.defaultsSet = 'Figure (Large)'
      ENDCASE

   'Color (Portrait)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 0
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 3.51 ELSE self.xoffsetSet = 1.5
         IF self.metric THEN self.yoffsetSet = 3.68 ELSE self.yoffsetSet = 1.5
         IF self.metric THEN self.xsizeSet = 14.00 ELSE self.xsizeSet = 5.5
         IF self.metric THEN self.ysizeSet = 22.40 ELSE self.ysizeSet = 8.0
         self.defaultsSet = 'Color (Portrait)'
      ENDCASE

   'Color (Landscape)': BEGIN
         self.bitsSet = '8'
         self.colorSet = 1
         self.directorySet = directoryName
         self.encapsulationSet = 0
         self.filenameSet = defaultFilename + ".ps"
         self.fonttypeSet = 2
         self.fontsizeSet = 12
         self.fontstyleSet = Replicate(0, 8)
         self.fontnameSet = "Helvetica"
         self.inchesSet = units
         self.landscapeSet = 1
         self.langlevelSet = 1                      
         self.isolatinSet = 1
         self.pagetypeSet = pagetype
         self.pagetype = pagetype
         self.previewSet = 0
         self.truetypeSet = 0
         IF self.metric THEN self.xoffsetSet = 3.66 ELSE self.xoffsetSet = 1.5
         IF self.metric THEN self.yoffsetSet = 3.52 ELSE self.yoffsetSet = 1.5
         IF self.metric THEN self.xsizeSet = 22.43 ELSE self.xsizeSet = 8.0
         IF self.metric THEN self.ysizeSet = 13.97 ELSE self.ysizeSet = 5.5
         self.defaultsSet = 'Color (Landscape)'
      ENDCASE

   ELSE: self.defaultsSet = 'None'
ENDCASE


IF Obj_Valid(self.plotID) THEN BEGIN
   IF self.inchesSet THEN plotUnits = 'INCHES' ELSE plotUnits = 'CENTIMETERS'
   self.plotID->SetUnits, plotUnits
ENDIF

   ; Set the full filename.
self.fullFileNameSet = Filepath(ROOT_DIR=self.directorySet, self.filenameSet)

   ; If the font information is on the display, update it.

IF self.fontInfo THEN BEGIN

   IF self.fontTypeSet EQ -1 THEN BEGIN
      Widget_Control, self.fontnameID->GetID(), Sensitive=0
      Widget_Control, self.stylebaseID, Sensitive=0
      Widget_Control, self.clearbuttonID, Sensitive=0
      Widget_Control, self.fontSizeID->GetID(), Sensitive=0
      Widget_Control, self.truetypeID->GetID(), Sensitive=0
      Widget_Control, self.isolatinID->GetID(), Sensitive=0
      IF Obj_Valid(self.set_fontID) THEN Widget_Control, self.set_fontID->GetID(), Sensitive=0
   ENDIF ELSE BEGIN
      Widget_Control, self.fontnameID->GetID(), Sensitive=1
      Widget_Control, self.stylebaseID, Sensitive=1
      Widget_Control, self.clearbuttonID, Sensitive=1
      Widget_Control, self.fontSizeID->GetID(), Sensitive=1
      Widget_Control, self.truetypeID->GetID(), Sensitive=1
      Widget_Control, self.isolatinID->GetID(), Sensitive=1
      IF Obj_Valid(self.set_fontID) THEN Widget_Control, self.set_fontID->GetID(), Sensitive=1

ENDELSE

ENDIF

   ; Update the display.

self->UpDateDisplay

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::StatusLight, safe

; This method sets the status light (the background color of the
; page window). Safe equal 1 means the current configuration is saved.
; Safe equal 0 means the current configuration is not saved. The
; REVERT button is turned on or off to reflect the current SAFE state.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

safe = Keyword_Set(safe)

   ; Different colors based on depth of visual class.

Device, Get_Visual_Depth=theDepth
IF safe THEN BEGIN
   IF theDepth GT 8 THEN windowColor = [60, 140, 140] ELSE windowColor = [80, 80, 80]
   Widget_Control, self.revertID, Sensitive=0
ENDIF ELSE BEGIN
   IF theDepth GT 8 THEN windowColor = [200, 133, 133] ELSE windowColor = [140, 140, 140]
      Widget_Control, self.revertID, Sensitive=1
ENDELSE

   ; Update the background window color. Done only in NOBLOCK conditions.

IF self.noblock THEN self.plotID->SetWindowColor, windowColor
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::StyleButtons, event

; This event handler method responds to changes in the font style buttons.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

index = Where(self.fontStyleButtonID EQ event.id)
index = index[0]
self.fontStyleSet[index] = event.select
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::Units, event

; This event handler method responds to changes in the Units droplist.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

units = self.unitsID->GetSelection()
self.plotID->SetUnits, units
self.plotID->GetWindowLocation, xsize, ysize, xoffset, yoffset
self->UpdateSizes, xsize, ysize, xoffset, yoffset
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::UpdateDisplay

; This method updates the GUI display with the current object settings.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Can only update if widget is valid and realized.

IF Widget_Info(self.tlb, /Valid_ID) EQ 0 THEN RETURN
IF Widget_Info(self.tlb, /Realized) EQ 0 THEN RETURN

   ; Set the normal GUI widgets.

IF Obj_Valid(self.decomposedID) THEN self.decomposedID->SetIndex, self.decomposedSet
self.bitsID->SetSelection, self.bitsSet
self.cmykID->SetIndex, self.cmykSet
self.colorID->SetIndex, self.colorSet
self.filenameID->SetProperty, Directory=self.directorySet, Filename=self.filenameSet
self.encapsulationID->SetIndex, self.encapsulationSet
self.unitsID->SetIndex, self.inchesSet
self.orientationID->SetIndex, self.landscapeSet
self.langlevelID->SetIndex, self.langlevelSet
self.pagetypeID->SetSelection, self.pagetypeSet
self.previewID->SetIndex, self.previewSet
self.xsizeID->Set_Value, self.xsizeSet
self.ysizeID->Set_Value, self.ysizeSet
self.xoffsetID->Set_Value, self.xoffsetSet
self.yoffsetID->Set_Value, self.yoffsetSet
self.defaultsID->SetSelection, self.defaultsSet

   ; If font information is on the display, set that too.

IF self.fontInfo THEN BEGIN
   self.fontTypeID->SetIndex, (0 > (self.fonttypeSet + 1) < 2)
   self.fontsizeID->SetSelection, StrTrim(self.fontsizeSet,2)
   self.isolatinID->SetIndex, self.isolatinSet
   self->UpDateFontStyle
   self.fontnameID->SetSelection, self.fontnameSet
   IF Obj_Valid(self.set_fontID) THEN self.set_fontID->Set_Value, self.set_fontSet
   self.truetypeID->SetIndex, self.truetypeSet
ENDIF

   ; Update the plot window and set the size and offset widgets.

self.plotID->SetPageSize, self.pagetypeSet, Landscape=self.landscapeSet, TLB=self.tlb
self.plotID->SetWindowLocation, self.xsizeSet, self.ysizeSet, self.xoffsetSet, self.yoffsetSet
self.plotID->GetWindowLocation, xsize, ysize, xoffset, yoffset
self.xsizeID->Set_Value, xsize
self.ysizeID->Set_Value, ysize
self.xoffsetID->Set_Value, xoffset
self.yoffsetID->Set_Value, yoffset
self.plotID->SetColor, self.colorSet

IF self.encapsulationSet THEN self.previewID->Sensitive, 1 ELSE self.previewID->Sensitive, 0

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::UpdateFontStyle, Clear = Clear

; This method updates the font style box. If the CLEAR keyword
; is set, all the widgets are turned OFF.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

IF Keyword_Set(clear) THEN BEGIN
   FOR j=0,7 DO BEGIN
      Widget_Control, self.fontstyleButtonID[j], Set_Button=0
      self.fontStyleSet[j] = 0
   ENDFOR
ENDIF ELSE BEGIN
   FOR j=0,7 DO Widget_Control, self.fontstyleButtonID[j], Set_Button=self.fontStyleSet[j]
ENDELSE

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::UpdateSizes, xsize, ysize, xoffset, yoffset

; This method updates the size and offset widgets.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

self.xsizeID->Set_Value, xsize
self.ysizeID->Set_Value, ysize
self.xoffsetID->Set_Value, xoffset
self.yoffsetID->Set_Value, yoffset
END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::WindowSize, event

; This is the event handler method that responds to events
; in the XSize, YSize, XOffset, and YOffset text widgets.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=self.debug)
   RETURN
ENDIF

   ; Get the values in the appropriate widgets.

IF N_Elements(self.xsizeID->Get_Value()) EQ 0 THEN BEGIN
   IF self.inchesSet THEN xsize = 1.0 ELSE xsize = 2.54
ENDIF ELSE xsize = self.xsizeID->Get_Value()
IF N_Elements(self.ysizeID->Get_Value()) EQ 0 THEN BEGIN
   IF self.inchesSet THEN ysize = 1.0 ELSE ysize = 2.54
ENDIF ELSE ysize = self.ysizeID->Get_Value()
IF N_Elements(self.xoffsetID->Get_Value()) EQ 0 THEN xoffset = 0.0 $
   ELSE xoffset = self.xoffsetID->Get_Value()
IF N_Elements(self.yoffsetID->Get_Value()) EQ 0 THEN yoffset = 0.0 $
   ELSE yoffset = self.yoffsetID->Get_Value()

   ; Change the plot window to these sizes and offsets.

self.plotID->SetWindowLocation, xsize, ysize, xoffset, yoffset

   ; The input values may be bogus. Get the "real" values from the
   ; plot object and update the size and offset widgets.

self.plotID->GetWindowLocation, xsize, ysize, xoffset, yoffset
self.xsizeID->Set_Value, xsize
self.ysizeID->Set_Value, ysize
self.xoffsetID->Set_Value, xoffset
self.yoffsetID->Set_Value, yoffset
END ;--------------------------------------------------------------------------------




PRO FSC_PSCONFIG::SetProperty,        $ ; The SetProperty method of the object.
   AvantGarde=avantgarde,             $ ; Set this keyword to select the AvantGarde font.
   Bits_per_Pixel=bits_per_pixel,     $ ; The number of image bits saved for each image pixel: 2, 4, or 8.
   Bold=bold,                         $ ; Set this keyword to select the Bold font style.
   BookStyle=book,                    $ ; Set this keyword to select the Book font style.
   Bkman=bookman,                     $ ; Set this keyword to select the Bookman font.
   CMYK=cmyk,                         $ ; Set this keywprd to use CMYK colors instead of RGB.
   Color=color,                       $ ; Set this keyword to select Color PostScript output.
   Courier=courier,                   $ ; Set this keyword to select the Courier font.
   DefaultSetup=defaultsetup,         $ ; Set this keyword to the "name" of a default style.
   Demi=demi,                         $ ; Set this keyword to select the Demi font style.
   Directory=directory,               $ ; Set thie keyword to the name of the starting directory. Current directory by default.
   Encapsulated=encapsulated,         $ ; Set this keyword to select Encapsulated PostScript output.
;   European=european,                 $ ; Set this keyword to indicate "european" mode (i.e., A4 page and centimeter units).
   Filename=filename,                 $ ; Set this keyword to the name of the file. Default: 'idl.ps'
   FontSize=fontsize,                 $ ; Set this keyword to the font size. Between 6 and 36. Default is 12.
   FontType=fonttype,                 $ ; Set this keyword to select the font type: -1 is Hershey, 0 is hardward, 1 is true-type.
   Helvetica=helvetica,               $ ; Set this keyword to select the Helvetica font.
   Inches=inches,                     $ ; Set this keyword to indicate sizes and offsets are in inches as opposed to centimeters.
   Italic=italic,                     $ ; Set this keyword to select the Italic font style.
   Isolatin=isolatin,                 $ ; Set this keyword to select ISOlatin1 encoding.
   Landscape=landscape,               $ ; Set this keyword to select Landscape output.
   LanguageLevel=langlevel,           $ ; Set this keyword to the Language Level of the PostScript interpreter.
   Light=light,                       $ ; Set this keyword to select the Light font style.
   Medium=medium,                     $ ; Set this keyword to select the Medium font style.
   Metric=metric,                     $ ; Set this keyword to indicate "metric" mode (i.e., A4 page and centimeter units).
   Name=name,                         $ ; This is the "name" of the object.
   Narrow=narrow,                     $ ; Set this keyword to select the Narrow font style.
   Oblique=oblique, $                 $ ; Set this keyword to select the Oblique font style.
   PageType=pagetype,                 $ ; Set this keyword to the "type" of page: 'Letter', 'Legal', 'Ledger', or 'A4'.
   Palatino=palatino,                 $ ; Set this keyword to select the Palatino font.
   Preview=preview,                   $ ; Set this keyword to select Preview mode: 0, 1, or 2.
   Schoolbook=schoolbook,             $ ; Set this keyword to select the Schoolbook font.
   Set_Font=set_font,                 $ ; Set this keyword to the name of a font passed to PostScript with Set_Plot keyword.
   Symbol=symbol,                     $ ; Set this keyword to select the Symbol font.
   Times=times,                       $ ; Set this keyword to select the Times font.
   TrueType=truetype,                 $ ; Set this keyword to select True-Type fonts.
   UpDate=update, $                   $ ; Set this keyword to update the GUI if it is on the display.
   XOffset=xoffset,                   $ ; Set this keyword to the XOffset. (Note: offset calculated from lower-left corner of page.)
   XSize=xsize,                       $ ; Set this keyword to the X size of the PostScript "window".
   YOffset=yoffset,                   $ ; Set this keyword to the YOffset. (Note: offset calculated from lower-left corner of page.)
   YSize=ysize,                       $ ; Set this keyword to the Y size of the PostScript "window".
   ZapfChancery=zapfchancery,         $ ; Set this keyword to select the ZapfChancery font.
   ZapfDingbats=zapfdingbats            ; Set this keyword to select the ZapfDingbats font.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=Keyword_Set(self.debug))
   RETURN
ENDIF

   ; Check for undefined variables.

IF N_Elements(bits_per_pixel) NE 0 THEN BEGIN
   IF bits_per_pixel EQ 2 OR bits_per_pixel EQ 4 OR bits_per_pixel EQ 8 THEN $
      self.bitsSet = StrTrim(bits_per_pixel,2) ELSE self.bitsSet = '8'
ENDIF
IF N_Elements(color) NE 0 THEN self.colorSet = color
IF N_Elements(directory) NE 0 THEN self.directorySet = directory
IF N_Elements(filename) NE 0 THEN self.filenameSet = filename
self.fullfilenameSet = self->Construct_Full_Filename()
IF N_Elements(fontsize) NE 0 THEN self.fontsizeSet = fontsize
IF N_Elements(fonttype) NE 0 THEN self.fonttypeSet = fonttype
IF N_Elements(encapsulated) NE 0 THEN self.encapsulationSet = encapsulated
IF Keyword_Set(inches) NE 0 THEN self.inchesSet = inches
IF N_Elements(landscape) NE 0 THEN self.landscape = landscape
IF N_Elements(langlevel) NE 0 THEN self.langlevel = langlevel
IF Keyword_Set(name) NE 0 THEN self.name = name
IF N_Elements(pagetype) NE 0 THEN BEGIN
   self.pagetypeSet = pagetype
   self.pagetype = pagetype
ENDIF
IF N_Elements(preview) NE 0 THEN self.previewSet = 0 > preview < 2

IF N_Elements(set_font) NE 0 THEN self.set_fontSet = set_font
IF N_Elements(xsize) NE 0 THEN self.xsizeSet = xsize
IF N_Elements(ysize) NE 0 THEN self.ysizeSet = ysize

   ; Offsets are weird.

dims = self->PageDimensions()
IF N_Elements(xoffset) NE 0 THEN IF self.landscape THEN self.xoffsetSet = dims[1] - yoffset ELSE self.xoffsetSet = xoffset
IF N_Elements(yoffset) NE 0 THEN IF self.landscape THEN self.yoffsetSet = xoffset ELSE self.yoffsetSet = yoffset


avantgarde = Keyword_Set(avantgarde)
bold = Keyword_Set(bold)
book = Keyword_Set(book)
bookman = Keyword_Set(bookman)
cmyk = Keyword_Set(cmyk)
courier = Keyword_Set(courier)
demi = Keyword_Set(demi)
encapsulated = Keyword_Set(encapsulated)
;european = Keyword_Set(european)
helvetica = Keyword_Set(helvetica)
isolatin = Keyword_Set(isolatin)
italic = Keyword_Set(italic)
landscape = Keyword_Set(landscape)
light = Keyword_Set(light)
medium = Keyword_Set(medium)
metric = Keyword_Set(metric)
narrow = Keyword_Set(narrow)
oblique = Keyword_Set(oblique)
palatino = Keyword_Set(palatino)
schoolbook = Keyword_Set(schoolbook)
symbol = Keyword_Set(symbol)
times = Keyword_Set(times)
truetype = Keyword_Set(truetype)
zapfchancery = Keyword_Set(zapfchancery)
zapfdingbats = Keyword_Set(zapfdingbats)

fontset = [avantgarde, bookman, courier, helvetica, palatino, schoolbook, symbol, times, zapfchancery, zapfdingbats]
index = Where(fontset EQ 1, count)
IF count EQ 0 THEN self.fontnameSet = 'Helvetica' ELSE self.fontnameSet = (*self.fontnames)[index[0]]
IF self.fontnameSet EQ 'AvantGarde' THEN self.avantgarde = 1 ELSE self.avantgarde = 0
IF self.fontnameSet EQ 'Bookman' THEN self.bookman = 1 ELSE self.bookman = 0
IF self.fontnameSet EQ 'Courier' THEN self.courier = 1 ELSE self.courier = 0
IF self.fontnameSet EQ 'Helvetica' THEN self.helvetica = 1 ELSE self.helvetica = 0
IF self.fontnameSet EQ 'Palatino' THEN self.palatino = 1 ELSE self.palatino = 0
IF self.fontnameSet EQ 'Schoolbook' THEN self.schoolbook = 1 ELSE self.schoolbook = 0
IF self.fontnameSet EQ 'Symbol' THEN self.symbol = 1 ELSE self.symbol = 0
IF self.fontnameSet EQ 'Times' THEN self.times = 1 ELSE self.times = 0
IF self.fontnameSet EQ 'ZapfChancery' THEN self.zapfchancery = 1 ELSE self.zapfchancery = 0
IF self.fontnameSet EQ 'ZapfDingbats' THEN self.zapfdingbats = 1 ELSE self.zapfdingbats = 0
self.fontstyleSet = [bold, book, demi, italic, light, medium, narrow, oblique]

   ; Populate the self object by saving the configuration.

self->SaveConfiguration

   ; Update the display if required.

IF Keyword_Set(update) THEN self->UpdateDisplay

END ;--------------------------------------------------------------------------------



PRO FSC_PSCONFIG::CLEANUP
Ptr_Free, self.fontnames
END ;--------------------------------------------------------------------------------



FUNCTION FSC_PSCONFIG::INIT,          $ ; The INIT method of the FSC_PSCONFIG object.
   AvantGarde=avantgarde,             $ ; Set this keyword to select the AvantGarde font.
   Bits_per_Pixel=bits_per_pixel,     $ ; The number of image bits saved for each image pixel: 2, 4, or 8.
   Bold=bold,                         $ ; Set this keyword to select the Bold font style.
   BookStyle=book,                    $ ; Set this keyword to select the Book font style.
   Bkman=bookman,                     $ ; Set this keyword to select the Bookman font.
   CMYK=cmyk,                         $ ; Set this keywprd to use CMYK colors instead of RGB.
   Color=color,                       $ ; Set this keyword to select Color PostScript output.
   Courier=courier,                   $ ; Set this keyword to select the Courier font.
   Debug=debug,                       $ ; Set this keyword to get traceback information when errors are encountered.
   Decomposed=decomposed, $           $ ; Set this keyword to select 24-bit color. IDL 7.1 and above only.
   DefaultSetup=defaultsetup,         $ ; Set this keyword to the "name" of a default style.
   Demi=demi,                         $ ; Set this keyword to select the Demi font style.
   Directory=directory,               $ ; Set thie keyword to the name of the starting directory. Current directory by default.
   Encapsulated=encapsulated,         $ ; Set this keyword to select Encapsulated PostScript output.
;   European=european,                 $ ; Set this keyword to indicate "european" mode (i.e., A4 page and centimeter units).
   Filename=filename,                 $ ; Set this keyword to the name of the file. Default: 'idl.ps'
   FontSize=fontsize,                 $ ; Set this keyword to the font size. Between 6 and 36. Default is 12.
   FontType=fonttype,                 $ ; Set this keyword to select the font type: -1 is Hershey, 0 is hardward, 1 is true-type.
   Helvetica=helvetica,               $ ; Set this keyword to select the Helvetica font.
   Inches=inches,                     $ ; Set this keyword to indicate sizes and offsets are in inches as opposed to centimeters.
   Italic=italic,                     $ ; Set this keyword to select the Italic font style.
   Isolatin=isolatin,                 $ ; Set this keyword to select ISOlatin1 encoding.
   Landscape=landscape,               $ ; Set this keyword to select Landscape output.
   LanguageLevel=langlevel, $         $ ; Set this keyword to select the language level (1 or 2).
   Light=light,                       $ ; Set this keyword to select the Light font style.
   Medium=medium,                     $ ; Set this keyword to select the Medium font style.
   Metric=metric,                     $ ; Set this keyword to indicate metric mode (i.e., A4 page and centimeter units).
   Name=name,                         $ ; The "name" of the object. Objects with different names can have their
                                        ; graphical user interfaces appear simultaneously on the display.
   Narrow=narrow,                     $ ; Set this keyword to select the Narrow font style.
   Oblique=oblique, $                 $ ; Set this keyword to select the Oblique font style.
   PageType=pagetype,                 $ ; Set this keyword to the "type" of page: 'Letter', 'Legal', 'Ledger', or 'A4'.
   Palatino=palatino,                 $ ; Set this keyword to select the Palatino font.
   Preview=preview,                   $ ; Set this keyword to select Preview mode: 0, 1, or 2.
   Schoolbook=schoolbook,             $ ; Set this keyword to select the Schoolbook font.
   Set_Font=set_font,                 $ ; Set this keyword to the name of a font passed to PostScript with Set_Plot keyword.
   Symbol=symbol,                     $ ; Set this keyword to select the Symbol font.
   Times=times,                       $ ; Set this keyword to select the Times font.
   TrueType=truetype,                 $ ; Set this keyword to select True-Type fonts.
   XOffset=xoffset,                   $ ; Set this keyword to the XOffset. (Note: offset calculated from lower-left corner of page.)
   XSize=xsize,                       $ ; Set this keyword to the X size of the PostScript "window".
   YOffset=yoffset,                   $ ; Set this keyword to the YOffset. (Note: offset calculated from lower-left corner of page.)
   YSize=ysize,                       $ ; Set this keyword to the Y size of the PostScript "window".
   ZapfChancery=zapfchancery,         $ ; Set this keyword to select the ZapfChancery font.
   ZapfDingbats=zapfdingbats            ; Set this keyword to select the ZapfDingbats font.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = FSC_PSConfig_Error_Message(Traceback=Keyword_Set(debug))
   RETURN, 1
ENDIF

self.debug = 1; Keyword_Set(debug)
decomposed = Keyword_Set(decomposed)
encapsulated = Keyword_Set(encapsulated) ; Must come before LANDSCAPE.
landscape = Keyword_Set(landscape)
IF encapsulated THEN landscape = 0

   ; Set the available PostScript fonts.

availableFonts = [                      $
        'AvantGarde',                   $
        'Bookman',                      $
        'Courier',                      $
        'Helvetica',                    $
        'Palatino',                     $
        'Schoolbook',                   $
        'Symbol',                       $
        'Times',                        $
        'ZapfChancery',                 $
        'ZapfDingbats']

self.fontnames = Ptr_New(availableFonts)

   ; Default font and font type.

fontname = "Helvetica"
IF N_Elements(fonttype) EQ 0 THEN fonttype = 2
IF N_Elements(filename) NE 0 THEN self.filenameSet = filename

   ; Metric style
self.metric = Keyword_Set(metric)

   ; Set default values if a default setup was not asked for.

IF N_Elements(defaultsetup) EQ 0 THEN BEGIN

   self.defaultsSet = 'None'
   IF N_Elements(bits_per_pixel) EQ 0 THEN bits_per_pixel = 8
   IF N_Elements(bits_per_pixel) NE 0 THEN BEGIN
       IF bits_per_pixel EQ 2 OR bits_per_pixel EQ 4 OR bits_per_pixel EQ 8 THEN $
          self.bitsSet = StrTrim(bits_per_pixel,2) ELSE self.bitsSet = '8'
   ENDIF
   IF N_Elements(color) EQ 0 THEN color = 1 ELSE color = 0 > color < color
   IF N_Elements(filename) EQ 0 THEN filename = "idl.ps"
   
   ; Is this a fully-qualified filename?
   dirName = File_Dirname(filename)  
    
   ; Parse the filename and the directory name     
   IF N_Elements(directory) EQ 0 THEN BEGIN
        ; If no directory is provide, go get the last directory saved if you can.
        ; Otherwise, use the current directory.
        IF dirName EQ "." THEN BEGIN
            DEFSYSV, '!cgPostScript_LastDir', EXISTS=exists
            IF exists THEN directory = !cgPostScript_LastDir ELSE CD, Current=directory
        ENDIF ELSE BEGIN
            basename = File_Basename(filename)
            filename = basename
            directory = dirName
        ENDELSE
   ENDIF ELSE BEGIN
        IF dirName NE "." THEN BEGIN
            basename = File_Basename(filename)
            filename = basename
        ENDIF 
   ENDELSE
   IF N_Elements(fontsize) EQ 0 THEN fontsize = 12
   IF Keyword_Set(inches) EQ 0 THEN IF Keyword_Set(metric) THEN inches = 0 ELSE inches = 1
   IF N_Elements(name) EQ 0 THEN name = ""
   IF N_Elements(pagetype) EQ 0 THEN IF Keyword_Set(metric) THEN pagetype = "A4" ELSE pagetype = "LETTER"
   IF N_Elements(preview) EQ 0 THEN preview = 0
   IF N_Elements(set_font) EQ 0 THEN set_font = ""
   IF N_Elements(xoffset) EQ 0 THEN BEGIN
        IF landscape THEN BEGIN
            IF inches THEN xoffset = 0.75 ELSE xoffset = 0.75 * 2.54
        ENDIF ELSE BEGIN
            IF inches THEN xoffset = 0.5 ELSE xoffset = 0.5 * 2.54
        ENDELSE
   ENDIF
   IF N_Elements(xsize) EQ 0 THEN BEGIN
        IF landscape THEN BEGIN
            IF inches THEN xsize = 9.5 ELSE xsize = 9.5 * 2.54
        ENDIF ELSE BEGIN
            IF inches THEN xsize = 7.5 ELSE xsize = 7.5 * 2.54
        ENDELSE
   ENDIF
   IF N_Elements(yoffset) EQ 0 THEN BEGIN
        IF landscape THEN BEGIN
            IF inches THEN IF inches THEN yoffset = 10.25 ELSE yoffset = 10.25 * 2.54
        ENDIF ELSE BEGIN
            IF inches THEN yoffset = 2.5 ELSE yoffset = 2.5 * 2.54
        ENDELSE
   ENDIF
   IF N_Elements(ysize) EQ 0 THEN BEGIN
        IF landscape THEN BEGIN
            IF inches THEN IF inches THEN ysize = 7.0 ELSE ysize = 7.0 * 2.54
        ENDIF ELSE BEGIN
            IF inches THEN ysize = 6.0 ELSE ysize = 6.0 * 2.54
        ENDELSE
   ENDIF
   IF N_Elements(langlevel) EQ 0 THEN langlevel = 1

   avantgarde = Keyword_Set(avantgarde)
   bold = Keyword_Set(bold)
   book = Keyword_Set(book)
   bookman = Keyword_Set(bookman)
   cmyk = Keyword_Set(cmyk)
   courier = Keyword_Set(courier)
   demi = Keyword_Set(demi)
   helvetica = Keyword_Set(helvetica)
   IF N_Elements(isolatin) EQ 0 THEN isolatin = 1 ELSE isolatin = Keyword_Set(isolatin)
   italic = Keyword_Set(italic)
   light = Keyword_Set(light)
   medium = Keyword_Set(medium)
   metric = Keyword_Set(metric)
   narrow = Keyword_Set(narrow)
   oblique = Keyword_Set(oblique)
   palatino = Keyword_Set(palatino)
   schoolbook = Keyword_Set(schoolbook)
   symbol = Keyword_Set(symbol)
   times = Keyword_Set(times)
   truetype = Keyword_Set(truetype)
   zapfchancery = Keyword_Set(zapfchancery)
   zapfdingbats = Keyword_Set(zapfdingbats)

   fontset = [avantgarde, bookman, courier, helvetica, palatino, schoolbook, symbol, times, zapfchancery, zapfdingbats]
   index = Where(fontset EQ 1, count)
   IF count EQ 0 THEN self.fontnameSet = 'Helvetica' ELSE self.fontnameSet = (*self.fontnames)[index[0]]
   IF self.fontnameSet EQ 'AvantGarde' THEN self.avantgarde = 1 ELSE self.avantgarde = 0
   IF self.fontnameSet EQ 'Bookman' THEN self.bookman = 1 ELSE self.bookman = 0
   IF self.fontnameSet EQ 'Courier' THEN self.courier = 1 ELSE self.courier = 0
   IF self.fontnameSet EQ 'Helvetica' THEN self.helvetica = 1 ELSE self.helvetica = 0
   IF self.fontnameSet EQ 'Palatino' THEN self.palatino = 1 ELSE self.palatino = 0
   IF self.fontnameSet EQ 'Schoolbook' THEN self.schoolbook = 1 ELSE self.schoolbook = 0
   IF self.fontnameSet EQ 'Symbol' THEN self.symbol = 1 ELSE self.symbol = 0
   IF self.fontnameSet EQ 'Times' THEN self.times = 1 ELSE self.times = 0
   IF self.fontnameSet EQ 'ZapfChancery' THEN self.zapfchancery = 1 ELSE self.zapfchancery = 0
   IF self.fontnameSet EQ 'ZapfDingbats' THEN self.zapfdingbats = 1 ELSE self.zapfdingbats = 0

   self.bitsSet = StrTrim(bits_per_pixel, 2)
   self.decomposedSet = decomposed
   self.cmykSet = cmyk
   self.colorSet = color
   self.directorySet = directory
   self.metric = metric
   self.encapsulationSet = encapsulated
   self.filenameSet = filename
   self.fonttypeSet = -1 > fonttype < 1
   self.fontsizeSet = Strtrim( 6 > Fix(fontsize) < 36, 2)
   self.fontstyleSet = [bold, book, demi, italic, light, medium, narrow, oblique]
   self.inchesSet = inches
   self.isolatinSet = isolatin
   self.landscapeSet = landscape
   self.langlevelSet = langlevel
   self.name = name
   self.pagetype = pagetype
   self.pagetypeSet = pagetype
   self.previewSet = preview
   self.truetypeSet = truetype
   self.set_fontSet = set_font

   ; Offsets are harder to set because I am trying to shield the
   ; user from PostScript weirdness.

   IF landscape THEN BEGIN
      dims = self->PageDimensions()
      self.xoffsetSet = dims[1] - yoffset
      self.xsizeSet = xsize
      self.yoffsetSet = xoffset
      self.ysizeSet = ysize
   ENDIF ELSE BEGIN
      self.xoffsetSet = xoffset
      self.xsizeSet = xsize
      self.yoffsetSet = yoffset
      self.ysizeSet = ysize
   ENDELSE

   ; Get the correct directory separator.

    CASE StrUpCase(!Version.OS_Family) OF
       'WINDOWS' : sep = '\'    ; PCs
       'MACOS'   : sep = ':'    ; Macintoshes
       'VMS'     : sep = ']'    ; VMS machines
       ELSE      : sep = '/'    ; Unix machines
    ENDCASE
    
    index = FSC_PSCONFIG_RStrPos(directory, sep)
    IF index EQ - 1 THEN BEGIN
       self.fullFilenameSet = directory + sep + filename
    ENDIF ELSE BEGIN
       IF index EQ (StrLen(directory) - 1) THEN self.fullFilenameSet = directory + filename ELSE $
           self.fullFilenameSet = directory + sep + filename
    ENDELSE

ENDIF ELSE self->SetDefault, defaultsetup

   ; Save this configuration.

self->SaveConfiguration

RETURN, 1
END ;--------------------------------------------------------------------------------


PRO FSC_PSCONFIG__DEFINE

   struct = { FSC_PSCONFIG,            $

                  ; Font specifiers.

              avantgarde: 0,           $
              bkman: 0,                $
              bookman: 0,              $
              courier: 0,              $
              helvetica: 0,            $
              palatino: 0,             $
              portrait: 0,             $
              schoolbook: 0,           $
              symbol: 0,               $
              times: 0,                $
              zapfchancery: 0,         $
              zapfdingbats: 0,         $

                  ; Widget identifiers.

              clearbuttonID: 0L,            $ ; Clear Font Style settings button.
              tlb: 0L,                      $ ; The top-level base of the GUI.
              acceptID: 0L,                 $ ; The Accept or Apply button ID.
              revertID: 0L,                 $ ; The Revert button ID.
              pagetypeID: Obj_New(),        $ ; The pagetype droplist ID.
              orientationID: Obj_New(),     $ ; The orientation droplist ID.
              unitsID: Obj_New(),           $ ; The units droplist ID.
              encapsulationID: Obj_New(),   $ ; The encapsulation droplist ID.
              previewID: Obj_New(),         $ ; The preview droplist ID.
              colorID: Obj_New(),           $ ; The color droplist ID.
              cmykID: Obj_New(),            $ ; The cmyk droplist ID.
              bitsID: Obj_New(),            $ ; The bits droplist ID.
              decomposedID: Obj_New(),      $ ; The decomposed droplist ID.
              defaultsID: Obj_New(),        $ ; The defaults droplist ID.
              plotID: Obj_New(),            $ ; The plot window ID.
              filenameID: Obj_New(),        $ ; The filename widget ID.
              fontnameID: Obj_New(),        $ ; The font name droplist ID.
              fontsizeID: Obj_New(),        $ ; The font size droplist ID.
              fontstyleID: Obj_New(),       $ ; The font style droplist ID.
              fontstyleButtonID: LonArr(8), $ ; The pagetype droplist ID.
              fonttypeID: Obj_New(),        $ ; The font style button IDs.
              isolatinID: Obj_New(),        $ ; The isolatin1 droplist ID.
              langlevelID: Obj_New(), $     $ ; The language level droplist ID.
              set_fontID: Obj_New(),        $ ; The set font text widget ID.
              stylebaseID: 0L,              $ ; The style base widget ID.
              truetypeID: Obj_New(),        $ ; The true-type droplist ID.
              xsizeID: Obj_New(),           $ ; The xsize field ID.
              ysizeID: Obj_New(),           $ ; The ysize field  ID.
              xoffsetID: Obj_New(),         $ ; The xoffset field  ID.
              yoffsetID: Obj_New(),         $ ; The yoffset field ID.

                  ; Data values and flags.

               debug: 0,             $ ; Set if debugging is turned on.
               fontInfo: 0,          $ ; Set if the user wants font information on GUI.
               fontnames: Ptr_New(), $ ; The allowed font names.
;               european: 0,          $ ; Set if European units and page size are in effect.
               pagetype: "",         $ ; The current page size or type.
               metric: 0,            $ ; Set if metric units and page size are in effect.
               name: "",             $ ; The "name" of the object.
               noblock: 0,           $ ; A flag that tells whether the GUI is in NOBLOCK state.
               cancel: 0,            $ ; A flag that tells the status of the Cancel button.
               thisDevice: "",       $ ; The entry graphics device. Will be restored.

                  ; GUI property identifiers.

               bitsSet: "",             $ ; Sets the bits_per_pixel value.
               cmykSet:0,               $ ; Sets the CMYK on/off value.
               colorSet: 0,             $ ; Sets the color on/off value.
               decomposedSet: 0,        $ ; Set the decomposed value.
               defaultsSet: "",         $ ; Sets the default setup value.
               directorySet: "",        $ ; Sets the directory name value.
               inchesSet: 0,            $ ; Sets the units value.
               isolatinSet: 1,          $ ; Sets the isolatin1 value.
               encapsulationSet: 0,     $ ; Sets the encapsulation value.
               filenameSet: "",         $ ; Sets the file name value.
               fullFilenameSet: "",     $ ; Sets the fully-qualified filename.
               fontsizeSet: "",         $ ; Sets the font size value.
               fontstyleSet: IntArr(8), $ ; Sets the font style values.
               fontnameSet: "",         $ ; Sets the font name value.
               fonttypeSet: 0,          $ ; Sets the font type value.
               landscapeSet: 0,         $ ; Sets the orientation value.
               langlevelSet: 0,         $ ; Set the language level value.
               pagetypeSet: "",         $ ; Sets the page type or size value.
               previewSet: 0,           $ ; Sets the preview value.
               set_fontSet: "",         $ ; Sets the Set_Font font name value.
               truetypeSet: 0,          $ ; Sets the true-type font value.
               xoffsetSet: 0.0,         $ ; Sets the xoffset value.
               xsizeSet: 0.0,           $ ; Sets the xsize value.
               yoffsetSet: 0.0,         $ ; Sets the yoffset value.
               ysizeSet: 0.0,           $ ; Sets the ysize value.

                  ; GUI revert property identifiers. Mirrors of GUI property identifiers above.

               bitsRevert: "", $
               cmykRevert:0, $
               colorRevert: 0, $
               decomposedRevert: 0, $
               defaultsRevert: "", $
               directoryRevert: "", $
               inchesRevert: 0, $
               isolatinRevert: 1, $
               encapsulationRevert: 0, $
               filenameRevert: "", $
               fontsizeRevert: "", $
               fontstyleRevert: IntArr(8), $
               fontnameRevert: "", $
               fonttypeRevert: 0, $
               landscapeRevert: 0, $
               langlevelRevert: 0, $
               pagetypeRevert: "", $
               previewRevert: 0, $
               set_fontRevert: "", $
               truetypeRevert: 0, $
               xoffsetRevert: 0.0, $
               xsizeRevert: 0.0, $
               yoffsetRevert: 0.0, $
               ysizeRevert: 0.0 $

}

END
