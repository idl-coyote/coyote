;+
; NAME:
;   DRAWCOLORS
;
; FILENAME:
;
;   drawcolors__define.pro
;
; PURPOSE:
;
;   The purpose of this object program is provide a flexible way
;   to handle and select drawing colors. The program combines
;   features of two previous programs: GetColor and PickColor,
;   as well as adding features of its own. Sixteen original
;   colors are supplied, but users can create any color they
;   wish using the tools provided.
;
;   By default, these 16 colors are defined: Black, Magenta, Cyan, Yellow,
;   Green, Red, Blue, Navy, Aqua, Pink, Orchid, Sky, Beige, Charcoal, Gray, White.
;
; AUTHOR:
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   2642 Bradbury Court
;   Fort Collins, CO 80521 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;   General programming.
;
; CALLING SEQUENCE:
;
;   colors = Obj_New('DRAWCOLORS')
;
; OPTIONAL INPUT PARAMETERS:
;
;      IDL> colors = Obj_New('DRAWCOLORS', red, green, blue, names)
;
;   RED -- A 16-element byte vector of red values for the drawing colors.
;
;   GREEN -- A 16-element byte vector of green values for the drawing colors.
;
;   BLUE -- A 16-element byte vector of blue values for the drawing colors.
;
;   NAMES -- A 16-element string vector of names for the drawing colors.
;
;   By default, these colors are defined: Black, Magenta, Cyan, Yellow,
;   Green, Red, Blue, Navy, Aqua, Pink, Orchid, Sky, Beige, Charcoal, Gray, White.
;
; COMMON BLOCKS:
;
;   None.
;
; RESTRICTIONS:
;
;   If you are going to use the XCOLORS method, you will need
;   the XColors program from the Coyote library:
;
;     http://www.dfanning.com/programs/xcolors.pro
;
;   The program is set up to handle 16 drawing colors. You may
;   modify the program to have a different number, but you will
;   have to modify the code in two places: (1) in the drawcolors__define
;   module and (2) in the INIT method.
;
; FUNCTION METHODS:
;
;   COLOR24 ***************************************************************************
;
;      Purpose:
;
;       Turns a color triple into the equivalent 24-bit color integer value that
;       can be decomposed into the color.

;      Definition:
;
;          FUNCTION DrawColors::Color24, theColor
;
;     Parameters:
;
;        theColor -- A 3-element vector, representing a color triple.
;
;     Example:
;
;        yellow = colors->GetColor("yellow")
;        yellow24 = colors->Color24(yellow)
;
;
;   GETCOLOR ***************************************************************************
;
;      Purpose:
;
;       Returns the color triple, color index number, or the
;       24-bit integer representation, of the asked for color. In
;       normal operation, the colors are: Black, Magenta, Cyan, Yellow,
;       Green, Red, Blue, Navy, Aqua, Pink, Orchid, Sky, Beige, Charcoal,
;       Gray, and White.
;
;      Definition:
;
;          FUNCTION DrawColors::GetColor, theColor, startIndex, INDEXNUMBER=indexnumber, $
;             TRUECOLOR=truecolor, AutoDetermine=autodetermine
;
;     Parameters:
;
;        theColor -- A string representing the "name" of the color. If the name
;              can't be resolved or found, the first color is returned.
;
;        startIndex -- If present, and INDEXNUMBER is set, the colors are loaded at
;              this index number. Otherwise, the self.startIndex is used.
;
;     Keywords:
;
;        AUTODETERMINE -- If this keyword is set. the state of DECOMPOSITION is
;             determined (IDL 5.2 and higher) and either the INDEXNUMBER or
;             TRUECOLOR keyword is set appropriately. If the decomposition state
;             cannot be determined, the INDEXNUMBER keyword is set.
;
;        INDEXNUMBER -- If this keyword is set the colors are loaded and the
;             index number of the color is returned.
;
;        TRUECOLOR -- If this keyword is set, the color triple is converted into
;             a 24-bit integer before being returned. This keyword is ignored
;             if the INDEXNUMBER keyword is set.
;
;     Examples:
;
;        drawColor = colors->GetColor("yellow")
;        drawColor = colors->GetColor("blue", /Indexnumber)
;        drawColor = colors->GetColor("sky", /Truecolor)
;
;
;   GETCOLORS **************************************************************************
;
;      Purpose:
;
;       Returns the color triples, the color index numbers of, or the
;       24-bit integer representations of, all the colors.

;      Definition:
;
;          FUNCTION DrawColors::GetColors, startindex, INDEXNUMBER=indexnumber, $
;             TRUECOLOR=truecolor, Structure=structure
;
;      Parameters:
;
;        startIndex -- If present, and INDEXNUMBER is set, the colors are loaded at
;              this index number. If absent, startIndex = self.startIndex.
;
;     Keywords:
;
;        INDEXNUMBER -- If this keyword is set the colors are loaded and the
;             index numbers of all the colors are returned.
;
;        STRUCTURE -- If this keyword is set, the return value is a structure,
;             where each field of the structure is a color name and the value
;             of each field is either a color triple, an index number, or a
;             24-bit color value, depending upon the state of other keywords.
;
;        TRUECOLOR -- If this keyword is set, the color triples are converted to
;             24-bit integers before being returned. This keyword is ignored
;             if the INDEXNUMBER keyword is set.
;
;     Examples:
;
;        drawColors = colors->GetColors()  ; drawColors is a 16-by-3 byte array.
;        drawColors = colors->GetColors(/IndexNumber) ; drawColors is a 16-element array of color indices.
;        drawColors = colors->GetColors(/TrueColor) ; drawColors is a 16-element array of 24-bit integers.
;        drawColors = colors->GetColors(/IndexNumber, /Structure) ; drawColors is a structure of index numbers.
;        Plot, data, Color=drawColors.yellow, Background=drawColors.charcoal
;
;
;   SELECT  **************************************************************************
;
;      Purpose:
;
;       Puts up a blocking or modal widget dialog, allowing the user to select
;       from one of the 16 predefined colors available, or to mix their own color.
;       The user-defined color triple is returned as a result of the function.
;
;      Definition:
;
;          FUNCTION DrawColors::Select, Color=currentColor, StartIndex=startIndex, $
;             Title=title, Group_Leader=groupLeader, Cancel=cancelled, TrueColor=truecolor
;
;     Keywords:
;
;        CANCEL -- An output keyword that will return a value of 1 if the CANCEL
;             button is selected or if program operation is interrupted in any way.
;
;        COLOR -- The index number in the color table, where the current color
;             will be mixed. In other words, this color index will change when
;             the program is on the display. It will be restored to its previous
;             or entry color when the program exits.
;
;        GROUP_LEADER -- The group leader for the program. This keyword *must*
;             be set if calling this method from within a widget program if you
;             expect MODAL program operation.
;
;        NAME -- If this keyword is set, the return value of the function is
;             the "name" of the color.
;
;        STARTINDEX -- This is the starting index in the color table where the
;             16 predetermined colors will be loaded. The original colors will
;             be restored when the program exits. By default, this is set to
;             !D.Table-Size - (NCOLORS + 1).
;
;        TITLE -- The title of the program. By default: "Pick a Color"
;
;        TRUECOLOR -- If this keyword is set, the return value of the function
;            is a 24-bit eqivalent integer rather than the color triple.
;
;     Examples:
;
;        newColor = colors->Select()  ; A blocking widget.
;        newColor = color->Select(Group_Leader=event.top, Cancel=cancelled)
;        IF NOT cancelled THEN TVLCT, newColor, info.dataColor
;
;
;
; PROCEDURE METHODS:
;
;   GETPROPERTY ***********************************************************************
;
;    Purpose:
;
;       Allows the user to obtain the current properties of the object.
;
;    Definition:
;
;       PRO DrawColors::GetProperty, NAMES=names, RED=red, GREEN=green, BLUE=blue, $
;           STARTINDEX=startindex, NCOLORS=ncolors
;
;     Keywords:
;
;        NAMES -- Returns the current names of the colors as a string array.
;
;        RED -- Returns the current red values of the colors.
;
;        GREEN -- Returns the current green values of the colors.
;
;        BLUE -- Returns the current blue values of the colors.
;
;        STARTINDEX -- Returns the current starting index in the color table.
;
;        NCOLORS -- Returns the number of colors.
;
;     Example:
;
;        colors->GetProperty, Names=colorNames
;        Print, colorNames
;
;
;   LOADCOLORS ************************************************************************
;
;      Purpose:
;
;         Loads the predefined colors at a starting index.
;
;      Definition:
;
;         PRO DrawColors::LoadColors, startindex
;
;     Parameters:
;
;        STARTINDEX -- The starting color index in the color table. If not provided,
;            is set to !D.Table_Size - (self.ncolors + 1).
;
;     Example:
;
;        colors->LoadColors, 16
;
;
;   ORIGINALCOLORS *********************************************************************
;
;      Purpose:
;
;         Reloads the original 16 colors and their names
;
;      Definition:
;
;         PRO DrawColors::OriginalColors
;
;     Parameters:
;
;        None
;
;     Example:
;
;        colors->OriginalColors
;
;
;   REFRESH ***************************************************************************
;
;      Purpose:
;
;         Refreshes the modal GUI with the current drawing colors.
;
;      Definition:
;
;         PRO DrawColors::Refresh
;
;     Parameters:
;
;        None
;
;     Example:
;
;        colors->Refresh
;
;
;   SETPROPERTY ***********************************************************************
;
;    Purpose:
;
;       Allows the user to set the current properties of the object.
;
;    Definition:
;
;       PRO DrawColors::SetProperty, NAMES=names, RED=red, GREEN=green, BLUE=blue, $
;           STARTINDEX=startindex, NCOLORS=ncolors
;
;     Keywords:
;
;        NAMES -- The current names of the colors as a string array.
;
;        RED -- The current red values of the colors.
;
;        GREEN -- The current green values of the colors.
;
;        BLUE -- The current blue values of the colors.
;
;        STARTINDEX -- The current starting index in the color table.
;
;        NCOLORS -- The number of colors.
;
;     Example:
;
;        colorNames = 'Color ' + StrTrim(SIndGen(16),2)
;        colors->SetProperty, Names=colorNames
;
;
;   XCOLORS ***************************************************************************
;
;      Purpose:
;
;         Allows the user to select 16 new colors for the program by
;         using the XCOLORS program. The XColors program must be
;         somewhere in your !PATH.
;
;      Definition:
;
;         PRO DrawColors::XColors
;
;     Parameters:
;
;        None
;
;     Example:
;
;        colors->XColors
;
; TUTORIAL:
;
;   Here is a short tutorial in how this object can be used. Note
;   that this doesn't exhaust all the possibilities.
;
;  1. Create the object.
;
;     IDL> colors = Obj_New("DrawColors")
;
;  2. Find out what colors it knows about.
;
;     IDL> colors->GetProperty, Names=colorNames
;     IDL> Print, colorNames
;
;  3. Ask for a color by name and load it at a color
;     index. Draw a plot in that color.
;
;     IDL> yellow = colors->GetColor("yellow")
;     IDL> TVLCT, yellow, 200
;     IDL> Device, Decomposed=0
;     IDL> Plot, Findgen(11), Color=200
;
;  4. Do the same thing, but in DECOMPOSED color.
;
;     IDL> Device, Decomposed=1
;     IDL> green = colors->GetColor("green", /TrueColor)
;     IDL> Plot, Findgen(11), Color=green
;
;  5. Find the color index number of the sky blue color.
;
;     IDL> Device, Decomposed=0
;     IDL> skyIndex = colors->GetColor("sky", /IndexNumber)
;     IDL> Plot, Findgen(11), Color=skyIndex
;
;  6. Load all 16 drawing colors at color index 32.
;
;     IDL> colors->LoadColors, 32
;     IDL> CIndex ; If them, if you have CINDEX from my library.
;
;  7. Get a structure of colors, with each field set to
;     the appropriate index number of its associated color.
;
;     IDL> Device, Decomposed=0
;     IDL> col = colors->GetColors(/IndexNumber, /Structure)
;     IDL> Plot, Findgen(11), Color=col.yellow, Background=col.charcoal
;
;  8. Allow the user to select a color from a GUI, then
;     load it and use it.
;
;     IDL> Device, Decomposed=0
;     IDL> theColor = colors->Select(Cancel=cancelled)
;     IDL> IF NOT cancelled THEN TVLCT, theColor, 10
;     IDL> Plot, Findgen(11), Color=10
;
;  9. Allow the user to choose 16 new drawing colors.
;     (Requires my XCOLORS program.)
;
;     IDL> colors->XColors
;     IDL> theseColors = colors->Select()
;
; 10. Let the object decide according to the device decomposition
;     state whether to return an index number or 24-bit value
;     for the color.
;
;     IDL> Plot, Findgen(11), Color=colors->GetColor('beige', /Autodetermine)
;
; 11. Call the GUI from within a widget program and load the
;     new color.
;
;     newcolor = info.colors->Select(Group_Leader=event.top, $
;        Cancel=cancelled)
;     IF NOT cancelled THEN TVLCT, newcolor, info.drawColor
;
; 12. Destroy the object.
;
;     IDL> Obj_Destroy, colors
;
;
; MODIFICATION HISTORY:
;
;   Written by: David Fanning, 9 NOV 1999.
;   Added AUTODETERMINE keyword to the GetColor method. 10 NOV 1999. DWF.
;   Added NAME keyword to SELECT method. 18 MAR 2000. DWF.
;   Fixed a small bug in choosing the current color. 20 April 2000. DWF.
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
PRO DrawColors::Refresh

   ; Works only if GUI is on display.

IF Widget_Info(self.tlb, /Valid_ID) EQ 0 THEN RETURN

   ; Load the drawing colors.

FOR j=0, self.ncolors-1 DO BEGIN
   WSet, self.wids[j]
   PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.startIndex + j
ENDFOR

   ; Load the current color.

WSet, self.currentWID
TVLCT, self.r[self.currentColor], self.g[self.currentColor], $
   self.b[self.currentColor], self.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.currentColor

END ;--------------------------------------------------------------------------




PRO DrawColors::OriginalColors

; Method to load the original drawing colors and their names.

names =  ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
red =    [  0,        255,       0,      255,       0  ]
green =  [  0,          0,     255,      255,     255  ]
blue =   [  0,        255,     255,        0,       0  ]
names =  [names,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
red =    [red,     255,     0,      0,    255,    112]
green =  [green,     0,     0,      0,    127,    219]
blue =   [blue,      0,   255,    115,    127,    147]
self.names =  [names,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray', 'White']
self.red =    [red,      219,      0,     255,       80,      135,     255  ]
self.green =  [green,    112,    163,     171,       80,      135,     255  ]
self.blue =   [blue,     219,    255,     127,       80,      135,     255  ]

END ;--------------------------------------------------------------------------




PRO DrawColors::XColors_Load_Colors

; This method allows changing of drawing colors with XCOLORS program.
; This method is called from within XCOLORS.

TVLCT, r, g, b, /Get
red = r[self.startindex:self.startindex + self.ncolors-1]
green = g[self.startindex:self.startindex + self.ncolors-1]
blue = b[self.startindex:self.startindex + self.ncolors-1]
names = 'Color ' + StrTrim(Sindgen(self.ncolors), 2)
self->SetProperty, Red=red, Green=green, Blue=blue, Names=names
IF Widget_Info(self.tlb, /Valid_ID) THEN self->Refresh
END ;--------------------------------------------------------------------------




PRO DrawColors::XColors

; This method allows changing of drawing colors with XCOLORS program.

Catch, theError
IF theError NE 0 THEN BEGIN
    Message, ['The XCOLORS program is missing.', 'Download from the Coyote library:', $
       'http://www.dfanning.com/'], /Informational
    RETURN
ENDIF

self->LoadColors
struct = {XCOLORS_NOTIFYOBJ, OBJECT:self, METHOD:'XColors_Load_Colors', WID:!D.Window}
XColors, NColors=self.ncolors, Bottom=self.startIndex, NotifyObj=struct, $
   Title='Select Drawing Colors (' + StrTrim(self.currentWID, 2) + ')'
END ;--------------------------------------------------------------------------




PRO DrawColors::GetProperty, NAMES=names, RED=red, GREEN=green, BLUE=blue, $
    STARTINDEX=startindex, NCOLORS=ncolors

; Generic method for getting object properties.

IF Arg_Present(names) THEN names = self.names
IF Arg_Present(red) THEN red = self.red
IF Arg_Present(green) THEN green = self.green
IF Arg_Present(blue) THEN blue = self.blue
IF Arg_Present(startindex) THEN startindex = self.startindex
IF Arg_Present(ncolors) THEN ncolors = self.ncolors
END ;--------------------------------------------------------------------------




PRO DrawColors::SetProperty, NAMES=names, RED=red, GREEN=green, BLUE=blue, $
    STARTINDEX=startindex, NCOLORS=ncolors

; Generic method for setting object properties.

IF N_Elements(ncolors) NE 0 THEN self.ncolors = ncolors

   ; Check for correct length.

IF N_Elements(red) NE 0 THEN BEGIN
   IF N_Elements(red) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in RED parameter. Returning...', /Informational
      RETURN
   ENDIF
ENDIF

IF N_Elements(green) NE 0 THEN BEGIN
   IF N_Elements(green) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in GREEN parameter. Returning...', /Informational
      RETURN
   ENDIF
ENDIF

IF N_Elements(blue) NE 0 THEN BEGIN
   IF N_Elements(blue) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in BLUE parameter. Returning...', /Informational
      RETURN
   ENDIF
ENDIF

IF N_Elements(names) NE 0 THEN BEGIN
   IF N_Elements(names) NE self.ncolors THEN BEGIN
         Message, 'Incorrect number of elements in NAMES parameter. Returning...', /Informational
   RETURN
   ENDIF

   IF Float(!Version.Release) GE 5.2 THEN thisType = Size(names, /Type) ELSE BEGIN
      s = Size(names)
      thisType = s(s(0)+1)
   ENDELSE

   IF thisType NE 7 THEN BEGIN
         Message, 'NAMES paramether must be STRING type. Returning...', /Informational
   RETURN
   ENDIF
ENDIF

   ; Assign the values.

IF N_Elements(names) NE 0 THEN self.names = names
IF N_Elements(red) NE 0 THEN self.red = red
IF N_Elements(green) NE 0 THEN self.green = green
IF N_Elements(blue) NE 0 THEN self.blue = blue
IF N_Elements(startindex) NE 0 THEN self.startindex = startindex

END ;--------------------------------------------------------------------------




FUNCTION DrawColors::Color24, theColor

; This method converts a color triple to the corresponding 24-bit
; integer that can be decomposed into that color.

IF N_Elements(theColor) NE 3 THEN BEGIN
   Message, 'Augument must be a three-element vector. Returning...', /Informational
   RETURN, theColor
ENDIF

IF Max(theColor) GT 255 OR Min(theColor) LT 0 THEN BEGIN
   Message, 'Argument values must be in range of 0-255. Returning...', /Informational
   RETURN, theColor
ENDIF

base16 = [[1L, 16L], [256L, 4096L], [65536L, 1048576L]]

num24bit = 0L

FOR j=0,2 DO num24bit = num24bit + ((theColor(j) MOD 16) * base16(0,j)) + $
   (Fix(theColor(j)/16) * base16(1,j))

RETURN, num24bit
END ;--------------------------------------------------------------------------




FUNCTION DrawColors::GetColor, theColor, startIndex, INDEXNUMBER=indexnumber, $
   TRUECOLOR=truecolor, AUTODETERMINE=autodetermine

; This method returns the color triple representing the asked for color.
;

IF N_Elements(theColor) EQ 0 THEN theColor = self.names[0]
IF N_Elements(startIndex) EQ 0 THEN startIndex = self.startIndex

   ; If the AUTODETERMINE keyword is set, the return value will be
   ; an index number or a 24-bit value, depending upon the state of
   ; decomposition set for the device. (IDL 5.2 versions and higher.)

IF Keyword_Set(autodetermine) THEN BEGIN
   IF Float(!Version.Release) GE 5.2 THEN BEGIN
      IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
         Device, Get_Decomposed=decomposedState
      ENDIF ELSE decomposedState = 0
      IF decomposedState THEN truecolor = 1 ELSE indexnumber = 1
   ENDIF ELSE indexnumber = 1
ENDIF

theIndex = Where(StrUpCase(self.names) EQ StrUpCase(theColor), count)
IF count EQ 0 THEN theIndex = 0

IF Keyword_Set(indexnumber) THEN BEGIN
   self->LoadColors, startindex
   self.startindex = startindex
   RETURN, startindex + theIndex[0]
ENDIF

color = Reform([self.red[theIndex], self.green[theIndex], self.blue[theIndex]], 1, 3)

IF Keyword_Set(truecolor) THEN BEGIN
    color = self->Color24(color)
ENDIF

RETURN, color
END ;----------------------------------------------------------------------------



FUNCTION DrawColors::GetColors, startIndex, INDEXNUMBER=indexnumber, TRUECOLOR=truecolor, $
   Structure=structure

colors = [[self.red], [self.green], [self.blue]]
colors = Reform(colors, self.ncolors, 3)
IF N_Elements(startIndex) EQ 0 THEN startIndex = self.startIndex

IF Keyword_Set(indexnumber) THEN BEGIN
   self->LoadColors, startindex
   self.startindex = startindex
   numbers = startindex + Indgen(self.ncolors)
   returnValue = numbers
   IF Keyword_Set(structure) THEN BEGIN
      returnValue = Create_Struct(self.names[0], numbers[0])
      FOR j=1,self.ncolors-1 DO $
         returnValue = Create_Struct(returnValue, self.names[j], numbers[j])
   ENDIF
   RETURN, returnValue
ENDIF

IF Keyword_Set(truecolor) THEN BEGIN
    colors24 = LonArr(self.ncolors)
    FOR j=0,self.ncolors-1 DO colors24[j] = self->Color24(colors[j,*])
    returnValue = colors24
   IF Keyword_Set(structure) THEN BEGIN
      returnValue = Create_Struct(self.names[0], colors24[0])
      FOR j=1,self.ncolors-1 DO $
         returnValue = Create_Struct(returnValue, self.names[j], colors24[j])
   ENDIF
   RETURN, returnValue
ENDIF

IF Keyword_Set(structure) THEN BEGIN
   returnValue = Create_Struct(self.names[0], colors[0,*])
   FOR j=1,self.ncolors-1 DO $
      returnValue = Create_Struct(returnValue, self.names[j], colors[j,*])
   RETURN, returnValue
ENDIF

RETURN, colors
END ;----------------------------------------------------------------------------



PRO DrawColors::LoadColors, startindex

; The method loads the drawing colors starting at the startindex.

IF N_Elements(startindex) EQ 0 THEN BEGIN
   startindex = !D.Table_Size - (self.ncolors + 1)
ENDIF

TVLCT, self.red, self.green, self.blue, startindex
self.startindex = startindex
END ;----------------------------------------------------------------------------



PRO DrawColors::Button_Respond, event

; This event handler method responds to CANCEL and ACCEPT buttons.

Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF

   'Cancel': BEGIN
      TVLCT, self.r_old, self.g_old, self.b_old ; Restore old color table.
      Widget_Control, event.top, /Destroy
      ENDCASE

   'Accept': BEGIN
      TVLCT, r, g, b, /Get ; Get the new color table.
      TVLCT, self.r_old, self.g_old, self.b_old ; Restore old color table.

         ; Save the new color in the form info pointer.

      *(self.ptr) = {cancel:0.0, r:r[self.currentColor], g:g[self.currentColor], $
         b:b[self.currentColor], name:self.currentName}
      Widget_Control, event.top, /Destroy ; Exit

      ENDCASE
ENDCASE
END ;---------------------------------------------------------------------------



PRO DrawColors::Set_Slider_Color, event

; This event handler method allows the user to mix their own color.

   ; Get the color slider values.

Widget_Control, self.redID, Get_Value=red
Widget_Control, self.greenID, Get_Value=green
Widget_Control, self.blueID, Get_Value=blue

   ; Load the new color as the current color.

WSet, self.currentWID
TVLCT, red, green, blue, self.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.currentColor

END ;---------------------------------------------------------------------------



PRO DrawColors::Set_Standard_Color, event

; This event handler method permits color selection by clicking on a color window.

colorIndex = event.id - self.drawIDs[0]
r = self.red[colorIndex]
g = self.green[colorIndex]
b = self.blue[colorIndex]

   ; Get the color value and load it as the current color.

WSet, self.currentWID
TVLCT, r, g, b, self.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.currentColor

IF self.needSliders THEN BEGIN

      ; Update the slider values to this color value.

   Widget_Control, self.redID, Set_Value=r
   Widget_Control, self.greenID, Set_Value=g
   Widget_Control, self.blueID, Set_Value=b

ENDIF

   ; Get the current color name.

self.currentName = self.names[colorIndex]

END ;---------------------------------------------------------------------------


PRO DrawColors__Events, event

; This is the main event handler for the program. It
; dispatches events to the appropriate event handler method.
; It assumes the user value of the widget causing the event
; has the name of the event handler method.

Widget_Control, event.top, Get_UValue=selfObject
Widget_Control, event.id, Get_UValue=thisMethod
Call_Method, thisMethod, selfObject, event
END ;---------------------------------------------------------------------------



FUNCTION DrawColors::Select, Color=currentColor, StartIndex=startIndex, $
   Title=title, Group_Leader=groupLeader, Cancel=cancelled, $
      TrueColor=truecolor, Name=name

; This method implements a modal (or blocking, if you forget the Group Leader)
; dialog that permits the user to select one of 16 predetermined colors, or to
; mix their own color. The return value is the color triple, unless the
; TRUECOLOR keyword is set, in which case a 24 bit integer is returned. If the
; NAME keyword is set, the name of the color is returned.

IF N_Elements(title) EQ 0 THEN self.title = 'Select a Color' ELSE self.title = title
IF N_Elements(startIndex) EQ 0 THEN startIndex = !D.Table_Size - (self.ncolors + 1)
self.startIndex = startIndex < (!D.Table_Size - (self.ncolors + 1))
IF N_Elements(currentColor) EQ 0 THEN currentColor = (startIndex + self.ncolors - 1)
self.currentColor = currentColor < 255
IF N_Elements(groupLeader) EQ 0 THEN groupLeader = 0L
self.groupLeader = groupLeader
IF Keyword_Set(name) THEN self.needSliders = 0 ELSE self.needSliders = 1

   ; Device must support windows.

IF (!D.FLAGS AND 256) EQ 0 THEN BEGIN
   Message, 'Device must support windows. Returning...', /Continue
   self.cancelled = 1
   cancelled = 1
   RETURN, -1
ENDIF

   ; Working in decomposed color space with defined number of colors.
   ; Save decomposed state and restore it, if possible.

IF Float(!Version.Release) GE 5.2 THEN BEGIN
   Device, Get_Decomposed=decomposedState
ENDIF ELSE decomposedState = 0
Device, Decomposed=0

   ; Create the widgets. TLB is MODAL or BLOCKING.

IF self.groupLeader EQ 0L THEN BEGIN
   self.tlb = Widget_Base(Title=self.title, Column=1, /Base_Align_Center, $
      TLB_Frame_Attr=1, UValue=self)
ENDIF ELSE BEGIN
   self.tlb = Widget_Base(Title=self.title, Column=1, /Base_Align_Center, /Modal, $
      Group_Leader=groupLeader, TLB_Frame_Attr=1, UValue=self)
ENDELSE

self.colorbaseID = Widget_Base(self.tlb, Row=2)
FOR j=0,self.ncolors-1 DO BEGIN
   self.drawIDs[j] = Widget_Draw(self.colorbaseID, XSize=20, YSize=15, $
      Button_Events=1, UValue='Set_Standard_Color')
ENDFOR
currentIDBase = Widget_Base(self.tlb, Column=1, Base_Align_Center=1)
labelID = Widget_Label(currentIDBase, Value='Current Color')
self.currentColorID = Widget_Draw(currentIDBase, XSize=60, YSize=15)
nameIndex = self.currentColor - self.startIndex
IF nameIndex GE 0 AND nameIndex LE N_Elements(self.names) THEN BEGIN
   self.currentName = self.names[nameIndex]
ENDIF ELSE self.currentName = ""

IF self.needSliders THEN BEGIN

   sliderbase = Widget_Base(self.tlb, Column=1, Frame=1, Base_Align_Center=1)
   label = Widget_Label(sliderbase, Value='Specify a Color')

      ; Set the current color values in sliders.

   self.redID = Widget_Slider(sliderbase, Scr_XSize=200, Value=self.r[self.currentColor], $
      Max=255, Min=0, Title='Red', UValue='Set_Slider_Color')
   self.greenID = Widget_Slider(sliderbase, Scr_XSize=200, Value=self.g[self.currentColor], $
      Max=255, Min=0, Title='Green', UValue='Set_Slider_Color')
   self.blueID = Widget_Slider(sliderbase, Scr_XSize=200, Value=self.b[self.currentColor], $
      Max=255, Min=0, Title='Blue', UValue='Set_Slider_Color')

ENDIF ELSE BEGIN

   self.redID = 0L
   self.greenID = 0L
   self.blueID = 0L

ENDELSE

buttonbase = Widget_Base(self.tlb, ROW=1, Align_Center=1)
cancelID = Widget_Button(buttonbase, VALUE='Cancel', UValue='Button_Respond')
acceptID = Widget_Button(buttonbase, VALUE='Accept', UValue='Button_Respond')

Device, Get_Screen_Size=screenSize
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(self.tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, self.tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize
Widget_Control, self.tlb, /Realize

   ; Get the current color tables so they can be restored on exit.

TVLCT, r_old, g_old, b_old, /Get
self.r_old = r_old
self.g_old = g_old
self.b_old = b_old

TVLCT, self.red, self.green, self.blue, self.startIndex
TVLCT, r, g, b, /Get
self.r = r
self.g = g
self.b = b

   ; Load the drawing colors.

FOR j=0, self.ncolors-1 DO BEGIN
   Widget_Control, self.drawIDs[j], Get_Value=thisWID
   self.wids[j] = thisWID
   WSet, thisWID
   PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.startIndex + j
ENDFOR

   ; Load the current color.

Widget_Control, self.currentColorID, Get_Value=currentWID
self.currentWID = currentWID
WSet, self.currentWID
TVLCT, self.r[self.currentColor], self.g[self.currentColor], $
   self.b[self.currentColor], self.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=self.currentColor

   ; Pointer to hold the form information.

self.ptr = Ptr_New({cancel:1.0, r:self.r_old[self.currentColor], $
   g:self.g_old[self.currentColor], b:self.b_old[self.currentColor], $
   name:self.currentName})

   ; Block here until widget program is destroyed.

XManager, 'pickcolor', self.tlb , Event_Handler='DrawColors__Events'

   ; Retrieve the color information.

colorInfo = *(self.ptr)
Ptr_Free, self.ptr
self.cancelled = colorInfo.cancel
cancelled = self.cancelled

   ; Restore decomposed state if possible.

IF Float(!Version.Release) GE 5.2 THEN Device, Decomposed=decomposedState

   ; Return the appropriate return value.

IF Keyword_Set(name) THEN RETURN, colorInfo.name
IF Keyword_Set(truecolor) THEN RETURN, self->Color24(Reform([colorInfo.r, colorInfo.g, colorInfo.b], 1, 3))
RETURN, Reform([colorInfo.r, colorInfo.g, colorInfo.b], 1, 3)
END ;--------------------------------------------------------------------------



PRO DrawColors::CLEANUP
Ptr_Free, self.ptr
END ;--------------------------------------------------------------------------



FUNCTION DrawColors::INIT, red, green, blue, names

; The INIT method simply loads the original 16 colors.

Catch, theError
IF theError NE 0 THEN BEGIN
   IF (!D.FLAGS AND 256) EQ 0 THEN BEGIN
      Message, !Error_State.Msg, /Informational
      RETURN, 0
   ENDIF ELSE BEGIN
      ok = Dialog_Message(!Error_State.Msg)
      Print, !Error_State.Msg
      RETURN, 0
   ENDELSE
ENDIF

IF self.ncolors EQ 0 THEN self.ncolors = 16

IF N_Params() EQ 1 OR N_Params() EQ 2 OR N_Params() EQ 3 THEN BEGIN
   Message, 'Incorrect number of parameters. Returning...', /Informational
   RETURN, 0
ENDIF

IF N_Params() EQ 4 THEN BEGIN
   IF N_Elements(red) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in RED parameter. Returning...', /Informational
      RETURN, 0
   ENDIF ELSE self.red = red
   IF N_Elements(green) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in GREEN parameter. Returning...', /Informational
      RETURN, 0
   ENDIF ELSE self.green = green
   IF N_Elements(blue) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in BLUE parameter. Returning...', /Informational
      RETURN, 0
   ENDIF ELSE self.blue = blue
   IF N_Elements(names) NE self.ncolors THEN BEGIN
      Message, 'Incorrect number of elements in NAMES parameter. Returning...', /Informational
      RETURN, 0
   ENDIF
   IF Float(!Version.Release) GE 5.2 THEN thisType = Size(names, /Type) ELSE BEGIN
      s = Size(names)
      thisType = s(s(0)+1)
   ENDELSE
   IF thisType NE 7 THEN BEGIN
      Message, 'NAMES paramether must be STRING type. Returning...', /Informational
      RETURN, 0
   ENDIF
   self.names = names
   RETURN, 1
ENDIF

   ; Load the drawing colors and their names.

names =  ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
red =    [  0,        255,       0,      255,       0  ]
green =  [  0,          0,     255,      255,     255  ]
blue =   [  0,        255,     255,        0,       0  ]
names =  [names,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
red =    [red,     255,     0,      0,    255,    112]
green =  [green,     0,     0,      0,    127,    219]
blue =   [blue,      0,   255,    115,    127,    147]
self.names =  [names,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray', 'White']
self.red =    [red,      219,      0,     255,       80,      135,     255  ]
self.green =  [green,    112,    163,     171,       80,      135,     255  ]
self.blue =   [blue,     219,    255,     127,       80,      135,     255  ]

self.startIndex = !D.Table_Size - (self.ncolors + 1)

RETURN, 1
END ;----------------------------------------------------------------------



PRO DrawColors__Define

   ncolors = 16
   struct = { DRAWCOLORS, $                      ; The object class name.
              ncolors:0L, $                      ; The number of predefined drawing colors.
              names: StrArr(ncolors), $          ; The names of the predefined colors.
              red: BytArr(ncolors), $            ; The red values of the predefined colors.
              green: BytArr(ncolors), $          ; The green values of the predefined colors.
              blue: BytArr(ncolors), $           ; The blue values of the predefined colors.
              startIndex: 0L, $                  ; The starting index where predefined colors are loaded.
              currentColor: 0L, $                ; The current color index.
              currentColorID: 0L, $              ; The ID of the current color draw widget.
              currentWID:0L, $                   ; The current (selected) window index number.
              currentName:"", $                  ; The name of the current color.
              r_old: BytArr(!D.Table_Size), $    ; The red values of the old color table.
              g_old: BytArr(!D.Table_Size), $    ; The green values of the old color table.
              b_old: BytArr(!D.Table_Size), $    ; The blue values of the old color table.
              r: BytArr(!D.Table_Size), $        ; The red values of the currently loaded color table.
              g: BytArr(!D.Table_Size), $        ; The green values of the currently loaded color table.
              b: BytArr(!D.Table_Size), $        ; The blue values of the currently loaded color table.
              groupLeader: 0L, $                 ; The group leader for the GUI.
              tlb: 0L, $                         ; The top-level base ID of the GUI.
              title:'', $                        ; The title of the GUI window.
              needSliders:0L, $                  ; A flag in the SELECT method to determine if sliders are needed.
              colorbaseID: 0L, $                 ; The widget ID of the color base, holding the drawing colors.
              drawIDs: LonArr(ncolors), $        ; A vector of draw widget IDs.
              wIDs: LonArr(ncolors), $           ; A vector of window index numbers.
              redID: 0L, $                       ; The widget ID of the red slider.
              greenID: 0L, $                     ; The widget ID of the green slider.
              blueID: 0L, $                      ; The widget ID of the blue slider.
              cancelled: 0.0, $                  ; A flag indicating (1) if the CANCEL button was selected.
              ptr: Ptr_New() $                   ; A pointer holding the form dialog data.
            }
  END

