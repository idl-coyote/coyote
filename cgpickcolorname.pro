; docformat = 'rst'
;
; NAME:
;   cgPickColorName
;
; PURPOSE:
;   Provides an interactive method of selecting a draw color name. The program is useful
;   for learning the color names available in the Coyote Graphics System.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2000-2012, by Fanning Software Consulting, Inc. All rights reserved.      ;
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
;
;+
; Provides an interactive method of selecting a draw color name. The program is useful
; for learning the color names available in the Coyote Graphics System.
; 
;  .. image:: cgpickcolorname.png
;  
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics, Utilities, Color
;    
; :Examples:
;       To call the program from the IDL comamnd line::
;
;          IDL> color = cgPickColorName() & Print, color
;
;       To call the program from within a widget program::
;
;          color = cgPickColorName("red", Group_Leader=event.top) & Print, color
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;    Change History::
;       Written by: David W. Fanning, 31 August 2000.
;       Modified program to read colors from a file and to use more
;         colors on 24-bit platforms. 16 October 2000. DWF.
;       Added the COLUMNS keyword. 16 October 2000. DWF.
;       Fixed a small problem with mapping a modal widget. 2 Jan 2001. DWF
;       Now drawing small box around each color. 13 March 2003. DWF.
;       Added eight new colors. Total now of 104 colors. 11 August 2005. DWF.
;       Modified GUI to display system colors. 13 Dec 2005. DWF.
;       Added BREWER keyword to allow Brewer Colors. 15 May 2008. DWF.
;       Added all BREWER names to the default naming scheme. 3 July 2008. DWF.
;       Set a size for the color name label widget. Otherwise, the widget always
;          jumps back to the center of the display when I select a color on UNIX
;          machines. Also had to remove TLB updating with UPDATE keyword to avoid 
;          tickling the same IDL bug. Sigh... 13 March (Friday) 2009.
;       Removed system color names, since these are no longer available in cgColor. 
;          27 Nov 2010. DWF
;       Renamed cgPickColorName. 20 Oct 2012. DWF.
;       Added 12 colors suggested by Paul Krummel for people with color blindness. See the last line in 
;          Figure 3 of `this reference <http://www.sron.nl/~pault/>`. 16 Jan 2015. DWF.
;
; :Copyright:
;     Copyright (c) 2000-2015, Fanning Software Consulting, Inc.
;-


;+
; This event handler responds to users selecting a color patch for display.
;
; :Params:
;     event: in, required, type=struct
;        The event structure passed to the event handler.
;-
PRO cgPickColorName_Select_Color, event

  IF event.type NE 0 THEN RETURN
  
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; Get the color names from the window you clicked on and set it.
  Widget_Control, event.id, Get_UValue=thisColorName
  Widget_Control, info.labelID, Set_Value=thisColorName
  
  ; Get the color value and load it as the current color.
  WSet, info.mixWID
  info.theName = thisColorName
  theIndex = Where(info.colorNames EQ StrUpCase(StrCompress(thisColorName, /Remove_All)))
  theIndex = theIndex[0]
  info.nameIndex = theIndex
  
  IF info.theDepth GT 8 THEN BEGIN
     Erase, info.colors24[theIndex]
     PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.black
  ENDIF ELSE BEGIN
     TVLCT, info.red[theIndex], info.green[theIndex], info.blue[theIndex], info.mixcolorIndex
     Erase,info.mixcolorIndex
     PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.black
  ENDELSE
  
  Widget_Control, event.top, Set_UValue=info, /No_Copy
  
END ;---------------------------------------------------------------------------



;+
; This event handler responds to users selecting buttons on the GUI.
;
; :Params:
;     event: in, required, type=struct
;        The event structure passed to the event handler.
;-
PRO cgPickColorName_Buttons, event

  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; Which button is this?
  Widget_Control, event.id, Get_Value=buttonValue
  
  ; Branch on button value.
  CASE buttonValue OF
  
     'Cancel': BEGIN
  
        ; Simply destroy the widget. The pointer info is already
        ; set up for a CANCEL event.
        Widget_Control, event.top, /Destroy
        ENDCASE
  
     'Accept': BEGIN
  
        ; Get the new color table after the color has been selected.
        TVLCT, r, g, b, /Get
  
        ; Save the new color and name in the pointer.
        *(info.ptr) = {cancel:0.0, r:info.red[info.nameIndex], g:info.green[info.nameIndex], $
           b:info.blue[info.nameIndex], name:info.theName}
  
        ; Destoy the widget.
        Widget_Control, event.top, /Destroy
  
        ENDCASE
  ENDCASE
  
END ;---------------------------------------------------------------------------


;+
; This function creates the widgets that implements the color name selection widget.
; 
; :Params:
;     theName: in, optional, type=string, default="white"
;        A string with the name of the initial "display" color. Colors available are these::
;
;            Almond         Antique White   Aquamarine      Beige            Bisque        Black
;            Blue           Blue Violet     Brown           Burlywood        Cadet Blue    Charcoal
;            Chartreuse     Chocolate       Coral           Cornflower Blue  Cornsilk      Crimson
;            Cyan           Dark Goldenrod  Dark Gray        Dark Green      Dark Khaki    Dark Orchid
;            Dark Red       Dark Salmon     Dark Slate Blue  Deep Pink       Dodger Blue   Firebrick
;            Forest Green   Gold            Goldenrod        Gray            Green         Green Yellow
;            Honeydew       Hot Pink        Indian Red       Ivory           Khaki         Lavender
;            Lawn Green     Light Coral     Light Cyan       Light Gray      Light Salmon  Light Sea Green
;            Light Yellow   Lime Green      Linen            Magenta         Maroon        Medium Gray
;            Medium Orchid  Moccasin        Navy             Olive           Olive Drab    Orange
;            Orange Red     Orchid          Pale Goldenrod   Pale Green      Papaya        Peru
;            Pink           Plum            Powder Blue      Purple          Red           Rose
;            Rosy Brown     Royal Blue      Saddle Brown     Salmon          Sandy Brown   Sea Green
;            Seashell       Sienna          Sky Blue         Slate Blue      Slate Gray    Snow
;            Spring Green   Steel Blue      Tan              Teal            Thistle       Tomato
;            Turquoise      Violet          Violet Red       Wheat           White         Yellow
;            
;       The color WHITE is used if this parameter is absent.
;
;       These Brewer Color names are also available::
;
;            WT1       WT2       WT3       WT4       WT5       WT6       WT7       WT8
;            TAN1      TAN2      TAN3      TAN4      TAN5      TAN6      TAN7      TAN8
;            BLK1      BLK2      BLK3      BLK4      BLK5      BLK6      BLK7      BLK8
;            GRN1      GRN2      GRN3      GRN4      GRN5      GRN6      GRN7      GRN8
;            BLU1      BLU2      BLU3      BLU4      BLU5      BLU6      BLU7      BLU8
;            ORG1      ORG2      ORG3      ORG4      ORG5      ORG6      ORG7      ORG8
;            RED1      RED2      RED3      RED4      RED5      RED6      RED7      RED8
;            PUR1      PUR2      PUR3      PUR4      PUR5      PUR6      PUR7      PUR8
;            PBG1      PBG2      PBG3      PBG4      PBG5      PBG6      PBG7      PBG8
;            YGB1      YGB2      YGB3      YGB4      YGB5      YGB6      YGB7      YGB8
;            RYB1      RYB2      RYB3      RYB4      RYB5      RYB6      RYB7      RYB8
;            TG1       TG2       TG3       TG4       TG5       TG6       TG7       TG8
;       
;        Here are color names for colors appropriate for color blind users::
;
;           CG1 CG2 CG3 CG4 CG5 CG6 CG7 CG8 CG9 CG10 CG11 CG12
;           
; :Keywords:
;     bottom: in, optional, type=integer
;        The colors used in the program are loaded in the color table. This keyword indicates 
;        the index in which the colors start loading. By default `Bottom` is set equal to 
;        256-NCOLORS-1.
;      brewer: in, optional
;         Previously used to select Brewer colors. No longer in use, as the Brewer colors
;         are loaded automatically.
;      cancel: out, optional, type=integer
;          On exit, this keyword value is set to 0 if the user selected
;          the ACCEPT button. IF the user selected the CANCEL button, or
;          closed the window in any other way, this keyword value is set to 1.
;      columns: in, optional, type=integer, default=12
;          Set this keyword to the number of columns to use in the color display.
;      filename: in, optional, type=string
;          The name of an ASCII file that can be opened to read in
;          color values and color names. There should be one color per row
;          in the file. Please be sure there are no blank lines in the file.
;          The format of each row should be::
;
;              redValue  greenValue  blueValue  colorName
;
;          Color values should be between 0 and 255. Any kind of white-space
;          separation (blank characters, commas, or tabs) are allowed. The color
;          name should be a string, but it should NOT be in quotes. A typical
;          entry into the file would look like this::
;
;               255   255   0   Yellow
;       group_leader: in, optional, type=long
;          This keyword identifies a group leader if the program is called
;          from within a widget program. Note that this keyword *must* be provided
;          if you want to guarantee modal widget functionality. (If you don't know
;          what this means, believe me, you WANT to use this keyword, always.)
;       index: in, optional
;          This keyword identifies a color table index where the selected color
;          is to be loaded when the program exits. The default behavior is to restore
;          the input color table and NOT load a color.
;       title: in, optional, type=string, default="Select a Color"
;          This keyword accepts a string value for the window title. 
;-
FUNCTION cgPickColorName, theName, $         ; The name of the starting color.
   Brewer=brewer, $                        ; Select brewer colors.
   Bottom=bottom, $                        ; The index number where the colors should be loaded.
   Cancel=cancelled, $                     ; An output keyword set to 1 if the user cancelled or an error occurred.
   Columns = ncols, $                      ; The number of columns to display the colors in.
   Filename=filename, $                    ; The name of the file which contains color names and values.
   Group_Leader=group_leader, $            ; The group leader of the TLB. Required for modal operation.
   Index=index, $                          ; The color index number where the final selected color should be loaded.
   Title=title                             ; The title of the top-level base widget.

  ; Error handling for this program module.
  Catch, theError
  IF theError NE 0 THEN BEGIN
     Catch, /Cancel
     ok = cgErrorMsg(/Traceback)
     cancel = 1
     IF N_Elements(theName) NE 0 THEN RETURN, theName ELSE RETURN, 'White'
  ENDIF
  
  ; Get depth of visual display.
  IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
  
  ; Is there a filename? If so, get colors from there.
  IF N_Elements(filename) NE 0 THEN BEGIN
  
     ; Count the number of rows in the file.
     NCOLORS = File_Lines(filename)
  
     ; Read the data.
     OpenR, lun, filename, /Get_Lun
     red = BytArr(NCOLORS)
     green = BytArr(NCOLORS)
     blue = BytArr(NCOLORS)
     colors = StrArr(NCOLORS)
     redvalue = 0B
     greenvalue = 0B
     bluevalue = 0B
     namevalue = ""
     FOR j=0L, NCOLORS-1 DO BEGIN
        ReadF, lun, redvalue, greenvalue, bluevalue, namevalue
        colors[j] = namevalue
        red[j] = redvalue
        green[j] = greenvalue
        blue[j] = bluevalue
     ENDFOR
  
     ; Trim the colors array of blank characters.
     colors = StrTrim(colors, 2)
  
     ; Calculate the number of columns to display colors in.
     IF N_Elements(ncols) EQ 0 THEN ncols = Fix(Sqrt(ncolors))
     Free_Lun, lun
  ENDIF ELSE BEGIN
  
     IF theDepth GT 8 THEN BEGIN
  
     ; The colors with their names.
     IF N_Elements(ncols) EQ 0 THEN ncols = 12
     IF Keyword_Set(brewer) THEN BEGIN
         colors = [ 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
         red =   [   255,   255,   255,   255,   255,   245,   255,   250 ]
         green = [   255,   250,   255,   255,   248,   245,   245,   240 ]
         blue =  [   255,   250,   240,   224,   220,   220,   238,   230 ]
         colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
         red =   [ red,      250,   255,    255,    255,    255,    245,    222,    210 ]
         green = [ green,    235,   239,    235,    228,    228,    222,    184,    180 ]
         blue =  [ blue,     215,   213,    205,    196,    181,    179,    135,    140 ]
         colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
         red =   [ red,      250,   230,    210,    190,    112,     110,    70,       0 ]
         green = [ green,    250,   230,    210,    190,    128,     110,    70,       0 ]
         blue =  [ blue,     250,   230,    210,    190,    128,     110,    70,       0 ]
         colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
         red =   [ red,      250,   223,    173,    109,     53,     35,      0,       0 ]
         green = [ green,    253,   242,    221,    193,    156,     132,    97,      69 ]
         blue =  [ blue,     202,   167,    142,    115,     83,      67,    52,      41 ]
         colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
         red =   [ red,      232,   202,    158,     99,     53,     33,      8,       8 ]
         green = [ green,    241,   222,    202,    168,    133,    113,     75,      48 ]
         blue =  [ blue,     250,   240,    225,    211,    191,    181,    147,     107 ]
         colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
         red =   [ red,      254,    253,    253,    250,    231,    217,    159,    127 ]
         green = [ green,    236,    212,    174,    134,     92,     72,     51,     39 ]
         blue =  [ blue,     217,    171,    107,     52,     12,      1,      3,      4 ]
         colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
         red =   [ red,      254,    252,    252,    248,    225,    203,    154,    103 ]
         green = [ green,    232,    194,    146,     97,     45,     24,     12,      0 ]
         blue =  [ blue,     222,    171,    114,     68,     38,     29,     19,     13 ]
         colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
         red =   [ red,      244,    222,    188,    152,    119,    106,     80,     63 ]
         green = [ green,    242,    221,    189,    148,    108,     82,     32,      0 ]
         blue =  [ blue,     248,    237,    220,    197,    177,    163,    139,    125 ]
         colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
         red =   [ red,      243,    213,    166,     94,     34,      3,      1,      1 ]
         green = [ green,    234,    212,    189,    164,    138,    129,    101,     70 ]
         blue =  [ blue,     244,    232,    219,    204,    171,    139,     82,     54 ]
         colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
         red =   [ red,      244,    206,    127,     58,     30,     33,     32,      8 ]
         green = [ green,    250,    236,    205,    175,    125,     95,     48,     29 ]
         blue =  [ blue,     193,    179,    186,    195,    182,    168,    137,     88 ]
         colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
         red =   [ red,       201,    245,    253,    251,    228,    193,    114,     59 ]
         green = [ green,      35,    121,    206,    253,    244,    228,    171,     85 ]
         blue =  [ blue,       38,    72,     127,    197,    239,    239,    207,    164 ]
         colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
         red =   [ red,      84,    163,   197,   220,   105,    51,    13,     0 ]
         green = [ green,    48,    103,   141,   188,   188,   149,   113,    81 ]
         blue =  [ blue,      5,     26,    60,   118,   177,   141,   105,    71 ]   
     ENDIF ELSE BEGIN
         colors= ['White']
         red =   [ 255]
         green = [ 255]
         blue =  [ 255]
         colors= [ colors,      'Snow',     'Ivory','Light Yellow',   'Cornsilk',      'Beige',   'Seashell' ]
         red =   [ red,            255,          255,          255,          255,          245,          255 ]
         green = [ green,          250,          255,          255,          248,          245,          245 ]
         blue =  [ blue,           250,          240,          224,          220,          220,          238 ]
         colors= [ colors,     'Linen','Antique White',    'Papaya',     'Almond',     'Bisque',  'Moccasin' ]
         red =   [ red,            250,          250,          255,          255,          255,          255 ]
         green = [ green,          240,          235,          239,          235,          228,          228 ]
         blue =  [ blue,           230,          215,          213,          205,          196,          181 ]
         colors= [ colors,     'Wheat',  'Burlywood',        'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
         red =   [ red,            245,          222,          210,          230,          230,          210 ]
         green = [ green,          222,          184,          180,          230,          230,          210 ]
         blue =  [ blue,           179,          135,          140,          230,          250,          210 ]
         colors= [ colors,      'Gray', 'Slate Gray',  'Dark Gray',   'Charcoal',      'Black',   'Honeydew', 'Light Cyan' ]
         red =   [ red,            190,          112,          110,           70,            0,          240,          224 ]
         green = [ green,          190,          128,          110,           70,            0,          255,          255 ]
         blue =  [ blue,           190,          144,          110,           70,            0,          255,          240 ]
         colors= [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
         red =   [ red,            176,          135,         100,           95,            70,           30,           65,            0 ]
         green = [ green,          224,          206,         149,          158,           130,          144,          105,            0 ]
         blue =  [ blue,           230,          235,         237,          160,           180,          255,          225,          255 ]
         colors= [ colors,      'Navy', 'Pale Green','Aquamarine','Spring Green',       'Cyan' ]
         red =   [ red,              0,          152,          127,            0,            0 ]
         green = [ green,            0,          251,          255,          250,          255 ]
         blue =  [ blue,           128,          152,          212,          154,          255 ]
         colors= [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
         red =   [ red,             64,       143,             46,           34,             0,      173,           127,         124 ]
         green = [ green,          224,       188,            139,          139,           128,      255,           255,         252 ]
         blue =  [ blue,           208,       143,             87,           34,           128,       47,             0,           0 ]
         colors= [ colors,     'Green', 'Lime Green', 'Olive Drab',     'Olive','Dark Green','Pale Goldenrod']
         red =   [ red,              0,           50,          107,           85,            0,          238 ]
         green = [ green,          255,          205,          142,          107,          100,          232 ]
         blue =  [ blue,             0,           50,           35,           47,            0,          170 ]
         colors =[ colors,     'Khaki', 'Dark Khaki',     'Yellow',       'Gold','Goldenrod','Dark Goldenrod']
         red =   [ red,            240,          189,          255,          255,          218,          184 ]
         green = [ green,          230,          183,          255,          215,          165,          134 ]
         blue =  [ blue,           140,          107,            0,            0,           32,           11 ]
         colors= [ colors,'Saddle Brown',       'Rose',       'Pink', 'Rosy Brown','Sandy Brown',      'Peru']
         red =   [ red,            139,          255,          255,          188,          244,          205 ]
         green = [ green,           69,          228,          192,          143,          164,          133 ]
         blue =  [ blue,            19,          225,          203,          143,           96,           63 ]
         colors= [ colors,'Indian Red',  'Chocolate',     'Sienna','Dark Salmon',    'Salmon','Light Salmon' ]
         red =   [ red,            205,          210,          160,          233,          250,          255 ]
         green = [ green,           92,          105,           82,          150,          128,          160 ]
         blue =  [ blue,            92,           30,           45,          122,          114,          122 ]
         colors= [ colors,    'Orange',      'Coral', 'Light Coral',  'Firebrick',   'Dark Red',    'Brown',  'Hot Pink' ]
         red =   [ red,            255,          255,          240,          178,       139,          165,        255 ]
         green = [ green,          165,          127,          128,           34,         0,           42,        105 ]
         blue =  [ blue,             0,           80,          128,           34,         0,           42,        180 ]
         colors= [ colors, 'Deep Pink',    'Magenta',     'Tomato', 'Orange Red',        'Red', 'Crimson', 'Violet Red' ]
         red =   [ red,            255,          255,          255,          255,          255,      220,        208 ]
         green = [ green,           20,            0,           99,           69,            0,       20,         32 ]
         blue =  [ blue,           147,          255,           71,            0,            0,       60,        144 ]
         colors= [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
         red =   [ red,            176,          216,          221,          238,          218,          186 ]
         green = [ green,           48,          191,          160,          130,          112,           85 ]
         blue =  [ blue,            96,          216,          221,          238,          214,          211 ]
         colors= [ colors,'Dark Orchid','Blue Violet',     'Purple']
         red =   [ red,            153,          138,          160]
         green = [ green,           50,           43,           32]
         blue =  [ blue,           204,          226,          240]
         colors= [ colors, 'Slate Blue',  'Dark Slate Blue']
         red =   [ red,           106,            72]
         green = [ green,            90,            61]
         blue =  [ blue,           205,           139]
         colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
         red =   [ red,     255,   255,   255,   255,   255,   245,   255,   250 ]
         green = [ green,   255,   250,   255,   255,   248,   245,   245,   240 ]
         blue =  [ blue,    255,   250,   240,   224,   220,   220,   238,   230 ]
         colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
         red =   [ red,      250,   255,    255,    255,    255,    245,    222,    210 ]
         green = [ green,    235,   239,    235,    228,    228,    222,    184,    180 ]
         blue =  [ blue,     215,   213,    205,    196,    181,    179,    135,    140 ]
         colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
         red =   [ red,      250,   230,    210,    190,    112,     110,    70,       0 ]
         green = [ green,    250,   230,    210,    190,    128,     110,    70,       0 ]
         blue =  [ blue,     250,   230,    210,    190,    128,     110,    70,       0 ]
         colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
         red =   [ red,      250,   223,    173,    109,     53,     35,      0,       0 ]
         green = [ green,    253,   242,    221,    193,    156,     132,    97,      69 ]
         blue =  [ blue,     202,   167,    142,    115,     83,      67,    52,      41 ]
         colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
         red =   [ red,      232,   202,    158,     99,     53,     33,      8,       8 ]
         green = [ green,    241,   222,    202,    168,    133,    113,     75,      48 ]
         blue =  [ blue,     250,   240,    225,    211,    191,    181,    147,     107 ]
         colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
         red =   [ red,      254,    253,    253,    250,    231,    217,    159,    127 ]
         green = [ green,    236,    212,    174,    134,     92,     72,     51,     39 ]
         blue =  [ blue,     217,    171,    107,     52,     12,      1,      3,      4 ]
         colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
         red =   [ red,      254,    252,    252,    248,    225,    203,    154,    103 ]
         green = [ green,    232,    194,    146,     97,     45,     24,     12,      0 ]
         blue =  [ blue,     222,    171,    114,     68,     38,     29,     19,     13 ]
         colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
         red =   [ red,      244,    222,    188,    152,    119,    106,     80,     63 ]
         green = [ green,    242,    221,    189,    148,    108,     82,     32,      0 ]
         blue =  [ blue,     248,    237,    220,    197,    177,    163,    139,    125 ]
         colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
         red =   [ red,      243,    213,    166,     94,     34,      3,      1,      1 ]
         green = [ green,    234,    212,    189,    164,    138,    129,    101,     70 ]
         blue =  [ blue,     244,    232,    219,    204,    171,    139,     82,     54 ]
         colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
         red =   [ red,      244,    206,    127,     58,     30,     33,     32,      8 ]
         green = [ green,    250,    236,    205,    175,    125,     95,     48,     29 ]
         blue =  [ blue,     193,    179,    186,    195,    182,    168,    137,     88 ]
         colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
         red =   [ red,       201,    245,    253,    251,    228,    193,    114,     59 ]
         green = [ green,      35,    121,    206,    253,    244,    228,    171,     85 ]
         blue =  [ blue,       38,    72,     127,    197,    239,    239,    207,    164 ]
         colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
         red =   [ red,      84,    163,   197,   220,   105,    51,    13,     0 ]
         green = [ green,    48,    103,   141,   188,   188,   149,   113,    81 ]
         blue =  [ blue,      5,     26,    60,   118,   177,   141,   105,    71 ]   
         colors = [ colors, 'CG1', 'CG2', 'CG3', 'CG4', 'CG5', 'CG6', 'CG7', 'CG8']
         red =   [ red,  51,    102,   136,    68,    17,   153,   221,    102 ]
         green = [ green,  34,    153,   204,   170,   119,   153,   204,     17]
         blue =  [ blue, 136,    204,   238,   153,    51,    51,   119,      0 ]
         colors = [ colors, 'CG9', 'CG10', 'CG11', 'CG12']
         red =   [ red,  204,    170,   136,   170 ]
         green = [ green,  102,     68,    34,   68 ]
         blue =  [ blue,  119,    102,    85,   153 ]
     ENDELSE
     ENDIF ELSE BEGIN
  
     IF N_Elements(ncols) EQ 0 THEN ncols = 8
     colors  = ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
     red =     [  0,        255,       0,      255,       0  ]
     green =   [  0,          0,     255,      255,     255  ]
     blue =    [  0,        255,     255,        0,       0  ]
     colors  = [colors,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
     red =     [red,     255,     0,      0,    255,    112]
     green =   [green,     0,     0,      0,    127,    219]
     blue =    [blue,      0,   255,    115,    127,    147]
     colors  = [colors,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray','White']
     red =     [red,     219,      0,     245,       80,      135,    255  ]
     green =   [green,   112,    163,     245,       80,      135,    255  ]
     blue =    [blue,    219,    255,     220,       80,      135,    255  ]
  
     ENDELSE
  ENDELSE
  
  NCOLORS = N_Elements(colors)
  
  ; Save decomposed state and restore it, if possible.
  IF Float(!Version.Release) GE 5.2 THEN BEGIN
     Device, Get_Decomposed=decomposedState
  ENDIF ELSE decomposedState = 0
  
  ; Different color decomposition based on visual depth.
  IF theDepth GT 8 THEN BEGIN
     Device, Decomposed=1
     colors24 = cgColor24([[red], [green], [blue]])
  ENDIF ELSE BEGIN
     IF NCOLORS GT !D.Table_Size THEN $
        Message, /NoName, 'Number of colors exceeds color table size. Returning...'
     Device, Decomposed=0
     colors24 = -1
  ENDELSE
  
  ; Check argument values. All arguments are optional.
  IF N_Elements(theName) EQ 0 THEN IF Keyword_Set(brewer) THEN theName = 'WT1' ELSE theName = 'White'
  IF Size(theName, /TName) NE 'STRING' THEN $
     Message, 'Color name argument must be STRING type.', /NoName
  theName = StrCompress(theName, /Remove_All)
  
  IF N_Elements(bottom) EQ 0 THEN bottom = 0 > (!D.Table_Size - (NCOLORS + 2))
  mixcolorIndex = bottom
  IF N_Elements(title) EQ 0 THEN title='Select a Color'
  
  ; We will work with all uppercase names.
  colorNames = StrUpCase(StrCompress(colors, /Remove_All))
  
  ; Get the current color table vectors before we change anything.
  ; This will allow us to restore the color table when we depart.
  TVLCT, r_old, g_old, b_old, /Get
  
  ; Load the colors if needed. The "bottom" index is reserved as the "mixing color" index.
  IF theDepth LE 8 THEN TVLCT, red, green, blue, bottom+1
  
  ; Can you find the color name in the colors array?
  nameIndex = WHERE(colorNames EQ StrUpCase(theName), count)
  IF count EQ 0 THEN BEGIN
     Message, 'Unable to resolve color name: ' + StrUpCase(theName) + '. Replacing with WHITE.', /Informational
     theName = 'WT1'
     nameIndex = WHERE(colorNames EQ StrUpCase(theName), count)
     IF count EQ 0 THEN Message, /NoName, 'Unable to resolve color name: ' + StrUpCase(theName) + '. Returning...'
  ENDIF
  nameIndex = nameIndex[0]
  
  ; Who knows how the user spelled the color? Make it look nice.
  theName = colors[nameIndex]
  
  ; Load the mixing color in the mixcolorIndex.
  IF theDepth LE 8 THEN TVLCT, red[nameIndex], green[nameIndex], blue[nameIndex], mixcolorIndex
  
  ; Create the widgets. TLB is MODAL or BLOCKING, depending upon presence of
  ; Group_Leader variable.
  IF N_Elements(group_leader) EQ 0 THEN BEGIN
     tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center)
  ENDIF ELSE BEGIN
     tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, /Modal, $
        Group_Leader=group_leader)
  ENDELSE
  
  ; Draw widgets for the possible colors. Store the color name in the UVALUE.
  colorbaseID = Widget_Base(tlb, Column=ncols, Event_Pro='cgPickColorName_Select_Color')
  drawID = LonArr(NCOLORS)
  FOR j=0,NCOLORS-1 DO BEGIN
     drawID[j] = Widget_Draw(colorbaseID, XSize=20, YSize=15, $
        UValue=colors[j], Button_Events=1)
  ENDFOR
  
  ; System colors.
  IF N_Elements(colors) GT NCOLORS THEN BEGIN
     systemColorbase = Widget_Base(tlb, Column=8, Event_Pro='cgPickColorName_Select_Color')
     drawID = [Temporary(drawID), LonArr(8)]
     FOR j=NCOLORS,N_Elements(colors)-1 DO BEGIN
        drawID[j] = Widget_Draw(systemColorbase, XSize=20, YSize=15, $
           UValue=colors[j], Button_Events=1)
     ENDFOR
  ENDIF
  
  ; Set up the current or mixing color draw widget.
  currentID = Widget_Base(tlb, Column=1, Base_Align_Center=1)
  
  ; Special concerns with LABEL widgets to work around a widget update bug in
  ; X11 libraries.
  IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' $
      THEN labelID = Widget_Label(currentID, Value=theName, /Dynamic_Resize) $
      ELSE labelID = Widget_Label(currentID, Value=theName, /Dynamic_Resize, SCR_XSIZE=150)
  mixColorID = Widget_Draw(currentID, XSize=60, YSize=15)
  
  ; CANCEL and ACCEPT buttons.
  buttonbase = Widget_Base(tlb, ROW=1, Align_Center=1, Event_Pro='cgPickColorName_Buttons')
  cancelID = Widget_Button(buttonbase, VALUE='Cancel')
  acceptID = Widget_Button(buttonbase, VALUE='Accept')
  
  ; Center the TLB.
  cgCenterTLB, tlb
  Widget_Control, tlb, /Realize
  
  ; Load the drawing colors.
  wids = IntArr(NCOLORS)
  IF theDepth GT 8 THEN BEGIN
     FOR j=0,NCOLORS-1 DO BEGIN
        Widget_Control, drawID[j], Get_Value=thisWID
        wids[j] = thisWID
        WSet, thisWID
        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=colors24[j]
     ENDFOR
     IF (N_Elements(colors) GT NCOLORS) THEN BEGIN
        wids = [Temporary(wids), Intarr(8)]
        FOR j=NCOLORS, N_Elements(colors)-1 DO BEGIN
        Widget_Control, drawID[j], Get_Value=thisWID
        wids[j] = thisWID
        WSet, thisWID
        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=colors24[j]
        ENDFOR
     ENDIF
  ENDIF ELSE BEGIN
     FOR j=1,NCOLORS DO BEGIN
        Widget_Control, drawID[j-1], Get_Value=thisWID
        wids[j-1] = thisWID
        WSet, thisWID
        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=bottom + black
     ENDFOR
     IF (N_Elements(colors) GT NCOLORS) THEN BEGIN
        wids = [Temporary(wids), Intarr(8)]
        FOR j=NCOLORS+1, N_Elements(colors) DO BEGIN
        Widget_Control, drawID[j-1], Get_Value=thisWID
        wids[j-1] = thisWID
        WSet, thisWID
        PolyFill, [1,1,19,19,1], [0,13,13,0,0], /Device, Color=bottom + j
        ENDFOR
     ENDIF
  ENDELSE
  
  ; Load the current or mixing color.
  Widget_Control, mixColorID, Get_Value=mixWID
  WSet, mixWID
  IF theDepth GT 8 THEN BEGIN
     Erase, colors24[nameIndex]
     eraseColor = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
     black = Where(colornames EQ eraseColor)
     black = black[0]
     PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=colors24[black]
  ENDIF ELSE BEGIN
     eraseColor = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
     black = Where(colornames EQ eraseColor)
     Erase, mixcolorIndex
     PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=black
  ENDELSE
  
  ; Pointer to hold the color form information.
  ptr = Ptr_New({cancel:1.0, r:0B, g:0B, b:0B, name:theName})
  
  ; Info structure for program information.
  info = { ptr:ptr, $                   
           mixColorIndex:mixColorIndex, $
           colorNames:colorNames, $
           nameIndex:nameIndex, $
           red:red, $
           green:green, $
           blue:blue, $
           black:black, $
           colors24:colors24, $
           mixWid:mixWid, $
           theDepth:theDepth, $
           labelID:labelID, $
           theName:theName $
         }
  
  ; Store the info structure in the UVALUE of the TLB.
  Widget_Control, tlb, Set_UValue=info, /No_Copy
  
  ; Set up program event loop. This will be blocking widget
  ; if called from the IDL command line. Program operation
  ; will stop here until widget interface is destroyed.
  XManager, 'pickcolor', tlb
  
  ; Retrieve the color information from the pointer and free the pointer.
  colorInfo = *ptr
  Ptr_Free, ptr
  
  ; Set the Cancel flag.
  cancelled = colorInfo.cancel
  
  ; Restore color table, taking care to load the color index if required.
  IF N_Elements(index) NE 0 AND (NOT cancelled) THEN BEGIN
     r_old[index] = colorInfo.r
     g_old[index] = colorInfo.g
     b_old[index] = colorInfo.b
  ENDIF
  TVLCT, r_old, g_old, b_old
  
  ; Restore decomposed state if possible.
  IF Float(!Version.Release) GE 5.2 THEN Device, Decomposed=decomposedState
  
  ; Return the color name.
  RETURN, colorInfo.name
  
END
