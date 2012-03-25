;+
; NAME:
;       PICKCOLOR
;
; PURPOSE:
;
;       A modal dialog widget allowing the user to select
;       the RGB color triple specifying a color. The return
;       value of the function is the color triple specifying the
;       color or the "name" of the color if the NAME keyword is set.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Graphics, Color Specification. See related program cgColor.
;
; CALLING SEQUENCE:
;
;       color = PickColor(colorindex)
;
; RETURN VALUE:
;
;       The return value of the function is a 1-by-3 array containing
;       the values of the color triple that specifies the selected color.
;       The color can be loaded, for example, in any color index:
;
;           color = PickColor(240)
;           TVLCT, color, 240
;
;       The return value is the original color triple if the user
;       selects the CANCEL button.
;
;       IF the NAMES keyword is set, the return value of the function is
;       the "name" of the selected color. This would be appropriate for
;       passing to the cgColor program, for example.
;
; OPTIONAL INPUT POSITIONAL PARAMETERS:
;
;       COLORINDEX: The color index of the color to be changed. If not
;              specified the color index !D.Table_Size - 2 is used.
;              The Current Color and the Color Sliders are set to the
;              values of the color at this color index.
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;
;       BREWER: Set this keyword if you wish to use the Brewer Colors, as defined
;              in this reference:
;
;              http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_intro.html
;
;       GROUP_LEADER: The group leader for this widget program. This
;              keyword is required for MODAL operation. If not supplied
;              the program is a BLOCKING widget. Be adviced, however, that
;              the program will NOT work if called from a blocking widget
;              program, unless a GROUP_LEADER is supplied.
;
;       NAMES: Set this keyword to return the "name" of the selected color
;              rather than its color triple.
;
;       STARTINDEX: 88 pre-determined colors are loaded The STARTINDEX
;              is the index in the color table where these 88 colors will
;              be loaded. By default, it is !D.Table_Size - 89.
;
;       TITLE: The title on the program's top-level base. By default the
;              title is "Pick a Color".
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;
;       CANCEL: A keyword that is set to 1 if the CANCEL button is selected
;              and to 0 otherwise.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       88 pre-determined colors are loaded in the color table.
;       In addition, the color index at COLORINDEX is modified while
;       the program is on the display. When the program exits, the
;       entry color table is restored. Thus, on 8-bit displays there
;       might be some color effects in graphics windows while PICKCOLOR
;       is on the display. Changes in the color table are not noticable
;       on 16-bit and 24-bit displays.
;
; EXAMPLE:
;
;       To specify a color for a plot in color decomposition OFF mode:
;
;          Device, Decomposed=0
;          !P.Color = !P.Color < (!D.Table_Size - 1)
;          color = PickColor(!P.Color, Cancel=cancelled)
;          IF NOT cancelled THEN BEGIN
;              TVLCT, color, !P.Color
;              Plot, data
;          ENDIF
;
;       To specify a color for a plot in color decomposition ON mode:
;
;          Device, Decomposed=1
;          color = PickColor(Cancel=cancelled)
;          !P.Color = Color24(color)
;          IF NOT cancelled THEN Plot, data
;
;        To obtain the name of the selected color to pass to GetColor:
;
;          selectedColor = PickColor(/Name)
;          axisColor = cgColor(selectedColor, !D.Table_Size-4)
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, 28 Oct 99.
;       Added NAME keyword. 18 March 2000, DWF.
;       Fixed a small bug when choosing a colorindex less than !D.Table_Size-17. 20 April 2000. DWF.
;       Added actual color names to label when NAMES keyword selected. 12 May 2000. DWF.
;       Modified to use 88 colors and cgColor instead of 16 colors and GETCOLOR. 4 Dec 2000. DWF.
;       Now drawing small box around each color. 13 March 2003. DWF.
;       Added CURRENTCOLOR keyword. 3 July 2003. DWF.
;       Added BREWER keyword. 15 May 2008. DWF.
;       Fixed a couple of problems with outline color. 19 May 2008. DWF.
;       Added all the colors available from cgColor. 28 Nov 2010. DWF.
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

PRO PickColor_CenterTLB, tlb

Device, Get_Screen_Size=screenSize
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize
END ;---------------------------------------------------------------------------



PRO PickColor_Select_Color, event

; This event handler permits color selection by clicking on a color window.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the color names from the window you clicked on.

Widget_Control, event.id, Get_UValue=thisColorName

IF info.needsliders EQ 0 THEN Widget_Control, info.labelID, Set_Value=thisColorName

   ; Get the color value and load it as the current color.

WSet, info.currentWID
thisColor = cgColor(thisColorName, /Triple, BREWER=info.brewer)
info.currentName = thisColorName
TVLCT, thisColor, info.currentColorIndex
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=info.currentColorIndex
PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.outlineColor

IF info.needSliders THEN BEGIN

      ; Update the slider values to this color value.

   Widget_Control, info.redID, Set_Value=thisColor[0,0]
   Widget_Control, info.greenID, Set_Value=thisColor[0,1]
   Widget_Control, info.blueID, Set_Value=thisColor[0,2]

ENDIF

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------


PRO PickColor_Sliders, event

; This event handler allows the user to mix their own color.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the color slider values.

Widget_Control, info.redID, Get_Value=red
Widget_Control, info.greenID, Get_Value=green
Widget_Control, info.blueID, Get_Value=blue

   ; Load the new color as the current color.

WSet, info.currentWID
TVLCT, red, green, blue, info.currentColorIndex
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=info.currentColorIndex
PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=info.outlineColor
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------



PRO PickColor_Buttons, event

; This event handler responds to CANCEL and ACCEPT buttons.

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF

   'Cancel': BEGIN
      TVLCT, info.r_old, info.g_old, info.b_old ; Restore old color table.
      Widget_Control, event.top, /Destroy       ; Exit.
      ENDCASE

   'Accept': BEGIN
      TVLCT, r, g, b, /Get ; Get the new color table.
      TVLCT, info.r_old, info.g_old, info.b_old ; Restore old color table.

         ; Save the new color in the form info pointer.

      *(info.ptr) = {cancel:0.0, r:r[info.currentColorIndex], g:g[info.currentColorIndex], $
         b:b[info.currentColorIndex], name:info.currentName}
      Widget_Control, event.top, /Destroy ; Exit

      ENDCASE
ENDCASE
END ;---------------------------------------------------------------------------



FUNCTION PickColor, currentColorIndex, StartIndex=startIndex, Title=title, $
   Group_Leader=groupLeader, Cancel=cancelled, Names=name, CurrentColor=currentColor, $
   BREWER=brewer

   ; Device must support windows.

IF (!D.FLAGS AND 256) EQ 0 THEN BEGIN
   Message, 'Device must support windows. Returning...', /Continue
   cancelled = 1
   RETURN, -1
ENDIF

   ; Working in decomposed color space with defined number of colors.
   ; Save decomposed state and restore it, if possible.

IF Float(!Version.Release) GE 5.2 THEN BEGIN
   Device, Get_Decomposed=decomposedState
ENDIF ELSE decomposedState = 0

Device, Decomposed=0
NCOLORS = 192

   ; Check parameters.

IF N_Elements(title) EQ 0 THEN title = 'Pick a Color'
IF N_Elements(startIndex) EQ 0 THEN startIndex = !D.Table_Size - (NCOLORS + 1)
startIndex = startIndex < (!D.Table_Size - (NCOLORS + 1))
IF N_Elements(currentColorIndex) EQ 0 THEN currentColorIndex = (startIndex + NCOLORS - 1)
currentColorIndex = currentColorIndex < 255
IF Keyword_Set(name) THEN needSliders = 0 ELSE needSliders = 1

   ; Get the current color tables so they can be restored on exit.

TVLCT, r_old, g_old, b_old, /Get

   ; Load the new drawing colors and get their names.

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
   ENDELSE

NCOLORS = N_Elements(colors)
colorNames = colors
nameIndex = currentColorIndex - startIndex
IF nameIndex GE 0 AND nameIndex LE N_Elements(colorNames) THEN BEGIN
   currentName = colorNames[nameIndex]
ENDIF ELSE currentName = ""
TVLCT, red, green, blue, startIndex
TVLCT, r, g, b, /Get
IF Keyword_Set(names) THEN labelTitle = currentName ELSE labelTitle = 'Current Color'

   ; Create the widgets. TLB is MODAL or BLOCKING.

IF N_Elements(groupLeader) EQ 0 THEN BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, Map=0)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, /Modal, $
      Group_Leader=groupLeader)
ENDELSE

colorbaseID = Widget_Base(tlb, Column=12, Event_Pro='PickColor_Select_Color')
drawID = LonArr(NCOLORS)
FOR j=0,NCOLORS-1 DO BEGIN
   drawID[j] = Widget_Draw(colorbaseID, XSize=20, YSize=15, $
      UValue=colorNames[j], Button_Events=1)
ENDFOR
currentID = Widget_Base(tlb, Column=1, Base_Align_Center=1)
labelID = Widget_Label(currentID, Value=labelTitle, /Dynamic_Resize)
currentColorID = Widget_Draw(currentID, XSize=60, YSize=15)

IF needSliders THEN BEGIN

   sliderbase = Widget_Base(tlb, COLUMN=1, FRAME=1, BASE_ALIGN_CENTER=1, $
      EVENT_PRO='PickColor_Sliders')
   label = Widget_Label(sliderbase, Value='Specify a Color')

      ; Set the current color values in sliders.

   redID = Widget_Slider(sliderbase, Scr_XSize=200, Value=r[currentColorIndex], $
      Max=255, Min=0, Title='Red')
   greenID = Widget_Slider(sliderbase, Scr_XSize=200, Value=g[currentColorIndex], $
      Max=255, Min=0, Title='Green')
   blueID = Widget_Slider(sliderbase, Scr_XSize=200, Value=b[currentColorIndex], $
      Max=255, Min=0, Title='Blue')

ENDIF ELSE BEGIN

   redID = 0L
   greenID = 0L
   blueID = 0L

ENDELSE

buttonbase = Widget_Base(tlb, ROW=1, Align_Center=1, Event_Pro='PickColor_Buttons')
cancelID = Widget_Button(buttonbase, VALUE='Cancel')
acceptID = Widget_Button(buttonbase, VALUE='Accept')

   ; Center the TLB.

PickColor_CenterTLB, tlb
Widget_Control, tlb, /Realize
Widget_Control, tlb, Map=1

   ; Load the drawing colors.

wids = IntArr(NCOLORS)
FOR j=0, NCOLORS-1 DO BEGIN
   Widget_Control, drawID[j], Get_Value=thisWID
   wids[j] = thisWID
   WSet, thisWID
   ;PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=startIndex + j
   Erase, startIndex + j
   outlineName = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
   black = Where(StrUpCase(colornames) EQ outlineName)
   black = black[0]
   PlotS, [0,0,19,19,0], [0,14,14,0,0], /Device, Color=startIndex + black
ENDFOR

   ; Load the current color.

Widget_Control, currentColorID, Get_Value=currentWID
WSet, currentWID
IF N_Elements(currentcolor) NE 0 THEN $
   TVLCT, Reform(currentcolor, 1, 3), currentColorIndex ELSE $
   TVLCT, r[currentColorIndex], g[currentColorIndex], b[currentColorIndex], currentColorIndex
Erase, currentColorIndex
   outlineName = Keyword_Set(brewer) ? 'BLK8' : 'BLACK
   black = Where(StrUpCase(colornames) EQ outlineName)
black = black[0]
PlotS, [0,0,58,58,0], [0,13,13,0,0], /Device, Color=startIndex + black

outlineColor = startIndex + black

   ; Pointer to hold the form information.

ptr = Ptr_New({cancel:1.0, r:r_old[currentColorIndex], g:g_old[currentColorIndex], $
         b:b_old[currentColorIndex], name:currentName})

   ; Info structure for program information.

info = { ptr:ptr, $                    ; The pointer to the form information.
         r_old:r_old, $                ; The old color table.
         g_old:g_old, $
         b_old:b_old, $
         r:r, $                        ; The new color table.
         g:g, $
         b:b, $
         outlineColor:outlineColor, $  ; The outline color.
         brewer:Keyword_Set(brewer), $ ; Using Brewer Colors.
         labelID:labelID, $
         needSliders:needSliders, $    ; A flag that indicates if sliders are needed.
         redID:redID, $                ; The IDs of the color sliders.
         greenID:greenID, $
         blueID:blueID, $
         currentName:currentName, $    ; The current color name.
         currentColorIndex:currentColorIndex, $  ; The current color index.
         currentWID:currentWID, $      ; The current color window index number.
         wids:wids $                   ; The window index number of the drawing colors.
       }

Widget_Control, tlb, Set_UValue=info, /No_Copy
XManager, 'pickcolor', tlb ; Block here until widget program is destroyed.

   ; Retrieve the color information.

colorInfo = *ptr
Ptr_Free, ptr
cancelled = colorInfo.cancel

   ; Restore decomposed state if possible.

IF Float(!Version.Release) GE 5.2 THEN Device, Decomposed=decomposedState

   ; Return the color triple.

IF Keyword_Set(name) THEN RETURN, colorInfo.name ELSE $
   RETURN, Reform([colorInfo.r, colorInfo.g, colorInfo.b], 1, 3)
END
