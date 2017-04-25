; docformat = 'rst'
;
; NAME:
;   cgColor
;
; PURPOSE:
; The purpose of this function is to obtain drawing colors
; by name and in a device/decomposition independent way.
; The color names and values may be read in as a file, or 192 color
; names and values are supplied with the program. These colors were
; obtained from the file rgb.txt, found on most X-Window distributions,
; and from colors in the Brewer color tables (http://colorbrewer2.org/).
; Representative colors were chosen from across the color spectrum. 
; If the color names '0', '1', '2', ..., '255' are used, they will
; correspond to the colors in the current color table in effect at
; the time the cgColor program is called.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to obtain drawing colors
; by name and in a device/decomposition independent way.
; The color names and values may be read in as a file, or 192 color
; names and values are supplied with the program. These colors were
; obtained from the file rgb.txt, found on most X-Window distributions,
; and from colors in the `Brewer color tables <http://colorbrewer2.org/>`.
; Representative colors were chosen from across the color spectrum. 
; If the color names '0', '1', '2', ..., '255' are used, they will
; correspond to the colors in the current color table in effect at
; the time the `cgColor` program is called.
; 
; Please note that all Coyote Graphics routines use cgColor internally to specify
; their colors in a color-model independent way. It is not necessary to call
; cgColor yourself unless you are using it with a traditional IDL command (e.g., Plot).
; For example::
;  
;     Plot, data, Color=cgColor('dodger blue')
;     
; But, it is not needed with Coyote Graphics commands::
; 
;     cgPlot, data, Color='dodger blue'
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    To get drawing colors in a device-decomposed independent way::
;
;        axisColor = cgColor("Green", !D.Table_Size-2)
;        backColor = cgColor("Charcoal", !D.Table_Size-3)
;        dataColor = cgColor("Yellow", !D.Table_Size-4)
;        Plot, Findgen(11), Color=axisColor, Background=backColor, /NoData
;        OPlot, Findgen(11), Color=dataColor
;
;    To set the viewport color in object graphics::
;
;        theView = Obj_New('IDLgrView', Color=cgColor('Charcoal', /Triple))
;
;    To change the viewport color later::
;
;        theView->SetProperty, Color=cgColor('Antique White', /Triple)
;
;    To load the drawing colors "red", "green", and "yellow" at indices 100-102, type this::
;
;        IDL> TVLCT, cgColor(["red", "green", "yellow"], /Triple), 100
;           
;    To interactively choose a color, set the SELECTCOLOR keyword::
;    
;        IDL> color = cgColor(/SelectColor)
;        
;    The cgPickColorName program is a good way to learn the names of the colors available::
;    
;        IDL> color = cgPickColorName()
;
; .. image:: cgpickcolorname.png
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
;     Change History::
;        Written by: David W. Fanning
;        Modified FSC_COLOR to create cgColor 9 February 2011. DWF.
;        Modified to allow a three-element color triple to be used in place of the color
;           name parameter. This allows one user-defined color to be used. 4 Dec 2011. DWF.
;        Modified to allow byte and 16-bit integer values to be used to specify colors
;           in the current color table. 5 Dec 2011. DWF.
;        Modified to allow the "opposite" pixel to be determined in the Z-graphics buffer. 24 Dec 2011. DWF.
;        Modified the code to handle long integers depending on the current color mode and the
;            number of values passed in. 10 January 2012. DWF.
;        Made sure the return values are BYTES not INTEGERS, in cases where this is expected. 10 Jan 2012. DWF.
;        Added "Background" as a color name. The opposite of "Opposite". 1 Feb 2012. DWF.
;        When returning a vector of color values, now making sure to return a byte array if 
;             in indexed color mode. 27 Feb 2012. DWF.
;        Added Compile Opt id2 to all file modules. 22 July 2012. DWF.
;        Added "opposite" and "background" colors to Brewer colors. 14 August 2012. DWF.
;        Some versions of IDL report the size of widget windows incorrectly, so instead of
;              sampling the very top-right pixel, I now back off a little. 1 Nov 2012. DWF.
;        For numerical values less than 256, in indexed color state, I now return the values
;              directly to the user. This should significantly speed up many Coyote Graphics
;              processes. 14 December 2012. DWF.
;        Removed cgColor_Color24 module in favor of using Coyote Library routine cgColor24. 5 Jan 2013. DWF.
;        The keyword ROW was being ignored if multiple colors were specified with TRIPLE keyword. Fixed. 10 July 2013. DWF.
;        Another fix to handle Windows 8 computers that report their window size incorrectly. 21 Oct 2013. DWF.
;        Added 12 colors suggested by Paul Krummel for people with color blindness. See the last line in 
;              Figure 3 of `this reference <http://www.sron.nl/~pault/>`. 16 Jan 2015. DWF.
;        Getting reports that Mac computers are now reporting inaccurate draw widget sizes (similar to Windows
;              computers). So, I changed the algorithm for picking the "opposite" or "background" color.
;              Previously, I read the color from the open graphics window. Now, I only do that if you ask for
;              the "OPPOSITE" or "BACKGROUND" color. Otherwise, I assume the background color is "white". If you 
;              do ask for the color, I read the open graphics window at a location 5 pixels removed from what
;              is "supposed" to be the upper-right corner of the window (often reported incorrectly). If I have
;              any problems reading this pixel, I report the background color as "white". 27 Feb 2015. DWF.
;         Ran into an ATV image that was less than 5 pixels on a side! More fooling around to avoid calling
;              cgSnapshot when it might fail. 29 March 2016. DWF.
;         Don't call cgSnapshot on X-windows when there is no backing store (Retain = 2) as this often   
;              yields spurious results.     12 Oct 2016 W. Landsman
;        
; :Copyright:
;     Copyright (c) 2009-2016, Fanning Software Consulting, Inc.
;-
;
;+
; The purpose of this function is to obtain drawing colors
; by name and in a device and color model independent way.
; 
; :Returns:
;     The return value depends on the color mode in effect at the time
;     the program is called and which keyword is used with the program.
;     In normal operation, if the graphics device is using indexed color
;     mode, the program will load a color at a unique (or specified)
;     index and return that index number. If the graphics device is using
;     decomposed color mode, the program will create a 24-bit color value
;     that can be used to specify the particular color desired. In this
;     case, no color is loaded in the color table. This is the preferred
;     mode for working with colors in IDL.
;     
; :Params:
;    theColour: required, optional, type=varies
;        Normally the name of the color desired. However, this can also be
;        a string index number (e.g., '215') or a byte or short integer
;        value (e.g, 215B or 215S). If this is the case, the color
;        in the current color table at this index number is used for the 
;        color that is returned. The value may also be a vector of color names. 
;        The color may also be a three-element byte or integer array specifying a 
;        user-defined color triple. Only one color triple is allowed.
;
;        To see a list of the color names available set the NAMES keyword. Colors available are these::
;
;           Active            Almond     Antique White        Aquamarine             Beige            Bisque
;           Black               Blue       Blue Violet             Brown         Burlywood        Cadet Blue
;           Charcoal       Chartreuse         Chocolate             Coral   Cornflower Blue          Cornsilk
;           Crimson              Cyan    Dark Goldenrod         Dark Gray        Dark Green        Dark Khaki
;           Dark Orchid      Dark Red       Dark Salmon   Dark Slate Blue         Deep Pink       Dodger Blue
;           Edge                 Face         Firebrick      Forest Green             Frame              Gold
;           Goldenrod            Gray             Green      Green Yellow         Highlight          Honeydew
;           Hot Pink       Indian Red             Ivory             Khaki          Lavender        Lawn Green
;           Light Coral    Light Cyan        Light Gray      Light Salmon   Light Sea Green      Light Yellow
;           Lime Green          Linen           Magenta            Maroon       Medium Gray     Medium Orchid
;           Moccasin             Navy             Olive        Olive Drab            Orange        Orange Red
;           Orchid     Pale Goldenrod        Pale Green            Papaya              Peru              Pink
;           Plum          Powder Blue            Purple               Red              Rose        Rosy Brown
;           Royal Blue   Saddle Brown            Salmon       Sandy Brown         Sea Green          Seashell
;           Selected           Shadow            Sienna          Sky Blue        Slate Blue        Slate Gray
;           Snow         Spring Green        Steel Blue               Tan              Teal              Text
;           Thistle            Tomato         Turquoise            Violet        Violet Red             Wheat
;           White              Yellow
;
;        Here are the Brewer color names::
;
;           WT1        WT2       WT3       WT4       WT5       WT6       WT7       WT8
;           TAN1      TAN2      TAN3      TAN4      TAN5      TAN6      TAN7      TAN8
;           BLK1      BLK2      BLK3      BLK4      BLK5      BLK6      BLK7      BLK8
;           GRN1      GRN2      GRN3      GRN4      GRN5      GRN6      GRN7      GRN8
;           BLU1      BLU2      BLU3      BLU4      BLU5      BLU6      BLU7      BLU8
;           ORG1      ORG2      ORG3      ORG4      ORG5      ORG6      ORG7      ORG8
;           RED1      RED2      RED3      RED4      RED5      RED6      RED7      RED8
;           PUR1      PUR2      PUR3      PUR4      PUR5      PUR6      PUR7      PUR8
;           PBG1      PBG2      PBG3      PBG4      PBG5      PBG6      PBG7      PBG8
;           YGB1      YGB2      YGB3      YGB4      YGB5      YGB6      YGB7      YGB8
;           RYB1      RYB2      RYB3      RYB4      RYB5      RYB6      RYB7      RYB8
;           TG1        TG2       TG3       TG4       TG5       TG6       TG7       TG8
;           
;        Here are color names for colors appropriate for color blind users::
;        
;           CG1 CG2 CG3 CG4 CG5 CG6 CG7 CG8 CG9 CG10 CG11 CG12
;            
;        The color name "OPPOSITE" is also available. It chooses a color "opposite" to the 
;        color of the pixel in the upper-right corner of the display, if a window is open.
;        Otherwise, this color is "black" in PostScript and "white" everywhere else.
;        The color OPPOSITE is used if this parameter is absent or a color name is mis-spelled.
;        
;         The color name "BACKGROUND" can similarly be used to select the color of the pixel
;         in the upper-right corner of the display, if a window is open.
;           
;    colorindex: in, optional, type=byte
;        The color table index where the color should be loaded. Colors are
;        loaded into the color table only if using indexed color mode in the
;        current graphics device. If this parameter is missing, the color will
;        be loaded at a unique color index number, if necessary.
;        
; :Keywords:
;     allcolors: in, optional, type=boolean, default=0
;        Set this keyword to return indices, or 24-bit values, or color
;        triples, for all the known colors, instead of for a single color.
;     brewer: in, optional, type=boolean, default=0
;        An obsolete keyword. If used, only Brewer colors are loaded into the color
;        vectors internally.
;     cancel: out, optional, type=boolean, default=0
;        This keyword is always set to 0, unless that SELECTCOLOR keyword is used.
;        Then it will correspond to the value of the CANCEL output keyword in cgPickColorName.
;     check_connection: in, optional, type=boolean, default=0
;         An obsolete keyword now completely ignored.
;     colorstructure: out, optional, type=structure
;        This output keyword (if set to a named variable) will return a
;        structure in which the fields will be the known color names (without spaces)
;        and the values of the fields will be either color table index numbers or
;        24-bit color values. If you have specified a vector of color names, then
;        this will be a structure containing just those color names as fields.
;     decomposed: in, optional, type=boolean
;        Set this keyword to 0 or 1 to force the return value to be
;        a color table index or a 24-bit color value, respectively. This
;        keyword is normally set by the color state of the current graphics device.
;     filename: in, optional, type=string
;        The  name of an ASCII file that can be opened to read in color values and color 
;        names. There should be one color per row in the file. Please be sure there are 
;        no blank lines in the file. The format of each row should be::
;
;           redValue  greenValue  blueValue  colorName
;
;        Color values should be between 0 and 255. Any kind of white-space
;        separation (blank characters, commas, or tabs) are allowed. The color
;        name should be a string, but it should NOT be in quotes. A typical
;        entry into the file would look like this::
;
;           255   255   0   Yellow
;     names: in, optional, type=boolian, default=0
;        If this keyword is set, the return value of the function is a string array 
;        containing the names of the colors. These names would be appropriate, for example, 
;        in building a list widget with the names of the colors. If the NAMES
;        keyword is set, the COLOR and INDEX parameters are ignored.
;     ncolors: out, optional, type=integer
;        Returns the number of colors that cgColor "knows" about. Currently ncolors=193.
;     nodisplay: in, optional, type=boolean, default=0
;        An obsolete keyword, now totally ignored.
;     row: in, optional, type=boolean
;        If this keyword is set, the return value of the function when the TRIPLE
;        keyword is set is returned as a row vector, rather than as the default
;        column vector. This is required, for example, when you are trying to
;        use the return value to set the color for object graphics objects. This
;        keyword is completely ignored, except when used in combination with the
;        TRIPLE keyword.
;     selectcolor: in, optional, type=boolean
;       Set this keyword if you would like to select the color name with
;       the cgPickColorName program. Selecting this keyword automaticallys sets
;       the INDEX positional parameter. If this keyword is used, any keywords
;       appropriate for cgPickColorName can also be used. If this keyword is used,
;       the first positional parameter can be a color name that will appear in
;       the SelectColor box.
;     triple: in, optional, type=boolean
;        Setting this keyword will force the return value of the function to
;        always be a color triple, regardless of color decomposition state or
;        visual depth of the machine. The value will be a three-element column
;        vector unless the ROW keyword is also set.
;     _ref_extra: in, optional
;        Any keyword parameter appropriate for cgPickColorName can be used.
;       These include BOTTOM, COLUMNS, GROUP_LEADER, INDEX, and TITLE.
;
;-
FUNCTION cgColor, theColour, colorIndex, $
   ALLCOLORS=allcolors, $
   BREWER=brewer, $ ; This keyword is no longer used.
   CHECK_CONNECTION=check_connection, $ ; This keyword is completely ignored.
   COLORSTRUCTURE=colorStructure, $
   CANCEL=cancelled, $
   DECOMPOSED=decomposedState, $
   FILENAME=filename, $
   NAMES=names, $
   NCOLORS=ncolors, $
   NODISPLAY=nodisplay, $ ; This keyword is completely ignored.
   ROW=row, $
   SELECTCOLOR=selectcolor, $
   TRIPLE=triple, $
  _REF_EXTRA=extra
  
    Compile_Opt idl2
   
    ; Return to caller as the default error behavior.
    On_Error, 2
        
    ; Error handling for the rest of the program.
    Catch, theError
    IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = cgErrorMsg()
       cancelled = 1
       RETURN, !P.Color
    ENDIF
    
    ; Get the current color state. This will help you determine what to 
    ; do with the input color.
    IF N_Elements(decomposedState) NE 0 THEN colorState = Keyword_Set(decomposedState) $
        ELSE colorState = cgGetColorState()
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; I don't want to change the original variable.
    IF N_Elements(theColour) NE 0 THEN theColor = theColour ELSE $
        theColor = 'OPPOSITE'
        
     ; Allow the color values to be something other than a string.
     ; There will be some ambiguity between a color triple and a number
     ; array of three elements, but I am willing to live with this.
     IF Size(theColor, /TNAME) NE 'STRING' THEN BEGIN
     
        ; Make sure this is not a 1x3 array, which we want to treat as a color triple.
        IF (N_Elements(theColor) EQ 3) && (Size(theColor, /N_DIMENSIONS) EQ 2) THEN BEGIN
            theColor = Reform(theColor)
        ENDIF
     
        ; Allow the color to be a three-element array of byte values.
        ; If it is, we will define the USERDEF color with these values.
        ; Otherwise the USERDEF color will be unused.
        IF (Size(theColor, /N_DIMENSIONS) EQ 1) && $
           (N_Elements(theColor) EQ 3) && $
           (Max(theColor) LE 255) && $
           (Min(theColor) GE 0) THEN BEGIN
           usercolor = theColor
           theColor = 'USERDEF'
        ENDIF
        
        ; If the input didn't qualify as a color triple, then see if you 
        ; can somehow make sense of the number values.
        IF Size(theColor, /TNAME) NE 'STRING' THEN BEGIN
        
          ; We can assume that any number that is a byte or short integer must
          ; be an index into the color table. Return that value directly, if
          ; you are currently in an indexed color state.
          IF (Size(theColor, /TYPE) LE 2) THEN BEGIN
             IF (colorState EQ 1) THEN theColor = StrTrim(Fix(theColor),2) ELSE RETURN, theColor
          ENDIF 
          
          ; Long integers are problematic. If the current color mode is INDEXED, then
          ; we will treat long integers as color indices. If it is DECOMPOSED, then if
          ; there is just one value, we can handle this as a color triple.
          IF (Size(theColor, /TYPE) EQ 3) THEN BEGIN
             
               IF (colorState EQ 1) THEN BEGIN
                   IF N_Elements(theColor) EQ 1 THEN BEGIN
                      usercolor = [theColor MOD 2L^8, (theColor MOD 2L^16)/2L^8, theColor/2L^16]
                      theColor = 'USERDEF'
                   ENDIF ELSE Message, 'Do not know how to handle a vector of LONG integers!
               ENDIF ELSE BEGIN
                   IF N_Elements(theColor) EQ 1 THEN BEGIN
                      IF theColor LE 255 THEN BEGIN
                          RETURN, theColor
                      ENDIF ELSE Message, 'Long integer ' + StrTrim(theColor,2) + ' is out of indexed color range.'
                   ENDIF ELSE Message, 'Do not know how to handle a vector of LONG integers!
               ENDELSE
               
          ENDIF
          
          ; Anything that is not an BYTE, INTEGER, LONG, or STRING causes problems.
          IF (Size(theColor, /TYPE) GT 4) && (Size(theColor, /TNAME) NE 'STRING') THEN BEGIN
             IF (colorstate EQ 0) AND (theColor LE 255) THEN BEGIN
                RETURN, theColor
             ENDIF ELSE BEGIN
                Message, 'Use BYTE, INTEGER, or STRING data to specify a color.'
             ENDELSE
          ENDIF
        ENDIF
     ENDIF
        
    ; Make sure the color parameter is a string.
    varName = Size(theColor, /TNAME)
    IF varName NE 'STRING' THEN $
       Message, 'The color name parameter must be a string.', /NoName
       
    ; We don't want underscores in color names. Turn all underscores
    ; to spaces.
    FOR j=0,N_Elements(theColor)-1 DO BEGIN
        theColor[j] = StrJoin( StrSplit(theColor[j], '_', /Extract, $
           /Preserve_Null), ' ')
    ENDFOR
    
    ; Make sure the color is compressed and uppercase.   
    theColor = StrUpCase(StrCompress(StrTrim(theColor,2), /Remove_All))
    
    ; If needed, get the pixel value of the "opposite" color (also called "background").
    ; This is the pixel color opposite the pixel color in the upper-right corner of the
    ; display. Unfortunately, Windows versions of IDL (though at least IDL 8.2.1),
    ; and now some Macintosh versions of IDL, report the size of draw widgets which are
    ; not yet the current graphics window inaccurately. So, I can't tell exactly which
    ; pixel to read, since it depends on the size of the window.
    ;
    ; The solution I am going to try now is to ONLY read this pixel if I need to, and if
    ; I try to read it, I am going to catch any reading errors. If I get an error,
    ; I am going to make the "background" color "white" and the "opposite" color "black".
    ; The pixel I am going to read, an an attempt to avoid problems is a pixel five pixels
    ; into what I *think* is the end of the window. If the image is smaller than 5 pixels on
    ; a side, I'm going to give up and shoot myself (after I make the background white and
    ; the opposite color black).     I will also give up if using X windows without
    ; backing store because TVRD() can then give spurious results.
    index = Where(theColor EQ 'OPPOSITE' OR theColor EQ 'BACKGROUND', bgcount)
    IF bgcount GT 0 THEN BEGIN 
        IF ((!D.Window GE 0) && ((!D.Flags AND 256) NE 0)) || (!D.Name EQ 'Z') THEN BEGIN
           Catch, theError
           IF (theError NE 0) || $
              ((!D.NAME EQ 'X') && (pref_get('IDL_GR_X_RETAIN') LT 2)) THEN BEGIN
              opixel =  [255B, 255B, 255B]
           ENDIF
           IF N_Elements(opixel) EQ 0 THEN BEGIN
               IF (!D.X_Size GT 5) AND (!D.Y_Size GT 5) THEN BEGIN
                  opixel = cgSnapshot(!D.X_Size-5, !D.Y_Size-5, 1, 1)
               ENDIF
           ENDIF
           Catch, /Cancel
           IF N_Elements(opixel) NE 3 THEN BEGIN
              IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /Get
              opixel = [rrr[opixel], ggg[opixel], bbb[opixel]]
           ENDIF
            
           bgcolor = opixel
           opixel = 255 - bgcolor
        ENDIF ELSE BEGIN
           bgcolor = [255B,255B,255B] ; White
           opixel = [0B, 0B, 0B]      ; Black
        ENDELSE
    ENDIF ELSE BEGIN
       bgcolor = [255B,255B,255B] ; White
       opixel = [0B, 0B, 0B]      ; Black
    ENDELSE
    
    
    
    ; Read the first color as bytes. If none of the bytes are less than 48
    ; or greater than 57, then this is a "number" string and you should
    ; assume the current color table is being used.
    bytecheck = Byte(theColor[0])
    i = Where(bytecheck LT 48, lessthan)
    i = Where(bytecheck GT 57, greaterthan)
    IF (lessthan + greaterthan) EQ 0 THEN useCurrentColors = 1 ELSE useCurrentColors = 0
    
    ; Get the decomposed state of the IDL session right now.
    IF N_Elements(decomposedState) EQ 0 THEN BEGIN
       IF Float(!Version.Release) GE 5.2 THEN BEGIN
          IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
             Device, Get_Decomposed=decomposedState
          ENDIF ELSE decomposedState = 0
       ENDIF ELSE decomposedState = 0
       IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
          Device, Get_Decomposed=decomposedState, Get_Pixel_Depth=theDepth
          IF theDepth LT 24 THEN decomposedState = 0
       ENDIF
    ENDIF ELSE decomposedState = Keyword_Set(decomposedState)
    
    ; Get depth of visual display (and decomposed state for PostScript devices).
    IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
    IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN Device, Get_Pixel_Depth=theDepth
    IF (!D.NAME EQ 'PS') AND (Float(!Version.Release) GE 7.1) THEN BEGIN
       decomposedState = cgGetColorState(DEPTH=theDepth)
    ENDIF

    ; Need brewer colors?
    brewer = Keyword_Set(brewer)
    
    ; Load the colors.
    IF N_Elements(filename) NE 0 THEN BEGIN
    
       ; Count the number of rows in the file.
       ncolors = File_Lines(filename)
    
       ; Read the data.
       OpenR, lun, filename, /Get_Lun
       rvalue = BytArr(NCOLORS)
       gvalue = BytArr(NCOLORS)
       bvalue = BytArr(NCOLORS)
       colors = StrArr(NCOLORS)
       redvalue = 0B
       greenvalue = 0B
       bluevalue = 0B
       colorvalue = ""
       FOR j=0L, NCOLORS-1 DO BEGIN
          ReadF, lun, redvalue, greenvalue, bluevalue, colorvalue
          rvalue[j] = redvalue
          gvalue[j] = greenvalue
          bvalue[j] = bluevalue
          colors[j] = colorvalue
       ENDFOR
       Free_Lun, lun
    
       ; Trim the colors array of blank characters.
       colors = StrTrim(colors, 2)
    
    ENDIF ELSE BEGIN
    
       ; Set up the color vectors.
       IF Keyword_Set(Brewer) THEN BEGIN
       
           ; Set up the color vectors.
           colors = [ 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
           rvalue = [  255,   255,   255,   255,   255,   245,   255,   250 ]
           gvalue = [  255,   250,   255,   255,   248,   245,   245,   240 ]
           bvalue = [  255,   250,   240,   224,   220,   220,   238,   230 ]
           colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
           rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
           gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
           bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
           colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
           rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
           rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
           gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
           bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
           colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
           rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
           gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
           bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
           colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
           rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
           gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
           bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
           colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
           rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
           gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
           bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
           colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
           rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
           gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
           bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
           colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
           rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
           gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
           bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
           colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
           rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
           gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
           bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
           colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
           rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
           gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
           bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
           colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
           rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0 ]
           gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81 ]
           bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71 ]
           colors = [ colors, 'OPPOSITE', 'BACKGROUND']
           rvalue = [ rvalue,  opixel[0],  bgcolor[0]]
           gvalue = [ gvalue,  opixel[1],  bgcolor[1]]
           bvalue = [ bvalue,  opixel[2],  bgcolor[2]]
       
       ENDIF ELSE BEGIN
       
           ; Set up the color vectors. Both original and Brewer colors.
           colors= ['White']
           rvalue = [ 255]
           gvalue = [ 255]
           bvalue = [ 255]
           colors = [ colors,   'Snow',     'Ivory','Light Yellow', 'Cornsilk',     'Beige',  'Seashell' ]
           rvalue = [ rvalue,     255,         255,       255,          255,          245,        255 ]
           gvalue = [ gvalue,     250,         255,       255,          248,          245,        245 ]
           bvalue = [ bvalue,     250,         240,       224,          220,          220,        238 ]
           colors = [ colors,   'Linen','Antique White','Papaya',     'Almond',     'Bisque',  'Moccasin' ]
           rvalue = [ rvalue,     250,        250,        255,          255,          255,          255 ]
           gvalue = [ gvalue,     240,        235,        239,          235,          228,          228 ]
           bvalue = [ bvalue,     230,        215,        213,          205,          196,          181 ]
           colors = [ colors,   'Wheat',  'Burlywood',    'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
           rvalue = [ rvalue,     245,        222,          210,      230,          230,         210 ]
           gvalue = [ gvalue,     222,        184,          180,      230,          230,         210 ]
           bvalue = [ bvalue,     179,        135,          140,      230,          250,         210 ]
           colors = [ colors,  'Gray', 'Slate Gray',  'Dark Gray',  'Charcoal',   'Black',  'Honeydew', 'Light Cyan' ]
           rvalue = [ rvalue,      190,      112,          110,          70,         0,         240,          224 ]
           gvalue = [ gvalue,      190,      128,          110,          70,         0,         255,          255 ]
           bvalue = [ bvalue,      190,      144,          110,          70,         0,         255,          240 ]
           colors = [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
           rvalue = [ rvalue,     176,          135,          100,              95,            70,           30,           65,            0 ]
           gvalue = [ gvalue,     224,          206,          149,             158,           130,          144,          105,            0 ]
           bvalue = [ bvalue,     230,          235,          237,             160,           180,          255,          225,          255 ]
           colors = [ colors,  'Navy', 'Pale Green','Aquamarine','Spring Green',  'Cyan' ]
           rvalue = [ rvalue,        0,     152,          127,          0,            0 ]
           gvalue = [ gvalue,        0,     251,          255,        250,          255 ]
           bvalue = [ bvalue,      128,     152,          212,        154,          255 ]
           colors = [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
           rvalue = [ rvalue,      64,          143,               46,          34,             0,      173,           127,         124 ]
           gvalue = [ gvalue,     224,          188,              139,         139,           128,      255,           255,         252 ]
           bvalue = [ bvalue,     208,          143,               87,          34,           128,       47,             0,           0 ]
           colors = [ colors, 'Green', 'Lime Green', 'Olive Drab',  'Olive','Dark Green','Pale Goldenrod']
           rvalue = [ rvalue,      0,        50,          107,        85,            0,          238 ]
           gvalue = [ gvalue,    255,       205,          142,       107,          100,          232 ]
           bvalue = [ bvalue,      0,        50,           35,        47,            0,          170 ]
           colors = [ colors,     'Khaki', 'Dark Khaki', 'Yellow',  'Gold', 'Goldenrod','Dark Goldenrod']
           rvalue = [ rvalue,        240,       189,        255,      255,      218,          184 ]
           gvalue = [ gvalue,        230,       183,        255,      215,      165,          134 ]
           bvalue = [ bvalue,        140,       107,          0,        0,       32,           11 ]
           colors = [ colors,'Saddle Brown',  'Rose',   'Pink', 'Rosy Brown','Sandy Brown', 'Peru']
           rvalue = [ rvalue,     139,          255,      255,        188,        244,        205 ]
           gvalue = [ gvalue,      69,          228,      192,        143,        164,        133 ]
           bvalue = [ bvalue,      19,          225,      203,        143,         96,         63 ]
           colors = [ colors,'Indian Red',  'Chocolate',  'Sienna','Dark Salmon',   'Salmon','Light Salmon' ]
           rvalue = [ rvalue,    205,          210,          160,        233,          250,       255 ]
           gvalue = [ gvalue,     92,          105,           82,        150,          128,       160 ]
           bvalue = [ bvalue,     92,           30,           45,        122,          114,       122 ]
           colors = [ colors,  'Orange',      'Coral', 'Light Coral',  'Firebrick', 'Dark Red', 'Brown',  'Hot Pink' ]
           rvalue = [ rvalue,       255,         255,        240,          178,        139,       165,        255 ]
           gvalue = [ gvalue,       165,         127,        128,           34,          0,        42,        105 ]
           bvalue = [ bvalue,         0,          80,        128,           34,          0,        42,        180 ]
           colors = [ colors, 'Deep Pink',    'Magenta',   'Tomato', 'Orange Red',   'Red', 'Crimson', 'Violet Red' ]
           rvalue = [ rvalue,      255,          255,        255,        255,          255,      220,        208 ]
           gvalue = [ gvalue,       20,            0,         99,         69,            0,       20,         32 ]
           bvalue = [ bvalue,      147,          255,         71,          0,            0,       60,        144 ]
           colors = [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
           rvalue = [ rvalue,       176,          216,          221,          238,         218,        186 ]
           gvalue = [ gvalue,        48,          191,          160,          130,         112,         85 ]
           bvalue = [ bvalue,        96,          216,          221,          238,         214,        211 ]
           colors = [ colors,'Dark Orchid','Blue Violet',  'Purple']
           rvalue = [ rvalue,      153,          138,       160]
           gvalue = [ gvalue,       50,           43,        32]
           bvalue = [ bvalue,      204,          226,       240]
           colors = [ colors, 'Slate Blue',  'Dark Slate Blue']
           rvalue = [ rvalue,      106,            72]
           gvalue = [ gvalue,       90,            61]
           bvalue = [ bvalue,      205,           139]
           colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
           rvalue = [ rvalue,  255,   255,   255,   255,   255,   245,   255,   250 ]
           gvalue = [ gvalue,  255,   250,   255,   255,   248,   245,   245,   240 ]
           bvalue = [ bvalue,  255,   250,   240,   224,   220,   220,   238,   230 ]
           colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
           rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
           gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
           bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
           colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
           rvalue = [ rvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
           rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
           gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
           bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
           colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
           rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
           gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
           bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
           colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
           rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
           gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
           bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
           colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
           rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
           gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
           bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
           colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
           rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
           gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
           bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
           colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
           rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
           gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
           bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
           colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
           rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
           gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
           bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
           colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
           rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
           gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
           bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
           colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
           rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0 ]
           gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81 ]
           bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71 ]
           colors = [ colors, 'CG1', 'CG2', 'CG3', 'CG4', 'CG5', 'CG6', 'CG7', 'CG8']
           rvalue = [ rvalue,  51,    102,   136,    68,    17,   153,   221,    102 ]
           gvalue = [ gvalue,  34,    153,   204,   170,   119,   153,   204,     17]
           bvalue = [ bvalue, 136,    204,   238,   153,    51,    51,   119,      0 ]
           colors = [ colors, 'CG9', 'CG10', 'CG11', 'CG12']
           rvalue = [ rvalue,  204,    170,   136,   170 ]
           gvalue = [ gvalue,  102,     68,    34,   68 ]
           bvalue = [ bvalue,  119,    102,    85,   153 ]           
           colors = [ colors, 'OPPOSITE', 'BACKGROUND']
           rvalue = [ rvalue,  opixel[0],  bgcolor[0]]
           gvalue = [ gvalue,  opixel[1],  bgcolor[1]]
           bvalue = [ bvalue,  opixel[2],  bgcolor[2]]
         ENDELSE
   ENDELSE
   
    ; If you have a USERDEF color (from a color triple) then load it here.
    IF N_Elements(usercolor) NE 0 THEN BEGIN
       colors = [colors, 'USERDEF']
       rvalue = [rvalue, usercolor[0]]
       gvalue = [gvalue, usercolor[1]]
       bvalue = [bvalue, usercolor[2]]
    ENDIF
       
    ; Load the colors from the current color table, if you need them.
    IF useCurrentColors THEN BEGIN
        IF (!D.Name NE 'NULL') THEN TVLCT, rrr, ggg, bbb, /GET
        IF decomposedState EQ 0 THEN BEGIN
            colors = SIndgen(256)
            rvalue = rrr
            gvalue = ggg
            bvalue = bbb           
        ENDIF ELSE BEGIN
            colors = [colors, SIndgen(256)]
            rvalue = [rvalue, rrr]
            gvalue = [gvalue, ggg]
            bvalue = [bvalue, bbb]
        ENDELSE
    ENDIF
    
    ; Make sure we are looking at compressed, uppercase names.
    colors = StrUpCase(StrCompress(StrTrim(colors,2), /Remove_All))

    ; Check synonyms of color names.
    FOR j=0, N_Elements(theColor)-1 DO BEGIN
       IF StrUpCase(theColor[j]) EQ 'GREY' THEN theColor[j] = 'GRAY'
       IF StrUpCase(theColor[j]) EQ 'LIGHTGREY' THEN theColor[j] = 'LIGHTGRAY'
       IF StrUpCase(theColor[j]) EQ 'MEDIUMGREY' THEN theColor[j] = 'MEDIUMGRAY'
       IF StrUpCase(theColor[j]) EQ 'SLATEGREY' THEN theColor[j] = 'SLATEGRAY'
       IF StrUpCase(theColor[j]) EQ 'DARKGREY' THEN theColor[j] = 'DARKGRAY'
       IF StrUpCase(theColor[j]) EQ 'AQUA' THEN theColor[j] = 'AQUAMARINE'
       IF StrUpCase(theColor[j]) EQ 'SKY' THEN theColor[j] = 'SKYBLUE'
       IF StrUpCase(theColor[j]) EQ 'NAVYBLUE' THEN theColor[j] = 'NAVY'
       IF StrUpCase(theColor[j]) EQ 'CORNFLOWER' THEN theColor[j] = 'CORNFLOWERBLUE'
       IF StrUpCase(theColor[j]) EQ 'BROWN' THEN theColor[j] = 'SIENNA'
    ENDFOR
    
    ; How many colors do we have?
    ncolors = N_Elements(colors)
    
    ; Check for offset.
    IF (theDepth EQ 8) OR (decomposedState EQ 0) THEN offset = !D.Table_Size - ncolors - 2 ELSE offset = 0
    IF (useCurrentColors) AND (decomposedState EQ 0) THEN offset = 0
        
    ; Did the user want to select a color name? If so, we set
    ; the color name and color index, unless the user provided
    ; them. In the case of a single positional parameter, we treat
    ; this as the color index number as long as it is not a string.
    cancelled = 0.0
    IF Keyword_Set(selectcolor) THEN BEGIN
    
       CASE N_Params() OF
          0: BEGIN
             theColor = cgPickColorName(Filename=filename, _Strict_Extra=extra, Cancel=cancelled, BREWER=brewer)
             IF cancelled THEN RETURN, !P.Color
             IF theDepth GT 8 AND (decomposedState EQ 1) THEN BEGIN
                   colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
             ENDIF ELSE BEGIN
                   colorIndex = Where(StrUpCase(colors) EQ StrUpCase(StrCompress(theColor, /Remove_All)), count) + offset
                   colorIndex = Fix(colorIndex[0])
                   IF count EQ 0 THEN Message, 'Cannot find color: ' + StrUpCase(theColor), /NoName
             ENDELSE
    
             END
          1: BEGIN
             IF Size(theColor, /TName) NE 'STRING' THEN BEGIN
                colorIndex = Fix(theColor)
                theColor = brewer ? 'WT1' : 'White'
             ENDIF ELSE colorIndex = Fix(!P.Color < 255)
             theColor = cgPickColorName(theColor, Filename=filename, _Strict_Extra=extra, Cancel=cancelled, BREWER=brewer)
             IF cancelled THEN RETURN, !P.Color
             END
          2: BEGIN
             theColor = cgPickColorName(theColor, Filename=filename, _Strict_Extra=extra, Cancel=cancelled, BREWER=brewer)
             IF cancelled THEN RETURN, !P.Color
             END
       ENDCASE
    ENDIF
    
    ; Make sure you have a color name and color index.
    CASE N_Elements(theColor) OF
       0: BEGIN
             theColor = brewer ? 'WT1' : 'White'
             IF N_Elements(colorIndex) EQ 0 THEN BEGIN
                IF theDepth GT 8 THEN BEGIN
                   colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
                ENDIF ELSE BEGIN
                   colorIndex = Where(colors EQ theColor, count) + offset
                   colorIndex = Fix(colorIndex[0])
                   IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
                ENDELSE
             ENDIF ELSE colorIndex = 0S > colorIndex < Fix((!D.Table_Size - 1))
          ENDCASE
    
       1: BEGIN
             type = Size(theColor, /TNAME)
             IF type NE 'STRING' THEN Message, 'The color must be expressed as a color name.'
             theColor = theColor[0] ; Make it a scalar or you run into a WHERE function "feature". :-(
             IF N_Elements(colorIndex) EQ 0 THEN BEGIN
                IF (theDepth GT 8) AND (decomposedState EQ 1) THEN BEGIN
                   colorIndex = Fix(!P.Color < (!D.Table_Size - 1))
                ENDIF ELSE BEGIN
                   colorIndex = Where(colors EQ theColor, count) + offset
                   colorIndex = Fix(colorIndex[0])
                   IF count EQ 0 THEN Message, 'Cannot find color: ' + theColor, /NoName
                ENDELSE
             ENDIF ELSE colorIndex = 0S > colorIndex < Fix(!D.Table_Size - 1)
             ENDCASE
    
       ELSE: BEGIN
             type = Size(theColor, /TNAME)
             IF type NE 'STRING' THEN Message, 'The colors must be expressed as color names.'
             ncolors = N_Elements(theColor)
             CASE N_Elements(colorIndex) OF
                0: colorIndex = Fix(Indgen(ncolors) + (!D.Table_Size - (ncolors + 1)))
                1: colorIndex = Fix(Indgen(ncolors) + colorIndex)
                ELSE: IF N_Elements(colorIndex) NE ncolors THEN $
                   Message, 'Index vector must be the same length as color name vector.'
             ENDCASE
    
                ; Did the user want color triples?
    
             IF Keyword_Set(triple) THEN BEGIN
                colors = BytArr(ncolors, 3)
                FOR j=0,ncolors-1 DO colors[j,*] = cgColor(theColor[j], colorIndex[j], Filename=filename, $
                   Decomposed=decomposedState, /Triple, BREWER=brewer)
                IF Keyword_Set(row) THEN RETURN, Transpose(Byte(colors)) ELSE RETURN, Byte(colors)
             ENDIF ELSE BEGIN
                colors = LonArr(ncolors)
                FOR j=0,ncolors-1 DO colors[j] = cgColor(theColor[j], colorIndex[j], Filename=filename, $
                   Decomposed=decomposedState, BREWER=brewer)
                IF decomposedState THEN RETURN, colors ELSE RETURN, Byte(colors)
            ENDELSE
          END
    ENDCASE
    
    ; Did the user ask for the color names? If so, return them now.
    IF Keyword_Set(names) THEN RETURN, Reform(colors, 1, ncolors)
    
    ; Process the color names.
    theNames = StrUpCase( StrCompress(colors, /Remove_All ) )
    
    ; Find the asked-for color in the color names array.
    theIndex = Where(theNames EQ StrUpCase(StrCompress(theColor, /Remove_All)), foundIt)
    theIndex = theIndex[0]
    
    ; If the color can't be found, report it and continue with the color set to "OPPOSITE."
    IF foundIt EQ 0 THEN BEGIN
       Message, "Can't find color " + theColor + ". Substituting 'OPPOSITE'.", /Informational
       theColor = 'OPPOSITE'
       theIndex = Where(StrUpCase(colors) EQ 'OPPOSITE')
    ENDIF
    
    ; Get the color triple for this color.
    r = rvalue[theIndex]
    g = gvalue[theIndex]
    b = bvalue[theIndex]
    
    ; Did the user want a color triple? If so, return it now.
    IF Keyword_Set(triple) THEN BEGIN
       IF Keyword_Set(allcolors) THEN BEGIN
          IF Keyword_Set(row) $
             THEN RETURN, Byte(Transpose([[rvalue], [gvalue], [bvalue]])) $
             ELSE RETURN, Byte([[rvalue], [gvalue], [bvalue]])
       ENDIF ELSE BEGIN
          IF Keyword_Set(row) THEN RETURN, Byte([r, g, b]) ELSE RETURN, Byte([[r], [g], [b]])
       ENDELSE
    ENDIF
    
    ; Otherwise, we are going to return either an index
    ; number where the color has been loaded, or a 24-bit
    ; value that can be decomposed into the proper color.
    IF decomposedState THEN BEGIN
    
       ; Need a color structure?
       IF Arg_Present(colorStructure) THEN BEGIN
          theColors = cgColor24([[rvalue], [gvalue], [bvalue]])
          colorStructure = Create_Struct(theNames[0], theColors[0])
          FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
       ENDIF
    
       IF Keyword_Set(allcolors) THEN BEGIN
          RETURN, cgColor24([[rvalue], [gvalue], [bvalue]])
       ENDIF ELSE BEGIN
          RETURN, cgColor24([r, g, b])
       ENDELSE
    
    ENDIF ELSE BEGIN
    
       IF Keyword_Set(allcolors) THEN BEGIN
    
          ; Need a color structure?
          IF Arg_Present(colorStructure) THEN BEGIN
             allcolorIndex = !D.Table_Size - ncolors - 2
             IF allcolorIndex LT 0 THEN $
                Message, 'Number of colors exceeds available color table values. Returning.', /NoName
             IF (allcolorIndex + ncolors) GT 255 THEN $
                Message, 'Number of colors exceeds available color table indices. Returning.', /NoName
             theColors = IndGen(ncolors) + allcolorIndex
             colorStructure = Create_Struct(theNames[0],  theColors[0])
             FOR j=1, ncolors-1 DO colorStructure = Create_Struct(colorStructure, theNames[j], theColors[j])
          ENDIF
    
          IF N_Elements(colorIndex) EQ 0 THEN colorIndex = Fix(!D.Table_Size - ncolors - 2)
          IF colorIndex LT 0 THEN $
             Message, 'Number of colors exceeds available color table values. Returning.', /NoName
          IF (colorIndex + ncolors) GT 255 THEN BEGIN
             colorIndex = Fix(!D.Table_Size - ncolors - 2)
          ENDIF
          IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN TVLCT, rvalue, gvalue, bvalue, colorIndex
          RETURN, BIndGen(ncolors) + colorIndex
       ENDIF ELSE BEGIN
    
          ; Need a color structure?
          IF Arg_Present(colorStructure) THEN BEGIN
             colorStructure = Create_Struct(theColor,  colorIndex)
          ENDIF
    
          IF (!D.Name NE 'PRINTER') AND (!D.Name NE 'NULL') THEN $
              TVLCT, rvalue[theIndex], gvalue[theIndex], bvalue[theIndex], colorIndex
          RETURN, Byte(colorIndex)
       ENDELSE
    
    
    ENDELSE

END ;-------------------------------------------------------------------------------------------------------
