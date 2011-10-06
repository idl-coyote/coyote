; docformat = 'rst'
;
; NAME:
;   WindowImage
;
; PURPOSE:
;   Allows the user to interactively adjust image contrast by means of "windowing and
;   leveling" the image. Move the cursor vertically in the window to adjust the image
;   stretch "window". Move the cursor horizontally in the window to adjust the image
;   "level".
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
PRO WindowImage_OriginalSettings, event
    
    ; This event handler resotres the original window settings.
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN $
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
        RETURN
    ENDIF
    
    ; Get the program information.
    Widget_Control, event.top, GET_UVALUE=info, /NO_COPY
    
    ; Restore original contrast and brightness values.
    contrast = 50
    brightness = 50
    level = (1-brightness/100.)*(info.maxImage - info.minImage) + info.minImage
    width = (1-contrast/100.)*(info.maxImage - info.minImage)
            
    ; Calculate the window size.
    displayMax = (level + (width / 2))
    displayMin = (level - (width / 2))
    IF displayMax GT info.maxImage THEN BEGIN
         difference = Abs(displayMax - info.maxImage)
         displayMax = displayMax - difference
         displayMin = displayMin - difference
    ENDIF
    IF displayMin LT info.minImage THEN BEGIN
         difference = Abs(info.minImage - displayMin)
         displayMin = displayMin + difference
         displayMax = displayMax + difference
    ENDIF
            
    ; Store everything.
    info.iWindow = [displayMin, displayMax]
    info.iLevel = level
    info.contrast = contrast
    info.brightness = brightness
    info.x = -1
    info.y = -1
            
    ; Display it in the window.
    WindowImage_Display, info
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;---------------------------------------------------------------------------



PRO WindowImage_Quit, event
    Widget_Control, event.top, /Destroy
END ;---------------------------------------------------------------------------------------


PRO WindowImage_Resize, event

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN $
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
        RETURN
    ENDIF
    
    ; Get the program information.
    Widget_Control, event.top, GET_UVALUE=info, /NO_COPY
    
    ; Calculate a new draw widget size.
    newx =info.labelxsize > event.x 
    newy = event.y - info.labelysize - 86
    
    ; Calculate a window size for the image. Maximum of 800 pixels. Minimum of 400.
    maximageSize = Max([newx, newy])
    wsize = MaxWindowSize()
    IF info.imageAspect GE 1 THEN BEGIN
         maxSize = wsize[1] < (maxImageSize > 200)
         ywinsize = maxsize
         xwinsize = (maxsize / info.imageAspect) < wsize[0]
    ENDIF ELSE BEGIN
         maxSize = wsize[0] < (maxImageSize > 200)
         xwinsize = maxsize
         ywinsize = (maxsize * info.imageAspect) < (wsize[1] - info.labelysize - 86)
    ENDELSE
    
    ; Resize the draw widgets.
    Widget_Control, info.imgDrawID, DRAW_XSIZE=xwinSize, DRAW_YSIZE=ywinsize
    Widget_Control, info.cbDrawID, DRAW_XSIZE=xwinSize
    
    ; Redisplay the image.
    WindowImage_Display, info
    
    ; Put the program information back in storage.
    Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
    
END ;---------------------------------------------------------------------------------------


PRO WindowImage_DrawEvents, event

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(info) NE 0 THEN $
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
        RETURN
    ENDIF
    
    ; What kind of event is this?
    possibleEvents = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSE']
    thisEvent = possibleEvents[event.type]
    
    CASE thisEvent OF
        
        'DOWN': BEGIN
            
            ; Get the program information.
            Widget_Control, event.top, GET_UVALUE=info, /NO_COPY
            
            ; Set the initial point.
            info.x = event.x
            info.y = event.y
            
            ; Clear events for this widget.
            Widget_Control, event.id, /CLEAR_EVENTS
            
            ; Turn motion events on.
            Widget_Control, event.ID, DRAW_MOTION_EVENTS=1
            
            ; Put the program information back in storage and return.
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
            RETURN
            
            END
            
        'UP': BEGIN
        
            ; Get the program information.
            Widget_Control, event.top, GET_UVALUE=info, /NO_COPY

            ; Turn motion events off and clear any queued up events.
            Widget_Control, event.ID, DRAW_MOTION_EVENTS=0
            Widget_Control, event.id, /CLEAR_EVENTS
            
            ; Calculate new contrast/brightness values.
            contrast = 0 > ((info.y - event.y) * info.cstep + info.contrast) < 99
            brightness = 0 > ((info.x - event.x) * info.bstep + info.brightness) < 100
            level = (1-brightness/100.)*(info.maxImage - info.minImage) + info.minImage
            width = (1-contrast/100.)*(info.maxImage - info.minImage)
            
            ; Calculate new display min/max.
            displayMax = (level + (width / 2))
            displayMin = (level - (width / 2))
            IF displayMax GT info.maxImage THEN BEGIN
               difference = Abs(displayMax - info.maxImage)
               displayMax = displayMax - difference
               displayMin = displayMin - difference
            ENDIF
            IF displayMin LT info.minImage THEN BEGIN
               difference = Abs(info.minImage - displayMin)
               displayMin = displayMin + difference
               displayMax = displayMax + difference
            ENDIF
            
            ; Store everything.
            info.iWindow = [displayMin, displayMax]
            info.iLevel = level
            info.contrast = contrast
            info.brightness = brightness
            info.x = event.x
            info.y = event.y
            
            ; Display the undated image.
            WindowImage_Display, info
            
            ; Put the program information back in storage and return.
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
            
            END
            
        'MOTION': BEGIN

            ; Get the program information.
            Widget_Control, event.top, GET_UVALUE=info, /NO_COPY
             
            ; Calculate new contrast/brightness values.
            contrast = 0 > ((info.y - event.y) * info.cstep + info.contrast) < 99
            brightness = 0 > ((info.x - event.x) * info.bstep + info.brightness) < 100
            level = (1-brightness/100.)*(info.maxImage - info.minImage) + info.minImage
            width = (1-contrast/100.)*(info.maxImage - info.minImage)
            
            ; Calculate new display min/max.
            displayMax = (level + (width / 2))
            displayMin = (level - (width / 2))
            IF displayMax GT info.maxImage THEN BEGIN
               difference = Abs(displayMax - info.maxImage)
               displayMax = displayMax - difference
               displayMin = displayMin - difference
            ENDIF
            IF displayMin LT info.minImage THEN BEGIN
               difference = Abs(info.minImage - displayMin)
               displayMin = displayMin + difference
               displayMax = displayMax + difference
            ENDIF
 
            ; Store the information.           
            info.iWindow = [displayMin, displayMax]
            info.iLevel = level
            
            ; Update the image.            
            WindowImage_Display, info
            
            ; Put the program information back in storage and return.
            Widget_Control, event.top, SET_UVALUE=info, /NO_COPY
            
            END
    
    ENDCASE
    
    
END ;---------------------------------------------------------------------------------------


PRO WindowImage_Display, info

    SetDecomposedState, 1, CURRENTSTATE=currentState

    ; Load the color table.
    cgLoadCT, info.colortable, REVERSE=info.reverse, BREWER=info.brewer, NCOLORS=253
    TVLCT, cgColor(info.neutralColor, /TRIPLE), 254
    
    ; Draw the image.
    WSet, info.imgWinID
    cgImage, info.image, SCALE=info.scale, NCOLORS=253, /Keep, $
        MINVALUE=info.iwindow[0], MAXVALUE=info.iwindow[1], NOERASE=1

    ; Draw the color bar.
    WSet, info.cbWinID
    cgErase
    cgColorbar, NCOLORS=253, CLAMP=info.iwindow, NEUTRALINDEX=254, $
        POSITION=[0.05, 0.35, 0.95, 0.65], FONT=0, ANNOTATECOLOR='black', $
        RANGE=[info.minImage, info.maxImage], DIVISIONS=5, FORMAT='(F0.2)'
    
    ; Write a label on the color bar.
    format = '(F0.3)'
    txt = 'Window: [' + String(info.iwindow[0], FORMAT=format) + ', ' + $
                        String(info.iwindow[1], FORMAT=format) + ']  Level: ' + $
                        String(info.ilevel, FORMAT=format)
    cgText, 0.5, 0.75, /Normal, Alignment=0.5, Font=0, txt, Color='black'
  
    SetDecomposedState, currentState

END ;---------------------------------------------------------------------------------------


;+
; :Description:
;   Allows the user to interactively adjust image contrast by means of "windowing and
;   leveling" the image. Move the cursor vertically in the window to adjust the image
;   stretch "window". Move the cursor horizontally in the window to adjust the image
;   "level".
;
; :Categories:
;    Graphics
;    
; :Params:
;    image: in, required, type=any
;         Any 2D array that you wish to adjust the contrast of.
;       
; :Keywords:
;     brewer: in, optional, type=boolean, default=0
;         Set this keyword to indicate a Brewer color table is desired.
;     colortable: in, optional, type=integer, default=0
;          The index number of a color table to load with cgLoadCT.
;     neutralcolor: in, optional, type=string
;         The name of the color to use for values outside the image "window" in
;         the color table. If a default grayscale color table is loaded, the default
;         color is "rose", otherwise the default is "black".
;     reverse: in, optional, type=boolean, default=0
;         Set this keyword if you wish to reverse the color table.
;         
; :Examples:
;    To see a demonstation::
;       IDL> WindowImage
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 29 November 2010. DWF. 
;        Added color protection to the program. 30 Nov 2010. DWF.
;        Modification of cgImage command to prevent flashing. 27 Feb 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO WindowImage, image, $
    BREWER=brewer, $
    COLORTABLE=colortable, $
    NEUTRALCOLOR=neutralColor, $
    REVERSE=reverse
    
    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; Check parameters.
    IF N_Elements(image) EQ 0 THEN BEGIN
         filename = Filepath(Subdir=['examples','data'], 'muscle.jpg')
         Read_JPEG, filename, image
    ENDIF
    minImage = Min(image, MAX=maxImage, /NAN)
    IF (minImage LT 0) OR (maxImage GT 253) THEN scale = 1 ELSE scale = 0
    brewer = Keyword_Set(brewer)
    reverse = Keyword_Set(brewer)
    IF N_Elements(colortable) EQ 0 THEN BEGIN
        IF N_Elements(neutralColor) EQ 0 THEN neutralColor = 'rose'
        IF brewer THEN BEGIN
            cgLoadCT, 17, REVERSE=reverse, BREWER=1, NCOLORS=253
            colortable = 17
        ENDIF ELSE BEGIN
            cgLoadCT,  0, REVERSE=reverse, BREWER=0, NCOLORS=253
            colortable = 0
        ENDELSE
    ENDIF ELSE BEGIN
        IF N_Elements(neutralColor) EQ 0 THEN neutralColor = 'gray'
        cgLoadCT, colortable, REVERSE=reverse, BREWER=brewer, NCOLORS=253
    ENDELSE
    TVLCT, cgColor(neutralColor, /TRIPLE), 254
        
    IF N_Elements(ilevel) EQ 0 THEN ilevel = (maxImage - minImage) / 2.0
    IF N_Elements(iwindow) EQ 0 THEN BEGIN
       w20 = (maxImage - minImage) * 0.20
       iwindow = minImage > [ilevel - w20, ilevel + w20] < maxImage
    ENDIF
    IF (iLevel LT iwindow[0]) OR (iLevel GT iwindow[1]) THEN BEGIN
        ilevel = (iwindow[1] - iwindow[0]) / 2.0
    ENDIF
    
    ; Check image, get dimensions.
    IF Size(image, N_DIMENSIONS=1) NE 2 THEN Message, 'The image argument must be 2D.'
    s = Size(image, /DIMENSIONS)
    xsize = s[0]
    ysize = s[1]
    imageAspect = Float(s[1])/s[0]
    
    ; Calculate a window size for the image. Maximum of 800 pixels. Minimum of 400.
    maximageSize = Max([xsize, ysize])
    maxSize = 800 < (maxImageSize > 400)
    IF imageAspect GE 1 THEN BEGIN
         ywinsize = maxsize
         xwinsize = maxsize / imageAspect
    ENDIF ELSE BEGIN
         xwinsize = maxsize
         ywinsize = maxsize * imageAspect
    ENDELSE
    
    ; Create widgets.
    tlb = Widget_Base(Title='Image Window/Leveling', Column=1, MBar=menuID, $
        Base_Align_Center=1, TLB_Size_Events=1, YPAD=0)
    fileID = Widget_Button(menuID, Value='File')
    button = Widget_Button(fileID, Value='Restore Original Settings', $
        Event_Pro='WindowImage_OriginalSettings')
    quitID = Widget_Button(fileID, Value='Quit', Event_Pro="WindowImage_Quit", /Separator)
    cbDrawID = Widget_Draw(tlb, XSize=xwinsize, YSize=80)
    imgDrawID = Widget_Draw(tlb, XSize=xwinsize, YSize=ywinsize, Button_Events=1, $
       Event_Pro='WindowImage_DrawEvents')
    labelID = Widget_Label(tlb, Value='Drag cursor horizontally for BRIGHTNESS, ' + $
        'vertically for CONTRAST.', /Dynamic_Resize)
    Widget_Control, tlb, /REALIZE
    
    ; Get window index numbers.
    Widget_Control, cbDrawID, Get_Value=cbWinID
    Widget_Control, imgDrawID, Get_Value=imgWinID
    
    ; Find the Y screen sizes of the label widget.
    g = Widget_Info(labelID, /Geometry)
    labelysize = g.scr_ysize
    labelxsize = g.scr_xsize
    
    ; Create an info structure to hold program information.
    info = { image: image, $
             xsize: xsize, $
             ysize: ysize, $
             labelxsize: labelxsize, $
             labelysize: labelysize, $
             iWindow: iWindow, $
             iLevel: iLevel, $
             cbWinID: cbWinID, $
             imgWinID: imgWinID, $
             scale: scale, $
             colortable: colortable, $
             neutralcolor: neutralcolor, $
             brewer: brewer, $
             reverse: reverse, $
             x: -1, $
             y: -1, $
             imageAspect: imageAspect, $
             imgDrawID: imgDrawID, $
             cbDrawID: cbDrawID, $
             contrast: 50, $
             brightness: 50, $
             cstep: ysize / 512., $
             bstep: xsize / 2048., $
             minImage: minImage, $
             maxImage: maxImage }
             
    ; Display the initial window contents.
    WindowImage_Display, info
             
    Widget_Control, tlb, SET_UVALUE=info, /NO_COPY
     
    XManager, 'windowimage', tlb, /NO_BLOCK, EVENT_HANDLER='WindowImage_Resize'

END ;---------------------------------------------------------------------------------------
