;+
; NAME:
;  SCROLLWINDOW
;
; PURPOSE:
;
;  This procedure is more or less a drop-in replacement for the WINDOW
;  command. The main difference is that if the requested window size 
;  is larger then the current display size, the window is created in a 
;  base widget with scroll bars so the user can scroll around
;  the larger window. Use the WID keyword to pass in the window
;  index number of the window you want to create (a small change
;  from the WINDOW syntax). If the program can create a window with
;  this window index number, it will. Otherwise, this keyword will
;  return the window index number of the window that was actually
;  created.

;  I use ScrollWindow to create windows that I can view both on 
;  my large monitor at work and on my smaller laptop monitor when 
;  I travel.
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
;  ScrollWindow, xsize, ysize
;
; ARGUMENTS:
;
;  xsize:       The x size of the graphics window. By default, 640.
;
;  ysize:       The y size of the graphics window. By default, 512.
;
; KEYWORD PARAMETERS:
;
;  FREE:        Get a window with a free or unused window index number.
;               This is *always* done with a scrollable window. The window
;               index number of the window is returned in the WID keyword.
;
;  PIXMAP:      Set to create a pixmap window. In this case, no scrollable
;               window is possible. A normal IDL graphics window is
;               always created.
;
;  SIZEFRAC:    Make the window this fraction of the screen dimensions.
;               A number between 0.0 and 1.0
;  
;  TITLE:       The title string that is displayed on the window.
;
;  WID:         The window index number. If supplied as an IDL variable,
;               this can be both an input and an output keyword. If a
;               window with this window index number can be created, it
;               is. Otherwise, this varible upon exit from the program
;               contains the window index number of the graphics window
;               that was created.
;
;  XPOS:        The x offset of the upper-left corner of the window.
;
;  XSIZE:       The same as the xsize argument. Provided so ScrollWindow
;               can be a drop-in replacement for the Window command.
;
;  YPOS:        The y offset of the upper-left corner of the window.
;
;  YSIZE:       The same as the ysize argument. Provided so ScrollWindow
;               can be a drop-in replacement for the Window command.
;
;
;
; EXAMPLE:
;
;  ScrollWindow, XSIZE=800, YSIZE=400   ; Produces normal IDL graphics window.
;  ScrollWindow, XSIZE=1800, YSIZE=1200 ; Produces a scrollable graphics window.
;
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, 25 March 2009
;  Added SIZEFRACTION keyword, Mats Löfdahl, 25 November 2012.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009-2012, by Fanning Software Consulting, Inc.                           ;
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
PRO ScrollWindow, xsize, ysize, $
                  FREE=free, $
                  PIXMAP=pixmap, $
                  SIZEFRACTION = sizefraction, $
                  TITLE=title, $
                  WID=wid, $
                  XPOS=xpos, $
                  XSIZE=xs, $
                  YSIZE=ys, $
                  YPOS=ypos

    ; Get default values. Easier to pass sizes as parameters, but
    ; I want this to be a drop-in replacement for WINDOW, too.
    ; So, XSIZE and YSIZE keywords only checked if xsize and ysize
    ; are undefined.
    IF N_Elements(xsize) EQ 0 THEN BEGIN
        IF N_Elements(xs) EQ 0 THEN xsize = 640 ELSE xsize = xs
    ENDIF
    IF N_Elements(ysize) EQ 0 THEN BEGIN
        IF N_Elements(ys) EQ 0 THEN ysize = 512 ELSE ysize = ys
    ENDIF
    IF N_Elements(wid) EQ 0 THEN wid = 0
    
    ; If the user wants a pixmap, just make it and return.
    IF Keyword_Set(pixmap) THEN BEGIN
        IF Keyword_Set(free) THEN BEGIN
             Window, XSIZE=xsize, YSIZE=ysize, RETAIN=retain, FREE=1       
        ENDIF ELSE BEGIN
             Window, wid, XSIZE=xsize, YSIZE=ysize, RETAIN=retain
        ENDELSE  
        wid = !D.Window
        RETURN
    ENDIF
    
    ; Get the screen size of the display. Account for multiple monitors.
    s = Get_Screen_Size()
    IF s[0] GT 2000 THEN s[0] = s[0] / 2
    
    ;; Need different fudge factors for different operating systems.
    IF StrUpCase(!Version.OS_FAMILY) EQ 'WINDOWS' THEN BEGIN
          retain = 1
          xfudge = 25
          yfudge = 80
    ENDIF ELSE BEGIN
          retain = 2
          xfudge = 40
          yfudge = 100
    ENDELSE
    maxxsize = s[0] - xfudge
    maxysize = s[1] - yfudge
    
    ; Do you have a size fraction to consider?
    IF N_Elements(sizefraction) NE 0 THEN BEGIN
       maxxsize = Round(maxxsize * sizefraction)
       maxysize = Round(maxysize * sizefraction)
    ENDIF 
    
        ; Either make a window, or made a scrollable window.
    IF (xsize LT maxxsize) AND (ysize LT maxysize) THEN BEGIN
        IF Keyword_Set(free) THEN BEGIN
            Window, XSIZE=xsize, YSIZE=ysize, TITLE=title, $
                XPOS=xpos, YPOS=ypos, RETAIN=retain, FREE=1
        ENDIF ELSE BEGIN
            Window, wid, XSIZE=xsize, YSIZE=ysize, TITLE=title, $
                XPOS=xpos, YPOS=ypos, RETAIN=retain
        ENDELSE
        wid = !D.Window
    ENDIF ELSE BEGIN
        tlb = Widget_Base(TITLE=title, XOFFSET=xpos, YOFFSET=ypos, $
            X_SCROLL_SIZE=xsize < maxxsize, Y_SCROLL_SIZE=ysize < maxysize)
        draw = Widget_Draw(tlb, XSIZE=xsize, YSIZE=ysize, RETAIN=retain)
        Widget_Control, tlb, /Realize
        Widget_Control, draw, Get_Value=wid
        WSet, wid
     ENDELSE

END
