; docformat = 'rst'
;
; NAME:
;   cgAspect
;
; PURPOSE:
;   The purpose of this function is to calculate a position in a graphics window with
;   a specified aspect ratio (ysize/xsize).
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of this function is to calculate a position in a graphics window with
; a specified aspect ratio (ysize/xsize).
;
; :Categories:
;    Utility
;    
; :Params:
;    aspect_ratio: in, required, type=float
;         The requested aspect ratio. Be careful to specify this as a float.
;       
; :Keywords:
;    align: in, optional, type=string, default='center'
;         Normally, the output position is centered in the intial input position. This
;         keyword can alter the placement of the final position with respect to the initial
;         position. Allowed values are "top", "right", "bottom", "left", and "center".
;    aspect: in, optional, type=array
;         This keyword provides an alternative way of specifying the `aspect_rato` value. Pass
;         this keyword any 2D array, and the aspect ratio will be calculated to match the 2D array.
;    position: in, optional, type=array
;         Use this keyword to specify a four-element normalized starting position for calculating
;         the final position in the graphics window. By default, the staring position is given 
;         by [0.0, 0.0, 1.0, 1.0] (the entire window). If the `Single_Plot` keyword is set, the
;         default position is given by [0.125, 0.125, 0.9, 0.9].
;    single_plot: in, optional, type=boolean, default=0
;         Set this keyword to change the default starting `Position` to [0.125, 0.125, 0.9, 0.9].
;    waspect: in, optional, type=float
;         Set this keyword to the window aspect ratio. If not used, the window aspect is calculated from
;         the current graphics window as Float(!D.Y_VSize) / !D.X_VSize.
;         
; :Examples:
;    Here is how to use this program to display two differently sized images aligned at their tops::
;        img1 = cgDemoData(7)
;        img2 = congrid(img1, 360, 180)
;        cgDisplay, 900, 450
;        
;        pos = cgLayout([2,1], OYMargin=[4, 11], OXMargin=[5, 8], XGap=6)
;        pos1 = pos[*,0]
;        img1pos = cgAspect(Position=pos1, Aspect=img1, Align='top')
;        cgImage, img1, CTIndex=3, /AXES, POSITION=img1pos, OPOSITION=op
;        cgColorbar, CTIndex=3, /Fit    
;            
;        pos2 = pos[*,1]
;        img2pos = cgAspect(Position=pos2, Aspect=img2, Align='top')
;        cgImage, img2, /AXES, POSITION=img2pos, CTIndex=2, /NoErase
;        cgColorbar, CTIndex=2, /Fit  
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
;        Written, 12 February 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
FUNCTION cgAspect, aspect_ratio, $
   ALIGN=align, $
   ASPECT=aspect, $
   POSITION=position, $
   SINGLE_PLOT=single_plot, $
   WASPECT=waspect
   
   ; Return to caller in the event of an error.
   On_Error, 2
   
   ; Is there a 2D array to use to set the requested aspect_ratio?
   IF N_Elements(aspect) NE 0 THEN BEGIN
        dims = Size(aspect, /Dimensions)
        aspect_ratio = Double(dims[1]) / dims[0]
   ENDIF
   
   ; Do you have an aspect ratio?
   IF N_Elements(aspect_ratio) EQ 0 THEN BEGIN
      Message, 'Calling Syntax:  position = cgAspect(aspect_ratio)', /Informational
      RETURN, Replicate(0.0,4)
   ENDIF ELSE aspect_ratio = Double(aspect_ratio)
   
   ; Do we need a position in the window?
   IF N_Elements(position) EQ 0 THEN BEGIN
      IF Keyword_Set(single_plot) THEN BEGIN
          position = [0.125d, 0.125d, 0.9d, 0.9d]
      ENDIF ELSE BEGIN
          position = [0.0d, 0.0d, 1.0d, 1.0d]
      ENDELSE
   ENDIF
   
   ; Do we need a window aspect ratio?
   IF N_Elements(waspect) EQ 0 THEN BEGIN
      waspect = Double(!D.Y_VSize) / !D.X_VSize
   ENDIF
   
   ; Do you have an alignment?
   IF N_Elements(align) EQ 0 THEN align = 'center'
   align = StrUpCase(align)
   
   ; Find the proposed size in pixels without aspect considerations.
   xpixSize = (position[2] - position[0]) * !D.X_VSize
   ypixSize = (position[3] - position[1]) * !D.Y_VSize
   
   ; Try to fit the width. If you can't maintain the aspect ratio, fit the height.
   trialX = xpixSize
   trialY = trialX * aspect_ratio
   IF trialY GT ypixSize THEN BEGIN
       trialY = ypixSize
       trialX = trialY / aspect_ratio
   ENDIF
   
   ; Recalculate the position in the window.
   pos = FltArr(4)
   pos[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + position[0]
   pos[2] = pos[0] + (trialX/FLOAT(!D.X_VSize))
   pos[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + position[1]
   pos[3] = pos[1] + (trialY/FLOAT(!D.Y_VSize))

   CASE align OF
    
       'RIGHT': BEGIN
           xdiff = position[2] - pos[2]
           pos[0] = pos[0] + xdiff
           pos[2] = position[2]
           END
    
       'TOP': BEGIN
           ydiff = position[3] - pos[3]
           pos[3] = position[3]
           pos[1] = pos[1] + ydiff
           END
           
       'LEFT': BEGIN
           xdiff = pos[0] - position[0]
           pos[0] = position[0]
           pos[2] = position[2] - xdiff
           END
           
       'BOTTOM': BEGIN
           ydiff = pos[1] - position[1]
           pos[1] = position[1]
           pos[3] = pos[3] - ydiff
           END
          
       'CENTER': 
           
        ELSE: Message, 'Unknown alignment parameter: ' + align
       
   ENDCASE
   
   ; Return the position
   RETURN, pos
   
END