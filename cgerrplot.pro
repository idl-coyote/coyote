; docformat = 'rst'
;
; NAME:
;   cgErrPlot
;
; PURPOSE:
;   The purpose of cgErrPlot is to overplot error bars for data points on 
;   previously drawn line and bar plots.
;;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; The purpose of cgErrPlot is to overplot error bars for data points on 
; previously drawn line and bar plots.
; 
; To allow cgErrPlot to be a drop-in replacement for the IDL ErrPlot program, the program
; can be called in either of two ways::
; 
;     cgErrPlot, indep, low, high
;     
; Or::
;     
;     cgErrPlot, high, low
;     
; This is a really weird syntax (high and low reversed!), so be careful!
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;    
; :Params:
;    indep: in, required, type=any
;         The independent data vector (X vector) if all three input parameters are used.
;         If only two input parameters are used, this parameter is assumed to be the `High`
;         parameter.
;    low: in, required, type=any
;         If three input parameters are used, this parameter is assumed to contain the low
;         values of the error estimates. Error bars will be drawn from the low value to the
;         high value. 
;    high: in, optional, type=any
;         If three input parameters are used, this parameter is assumed to contain the high
;         values of the error estimates. Error bars will be drawn from the low value to the
;         high value. 
;         
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow display.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table. May be a vector of the same
;        length as X.
;     device: in, optional, type=boolean
;        Not used. Just defined so it can't be used by the user of the program.
;     horizontal: in, optional, type=boolean, default=0
;        Set this keyword to draw an errorbar along the X direction.
;        In this case, indep contains coordinates along the Y axis,
;        and low and high along the X axis
;     noclip: in, optional, type=boolean, default=!P.NoClip
;        Defined differently than in PlotS.
;     psym: in, optional, type=boolean
;        Not used. Just defined so it can't be used by the user of the program.
;     thick: in, optional, type=float, default=!P.Thick
;        The width of the lines used to draw the error bars.
;     width: in, optional, type=float
;        The width of the error bars, in units of the width of the plot area.  The default 
;        is 1% of plot width. May be a vector of the same length as X.
;     _extra: in, optional, type=any
;        Any graphics keywords supported by the PLOTS command are allowed.
;         
; :Examples:
;    Use like the IDL ErrPlot command::
;       data = cgDemoData(1)
;       seed = 3L ; So you see what I see.
;       x = Indgen(N_Elements(data))
;       cgPlot, x, data
;       errLow = RandomU(seed, N_Elements(data)) * 1.5
;       errHigh = RandomU(seed, N_Elements(data)) * 1.5
;       
;    Then, use this::   
;       cgErrPlot, x, data-errLow, data+errHigh, COLOR='red'
;
;    Or, use this::
;       cgErrPlot, data+errHigh, data-errLow, Color='red'
;
;    Use with a bar plot::
;       bardata = Indgen(10)+1
;       barLow = RandomU(seed, N_Elements(bardata)) * 1.5
;       barHigh = RandomU(seed, N_Elements(bardata)) * 1.5
;       cgBarPlot, bardata, BarCoords=x
;       cgErrPlot, x, bardata-barLow, bardata+barHigh, Color='blue'
;       
;    Use with horizontal bars.
;       y = cgDemoData(1)
;       seed = 3L
;       x = Indgen(N_Elements(y))
;       cgPlot, x, y, /WINDOW
;       errLow = RandomU(seed, N_Elements(y)) * 1.5
;       errHigh = RandomU(seed, N_Elements(y)) * 1.5
;       cgErrPlot, x, y-errLow, y+errHigh, COLOR='red', /ADDCMD
;       cgErrPlot, y, x-errLow, x+errHigh, COLOR='blue', /HORIZONTAL, /ADDCMD
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
;        Written, 30 Jan 2012. Modeled on ErrPlot from IDL library. David W. Fanning.
;        Added Horizontal keyword. 19 March 2013. Fabien Maussion.
;        Forgot to allow COLOR keyword to be a vector of colors. 18 June 2013. DWF.
;        
; :Copyright:
;     Copyright (c) 2012, Fanning Software Consulting, Inc.
;-
PRO cgErrPlot, indep, low, high, $
   AddCmd=addcmd, $
   Color=scolor, $
   Device=device, $
   Horizontal=horizontal, $
   NoClip=noclip, $
   PSym=psym, $
   Thick=thick, $
   Width=width, $
   _Extra=extra
   
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        IF N_Elements(dataSwitch) NE 0 THEN BEGIN
            indep = low
            low = high
            Undefine, high
        ENDIF
        RETURN
    ENDIF
    
    ; Did the user pass parameters?
    IF N_Params() EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgErrPlot, indep, lowError, highError'
        RETURN
    ENDIF
    
    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(addcmd)) && ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        cgWindow, 'cgErrPlot', indep, low, high, $
           AddCmd=1, $
           Color=scolor, $
           Device=device, $
           Horizontal=horizontal, $
           NoClip=noclip, $
           PSym=psym, $
           Thick=thick, $
           Width=width, $
           _Extra=extra
                    
         RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Sort the parameters out.
    n_params = N_Params()
    IF n_params EQ 1 THEN Message, 'At least two positional parameters are required.'
    IF (n_params EQ 2) THEN BEGIN
        high = indep
        indep = Findgen(N_Elements(low))
        dataSwitch = 1
    ENDIF
    
    ; Going to draw the lines in decomposed color, if possible
    SetDecomposedState, 1, CurrentState=currentState
    
    ; Choose a color.
    color = cgDefaultColor(sColor, DEFAULT='OPPOSITE')
    IF Size(color, /TNAME) EQ 'STRING' THEN color = cgColor(color)
    
    ; Check parameters and keywords.
    device = 1
    psym = 0
    IF N_Elements(thick) EQ 0 THEN thick = !P.Thick
    IF Keyword_Set(horizontal) THEN BEGIN
      w = ((N_Elements(width) EQ 0) ? 0.01 : width) * (!Y.Window[1] - !Y.Window[0]) * !D.Y_Size*0.5
    ENDIF ELSE BEGIN
      w = ((N_Elements(width) EQ 0) ? 0.01 : width) * (!X.Window[1] - !X.Window[0]) * !D.X_Size*0.5
    ENDELSE
    n = N_Elements(high) < N_Elements(low) < N_Elements(indep) ;# of pnts
    
    ; If user hasn't set NOCLIP, follow what is in !P.NoClip.
    noclip = (N_Elements(noclip) GT 0) ? noclip : !P.NOCLIP
    
    ; Make sure color is the same length as indep.
    IF N_Elements(color) NE N_Elements(indep) THEN color = Replicate(color, N_Elements(indep))
    
    ; Draw each error bar in a loop.
    IF Keyword_Set(horizontal) THEN BEGIN
      FOR i=0,n-1 DO BEGIN
        xy0 = Convert_Coord(low[i], indep[i], /DATA, /TO_DEVICE)
        xy1 = Convert_Coord(high[i], indep[i], /DATA, /TO_DEVICE)
        PlotS, [Replicate(xy0[0],3), Replicate(xy1[0],3)], $
          [xy0[1] + [-w, w,0], xy1[1] + [0, -w, w]], $
          DEVICE=device, NOCLIP=noclip, PSYM=psym, COLOR=color[i], THICK=thick, $
          _STRICT_EXTRA=extra
      ENDFOR
    ENDIF ELSE BEGIN
      FOR i=0,n-1 DO BEGIN
        xy0 = Convert_Coord(indep[i], low[i], /DATA, /TO_DEVICE)
        xy1 = Convert_Coord(indep[i], high[i], /DATA, /TO_DEVICE)
        PlotS, [xy0[0] + [-w, w,0], xy1[0] + [0, -w, w]], $
          [Replicate(xy0[1],3), Replicate(xy1[1],3)], $
          DEVICE=device, NOCLIP=noclip, PSYM=psym, COLOR=color[i], THICK=thick, $
          _STRICT_EXTRA=extra
      ENDFOR
    ENDELSE

    
    SetDecomposedState, currentState
END
   