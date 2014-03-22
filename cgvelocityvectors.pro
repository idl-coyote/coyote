; docformat = 'rst'
;
; NAME:
;   cgVelocityVectors
;
; PURPOSE:
;   Plots the velocity vectors of particles at their position. Vectors may be
;   overplotted onto other IDL graphics plots.
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
;+
; Plots the velocity vectors of particles at their position. Vectors may be
; overplotted onto other IDL graphics plots. Vectors are centered on the particle
; position
;
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics
;
; :Params:
;    velx: in, required, type=integer/float
;         An array containing the X component of the particle velocity vector.
;    vely: in, required, type=integer/float
;         An array containing the Y component of the particle velocity vector.
;    posx: in, required, type=integer/float
;         An array containing the X posiiton of the particle velocity vector. The
;         shaft end of the arrow vector is positioned here.
;    posy: in, required, type=integer/float
;         An array containing the Y posiiton of the particle velocity vector. The
;         shaft end of the arrow vector is positioned here.
;
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;         An alternative way to set the `Window` keyword.
;     device: in, optional, type=boolean, default=0
;         Set this keyword to indicate the vector positions are given in 
;         device coordinates. Data coordinates are assumed.
;     fraction: in, optional, type=float, default=1.0
;         A number between 0.0 and 1.0 indicating the fraction of the vectors to
;         draw on the plot. Vectors are selected randomly from the total population.  
;     hsize: in, optional, type=float
;         The size of the the arrow head. By default 1/100th the width
;         of the device. (!D.X_SIZE / 100.)
;     hthick: in, optional, type=float
;         The thickness of the line drawing the arrowhead of the vector. The
;         default is 3 for the PostScript device and 1 for all other devices.
;     length: in, optional, type=float, default=0.075
;         The length of the "maximum" vector in normalized units. All vectors 
;         are scaled according to this length.
;     linestyle: in, optional, type=integer, default=0
;         The line style of the arrow. Line style integers as in PLOT.
;     maxvector: in, optional, type=float
;         The maximum magnitude of the vectors. If not defined, calculated
;         from the input vectors. All vectors are scaled according the the
;         value of this keyword and the `Length`. This keyword will return 
;         the maximum vector magnitude used in the program on exit.
;     normal: in, optional, type=boolean, default=0
;         Set this keyword to indicate the vector positions are given in 
;         normalized coordinates. Data coordinates are assumed.
;     overplot: in, optional, type=boolean, default=0
;         Set this keyword to overplot the vectors on an established coordinate
;         system plot.
;     solid: in, optional, type=boolean, default=0
;         Set this keyword to draw solid, filled arrows.
;     thick: in, optional, type=float
;         The thickness of the line drawing the shaft of the arrow. The
;         default is 3 for the PostScript device and 1 for all other devices.
;     veccolors: in, optional
;         A scalar or vector of colors the same size as `velx`. May be bytes, short integers,
;         or strings. Bytes and short integers are treated as indices into the current color 
;         table.
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to an cgWindow application.
;     xrange: in, optional, type=float
;         If a plot is to be drawn, the X range of the plot. By default, the X range is
;         calculated to contain all of the velocity vectors.
;     yrange: in, optional, type=float
;         If a plot is to be drawn, the Y range of the plot. By default, the Y range is
;         calculated to contain all of the velocity vectors.
;
; :Examples:
;    Generate some particle positions and velocities::
;
;         posx = RandomU(seed,200)
;         posy = RandomU(seed,200)
;         velx = RandomU(seed,200)-0.5
;         vely = RandomU(seed,200)-0.5
;
;    Plot the particle velocities::
;
;         cgVelocityVectors, velx, vely, posx, posy
;
;    Example using vector colors::
;
;         magnitude = SQRT(velx^2 + vely^2)
;         cgLoadCT, 5
;         colors = BytScl(magnitude)
;         cgVelocityVectors, velx, vely, posx, posy, VecColors=colors
;
; :Author:
;    FANNING SOFTWARE CONSULTING::
;        David W. Fanning
;        1645 Sheely Drive
;        Fort Collins, CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: david@idlcoyote.com
;        Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 22 March 2014
;
; :Copyright:
;     Copyright (c) 2014, Fanning Software Consulting, Inc.
;-
PRO cgVelocityVectors, velx, vely, posx, posy, $
   ADDCMD=addcmd, $
   FRACTION=fraction, $
   HSIZE=hsize, $
   HTHICK=hthick, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   MAXVECTOR=maxvector, $
   OVERPLOT=overplot, $
   SOLID=solid, $
   THICK=thick, $
   VECCOLORS=veccolors_in, $
   WINDOW=window, $
   XRANGE=xrange, $
   YRANGE=yrange, $
   _EXTRA=extra
   
   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /CANCEL
       void = cgErrorMsg()
       IF N_Elements(currentState) NE 0 THEN cgSetColorState, currentState
       RETURN
   ENDIF
   
   ; No parameters. Get some help.
   IF N_Params() EQ 0 THEN BEGIN
       Print, 'Syntax of cgVelocityVectors: cgVelocityVectors, velx, vely, posx, posy'
       RETURN
   ENDIF
   
   ; Need all four parameters. Get some help.
   IF N_Params() NE 4 THEN BEGIN
       Print, 'Syntax of cgVelocityVectors: cgVelocityVectors, velx, vely, posx, posy'
       Print, 'All four input parameters are required.'
       RETURN
   ENDIF
   
   ; All input parameters have to be the same length.
   IF ((N_Elements(velx) + N_Elements(vely) + N_Elements(posx) + N_Elements(posy)) / 4.0) NE N_Elements(velx) THEN BEGIN
        Message, 'Input parameters are not all the same length.'
   ENDIF
   
   ; Do they want this plot in a resizeable graphics window?
   IF Keyword_Set(addcmd) THEN window = 1
   IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
   
       ; Special treatment for overplotting or adding a command.
       IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
           cgWindow, 'cgVelocityVectors', velx, vely, posx, posy, $
               FRACTION=fraction, $
               HSIZE=hsize, $
               HTHICK=hthick, $
               LENGTH=length, $
               LINESTYLE=linestyle, $
               MAXVECTOR=maxvector, $
               OVERPLOT=overplot, $
               SOLID=solid, $
               THICK=thick, $
               VECCOLORS=veccolors_in, $
               XRANGE=xrange, $
               YRANGE=yrange, $
               ADDCMD=1, $
               _STRICT_EXTRA=extra
           RETURN
       ENDIF
       
       ; Open a new window or replace the current commands, as required.
       currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
       IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
       cgWindow, 'cgVelocityVectors', velx, vely, posx, posy, $
               FRACTION=fraction, $
               HSIZE=hsize, $
               HTHICK=hthick, $
               LENGTH=length, $
               LINESTYLE=linestyle, $
               MAXVECTOR=maxvector, $
               OVERPLOT=overplot, $
               SOLID=solid, $
               THICK=thick, $
               VECCOLORS=veccolors_in, $
               XRANGE=xrange, $
               YRANGE=yrange, $
               REPLACECMD=replaceCmd, $
               _STRICT_EXTRA=extra
       RETURN
   ENDIF
   
   ; Default keyword values.
   SetDefaultValue, fraction, 1.0, RANGE=[0.0, 1.0]
   SetDefaultValue, length, 0.075, RANGE=[0.0, 1.0]
   SetDefaultValue, overplot, /Boolean
   SetDefaultValue, solid, /Boolean
   IF N_Elements(thick) EQ 0 THEN BEGIN
       thick = (!D.Name EQ 'PS') ? 3 : 1
   ENDIF
   SetDefaultValue, hthick, thick
   SetDefaultValue, veccolors_in, 'opposite'
   
   ; Vector colors should be the same length as other vectors.
   veclength = N_Elements(velx)
   IF N_Elements(veccolors_in) EQ 1 THEN veccolors = Replicate(veccolors_in, veclength) ELSE veccolors = veccolors_in
   
   ; Calculated keyword values.
   magnitudes = SQRT(velx^2 + vely^2)
   IF N_Elements(maxvector) EQ 0 THEN maxvector = Max(magnitudes)
   maxvector = Double(maxvector)
   
   ; Scale the magnitudes into the maxVector.
   scaledMags = cgScaleVector(magnitudes, 0, maxvector)

   ; Calculate scaled velocities.
   scaledVx = length * (velx/maxvector)
   scaledVy = length * (vely/maxvector)
   
   ; Other end of vectors.
   x1 = posx + scaledVx
   y1 = posy + scaledVy
   
   ; Are we doing just a fraction of the vectors.
   IF fraction LT 1.0 THEN BEGIN
    
        ; Make sure you can get some.
        numGood = Long(fraction * vecLength)
        IF numGood EQ 0 THEN RETURN
        
        ; Compute indices of the vectors to plot. 
        goodIndices = Long(RandomU(seed, numGood) * vecLength)
        scaledVx = scaledVx[goodIndices]
        scaledVy = scaledVy[goodIndices]
        px = posx[goodIndices]
        py = posy[goodIndices]
        veccolors = veccolors[goodIndices]
        x1 = x1[goodIndices]
        y1 = y1[goodIndices]
        veclength = N_Elements(px)
        
   ENDIF ELSE BEGIN
        px = posx
        py = posy
        
   ENDELSE

   ; Do we need a plot?
   
   IF N_Elements(xrange) EQ 0 THEN BEGIN
       xr = Max(posx)- Min(posx)
       xrange = [Min(posx) - (xr*0.1), Max(posx)+(xr*0.1)]
   ENDIF
   IF N_Elements(yrange) EQ 0 THEN BEGIN
       yr = Max(posy)- Min(posy)
       yrange = [Min(posy) - (yr*0.1), Max(posy)+(yr*0.1)]
   ENDIF
    
   
   
   IF ~overplot THEN cgPlot, [1], /NoData, XRANGE=xrange, YRANGE=yrange, _STRICT_EXTRA=extra
   
   ; Calculate default head size.
   IF N_Elements(hsize) EQ 0 THEN hsize = !D.X_SIZE / 100.
   
   ; What kind of coordinate system are you using? cgArrow assumes (inexplicably!) a DEVICE coordinate system. This program
   ; assumes a more natural data coordinate system.
   CASE 1 OF
      ( Keyword_Set(normal) + Keyword_Set(device) ) EQ 0: BEGIN
        data = 1B
        normal = 0B
        END
      Keyword_Set(normal): BEGIN
        data = 0B
        normal = 1B
        END
      ELSE: BEGIN
        data = 0B
        normal = 0B
        END
   ENDCASE
   
   ; Make sure the endpoints of the vectors don't extend beyond the plot window.
   px = !X.CRange[0] > px < !X.CRange[1]
   py = !Y.CRange[0] > py < !Y.CRange[1]
   x1 = !X.CRange[0] > x1 < !X.CRange[1]
   y1 = !Y.CRange[0] > y1 < !Y.CRange[1]
   
   ; Draw the vectors.
   FOR j=0,vecLength-1 DO BEGIN
       cgArrow, px[j], py[j], x1[j], y1[j], $
           COLOR = veccolors[j], $
           DATA = data, $
           HSIZE = hsize, $
           HTHICK = hthick, $
           LINESTYLE=linestyle, $
           NORMAL = normal, $
           SOLID = solid, $
           THICK = thick
    ENDFOR
END