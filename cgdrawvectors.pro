; docformat = 'rst'
;
; NAME:
;   cgDrawVectors
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
; overplotted onto other IDL graphics plots.
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
;    posx_: in, required, type=integer/float
;         An array containing the X posiiton of the particle velocity vector. The
;         shaft end of the arrow vector is positioned here.
;    posy_: in, required, type=integer/float
;         An array containing the Y posiiton of the particle velocity vector. The
;         shaft end of the arrow vector is positioned here.
;
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;         An alternative way to set the `Window` keyword.
;     clip: in, optional, type=boolean, default=0
;         Set the keyword to clip vector output to the clipping rectangle specified
;         in the `CRect` keyword. See the documentation for the IDL graphics
;         keyword NOCLIP.
;     crect: in, optional, type=float
;          A four-element array giving the clipping rectangle [xo, yo, x1, y1]. The
;          default clipping rectangle is the plot area. See the documenation for the
;          IDL graphics keyword CLIP.
;     device: in, optional, type=boolean, default=0
;         Set this keyword to indicate the vector positions are given in 
;         device coordinates. Data coordinates are assumed.
;     fraction: in, optional, type=float, default=1.0
;         A number between 0.0 and 1.0 indicating the fraction of the vectors to
;         draw on the plot. Vectors are selected randomly from the total population,
;         unless the `Ordered` keyword is set, in which case they are selected
;         in an ordered, systematic fashion. For example, Fraction=0.5 will select
;         every other input vector.
;     hsize: in, optional, type=float
;         The size of the the arrow head. By default 1/100th the width
;         of the device. (!D.X_SIZE / 100.)
;     hthick: in, optional, type=float
;         The thickness of the line drawing the arrowhead of the vector. The
;         default is 3 for the PostScript device and 1 for all other devices.
;     length: in, optional, type=float, default=0.075
;         The length of the `ReferenceVector` in normalized units. All vectors 
;         are scaled according to this length.
;     linestyle: in, optional, type=integer, default=0
;         The line style of the arrow. Line style integers as in PLOT.
;     mapCoord: in, optional, type=object
;         A map coordinate object (e.g., cgMap) that describes the map projection
;         and datum used to specify the vector locations. Note that this could also be a 
;         map structure as returned from MAP_PROJ_INIT, but in that case the user is 
;         responsible for setting up the XY map coordinate space independently and 
;         outside of this program. This coordinate object will be used to transform
;         lat/lon locations into the XY locations of the map projection.
;     normal: in, optional, type=boolean, default=0
;         Set this keyword to indicate the vector positions are given in 
;         normalized coordinates. Data coordinates are assumed.
;     ordered: in, optional, type=boolean, default=0
;         If this keyword is set, and the `Fraction` keyword is used, the fraction
;         of vectors used in the plot are chosen from the entire population in a
;         systematic, ordered fashion.
;     overplot: in, optional, type=boolean, default=0
;         Set this keyword to overplot the vectors on an established coordinate
;         system plot.
;     referencevector: in, optional, type=float
;         The magnitude of a reference vector that is used to scale all other vectors before display.
;     solid: in, optional, type=boolean, default=0
;         Set this keyword to draw solid, filled arrows.
;     thick: in, optional, type=float
;         The thickness of the line drawing the shaft of the arrow. The
;         default is 3 for the PostScript device and 1 for all other devices.
;     veccolors: in, optional
;         A scalar or vector of colors the same size as `velx`. May be bytes, short integers,
;         or strings. Bytes and short integers are treated as indices into the current color 
;         table. The default is "opposite".
;     window: in, optional, type=boolean, default=0
;         Set this keyword to add the command to an cgWindow application.
;     xrange: in, optional, type=float
;         If a plot is to be drawn, the X range of the plot. By default, the X range is
;         calculated to contain all of the velocity vectors.
;     yrange: in, optional, type=float
;         If a plot is to be drawn, the Y range of the plot. By default, the Y range is
;         calculated to contain all of the velocity vectors.
;     _extra: in, optional
;         Any keywords appropriate for the cgPlot command can be used to create the plot.
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
;         cgDrawVectors, velx, vely, posx, posy
;
;    Example using vector colors::
;
;         magnitude = SQRT(velx^2 + vely^2)
;         cgLoadCT, 5
;         colors = BytScl(magnitude)
;         cgDrawVectors, velx, vely, posx, posy, VecColors=colors
;
;    Example using a map projection::
;        
;          posx = Indgen(10)*36
;          posy = Replicate(30,10)
;          velx = Replicate(10.,10) ; Wind out of the West
;          vely = Replicate(0, 10)  ; No vertical component
;          cgDisplay
;          mapNorth = Obj_New('cgMap', 'Polar Stereographic', $
;                Limit=[0, -180, 90, 180], /NoBorder)
;          mapNorth -> Draw
;          cgMap_Continents, Color='black', Map_Structure=mapNorth
;          cgMap_Grid, LatDel=15, LonDel=15, LineStyle=1, Color='charcoal', $
;                /Label, LonLabel=2, Map_Structure=mapNorth
;          cgDrawVectors, velx, vely, posx, posy, VecColors='blu6', /Overplot, $
;                ReferenceVector=10, /Solid, Thick=2, MapCoord=mapNorth
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
;        Written by David Fanning, based on NASA Astronomy Library program PartVelVec, 22 March 2014
;        Added ORDERED keyword. 24 March 2014. DWF.
;        Drawing the vectors All Wrong. Using what I think is the correct algorithm now. 24 March 2014. DWF.
;        Modified so I don't change into position variables. 24 March 2014. DWF.
;        Fixed a small problem with the algorithm for calculating the direction of the scaled vectors and
;           added CLIP and CRECT keywords. 25 March 2014. DWF.
;        Added MAPCOORD keyword. 27 March 2014. DWF.
;        Modified to set color information once here, instead of allowing cgArrow to do it. 27 March 2014. DWF.
;        Some map projections (e.g., polar stereographic) can change the direction of the vector on the 
;           projected map. To solve this problem, I needed to create a second point in the original coordinate
;           system and project both points into the map coordinate system before calculating the angle
;           between points. Also then had to figure out how to scale the moved point to the reference
;           vector. All appears normal now. 2 Nov 2014. DWF.
;
; :Copyright:
;     Copyright (c) 2014, Fanning Software Consulting, Inc.
;-
PRO cgDrawVectors, velx, vely, posx_, posy_, $
   ADDCMD=addcmd, $
   CLIP=clipit, $
   CRECT=crect, $
   DEVICE=device, $
   FRACTION=fraction, $
   HSIZE=hsize, $
   HTHICK=hthick, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   MAPCOORD=mapcoord, $
   NORMAL=normal, $
   ORDERED=ordered, $
   OVERPLOT=overplot, $
   REFERENCEVECTOR=referenceVector, $
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
       Print, 'Syntax of cgDrawVectors: cgDrawVectors, velx, vely, posx, posy'
       RETURN
   ENDIF
   
   ; Need all four parameters. Get some help.
   IF N_Params() NE 4 THEN BEGIN
       Print, 'Syntax of cgDrawVectors: cgDrawVectors, velx, vely, posx, posy'
       Print, 'All four input parameters are required.'
       RETURN
   ENDIF
   
   ; All input parameters have to be the same length.
   IF ((N_Elements(velx) + N_Elements(vely) + N_Elements(posx_) + N_Elements(posy_)) / 4.0) NE N_Elements(velx) THEN BEGIN
        Message, 'Input parameters are not all the same length.'
   ENDIF
   
   ; Do they want this plot in a resizeable graphics window?
   IF Keyword_Set(addcmd) THEN window = 1
   IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
   
       ; Special treatment for overplotting or adding a command.
       IF Keyword_Set(overplot) OR Keyword_Set(addcmd) THEN BEGIN
           cgWindow, 'cgDrawVectors', velx, vely, posx_, posy_, $
               CLIP=clipit, $
               CRECT=crect, $
               DEVICE=device, $
               FRACTION=fraction, $
               HSIZE=hsize, $
               HTHICK=hthick, $
               LENGTH=length, $
               LINESTYLE=linestyle, $
               MAPCOORD=mapcoord, $
               NORMAL=normal, $
               ORDERED=ordered, $
               OVERPLOT=overplot, $
               REFERENCEVECTOR=referenceVector, $
               SOLID=solid, $
               THICK=thick, $
               VECCOLORS=veccolors_in, $
               WINDOW=window, $
               XRANGE=xrange, $
               YRANGE=yrange, $
               ADDCMD=1, $
               _STRICT_EXTRA=extra
           RETURN
       ENDIF
       
       ; Open a new window or replace the current commands, as required.
       currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
       IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
       cgWindow, 'cgDrawVectors', velx, vely, posx_, posy_, $
               CLIP=clipit, $
               CRECT=crect, $
               DEVICE=device, $
               FRACTION=fraction, $
               HSIZE=hsize, $
               HTHICK=hthick, $
               LENGTH=length, $
               LINESTYLE=linestyle, $
               MAPCOORD=mapcoord, $
               NORMAL=normal, $
               ORDERED=ordered, $
               OVERPLOT=overplot, $
               REFERENCEVECTOR=referenceVector, $
               SOLID=solid, $
               THICK=thick, $
               VECCOLORS=veccolors_in, $
               WINDOW=window, $
               XRANGE=xrange, $
               YRANGE=yrange, $
               REPLACECMD=replaceCmd, $
               _STRICT_EXTRA=extra
       RETURN
   ENDIF
   
   ; Doing this in decomposed color.
   cgSetColorState, 1, Current=thisColorState
   
   ; Default keyword values.
   SetDefaultValue, fraction, 1.0, RANGE=[0.0, 1.0]
   SetDefaultValue, length, 0.075, RANGE=[0.0, 1.0]
   SetDefaultValue, overplot, /Boolean
   SetDefaultValue, solid, /Boolean
   IF N_Elements(thick) EQ 0 THEN BEGIN
       thick = (!D.Name EQ 'PS') ? 3 : 1
   ENDIF
   SetDefaultValue, hthick, thick
   
   ; Calculated keyword values.
   magnitudes = SQRT(velx^2 + vely^2)
   IF N_Elements(referenceVector) EQ 0 THEN referenceVector = Max(magnitudes)
   referenceVector = Double(referenceVector)
   
   ; Calculate the angle between velocity vectors in radians.
   angle = ATan(vely, Double(velx))
   
   ; Pick an arbitary point in the correct direction. We need this point if we
   ; are going to point the vector in the correct direction if we have a weird
   ; map projection space.
   aPx = (ABS(velx) * Cos(angle)) + posx_
   aPy = (ABS(vely) * Sin(angle)) + posy_
   
   ; Do we need to transform the position coordinates with a map coordinate object?
   IF N_Elements(mapcoord) NE 0 THEN BEGIN
       IF Obj_Valid(mapCoord) THEN mapStruct = mapCoord -> GetMapStruct() ELSE mapStruct = mapCoord
       xy = Map_Proj_Forward(posx_, posy_, MAP_STRUCTURE=mapStruct)
       posx = Reform(xy[0,*])
       posy = Reform(xy[1,*])
       xy = Map_Proj_Forward(aPx, aPy, MAP_STRUCTURE=mapStruct)
       aposx = Reform(xy[0,*])
       aposy = Reform(xy[1,*])
             
       ; Recalculate the angle in the XY map projection space.
       origAngle = angle
       angle = ATan(aposy-posy, Double(aposx-posx))
       mapTransform = 1
   ENDIF ELSE BEGIN
       posx = posx_
       posy = posy_
       mapTransform = 0
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
   
   ; Color determination is postponed to here, after a plot is drawn.
   SetDefaultValue, veccolors_in, cgColor('opposite')
   
   ; Vector colors should be the same length as other vectors.
   veclength = N_Elements(velx)
   IF N_Elements(veccolors_in) EQ 1 THEN veccolors = Replicate(veccolors_in, veclength) ELSE veccolors = veccolors_in
   
   ; Calculate default head size.
   IF N_Elements(hsize) EQ 0 THEN hsize = !D.X_SIZE / 100.
   
   ; Calculate scaled velocities in normalized coordinate units.
   IF mapTransform THEN BEGIN
       scaledVx = length * (ABS(velx) * Cos(origAngle) / referenceVector)
       scaledVy = length * (ABS(vely) * Sin(origAngle) / referenceVector)
   ENDIF ELSE BEGIN
       scaledVx = length * (ABS(velx) * Cos(angle) / referenceVector)
       scaledVy = length * (ABS(vely) * Sin(angle) / referenceVector)
   ENDELSE
   
   ; What kind of coordinate system are you using? You need to know to 
   ; calcuate the arrow end  of the vector.
   CASE 1 OF
       Keyword_Set(device): BEGIN
         xy = Convert_Coord(posx, posy, /Device, /To_Normal)
         x1 = scaledVx + Reform(xy[0,*])
         y1 = scaledVy + Reform(xy[1,*])
         px = Reform(xy[0,*])
         py = Reform(xy[1,*])
         
         ; If you have a map projection, you have to turn these normalized points into
         ; the map projection location.
         IF mapTransform THEN BEGIN
             x1 = length * Cos(angle)
             y1 = length * Sin(angle)
             xyd = Convert_Coord(px+x1, py+y1, /Device, /To_Data)
             xyn = Convert_Coord(xyd[0,*], xyd[1,*], /Data, /To_Normal)
             x1 = xyn[0,*]
             y1 = xyn[1,*]
         ENDIF
         
         END
        
       Keyword_Set(normal): BEGIN  
          x1 = posx + scaledVx
          y1 = posy + scaledVy
          px = posx
          py = posy
          
          ; If you have a map projection, you have to turn these normalized points into
          ; the map projection location.
          IF mapTransform THEN BEGIN
              x1 = length * Cos(angle)
              y1 = length * Sin(angle)
              xyn = Convert_Coord(px+x1, py+y1, /Normal, /To_Data)
              xyd = Convert_Coord(xyn[0,*], xyn[1,*], /Data, /To_Normal)
              x1 = xyd[0,*]
              y1 = xyd[1,*]
          ENDIF
          
          END

       ELSE: BEGIN
        
         ; Create normalized points.
         xy = Convert_Coord(posx, posy, /Data, /To_Normal)
         x1 = scaledVx + Reform(xy[0,*])
         y1 = scaledVy + Reform(xy[1,*])
         px = Reform(xy[0,*])
         py = Reform(xy[1,*])
         
         ; If you have a map projection, you have to turn these normalized points into
         ; the map projection location.
         IF mapTransform THEN BEGIN
            x1 = length * Cos(angle)
            y1 = length * Sin(angle)
            xyd = Convert_Coord(px+x1, py+y1, /Normal, /To_Data)
            xyn = Convert_Coord(xyd[0,*], xyd[1,*], /Data, /To_Normal)
            x1 = xyn[0,*]
            y1 = xyn[1,*]
         ENDIF
          
       END
   ENDCASE
   
   ; Are we doing just a fraction of the vectors.
   IF fraction LT 1.0 THEN BEGIN
    
        ; Make sure you can get some.
        numGood = Long(fraction * vecLength)
        IF numGood EQ 0 THEN RETURN
        
        ; Compute indices of the vectors to plot. 
        IF Keyword_Set(ordered) THEN BEGIN
            goodIndices = Long(DIndGen(numGood) / (numGood - 1L) * vecLength)
        ENDIF ELSE BEGIN
            goodIndices = Long(RandomU(seed, numGood) * vecLength)
        ENDELSE
        scaledVx = scaledVx[goodIndices]
        scaledVy = scaledVy[goodIndices]
        px = px[goodIndices]
        py = py[goodIndices]
        veccolors = veccolors[goodIndices]
        x1 = x1[goodIndices]
        y1 = y1[goodIndices]
        veclength = N_Elements(px)
        
   ENDIF 

   ; Draw the vectors.
   FOR j=0,vecLength-1 DO BEGIN
       cgArrow, px[j], py[j], x1[j], y1[j], $
           CLIP=clipit, $
           COLOR = veccolors[j], $
           CRECT=crect, $
           HSIZE = hsize, $
           HTHICK = hthick, $
           LINESTYLE=linestyle, $
           NORMAL = 1, $
           SOLID = solid, $
           THICK = thick
    ENDFOR
    
    ; Restore color state
    cgSetColorState, thisColorState
END