; docformat = 'rst'
;
; NAME:
;   cgWindRose
;
; PURPOSE:
;   This program draws a wind rose diagram.
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
; This program draws a wind rose diagram. A wind rose diagram shows the frequency, speed, and 
; direction of winds over some defined period of time. It is widely used in meteorological
; applications (`see here: <http://en.wikipedia.org/wiki/Wind_rose>`).
; 
; Data to test the program (in SAMPSON formatted data files) can be freely downloaded from
; the `Meteorological Resource Center <http://www.webmet.com/>`.
;
; :Categories:
;    Graphics
;
; :Examples:
;    An example of how to use the program can be found in the
;    `Coyote Plot Gallery <http://www.idlcoyote.com/gallery.index#WINDROSE>`.
;
; .. image:: cgwindrose.png
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
;        Written, 7 March 2013 by David W. Fanning.
;        Fixed error in which I was assuming some calm winds. 23 May 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-

;+
; This function returns 100 x and y points as a 2x100 array that forms an arc between two angles
; when plotted. The arc is created as if the 0 angle was to the North (top on drawings).
; 
; :Params:
;     xcenter: in, required
;        The X center of the arc.
;     ycenter: in, required
;        The Y center of the arc.
;     radius: in, required
;        The radius of the desired arc.
;     angle1: in, optional, type=float, default=0.0
;        The first angle. The arc is drawn between the first angle and the second angle.
;     angle2: in, optional, type=float, default=360.0
;        The second angle. The arc is drawn between the first angle and the second angle.
;-
FUNCTION cgWindRoseArc, xcenter, ycenter, radius, angle1, angle2

    ; Return to caller on an error.
    On_Error, 2
    
    ; Need angles?
    IF N_Elements(angle1) EQ 0 THEN angle1 = 0.0
    IF N_Elements(angle2) EQ 0 THEN angle2 = 360.0
    
    ; Scale 100 points between the first and second angle.
    points = cgScaleVector(Findgen(100), angle1, angle2)
    
    
    ; Calculate the X and Y values of these points. Do this so
    ; that the zero angle is North, not East as is normal in
    ; most IDL coordinate systems.
    x = xcenter + radius * Sin(points * !DtoR )
    y = ycenter + radius * Cos(points * !DtoR)
    
    ; Return a 2-column array. X values are in the first column and Y values are in the second column.
    RETURN, Transpose([[x],[y]])
    
END

;+
; This is a tick labeling function that prevents negative tick values.
; It also labels all tick values as precentages. Set this function as
; the name of either the XTickFormat or YTickFormat keyword on a Plot or
; Axis command. This is not called directly by a user.
; 
; :Params:
;     axis: in, required, type=integer
;        The type of axis. 0 is X, 1 is Y, etc.
;     index: in, required, type=integer
;         The index of the tick label.
;     value: in, required
;         The tick value to be transformed by the function.
;-
FUNCTION cgWindRose_PositiveLabel, axis, index, value
    IF value LT 0 THEN value = Abs(value)
    RETURN, String(value, Format='(F0.1)') + '%'
END


;+
; Read a SAMSON format meteorological data file to obtain the wind
; speed and direction arrays.
;
; :Params:
;       filename: in, optional, type=string
;           The path to a SAMSON (*.sam) format meteorological data file from which wind speed and wind
;           direction can be obtained.
;
; :Keywords:
;      speed: out, optional, type=float
;         The wind speed as obtained from the file.
;      direction: out, optional, type=float
;         The direction of the wind as obtained from the file.
; 
;-
PRO cgWindRose_ReadSamFile, filename, SPEED=speed, DIRECTION=direction

    Compile_Opt idl2
    
    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = cgErrorMsg()
        RETURN
    ENDIF

    ; Open the SAMSON format file and obtain the wind speed and direction values.
    OpenR, lun, filename, /Get_Lun
    header = StrArr(2)
    lines = File_Lines(filename) - 2
    format = '(76x,I3,x,f0.1)'
    direction = IntArr(lines)
    speed = FltArr(lines)
    d=0
    s = 0.0
    ReadF, lun, header
    FOR j=0,lines-1 DO BEGIN
        ReadF, lun, d, s, Format=format
        direction[j] = d
        speed[j] = s
    ENDFOR
    Free_Lun, lun
    
    ; Find any missing data in the direction and speed arrays.
    missingIndices = Where((direction NE 999) AND (speed NE 999.0), missingCnt)
    IF missingCnt GT 0 THEN BEGIN
        direction = direction[missingIndices]
        speed = speed[missingIndices]
    ENDIF
    
    ; Make sure the direction array is a float.
    direction = Temporary(Float(direction))
    
END

;+
; Draw a wind rose diagram from the input wind speed and wind direction parameters.
; The wind rose diagram treats a 0 angle as pointing to the North, or toward the top
; of the diagram. Note this is different from normal IDL coordinate systems, which treat the
; 0 angle as pointing to the right of the diagram or plot.
; 
; :Params:
; 
;      speed: in, required, type=float
;         The wind speed array. Note that missing values should be removed prior to passing the
;         speed array to the program. This vector should have the same number of elements as the
;         `direction` array. It is assumed there are no negative speed values in the array. Winds
;         assumed to be measured in meters/second.
;      direction: in, required, type=float
;         The direction of the wind. This should have the same number of elements as the `speed` array.
;        The wind directions are assumed to be in the range 0 to 360.
;
; :Keywords:
;       calmsfreq: out, optional, type=float
;           The frequency of calms measurements in the data. (Winds less than 1 meter/second.)
;       samfile: in, optional, type=string
;           The path to a SAMSON (*.sam) format meteorological data file from which wind speed and wind
;           direction can be obtained.
;       speedbinsize:, in, optional, type=float, default=3
;           The size of the bin when the speed is binned with the Histogram command.
;       title: in, optional, type=string
;           The plot title.
;-
PRO cgWindRose, speed, direction, $
    CALMSFREQ=calmsFreq, $
    SAMFILE=samFile, $
    SPEEDBINSIZE=speedBinSize, $
    TITLE=title

   Compile_Opt idl2
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      void = cgErrorMsg()
      RETURN
   ENDIF
   
   ; We need speed and direction arrays. If we don't have them, many we have the
   ; name of a SAMPSON file that we can read to obtain these arrays.
   IF N_Params() NE 2 THEN BEGIN
    
      ; Do we have a SAMSON file? If not, ask the user to choose one. If that
      ; proves fruitless, then jump ship. SAMSON meteorological data files can be obtained for
      ; free here: http://www.webmet.com/State_pages/met_co.htm#samson.
      IF N_Elements(samFile) EQ 0 THEN BEGIN
         samFile = cgPickfile(Filter='*.sam', Title='Select a Meteorological SAMSON File...')
         IF samFile EQ "" THEN RETURN
      ENDIF 
      
      ; Open the SAM file. It should be easy to add your own file reader to this program!
      cgWindRose_ReadSamFile, samFile, SPEED=speed, DIRECTION=direction
      
   ENDIF

   ; Do we have both speed and direction arrays?
   IF (N_Elements(speed) EQ 0) || (N_Elements(direction) EQ 0) THEN BEGIN
       Print, 'Calling Syntax: cgWindRose, speed, direction'
       RETURN
   ENDIF
   
   ; Set keyword default values.
   SetDefaultValue, speedBinSize, 3.0
   SetDefaultValue, title, ''
   
   ; Are these arrays the same size?
   IF N_Elements(speed) NE N_Elements(direction) THEN $
      Message, 'Speed and Direction arrays must be the same size.'
      
   ; Make sure direction values of 360 are treated as 0.
   direction = direction MOD 360.

   ; How many elements of speed. Need to calculate frequency.
   speedCount = N_Elements(speed)
   
   ; How many calm measurements are there with winds less than 1.0.
   calms = Where(speed LT 1.0, count, Complement=winds)
   calmsFreq =  count/Float(speedCount)*100
   
   ; Remove the calms data from the data set.
   IF count GT 0 THEN BEGIN
      speed_final = speed[winds]
      direction_final = direction[winds]
   ENDIF ELSE BEGIN
       speed_final = speed
       direction_final = direction
   ENDELSE
   
   maxSpeed = Round(Max(speed)/10.)*10

   ; Compute the 2D matrix for speed and direction.
   matrix = Hist_ND(Transpose([[speed_final],[direction_final]]), [speedBinSize, 22.5], $
       MIN=[1.0, -11.25], MAX=[maxSpeed, 360])
      
   ; Combine the first and last direction bins and trim the matrix.
   s = Size(matrix, /Dimensions)
   matrix[*,0] = matrix[*,0] + matrix[*,s[1]-1]
   matrix = matrix[*,0:s[1]-2]
      
   ; Calculate the maximum frequency to plot.
   frequency = total(matrix,1)/N_Elements(speed_final)*100
   maxFreq = Round(Max(frequency*1.1)*10)/10
   IF maxFreq MOD 2 NE 0 THEN maxFreq = maxFreq + 1
   ;Print, 'Maximum Frequency: ', maxFreq
   
   ; Open a display window.
   cgDisplay, 600, 600
   
   ; Set up the data coordinate system. Nothing shown here.
   cgPlot, [-maxFreq, maxFreq],[-maxFreq, maxFreq], /NoData, $
       ASPECT=1.0, XStyle=5, YStyle=5, position=[0.15, 0.15, 0.89, 0.9]
       
   ; Draw four circles, last at maxFreq.
   radii = (Indgen(4)+1) *(maxFreq/4.0)
   FOR j=0,3 DO BEGIN
       pts = cgWindroseArc(0, 0, radii[j])
       cgPlotS, pts, Linestyle=2
   ENDFOR
   
   ; Create a blank space for writing axis labels.
   cgColorFill, [-maxFreq,-maxFreq, maxFreq, maxFreq, -maxFreq], [0,2.0,2.0,0,0],Color='white'
   
   ; Draw a vertical N-S line.
   cgPlots, [0,0], !Y.CRange, LineStyle=2
   
   ; Draw a horizontal E-W axis.
   cgAxis, 0, 0, /XAxis, XTickformat='cgWindRose_PositiveLabel', $
      XTicks=8, XRange=[-maxFreq, maxFreq], XStyle=1, Charsize=cgDefCharSize()*0.75
      
   ; Label the cardinal directions.
   cgText, !X.CRange[0]-1.75, 0.0, 'West ', Alignment=1.0
   cgText, !X.CRange[1]+1.75, 0.0, ' East', Alignment=0.0
   cgText, 0.525, !Y.Window[1]+ 0.025, 'North', Alignment=0.5, /Normal
   cgText, 0.525, !Y.Window[0]-0.040, 'South', Alignment=0.5, /Normal
   
   ; Draw the wind triangles.
   angles = (Indgen(16)*22.5)
   colors = Bindgen(s[0])
   cgLoadCT, 1, /Brewer, NColors=s[0], Clip=[30, 250]
   ;colors = ['pink', 'dark khaki', 'red', 'blu7', 'grn5', 'sky blue', 'purple']
   FOR j=0,15 DO BEGIN
      FOR k=s[0]-1,0,-1 DO BEGIN
         radii = Total(matrix[0:k,*],1) / N_Elements(speed_final) * 100
         radius = radii[j]
         xr1 = [ 0, radius * Sin((angles[j]-10)*!DtoR)]
         yr1 = [ 0, radius * Cos((angles[j]-10)*!DtoR)]
         xr2 = [ 0, radius * Sin((angles[j]+10)*!DtoR)]
         yr2 = [ 0, radius * Cos((angles[j]+10)*!DtoR)]
         pts = cgWindroseArc(0, 0, radius, (angles[j]-10), (angles[j]+10))
         xr3 = [xr1, Reform(pts[0,*]), xr2]
         yr3 = [yr1, Reform(pts[1,*]), yr2]
         cgColorFill, xr3, yr3, Color=colors[k]
         cgPolygon, xr3, yr3, Color='black'
      ENDFOR
   ENDFOR
   
   ; Put a circle in the center of the plot.
   TVCircle, 0.5, 0, 0, /Data, Color='white', /Fill
 
   ; Add a legend.
   speeds = Indgen(s[0]-1) * speedBinSize + 1
   sym = cgCheckForSymbols('$\geq$' + String(speeds[N_Elements(speeds)-1], Format='(F0.1)'))
   speedStr = StrArr(N_Elements(speeds))
   speedStr[0] = '1.0-' + String(speeds[1], Format='(F0.1)')
   FOR j=2,N_Elements(speeds)-1 DO BEGIN
      speedStr[j-1] = String(speeds[j-1], Format='(F0.1)') + '-' + String(speeds[j], Format='(F0.1)')
   ENDFOR
   speedStr[N_Elements(speeds)-1] = sym
   
   AL_Legend, speedStr, PSym=Replicate(15,N_Elements(speeds)), SymSize=1.5, Color=colors, $
       Position=[0.05, 0.22], /Normal, Spacing=1.5, Charsize=cgDefCharsize()*0.75
   cgText, 0.045, 0.24, /Normal, 'Wind Speed (m/s)', Charsize=cgDefCharsize()*0.85
   
   ; Add a title to the plot.
   cgText, 0.045, 0.90, /Normal, title
   
 END
 