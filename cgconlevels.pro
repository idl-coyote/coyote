; docformat = 'rst'
;
; NAME:
;   cgConLevels
;
; PURPOSE:
;   This program is designed to create "nicely formatted" contour levels for use in
;   contour plots. The idea is to be able to produce evenly spaced contour intervals
;   with the contour levels rounded off to the preferred degree of accuracy. The program
;   will make a "guess" as to how to do this, but users can also take control of the
;   process if the results are not pleasing enough.
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
; This program is designed to create "nicely formatted" contour levels for use in
; contour plots. The idea is to be able to produce evenly spaced contour intervals
; with the contour levels rounded off to the preferred degree of accuracy. The program
; will make a "guess" as to how to do this, but users can also take control of the
; process if the results are not pleasing enough.
; 
; There is no claim that this program always produces the best results. It is more
; of a tool that can produce decent result in many situations.
; 
; The program requires the `Coyote Library <http://www.idlcoyote.com/documents/programs.php>`
; to be installed on your machine.
;
; :Categories:
;    Graphics, Utilities
;    
; :Returns:
;     A vector of contour levels of the requested number. Each level is
;     rounded to a predefined or specified degree of accuracy.
;    
; :Params:
;     data: in, required 
;         The data for which contour levels are desired.
;          
; :Keywords:
;     factor: in, optional, type=float
;         The program makes a "guess" at how to best round the levels in the data
;         presented to it. This guess is not always what the user wants. This keyword
;         allows the user to "fine tune" the guess, so that it behaves better. See the
;         examples for details on how this keyword can be used. There is some danger,
;         when using the factor keyword that the results will be terrible. Don't dismay!
;         Just try something else.
;     maxvalue: in, optional
;         Normally, the levels are calculated using the minimum and maximum values 
;         of the input data. The maximum value used in the calculation can be replaced
;         with this value, if you wish. If both the `MinValue` and MaxValue keywords
;         are used, you do not need to pass the data parameter.
;     minmax: out, optional
;         The actual minimum and maximum values used to calculate the levels.
;     minvalue: in, optional
;         Normally, the levels are calculated using the minimum and maximum values 
;         of the input data. The minimum value used in the calculation can be replaced
;         with this value, if you wish.
;     nlevels: in, optional, type=integer, default=10
;         The number of contour levels desired.
;     silent: in, optional, type=boolean, default=0
;         Set this keyword if you want the program to remain "silent" in the face of 
;         errors. If this keyword is set, the user should rely on the `Success` keyword
;         to determine whether the program completed its work.
;     step: out, optional
;         The step size actually used in the program to calculate the levels.
;     success: out, optional, type=boolen
;         Upon return, will contain a 1 if the program executed succesfully and
;         a 0 otherwise.
;          
; :Examples:
;    Here is the normal way a contour plot might be created::
;
;        cgDisplay, WID=0
;        data = Scale_Vector(cgDemoData(2), 0.1, 4534.5)
;        cgLoadCT, 33, NColors=10, Bottom=1
;        cgContour, data, NLevels=10, /Fill, /Outline, $
;           Position=[0.1, 0.1, 0.9, 0.75], C_Colors=Indgen(10)+1
;        cgColorbar, NColors=9, Bottom=1, /Discrete, /Fit, $
;           Range=[Min(data), Max(data)], OOB_High=10, OOB_Low='white'
;           
;    Here is how the same plot might be created with cgConLevels
;    to produce contour levels at 500 step intervals::
;    
;        cgDisplay, WID=1
;        data = Scale_Vector(cgDemoData(2), 0.1, 4534.5)
;        cgLoadCT, 33, NColors=10, Bottom=1
;        levels = cgConLevels(data, Factor=100, MINVALUE=0)
;        cgContour, data, Levels=levels, /Fill, /Outline, $
;           Position=[0.1, 0.1, 0.9, 0.75], C_Colors=Indgen(10)+1
;        cgColorbar, NColors=9, Bottom=1, /Discrete, /Fit, $
;           Range=[Min(levels), Max(levels)], OOB_High=10, OOB_Low='white'
;           
;    In this example, the data is scaled so that it is a bit more perverse.
;    The levels have been chosen so they round in the third decimal place::
;    
;        cgDisplay, WID=2
;        data = Scale_Vector(cgDemoData(2), 0.153, 0.986)
;        cgLoadCT, 33, NColors=10, Bottom=1
;        levels = cgConLevels(data)
;        cgContour, data, Levels=levels, /Fill, /Outline, $
;           Position=[0.1, 0.1, 0.9, 0.75], C_Colors=Indgen(10)+1
;        cgColorbar, NColors=9, Bottom=1, /Discrete, /Fit, $
;           Range=[Min(levels), Max(levels)], OOB_High=10, OOB_Low='white'
;    
;    It might be better to have the data rounded in the second data place, to
;    the nearest 0.05 value. This can be done with the `Factor` keyword::
;     
;        cgDisplay, WID=3
;        data = Scale_Vector(cgDemoData(2), 0.153, 0.986)
;        cgLoadCT, 33, NColors=10, Bottom=1
;        levels = cgConLevels(data, Factor=0.05)
;        cgContour, data, Levels=levels, /Fill, /Outline, $
;           Position=[0.1, 0.1, 0.9, 0.75], C_Colors=Indgen(10)+1
;        cgColorbar, NColors=9, Bottom=1, /Discrete, /Fit, $
;           Range=[Min(levels), Max(levels)], OOB_High=10, OOB_Low='white'
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
;        Written, 8 December 2011. David W. Fanning
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION cgConLevels, data, $
    FACTOR=factor, $
    MAXVALUE=maxvalue, $
    MINMAX=minmax, $
    MINVALUE=minvalue, $
    NLEVELS=nlevels, $
    SILENT=silent, $
    STEP=step, $
    SUCCESS=success

    Compile_Opt idl2
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF ~Keyword_Set(silent) THEN void = Error_Message()
        success = 0
        RETURN, -1
    ENDIF

    ; Assume success.
    success = 1
    
    ; Data is required, unless both MINVALUE and MAXVALUE are used.
    IF N_Elements(data) EQ 0 THEN BEGIN
        IF ~( (N_Elements(maxvalue)NE 0) && (N_Elements(minvalue) NE 0) ) THEN $ 
           Message, 'Data must be passed to calculate contour levels.'
    ENDIF
    
    ; Set default number of levels to 10.
    SetDefaultValue, nlevels, 10
    
    ; Find the step.
    IF N_Elements(data) NE 0 THEN mindata = Min(data, /NAN, MAX=maxdata)
    IF N_Elements(minvalue) NE 0 THEN mindata = minvalue
    IF N_Elements(maxvalue) NE 0 THEN maxdata = maxvalue 
    step  = (maxdata - mindata) / nlevels

    ; We are going to work with abolute values. Save the signs for the end.
    minSign = (mindata LT 0) ? -1 : 1
    maxSign = (maxdata LT 0) ? -1 : 1
    minabs = Abs(mindata)
    maxabs = Abs(maxdata)
    
    ; If you have a factor supplied by the user, then use it. Otherwise, try to figure
    ; out which factor you might use.
    IF N_Elements(factor) EQ 0 THEN BEGIN
        CASE 1 OF
            step GE 10000: BEGIN
               minabs = (minsign GT 0) ? Floor(minabs / 1000) * 1000. : Ceil(minabs / 1000) * 1000.
               maxabs = (maxsign GT 0) ? Ceil(maxabs / 1000) * 1000.  : Floor(maxabs / 1000) * 1000.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step / 1000) * 1000
               END
            (step GE 1000) && (step LT 10000): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs / 100) * 100. : Ceil(minabs / 100) * 100.
               maxabs = (maxsign GT 0) ? Ceil(maxabs / 100) * 100. : Floor(maxabs / 100) * 100.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step / 100) * 100.
               END
            (step GE 100) && (step LT 1000): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs / 10) * 10. : Ceil(minabs / 10) * 10.
               maxabs = (maxsign GT 0) ? Ceil(maxabs / 10) * 10. : Floor(maxabs / 10) * 10.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step / 10) * 10.
               END
            (step GE 10) && (step LT 100): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs) : Ceil(minabs)
               maxabs = (maxsign GT 0) ? Ceil(maxabs)  : Floor(maxabs)
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Float(Round(step))
               END
            (step GE 1) && (step LT 10): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs * 10) / 10. : Ceil(minabs * 10) / 10.
               maxabs = (maxsign GT 0) ? Ceil(maxabs * 10) / 10.  : Floor(maxabs * 10) / 10.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step * 10) / 10.
               END
            (step GE 0.1) && (step LT 1): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs * 100) / 100. : Ceil(minabs * 100) / 100.
               maxabs = (maxsign GT 0) ? Ceil(maxabs * 100) / 100.  : Floor(maxabs * 100) / 100.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step * 100) / 100.
               END
            (step GE 0.01) && (step LT 0.1): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs * 1000) / 1000. : Ceil(minabs * 1000) / 1000.
               maxabs = (maxsign GT 0) ? Ceil(maxabs * 1000) / 1000.  : Floor(maxabs * 1000) / 1000.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step * 1000) / 1000.
               END
            (step GE 0.001) && (step LT 0.01): BEGIN
               minabs = (minsign GT 0) ? Floor(minabs * 10000) / 10000. : Ceil(minabs * 10000) / 100000.
               maxabs = (maxsign GT 0) ? Ceil(maxabs * 10000) / 10000.  : Floor(maxabs * 10000) / 100000.
               mindata = (minsign GT 0) ? minabs : minabs * (-1)
               maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
               step  = (maxdata - mindata) / nlevels
               step = Round(step * 10000) / 10000.
               END
             ELSE:    
        ENDCASE
    ENDIF ELSE BEGIN
        minabs = (minsign GT 0) ? Floor(minabs / factor) * Float(factor)  : Ceil(minabs / factor) * Float(factor)
        maxabs = (maxsign GT 0) ? Ceil(maxabs / factor)  * Float(factor) : Floor(maxabs / factor) * Float(factor)
        mindata = (minsign GT 0) ? minabs : minabs * (-1)
        maxdata = (maxsign GT 0) ? maxabs : maxabs * (-1)
        step  = (maxdata - mindata) / nlevels
        step = Round(step / factor) * Float(factor)
    ENDELSE
    
    ; Calculate the levels so they can be returned.
    levels = Indgen(nlevels) * step + mindata

    ; Set up the output variable, minmax, with the actual min and max values used for the levels.
    minmax = [mindata, maxdata]
    
    RETURN, levels
END