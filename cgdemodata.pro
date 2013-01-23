; docformat = 'rst'
;
; NAME:
;   cgDemoData
;
; PURPOSE:
;   The purpose of this function is to read a selection of standard
;   data sets that are found in the normal IDL distribution in the
;   subdirectory $IDL_DIR/examples/data. At least 25 data sets are
;   available in all categories of data. The user selects one of the
;   possible data sets with the mouse. Several of the data sets 
;   simply create data used to illustrate graphics commands (numbers 1-3), 
;   and two of the data sets produce random data. Data set 17 produces 
;   a random vector of 101 elements. Data set 18 produces a 2D array of 
;   random values, suitable for display as a contour plot or image.
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
;   The purpose of this function is to read a selection of standard
;   data sets that are found in the normal IDL distribution in the
;   subdirectory $IDL_DIR/examples/data. At least 25 data sets are
;   available in all categories of data. The user selects one of the
;   possible data sets with the mouse. Several of the data sets 
;   simply create data used to illustrate graphics commands (numbers 1-3), 
;   and two of the data sets produce random data. Data set 17 produces 
;   a random vector of 101 elements. Data set 18 produces a 2D array of 
;   random values, suitable for display as a contour plot or image.
;
; :Categories:
;    Utilities
;    
; :Examples:
;       To load the world elevation data set::
;
;           image = cgDemoData(7)
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
;       Written by:  David W. Fanning, 5 March 1999.
;       Added some additonal random data capability. 29 April 99. DWF
;       Added IMAGES keyword. 31 March 2000. DWF.
;       Fixed a problem with the CANCEL button. 25 Oct 2002. DWF.
;       Added new JPEG, DICOM, TIFF, and PGN images. 30 Oct 2002. DWF.
;       Modified old program units to work with IDL strict arrays. 29 June 2003. DWF
;       Yikes! Somehow lost the event handler for the CANCEL button! Fixed. 1 Jan 2012. DWF.
;       Added 2D Combined Gaussian data set. Renamed utility routines. 22 Jan 2013. DWF.
;
; :Copyright:
;     Copyright (c) 2011-2013, Fanning Software Consulting, Inc.
;-
;
; NAME:
;       SMOOTH2
; PURPOSE:
;       Do multiple smoothing. Gives near Gaussian smoothing.
; CATEGORY:
; CALLING SEQUENCE:
;       b = smooth2(a, w)
; INPUTS:
;       a = array to smooth (1,2, or 3-d).  in
;       w = smoothing window size.          in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       b = smoothed array.                 out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner.  8 Jan, 1987.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES  14 Jan, 1987 --- made both 2-d and 1-d.
;       RES 30 Aug, 1989 --- converted to SUN.
;       R. Sterner, 1994 Feb 22 --- cleaned up some.
;
; Copyright (C) 1987, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;
;-------------------------------------------------------------
;+
; Does multiple smoothing. A routine from the JHUAPL IDL Library.
; 
; :Params:
;    i: in, required
;        The input array to be smoothed.
;    w: in, required, type=integer
;        The size of the smoothing window. Forced to be an odd integer. 
; :Keywords:
;    help: in, optional, type=boolean, default=0
;        Set this keyword to print function syntax help.
;-
    function cgdemodata_smooth2, i, w, help=hlp

    if (n_params(0) lt 2) or keyword_set(hlp)  then begin
      print,' Do multiple smoothing. Gives near Gaussian smoothing.'
      print,' b = cgdemodata_smooth2(a, w)'
      print,'   a = array to smooth (1,2, or 3-d).  in'
      print,'   w = smoothing window size.          in'
      print,'   b = smoothed array.                 out'
      return, -1
    endif

    w1 = w > 2
    w2 = w/2 > 2

    i2 = smooth(i, w1)
    i2 = smooth(i2, w1)
    i2 = smooth(i2, w2)
    i2 = smooth(i2, w2)

    return, i2
    end


;-------------------------------------------------------------
;
; NAME:
;       MAKEZ
; PURPOSE:
;       Make simulated 2-d data.  Useful for software development.
; CATEGORY:
; CALLING SEQUENCE:
;       data = makez( nx, ny, w)
; INPUTS:
;       nx, ny = size of 2-d array to make.                   in
;       w = smoothing window size (def = 5% of sqrt(nx*ny)).  in
; KEYWORD PARAMETERS:
;       Keywords:
;         /PERIODIC   forces data to match at ends.  Will not work
;           for smoothing windows much more than about 30% of n.
;         SEED=s      Set random seed for repeatable results.
;         LASTSEED=s  returns last random seed used.
; OUTPUTS:
;       data = resulting data array (def = undef).            out
; COMMON BLOCKS:
;       makez_com
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner,  29 Nov, 1986.
;       R. Sterner,  1994 Feb 22 --- Rewrote from new makey.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;
;-------------------------------------------------------------
;+
; Make simulated 2-d data. A routine from the JHUAPL IDL Library.
; 
; :Params:
;    nx: in, required
;        The X size of the data to be created.
;    ny: in, required, type=integer
;        The Y size of the data to be created.
;    w: in, required, type=integer
;        The smoothing window size.
; :Keywords:
;    help: in, optional, type=boolean, default=0
;        Set this keyword to print function syntax help.
;    lastseed: out, optional
;        Returns the last random seed used.
;    periodic: in, optional, type=boolean, default=0
;        Set this keyword to force the data to match at either end.
;    seed: in, optional
;        Sets the random seed for repeatable results.
;-
    function cgdemodata_makez, nx, ny, w, help=hlp, lastseed=lseed, periodic=per, seed=seed0
      

        common cgdemodata_makez_com, seed
    ;-----------------------------------------------------------------
    ;   Must store seed in common otherwise it is undefined
    ;   on entry each time and gets set by the clock but only
    ;   to a one second resolution (same random data for a whole sec).
    ;-----------------------------------------------------------------

    if keyword_set(hlp) then begin
      print,' Make simulated 2-d data.  Useful for software development.'
      print,' data = cgdemodata_makez( nx, ny, w)'
      print,'   nx, ny = size of 2-d array to make.                   in'
      print,'   w = smoothing window size (def = 5% of sqrt(nx*ny)).  in'
      print,'   data = resulting data array (def = undef).            out'
      print,' Keywords:'
      print,'   /PERIODIC   forces data to match at ends.  Will not work'
      print,'     for smoothing windows much more than about 30% of n.'
      print,'   SEED=s      Set random seed for repeatable results.'
      print,'   LASTSEED=s  returns last random seed used.'
      return, -1
    endif

    ;-----  Return last random seed or set new  -----
    if n_elements(seed) ne 0 then lseed=seed else lseed=-1
    if n_elements(nx) eq 0 then return,0            ; That was all.
        if n_elements(seed0) ne 0 then seed = seed0

    ;-----  Default smoothing window size  ---------
    if n_elements(w) eq 0 then w = .05*sqrt(long(nx)*ny) > 5

    ;-----  Compute size of edge effect  --------
    lo = fix(w)/2 + fix(w)  ; First index after edge effects.
    ntx = nx + 2*lo     ; X size extended to include edge effects.
    nty = ny + 2*lo     ; Y size extended to include edge effects.
    hix = ntx - 1 - lo  ; Last X index before edge effects.
    hiy = nty - 1 - lo  ; Last Y index before edge effects.

    ;-----  Make data  ---------------------------
    r = randomu(seed, ntx, nty) ; Random starting data.
    seed0 = seed            ; Return new seed.
    if keyword_set(per) then begin  ; Want periodic data.
      r[ntx-2*lo,0] = r[0:2*lo-1,*] ; Copy part of random data.
      r[0,nty-2*lo] = r[*,0:2*lo-1]
    endif
    s = cgdemodata_smooth2(r,w)        ; Smooth.
    s = s[lo:hix, lo:hiy]       ; Trim edge effects.
    s = s - min(s)          ; Normalize.
    s = s/max(s)

    lseed = seed            ; Return last seed.

    return, s
    end
;-------------------------------------------------------------
;
; NAME:
;       MAKEY
; PURPOSE:
;       Make simulated data.  Useful for software development.
; CATEGORY:
; CALLING SEQUENCE:
;       data = makey( n, w)
; INPUTS:
;       n = number of data values to make.                in
;       w = smoothing window size (def = 5% of n).        in
; KEYWORD PARAMETERS:
;       Keywords:
;         /PERIODIC   forces data to match at ends.  Will not work
;           for smoothing windows much more than about 30% of n.
;         SEED=s      Set random seed for repeatable results.
;         LASTSEED=s  returns last random seed used.
; OUTPUTS:
;       data = resulting data array (def = undef).        out
; COMMON BLOCKS:
;       makey_com
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner,  2 Apr, 1986.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES 21 Nov, 1988 --- added SEED.
;       R. Sterner, 2 Feb, 1990 --- added periodic.
;       R. Sterner, 29 Jan, 1991 --- renamed from makedata.pro.
;       R. Sterner, 24 Sep, 1992 --- Added /NORMALIZE.
;       R. Sterner, 1994 Feb 22 --- Greatly simplified.  Always normalize.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;
;-------------------------------------------------------------
;+
; Make simulated data vector. A routine from the JHUAPL IDL Library.
; 
; :Params:
;    n: in, required
;        The size of the data vector to be created.
;    w: in, required, type=integer
;        The smoothing window size.
; :Keywords:
;    help: in, optional, type=boolean, default=0
;        Set this keyword to print function syntax help.
;    lastseed: out, optional
;        Returns the last random seed used.
;    periodic: in, optional, type=boolean, default=0
;        Set this keyword to force the data to match at either end.
;    seed: in, optional
;        Sets the random seed for repeatable results.
;-
    function cgdemodata_makey, n, w, seed=seed0, lastseed=lseed, $
      periodic=per, help=hlp

        common cgdemodata_makey_com, seed
    ;-----------------------------------------------------------------
    ;   Must store seed in common otherwise it is undefined
    ;   on entry each time and gets set by the clock but only
    ;   to a one second resolution (same random data for a whole sec).
    ;-----------------------------------------------------------------

    if keyword_set(hlp) then begin
      print,' Make simulated data.  Useful for software development.'
      print,' data = cgdemodata_makey( n, w)'
      print,'   n = number of data values to make.                in'
      print,'   w = smoothing window size (def = 5% of n).        in'
      print,'   data = resulting data array (def = undef).        out'
      print,' Keywords:'
      print,'   /PERIODIC   forces data to match at ends.  Will not work'
      print,'     for smoothing windows much more than about 30% of n.'
      print,'   SEED=s      Set random seed for repeatable results.'
      print,'   LASTSEED=s  returns last random seed used.'
      return, -1
    endif

    ;-----  Return last random seed or set new  -----
    if n_elements(seed) ne 0 then lseed=seed else lseed=-1
    if n_elements(n) eq 0 then return,0 ; That was all.
        if n_elements(seed0) ne 0 then seed = seed0

    ;-----  Default smoothing window size  ---------
    if n_elements(w) eq 0 then w = .05*n > 5

    ;-----  Compute size of edge effect  --------
    lo = long(w)/2L + long(w)   ; First index after edge effects.
    nt = n + 2*lo           ; Size extended to include edge effects.
    hi = nt - 1 - lo        ; Last index before edge effects.

    ;-----  Make data  ---------------------------
    r = randomu(seed, nt)       ; Random starting data.
    seed0 = seed            ; Return new seed.
    if keyword_set(per) then begin  ; Want periodic data.
      r[nt-2*lo] = r[0:2*lo-1]  ; Copy part of random data.
    endif
    s = cgdemodata_smooth2(r,w)        ; Smooth.
    s = s[lo:hi]            ; Trim edge effects.
    s = s - min(s)          ; Normalize.
    s = s/max(s)

    lseed = seed            ; Return last seed.

    return, s
    end

;
; NAME: 
;  gauss2d
; PURPOSE: 
;  Compute a two dimensional gaussian within an array.
; DESCRIPTION:
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  pro gauss2d,nx,ny,x,y,fwhm,array
; INPUTS:
;  nx   - X size of output array
;  ny   - Y size of output array
;  x    - X location of gaussian in array
;  y    - Y location of gaussian in array
;  fwhm - Full width at half-maximum of gaussian.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  array - Result array with gaussian inserted.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/04/07, Written by Marc W. Buie, Lowell Observatory
;
;+
; Creates a two-dimensions Gaussian data set.
;
; :Params:
;     nx: in, required, type=integer
;        The X size of the output array.
;     ny: in, required, type=integer
;        The Y size of the output array.
;     x: in, required, type=integer
;        The X location of the Gaussian in the array.   
;     y: in, required, type=integer
;        The Y location of the Gaussian in the array.   
;     fwhm: in, required, type=float
;        The full width at half-maximum of Gaussian.
;     array: out, optional, type=float
;        The output array containing the Gaussian.
;-
pro cgdemodata_gauss2d,nx,ny,x,y,fwhm,array

   ehwd = fwhm/2.0/sqrt(alog(2.0))

   ix = findgen(nx)
   iy = findgen(ny)
   onex = replicate(1.0,nx)
   oney = replicate(1.0,ny)

   xarr = ((ix-x)/ehwd)^2 # oney
   yarr = onex # ((iy-y)/ehwd)^2

   rsq = xarr + yarr
   array = fltarr(nx,ny)

   ; Protection against underflow in exp call.
   big = where(rsq le 87.3, count)
   if count ne 0 then array[big] = exp(-rsq[big])

end


;+
; This event handler responds to the CANCEL button.
; 
; :Params:
;    event: in, required, type=structure
;        The event structure passed by the window manager from the CANCEL button.
;-
PRO cgDemoData_Cancel, event
    Widget_Control, event.top, /Destroy
END



;+
; Reads the data set from its current location.
; 
; :Params:
;    number: in, required, type=integer
;        The selection number.
;-
FUNCTION cgDemoData_ReadData, number

COMMON cgDemoData_SEED, seed

; Read a data set in the $IDL_DIR/examples/data subdirectory.

IF N_Elements(number) EQ 0 THEN BEGIN
   ok = Dialog_Message("Data set index number is required in cgDemoData.")
   RETURN, -1
ENDIF

CASE number OF

   0: BEGIN
      data = cgdemodata_makey(101, 5, Seed=1L) * 30.0
      END

   1: BEGIN
      data = cgdemodata_makez(41, 41, 8, Seed=-2L)  * 1550
      END

   2: BEGIN
      data = cgdemodata_makez(41, 41, 10, Seed=-5L)
      data = Scale_Vector(data, 0, 60)
      END

   3: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'galaxy.dat')
      data = BYTARR(256, 256)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   4: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'ctscan.dat')
      data = BYTARR(256, 256)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   5: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'abnorm.dat')
      data = BYTARR(64, 64, 15)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   6: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'worldelv.dat')
      data = BYTARR(360,360)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   7: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'head.dat')
      data = BYTARR(80, 100, 57)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   8: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'cereb.dat')
      data = BYTARR(512, 512)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

   9: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'people.dat')
      data = BYTARR(192, 192, 2)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

  10: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'convec.dat')
      data = BYTARR(248, 248)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

  11: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'm51.dat')
      data = BYTARR(340, 440)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

  12: BEGIN
      file = FILEPATH(SUBDIR=['examples', 'data'], 'hurric.dat')
      data = BYTARR(440, 340)
      OPENR, lun, file, /GET_LUN
      READU, lun, data
      FREE_LUN, lun
      END

 13: BEGIN

        ; Create randomly-distributed (xyz) data.

      seed = -1L
      x = RANDOMU(seed, 41)
      y = RANDOMU(seed, 41)
      distribution = SHIFT(DIST(41,41), 25, 15)
      distribution = EXP(-(distribution/15)^2)

      lat = x * (24./1.0) + 24
      lon = y * 50.0/1.0 - 122
      temp = distribution(x*41, y*41) * 273
      data = FLTARR(3, 41)
      data(0,*) = lon
      data(1,*) = lat
      data(2,*) = temp
      END

 14: BEGIN

        ; Create randomly-distributed (xyz) data.

      seed = -1L
      x = RANDOMU(seed, 41)
      y = RANDOMU(seed, 41)
      distribution = SHIFT(DIST(41,41), 25, 15)
      distribution = EXP(-(distribution/15)^2)

      lat = x * (24./1.0) + 24
      lon = y * 50.0/1.0 - 122
      temp = distribution(x*41, y*41) * 273
      data = {lat:lat, lon:lon, temp:temp}
      END

 15: BEGIN

        ; The ROSE data set.

     file = FILEPATH(SUBDIR=['examples', 'data'], 'rose.jpg')
     Read_JPEG, file, data
     END

 16: BEGIN

        ; Random 1D vector of 101 elements.

     scale = RandomU(seed, 1) * 100
     data = Scale_Vector(cgdemodata_makey(101, 5, Seed=seed) * scale[0], 0, 100)
     END

 17: BEGIN

        ; Random 401 by 401 array.

     data = cgdemodata_makez(401, 401, 41, Seed=seed)
     data = Hist_Equal(data)
     data = BytScl(data, Top=!D.Table_Size-1)


     END

 18: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'elev_t.jpg')
     Read_JPEG, file, data
     END

 19: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'endocell.jpg')
     Read_JPEG, file, data
     END

 20: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'rbcells.jpg')
     Read_JPEG, file, data
     END

 21: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'muscle.jpg')
     Read_JPEG, file, data
     END

 22: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'image.tif')
     data = Read_TIFF(file)
     END


 23: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'mr_knee.dcm')
     data = Read_DICOM(file)
     data = Reverse(data,2)
     END

 24: BEGIN
     file = FILEPATH(SUBDIR=['examples', 'data'], 'mineral.png')
     data = Read_PNG(file)
     END
     
25: BEGIN ; 2D Gaussian.
    cgDemoData_Gauss2D, 101, 101, 70, 70, 20., array1
    cgDemoData_Gauss2D, 101, 101, 20, 20, 90., array2
    data = (array1 + array2) * 10.
    END


ELSE: ok = Dialog_Message("No data set with that index number. Sorry.")

ENDCASE

RETURN, data
END
;------------------------------------------------------------------------------



;+
; Reads the data set from its current location.
; 
; :Params:
;    event: in, required, type=structure
;        The event handler for the graphical user interface.
;-
PRO cgDemoData_EVENT, event
WIDGET_CONTROL, event.top, GET_UVALUE=ptr
WIDGET_CONTROL, event.id, GET_UVALUE=indexValue
*ptr = cgDemoData_ReadData(indexValue(event.index))
WIDGET_CONTROL, event.top, /DESTROY
END
;------------------------------------------------------------------


;+
; Select a dataset either interactively or by selection.
; 
; :Params:
; 
;    selection: in, optional, type=integer
;       The data set selection number. If not present, a graphical user
;       interface is presented to the user so that a data set may be
;       selected.
;       
; :Keywords:
;    cancel: out, optional, type=boolean
;       If a graphical user interface is used for the data selection, this output 
;       keyword is set to 1 if the user clicks the CANCEL button. Otherwise, it is 
;       set to 0.
;    group_leader: in, optional, type=long
;       The identifier of a widget group leader for this widget application. If the
;       group leader dies, this program will die as well.
;    images: in, optional, type=boolean, default=0
;       Set this keyword if you wish to see only 2D images in the selection widget.
;-
FUNCTION cgDemoData, selection, $
   CANCEL=cancel, $
   GROUP_LEADER=groupleader, $
   IMAGES=images

On_Error, 1

; Function to read and return training data sets.

COMMON cgDemoData_SEED, seed

   ; If a parameter is passed in, read that data set and return.

IF N_Params() EQ 1 THEN BEGIN
   type = Size(selection)
   type = type( type(0) + 1 )
   IF type EQ 0 THEN Message, 'Supplied argument is undefined.'
   IF type GT 5 THEN Message, 'Supplied argument must be a number.'
   selection = selection - 1
   selection = 0 > selection < 25
   data = cgDemoData_ReadData(selection)
   RETURN, data
ENDIF

IF Keyword_Set(images) THEN BEGIN

   value = ['Galaxy -- (BYTE 256-by-256 array)', $
            'CT Scan Thoracic Cavity -- (BYTE 256-by-256 array)', $
            'World Elevation Data -- (BYTE 360-by-360 array)', $
            'Brain X-Ray -- (BYTE 512-by-512 array)', $
            'Earth Mantle Convection -- (BYTE 248-by-248 array)', $
            'M51 Whirlpool Galaxy -- (BYTE 340-by-440 array)', $
            'Hurricane Gilbert -- (BYTE 440-by-340 array)', $
            'Random Image Array -- (BYTE 400-by-400)', $
            'Endocell (JPEG 615-by-416 2D BYTE array)', $
            'Red Blood Cells (JPEG 432-by-389 2D BYTE array)', $
            'Muscle (JPEG 652-by-444 2D BYTE array)', $
            'River Delta (TIFF 786-by-512 2D BYTE array)', $
            'MRI of Knee (DICOM 256-by-256 2D INT array)', $
            'Mineral Micrograph (PNG 288-by-216 2D BYTE array)']

   indexValue = [3,4,6,8,10,11,12,17,19,20,21,22,23,24]   ; The data sets.
   listsize = 14
   title = 'Select 2D Image...'

ENDIF ELSE BEGIN

   value = [' 1. Time Series Data (FLOAT 101 vector)', $
            ' 2. Elevation Data (FLOAT 41-by-41 array)', $
            ' 3. Snow Pack Data (FLOAT 41-by-41 array)', $
            ' 4. Galaxy (BYTE 256-by-256 array)', $
            ' 5. CT Scan Thoracic Cavity (BYTE 256-by-256 array)', $
            ' 6. Heart Gated Blood Pool Study (BYTE 64-by-64-by-15 array)', $
            ' 7. World Elevation Data (BYTE 360-by-360 array)', $
            ' 8. MRI Head Data (BYTE 80-by-100-by-57 array)', $
            ' 9. Brain X-Ray (BYTE 512-by-512 array)', $
            '10. Ali and Dave (192-by-192-by-2 array)', $
            '11. Earth Mantle Convection (BYTE 248-by-248 array)', $
            '12. M51 Whirlpool Galaxy (BYTE 340-by-440 array)', $
            '13. Hurricane Gilbert (BYTE 440-by-340 array)', $
            '14. Randomly Distributed (XYZ) Data (FLOAT 3-by-41 array)', $
            '15. Lat/Lon/Temperature Data Set (Structure)', $
            '16. Rose Data (24-bit JPEG image)', $
            '17. Random Data Vector (FLOAT 101 elements)', $
            '18. Random Image Array (BYTE 400-by-400)', $
            '19. Boulder Elevation Image (24-bit JPEG image)', $
            '20. Endocell (JPEG 615-by-416 2D BYTE array)', $
            '21. Red Blood Cells (JPEG 432-by-389 2D BYTE array)', $
            '22. Muscle (JPEG 652-by-444 2D BYTE array)', $
            '23. River Delta (TIFF 786-by-512 2D BYTE array)', $
            '24. MRI of Knee (DICOM 256-by-256 2D INT array)', $
            '25. Mineral Micrograph (PNG 288-by-216 2D BYTE array)', $
            '26. 2D Combined Gaussian (101-by-101 FLOAT array)']

   indexValue = IndGen(N_Elements(value))
   listsize = N_Elements(value)
   title = 'Select Data Set...'
ENDELSE

IF Keyword_Set(groupleader) THEN BEGIN
   tlb = WIDGET_BASE(TITLE=title, COLUMN=1, $
      GROUP_LEADER=groupleader, /MODAL)
ENDIF ELSE BEGIN
   tlb = WIDGET_BASE(TITLE=title, COLUMN=1)
ENDELSE

list = WIDGET_LIST(tlb, VALUE=value, SCR_XSIZE=400, YSIZE=listsize, UValue=indexValue)
button = WIDGET_BUTTON(tlb, VALUE='Cancel', EVENT_PRO='cgDemoData_CANCEL')
cgCenterTLB, tlb
WIDGET_CONTROL, tlb, /REALIZE

   ; Create a pointer to store the data.

ptr = Ptr_New(-1)
WIDGET_CONTROL, tlb, SET_UVALUE=ptr


XMANAGER, 'cgDemoData', tlb

   ; Get the data if it is there. If it is not there,
   ; user canceled or error occured.

data = *ptr
IF N_Elements(data) LE 1 THEN BEGIN
   cancel = 1
   Ptr_Free, ptr
   RETURN, -1
ENDIF ELSE BEGIN
   cancel = 0
   Ptr_Free, ptr
   RETURN, data
ENDELSE
END
