;+
; NAME:
;       LOADDATA
;
; PURPOSE:
;
;       The purpose of this function is to read a selection of standard
;       data sets that are found in the normal IDL distribution in the
;       subdirectory $IDL_DIR/examples/data. At least 17 data sets are
;       available in all categories of data. The user selects one of the
;       possible data sets with the mouse.
;
; CATEGORY:
;
;       File I/O.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CALLING SEQUENCE:
;
;       If calling from the IDL command line:
;
;          data = LoadData()
;
;       If calling from within a widget program:
;
;          data = LoadData(Cancel=cancelled, Group_Leader=event.top)
;
;       If you know which data set you want, you can load it directly:
;
;          data = LoadData(7)
;
; OPTIONAL INPUTS:
;
;       selection : The number of the data selection. Values start at 1,
;           and go up to the number of data sets available (currently 17).
;
; KEYWORD PARAMETERS:
;
;       CANCEL : An output keyword that is 1 of the use clicked the CANCEL
;           button and 0 otherwise.
;
;              data = Loaddata(Cancel=cancelled)
;              IF cancelled THEN RETURN
;
;        GROUP_LEADER: The group leader of the widget. This keyword
;           is required if you wish LOADDATA to be a modal widget program.
;           (Which you *always* do when calling it from a widget program.)
;
;        IMAGES: Set this keyword if you only want to select 2D image
;           data sets. Note that the selection number does *not* change
;           just because fewer data sets are available in the selection
;           widget.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;
;       To load the world elevation data set:
;
;       image = LoadData(7)
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 5 March 1999.
;       Added some additonal random data capability. 29 April 99. DWF
;       Added IMAGES keyword. 31 March 2000. DWF.
;       Fixed a problem with the CANCEL button. 25 Oct 2002. DWF.
;       Added new JPEG, DICOM, TIFF, and PGN images. 30 Oct 2002. DWF.
;       Modified old program units to work with IDL strict arrays. 29 June 2003. DWF
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
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

    function smooth2, i, w, help=hlp

    if (n_params(0) lt 2) or keyword_set(hlp)  then begin
      print,' Do multiple smoothing. Gives near Gaussian smoothing.'
      print,' b = smooth2(a, w)'
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

    function makez, nx, ny, w, seed=seed0, lastseed=lseed, $
      periodic=per, help=hlp

        common makez_com, seed
    ;-----------------------------------------------------------------
    ;   Must store seed in common otherwise it is undefined
    ;   on entry each time and gets set by the clock but only
    ;   to a one second resolution (same random data for a whole sec).
    ;-----------------------------------------------------------------

    if keyword_set(hlp) then begin
      print,' Make simulated 2-d data.  Useful for software development.'
      print,' data = makez( nx, ny, w)'
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
    s = smooth2(r,w)        ; Smooth.
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


    function makey, n, w, seed=seed0, lastseed=lseed, $
      periodic=per, help=hlp

        common makey_com, seed
    ;-----------------------------------------------------------------
    ;   Must store seed in common otherwise it is undefined
    ;   on entry each time and gets set by the clock but only
    ;   to a one second resolution (same random data for a whole sec).
    ;-----------------------------------------------------------------

    if keyword_set(hlp) then begin
      print,' Make simulated data.  Useful for software development.'
      print,' data = makey( n, w)'
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
    s = smooth2(r,w)        ; Smooth.
    s = s[lo:hi]            ; Trim edge effects.
    s = s - min(s)          ; Normalize.
    s = s/max(s)

    lseed = seed            ; Return last seed.

    return, s
    end


FUNCTION LoadData_ReadData, number

COMMON LOADDATA_SEED, seed

; Read a data set in the $IDL_DIR/examples/data subdirectory.

IF N_Elements(number) EQ 0 THEN BEGIN
   ok = Dialog_Message("Data set index number is required in LOADDATA.")
   RETURN, -1
ENDIF

CASE number OF

   0: BEGIN
      data = MAKEY(101, 5, Seed=1L) * 30.0
      END

   1: BEGIN
      data = MAKEZ(41, 41, 8, Seed=-2L)  * 1550
      END

   2: BEGIN
      data = MAKEZ(41, 41, 10, Seed=-5L)
      data = BYTSCL(data, Top=!D.Table_Size-1)
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
     data = Scale_Vector(MAKEY(101, 5, Seed=seed) * scale[0], 0, 100)
     END

 17: BEGIN

        ; Random 400 by 400 array.

     data = MAKEZ(400, 400, 41, Seed=seed)
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

ELSE: ok = Dialog_Message("No data set with that index number. Sorry.")

ENDCASE

RETURN, data
END
;------------------------------------------------------------------------------



PRO LoadData_CenterTLB, tlb

Device, Get_Screen_Size=screenSize
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;; CenterTLB

PRO LOADDATA_CANCEL, event
WIDGET_CONTROL, event.top, /Destroy
END
;------------------------------------------------------------------



PRO LOADDATA_EVENT, event
WIDGET_CONTROL, event.top, GET_UVALUE=ptr
WIDGET_CONTROL, event.id, GET_UVALUE=indexValue
*ptr = LoadData_ReadData(indexValue(event.index))
WIDGET_CONTROL, event.top, /DESTROY
END
;------------------------------------------------------------------



FUNCTION LOADDATA, CANCEL=cancel, number, Group_Leader=groupleader, $
   Images=images

On_Error, 1

; Function to read and return training data sets.

COMMON LOADDATA_SEED, seed

   ; If a parameter is passed in, read that data set and return.

IF N_Params() EQ 1 THEN BEGIN
   type = Size(number)
   type = type( type(0) + 1 )
   IF type EQ 0 THEN Message, 'Supplied argument is undefined.'
   IF type GT 5 THEN Message, 'Supplied argument must be a number.'
   number = number - 1
   number = 0 > number < 24
   data = LoadData_ReadData(number)
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
            '25. Mineral Micrograph (PNG 288-by-216 2D BYTE array)']



   indexValue = IndGen(25)
   listsize = 25
   title = 'Select Data Set...'
ENDELSE

IF Keyword_Set(groupleader) THEN BEGIN
   tlb = WIDGET_BASE(TITLE=title, COLUMN=1, $
      GROUP_LEADER=groupleader, /MODAL)
ENDIF ELSE BEGIN
   tlb = WIDGET_BASE(TITLE=title, COLUMN=1)
ENDELSE

list = WIDGET_LIST(tlb, VALUE=value, SCR_XSIZE=400, YSIZE=listsize, UValue=indexValue)
button = WIDGET_BUTTON(tlb, VALUE='Cancel', EVENT_PRO='LOADDATA_CANCEL')
LoadData_CenterTLB, tlb
WIDGET_CONTROL, tlb, /REALIZE

   ; Create a pointer to store the data.

ptr = Ptr_New(-1)
WIDGET_CONTROL, tlb, SET_UVALUE=ptr


XMANAGER, 'loaddata', tlb

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
