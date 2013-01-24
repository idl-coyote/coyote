;+
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
;-
pro gauss2d,nx,ny,x,y,fwhm,array

;   if badpar(nx,  [1,2,3,4,5],0,CALLER='gauss2d (nx)') then return
;   if badpar(ny,  [1,2,3,4,5],0,CALLER='gauss2d (ny)') then return
;   if badpar(x,   [1,2,3,4,5],0,CALLER='gauss2d (x)') then return
;   if badpar(y,   [1,2,3,4,5],0,CALLER='gauss2d (y)') then return
;   if badpar(fwhm,[1,2,3,4,5],0,CALLER='gauss2d (fwhm)') then return

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