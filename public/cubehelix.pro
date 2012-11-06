pro cubehelix,start=start,rots=rots,hue=hue,gamma=gamma,get=get,plot=plot,white=white
;+
; NAME: CUBEHELIX
;
; PURPOSE: Calculate a "cube helix" color table
;          Based on the FORTRAN 77 code provided in 
;          D.A. Green, 2011, BASI, 39, 289
;
;          http://adsabs.harvard.edu/abs/2011arXiv1108.5083G
;
; CALLING SEQUENCE:  CUBEHELIX,[start=start,rots=rots,hue=hue,gamma=gamma,
;                               get=get,/plot,/white]
;
; OPTIONAL INPUTS:  [this wording is taken from the paper]
;          START: color (1=red, 2=green, 3=blue)
;                 e.g.  0.5=purple
;                 DEFAULT = 0.5
;
;          ROTS:  rotations in colour (typically -1.5 to 1.5)
;                 DEFAULT = -1.5
;
;          HUE:   hue intensity scaling (in the range 0 (B+W) to 1
;                 to be strictly correct, larger values may be OK with
;                 particular star/end colours)
;                 DEFAULT = 1.2
;
;          GAMMA: set the gamma correction for intensity
;                 DEFAULT = 1.0
;
; KEYWORD PARAMETERS:
;          PLOT:  Have the program plot a color-bar to the screen,
;                 with colors 0->255 going left->right 
;
;          WHITE: let the last color be white
;
; OPTIONAL OUTPUTS:
;          GET:   Set this to a named vector which will have
;                 dimensions [256,3] to hold the RGB color vectors 
;
; EXAMPLE:
;    Create a color table of only greens and plot to the screen
;          IDL> cubehelix,/plot,gamma=1.5,rot=0,start=2
;
; MODIFICATION HISTORY:
;          August 2011: Created by J.R.A. Davenport
;          June 2012: Updated plot, fixed setting optional inputs = 0,
;                     removed the unsightly FOR loop
;          November 2012: Modified plot to use Coyote Graphics. DWF.
;          November 2012: Added CubeHelix color table to cgLoadCT and XColors programs. DWF.
;
;I would appreciate a simple acknowledgement for published works using my code:
;   "This publication has made use of code written by James R. A. Davenport."
;
;-

  compile_opt defint32, strictarr, strictarrsubs
  compile_opt HIDDEN

  nlo = 0.
  nhi = 0.

; will always assume 256 colors for IDL
  nlev = 256.

; use defaults from the preprint if not otherwise set
;== updated to deal with user entering 0's
  if n_elements(start) eq 0 then start = 0.5 ; purple
  if n_elements(rots) eq 0 then rots = -1.5
  if n_elements(gamma) eq 0 then gamma = 1.0
  if n_elements(hue) eq 0 then hue = 1.

  fract = findgen(nlev)/(nlev-1.)
  angle = 2.*!dpi*(start/3.0+1.0+rots*fract)
  fract = fract^gamma
  amp   = hue*fract*(1.-fract)/2.
  red   = fract+amp*(-0.14861*cos(angle)+1.78277*sin(angle))
  grn   = fract+amp*(-0.29227*cos(angle)-0.90649*sin(angle))
  blu   = fract+amp*(1.97294*cos(angle))
  

  nhi = total(blu gt 1) + total(grn gt 1) + total(red gt 1)
  nlo = total(blu lt 0) + total(grn lt 0) + total(red lt 0)

  red = red * (red ge 0)
  blu = blu * (blu ge 0)
  grn = grn * (grn ge 0)

  xb = where(blu gt 1)
  xr = where(red gt 1)
  xg = where(grn gt 1)

  if xb[0] ne -1 then blu[xb] = 1
  if xg[0] ne -1 then grn[xg] = 1
  if xr[0] ne -1 then red[xr] = 1


  if total(nhi) gt 0 then print,'Warning: color-clipping on high-end'
  if total(nlo) gt 0 then print,'Warning: color-clipping on low-end'

  if keyword_set(white) then $
     tvlct,red*255.,grn*255.,blu*255. ; load the new color table
  if not keyword_set(white) then $
     tvlct,red*254.,grn*254.,blu*254.

; output the color vectors if requested
  get=[[red],[grn],[blu]]

; show on screen if requested
  if keyword_set(plot) then begin
     cgerase
     cgplot,[-1,256],[1,1],/nodata,/xstyle,/ystyle,yrange=[-1,1],$
          ytickname=replicate(' ',8),xtitle='Color Index',$
          position=[.1,.1,.8,.94]
     for i=0S,255 do cgColorFill,[i-.5,i+.5,i+.5,i-.5],[-1,-1,1,1],/data,color=i
     cgText,.825,.95,/norm,'Cubehelix',charsize=1.5
     cgText,.825,.85,/norm,'start='+string(start,format='(f0.2)')
     cgText,.825,.8 ,/norm,'rots='+string(rots,format='(f0.2)')
     cgText,.825,.75,/norm,'gamma='+string(gamma,format='(f0.2)')
     cgText,.825,.7 ,/norm,'hue='+string(hue,format='(f0.2)')
     cgplot,[-1,256],[1,1],/nodata,/xstyle,/ystyle,yrange=[-1,1],$
          ytickname=replicate(' ',8),xtitle='Color Index',$
          position=[.1,.1,.8,.94], /noerase
  endif

return
end

