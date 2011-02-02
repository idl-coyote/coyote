; docformat = 'rst'
;+
; :Description:
;   Simply a Coyote Graphics System wrapper for the TVIMAGE program, with
;   one minor change. But default, nearest neighbor smampling is used, unless
;   the INTERPOLATE keyword is set.
;
; :Categories:
;    Graphics
;+
PRO cgImage, image, x, y, $
   ACOLOR=acolorname, $
   ADDCMD=addcmd, $
   ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
   ALPHABGPOSITION=alphapos, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BREWER=brewer, $ ; Obsolete and not used.
   BOTTOM=bottom, $
   COLOR=color, $
   ERASE=eraseit, $
   HALF_HALF=half_half, $ ; Obsolete and not used.
   INTERPOLATE=interpolate, $
   KEEP_ASPECT_RATIO=keep, $
   LAYOUT=layout, $
   MARGIN=margin, $
   MAXVALUE=max, $
   MINUS_ONE=minusOne, $
   MINVALUE=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOINTERPOLATION=nointerp, $
   NORMAL=normal, $
   PALETTE=palette, $
   POSITION=position, $
   OVERPLOT=overplot, $
   QUIET=quiet, $
   SAVE=save, $
   SCALE=scale, $
   TOP=top, $
   TV=tv, $
   WHITE=white, $
   WINDOW=window, $
   XRANGE=plotxrange, $
   XTITLE=plotxtitle, $
   YRANGE=plotyrange, $
   YTITLE=plotytitle, $
   _REF_EXTRA=extra
   
   ; Change the default behavior from interpolation to no interpolation.
   IF N_Elements(nointerp) EQ 0 THEN nointerp = 1
   IF N_Elements(interpolate) NE 0 THEN nointerp = 1 - Keyword_Set(interpolate)

   TVIMAGE, image, x, y, $
       ACOLOR=acolorname, $
       ADDCMD=addcmd, $
       ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
       ALPHABGPOSITION=alphapos, $
       AXIS=axis, $
       AXES=axes, $
       AXKEYWORDS=axkeywords, $
       BACKGROUND=background, $
       BREWER=brewer, $ ; Obsolete and not used.
       BOTTOM=bottom, $
       COLOR=color, $
       ERASE=eraseit, $
       HALF_HALF=half_half, $ ; Obsolete and not used.
       KEEP_ASPECT_RATIO=keep, $
       LAYOUT=layout, $
       MARGIN=margin, $
       MAXVALUE=max, $
       MINUS_ONE=minusOne, $
       MINVALUE=min, $
       MULTIMARGIN=multimargin, $
       NCOLORS=ncolors, $
       NOINTERPOLATION=nointerp, $
       NORMAL=normal, $
       PALETTE=palette, $
       POSITION=position, $
       OVERPLOT=overplot, $
       QUIET=quiet, $
       SAVE=save, $
       SCALE=scale, $
       TOP=top, $
       TV=tv, $
       WHITE=white, $
       WINDOW=window, $
       XRANGE=plotxrange, $
       XTITLE=plotxtitle, $
       YRANGE=plotyrange, $
       YTITLE=plotytitle, $
       _EXTRA=extra
   
END