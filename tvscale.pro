;+
; NAME:
;     TVSCALE
;
; PURPOSE:
;     TVSCALE is now obsolete and is simply a wrapper for TVIMAGE.
;-
PRO TVSCALE, image, x, y, $
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
   TOP=top, $
   TV=tv, $
   WHITE=white, $
   WINDOW=window, $
   XRANGE=plotxrange, $
   XTITLE=plotxtitle, $
   YRANGE=plotyrange, $
   YTITLE=plotytitle, $
   _EXTRA=extra
   
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
       SCALE=1, $
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