;+
; NAME:
;     TVSCALE
;
; PURPOSE:
;     TVSCALE is now obsolete and is simply a wrapper for TVIMAGE.
;-
PRO TVSCALE, image, x, y, $
   ACOLOR=acolor, $
   AXIS=axis, $
   AXES=axes, $
   AXKEYWORDS=axkeywords, $
   BACKGROUND=background, $
   BOTTOM=bottom, $
   BREWER=brewer, $ ; Obsolete and not used
   ERASE=eraseit, $
   HALF_HALF=half_half, $ ; Obsolete and not used
   KEEP_ASPECT_RATIO=keep, $
   MARGIN=margin, $
   MAXValue=max, $
   MINUS_ONE=minusOne, $
   MINValue=min, $
   MULTIMARGIN=multimargin, $
   NCOLORS=ncolors, $
   NOINTERPOLATION=nointerp, $
   NORMAL=normal, $
   POSITION=position, $
   OVERPLOT=overplot, $
   QUIET=quiet, $
   SAVE=save, $
   TOP=top, $
   TVSCL=tvscl, $
   WHITE=white, $
   XRANGE=plotxrange, $
   YRANGE=plotyrange, $
   _EXTRA=extra
   
   TVIMAGE, image, x, y, $
       ACOLOR=acolor, $
       AXIS=axis, $
       AXES=axes, $
       AXKEYWORDS=axkeywords, $
       BACKGROUND=background, $
       BOTTOM=bottom, $
       BREWER=brewer, $ ; Obsolete and not used
       ERASE=eraseit, $
       HALF_HALF=half_half, $ ; Obsolete and not used
       KEEP_ASPECT_RATIO=keep, $
       MARGIN=margin, $
       MAXValue=max, $
       MINUS_ONE=minusOne, $
       MINValue=min, $
       MULTIMARGIN=multimargin, $
       NCOLORS=ncolors, $
       NOINTERPOLATION=nointerp, $
       NORMAL=normal, $
       POSITION=position, $
       OVERPLOT=overplot, $
       QUIET=quiet, $
       SAVE=save, $
       SCALE=1, $
       TOP=top, $
       TV=tvscl, $
       WHITE=white, $
       XRANGE=plotxrange, $
       YRANGE=plotyrange, $
       _EXTRA=extra
       
END