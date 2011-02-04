; docformat = 'rst'
;+
; :Description:
;   Simply a Coyote Graphics System wrapper for the FSC_Colorbar program.
;
; :Categories:
;    Graphics
;+
PRO cgColorbar, $
    ADDCMD=addcmd, $
    ANNOTATECOLOR=annotatecolor, $
    BOTTOM=bottom, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    DIVISIONS=divisions, $
    FONT=font, $
    FORMAT=format, $
    INVERTCOLORS=invertcolors, $
    MAXRANGE=maxrange, $
    MINOR=minor, $
    MINRANGE=minrange, $
    NCOLORS=ncolors, $
    NEUTRALINDEX=neutralIndex, $
    NODISPLAY=nodisplay, $
    PALETTE=palette, $
    POSITION=position, $
    RANGE=range, $
    REVERSE=reverse, $
    RIGHT=right, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TITLE=title, $
    TOP=top, $
    VERTICAL=vertical, $
    XLOG=xlog, $
    YLOG=ylog, $
    WINDOW=window, $
    _REF_EXTRA=extra
    
    FSC_COLORBAR, $
        ADDCMD=addcmd, $
        ANNOTATECOLOR=annotatecolor, $
        BOTTOM=bottom, $
        CHARSIZE=charsize, $
        CLAMP=clamp, $
        COLOR=color, $
        DIVISIONS=divisions, $
        FONT=font, $
        FORMAT=format, $
        INVERTCOLORS=invertcolors, $
        MAXRANGE=maxrange, $
        MINOR=minor, $
        MINRANGE=minrange, $
        NCOLORS=ncolors, $
        NEUTRALINDEX=neutralIndex, $
        NODISPLAY=nodisplay, $
        PALETTE=palette, $
        POSITION=position, $
        RANGE=range, $
        REVERSE=reverse, $
        RIGHT=right, $
        TICKLEN=ticklen, $
        TICKNAMES=ticknames, $
        TITLE=title, $
        TOP=top, $
        VERTICAL=vertical, $
        XLOG=xlog, $
        YLOG=ylog, $
        WINDOW=window, $
        _EXTRA=extra
    
END