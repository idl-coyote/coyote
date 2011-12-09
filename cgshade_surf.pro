; docformat = 'rst'
;
; NAME:
;   cgShade_Surf
;
; PURPOSE:
;   The purpose of cgShade_Surf is simply to make it easier for people to remember
;   how to create a shaded surface with cgSurf. See the documentation for cgSurf to
;   learn more about surface rendering. All this program does is make sure the SHADED
;   keyword to cgSurf is set.
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
;   The purpose of cgShade_Surf is simply to make it easier for people to remember
;   how to create a shaded surface with cgSurf. See the documentation for cgSurf to
;   learn more about surface rendering. All this program does is make sure the SHADED
;   keyword to cgSurf is set.
;
; :Categories:
;    Graphics
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
;        Written, 1 February 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO cgShade_Surf, data, x, y, $
    ADDCMD=addcmd, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    BOTTOM=sbottom, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    ELEVATION_SHADING=elevation_shading, $
    FONT=font, $
    LAYOUT=layout, $
    NOERASE=noerase, $
    OUTFILENAME=outfilename, $
    OUTPUT=output, $
    PALETTE=palette, $
    ROTX=rotx, $
    ROTZ=rotz, $
    SHADED=shaded, $
    SHADES=shades, $
    SKIRT=skirt, $
    TITLE=title, $
    TRADITIONAL=traditional, $
    TSIZE=tsize, $
    TSPACE=tspace, $
    WINDOW=window, $
    XSTYLE=xstyle, $
    YSTYLE=ystyle, $
    ZSTYLE=zstyle, $
    _Ref_Extra=extra
    
    cgSurf, data, x, y, $
        ADDCMD=addcmd, $
        AXISCOLOR=saxiscolor, $
        AXESCOLOR=saxescolor, $
        BACKGROUND=sbackground, $
        BOTTOM=sbottom, $
        CHARSIZE=charsize, $
        COLOR=scolor, $
        ELEVATION_SHADING=elevation_shading, $
        FONT=font, $
        LAYOUT=layout, $
        NOERASE=noerase, $
        OUTFILENAME=outfilename, $
        OUTPUT=output, $
        PALETTE=palette, $
        ROTX=rotx, $
        ROTZ=rotz, $
        SHADED=shaded, $
        SHADES=shades, $
        SKIRT=skirt, $
        TITLE=title, $
        TRADITIONAL=traditional, $
        TSIZE=tsize, $
        TSPACE=tspace, $
        WINDOW=window, $
        XSTYLE=xstyle, $
        YSTYLE=ystyle, $
        ZSTYLE=zstyle, $
       _Ref_Extra=extra
      
END