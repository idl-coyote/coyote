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
; .. image:: cgshade_surf.png
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
; :Params:
;    data: in, required, type=any
;         A two-dimensional array of data to be displayed.
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates of the
;         surface grid.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates of the
;         surface grid.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, hidden, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     bottom: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the bottom color. By default, same as COLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     charsize: in, optional, type=float, default=cgDefCharSize*1.25
;        The character size for axes annotations. Uses cgDefCharSize()*1.25 to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize*1.25 is always used.
;     color: in, optional, type=string/integer, default='blu6'
;        If this keyword is a string, the name of the data color. By default, "BLU6".
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     elevation_shading: in, optional, type=boolean, default=0
;        Set this keyword to put elevation shading into effect for the surface.
;     font: in, optional, type=integer, default=-1
;        The type of font desired. If undefined, and the current graphics device is PostScript,
;        the FONT keyword will be set to 1, indicating true-type fonts. The FONT keyword must
;        be set to -1 (Hershey fonts) or 1 (true-type fonts) for surface annotations to be
;        rotated correctly in PostScript output.
;     layout: in, optional, type=intarr(3)
;         This keyword specifies a grid with a graphics window and determines where the
;         graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;         The grid is determined by the number of columns (ncolumns) by the number of 
;         rows (nrows). The location of the graphic is determined by the third number. The
;         grid numbering starts in the upper left (1) and goes sequentually by column and then
;         by row.
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to prevent the window from erasing the contents before displaying
;        the surface plot.
;     outfilename: in, optional, type=string
;        If the `Output` keyword is set, the user will be asked to supply an output
;        filename, unless this keyword is set to a non-null string. In that case, the
;        value of this keyword will be used as the filename and there will be no dialog
;        presented to the user.
;     output: in, optional, type=string, default=""
;        Set this keyword to the type of output desired. Possible values are these::
;            
;            'PS'   - PostScript file
;            'EPS'  - Encapsulated PostScript file
;            'PDF'  - PDF file
;            'BMP'  - BMP raster file
;            'GIF'  - GIF raster file
;            'JPEG' - JPEG raster file
;            'PNG'  - PNG raster file
;            'TIFF' - TIFF raster file
;            
;        Or, you can simply set this keyword to the name of the output file, and the type of
;        file desired will be determined by the file extension. If you use this option, the
;        user will not be prompted to supply the name of the output file.
;            
;        All raster file output is created through PostScript intermediate files (the
;        PostScript files will be deleted), so ImageMagick and Ghostview MUST be installed 
;        to produce anything other than PostScript output. (See cgPS2PDF and PS_END for 
;        details.) And also note that you should NOT use this keyword when doing multiple 
;        plots. The keyword is to be used as a convenient way to get PostScript or raster 
;        output for a single graphics command.
;     palette: in, optional, type=byte
;         Set this keyword to a 3 x N or N x 3 byte array containing the RGB color vectors 
;         to be loaded before the surface is displayed. Such vectors can be obtained, for 
;         example, from cgLoadCT with the RGB_TABLE keyword:
;               
;             cgLoadCT, 33, RGB_TABLE=palette
;             cgSurf, cgDemoData(2), PALETTE=palette, /Elevation
;                    
;     rotx: in, optional, type=float, default=30
;        The rotation about the X axis.
;     rotz: in, optional, type=float, default=30
;        The rotation about the Z axis.
;     shaded: in, optional, type=boolean, default=1
;        Set this keyword if you wish to display a shaded surface. To display shaded surfaces
;        in a device-independent way, the shading values are confined to indices 0 to 253 with
;        SET_SHADING, and the background color is placed in color index 254. The color table vectors
;        are reduced to 254 elements when this happens. This all happens behind the stage, 
;        and the original color table is restore upon exit. Because I can't tell how many values
;        SET_SHADING is using on entering the program, I just set it back to its default 256 values
;        on exiting the program.
;     shades: in, optional, type=byte
;        Set this keyword to a byte scaled 2D array of the same size as data to shade the surface
;        with these color indices.
;     skirt: in, optional, type=any
;        Set this keyword to a Z value where a skirt will be drawn for the surface.
;     title: in, optional, type=string
;        The title of the plot. It will be written "flat to the screen", rather than rotated.
;     traditional: in, optional, type=boolean, default=0
;         If this keyword is set, the traditional color scheme of a black background for
;         graphics windows on the display is used and PostScript files always use a white background.
;     tsize: in, optional, type=float
;        The character size for the title. Normally, the title character size is 1.1 times
;        the character size of the surface annotation.
;     tspace: in, optional, type=float
;        The title Y spacing. This should be a number, between 0 and 1 that is the fraction 
;        of the distance between !Y.Window[1] and !Y.Window[0] to locate the title above 
;        !Y.Window[1]. When Total(!P.MULTI) EQ 0, the default is 0.005, and it is 0.0025 otherwise.
;     window: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the plot in a resizable graphics window.
;     xstyle: in, hidden
;         The normal XSTYLE keyword.
;     xtitle: in, optional, type=string
;         The X title of the plot.
;     ystyle: in, hidden
;         The normal YSTYLE keyword.
;     ytitle: in, optional, type=string
;         The Y title of the plot.
;     zstyle: in, hidden
;         The normal ZSTYLE keyword.
;     ztitle: in, optional, type=string
;         The Z title of the plot.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Surface command is allowed in the program.
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
    XTITLE=xtitle, $
    YSTYLE=ystyle, $
    YTITLE=ytitle, $
    ZSTYLE=zstyle, $
    ZTITLE=ztitle, $
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
        SHADED=1, $
        SHADES=shades, $
        SKIRT=skirt, $
        TITLE=title, $
        TRADITIONAL=traditional, $
        TSIZE=tsize, $
        TSPACE=tspace, $
        WINDOW=window, $
        XSTYLE=xstyle, $
        XTITLE=xtitle, $
        YSTYLE=ystyle, $
        YTITLE=ytitle, $
        ZSTYLE=zstyle, $
        ZTITLE=ztitle, $
       _Ref_Extra=extra
      
END