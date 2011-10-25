;+
; NAME:
;   PSCONFIG
;
; PURPOSE:
;
;   This program is simply a function wrapper for the FSC_PSCONFIG
;   object program (fsc_psconfig__define.pro). It was written so
;   that it could serve as a drop-in replacement for the PS_FORM
;   program it replaces. It calls the object program's graphical
;   user interface as a modal widget and returns the DEVICE keywords
;   collected from the form in a form that is appropriate for
;   configuring the PostScript device.
;
;   It is now possible to call the program without a graphical user
;   interface, thus getting the default keywords directly. This is
;   appropriate for many applications. Use the NOGUI keyword when
;   you call the program. For example, like this:
;
;       Set_Plot, 'PS'
;       Device, _Extra=PSConfig(/NoGUI, Filename='myfilename.eps')
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: david@idlcoyote.com
;   Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CALLING SEQUENCE:
;
;   psKeywords = PSConfig()
;
; CATEGORY:
;
;   Configuring PostScript output.
;
; DOCUMENTATION:
;
;   Complete documentation for the FSC_PSCONFIG object, including
;   keyword and method descriptions, and example programs using the object
;   can be found on the Coyote's Guide to IDL Programming web page:
;
;     http://www.idlcoyote.com/programs/docs/fsc_psconfig.html
;
; INPUT:
;
;    psConfigObject -- An optional FSC_PSCONFIG object reference can be
;       passed as an argument to the function. The object is not destroyed
;       if passed in as an argument.
;
;       psConfigObject = Obj_New("FSC_PSCONFIG")
;       keywords = PSConfig(psConfigObject)
;
;    Having the object means that you have an on-going and current record
;    of exactly how your PostScript device is configured. Be sure to destroy
;    the object when you are finished with it.
;
; KEYWORDS:
;
;   NOGUI:   Setting this keyword returns the default keyword settings directly,
;            without allowing user interaction.
;
;   Any keyword accepted by the FSC_PSCONFIG object can be used with
;   this program. Here are a few of the most popular keywords.
;
;   Bits_per_Pixel - The number of image bits saved for each image pixel: 2, 4, or 8. The default is 8.
;   Color - Set this keyword to select Color PostScript output. Turned on by default.
;   Decomposed - Set this keyword to turn on 24-bit color support. Set to zero to select indexed color 
;     support. Default is 0. Applies only to IDL versions 7.1 and higher.
;   DefaultSetup - Set this keyword to the "name" of a default style. Current styles (you can easily
;     create and add your own to the source code) are the following:
;
;       "System (Portrait)" - The normal "default" system set-up. Also, "System".
;       "System (Landcape)" - The normal "default" landscape system set-up.
;       "Centered (Portrait)" - The window centered on the page. Also, "Center" or "Centered".
;       "Centered (Landscape)" - The window centered on the landscape page. Also, "Landscape".
;       "Square (Portrait)" - A square plot, centered on the page.
;       "Square (Landscape)" - A square plot, centered on the landscape page.
;       "Figure (Small)" - A small encapsulated figure size, centered on page. Also, "Encapsulated" or "Encapsulate".
;       "Figure (Large)" - A larger encapsulated figure size, centered on page. Also, "Figure".
;       "Color (Portrait)" - A "centered" plot, with color turned on. Also, "Color".
;       "Color (Landscape)" - A "centered" landscape plot, with color turned on.
;
;   Directory - Set this keyword to the name of the starting directory. The current directory is used by default.
;   Encapsulated - Set this keyword to select Encapsulated PostScript output. Turned off by default.
;   European - This keyword is depreciated in favor or "METRIC".
;   Filename - Set thie keyword to the name of the PostScript file. The default is "idl.ps".
;   Inches - Set this keyword to indicate sizes and offsets are in inches as opposed to centimeters. Set by Metric keyword by default.
;   Landscape - Set this keyword to select Landscape page output. Portrait page output is the default.
;   Match - If this keyword is set, the initial PostScript window will match the aspect ratio of the current graphics window.
;   Metric - Set this keyword to indicate metric mode (i.e., A4 page and centimeter units). Turned off by default.
;   NoGUI - Set this keyword if you don't want a graphical user interface, but just want to get the return structure.
;   PageType - Set this keyword to the "type" of page. Possible values are:
;       "Letter" - 8.5 by 11 inches. (Default, unless the Metric keyword is set.)
;       "Legal" - 8.5 by 14 inches.
;       "Ledger" - 11 by 17 inches.
;       "A4" - 21.0 by 29.7 centimeters. (Default, if the Metric keyword is set.)
;   XOffset - Set this keyword to the X Offset. Uses "System (Portrait)" defaults. (Note: offset calculated from lower-left corner of page.)
;   XSize - Set this keyword to the X size of the PostScript "window". Uses "System (Portrait)" defaults.
;   YOffset - Set this keyword to the Y Offset. Uses "System (Portrait)" defaults. (Note: offset calculated from lower-left corner of page.)
;   YSize - Set this keyword to the Y size of the PostScript "window". Uses "System (Portrait)" defaults.
;
;   In addition, the following keywords can be used:
;
;   CANCEL -- An output keyword that will be set to 1 if the user
;   chooses the Cancel button on the form. It will be 0 otherwise.
;
;   FONTINFO -- Set this keyword is you wish to have font information
;   appear on the form. The default is to not include font information.
;
;   FONTTYPE -- Set this keyword to a named variable that will indicate
;   the user's preference for font type. Values will be -1 (Hershey fonts),
;   0 (hardware fonts), and 1 (true-type fonts). This keyword will always
;   return -1 unless the FONTINFO keyword has also been set.
;
;   GROUP_LEADER -- Set this keyword to a widget identifier of the widget
;   you wish to be a group leader for this program.
;
; EXAMPLE:
;
;   To have the user specify PostScript configuration parameters, use
;   the program like this:
;
;     keywords = PSConfig(Cancel=cancelled)
;     IF cancelled THEN RETURN
;     thisDevice = !D.Name
;     Set_Plot, 'PS'
;     Device, _Extra=keywords
;     Plot, findgen(11) ; Or whatever graphics commands you use.
;     Device, /Close_File
;     Set_Plot, thisDevice
;
; OTHER PROGRAMS NEEDED:
;
;   The following programs are required to run this one:
;
;     fsc_droplist.pro
;     fsc_fileselect.pro
;     fsc_inputfield.pro
;     fsc_plotwindow
;     fsc_psconfig__define.pro
;
; MODIFICATIONS:
;
;   Written by David W. Fanning, 31 January 2000.
;   Added NOGUI keyword to allow default keywords to be obtained without
;     user interaction. 11 Oct 2004. DWF.
;   Added CMYK option 24 August 2007. Requires LANGUAGE_LEVEL=2 printer. L. Anderson
;   Updated for IDL 7.1 and 24-bt color PostScript support. 24 May 2009. DWF.
;   Added MATCH keyword. 14 Dec 2010. DWF.
;   Changed ENCAPSULATE keyword to ENCAPSULATED, which is what I always type! 29 Jan 2011. DWF.
;   Depreciated EUROPEAN keyword in favor of METRIC. 31 Jan 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2009, by Fanning Software Consulting, Inc.                           ;
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


FUNCTION PSConfig,                    $
   psObject,                          $ ; A FSC_PSCONFIG object. If psObject is present, all input keywords
                                        ; except Group_Leader and FontInfo are ignored.
   AvantGarde=avantgarde,             $ ; Set this keyword to select the AvantGarde font.
   Bits_per_Pixel=bits_per_pixel,     $ ; The number of image bits saved for each image pixel: 2, 4, or 8.
   Bold=bold,                         $ ; Set this keyword to select the Bold font style.
   BookStyle=book,                    $ ; Set this keyword to select the Book font style.
   Bkman=bookman,                     $ ; Set this keyword to select the Bookman font.
   Cancel=cancelled,                  $ ; This output keyword will be set to 1 if the user CANCELs. Set to 0 otherwise.
   Color=color,                       $ ; Set this keyword to select Color PostScript output.
   Courier=courier,                   $ ; Set this keyword to select the Courier font.
   CMYK=cmyk,                         $ ; Set this keyword to use CMYK colors instead of RGB. (Requires LANGUAGE_LEVEL=2 printer.)
   Debug=debug,                       $ ; Set this keyword to get traceback information when errors are encountered.
   Decomposed=decomposed,             $ ; Set this keyword to select 24-bit color PostScript support. (IDL 7.1 and above.)
   DefaultSetup=defaultsetup,         $ ; Set this keyword to the "name" of a default style.
   Demi=demi,                         $ ; Set this keyword to select the Demi font style.
   Directory=directory,               $ ; Set thie keyword to the name of the starting directory. Current directory by default.
   Encapsulated=encapsulated,         $ ; Set this keyword to select Encapsulated PostScript output.
   European=european,                 $ ; This keyword depreciated in favor of "metric".
   Filename=filename,                 $ ; Set this keyword to the name of the file. Default: 'idl.ps'
   FontInfo=fontinfo,                 $ ; Set this keyword if you want font information in the FSC_PSCONFIG GUI.
   FontSize=fontsize,                 $ ; Set this keyword to the font size. Between 6 and 36. Default is 12.
   FontType=fonttype,                 $ ; An input/output keyword that will have the FontType. Will be !P.Font unless FontInfo is selected.
   Group_Leader=group_leader,         $ ; The group leader of the PSConfig modal widget.
   Helvetica=helvetica,               $ ; Set this keyword to select the Helvetica font.
   Inches=inches,                     $ ; Set this keyword to indicate sizes and offsets are in inches as opposed to centimeters.
   Italic=italic,                     $ ; Set this keyword to select the Italic font style.
   Isolatin=isolatin,                 $ ; Set this keyword to select ISOlatin1 encoding.
   Landscape=landscape,               $ ; Set this keyword to select Landscape output.
   Light=light,                       $ ; Set this keyword to select the Light font style.
   Match=match,                       $ ; Set this keyword to match the aspect ratio of the current graphics window.
   Medium=medium,                     $ ; Set this keyword to select the Medium font style.
   Metric=metric,                     $ ; Set this keyword to indicate metric mode (i.e., A4 page and centimeter units).
   Name=name,                         $ ; The "name" of the object.
   Narrow=narrow,                     $ ; Set this keyword to select the Narrow font style.
   NOGUI=nogui, $                     $ ; Return the default keywords directly, without user interaction.
   Oblique=oblique, $                 $ ; Set this keyword to select the Oblique font style.
   PageType=pagetype,                 $ ; Set this keyword to the "type" of page: 'Letter', 'Legal', 'Ledger', or 'A4'.
   Palatino=palatino,                 $ ; Set this keyword to select the Palatino font.
   Preview=preview,                   $ ; Set this keyword to select Preview mode: 0, 1, or 2.
   Schoolbook=schoolbook,             $ ; Set this keyword to select the Schoolbook font.
   Set_Font=set_font,                 $ ; Set this keyword to the name of a font passed to PostScript with Set_Plot keyword.
   Symbol=symbol,                     $ ; Set this keyword to select the Symbol font.
   Times=times,                       $ ; Set this keyword to select the Times font.
   TrueType=truetype,                 $ ; Set this keyword to select True-Type fonts.
   XOffset=xoffset,                   $ ; Set this keyword to the XOffset. (Note: offset calculated from lower-left corner of page.)
   XSize=xsize,                       $ ; Set this keyword to the X size of the PostScript "window".
   YOffset=yoffset,                   $ ; Set this keyword to the YOffset. (Note: offset calculated from lower-left corner of page.)
   YSize=ysize,                       $ ; Set this keyword to the Y size of the PostScript "window".
   ZapfChancery=zapfchancery,         $ ; Set this keyword to select the ZapfChancery font.
   ZapfDingbats=zapfdingbats            ; Set this keyword to select the ZapfDingbats font.

On_Error, 2

; Depreciated keywords.
IF N_Elements(metric) EQ 0 THEN metric = Keyword_Set(european) ELSE metric = Keyword_Set(metric)

; Cannot have landscape orientation with encapsulated PostScript output.
IF Keyword_Set(encapsulated) THEN landscape = 0

; Did the user ask us to match the aspect ratio of the current graphics window?
IF Keyword_Set(match) THEN BEGIN
    
    ; Is this a device that supports windows?
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
    
        ; Is there a current graphics window?
        IF !D.Window GE 0 THEN BEGIN
            IF N_Elements(inches) NE 0 THEN cm = 1 - Keyword_Set(inches)
            keywords = PSWindow(Landscape=landscape, CM=cm, Metric=metric)
            xsize = keywords.xsize
            ysize = keywords.ysize
            xoffset = keywords.xoffset
            yoffset = keywords.yoffset
            inches = keywords.inches
            landscape = keywords.landscape
            portrait = keywords.portrait
        ENDIF
    ENDIF
ENDIF

IF N_Elements(psObject) EQ 0 THEN BEGIN
   psObject = Obj_New('FSC_PSCONFIG', $
      AvantGarde=avantgarde,             $
      Bits_per_Pixel=bits_per_pixel,     $
      Bold=bold,                         $
      BookStyle=book,                    $
      Bkman=bookman,                     $
      CMYK=cmyk,                         $
      Color=color,                       $
      Courier=courier,                   $
      Debug=debug,                       $
      Decomposed=decomposed,             $
      DefaultSetup=defaultsetup,         $
      Demi=demi,                         $
      Directory=directory,               $
      Encapsulated=encapsulated,         $
      Filename=filename,                 $
      FontSize=fontsize,                 $
      FontType=fonttype,                 $
      Helvetica=helvetica,               $
      Inches=inches,                     $
      Italic=italic,                     $
      Isolatin=isolatin,                 $
      Landscape=landscape,               $
      Light=light,                       $
      Medium=medium,                     $
      Metric=metric,                     $
      Name=name,                         $
      Narrow=narrow,                     $
      Oblique=oblique,                   $
      PageType=pagetype,                 $
      Palatino=palatino,                 $
      Preview=preview,                   $
      Schoolbook=schoolbook,             $
      Set_Font=set_font,                 $
      Symbol=symbol,                     $
      Times=times,                       $
      TrueType=truetype,                 $
      XOffset=xoffset,                   $
      XSize=xsize,                       $
      YOffset=yoffset,                   $
      YSize=ysize,                       $
      ZapfChancery=zapfchancery,         $
      ZapfDingbats=zapfdingbats )
      create = 1
ENDIF ELSE BEGIN
   type = Size(psObject, /Type)
   IF type NE 11 THEN BEGIN
      Message, 'Object Reference required as an argument'
   ENDIF
   create = 0
ENDELSE

   ; Call the GUI of the FSC_PSCONFIG object.

IF Keyword_Set(nogui) EQ 0 THEN $
   psObject->GUI, Group_Leader=group_leader, Cancel=cancelled, FontInfo=Keyword_Set(fontinfo)

   ; Get the PostScript device keywords, along with the font type information.

keywords = psObject->GetKeywords(FontType=fonttype)

   ; If this program created the psObject, destroy it. Otherwise leave it.

IF create THEN Obj_Destroy, psObject

   ; Return the PostScript device keywords.

RETURN, keywords
END ;----------------------------------------------------------------------


