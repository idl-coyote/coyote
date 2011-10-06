;+
; NAME:
;       HCOLORBAR
;
; FILENAME:
;
;       hcolorbar__define.pro
;;
; PURPOSE:
;
;       The purpose of this program is to create a horizontal
;       colorbar object to be used in conjunction with other
;       IDL 5 graphics objects.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com/
;
; CATEGORY:
;
;       Object Graphics.
;
; CALLING SEQUENCE:
;
;       thisColorBar = Obj_New('HColorBar')
;
; REQUIRED INPUTS:
;
;       None.
;
; INIT METHOD KEYWORD PARAMETERS:
;
;       COLOR: A three-element array representing the RGB values of a color
;          for the colorbar axes and annotation. The default value is
;          white: [255,255,255].
;
;       FONTSIZE: A floating value that is the point size of the font
;           used for the axis and title annotations. Set to 8 point by default.
;
;       NAME: The name associated with this object.
;
;       NCOLORS: The number of colors associated with the colorbar. The
;          default is 256.
;
;       MAJOR: The number of major tick divisions on the colorbar axes.
;          The default is 5.
;
;       MINOR: The number of minor tick marks on the colorbar axes.
;          The default is 4.
;
;       PALETTE: A palette object for the colorbar. The default palette
;           is a gray-scale palette object.
;
;       POSITION: A four-element array specifying the position of the
;           colorbar in normalized coordinate space. The default position
;           is [0.10, 0.90, 0.90, 0.95].
;
;       RANGE: The range associated with the colorbar axis. The default
;           is [0, NCOLORS].
;
;       TITLE: A string containing a title for the colorbar axis
;           annotation. The default is a null string.
;
; OTHER METHODS:
;
;       Clamp (Procedure): Given a two-element array in the data range of
;          the colorbar, the colorbar image is clamped to this range. In
;          other words, the range of colors is clamped to the specified
;          range. Values above or below the range in the colorbar are set to
;          the minimum and maximum range values, respectively.
;
;       GetProperty (Procedure): Returns colorbar properties in keyword
;          parameters as defined for the INIT method. Keywords allowed are:
;
;               COLOR
;               MAJOR
;               MINOR
;               NAME
;               PALETTE
;               POSITION
;               RANGE
;               TEXT
;               TITLE
;               TRANSFORM
;
;       SetProperty (Procedure): Sets colorbar properties in keyword
;          parameters as defined for the INIT method. Keywords allowed are:
;
;               COLOR
;               MAJOR
;               MINOR
;               NAME
;               PALETTE
;               POSITION
;               RANGE
;               TEXT
;               TITLE
;               TRANSFORM
;
; SIDE EFFECTS:
;
;       A HColorBar structure is created. The colorbar INHERITS IDLgrMODEL.
;       Thus, all IDLgrMODEL methods and keywords can also be used. It is
;       the model that is selected in a selection event, since the SELECT_TARGET
;       keyword is set for the model.
;
; RESTRICTIONS:
;
;       Requires FSC_NORMALIZE from Coyote Library:
;
;         http://www.idlcoyote.com/programs/fsc_normalize.pro
;
; EXAMPLE:
;
;       To create a colorbar object and add it to a plot view object, type:
;
;       thisColorBarObject = Obj_New('HColorBar')
;       plotView->Add, thisColorBarObject
;       plotWindow->Draw, plotView
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, from VColorBar code, 20 Sept 98. DWF.
;       Changed a reference to _Ref_Extra to _Extra. 27 Sept 98. DWF.
;       Fixed bug when adding a text object via the TEXT keyword. 9 May 99. DWF.
;       Fixed the same bug when getting the text using the TEXT keyword. :-( 16 Aug 2000. DWF.
;       Fixed a bug with getting the text object via the TEXT keyword. 16 Aug 2000. DWF.
;       Added the TRANSFORM keyword to GetProperty and SetProperty methods. 16 Aug 2000. DWF.
;       Added RECOMPUTE_DIMENSIONS=2 to text objects. 16 Aug 2000. DWF.
;       Added a polygon object around the image object. This allows rotation in 3D space. 16 Aug 2000. DWF.
;       Removed TEXT keyword (which was never used) and improved documentation. 15 AUG 2001. DWF.
;       Added ENABLE_FORMATTING keyword to title objects. 22 October 2001. DWF.
;       Added a CLAMP method. 18 November 2001. DWF.
;       Forgot to pass extra keywords along to the text widget. As a result, you couldn't
;          format tick labels, etc. Fixed this. Any keywords appropriate for IDLgrTick objects
;          are now available. 26 June 2002. DWF.
;       Fixed a problem with POSITION keyword in SetProperty method. 23 May 2003. DWF.
;       Fixed a problem with setting RANGE keyword in SetProperty method. 6 Sept 2003. DWF.
;       Removed NORMALIZE from source code. 19 November 2005. DWF.
;       Font sizes have changed. Now using a 12 point font. 6 May 2011. DWF.
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

PRO HColorBar::Clamp, datarange

; This method clamps the data to a particular data range.

self->GetProperty, Range=currentRange

thisclamp = Bytscl(datarange, Max=currentRange[1], Min=currentRange[0])
bar = BytScl(Bindgen(self.ncolors) # Replicate(1B,10), Min=thisclamp[0], Max=thisclamp[1])
self.thisImage->SetProperty, Data=bar
END
;-------------------------------------------------------------------------



FUNCTION HColorBar::INIT, Position=position, Text=text, $
    NColors=ncolors, Title=title, Palette=palette, $
    Major=major, Minor=minor, Range=range, Color=color, $
    _Extra=extra, Name=name, FontSize=fontsize

   ; Catch possible errors.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   Message, !Error_State.Msg, /Informational
   RETURN, 0
ENDIF

   ; Initialize superclass.

IF (self->IDLgrModel::Init(_EXTRA=extra) NE 1) THEN RETURN, 0

    ; Define default values for keywords, if necessary.

IF N_Elements(name) EQ 0 THEN name=''
IF N_Elements(color) EQ 0 THEN self.color = [255,255,255] $
   ELSE self.color = color
IF N_Elements(fontsize) EQ 0 THEN fontsize = 12.0
thisFont = Obj_New('IDLgrFont', 'Helvetica', Size=fontsize)
IF N_Elements(title) EQ 0 THEN title=''
thisTitle = Obj_New('IDLgrText', title, Color=self.color, $
    Font=thisFont, Recompute_Dimensions=2, /Enable_Formatting)
IF N_Elements(ncolors) EQ 0 THEN self.ncolors = 256 $
   ELSE self.ncolors = ncolors
IF N_Elements(palette) EQ 0 THEN BEGIN
    red = (green = (blue = BIndGen(self.ncolors)))
    self.palette = Obj_New('IDLgrPalette', red, green, blue)
ENDIF ELSE self.palette = palette
IF N_Elements(range) EQ 0 THEN self.range = [0, self.ncolors] $
   ELSE self.range = range
IF N_Elements(major) EQ 0 THEN self.major = 5 $
   ELSE self.major = major
IF N_Elements(minor) EQ 0 THEN self.minor = 4 $
   ELSE self.minor = minor
IF N_Elements(position) EQ 0 THEN self.position = [0.10, 0.90, 0.90, 0.95] $
   ELSE self.position = position

    ; Create the colorbar image. Get its size.

bar = BINDGEN(self.ncolors) # REPLICATE(1B,10)
s = SIZE(bar, /Dimensions)
xsize = s[0]
ysize = s[1]

    ; Create the colorbar image object. Add palette to it.

thisImage = Obj_New('IDLgrImage', bar, Palette=self.palette)
xs = FSC_Normalize([0,xsize], Position=[0,1.])
ys = FSC_Normalize([0,ysize], Position=[0,1.])
thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

   ; Create a polygon object. Add the image as a texture map. We do
   ; this so the image can rotate in 3D space.

thisPolygon = Obj_New('IDLgrPolygon', [0, 1, 1, 0], [0, 0, 1, 1], [0,0,0,0], $
   Texture_Map=thisImage, Texture_Coord = [[0,0], [1,0], [1,1], [0,1]], color=[255,255,255])
self.thisPolygon = thisPolygon

   ; Scale the Polygon into the correct position.

xs = FSC_Normalize([0,1], Position=[self.position[0], self.position[2]])
ys = FSC_Normalize([0,1], Position=[self.position[1], self.position[3]])
thispolygon->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ; Create scale factors to position the axes.

longScale = FSC_Normalize(self.range, Position=[self.position[0], self.position[2]])
shortScale = FSC_Normalize([0,1], Position=[self.position[1], self.position[3]])

    ; Create the colorbar axes.

shortAxis1 = Obj_New("IDLgrAxis", 1, Color=self.color, Ticklen=0.0, $
    Major=1, Range=[0,1], /NoText, /Exact, YCoord_Conv=shortScale,  $
    Location=[self.position[0], 1000, 0.001])
shortAxis2 = Obj_New("IDLgrAxis", 1, Color=self.color, Ticklen=0.025, $
    Major=1, Range=[0,1], /NoText, /Exact, YCoord_Conv=shortScale,  $
    Location=[self.position[2], 1000, 0.001], TickDir=1)

textAxis = Obj_New("IDLgrAxis", 0, Color=self.color, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Title=thisTitle, Range=self.range, /Exact, $
    XCoord_Conv=longScale, Location=[1000, self.position[1], 0.001], _Extra=extra)
textAxis->GetProperty, TickText=thisText
thisText->SetProperty, Font=thisFont, Recompute_Dimensions=2

longAxis2 = Obj_New("IDLgrAxis", 0, Color=self.color, /NoText, Ticklen=0.025, $
    Major=self.major, Minor=self.minor, Range=self.range, TickDir=1, $
    XCoord_Conv=longScale, Location=[1000, self.position[3], 0.001], /Exact)

    ; Add the parts to the colorbar model.

self->Add, shortAxis1
self->Add, shortAxis2
self->Add, textAxis
self->Add, longAxis2
self->Add, thisPolygon

   ; Assign the name.

self->IDLgrModel::SetProperty, Name=name, Select_Target=1

    ; Create a container object and put the model into it.

thisContainer = Obj_New('IDL_Container')
thisContainer->Add, thisFont
thisContainer->Add, thisText
thisContainer->Add, thisTitle
thisContainer->Add, thisImage
thisContainer->Add, self.palette
thisContainer->Add, textAxis
thisContainer->Add, shortAxis1
thisContainer->Add, shortAxis2
thisContainer->Add, longAxis2

    ; Update the SELF structure.

self.thisImage = thisImage
self.thisFont = thisFont
self.thisText = thisText
self.textAxis = textAxis
self.shortAxis1 = shortAxis1
self.shortAxis2 = shortAxis2
self.longAxis2 = longAxis2
self.thisContainer = thisContainer
self.thisTitle = thisTitle
self.fontsize = fontsize

RETURN, 1
END
;-------------------------------------------------------------------------



PRO HColorBar::Cleanup

    ; Lifecycle method to clean itself up.

Obj_Destroy, self.thisContainer
self->IDLgrMODEL::Cleanup
END
;-------------------------------------------------------------------------



PRO HColorBar::GetProperty, Position=position, Text=text, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Transform=transform, Name=name, _Ref_Extra=extra

    ; Get the properties of the colorbar.

IF Arg_Present(position) THEN position = self.position
IF Arg_Present(title) THEN self.thisTitle->GetProperty, Strings=title
IF Arg_Present(text) THEN self.thisText->GetProperty, Strings=text
IF Arg_Present(palette) THEN palette = self.palette
IF Arg_Present(major) THEN major = self.major
IF Arg_Present(minor) THEN minor = self.minor
IF Arg_Present(range) THEN range = self.range
IF Arg_Present(color) THEN color = self.color
IF Arg_Present(name) THEN self->IDLgrMODEL::GetProperty, Name=name
IF Arg_Present(transform) THEN self->IDLgrMODEL::GetProperty, Transform=transform
IF Arg_Present(extra) THEN self->IDLgrMODEL::GetProperty, _Ref_Extra=extra

END
;-------------------------------------------------------------------------



PRO HColorBar::SetProperty, Position=position, Text=text, $
    Title=title, Palette=palette, Major=major, Minor=minor, $
    Range=range, Color=color, Transform=transform, Name=name, _Extra=extra

    ; Set properties of the colorbar.

IF N_Elements(position) NE 0 THEN BEGIN
    self.position = position

        ; Move the image polygon into its new positon.

    xs = FSC_Normalize([0,1], Position=[position[0], position[2]])
    ys = FSC_Normalize([0,1], Position=[position[1], position[3]])
    self.thisPolygon->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

        ; Create new scale factors to position the axes.

    longScale = FSC_Normalize(self.range, $
       Position=[self.position[1], self.position[3]])
    shortScale = FSC_Normalize([0,1], $
       Position=[self.position[0], self.position[2]])

        ; Position the axes. 1000 indicates this location is ignored.

    self.textaxis->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position[0], 1000, 0]
    self.longaxis2->SetProperty, YCoord_Conv=longScale, $
       Location=[self.position[2], 1000, 0]
    self.shortAxis1->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position[1], 0]
    self.shortAxis2->SetProperty, XCoord_Conv=shortScale, $
       Location=[1000, self.position[3], 0]

ENDIF
IF N_Elements(text) EQ self.Major THEN self.thisText->SetProperty, Strings=text
IF N_Elements(title) NE 0 THEN self.thisTitle->SetProperty, Strings=title
IF N_Elements(transform) NE 0 THEN self->IDLgrMODEL::SetProperty, Transform=transform
IF N_Elements(palette) NE 0 THEN BEGIN
    self.palette = palette
    self.thisImage->SetProperty, Palette=palette
ENDIF
IF N_Elements(major) NE 0 THEN BEGIN
    self.major = major
    self.textAxis->SetProperty, Major=major
    self.longAxis2->SetProperty, Major=major
END
IF N_Elements(minor) NE 0 THEN BEGIN
    self.minor = minor
    self.textAxis->SetProperty, Minor=minor
    self.longAxis2->SetProperty, Minor=minor
END
IF N_Elements(range) NE 0 THEN BEGIN
    self.range = range
    longScale = FSC_Normalize(range, $
       Position=[self.position[0], self.position[2]])
    self.textAxis->SetProperty, Range=range, XCoord_Conv=longScale
    self.longAxis2->SetProperty, Range=range, XCoord_Conv=longScale
ENDIF
IF N_Elements(color) NE 0 THEN BEGIN
    self.color = color
    self.textAxis->SetProperty, Color=color
    self.longAxis2->SetProperty, Color=color
    self.shortAxis1->SetProperty, Color=color
    self.shortAxis2->SetProperty, Color=color
    self.thisText->SetProperty, Color=color
ENDIF
IF N_Elements(name) NE 0 THEN self->IDLgrMODEL::SetProperty, Name=name
IF N_Elements(extra) NE 0 THEN BEGIN
   self->IDLgrMODEL::SetProperty, _Extra=extra
   self.textAxis->SetProperty, _Extra=extra
   self.thisTitle->SetProperty, _Extra=extra
ENDIF
END
;-------------------------------------------------------------------------



PRO HColorBar__Define

colorbar = { HCOLORBAR, $
             INHERITS IDLgrMODEL, $      ; Inherits the Model Object.
             Position:FltArr(4), $       ; The position of the colorbar.
             Palette:Obj_New(), $        ; The colorbar palette.
             thisImage:Obj_New(), $      ; The colorbar image.
             thisPolygon:Obj_New(), $    ; The image polygon.
             imageModel:Obj_New(), $     ; The colorbar image model.
             thisContainer:Obj_New(), $  ; Container for cleaning up.
             thisFont:Obj_New(), $       ; The annotation font object.
             thisText:Obj_New(), $       ; The bar annotation text object.
             thisTitle: Obj_New(), $     ; The title of the colorbar.
             textAxis:Obj_New(), $       ; The axis containing annotation.
             shortAxis1:Obj_New(), $     ; A short axis.
             shortAxis2:Obj_New(), $     ; A second short axis.
             longAxis2:Obj_New(), $      ; The other long axis.
             NColors:0, $                ; The number of colors in the bar.
             Major:0, $                  ; Number of major axis intervals.
             Minor:0, $                  ; Number of minor axis intervals.
             fontsize:0.0, $             ; The font size of the axis labels. 8 pt by default.
             Color:BytArr(3), $          ; Color of axes and annotation.
             Range:FltArr(2) }           ; The range of the colorbar axis.

END
;-------------------------------------------------------------------------
