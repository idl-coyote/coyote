;+
; NAME:
;       TRANSFORM_VOLUME
;
; PURPOSE:
;
;       The purpose of this program is to transform (e.g., rotate,
;       scale, and translate) a 3D array or volume.
;
; AUTHOR:
;
;       Martin Downing,
;       Clinical Research Physicist,
;       Grampian Orthopaedic RSA Research Centre,
;       Woodend Hospital, Aberdeen, AB15 6LS.
;       Pnone: 01224 556055 / 07903901612
;       Fa: 01224 556662
;       E-mail: m.downing@abdn.ac.uk
;
; CATEGORY:
;
;      Mathematics, graphics.
;
; CALLING SEQUENCE:
;
;      result = TRANSFORM_VOLUME( volume )
;
; INPUTS:
;
;       volume:    The 3D array or volume to be transformed.
;
; OPTIONAL KEYWORDS:
;
;      BUFFER_SIZE: To reduce memory overhead the routine processes the job in chunks, the number
;         of elements of which can be set using the BUFFER_SIZE keyword, set this keyword to
;         0 to force the whole array to be processed at one time. The default value is 128.
;
;      MISSING: The value to return for transformed values outside the bounds of
;         the volume. (Passed to the INTERPOLATE function.) Default is 0.
;
;      T3DMAT: The homogeneous transforamtion matrix. If this keyword is not present,
;         the following keywords can be used to create a homogeneous transformation matrix:
;
;         ROTATION - The rotation vector [rx,ry,rz]. The order of rotation is ZYX.
;         TRANSLATE - The translation vector [tx,ty,tz].
;         SCALE - The scale vector [sx,sy,sz].
;         CENTRE_ROTATION - The centre of rotation [cx,cy,cz].
;
; OUTPUTS:
;
;       result:    The transformed array or volume.
;
; COMMON BLOCKS:
;
;       None.
;
; DEPENDENCIES:
;
;       The program uses the library INTERPLOLATE routine, which currently (IDL 5.4)
;       uses linear interpolation. Note that the operation is performed in chunks,
;       each of which is independant of the result of the others, so the operation
;       could easiliy be parallelised.
;
; MODIFICATION HISTORY:
;
;       Written by: Martin Downing, 16 September 2001.
;       Added MISSING keyword. Removed INPLACE keyword. 25 Nov 2001. MD
;-
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
FUNCTION Transform_Volume, volume, Rotation=rotation, $
    Scale=scale, Translate=translate, Centre_Rotation=centre_rotation, $
    T3Dmat=t3dmat, Buffer_Size=buffer_size, Missing=missing

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error.State.Msg)
   RETURN, -1
ENDIF

   ; Find the dimensions of the volume.

 s = Size(volume)
 sx=s[1] & sy=s[2] & sz=s[3]
 st = sx*sy*sz

vol_t = volume
IF N_Elements(missing) THEN missing = 0

   ; Create a transform matrix, if one is not provided.

IF N_Elements(t3dmat) EQ 0 THEN begin

   IF N_Elements(rotation) EQ 0 THEN rotation =[0,0,0]
   IF N_Elements(centre_rotation) EQ 0  THEN centre_rotation=[(sx-1)/2.0,(sy-1)/2.0,(sz-1)/2.0]
   IF N_Elements(translate) EQ 0 THEN translate =[0,0,0]
   IF N_Elements(scale) EQ 0 THEN scale =[1,1,1]

   T3D, /Reset, Translate = -centre_rotation
   T3D, Rotate=rotation
   T3D, Translate= centre_rotation + translate, Scale=scale
   t3dmat = !P.T

ENDIF

   ; Check buffer size. The size 128 is optimim on my system, You may
   ; want to try other values.

 IF N_Elements(buffer_size) EQ 0 THEN buffer_size = 128
 IF buffer_size LE 0 THEN buffer_size = st

   ; Perform the transformations.

 FOR j=0L,(st-1),buffer_size DO BEGIN

      ; Account for possible odd last chunk.

   bufsize = buffer_size < (st-j)

      ; Generate volume coordinates by interpolating temporary array of volume indices.

   i = j + Lindgen(bufsize)
   coords = [ [(i MOD sx)],[((i / sx) MOD (sy))], [(i / (sx * sy))], [Replicate(1b, bufsize)]]
   coords = Temporary(coords) # t3dmat
   vol_t[j:j+bufsize-1] = Interpolate(volume, coords[*,0], coords[*,1], coords[*,2], Missing=missing)
ENDFOR

   ; Return the transformed volume.

RETURN, vol_t
END
