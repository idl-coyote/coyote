;========================================================================
; :Author: Matt Savoie  <savoie@nsidc.org>  
;
; Created 05/18/2011 
;
;******************************************************************************************;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc.                                ;
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
;========================================================================*/

;
;  Display information about an image at a given location
;
; :Params:
;    image: in, required, type=image
;       A 2D or 3D image array
;    id: in, required, type=string
;       Prefix to display when showing information about the image.
;    xpixel: in, required, type=int
;       The X location on the image to fetch information from
;    ypixel: in, required, type=int
;       The y location on the image to fetch information from
;
;
pro cgDescribeThisImgValues,  image,  id, xpixel,  ypixel
   COMPILE_OPT idl2, logical_predicate


   dims = Size( image, /Dimensions )
   trueIndex = Where( dims EQ 3 )

   ;; I guess a legit dimension could be three, too, but I
   ;; don't know what to do in that case.
   IF N_Elements( trueIndex ) GT 1 THEN $
      Message, 'Dude. This is one strange image! Returning.'

   IF trueIndex[ 0 ] EQ -1 THEN BEGIN
      
      value = image[ xpixel, ypixel ]
      IF Size( value, /TNAME ) EQ 'BYTE' THEN value = Fix( value )
      Print, id +' (' + StrTrim( xpixel, 2 ) + ',' + StrTrim( ypixel, 2 ) + ') is ', StrTrim( value, 2 )

   ENDIF ELSE BEGIN

      ;; 3D image processing here.
      CASE trueindex[ 0 ] OF
         0: rgb = image[ *, xpixel, ypixel ]
         1: rgb = image[ xpixel, *, ypixel ]
         2: rgb = image[ xpixel, ypixel, * ]
      ENDCASE
      IF Size( rgb, /TNAME ) EQ 'BYTE' THEN rgb = Fix( rgb )
      value = '[' + StrTrim( rgb[ 0 ], 2 ) + ', ' + StrTrim( rgb[ 1 ], 2 ) + ', ' + StrTrim( rgb[ 2 ], 2 ) + ']'
      Print, id + ' (' + StrTrim( xpixel, 2 ) + ',' + StrTrim( ypixel, 2 ) + ') is ' + value
   ENDELSE

end


;+
;  Provide information about the selected xy-pixel from an image, or a hash of
;  images.
;
; :Params:
;    iimage: in, required, type=image
;       A 2D or 3D image array
;    xpixel: in, required, type=int
;       The X location on the image to fetch information from
;    ypixel: in, required, type=int
;       The y location on the image to fetch information from
;    hsh: in, optional, type=HASH
;       If a valid HASH, it should contain key/Image (string/array) pairs that
;       will be described.
;       
;-
pro cgImageInfoDescribeValues,  iimage,  xpixel,  ypixel,   hsh
   compile_opt idl2, logical_predicate

   ;; If you've got a hash of images, display the information for those.
   IF obj_valid( hsh ) &&  obj_isa( hsh, 'HASH' ) THEN BEGIN 
      keys =  hsh -> keys( )
      FOR k = 0, n_elements( keys ) - 1 DO BEGIN
         image =  hsh[ keys[ k ] ]
         cgDescribeThisImgValues,  image,  keys[ k ],  xpixel,  ypixel
      ENDFOR 
   ENDIF ELSE BEGIN
      ;; This was just a plain image input so use it.
      cgDescribeThisImgValues,  iimage,  'Value at ',  xpixel,  ypixel      
   ENDELSE 
END
