; docformat = 'rst'
;
; NAME:
;   SetDecomposedState
;
; PURPOSE:
;   Provides a device-independent way to set the color decomposition state of the
;   current graphics device. Devices that do not have a DECOMPOSED keyword to the
;   DEVICE command are assumed to be in indexed color mode always.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   Provides a device-independent way to set the color decomposition state of the
;   current graphics device. Devices that do not have a DECOMPOSED keyword to the
;   DEVICE command are assumed to be in indexed color mode always.
;   
;   I have removed the Z-graphics buffer from being controlled by this program. I
;   do so reluctantly, but I am pragmatic enough to realize that progress forward
;   is necessarily slow and that I must recognize the reality of legacy IDL code.
;   
;   My personal view is that all graphics routines should use 24-bit decomposed
;   color. There are myriad advantages, but basically they boil down to this:
;   (1) You have 16.7 million colors available to you simultaneously, and (2) you
;   don't have to contaminate color tables to use drawing colors. Coyote Graphics
;   routines are in the habit of switching out of whatever color mode the user happens 
;   to be using, into 24-bit decomposed color mode, then switching back when finished
;   with its business. But, it is impossible to do this correctly in the Z-graphics
;   buffer.
;   
;   The reason for this is that in the Z-graphics buffer, you need to switch not only
;   the color mode, but also the pixel depth. In other words, I would prefer to set
;   the Z-graphics buffer up like this::
;   
;       Set_Plot, 'Z'
;       Device, Decomposed=1, Set_Pixel_Depth=24
;       
;   But, if I do that, then I need to set it back (for 99 people out of a 100) like this::
;   
;       Device, Decomposed=0, Set_Pixel_Depth=8
;       
;   Unfortunately, using this command will erase whatever is in the Z-graphics buffer!
;   
;   My solution to this problem is to leave it to the user to configure the Z-graphics buffer
;   the way they want it. If you configure it to use 24-bit decomposed color, all of the Coyote
;   Graphics routines will work as they normally do. If you configure it to use 8-bit indexed color,
;   which is the default configuration, then it will work "normally", but you will be in great
;   danger of contaminating the system color table. The reason for this is that Coyote Graphics
;   cannot "restore" the entry color table in the Z-buffer without obliterating what is already
;   in the graphics window. Users who work with indexed color are probably already very much
;   aware of this problem, so it shouldn't surprise them. (They might not expect this with
;   Coyote Graphics, but this is the price that has to be paid.)
;   
;   My suggestion is to put the Z-graphics configuration in your IDL startup file. Set it
;   up in 24-bit decomposed color mode, use Coyote Graphics to do all your graphical output,
;   and you will find things working perfectly. 
;   See `Configuring the Z-Graphics Buffer for Coyote Graphics <http://www.idlcoyote.com/cg_tips/configz.php>` 
;   for additional information.
;
; :Categories:
;    Graphics, Utilities
;    
; :Params:
;    state: in, required, type=integer, default=0
;         Set to 1 to set the current graphics device to decomposed color. Set to 0 
;         to set the current graphics device to indexed color. Devices lacking a 
;         DECOMPOSED keyword are assumed to be perpetually in indexed color mode.
;         The Z-graphics buffer is always unchanged after the 24 Dec 2011 update. 
;       
; :Keywords:
;     currentstate: out, optional, type=integer
;         The current decomposition state of the current graphics device when the
;         program is called. A 1 indicates decomposed color. A 0 indicates indexed 
;         color.
;     depth: out, optional, type=integer
;         The currnet pixel depth of the graphics device. 
;     zdepth: in, optional, type=integer
;         The pixel depth of the Z-graphics device. Set to 8 or 24. Applies ONLY 
;         when setting the Z-graphics device state to 0. If undefined, the current 
;         depth of the Z-graphics device is unchanged from its current state. No
;         longer used after 24 Dec 2011 update. Code still in place, however.
;          
; :Examples:
;     To set the device in color decomposition mode, then return it, do something like this::
;     
;        SetDecomposedState, 1, CurrentState=mode
;        ...
;        SetDecomposeState, mode
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
;        Written, 16 November 2010. DWF.
;        Changes to include SET_PIXEL_DEPTH in Z-graphics buffer. 19 Nov 2010. DWF.
;        Allow PostScript 7.0 to set the decomposition keyword. 12 Dec 2010. DWF.
;        Added DEPTH and ZDEPTH keywords. 31 Dec 2010. DWF.
;        Added a do-nothing NULL device to Case statement. 4 Jan 2011. DWF.
;        Removed the Z-graphics buffer from control by this program. 24 Dec 2011. DWF.
;        Added back the ability to set decomposed state for the Z-buffer, but only
;           if the depth buffer is 24-bits or higher. 25 May 2012. DWF.
;
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO SetDecomposedState, state, CURRENTSTATE=currentState, DEPTH=depth, ZDEPTH=zdepth

    Compile_Opt idl2
    
    On_Error, 2
    
    ; Set to indexed color if you are not told differently.
    state = Keyword_Set(state)
    
    ; Get the current state and pixel depth.
    currentState = DecomposedColor(DEPTH=depth)
    
    ; Set the decomposition state, if you are able. Otherwise assume
    ; indexed color mode.
    CASE StrUpCase(!D.Name) OF
    
       'PS': IF (!Version.Release GE 7.1) THEN Device, Decomposed=state
                             
        'Z': BEGIN ; Z-Graphics Buffer ; See program notes for why the Z-buffer is no longer controlled here.
;             IF Float(!Version.Release) GE 6.4 THEN BEGIN
;                IF N_Elements(zdepth) NE 0 THEN BEGIN
;                   IF (zdepth NE 8) AND (zdepth NE 24) THEN Message, 'ZDEPTH must be set to 8 or 24.'
;                ENDIF
;                CASE state OF
;                    0: BEGIN
;                    
;                        ; Set depth if you have it, otherwise, just leave it alone.
;                        IF N_Elements(zdepth) NE 0 THEN BEGIN
;                            Device, Decomposed=state, Set_Pixel_Depth=zdepth
;                        ENDIF ELSE BEGIN
;                            Device, Decomposed=state
;                        ENDELSE
;                        END
;                    1: Device, Decomposed=state, Set_Pixel_Depth=24
;                 ENDCASE
;             ENDIF
             
             ; I am going to allow the user to set the decomposed state, but only if
             ; they are using a buffer depth that will allow that.
             IF (depth GE 24) THEN Device, Decomposed=state
             END
             
        'MAC': BEGIN
             IF (Float(!Version.Release) GE 5.2) THEN Device, Decomposed=state
             END
             
         'X': Device, Decomposed=state
         
         'WIN': Device, Decomposed=state
         
         'NULL':
         
         ELSE: Message, 'Unrecognized device. Assuming indexed color state.', /INFORMATIONAL
         
    ENDCASE
    
END    
