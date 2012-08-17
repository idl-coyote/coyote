; docformat = 'rst'
;
; NAME:
;   ANSI_VALUE
;
; PURPOSE:
;   Provides a new way to display non-printable characters in widget elements.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Bernat Puigdomenech. All rights reserved.                        ;
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
;  THIS SOFTWARE IS PROVIDED BY BERNAT PUIGDOMENECH. ''AS IS'' AND ANY                     ;
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
; :Description:
;   Provides a way to display non-printable characters in widget elements.
;
; :Categories:
;    Widgets
;    
; :Params:
;    str_in: in, required, type=string
;         The input string that you wish to render on a widget element.
;       
; :Keywords:
;     example: in, optional, type=boolean, default=0
;         Set this keyword to see an example of non-printable characters
;         rendered in a Dialog_Pickfile widget.
;          
; :Examples:
;    Call the built-in example::
;       IDL> void = ANSI_Value(/EXAMPLE)
;       
; :Author:
;       Bernat Puigdomenech
;
; :History:
;     Change History::
;        Written, 2 September 2011. Bernat Puigdomenech.
;
; :Copyright:
;     Copyright (c) 2011, Bernat Puigdomenech.
;-
Forward_Function ANSI_VALUE

FUNCTION ANSI_VALUE, str_in, EXAMPLE=example

  IF KEYWORD_SET(example) THEN BEGIN
    str_in = 'à é l·l © ñ ½ » Ø þ ü'
    str_out = ANSI_VALUE(str_in)
    void = DIALOG_MESSAGE(str_out, /CENTER, /INFO)
    RETURN, str_out
  ENDIF
  
  str_out=str_in
  
  FOR i=0, N_ELEMENTS(str_in)-1 DO BEGIN
  
    str_out[i]=''
    str_inb=BYTE(str_in[i])
    
    FOR j=0l, N_ELEMENTS(str_inb)-1 DO BEGIN
    
      IF str_inb[j] GT 126b THEN BEGIN
      
        ansi_char=STRING(str_inb[j:(j+1)<(N_ELEMENTS(str_inb)-1)])
        j++
        
        CASE ansi_char OF
        
          '€': ansi_char=STRING(128B)
          '‚': ansi_char=STRING(130B)
          'ƒ': ansi_char=STRING(131B)
          '„': ansi_char=STRING(132B)
          '…': ansi_char=STRING(133B)
          '†': ansi_char=STRING(134B)
          '‡': ansi_char=STRING(135B)
          'ˆ': ansi_char=STRING(136B)
          '‰': ansi_char=STRING(137B)
          'Š': ansi_char=STRING(138B)
          '‹': ansi_char=STRING(139B)
          'Œ': ansi_char=STRING(140B)
          'Ž': ansi_char=STRING(142B)
          '‘': ansi_char=STRING(145B)
          '’': ansi_char=STRING(146B)
          '“': ansi_char=STRING(147B)
          '”': ansi_char=STRING(148B)
          '•': ansi_char=STRING(149B)
          '–': ansi_char=STRING(150B)
          '—': ansi_char=STRING(151B)
          '˜': ansi_char=STRING(152B)
          '™': ansi_char=STRING(153B)
          'š': ansi_char=STRING(154B)
          '›': ansi_char=STRING(155B)
          'œ': ansi_char=STRING(156B)
          'ž': ansi_char=STRING(158B)
          'Ÿ': ansi_char=STRING(159B)
          '¡': ansi_char=STRING(161B)
          '¢': ansi_char=STRING(162B)
          '£': ansi_char=STRING(163B)
          '¤': ansi_char=STRING(164B)
          '¥': ansi_char=STRING(165B)
          '¦': ansi_char=STRING(166B)
          '§': ansi_char=STRING(167B)
          '¨': ansi_char=STRING(168B)
          '©': ansi_char=STRING(169B)
          'ª': ansi_char=STRING(170B)
          '«': ansi_char=STRING(171B)
          '¬': ansi_char=STRING(172B)
          '®': ansi_char=STRING(174B)
          '¯': ansi_char=STRING(175B)
          '°': ansi_char=STRING(176B)
          '±': ansi_char=STRING(177B)
          '²': ansi_char=STRING(178B)
          '³': ansi_char=STRING(179B)
          '´': ansi_char=STRING(180B)
          'µ': ansi_char=STRING(181B)
          '¶': ansi_char=STRING(182B)
          '·': ansi_char=STRING(183B)
          '¸': ansi_char=STRING(184B)
          '¹': ansi_char=STRING(185B)
          'º': ansi_char=STRING(186B)
          '»': ansi_char=STRING(187B)
          '¼': ansi_char=STRING(188B)
          '½': ansi_char=STRING(189B)
          '¾': ansi_char=STRING(190B)
          '¿': ansi_char=STRING(191B)
          'À': ansi_char=STRING(192B)
          'Á': ansi_char=STRING(193B)
          'Â': ansi_char=STRING(194B)
          'Ã': ansi_char=STRING(195B)
          'Ä': ansi_char=STRING(196B)
          'Å': ansi_char=STRING(197B)
          'Æ': ansi_char=STRING(198B)
          'Ç': ansi_char=STRING(199B)
          'È': ansi_char=STRING(200B)
          'É': ansi_char=STRING(201B)
          'Ê': ansi_char=STRING(202B)
          'Ë': ansi_char=STRING(203B)
          'Ì': ansi_char=STRING(204B)
          'Í': ansi_char=STRING(205B)
          'Î': ansi_char=STRING(206B)
          'Ï': ansi_char=STRING(207B)
          'Ð': ansi_char=STRING(208B)
          'Ñ': ansi_char=STRING(209B)
          'Ò': ansi_char=STRING(210B)
          'Ó': ansi_char=STRING(211B)
          'Ô': ansi_char=STRING(212B)
          'Õ': ansi_char=STRING(213B)
          'Ö': ansi_char=STRING(214B)
          '×': ansi_char=STRING(215B)
          'Ø': ansi_char=STRING(216B)
          'Ù': ansi_char=STRING(217B)
          'Ú': ansi_char=STRING(218B)
          'Û': ansi_char=STRING(219B)
          'Ü': ansi_char=STRING(220B)
          'Ý': ansi_char=STRING(221B)
          'Þ': ansi_char=STRING(222B)
          'ß': ansi_char=STRING(223B)
          'à': ansi_char=STRING(224B)
          'á': ansi_char=STRING(225B)
          'â': ansi_char=STRING(226B)
          'ã': ansi_char=STRING(227B)
          'ä': ansi_char=STRING(228B)
          'å': ansi_char=STRING(229B)
          'æ': ansi_char=STRING(230B)
          'ç': ansi_char=STRING(231B)
          'è': ansi_char=STRING(232B)
          'é': ansi_char=STRING(233B)
          'ê': ansi_char=STRING(234B)
          'ë': ansi_char=STRING(235B)
          'ì': ansi_char=STRING(236B)
          'í': ansi_char=STRING(237B)
          'î': ansi_char=STRING(238B)
          'ï': ansi_char=STRING(239B)
          'ð': ansi_char=STRING(240B)
          'ñ': ansi_char=STRING(241B)
          'ò': ansi_char=STRING(242B)
          'ó': ansi_char=STRING(243B)
          'ô': ansi_char=STRING(244B)
          'õ': ansi_char=STRING(245B)
          'ö': ansi_char=STRING(246B)
          '÷': ansi_char=STRING(247B)
          'ø': ansi_char=STRING(248B)
          'ù': ansi_char=STRING(249B)
          'ú': ansi_char=STRING(250B)
          'û': ansi_char=STRING(251B)
          'ü': ansi_char=STRING(252B)
          'ý': ansi_char=STRING(253B)
          'þ': ansi_char=STRING(254B)
          'ÿ': ansi_char=STRING(255B)
          
          ELSE: 
        END
        
      ENDIF ELSE ansi_char=STRING(str_inb[j])
      
      str_out[i]=str_out[i]+ansi_char
      
    ENDFOR
    
  ENDFOR
  
  RETURN, str_out
END