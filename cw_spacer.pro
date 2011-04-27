FUNCTION CW_Spacer, parent, spacer

IF N_Elements(parent) EQ 0 THEN Message, 'Parent ID parameter requried.'
IF N_Elements(spacer) EQ 0 THEN spacer = 10

base = Widget_Base(parent, Row=1)
spacer = Widget_Label(base, Value=String(Replicate(32B, spacer)))
RETURN, base
END