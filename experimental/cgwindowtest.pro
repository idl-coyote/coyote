Pro cgWindowTest_Events, event
  
     Widget_Control, event.top, Get_UValue=info
     drawObj = info.drawObj
     IF event.id EQ event.top THEN BEGIN
        drawobj -> Resize, event.x, event.y-30
     ENDIF ELSE BEGIN
        IF event.type NE 1 THEN RETURN
        drawobj -> GetProperty, Storage=data
        drawobj -> RestoreDataCoords
        d = Convert_Coord(event.x, event.y, /Device, /To_Data)
        index = Value_Locate(Indgen(N_Elements(data)),d[0])
        value = data[index]
        text = 'Location: (' + Strtrim(Round(d[0]),2) + ',' + $
            Strtrim(Round(d[1]),2) + ')   Data Value at X Location: ' + StrTrim(value,2)
        Widget_Control, info.label, Set_Value=text
        
        cgPlotS, index, value, PSYM=2, color='olive', symsize=1.5
        Wait, 0.5
        drawObj -> ExecuteCommands
     ENDELSE
END

PRO cgWindowTest

   tlb = Widget_Base(title='Example Program', /tlb_size_events, /Align_Left, column=1)
   drawObj = Obj_New('cgCmdWindow', tlb, Command='cgplot', P1=cgdemodata(1), Color='red', $)
     Event_Handler='cgWindowTest_Events', /Button_Events, Storage=cgdemodata(1))
   base = widget_base(tlb, row=1, /Align_Left, /Grid)
   label = Widget_Label(base, Value="Status Bar", /Sunken_Frame, /Dynamic_Resize)
   widget_control, tlb, /realize, set_uvalue={drawobj:drawObj, label:label}
   xmanager, 'cgWindowtest', tlb, /no_block, Event_Handler='cgWindowTest_Events'
END
   