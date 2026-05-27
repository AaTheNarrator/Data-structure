program lab_2_variant_11
    use Environment
    use Config
    use IO_Process
    use Process
    implicit none
    type(List) :: Text, Commands, Result
    integer :: Window_Size, Current_Pos

    call Text%Read_List(TEXT_FILE)
    call Commands%Read_Commands(CMD_FILE, Window_Size)
    
    call Text%Write_List(OUT_FILE, "Input text", "rewind")
    call Commands%Write_List(OUT_FILE, "Commands", "append")
    Current_Pos = 1
    
    call Save_Window(Get_Nth(Text%Head, Current_Pos), Window_Size, Result)
    call Process_Scrolling(Text%Head, Commands%Head, Window_Size, Current_Pos, Result)

    call Result%Write_List(OUT_FILE, "Scrolling result", "append")
end program
