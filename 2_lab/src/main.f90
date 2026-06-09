program lab_2_variant_11
   use Environment
   use Config
   use IO_Process

   implicit none

   type(Data_List)    :: Text
   type(Command_List) :: Commands
   type(Data_List)    :: Result

   call Text%Read(TEXT_FILE)
   call Commands%Read(CMD_FILE)

   call Text%Write(OUT_FILE, 'Input file:', 'rewind')
   call Commands%Write_Window_Size(OUT_FILE, 'Page size:', 'append')

   call Result%Process_All(Text, Commands)

   call Result%Write(OUT_FILE, 'Scrolling:', 'append')
end program lab_2_variant_11
