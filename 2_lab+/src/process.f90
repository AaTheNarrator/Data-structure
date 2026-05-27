module Process
   use Environment
   use Config
   use IO_Process

   implicit none
   private

   character(kind=CH_, len=1), parameter :: FORWARD  = Char(70, kind=CH_)
   character(kind=CH_, len=1), parameter :: BACKWARD = Char(66, kind=CH_)

   public :: Process_All

contains

   pure subroutine Process_All(Text, Commands, Result)
      type(Data_List), intent(in)    :: Text
      type(Command_List), intent(in) :: Commands
      type(Data_List), intent(inout) :: Result

      integer(I_) :: Current_Pos
      integer(I_) :: Window_Size

      call Result%Clear()

      Window_Size = Commands%Get_Window_Size()
      Current_Pos = 1

      if (Window_Size > 0 .and. Associated(Text%Head)) then
         call Result%Start(Text%Head%Line)

         if (Window_Size > 1 .and. Associated(Text%Head%Next)) &
            call Save_Window(Text%Head%Next, 1_I_, Window_Size - 1, Result)

         if (Associated(Commands%Head)) &
            call Process_Scrolling(Text, Commands%Head, Window_Size, Current_Pos, Result)
      endif
   end subroutine Process_All

   pure recursive subroutine Process_Scrolling(Text, Current_Command, &
      Window_Size, Current_Pos, Result)

      type(Data_List), intent(in)    :: Text
      type(Node), intent(in)         :: Current_Command
      integer(I_), intent(in)        :: Window_Size
      integer(I_), intent(inout)     :: Current_Pos
      type(Data_List), intent(inout) :: Result

      character(kind=CH_, len=1) :: Command

      Command = Current_Command%Line(1:1)

      call Result%Append(Char(32, kind=CH_))
      call Result%Append(Command)

      select case(Command)
         case(FORWARD)
            if (Current_Pos + Window_Size <= Text%Size) &
               Current_Pos = Current_Pos + 1

         case(BACKWARD)
            if (Current_Pos > 1) Current_Pos = Current_Pos - 1
      end select

      call Save_Window(Text%Head, Current_Pos, Window_Size, Result)

      if (Associated(Current_Command%Next)) &
         call Process_Scrolling(Text, Current_Command%Next, Window_Size, &
            Current_Pos, Result)
   end subroutine Process_Scrolling

   pure recursive subroutine Save_Window(Current, Start_Pos, Left, Result)
      type(Node), intent(in)         :: Current
      integer(I_), intent(in)        :: Start_Pos
      integer(I_), intent(in)        :: Left
      type(Data_List), intent(inout) :: Result

      if (Left > 0) then
         if (Start_Pos > 1) then
            if (Associated(Current%Next)) &
               call Save_Window(Current%Next, Start_Pos - 1, Left, Result)
         else
            call Result%Append(Current%Line)

            if (Associated(Current%Next)) &
               call Save_Window(Current%Next, 1_I_, Left - 1, Result)
         end if
      end if
   end subroutine Save_Window
end module Process
