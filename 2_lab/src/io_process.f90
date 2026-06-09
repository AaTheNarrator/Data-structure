module IO_Process
   use Environment
   use Config

   implicit none
   private

   character(kind=CH_, len=1), parameter :: FORWARD  = Char(70, kind=CH_)
   character(kind=CH_, len=1), parameter :: BACKWARD = Char(66, kind=CH_)

   type, public :: Node
      character(kind=CH_, len=:), allocatable :: Line
      type(Node), pointer                     :: Next => null()
   end type Node

   type, public :: Data_List
      type(Node), pointer, public  :: Head => null()
      type(Node), pointer, private :: Tail => null()
      integer(I_), public          :: Size = 0
   contains
      procedure, public :: Read        => Read_Data_List
      procedure, public :: Write       => Write_Data_List
      procedure, public :: Start       => Start_Data_List
      procedure, public :: Append      => Append_Data_List
      procedure, public :: Clear       => Clear_Data_List
      procedure, public :: Process_All => Process_All_Data_List
      final             :: Finalize_Data_List
   end type Data_List

   type, extends(Data_List), public :: Command_List
      private
      integer(I_) :: Window_Size = 0
   contains
      procedure, public :: Read              => Read_Command_List
      procedure, public :: Write_Window_Size => Write_Command_Window_Size
   end type Command_List

contains

   subroutine Read_Data_List(This, File_Name)
      class(Data_List), intent(inout) :: This
      character(*), intent(in)        :: File_Name

      integer :: In
      character(kind=CH_, len=LINE_LEN) :: Buffer

      open (file=File_Name, newunit=In, encoding=E_, status='old')

         read (In, '(a)') Buffer

         call This%Start(Trim(Buffer))
         call Read_Nodes(In, This)

      close (In)
   end subroutine Read_Data_List


   subroutine Read_Command_List(This, File_Name)
      class(Command_List), intent(inout) :: This
      character(*), intent(in)           :: File_Name

      integer :: In
      character(kind=CH_, len=LINE_LEN) :: Buffer

      open (file=File_Name, newunit=In, encoding=E_, status='old')

         read (In, *) This%Window_Size
         read (In, '(a)') Buffer

         call This%Start(Trim(Buffer))
         call Read_Nodes(In, This)

      close (In)
   end subroutine Read_Command_List


   recursive subroutine Read_Nodes(In, List)
      integer, intent(in)             :: In
      class(Data_List), intent(inout) :: List

      integer :: IO
      character(kind=CH_, len=LINE_LEN) :: Buffer

      read (In, '(a)', iostat=IO) Buffer

      if (IO == 0) then
         call List%Append(Trim(Buffer))
         call Read_Nodes(In, List)
      end if
   end subroutine Read_Nodes


   subroutine Write_Data_List(This, File_Name, Title, Position)
      class(Data_List), intent(in) :: This
      character(*), intent(in)     :: File_Name
      character(*), intent(in)     :: Title
      character(*), intent(in)     :: Position

      integer :: Out

      open (file=File_Name, newunit=Out, encoding=E_, position=Position)

         write (Out, '(/a)') Title
         call Write_Nodes(Out, This%Head)

      close (Out)
   end subroutine Write_Data_List


   subroutine Write_Command_Window_Size(This, File_Name, Title, Position)
      class(Command_List), intent(in) :: This
      character(*), intent(in)        :: File_Name
      character(*), intent(in)        :: Title
      character(*), intent(in)        :: Position

      integer :: Out

      open (file=File_Name, newunit=Out, encoding=E_, position=Position)

         write (Out, '(/a)') Title
         write (Out, '(i0)') This%Window_Size

      close (Out)
   end subroutine Write_Command_Window_Size


   recursive subroutine Write_Nodes(Out, Current)
      integer, intent(in)             :: Out
      type(Node), pointer, intent(in) :: Current

      if (Associated(Current)) then
         write (Out, '(a)') Current%Line
         call Write_Nodes(Out, Current%Next)
      end if
   end subroutine Write_Nodes


   pure subroutine Start_Data_List(This, Text)
      class(Data_List), intent(inout)        :: This
      character(kind=CH_, len=*), intent(in) :: Text

      call This%Clear()

      allocate (This%Head)
      This%Head%Line = Text
      nullify (This%Head%Next)

      This%Tail => This%Head
      This%Size = 1
   end subroutine Start_Data_List


   pure subroutine Append_Data_List(This, Text)
      class(Data_List), intent(inout)        :: This
      character(kind=CH_, len=*), intent(in) :: Text

      type(Node), pointer :: New_Node

      allocate (New_Node)
      New_Node%Line = Text
      nullify (New_Node%Next)

      This%Tail%Next => New_Node
      This%Tail => New_Node
      This%Size = This%Size + 1
   end subroutine Append_Data_List


   pure subroutine Clear_Data_List(This)
      class(Data_List), intent(inout) :: This

      call Delete_Nodes(This%Head)
      nullify (This%Tail)
      This%Size = 0
   end subroutine Clear_Data_List


   pure recursive subroutine Delete_Nodes(Current)
      type(Node), pointer, intent(inout) :: Current

      type(Node), pointer :: Next_Node

      if (Associated(Current)) then
         Next_Node => Current%Next
         nullify (Current%Next)
         deallocate (Current)
         Current => Next_Node
         call Delete_Nodes(Current)
      end if
   end subroutine Delete_Nodes


   pure subroutine Process_All_Data_List(This, Text, Commands)
      class(Data_List), intent(inout) :: This
      type(Data_List), intent(in)     :: Text
      type(Command_List), intent(in)  :: Commands

      integer(I_) :: Current_Pos
      integer(I_) :: Window_Size

      call This%Clear()

      Window_Size = Commands%Window_Size
      Current_Pos = 1

      if (Window_Size > 0 .and. Associated(Text%Head)) then
         call This%Start(Text%Head%Line)

         if (Window_Size > 1 .and. Associated(Text%Head%Next)) &
            call Save_Window(Text%Head%Next, 1_I_, Window_Size - 1, This)

         if (Associated(Commands%Head)) &
            call Process_Scrolling(Text, Commands%Head, Window_Size, Current_Pos, This)
      end if
   end subroutine Process_All_Data_List


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

      select case (Command)
         case (FORWARD)
            if (Current_Pos + Window_Size <= Text%Size) &
               Current_Pos = Current_Pos + 1

         case (BACKWARD)
            if (Current_Pos > 1) &
               Current_Pos = Current_Pos - 1
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


   pure subroutine Finalize_Data_List(This)
      type(Data_List), intent(inout) :: This

      call This%Clear()
   end subroutine Finalize_Data_List

end module IO_Process
