module IO_Process
   use Environment
   use Config

   implicit none
   private

   character(kind=CH_, len=*), parameter :: EMPTY_CH = ''

   type, public :: Node
      character(kind=CH_, len=LINE_LEN) :: Line = EMPTY_CH
      type(Node), pointer               :: Next => null()
   end type Node

   type, public :: Data_List
      type(Node), pointer, public  :: Head => null()
      type(Node), pointer, private :: Tail => null()
      integer(I_), public          :: Size = 0
   contains
      procedure, public :: Read   => Read_Data_List
      procedure, public :: Write  => Write_Data_List
      procedure, public :: Start  => Start_Data_List
      procedure, public :: Append => Append_Data_List
      procedure, public :: Clear  => Clear_Data_List
      final             :: Finalize_Data_List
   end type Data_List

   type, extends(Data_List), public :: Command_List
      private
      integer(I_) :: Window_Size = 0
   contains
      procedure, public :: Read              => Read_Command_List
      procedure, public :: Get_Window_Size   => Get_Command_Window_Size
      procedure, public :: Write_Window_Size => Write_Command_Window_Size
   end type Command_List

contains

   subroutine Read_Data_List(This, File_Name)
      class(Data_List), intent(inout) :: This
      character(*), intent(in)        :: File_Name

      integer :: In
      integer :: IO
      character(kind=CH_, len=LINE_LEN) :: Buffer

      call This%Clear()

      open (file=File_Name, newunit=In, encoding=E_, status='old', iostat=IO)
      call Handle_IO_status(IO, 'opening input file')
      if (IO /= 0) return

      read (In, '(a)', iostat=IO) Buffer
      call Handle_IO_status(IO, 'reading first list line')

      if (IO == 0) then
         call This%Start(Buffer)
         call Read_Nodes(In, This)
      end if

      close (In, iostat=IO)
      call Handle_IO_status(IO, 'closing input file')
   end subroutine Read_Data_List

   subroutine Read_Command_List(This, File_Name)
      class(Command_List), intent(inout) :: This
      character(*), intent(in)           :: File_Name

      integer :: In
      integer :: IO
      character(kind=CH_, len=LINE_LEN) :: Buffer

      call This%Clear()
      This%Window_Size = 0

      open (file=File_Name, newunit=In, encoding=E_, status='old', iostat=IO)
      call Handle_IO_status(IO, 'opening command file')
      if (IO /= 0) return

      read (In, *, iostat=IO) This%Window_Size
      call Handle_IO_status(IO, 'reading window size')

      read (In, '(a)', iostat=IO) Buffer
      call Handle_IO_status(IO, 'reading first command')

      if (IO == 0) then
         call This%Start(Buffer)
         call Read_Nodes(In, This)
      end if

      close (In, iostat=IO)
      call Handle_IO_status(IO, 'closing command file')
   end subroutine Read_Command_List

   recursive subroutine Read_Nodes(In, List)
      integer, intent(in)             :: In
      class(Data_List), intent(inout) :: List

      integer :: IO
      character(kind=CH_, len=LINE_LEN) :: Buffer

      read (In, '(a)', iostat=IO) Buffer
      call Handle_IO_status(IO, 'reading list line')

      if (IO == 0) then
         call List%Append(Buffer)
         call Read_Nodes(In, List)
      end if
   end subroutine Read_Nodes

   subroutine Write_Data_List(This, File_Name, Title, Position)
      class(Data_List), intent(in) :: This
      character(*), intent(in)     :: File_Name
      character(*), intent(in)     :: Title
      character(*), intent(in)     :: Position

      integer :: Out
      integer :: IO

      open (file=File_Name, newunit=Out, encoding=E_, position=Position, iostat=IO)
      call Handle_IO_status(IO, 'opening output file')
      if (IO /= 0) return

      write (Out, '(/a)') Trim(Title)
      call Write_Nodes(Out, This%Head)

      close (Out, iostat=IO)
      call Handle_IO_status(IO, 'closing output file')
   end subroutine Write_Data_List

   subroutine Write_Command_Window_Size(This, File_Name, Title, Position)
      class(Command_List), intent(in) :: This
      character(*), intent(in)        :: File_Name
      character(*), intent(in)        :: Title
      character(*), intent(in)        :: Position

      integer :: Out
      integer :: IO

      open (file=File_Name, newunit=Out, encoding=E_, position=Position, iostat=IO)
      call Handle_IO_status(IO, 'opening output file')
      if (IO /= 0) return

      write (Out, '(/a)') Trim(Title)
      write (Out, '(i0)') This%Window_Size

      close (Out, iostat=IO)
      call Handle_IO_status(IO, 'closing output file')
   end subroutine Write_Command_Window_Size

   recursive subroutine Write_Nodes(Out, Current)
      integer, intent(in)             :: Out
      type(Node), pointer, intent(in) :: Current

      if (Associated(Current)) then
         write (Out, '(a)') Trim(Current%Line)
         call Write_Nodes(Out, Current%Next)
      end if
   end subroutine Write_Nodes

   pure subroutine Start_Data_List(This, Text)
      class(Data_List), intent(inout)        :: This
      character(kind=CH_, len=*), intent(in) :: Text

      call This%Clear()

      allocate (This%Head)
      This%Head%Line = Trim(Text)
      nullify (This%Head%Next)

      This%Tail => This%Head
      This%Size = 1
   end subroutine Start_Data_List

   pure subroutine Append_Data_List(This, Text)
      class(Data_List), intent(inout)        :: This
      character(kind=CH_, len=*), intent(in) :: Text

      type(Node), pointer :: New_Node

      allocate (New_Node)
      New_Node%Line = Trim(Text)
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

   pure integer(I_) function Get_Command_Window_Size(This) result(Window_Size)
      class(Command_List), intent(in) :: This

      Window_Size = This%Window_Size
   end function Get_Command_Window_Size

   pure subroutine Finalize_Data_List(This)
      type(Data_List), intent(inout) :: This

      call This%Clear()
   end subroutine Finalize_Data_List

end module IO_Process
