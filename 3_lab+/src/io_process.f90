module IO_Process
   use Environment
   use Config

   implicit none
   private

   character(kind=CH_, len=*), parameter :: EMPTY_CH = ''

   type :: Node
      private
      character(kind=CH_, len=SURNAME_LEN) :: Surname = EMPTY_CH
      type(Node), allocatable              :: Next
   end type Node

   type, public :: Surname_List
      private
      type(Node), allocatable :: Head
   contains
      procedure, public :: Read                => Read_Surname_List
      procedure, public :: Remove_Last_And_Get => Remove_Last_And_Get_Surname
      procedure, public :: Write               => Write_Surname_List
      procedure, public :: Delete              => Delete_Surname_List
      procedure, public :: Clear               => Clear_Surname_List
      final             :: Finalize_Surname_List
   end type Surname_List

contains

   subroutine Read_Surname_List(This, File_Name)
      class(Surname_List), intent(inout) :: This
      character(*), intent(in)           :: File_Name

      integer :: In
      integer :: IO

      call This%Clear()

      open (file=File_Name, encoding=E_, newunit=In, iostat=IO, status='old')
      call Handle_IO_status(IO, 'opening input file')
      if (IO /= 0) return

      call Read_Nodes(In, This%Head)

      close (In, iostat=IO)
      call Handle_IO_status(IO, 'closing input file')
   end subroutine Read_Surname_List

   recursive subroutine Read_Nodes(In, Current)
      integer, intent(in)                    :: In
      type(Node), allocatable, intent(inout) :: Current

      integer :: IO
      character(kind=CH_, len=SURNAME_LEN) :: Buffer

      read (In, '(a)', iostat=IO) Buffer
      call Handle_IO_status(IO, 'reading surname')

      if (IO == 0) then
         allocate (Current)
         Current%Surname = Trim(Buffer)

         call Read_Nodes(In, Current%Next)
      end if
   end subroutine Read_Nodes

   pure subroutine Remove_Last_And_Get_Surname(This, Last_Surname)
      class(Surname_List), intent(inout)                :: This
      character(kind=CH_, len=SURNAME_LEN), intent(out) :: Last_Surname

      Last_Surname = EMPTY_CH
      call Remove_Last_Node(This%Head, Last_Surname)
   end subroutine Remove_Last_And_Get_Surname

   pure recursive subroutine Remove_Last_Node(Current, Last_Surname)
      type(Node), allocatable, intent(inout)            :: Current
      character(kind=CH_, len=SURNAME_LEN), intent(out) :: Last_Surname

      if (Allocated(Current)) then
         if (Allocated(Current%Next)) then
            call Remove_Last_Node(Current%Next, Last_Surname)
         else
            Last_Surname = Current%Surname
            deallocate (Current)
         end if
      else
         Last_Surname = EMPTY_CH
      end if
   end subroutine Remove_Last_Node

   subroutine Write_Surname_List(This, File_Name, Title, Position)
      class(Surname_List), intent(in) :: This
      character(*), intent(in)        :: File_Name
      character(*), intent(in)        :: Title
      character(*), intent(in)        :: Position

      integer :: Out
      integer :: IO

      open (file=File_Name, encoding=E_, position=Position, newunit=Out, iostat=IO)
      call Handle_IO_status(IO, 'opening output file')
      if (IO /= 0) return

      write (Out, '(/a)') Trim(Title)
      call Write_Nodes(Out, This%Head)

      close (Out, iostat=IO)
      call Handle_IO_status(IO, 'closing output file')
   end subroutine Write_Surname_List

   recursive subroutine Write_Nodes(Out, Current)
      integer, intent(in)                 :: Out
      type(Node), allocatable, intent(in) :: Current

      if (Allocated(Current)) then
         write (Out, '(a)') Trim(Current%Surname)
         call Write_Nodes(Out, Current%Next)
      end if
   end subroutine Write_Nodes

   pure subroutine Delete_Surname_List(This, Target_Surname)
      class(Surname_List), intent(inout)               :: This
      character(kind=CH_, len=SURNAME_LEN), intent(in) :: Target_Surname

      call Delete_Nodes(This%Head, Target_Surname)
   end subroutine Delete_Surname_List

   pure recursive subroutine Delete_Nodes(Current, Target_Surname)
      type(Node), allocatable, intent(inout)           :: Current
      character(kind=CH_, len=SURNAME_LEN), intent(in) :: Target_Surname

      type(Node), allocatable :: Next_Node

      if (Allocated(Current)) then
         if (Trim(Current%Surname) == Trim(Target_Surname)) then
            if (Allocated(Current%Next)) then
               call Move_Alloc(Current%Next, Next_Node)
               deallocate (Current)
               call Move_Alloc(Next_Node, Current)
               call Delete_Nodes(Current, Target_Surname)
            else
               deallocate (Current)
            end if
         else
            call Delete_Nodes(Current%Next, Target_Surname)
         end if
      end if
   end subroutine Delete_Nodes

   pure subroutine Clear_Surname_List(This)
      class(Surname_List), intent(inout) :: This

      if (Allocated(This%Head)) deallocate (This%Head)
   end subroutine Clear_Surname_List

   pure subroutine Finalize_Surname_List(This)
      type(Surname_List), intent(inout) :: This

      call This%Clear()
   end subroutine Finalize_Surname_List

end module IO_Process
