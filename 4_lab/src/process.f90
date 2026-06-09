module Process
   use Environment
   use Config

   implicit none
   private

   character(kind=CH_, len=*), parameter :: EMPTY_CH = ''

   type :: Node
      private
      character(kind=CH_, len=NAME_LEN) :: Name = EMPTY_CH
      type(Node), pointer               :: Next => null()
   end type Node

   type, public :: Counting_Game
      private
      type(Node), pointer :: Head     => null()
      type(Node), pointer :: Tail     => null()
      type(Node), pointer :: Current  => null()
      type(Node), pointer :: Previous => null()

      integer(I_) :: Size = 0
      integer(I_) :: M    = 0

      character(kind=CH_, len=NAME_LEN) :: Start_Name = EMPTY_CH
   contains
      procedure, public :: Read         => Read_Game
      procedure, public :: Write_Input  => Write_Game_Input
      procedure, public :: Prepare      => Prepare_Game
      procedure, public :: Write_Result => Write_Game_Result
      procedure, public :: Clear        => Clear_Game
      final             :: Finalize_Game
   end type Counting_Game

contains

   subroutine Read_Game(This, File_Name)
      class(Counting_Game), intent(inout) :: This
      character(*), intent(in)            :: File_Name

      integer :: In
      integer :: IO
      integer(I_) :: N
      character(kind=CH_, len=NAME_LEN) :: Name

      call This%Clear()

      open (file=File_Name, encoding=E_, newunit=In, iostat=IO, status='old')
      read (In, *, iostat=IO) N, This%M
      read (In, '(a)', iostat=IO) This%Start_Name

      if (N > 0) then
         read (In, '(a)', iostat=IO) Name

         if (IO == 0) then
            call Start_Ring(This, Name)
            call Read_Names(In, This, 2, N)
            This%Tail%Next => This%Head
         end if
      end if

      close (In, iostat=IO)
   end subroutine Read_Game

   recursive subroutine Read_Names(In, This, Position, N)
      integer, intent(in)                 :: In
      class(Counting_Game), intent(inout) :: This
      integer(I_), intent(in)             :: Position
      integer(I_), intent(in)             :: N

      integer :: IO
      character(kind=CH_, len=NAME_LEN) :: Name

      if (Position <= N) then
         read (In, '(a)', iostat=IO) Name
         call Handle_IO_status(IO, 'reading participant')

         if (IO == 0) call Append_Ring(This, Name)

         call Read_Names(In, This, Position + 1_I_, N)
      end if
   end subroutine Read_Names

   subroutine Write_Game_Input(This, File_Name)
      class(Counting_Game), intent(in) :: This
      character(*), intent(in)         :: File_Name

      integer :: Out
      integer :: IO

      open (file=File_Name, encoding=E_, newunit=Out, iostat=IO, status='replace')

      write (Out, '(a, i0)') 'N = ', This%Size
      write (Out, '(a, i0)') 'M = ', This%M
      write (Out, '(a, a)')  'Start = ', Trim(Adjustl(This%Start_Name))

      write (Out, '(/a)') 'Participants:'
      if (Associated(This%Head)) call Write_Ring(Out, This%Head, This%Size)

      close (Out, iostat=IO)
   end subroutine Write_Game_Input

   recursive subroutine Write_Ring(Out, Current, Left)
      integer, intent(in)             :: Out
      type(Node), pointer, intent(in) :: Current
      integer(I_), intent(in)         :: Left

      if (Left > 0_I_) then
         write (Out, '(a)') Trim(Adjustl(Current%Name))
         call Write_Ring(Out, Current%Next, Left - 1_I_)
      end if
   end subroutine Write_Ring

   pure subroutine Prepare_Game(This)
      class(Counting_Game), intent(inout) :: This

      nullify (This%Current)
      nullify (This%Previous)

      if (Associated(This%Head)) then
         This%Previous => This%Tail
         This%Current  => This%Head

         call Find_Start(This, This%Size)
      end if
   end subroutine Prepare_Game

   pure recursive subroutine Find_Start(This, Left)
      class(Counting_Game), intent(inout) :: This
      integer(I_), intent(in)             :: Left

      if (Left > 0 .and. This%Current%Name /= This%Start_Name) then
         This%Previous => This%Current
         This%Current  => This%Current%Next
         call Find_Start(This, Left - 1)
      end if
   end subroutine Find_Start

   subroutine Write_Game_Result(This, File_Name)
      class(Counting_Game), intent(inout) :: This
      character(*), intent(in)            :: File_Name

      integer :: Out
      integer :: IO

      open (file=File_Name, encoding=E_, position='append', newunit=Out, iostat=IO)
      call Handle_IO_status(IO, 'opening output file')

      write (Out, '(/a)') 'Counting result:'

      write (Out, '(/a)') 'Initial state:'
      call Write_Ring(Out, This%Current, This%Size)

      call Write_Counting(Out, This)

      write (Out, '(/a)') 'Winner:'
      write (Out, '(a)') Trim(Adjustl(This%Current%Name))

      close (Out, iostat=IO)
      call Handle_IO_status(IO, 'closing output file')
   end subroutine Write_Game_Result

   recursive subroutine Write_Counting(Out, This)
      integer, intent(in)                 :: Out
      class(Counting_Game), intent(inout) :: This

      character(kind=CH_, len=NAME_LEN) :: Deleted_Name

      if (This%Size > 1) then
         call Move_To_M(This, This%M)
         call Delete_Current(This, Deleted_Name)

         write (Out, '(/a, a)') 'Removed: ', Trim(Adjustl(Deleted_Name))
         write (Out, '(a)') 'Left:'
         call Write_Ring(Out, This%Current, This%Size)

         call Write_Counting(Out, This)
      end if
   end subroutine Write_Counting

   pure recursive subroutine Move_To_M(This, Left)
      class(Counting_Game), intent(inout) :: This
      integer(I_), intent(in)             :: Left

      if (Left > 1) then
         This%Previous => This%Current
         This%Current  => This%Current%Next
         call Move_To_M(This, Left - 1_I_)
      end if
   end subroutine Move_To_M

   pure subroutine Delete_Current(This, Deleted_Name)
      class(Counting_Game), intent(inout)                  :: This
      character(kind=CH_, len=NAME_LEN), intent(out)       :: Deleted_Name

      type(Node), pointer :: Victim

      Victim => This%Current
      Deleted_Name = Victim%Name

      This%Current => This%Current%Next
      This%Previous%Next => This%Current

      if (Associated(This%Head, Victim)) This%Head => This%Current
      if (Associated(This%Tail, Victim)) This%Tail => This%Previous

      nullify (Victim%Next)
      deallocate (Victim)

      This%Size = This%Size - 1
   end subroutine Delete_Current

   pure subroutine Start_Ring(This, Name)
      class(Counting_Game), intent(inout)                 :: This
      character(kind=CH_, len=*), intent(in)              :: Name

      allocate (This%Head)
      This%Head%Name = Trim(Name)
      nullify (This%Head%Next)

      This%Tail => This%Head
      This%Size = 1
   end subroutine Start_Ring

   pure subroutine Append_Ring(This, Name)
      class(Counting_Game), intent(inout)                 :: This
      character(kind=CH_, len=*), intent(in)              :: Name

      type(Node), pointer :: New_Node

      allocate (New_Node)
      New_Node%Name = Trim(Name)
      nullify (New_Node%Next)

      This%Tail%Next => New_Node
      This%Tail => New_Node
      This%Size = This%Size + 1
   end subroutine Append_Ring

   pure subroutine Clear_Game(This)
      class(Counting_Game), intent(inout) :: This

      if (Associated(This%Tail)) nullify (This%Tail%Next)

      call Delete_Nodes(This%Head)

      nullify (This%Tail)
      nullify (This%Current)
      nullify (This%Previous)

      This%Size = 0
      This%M = 0
      This%Start_Name = EMPTY_CH
   end subroutine Clear_Game

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

   pure subroutine Finalize_Game(This)
      type(Counting_Game), intent(inout) :: This

      call This%Clear()
   end subroutine Finalize_Game

end module Process
