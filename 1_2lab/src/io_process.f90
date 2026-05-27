module IO_Process
   use Environment
   use Config

   implicit none

contains

   function count_of_people(filename) result(n_people)
      character(*), intent(in) :: filename
      integer                  :: n_people

      integer                  :: In, IO
      character(:), allocatable :: line

      n_people = 0
      allocate(character(len=SURNAME_LEN + 1 + POSITION_LEN) :: line)

      open(newunit=In, file=filename, encoding=E_, action='read')
         read(In, '(a)', iostat=IO) line
         do while (IO == 0)
            n_people = n_people + 1
            read(In, '(a)', iostat=IO) line
         end do
      close(In)

      if (IO /= IOSTAT_END) call Handle_IO_status(IO, "counting records in " // filename)
   end function count_of_people


   subroutine read_original_list(Input_File, Surnames, Positions)
      character(*), intent(in) :: Input_File
      character(kind=CH_), allocatable, intent(out) :: Surnames(:, :)
      character(kind=CH_), allocatable, intent(out) :: Positions(:, :)

      integer                  :: In, IO, People_Amount, i
      character(:), allocatable :: fmt

      People_Amount = count_of_people(Input_File)

      !$omp allocators allocate(align(32): Surnames, Positions)
      allocate(Surnames(People_Amount, SURNAME_LEN), Positions(People_Amount, POSITION_LEN))
      !$omp end allocators

      fmt = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITION_LEN // 'a1)'

      open(file=Input_File, encoding=E_, newunit=In, action='read')
         read(In, fmt, iostat=IO) (Surnames(i, :), Positions(i, :), i = 1, People_Amount)
      close(In)

      call Handle_IO_status(IO, "reading original list")
   end subroutine read_original_list


   subroutine read_order(filename, Sorted_Positions)
      character(*), intent(in) :: filename
      character(kind=CH_), allocatable, intent(out) :: Sorted_Positions(:, :)

      integer                  :: In, IO, n, i
      character(:), allocatable :: fmt

      n = count_of_people(filename)

      !$omp allocators allocate(align(32): Sorted_Positions)
      allocate(Sorted_Positions(n, POSITION_LEN))
      !$omp end allocators

      fmt = '(' // POSITION_LEN // 'a1)'

      open(file=filename, encoding=E_, newunit=In, action='read')
         read(In, fmt, iostat=IO) (Sorted_Positions(i, :), i = 1, n)
      close(In)

      call Handle_IO_status(IO, "reading order list")
   end subroutine read_order


   subroutine write_original_list(Output_File, Surnames, Positions, Message, position)
      character(*), intent(in) :: Output_File, Message, position
      character(kind=CH_), intent(in) :: Surnames(:, :)
      character(kind=CH_), intent(in) :: Positions(:, :)

      integer                  :: Out, IO, i
      character(:), allocatable :: fmt

      fmt = '(' // SURNAME_LEN // 'a1, 1x, ' // POSITION_LEN // 'a1)'

      open(file=Output_File, encoding=E_, newunit=Out, position=position)
         write(Out, '(/a)') Message
         write(Out, fmt, iostat=IO) (Surnames(i, :), Positions(i, :), i = 1, ubound(Surnames, 1))
      close(Out)

      call Handle_IO_status(IO, "writing " // Message)
   end subroutine write_original_list

end module IO_Process
