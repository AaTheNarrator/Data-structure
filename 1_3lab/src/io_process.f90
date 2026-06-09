module IO_Process
   use Environment
   use Config

   implicit none

   type :: Employee
      sequence
      character(kind=CH_, len=SURNAME_LEN)  :: Surname
      character(kind=CH_, len=POSITION_LEN) :: Position
   end type Employee

contains

   function count_people(filename) result(n)
      character(*), intent(in) :: filename

      integer :: n
      integer :: In
      integer :: IO

      character(kind=CH_, len=SURNAME_LEN + 1 + POSITION_LEN) :: line

      n = 0

      open(file=filename, encoding=E_, newunit=In, action='read', iostat=IO)
      call Handle_IO_status(IO, "open file for counting")

      do
         read(In, '(a)', iostat=IO) line

         if (IO /= 0) then
            exit
         end if

         n = n + 1
      end do

      close(In)

      if (IO /= IOSTAT_END) then
         call Handle_IO_status(IO, "count records")
      end if

   end function count_people


   subroutine create_records_file(Input_File, Data_File)
      character(*), intent(in) :: Input_File
      character(*), intent(in) :: Data_File

      type(Employee) :: rec

      integer :: In
      integer :: Out
      integer :: IO
      integer :: i
      integer :: n
      integer :: recl

      n = count_people(Input_File)
      recl = storage_size(rec) / 8

      open(file=Input_File, encoding=E_, newunit=In, action='read', iostat=IO)
      call Handle_IO_status(IO, "open input file")

      open(file=Data_File, form='unformatted', access='direct', recl=recl, &
           newunit=Out, status='replace', action='readwrite', iostat=IO)
      call Handle_IO_status(IO, "open records file")

      do i = 1, n
         read(In, '(a15,1x,a15)', iostat=IO) rec%Surname, rec%Position
         call Handle_IO_status(IO, "read input record")

         write(Out, rec=i, iostat=IO) rec
         call Handle_IO_status(IO, "write binary record")
      end do

      close(In)
      close(Out)

   end subroutine create_records_file


   function read_records(Data_File) result(arr)
      character(*), intent(in) :: Data_File

      type(Employee), allocatable :: arr(:)

      integer :: In
      integer :: IO
      integer :: n
      integer :: recl
      integer :: file_size
      integer :: i

      type(Employee) :: tmp

      recl = storage_size(tmp) / 8

      open(file=Data_File, form='unformatted', access='direct', recl=recl, &
           newunit=In, action='read', iostat=IO)
      call Handle_IO_status(IO, "open records file for reading")

      inquire(unit=In, size=file_size)

      n = file_size / recl

      allocate(arr(n))

      do i = 1, n
         read(In, rec=i, iostat=IO) arr(i)
         call Handle_IO_status(IO, "read binary record")
      end do

      close(In)

   end function read_records


   subroutine read_order(file, order)
      character(*), intent(in) :: file

      character(kind=CH_, len=POSITION_LEN), allocatable, intent(out) :: order(:)

      integer :: In
      integer :: IO
      integer :: n
      integer :: i

      n = count_people(file)

      allocate(order(n))

      open(file=file, encoding=E_, newunit=In, action='read', iostat=IO)
      call Handle_IO_status(IO, "open order file")

      read(In, '(a15)', iostat=IO) (order(i), i = 1, n)
      call Handle_IO_status(IO, "read order file")

      close(In)

   end subroutine read_order


   subroutine write_list(file, arr, pos)
      character(*), intent(in) :: file
      character(*), intent(in) :: pos

      type(Employee), intent(in) :: arr(:)

      integer :: Out
      integer :: IO
      integer :: i

      open(file=file, encoding=E_, position=pos, newunit=Out, iostat=IO)
      call Handle_IO_status(IO, "open output file")

      write(Out, '(a15,1x,a15)', iostat=IO) &
         (arr(i)%Surname, arr(i)%Position, i = 1, size(arr))
      call Handle_IO_status(IO, "write sorted records")

      close(Out)

   end subroutine write_list


   subroutine write_elapsed_time(file, elapsed_time)
      character(*), intent(in) :: file
      real(8),      intent(in) :: elapsed_time

      integer :: Out
      integer :: IO

      open(file=file, encoding=E_, position='append', newunit=Out, iostat=IO)
      call Handle_IO_status(IO, "open output file for elapsed time")

      write(Out, '(/a, f12.8, a)', iostat=IO) &
         "Execution time of sort_array_by_position: ", elapsed_time, " sec."
      call Handle_IO_status(IO, "write elapsed time")

      close(Out)

   end subroutine write_elapsed_time

end module IO_Process
