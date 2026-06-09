program lab_1_3
   use Environment
   use Config
   use IO_Process
   use Process
   use omp_lib

   implicit none

   type(Employee), allocatable :: A(:)
   type(Employee), allocatable :: B(:)

   character(kind=CH_, len=POSITION_LEN), allocatable :: order(:)

   real(8) :: start_time
   real(8) :: end_time
   real(8) :: elapsed_time

   integer :: Out
   integer :: IO
   integer :: i

   call create_records_file(IN_FILE, DATA_FILE)

   A = read_records(DATA_FILE)

   call read_order(ORDER_FILE, order)

   start_time = omp_get_wtime()

   call sort_array_by_position(A, order, B)

   end_time = omp_get_wtime()

   elapsed_time = end_time - start_time

   open(file=OUT_FILE, encoding=E_, newunit=Out, position="rewind", iostat=IO)
   call Handle_IO_status(IO, "open output file")

   write(Out, '(a15,1x,a15)', iostat=IO) &
      (B(i)%Surname, B(i)%Position, i = 1, size(B))
   call Handle_IO_status(IO, "write sorted records")

   write(Out, '(/a, f12.8, a)', iostat=IO) &
      "Execution time of sort_array_by_position: ", elapsed_time, " sec."
   call Handle_IO_status(IO, "write elapsed time")

   close(Out)

end program lab_1_3
