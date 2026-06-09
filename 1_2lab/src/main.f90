program lab_1_1

   use Environment
   use Config
   use IO_Process
   use Process
   use omp_lib

   implicit none

   character(kind=CH_), allocatable :: Surnames(:, :)
   character(kind=CH_), allocatable :: SortSurnames(:, :)

   character(kind=CH_), allocatable :: Positions(:, :)
   character(kind=CH_), allocatable :: SortPositions(:, :)

   character(kind=CH_), allocatable :: Sorted_Positions(:, :)

   real(8) :: start_time
   real(8) :: end_time
   real(8) :: elapsed_time

   call read_original_list(IN_FILE, Surnames, Positions)
   call read_order(ORDER_FILE, Sorted_Positions)

   start_time = omp_get_wtime()

   call sort_array_by_position( &
      Surnames, Positions, Sorted_Positions, &
      SortSurnames, SortPositions &
   )

   end_time = omp_get_wtime()

   elapsed_time = end_time - start_time

   call write_sorted_list(OUT_FILE, SortSurnames, SortPositions, position='rewind')
   call write_elapsed_time(OUT_FILE, elapsed_time)

end program lab_1_1
