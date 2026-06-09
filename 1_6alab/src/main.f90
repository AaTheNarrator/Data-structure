program lab_1_6a
    use Environment
    use Config
    use IO_Process
    use Process
    use omp_lib

    implicit none

    type(Node), pointer :: List => null()
    type(Node), pointer :: Sorted => null()
    type(PositionNode), pointer :: Order => null()

    real(8) :: start_time
    real(8) :: end_time
    real(8) :: elapsed_time

    call read_list(IN_FILE, List)
    call read_order(ORDER_FILE, Order)

    start_time = omp_get_wtime()

    call sort_list_by_position(List, Order, Sorted)

    end_time = omp_get_wtime()

    elapsed_time = end_time - start_time

    call write_list(OUT_FILE, Sorted, "rewind")
    call write_elapsed_time(OUT_FILE, elapsed_time)

end program lab_1_6a
