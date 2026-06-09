program lab_1_5
    use Environment
    use Config
    use IO_Process
    use Process
    use omp_lib

    implicit none

    type(EmployeeList) :: A
    type(EmployeeList) :: B

    character(kind=CH_, len=POSITION_LEN), allocatable :: Order(:)

    real(8) :: start_time
    real(8) :: end_time
    real(8) :: elapsed_time

    call create_records_file(IN_FILE, RECORD_FILE)

    call read_records(RECORD_FILE, A)

    call read_order(ORDER_FILE, Order)

    start_time = omp_get_wtime()

    call sort_array_by_position(A, Order, B)

    end_time = omp_get_wtime()

    elapsed_time = end_time - start_time

    call write_list(OUT_FILE, B, 'rewind')
    call write_elapsed_time(OUT_FILE, elapsed_time)

end program lab_1_5
