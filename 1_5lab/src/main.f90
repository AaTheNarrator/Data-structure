program lab_1_5
    use Environment
    use Config
    use IO_Process
    use Process
    implicit none

    type(EmployeeList) :: A, B
    character(kind=CH_, len=POSITION_LEN), allocatable :: Order(:)

    call create_records_file(IN_FILE, RECORD_FILE)

    call read_records(RECORD_FILE, A)
    !call write_list(OUT_FILE, A, "Read from binary:", "rewind")

    call read_order(ORDER_FILE, Order)

    call write_list(OUT_FILE, A, "Input:", "rewind")

    call sort_array_by_position(A, Order, B)

    call write_list(OUT_FILE, B, "Sorted:", "append")
end program lab_1_5
