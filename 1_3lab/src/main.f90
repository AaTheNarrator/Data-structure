program lab_1_3
    use Environment
    use Config
    use IO_Process
    use Process
    implicit none

    type(Employee), allocatable :: A(:), B(:)
    character(kind=CH_, len=POSITION_LEN), allocatable :: order(:)

    call create_records_file(IN_FILE, DATA_FILE)

    A = read_records(DATA_FILE)
    !call write_list(OUT_FILE, A, "Read from binary:", "rewind")
    call read_order(ORDER_FILE, order)

    call write_list(OUT_FILE, A, "Input:", "rewind")

    call sort_array_by_position(A, order, B)

    call write_list(OUT_FILE, B, "Sorted:", "append")
end program lab_1_3
