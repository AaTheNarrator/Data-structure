program lab_1_6b
    use Environment
    use Config
    use IO_Process
    use Process
    implicit none

    type(Node), allocatable    :: List
    type(Node), allocatable    :: Sorted
    type(PosNode), allocatable :: Order

    call read_list(IN_FILE, List)
    call read_order(ORDER_FILE, Order)

    call write_list(OUT_FILE, List, "Input:", "rewind")
    call sort_list_by_position(List, Order, Sorted)
    call write_list(OUT_FILE, Sorted, "Sorted:", "append")
end program lab_1_6b
