program lab_1_1

    use Environment
    use Config
    use IO_Process
    use Process
   
    implicit none

    character(SURNAME_LEN,  kind=CH_), allocatable :: Surnames(:), SortSurnames(:)
    character(POSITION_LEN, kind=CH_), allocatable :: Positions(:), SortPositions(:), Sorted_Positions(:) 

    call read_original_list(IN_FILE, Surnames, Positions)
    call read_order(ORDER_FILE, Sorted_Positions)

    call write_original_list(OUT_FILE, Surnames, Positions, "Исходный список:", position="rewind")

    call sort_array_by_position(Surnames, Positions, Sorted_Positions, SortSurnames, SortPositions)

    call write_original_list( &
        OUT_FILE, SortSurnames, SortPositions, &
        "Отсортированный список:", position="append")

end program lab_1_1   
