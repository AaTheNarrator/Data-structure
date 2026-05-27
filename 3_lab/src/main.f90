program lab_3_1
    use Environment
    use Config
    use IO_Process
    use Process
    implicit none

    type(List) :: S
    character(kind=CH_, len=SURNAME_LEN) :: target_surname

    call S%Read_List(IN_FILE)
    call S%Remove_Last_And_Get(target_surname)

    call S%Write_List(OUT_FILE, "Input (without last)", "rewind")

    call Process_Delete_Matching(S%head, target_surname)

    call S%Write_List(OUT_FILE, "Output", "append")
    call S%Finalize_List()
end program lab_3_1
