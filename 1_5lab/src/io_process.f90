module IO_Process
    use Environment
    use Config

    implicit none

    type :: EmployeeList
        character(kind=CH_, len=SURNAME_LEN), allocatable :: Surnames(:)
        character(kind=CH_, len=POSITION_LEN), allocatable :: Positions(:)
    end type EmployeeList

contains

    function count_of_people(filename) result(n_people)
        character(*), intent(in) :: filename

        integer :: n_people
        integer :: In
        integer :: IO

        character(len=SURNAME_LEN + 1 + POSITION_LEN) :: line

        n_people = 0

        open(file=filename, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open file for counting")

        do
            read(In, '(a)', iostat=IO) line

            if (IO /= 0) then
                exit
            end if

            n_people = n_people + 1
        end do

        close(In)

        if (IO /= IOSTAT_END) then
            call Handle_IO_status(IO, "count records")
        end if

    end function count_of_people


    subroutine allocate_employee_list(List, n)
        type(EmployeeList), intent(out) :: List
        integer, intent(in) :: n

        allocate(List%Surnames(n))
        allocate(List%Positions(n))

        List%Surnames = ' '
        List%Positions = ' '

    end subroutine allocate_employee_list


    subroutine read_original_list(Input_File, Employees)
        character(*), intent(in) :: Input_File
        type(EmployeeList), intent(out) :: Employees

        integer :: In
        integer :: IO
        integer :: n
        integer :: i

        n = count_of_people(Input_File)

        call allocate_employee_list(Employees, n)

        open(file=Input_File, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open input file")

        read(In, S_FORMAT, iostat=IO) &
            (Employees%Surnames(i), Employees%Positions(i), i = 1, n)

        close(In)

        call Handle_IO_status(IO, "read original list")

    end subroutine read_original_list


    subroutine create_records_file(Input_File, Bin_File)
        character(*), intent(in) :: Input_File
        character(*), intent(in) :: Bin_File

        type(EmployeeList) :: Employees

        integer :: Out
        integer :: IO

        call read_original_list(Input_File, Employees)

        open(file=Bin_File, newunit=Out, access='stream', form='unformatted', &
             action='write', status='replace', iostat=IO)
        call Handle_IO_status(IO, "open records file for writing")

        write(Out, iostat=IO) Employees%Surnames, Employees%Positions
        call Handle_IO_status(IO, "write records file")

        close(Out)

    end subroutine create_records_file


    function count_records_in_file(filename) result(n_records)
        character(*), intent(in) :: filename

        integer :: n_records

        integer(kind=INT64) :: file_size
        integer(kind=INT64) :: one_record_size

        integer :: In
        integer :: IO

        character(kind=CH_, len=SURNAME_LEN) :: one_surname
        character(kind=CH_, len=POSITION_LEN) :: one_position

        open(file=filename, newunit=In, access='stream', form='unformatted', &
             action='read', iostat=IO)
        call Handle_IO_status(IO, "open records file for size inquiry")

        inquire(unit=In, size=file_size, iostat=IO)
        call Handle_IO_status(IO, "inquire records file size")

        close(In)

        one_record_size = int(storage_size(one_surname) / 8, kind=INT64) + &
                          int(storage_size(one_position) / 8, kind=INT64)

        n_records = int(file_size / one_record_size, kind=kind(n_records))

    end function count_records_in_file


    subroutine read_records(filename, Employees)
        character(*), intent(in) :: filename
        type(EmployeeList), intent(out) :: Employees

        integer :: In
        integer :: IO
        integer :: n

        n = count_records_in_file(filename)

        call allocate_employee_list(Employees, n)

        open(file=filename, newunit=In, access='stream', form='unformatted', &
             action='read', iostat=IO)
        call Handle_IO_status(IO, "open records file for reading")

        read(In, iostat=IO) Employees%Surnames, Employees%Positions
        call Handle_IO_status(IO, "read records file")

        close(In)

    end subroutine read_records


    subroutine read_order(filename, Sorted_Positions)
        character(*), intent(in) :: filename

        character(kind=CH_, len=POSITION_LEN), allocatable, intent(out) :: Sorted_Positions(:)

        integer :: In
        integer :: IO
        integer :: n
        integer :: i

        n = count_of_people(filename)

        allocate(Sorted_Positions(n))

        open(file=filename, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open order file")

        read(In, '(a15)', iostat=IO) (Sorted_Positions(i), i = 1, n)
        call Handle_IO_status(IO, "read order list")

        close(In)

    end subroutine read_order


    subroutine write_list(Output_File, Employees, Position)
        character(*), intent(in) :: Output_File
        character(*), intent(in) :: Position

        type(EmployeeList), intent(in) :: Employees

        integer :: Out
        integer :: IO
        integer :: i

        open(file=Output_File, encoding=E_, newunit=Out, action='write', &
             position=Position, iostat=IO)
        call Handle_IO_status(IO, "open output file")

        write(Out, S_FORMAT, iostat=IO) &
            (Employees%Surnames(i), Employees%Positions(i), i = 1, size(Employees%Surnames))
        call Handle_IO_status(IO, "write sorted list")

        close(Out)

    end subroutine write_list


    subroutine write_elapsed_time(Output_File, elapsed_time)
        character(*), intent(in) :: Output_File
        real(8),      intent(in) :: elapsed_time

        integer :: Out
        integer :: IO

        open(file=Output_File, encoding=E_, newunit=Out, action='write', &
             position='append', iostat=IO)
        call Handle_IO_status(IO, "open output file for elapsed time")

        write(Out, '(/a, f12.8, a)', iostat=IO) &
            "Execution time of sort_array_by_position: ", elapsed_time, " sec."
        call Handle_IO_status(IO, "write elapsed time")

        close(Out)

    end subroutine write_elapsed_time

end module IO_Process
