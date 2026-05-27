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

        integer :: In, IO
        character(len=SURNAME_LEN + 1 + POSITION_LEN) :: line

        n_people = 0

        open(file=filename, encoding=E_, newunit=In, action='read')
        do
            read(In, '(a)', iostat=IO) line
            if (IO /= 0) exit
            n_people = n_people + 1
        end do
        close(In)

        if (IO /= IOSTAT_END) then
            call Handle_IO_status(IO, "counting records in " // filename)
        end if
    end function count_of_people


    subroutine allocate_employee_list(List, n)
        type(EmployeeList), intent(out) :: List
        integer, intent(in) :: n

    end subroutine allocate_employee_list


    subroutine read_original_list(Input_File, Employees)
        character(*), intent(in) :: Input_File
        type(EmployeeList), intent(out) :: Employees

        integer :: In, IO, n, i

        n = count_of_people(Input_File)
        allocate(Employees%Surnames(n), Employees%Positions(n))

        open(file=Input_File, encoding=E_, newunit=In, action='read')
            read(In, S_FORMAT, iostat=IO) &
                (Employees%Surnames(i), Employees%Positions(i), i = 1, n)
        close(In)

        call Handle_IO_status(IO, "reading original list")
    end subroutine read_original_list


    subroutine create_records_file(Input_File, Bin_File)
        character(*), intent(in) :: Input_File, Bin_File

        type(EmployeeList) :: Employees
        integer :: Out, IO

        call read_original_list(Input_File, Employees)

        open(file=Bin_File, newunit=Out, access='stream', form='unformatted', &
             action='write', status='replace')
            write(Out, iostat=IO) Employees%Surnames, Employees%Positions
        close(Out)

        call Handle_IO_status(IO, "writing records file")
    end subroutine create_records_file


    function count_records_in_file(filename) result(n_records)
        character(*), intent(in) :: filename
        integer :: n_records

        integer(kind=int64) :: file_size
        integer(kind=int64) :: one_record_size
        integer :: In, IO
        character(kind=CH_, len=SURNAME_LEN) :: one_surname
        character(kind=CH_, len=POSITION_LEN) :: one_position

        open(file=filename, newunit=In, access='stream', form='unformatted', action='read')
            inquire(unit=In, size=file_size, iostat=IO)
        close(In)

        call Handle_IO_status(IO, "inquiring size of records file")

        one_record_size = int(storage_size(one_surname) / 8, kind=int64) + &
                          int(storage_size(one_position) / 8, kind=int64)

        n_records = int(file_size / one_record_size, kind=kind(n_records))
    end function count_records_in_file


    subroutine read_records(filename, Employees)
        character(*), intent(in) :: filename
        type(EmployeeList), intent(out) :: Employees

        integer :: In, IO, n

        n = count_records_in_file(filename)
        allocate(Employees%Surnames(n), Employees%Positions(n))

        open(file=filename, newunit=In, access='stream', form='unformatted', action='read')
            read(In, iostat=IO) Employees%Surnames, Employees%Positions
        close(In)

        call Handle_IO_status(IO, "reading records file")
    end subroutine read_records


    subroutine read_order(filename, Sorted_Positions)
        character(*), intent(in) :: filename
        character(kind=CH_, len=POSITION_LEN), allocatable, intent(out) :: Sorted_Positions(:)

        integer :: In, IO, n, i

        n = count_of_people(filename)
        allocate(Sorted_Positions(n))

        open(file=filename, encoding=E_, newunit=In, action='read')
            read(In, '(a15)', iostat=IO) (Sorted_Positions(i), i = 1, n)
        close(In)

        call Handle_IO_status(IO, "reading order list")
    end subroutine read_order


    subroutine write_list(Output_File, Employees, Message, Position)
        character(*), intent(in) :: Output_File, Message, Position
        type(EmployeeList), intent(in) :: Employees

        integer :: Out, IO, i

        open(file=Output_File, encoding=E_, newunit=Out, action='write', position=Position)
            write(Out, '(/a)', iostat=IO) Message
            call Handle_IO_status(IO, "writing " // Message)

            write(Out, S_FORMAT, iostat=IO) &
                (Employees%Surnames(i), Employees%Positions(i), i = 1, size(Employees%Surnames))
        close(Out)

        call Handle_IO_status(IO, "writing " // Message)
    end subroutine write_list

end module IO_Process
