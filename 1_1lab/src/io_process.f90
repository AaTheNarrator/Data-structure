module IO_Process

    use Config

    implicit none

contains

    function count_of_people(filename) result(n_people)

        character(*), intent(in) :: filename
        integer                  :: n_people

        integer :: In
        integer :: IO
        character(len=256) :: line

        n_people = 0

        open(newunit=In, file=filename, action='read')

        do
            read(In, '(a)', iostat=IO) line

            if (IO /= 0) then
                exit
            end if

            if (len_trim(line) > 0) then
                n_people = n_people + 1
            end if
        end do

        close(In)

    end function count_of_people


    subroutine read_original_list(Input_File, Surnames, Positions)

        character(*), intent(in) :: Input_File

        character(len=SURNAME_LEN),  allocatable, intent(out) :: Surnames(:)
        character(len=POSITION_LEN), allocatable, intent(out) :: Positions(:)

        integer :: In
        integer :: IO
        integer :: i
        integer :: People_Amount

        character(len=256) :: line

        People_Amount = count_of_people(Input_File)

        allocate(Surnames(People_Amount))
        allocate(Positions(People_Amount))

        open(file=Input_File, newunit=In, action='read')

        i = 1

        do
            read(In, '(a)', iostat=IO) line

            if (IO /= 0) then
                exit
            end if

            if (len_trim(line) == 0) then
                cycle
            end if

            Surnames(i) = line(1:SURNAME_LEN)

            if (len_trim(line) > SURNAME_LEN) then
                Positions(i) = adjustl(line(SURNAME_LEN + 2:))
            else
                Positions(i) = ''
            end if

            i = i + 1

            if (i > People_Amount) then
                exit
            end if
        end do

        close(In)

    end subroutine read_original_list


    subroutine read_order(filename, Sorted_Positions)

        character(*), intent(in) :: filename

        character(len=POSITION_LEN), allocatable, intent(out) :: Sorted_Positions(:)

        integer :: In
        integer :: IO
        integer :: n
        integer :: i

        character(len=256) :: line

        n = count_of_people(filename)

        allocate(Sorted_Positions(n))

        open(file=filename, newunit=In, action='read')

        i = 1

        do
            read(In, '(a)', iostat=IO) line

            if (IO /= 0) then
                exit
            end if

            if (len_trim(line) == 0) then
                cycle
            end if

            Sorted_Positions(i) = adjustl(trim(line))

            i = i + 1

            if (i > n) then
                exit
            end if
        end do

        close(In)

    end subroutine read_order


    subroutine write_original_list(Output_File, Surnames, Positions, position)

        character(*), intent(in) :: Output_File
        character(*), intent(in) :: position

        character(len=SURNAME_LEN),  intent(in) :: Surnames(:)
        character(len=POSITION_LEN), intent(in) :: Positions(:)

        integer :: Out
        integer :: i

        open(file=Output_File, newunit=Out, position=position)

        do i = 1, size(Surnames)
            write(Out, '(a,1x,a)') Surnames(i), trim(Positions(i))
        end do

        close(Out)

    end subroutine write_original_list
    subroutine write_elapsed_time(Output_File, elapsed_time)

        character(*), intent(in) :: Output_File
        real(8),      intent(in) :: elapsed_time

        integer :: Out

        open(file=Output_File, newunit=Out, position="append")

        write(Out, '(/a, f12.8, a)') "Время выполнения sort_array_by_position: ", elapsed_time, " сек."

        close(Out)

    end subroutine write_elapsed_time
end module IO_Process
