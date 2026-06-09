module test_data_generator

    implicit none

    integer, parameter :: SURNAME_LEN = 15

    ! По условию должность занимает 15 символов.
    ! Для UTF-8 нужен буфер больше 15 байт.
    integer, parameter :: POSITION_LEN = 32

    integer, parameter :: POSITIONS_AMOUNT = 5

    character(len=POSITION_LEN), parameter :: POSITIONS(POSITIONS_AMOUNT) = [ &
        'техник                          ', &
        'инженер                         ', &
        'старший инженер                 ', &
        'ведущий инженер                 ', &
        'главный инженер                 '  &
    ]

contains

    function random_string(length) result(str)

        integer, intent(in) :: length
        character(len=length) :: str

        character(len=*), parameter :: alphabet = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

        integer :: i
        integer :: rnd
        real :: r

        do i = 1, length
            call random_number(r)

            rnd = int(r * len(alphabet)) + 1

            if (rnd > len(alphabet)) then
                rnd = len(alphabet)
            end if

            str(i:i) = alphabet(rnd:rnd)
        end do

    end function random_string


    function random_position() result(position)

        character(len=POSITION_LEN) :: position

        integer :: index
        real :: r

        call random_number(r)

        index = int(r * POSITIONS_AMOUNT) + 1

        if (index > POSITIONS_AMOUNT) then
            index = POSITIONS_AMOUNT
        end if

        position = POSITIONS(index)

    end function random_position


    subroutine generate_test_file(filename, records_amount)

        character(*), intent(in) :: filename
        integer, intent(in) :: records_amount

        integer :: i
        integer :: Out

        character(len=SURNAME_LEN) :: surname
        character(len=POSITION_LEN) :: position

        open(file=filename, newunit=Out, status='replace', action='write')

        do i = 1, records_amount
            surname = random_string(SURNAME_LEN)
            position = random_position()

            write(Out, '(a,1x,a)') surname, position
        end do

        close(Out)

    end subroutine generate_test_file


    integer function last_not_space(str)

        character(*), intent(in) :: str
        integer :: i

        last_not_space = 0

        do i = 1, len(str)
            if (str(i:i) /= ' ') then
                last_not_space = i
            end if
        end do

    end function last_not_space

end module test_data_generator


program generator

    use test_data_generator

    implicit none

    character(len=256) :: filename
    character(len=32) :: records_amount_argument

    integer :: records_amount
    integer :: arguments_amount
    integer :: read_status
    integer :: filename_last

    arguments_amount = command_argument_count()

    if (arguments_amount < 2) then
        print *, 'Usage: ./app output_file records_amount'
        print *, 'Example: ./app ../data/input.txt 1000'
        stop 1
    end if

    call get_command_argument(1, filename)
    call get_command_argument(2, records_amount_argument)

    read(records_amount_argument, *, iostat=read_status) records_amount

    if (read_status /= 0) then
        print *, 'Error: records_amount must be integer'
        stop 1
    end if

    if (records_amount <= 0) then
        print *, 'Error: records_amount must be positive'
        stop 1
    end if

    filename_last = last_not_space(filename)

    if (filename_last == 0) then
        print *, 'Error: empty filename'
        stop 1
    end if

    call generate_test_file(filename(1:filename_last), records_amount)

    print *, 'Generated file: ', filename(1:filename_last)
    print *, 'Records amount:', records_amount

end program generator
