module IO_Process
    use Environment
    use Config
    implicit none

    type :: Employee
        sequence
        character(kind=CH_, len=SURNAME_LEN)  :: Surname
        character(kind=CH_, len=POSITION_LEN) :: Position
    end type Employee

contains

    function count_people(filename) result(n)
        character(*), intent(in) :: filename
        integer :: n, In, IO
        character(len=100) :: line

        n = 0
        open(file=filename, encoding=E_, newunit=In)
        do
            read(In, '(a)', iostat=IO) line
            if (IO /= 0) exit
            n = n + 1
        end do
        close(In)
    end function

    subroutine create_records_file(Input_File, Data_File)
        character(*), intent(in) :: Input_File, Data_File

        type(Employee) :: rec
        integer :: In, Out, IO, i, n, recl

        n = count_people(Input_File)
        recl = storage_size(rec) / 8

        open(file=Input_File, encoding=E_, newunit=In)
        open(file=Data_File, form='unformatted', access='direct', recl=recl, newunit=Out)

        do i = 1, n
            read(In, '(a15,1x,a15)', iostat=IO) rec%Surname, rec%Position
            call Handle_IO_status(IO, "read input")
            write(Out, rec=i, iostat=IO) rec
            call Handle_IO_status(IO, "write record")
        end do

        close(In)
        close(Out)
    end subroutine

    function read_records(Data_File) result(arr)
        character(*), intent(in) :: Data_File
        type(Employee), allocatable :: arr(:)

        integer :: In, IO, n, recl
        type(Employee) :: tmp

        integer :: rec_len
        
        rec_len = storage_size(tmp) / 8
        
        open(file=Data_File, form='unformatted', access='direct', recl=rec_len, newunit=In)
        inquire(unit=In, size=n)
        close(In)
        
        n = n / rec_len
        recl = rec_len * n
        
        allocate(arr(n))
        
        open(file=Data_File, form='unformatted', access='direct', recl=recl, newunit=In)
        read(In, rec=1, iostat=IO) arr
        call Handle_IO_status(IO, "read all")
        close(In)
    end function

    subroutine read_order(file, order)
        character(*), intent(in) :: file
        character(kind=CH_, len=POSITION_LEN), allocatable, intent(out) :: order(:)

        integer :: In, IO, n, i

        n = count_people(file)
        allocate(order(n))

        open(file=file, encoding=E_, newunit=In)
        read(In, '(a0)', iostat=IO) (order(i), i=1,n)
        close(In)
    end subroutine

    subroutine write_list(file, arr, msg, pos)
        character(*), intent(in) :: file, msg, pos
        type(Employee), intent(in) :: arr(:)

        integer :: Out, i

        open(file=file, encoding=E_, position=pos, newunit=Out)
            write(Out,'(/a)') msg
            write(Out,'( *(a0,1x,a0,/) )') (arr(i)%Surname, arr(i)%Position, i=1,size(arr))
        close(Out)
    end subroutine

end module IO_Process
