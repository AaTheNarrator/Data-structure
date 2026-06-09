module IO_Process
    use Environment
    use Config

    implicit none

    type :: Node
        character(kind=CH_, len=SURNAME_LEN) :: Surname
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(Node), allocatable :: Next
    end type Node

    type :: PosNode
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(PosNode), allocatable :: Next
    end type PosNode

contains

    subroutine read_list(file, head)
        character(*), intent(in) :: file
        type(Node), allocatable, intent(out) :: head

        integer :: In
        integer :: IO

        open(file=file, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open input file")

        call read_node(In, head)

        close(In)

    end subroutine read_list


    recursive subroutine read_node(In, elem)
        integer, intent(in) :: In
        type(Node), allocatable, intent(out) :: elem

        integer :: IO

        allocate(elem)

        read(In, '(a15,1x,a15)', iostat=IO) elem%Surname, elem%Position

        if (IO == IOSTAT_END) then
            deallocate(elem)
            return
        end if

        call Handle_IO_status(IO, "read input record")

        call read_node(In, elem%Next)

    end subroutine read_node


    subroutine read_order(file, order)
        character(*), intent(in) :: file
        type(PosNode), allocatable, intent(out) :: order

        integer :: In
        integer :: IO

        open(file=file, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open order file")

        call read_order_node(In, order)

        close(In)

    end subroutine read_order


    recursive subroutine read_order_node(In, elem)
        integer, intent(in) :: In
        type(PosNode), allocatable, intent(out) :: elem

        character(kind=CH_, len=POSITION_LEN) :: pos
        integer :: IO

        allocate(elem)

        read(In, '(a15)', iostat=IO) pos

        if (IO == IOSTAT_END) then
            deallocate(elem)
            return
        end if

        call Handle_IO_status(IO, "read order record")

        elem%Position = pos

        call read_order_node(In, elem%Next)

    end subroutine read_order_node


    subroutine write_list(file, head, pos)
        character(*), intent(in) :: file
        character(*), intent(in) :: pos
        type(Node), allocatable, intent(in) :: head

        integer :: Out
        integer :: IO

        open(file=file, encoding=E_, position=pos, newunit=Out, iostat=IO)
        call Handle_IO_status(IO, "open output file")

        call write_node(Out, head)

        close(Out)

    end subroutine write_list


    recursive subroutine write_node(Out, elem)
        integer, intent(in) :: Out
        type(Node), allocatable, intent(in) :: elem

        integer :: IO

        if (allocated(elem)) then
            write(Out, '(a15,1x,a15)', iostat=IO) elem%Surname, elem%Position
            call Handle_IO_status(IO, "write sorted record")

            call write_node(Out, elem%Next)
        end if

    end subroutine write_node


    subroutine write_elapsed_time(file, elapsed_time)
        character(*), intent(in) :: file
        real(8), intent(in) :: elapsed_time

        integer :: Out
        integer :: IO

        open(file=file, encoding=E_, position='append', newunit=Out, iostat=IO)
        call Handle_IO_status(IO, "open output file for elapsed time")

        write(Out, '(/a, f12.8, a)', iostat=IO) &
            "Execution time of sort_list_by_position: ", elapsed_time, " sec."
        call Handle_IO_status(IO, "write elapsed time")

        close(Out)

    end subroutine write_elapsed_time

end module IO_Process
