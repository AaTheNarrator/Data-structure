module IO_Process
    use Environment
    use Config

    implicit none

    type :: Node
        character(kind=CH_, len=SURNAME_LEN)  :: Surname
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(Node), pointer :: Next => null()
    end type Node

    type :: PositionNode
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(PositionNode), pointer :: Next => null()
    end type PositionNode

contains

    subroutine read_list(file, head)
        character(*), intent(in) :: file
        type(Node), pointer, intent(out) :: head

        integer :: In
        integer :: IO

        character(kind=CH_, len=SURNAME_LEN) :: surname
        character(kind=CH_, len=POSITION_LEN) :: position

        type(Node), pointer :: tail
        type(Node), pointer :: new_node

        nullify(head)
        nullify(tail)

        open(file=file, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open input file")

        do
            read(In, '(a15,1x,a15)', iostat=IO) surname, position

            if (IO == IOSTAT_END) then
                exit
            end if

            call Handle_IO_status(IO, "read input record")

            allocate(new_node)

            new_node%Surname = surname
            new_node%Position = position
            nullify(new_node%Next)

            if (.not. associated(head)) then
                head => new_node
                tail => new_node
            else
                tail%Next => new_node
                tail => new_node
            end if
        end do

        close(In)

    end subroutine read_list


    subroutine read_order(file, order)
        character(*), intent(in) :: file
        type(PositionNode), pointer, intent(out) :: order

        integer :: In
        integer :: IO

        character(kind=CH_, len=POSITION_LEN) :: position

        type(PositionNode), pointer :: tail
        type(PositionNode), pointer :: new_node

        nullify(order)
        nullify(tail)

        open(file=file, encoding=E_, newunit=In, action='read', iostat=IO)
        call Handle_IO_status(IO, "open order file")

        do
            read(In, '(a15)', iostat=IO) position

            if (IO == IOSTAT_END) then
                exit
            end if

            call Handle_IO_status(IO, "read order record")

            allocate(new_node)

            new_node%Position = position
            nullify(new_node%Next)

            if (.not. associated(order)) then
                order => new_node
                tail => new_node
            else
                tail%Next => new_node
                tail => new_node
            end if
        end do

        close(In)

    end subroutine read_order


    subroutine write_list(file, head, pos)
        character(*), intent(in) :: file
        character(*), intent(in) :: pos
        type(Node), pointer, intent(in) :: head

        integer :: Out
        integer :: IO

        type(Node), pointer :: cur

        open(file=file, encoding=E_, position=pos, newunit=Out, iostat=IO)
        call Handle_IO_status(IO, "open output file")

        cur => head

        do while (associated(cur))
            write(Out, '(a15,1x,a15)', iostat=IO) cur%Surname, cur%Position
            call Handle_IO_status(IO, "write sorted record")

            cur => cur%Next
        end do

        close(Out)

    end subroutine write_list


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
