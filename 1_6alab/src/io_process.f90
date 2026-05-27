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

    recursive subroutine read_node(In, cur, tail)
        type(Node), pointer, intent(inout) :: cur
        type(Node), pointer, intent(inout) :: tail
        integer, intent(in) :: In

        integer :: IO

        read(In, '(a,1x,a)', iostat=IO) cur%Surname, cur%Position
        nullify(cur%Next)

        if (.not. associated(tail)) then
            tail => cur
        else
            tail%Next => cur
            tail => cur
        end if

        call read_node(In, tail%Next, tail)
    end subroutine read_node

    subroutine read_list(file, head)
        character(*), intent(in) :: file
        type(Node), pointer, intent(out) :: head
        integer :: In
        type(Node), pointer :: tail

        nullify(head)
        nullify(tail)

        open(file=file, encoding=E_, newunit=In)
        call read_node(In, head, tail)
        close(In)

    end subroutine read_list

    recursive subroutine read_order_node(In, cur, tail)
        integer, intent(in) :: In
        type(PositionNode), pointer, intent(inout) :: cur
        type(PositionNode), pointer, intent(inout) :: tail
        character(kind=CH_, len=POSITION_LEN) :: position
        integer :: IO

        read(In, '(a15)', iostat=IO) position
        if (IO /= IOSTAT_END) then
            call Handle_IO_status(IO, "read order")

            allocate(cur)

            cur%Position = position
            nullify(cur%Next)
            if (.not. associated(tail)) then
                tail => cur
            else
                tail%Next => cur
                tail => cur
            end if

            call read_order_node(In, tail%Next, tail)
        end if
    end subroutine read_order_node

    subroutine read_order(file, order)
        character(*), intent(in) :: file
        type(PositionNode), pointer, intent(out) :: order
        integer :: In
        type(PositionNode), pointer :: tail

        nullify(order)
        nullify(tail)

        open(file=file, encoding=E_, newunit=In)
        call read_order_node(In, order, tail)
        close(In)
    end subroutine read_order

    subroutine write_list(file, head, msg, pos)
        character(*), intent(in) :: file
        character(*), intent(in) :: msg
        character(*), intent(in) :: pos
        type(Node), pointer, intent(in) :: head
        integer :: Out

        open(file=file, encoding=E_, position=pos, newunit=Out)
        write(Out, '(/a)') msg
        call write_node(Out, head)
        close(Out)

    end subroutine write_list

    recursive subroutine write_node(Out, cur)
        integer, intent(in) :: Out
        type(Node), pointer, intent(in) :: cur

        if (associated(cur)) then
            write(Out, '(a15,1x,a15)') &
                cur%Surname, &
                cur%Position

            call write_node(Out, cur%Next)
        end if
    end subroutine write_node
end module IO_Process
