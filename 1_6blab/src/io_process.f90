module IO_Process
    use Environment
    use Config
    implicit none

    type :: Node
        character(kind=CH_, len=SURNAME_LEN) :: Surname
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(Node), allocatable :: Next      ! allocatable, как в lab_alloc
    end type Node

    type :: PosNode
        character(kind=CH_, len=POSITION_LEN) :: Position
        type(PosNode), allocatable :: Next   ! allocatable
    end type PosNode

contains

    subroutine read_list(file, head)
        character(*), intent(in) :: file
        type(Node), allocatable, intent(out) :: head
        integer :: In
        open(file=file, encoding=E_, newunit=In)
        call read_node(In, head)
        close(In)
    end subroutine read_list

    recursive subroutine read_node(In, elem)
        integer, intent(in) :: In
        type(Node), allocatable, intent(out) :: elem
        integer :: IO

        allocate(elem)
        read(In, '(a,1x,a)', iostat=IO) elem%Surname, elem%Position

        if (IO == IOSTAT_END) then
            deallocate(elem)
            return
        end if

        call Handle_IO_status(IO, "read input")
        call read_node(In, elem%Next)
    end subroutine read_node

    subroutine read_order(file, order)
        character(*), intent(in) :: file
        type(PosNode), allocatable, intent(out) :: order
        integer :: In

        open(file=file, encoding=E_, newunit=In)
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

        call Handle_IO_status(IO, "read order")
        elem%Position = pos
        call read_order_node(In, elem%Next)
    end subroutine read_order_node

    subroutine write_list(file, head, msg, pos)
        character(*), intent(in) :: file, msg, pos
        type(Node), allocatable, intent(in) :: head
        integer :: Out

        open(file=file, encoding=E_, position=pos, newunit=Out)
        write(Out, '(/a)') msg
        call write_node(Out, head)
        close(Out)
    end subroutine write_list

    recursive subroutine write_node(Out, elem)
        integer, intent(in) :: Out
        type(Node), allocatable, intent(in) :: elem

        if (allocated(elem)) then
            write(Out, '(a15,1x,a15)') elem%Surname, elem%Position
            call write_node(Out, elem%Next)
        end if
    end subroutine write_node

end module IO_Process
