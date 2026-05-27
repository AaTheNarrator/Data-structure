module IO_Process
    use Environment
    use Config
    implicit none
    private

    type, public :: Node
        character(kind=CH_, len=LINE_LEN) :: Line
        type(Node), pointer :: Next => null()
    end type Node

    type, public :: List
        type(Node), pointer :: Head => null()
    contains
        procedure, public :: Read_List
        procedure, public :: Read_Commands
        procedure, public :: Write_List
        procedure, public :: Append
        final :: Finalize_List
    end type List

contains
    subroutine Read_List(this, file_name)
        class(List), intent(inout) :: this
        character(*), intent(in) :: file_name
        integer :: In, IO
        type(Node), pointer :: tail => null()

        if (associated(this%Head)) call Destroy_Nodes(this%Head)
        nullify(this%Head)

        open(file=file_name, newunit=In, encoding=E_, status="old", iostat=IO)
            call Handle_IO_status(IO, "opening input file")
            if (IO == 0) call Read_Nodes(In, this%Head, tail)
        close(In)
    end subroutine

    subroutine Read_Commands(this, file_name, window_size)
        class(List), intent(inout) :: this
        character(*), intent(in) :: file_name
        integer, intent(out) :: window_size
        integer :: In, IO
        type(Node), pointer :: tail => null()

        if (associated(this%Head)) call Destroy_Nodes(this%Head)
        nullify(this%Head)
        
        open(file=file_name, newunit=In, encoding=E_, status="old", iostat=IO)
            call Handle_IO_status(IO, "opening commands file")
            read(In, *, iostat=IO) window_size
            call Handle_IO_status(IO, "reading window size")
            call Read_Nodes(In, this%Head, tail)
        close(In)
    end subroutine

    recursive subroutine Read_Nodes(In, current, tail)
        integer, intent(in) :: In
        type(Node), pointer, intent(inout) :: current
        type(Node), pointer, intent(inout) :: tail
        integer :: IO
        character(kind=CH_, len=LINE_LEN) :: buffer
        read(In, '(a)', iostat=IO) buffer
        
        call Handle_IO_status(IO, "reading line")
        if (IO == 0) then
            allocate(current)
            current%Line = trim(buffer)
            nullify(current%Next)
            if (.not. associated(tail)) then
                tail => current
            else
                tail%Next => current
                tail => current
            end if
            call Read_Nodes(In, current%Next, tail)
        else
            nullify(current)
        end if
    end subroutine

    subroutine Write_List(this, file_name, title, position)
        class(List), intent(in) :: this
        character(*), intent(in) :: file_name, title, position
        integer :: Out, IO
        
        open(file=file_name, newunit=Out, encoding=E_, position=position, iostat=IO)
            call Handle_IO_status(IO, "opening output file")
            write(Out, '(/a)') trim(title)
            call Write_Nodes(Out, this%Head)
        close(Out)
    end subroutine

    recursive subroutine Write_Nodes(Out, current)
        integer, intent(in) :: Out
        type(Node), pointer :: current
        if (associated(current)) then
            write(Out, '(a)') trim(current%Line)
            call Write_Nodes(Out, current%Next)
        end if
    end subroutine

    subroutine Append(this, text)
        class(List), intent(inout) :: this
        character(kind=CH_, len=*), intent(in) :: text
        type(Node), pointer :: current, new_node
        allocate(new_node)
        new_node%Line = trim(text)
        nullify(new_node%Next)
        if (.not. associated(this%Head)) then
            this%Head => new_node
        else
            current => this%Head
            do while (associated(current%Next))
                current => current%Next
            end do
            current%Next => new_node
        end if
    end subroutine

    subroutine Finalize_List(this)
        type(List), intent(inout) :: this
        if (associated(this%Head)) call Destroy_Nodes(this%Head)
        nullify(this%Head)
    end subroutine

    recursive subroutine Destroy_Nodes(current)
        type(Node), pointer, intent(inout) :: current
        if (associated(current)) then
            call Destroy_Nodes(current%Next)
            deallocate(current)
            nullify(current)
        end if
    end subroutine
end module IO_Process
