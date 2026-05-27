module IO_Process
    use Environment
    use Config
    implicit none
    private

    type, public :: Node
        character(kind=CH_, len=SURNAME_LEN) :: Surname
        type(Node), pointer :: Next => null()
    end type Node

    type, public :: List
        type(Node), pointer :: head => null()
    contains
        procedure, public :: Read_List
        procedure, public :: Remove_Last_And_Get
        procedure, public :: Write_List
        procedure, public :: Finalize_List 
    end type List

contains

    subroutine Read_List(this, file)
        class(List), intent(inout) :: this
        character(*), intent(in) :: file
        integer :: In, IO
        type(Node), pointer :: tail => null()
        if (associated(this%head)) call Destroy_Node(this%head)
        nullify(this%head)

        open(file=file, encoding=E_, newunit=In, iostat=IO, status='old')
            call Handle_IO_status(IO, "opening input file")
            call Read_Nodes(In, this%head, tail, IO)
        close(In, iostat=IO)
        call Handle_IO_status(IO, "closing input file")
    end subroutine

    recursive subroutine Read_Nodes(In, current, tail, IO)
        integer, intent(in) :: In
        type(Node), pointer, intent(inout) :: current
        type(Node), pointer, intent(inout) :: tail
        integer, intent(inout) :: IO
        character(kind=CH_, len=SURNAME_LEN) :: temp_surname
        read(In, '(a)', iostat=IO) temp_surname
        call Handle_IO_status(IO, "reading surname")
        if (IO == 0) then
            allocate(current)
            current%Surname = trim(temp_surname)
            nullify(current%Next)
            if (.not. associated(tail)) then
                tail => current
            else
                tail%Next => current
                tail => current
            end if
            call Read_Nodes(In, current%Next, tail, IO)
        else
            nullify(current)
        end if
    end subroutine

    subroutine Remove_Last_And_Get(this, last_surname)
        class(List), intent(inout) :: this
        character(kind=CH_, len=SURNAME_LEN), intent(out) :: last_surname
        if (associated(this%head)) then
            call Remove_Last_And_Get_Node(this%head, last_surname)
        else
            last_surname = ' '
        end if
    end subroutine

    pure recursive subroutine Remove_Last_And_Get_Node(current, last_surname)
        type(Node), pointer, intent(inout) :: current
        character(kind=CH_, len=SURNAME_LEN), intent(out) :: last_surname
        if (associated(current)) then
            if (associated(current%Next)) then
                call Remove_Last_And_Get_Node(current%Next, last_surname)
            else
                last_surname = current%Surname
                deallocate(current)
                nullify(current)
            end if
        else
            last_surname = ' '
        end if
    end subroutine

    subroutine Write_List(this, file, message, position)
        class(List), intent(in) :: this
        character(*), intent(in) :: file, message, position
        integer :: Out, IO
        open(file=file, encoding=E_, position=position, newunit=Out, iostat=IO)
        call Handle_IO_status(IO, "opening output file")
        write(Out,'(/a)') message
        call Write_Nodes(Out, this%head)
        close(Out, iostat=IO)
        call Handle_IO_status(IO, "closing output file")
    end subroutine

    recursive subroutine Write_Nodes(Out, current)
        integer, intent(in) :: Out
        type(Node), pointer, intent(in) :: current
        if (associated(current)) then
            write(Out,'(a)') trim(adjustl(current%Surname))
            call Write_Nodes(Out, current%Next)
        end if
    end subroutine

    subroutine Finalize_List(this)
        class(List), intent(inout) :: this
        if (associated(this%head)) call Destroy_Node(this%head)
        nullify(this%head)
    end subroutine

    recursive subroutine Destroy_Node(current)
        type(Node), pointer, intent(inout) :: current
        if (associated(current)) then
            call Destroy_Node(current%Next)
            deallocate(current)
            nullify(current)
        end if
    end subroutine

end module IO_Process
