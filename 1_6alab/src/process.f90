module Process
    use Environment
    use Config
    use IO_Process

    implicit none

contains

    subroutine sort_list_by_position(head, order, sorted)
        type(Node), target, intent(in) :: head
        type(PositionNode), pointer, intent(in) :: order
        type(Node), pointer, intent(out) :: sorted

        type(Node), pointer :: cur
        type(PositionNode), pointer :: ord
        type(Node), pointer :: tail

        nullify(sorted)
        nullify(tail)

        call process_order(order, head, sorted, tail)
    end subroutine sort_list_by_position

    recursive subroutine process_order(ord, head, sorted, tail)
        type(PositionNode), pointer, intent(in) :: ord
        type(Node), pointer, intent(in) :: head
        type(Node), pointer, intent(inout) :: sorted
        type(Node), pointer, intent(inout) :: tail
        type(Node), pointer :: cur
    
        if (associated(ord)) then
            cur => head
    
            call collect_by_position(cur, ord%Position, sorted, tail)
            call process_order(ord%Next, head, sorted, tail)
        end if
    end subroutine process_order

    recursive subroutine collect_by_position(cur, pos, sorted, tail)
        type(Node), pointer, intent(in) :: cur
        character(kind=CH_, len=*), intent(in) :: pos

        type(Node), pointer, intent(inout) :: sorted
        type(Node), pointer, intent(inout) :: tail

        if (associated(cur)) then
            if (cur%Position == pos) then
                if (.not. associated(sorted)) then
                    allocate(sorted)
                    sorted%Surname = cur%Surname
                    sorted%Position = cur%Position
                    nullify(sorted%Next)
                    tail => sorted
                else
                    allocate(tail%Next)
                    tail => tail%Next
                    tail%Surname = cur%Surname
                    tail%Position = cur%Position
                    nullify(tail%Next)
                end if
            end if
            call collect_by_position(cur%Next, pos, sorted, tail)
        end if
    end subroutine collect_by_position

end module Process
