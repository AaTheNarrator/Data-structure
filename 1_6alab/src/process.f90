module Process
    use Environment
    use Config
    use IO_Process

    implicit none

contains

    subroutine sort_list_by_position(head, order, sorted)
        type(Node), pointer, intent(in) :: head
        type(PositionNode), pointer, intent(in) :: order
        type(Node), pointer, intent(out) :: sorted

        type(Node), pointer :: tail
        type(PositionNode), pointer :: ord
        type(Node), pointer :: cur

        nullify(sorted)
        nullify(tail)

        ord => order

        do while (associated(ord))
            cur => head

            do while (associated(cur))
                if (cur%Position == ord%Position) then
                    call append_node(sorted, tail, cur%Surname, cur%Position)
                end if

                cur => cur%Next
            end do

            ord => ord%Next
        end do

    end subroutine sort_list_by_position


    subroutine append_node(sorted, tail, surname, position)
        type(Node), pointer, intent(inout) :: sorted
        type(Node), pointer, intent(inout) :: tail

        character(kind=CH_, len=SURNAME_LEN), intent(in) :: surname
        character(kind=CH_, len=POSITION_LEN), intent(in) :: position

        type(Node), pointer :: new_node

        allocate(new_node)

        new_node%Surname = surname
        new_node%Position = position
        nullify(new_node%Next)

        if (.not. associated(sorted)) then
            sorted => new_node
            tail => new_node
        else
            tail%Next => new_node
            tail => new_node
        end if

    end subroutine append_node

end module Process
