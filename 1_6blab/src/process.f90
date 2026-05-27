module Process
    use Environment
    use Config
    use IO_Process
    implicit none
contains
    subroutine sort_list_by_position(head, order, sorted)
        type(Node), allocatable, intent(in) :: head
        type(PosNode), allocatable, intent(in) :: order
        type(Node), allocatable, intent(out) :: sorted

        if (.not. allocated(head) .or. .not. allocated(order)) return
        call process_order(order, head, sorted)
    end subroutine sort_list_by_position

    recursive subroutine process_order(ord, head, sorted)
        type(PosNode), allocatable, intent(in) :: ord
        type(Node), allocatable, intent(in) :: head
        type(Node), allocatable, intent(inout) :: sorted

        if (allocated(ord)) then
            call collect_by_position(head, ord%Position, sorted)
            call process_order(ord%Next, head, sorted)
        end if
    end subroutine process_order

    recursive subroutine collect_by_position(cur, pos, sorted)
        type(Node), allocatable, intent(in) :: cur
        character(kind=CH_, len=*), intent(in) :: pos
        type(Node), allocatable, intent(inout) :: sorted

        if (allocated(cur)) then
            if (cur%Position == pos) then
                call append_to_list(sorted, cur)
            end if
            call collect_by_position(cur%Next, pos, sorted)
        end if
    end subroutine collect_by_position

    recursive subroutine append_to_list(head, item)
        type(Node), allocatable, intent(inout) :: head
        type(Node), allocatable, intent(in)    :: item

        if (.not. allocated(head)) then
            allocate(head)
            head%Surname  = item%Surname
            head%Position = item%Position
        else
            call append_to_list(head%Next, item)
        end if
      ! Посчитать на сколько менее эффективно из-за того что мы не цепляем к концу, а каждый раз проматываем список  
    end subroutine append_to_list
end module Process
