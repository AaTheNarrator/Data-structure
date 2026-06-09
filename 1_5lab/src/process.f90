module Process
    use Environment
    use Config
    use IO_Process, only : EmployeeList

    implicit none

contains

    pure integer function rank_of_position(Position, SortedPosition) result(rank)
        character(kind=CH_, len=*), intent(in) :: Position
        character(kind=CH_, len=*), intent(in) :: SortedPosition(:)

        integer :: i

        rank = huge(rank)
        do i = 1, size(SortedPosition)
            if (trim(Position) == trim(SortedPosition(i))) then
                rank = i
                return
            end if
        end do
    end function rank_of_position


    pure recursive subroutine fill_by_rank(Employees, Ranks, Starts, i, m, SortedEmployees)
        type(EmployeeList), intent(in) :: Employees
        integer, intent(in) :: Ranks(:), Starts(:)
        integer, intent(in) :: i, m
        type(EmployeeList), intent(inout) :: SortedEmployees

        integer :: k, cnt

        if (i > m) return

        k   = Starts(i)
        cnt = count(Ranks == i)

        if (cnt > 0) then
            SortedEmployees%Surnames(k:k+cnt-1) = pack(Employees%Surnames,  Ranks == i)
            SortedEmployees%Positions(k:k+cnt-1) = pack(Employees%Positions, Ranks == i)
        end if

        call fill_by_rank(Employees, Ranks, Starts, i+1, m, SortedEmployees)
    end subroutine fill_by_rank


    subroutine sort_array_by_position(Employees, SortedPosition, SortedEmployees)
        type(EmployeeList), intent(in) :: Employees
        character(kind=CH_, len=*), intent(in) :: SortedPosition(:)
        type(EmployeeList), intent(out) :: SortedEmployees

        integer, allocatable :: Counts(:), Starts(:), Ranks(:)
        integer :: n, m, i   ! <-- ВАЖНО

        n = size(Employees%Surnames)
        m = size(SortedPosition)

        allocate(SortedEmployees%Surnames(n), SortedEmployees%Positions(n), &
            Counts(m), Starts(m), Ranks(n))

        Ranks = [(rank_of_position(Employees%Positions(i), SortedPosition), i = 1, n)]

        !!$omp parallel workshare
        Counts = [(count(Ranks == i), i = 1, m)]

        Starts = [(sum(Counts(:i-1)) + 1, i = 1, m)]
        !!$omp end parallel workshare
        call fill_by_rank(Employees, Ranks, Starts, 1, m, SortedEmployees)
    end subroutine sort_array_by_position

end module Process
