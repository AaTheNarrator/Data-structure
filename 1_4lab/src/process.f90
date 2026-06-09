module Process
    use Environment
    use Config
    use IO_Process, only : EmployeeList

    implicit none

contains

    subroutine sort_array_by_position(Employees, SortedPosition, SortedEmployees)
        type(EmployeeList), intent(in) :: Employees
        character(kind=CH_, len=*), intent(in) :: SortedPosition(:)
        type(EmployeeList), intent(out) :: SortedEmployees
    
        integer, allocatable :: Counts(:), Starts(:)
        integer :: i, j, k, n, m
    
        n = size(Employees%Positions)
        m = size(SortedPosition)
    
        allocate(Counts(m), Starts(m), &
            SortedEmployees%Surnames(n), SortedEmployees%Positions(n))
    
        !!$omp parallel workshare
        Counts = [(count(Employees%Positions == SortedPosition(i)), i = 1, m)]
        Starts = [(sum(Counts(:i-1)) + 1, i = 1, m)]
        !!$omp end parallel workshare
    
        !!$omp parallel do default(none) private(i, j, k) &
        !!$omp shared(m, n, Starts, Employees, SortedPosition, SortedEmployees)
        do i = 1, m
            k = Starts(i)
            do j = 1, n
                if (Employees%Positions(j) == SortedPosition(i)) then
                    SortedEmployees%Surnames(k)  = Employees%Surnames(j)
                    SortedEmployees%Positions(k) = Employees%Positions(j)
                    k = k + 1
                end if
            end do
        end do
        !!$omp end parallel do
    
    end subroutine sort_array_by_position

end module Process
