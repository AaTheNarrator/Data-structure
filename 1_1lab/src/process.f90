module Process

    use Environment
    use Config

    implicit none

contains
    subroutine sort_array_by_position(Surnames, Positions, SortedPosition, SortSurnames, SortPosition)
        character(SURNAME_LEN), intent(in)  :: Surnames(:)
        character(POSITION_LEN), intent(in)  :: Positions(:)
        character(POSITION_LEN), intent(in)  :: SortedPosition(:)
        character(SURNAME_LEN), allocatable, intent(out) :: SortSurnames(:)
        character(POSITION_LEN), allocatable, intent(out) :: SortPosition(:)
    
        integer, allocatable :: Counts(:), Starts(:)
        integer :: i, j, n, m, k
    
        n = size(Positions)
        m = size(SortedPosition)
    
        allocate(SortSurnames(n), SortPosition(n))
        allocate(Counts(m), Starts(m))
    
        !!$omp parallel workshare
        Counts = [(count(Positions == SortedPosition(i)), i = 1, m)]
        Starts = [(sum(Counts(:i-1)) + 1, i = 1, m)]
        !!$omp end parallel workshare
    
    
        !!$omp parallel do private(j, k)
        do i = 1, m
            k = Starts(i)
            do j = 1, n
                if (Positions(j) == SortedPosition(i)) then
                    SortSurnames(k) = Surnames(j)
                    SortPosition(k) = Positions(j)
                    k = k + 1
                end if
            end do
        end do
        !!$omp end parallel do
    end subroutine
end module Process   
