module Process
    use Environment
    use Config
    use IO_Process
    implicit none

contains

    pure integer function rank(pos, order)
        character(kind=CH_, len=*), intent(in) :: pos
        character(kind=CH_, len=*), intent(in) :: order(:)
        integer :: i

        rank = huge(1)
        do i=1,size(order)
            if (trim(pos) == trim(order(i))) then
                rank = i
                return
            end if
        end do
    end function

    subroutine sort_array_by_position(arr, order, res)
        type(Employee), intent(in) :: arr(:)
        character(kind=CH_, len=*), intent(in) :: order(:)
        type(Employee), allocatable, intent(out) :: res(:)
    
        integer :: n, m, i, j, k
        integer, allocatable :: counts(:), starts(:), ranks(:)
    
        n = size(arr)
        m = size(order)
    
        allocate(res(n), counts(m), starts(m), ranks(n))
    
        !!$omp parallel do default(none) private(i) shared(n, arr, order, ranks)
        do i = 1, n
            ranks(i) = rank(arr(i)%Position, order)
        end do
        !!$omp end parallel do
    
        !!$omp parallel workshare
        counts = [(count(ranks == i), i = 1, m)]
        Starts = [(sum(Counts(:i-1)) + 1, i = 1, m)]
        !!$omp end parallel workshare
    
        !!$omp parallel do default(none) private(i, j, k) shared(m, n, starts, ranks, arr, res)
        do i = 1, m
            k = starts(i)
            do j = 1, n
                if (ranks(j) == i) then
                    res(k) = arr(j)
                    k = k + 1
                end if
            end do
        end do
        !!$omp end parallel do
    end subroutine
end module Process
