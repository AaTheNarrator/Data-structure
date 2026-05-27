module Process
   use Environment
   use Config

   implicit none

contains

    pure integer function rank_of_position(Position, SortedPosition) result(rank)
       character(kind=CH_), intent(in) :: Position(:)
       character(kind=CH_), intent(in) :: SortedPosition(:, :)
    
       integer              :: i
       logical, allocatable :: mask(:)
    
       allocate(mask(ubound(SortedPosition, 2)))
       mask = [(all(Position == SortedPosition(:, i)), i = 1, ubound(SortedPosition, 2))]
       rank = findloc(mask, .true., dim=1)
    
       if (rank == 0) rank = huge(rank)
    end function rank_of_position
    
    pure logical function position_gt(Pos1, Pos2, SortedPosition)
       character(kind=CH_), intent(in) :: Pos1(:), Pos2(:)
       character(kind=CH_), intent(in) :: SortedPosition(:, :)
    
       position_gt = rank_of_position(Pos1, SortedPosition) > &
                     rank_of_position(Pos2, SortedPosition)
    end function position_gt
    subroutine sort_array_by_position(Surnames, Positions, SortedPosition, SortSurnames, SortPosition)
       character(kind=CH_), intent(in)  :: Surnames(:, :)
       character(kind=CH_), intent(in)  :: Positions(:, :)
       character(kind=CH_), intent(in)  :: SortedPosition(:, :)
       character(kind=CH_), allocatable, intent(out) :: SortSurnames(:, :)
       character(kind=CH_), allocatable, intent(out) :: SortPosition(:, :)
    
       integer, allocatable :: Counts(:), Starts(:)
       integer              :: i, j, k, n, m
    
       n = ubound(Positions, 2)
       m = ubound(SortedPosition, 2)
    
       !$omp allocators allocate(align(32): SortSurnames, SortPosition, Counts, Starts)
       allocate(SortSurnames(SURNAME_LEN, n), SortPosition(POSITION_LEN, n), &
                Counts(m), Starts(m))
    
       !$omp parallel workshare
       Counts = [(count([(all(Positions(:, j) == SortedPosition(:, i)), j = 1, n)]), i = 1, m)]
       !$omp end parallel workshare
    
       Starts(1) = 1
       do i = 2, m
          Starts(i) = Starts(i - 1) + Counts(i - 1)
       end do
    
       !$omp parallel do default(none) private(i, j, k) &
       !$omp shared(m, n, Starts, Positions, Surnames, SortedPosition, SortSurnames, SortPosition)
       do i = 1, m
          k = Starts(i)
          do j = 1, n
             if (all(Positions(:, j) == SortedPosition(:, i))) then
                SortSurnames(:, k) = Surnames(:, j)
                SortPosition(:, k) = Positions(:, j)
                k = k + 1
             end if
          end do
       end do
       !$omp end parallel do
    end subroutine sort_array_by_position

end module Process
