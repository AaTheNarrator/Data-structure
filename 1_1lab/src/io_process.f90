module IO_Process
       
    use Environment
    use Config
      
    implicit none
    
contains
    
    function count_of_people(filename) result(n_people)
        character(*), intent(in) :: filename
        integer                  :: n_people
        integer                  :: In, IO
          
        n_people = 0
    
        open(newunit=In, file=filename, action='read')
            read(In, *, iostat=IO)
            do while(IO == 0)
                read(In, *, iostat=IO)
                n_people = n_people + 1
            end do
        close (In)
    
    end function count_of_people
    
    subroutine read_original_list (Input_File, Surnames, Positions)
    
        character(*),                                   intent(in)  :: Input_File 
        character(SURNAME_LEN,  kind=CH_), allocatable, intent(out) :: Surnames(:)
        character(POSITION_LEN, kind=CH_), allocatable, intent(out) :: Positions(:)
          
        integer                                                     :: In, IO, i, People_Amount
    
        People_Amount = count_of_people(Input_File)
        !$omp allocators allocate(align(32): Surnames, Positions)
        allocate(Surnames(People_Amount), Positions(People_Amount))
        !$omp end allocators   
    
        open (file=Input_File, encoding=E_, newunit=In)
            read (In, S_FORMAT, iostat=IO) (Surnames(i), Positions(i), &
                                           i = 1, People_Amount)
        close (In)
      
    end subroutine read_original_list
      
    subroutine read_order(filename, Sorted_Positions)
        character(*), intent(in) :: filename
        character(POSITION_LEN, kind=CH_), allocatable, intent(out) :: Sorted_Positions(:)
        integer :: In, IO, n, i
    
        n = count_of_people(filename)
    
        allocate(Sorted_Positions(n))
    
        open(file=filename, encoding=E_, newunit=In, action='read')
            read(In, '(a)', iostat=IO) (Sorted_Positions(i), i = 1, N)
        close(In)
    end subroutine
    
    subroutine write_original_list(Output_File, Surnames, Positions, Message, position)
    
        character(*),                      intent(in) :: Output_File, Message, position
        character(SURNAME_LEN,  kind=CH_), intent(in) :: Surnames(:)
        character(POSITION_LEN, kind=CH_), intent(in) :: Positions(:)
          
        integer                            :: Out, IO, i
    
        open  (file=Output_File, encoding=E_, newunit=Out, position=position)
        write (Out, '(/a)') Message   
        write (Out, S_FORMAT, iostat=IO) (Surnames(i), Positions(i), &
                                           i = 1, Size(Surnames))
        close (Out)
    
    end subroutine write_original_list
end module IO_Process   
