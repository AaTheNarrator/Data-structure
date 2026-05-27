module Process
    use Environment
    use Config
    use IO_Process
    implicit none
contains
    pure recursive subroutine Process_Delete_Matching(current, target)
        type(Node), pointer, intent(inout) :: current
        character(kind=CH_, len=SURNAME_LEN), intent(in) :: target
        type(Node), pointer :: tmp

        if (associated(current)) then
            if (current%Surname == target) then
                tmp => current
                current => current%Next
                deallocate(tmp)
                call Process_Delete_Matching(current, target)
            else
                call Process_Delete_Matching(current%Next, target)
            end if
        end if
    end subroutine
end module Process
