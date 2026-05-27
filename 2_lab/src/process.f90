module Process
    use Environment
    use IO_Process
    implicit none
contains
    recursive subroutine Process_Scrolling(Text_Head, Cmd_Head, Window_Size, Current_Pos, Result)
        type(Node), pointer, intent(in) :: Text_Head, Cmd_Head
        integer, intent(in) :: Window_Size
        integer, intent(inout) :: Current_Pos
        type(List), intent(inout) :: Result
        character(kind=CH_, len=1) :: Cmd
        type(Node), pointer :: Temp_Node
        if (associated(Cmd_Head)) then
            Cmd = Cmd_Head%Line(1:1)
            call Result%Append(Cmd)
            select case(Cmd)
                case(char(70, CH_))
                    Temp_Node => Get_Nth(Text_Head, Current_Pos+Window_Size)
                    if (associated(Temp_Node)) Current_Pos = Current_Pos + 1
                case(char(66, CH_))
                    if (Current_Pos > 1) Current_Pos = Current_Pos - 1
            end select
            call Save_Window(Get_Nth(Text_Head, Current_Pos), Window_Size, Result)
            call Process_Scrolling(Text_Head, Cmd_Head%Next, Window_Size, Current_Pos, Result)
        end if
    end subroutine

    recursive subroutine Save_Window(Start, Count, Result)
        type(Node), pointer, intent(in) :: Start
        integer, intent(in) :: Count
        type(List), intent(inout) :: Result
        if (Count > 0 .and. associated(Start)) then
            call Result%Append(Start%Line)
            call Save_Window(Start%Next, Count-1, Result)
        end if
    end subroutine

    recursive function Get_Nth(Head, Pos) result(Res)
        type(Node), pointer, intent(in) :: Head
        integer, intent(in) :: Pos
        type(Node), pointer :: Res
        if (.not. associated(Head)) then
            nullify(Res)
        else if (Pos == 1) then
            Res => Head
        else
            Res => Get_Nth(Head%Next, Pos-1)
        end if
    end function
end module Process
