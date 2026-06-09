module Config
    use Environment
    implicit none
    integer, parameter :: LINE_LEN = 1
    character(*), parameter :: TEXT_FILE = '../data/input.txt'
    character(*), parameter :: CMD_FILE  = '../data/commands.txt'
    character(*), parameter :: OUT_FILE  = 'output.txt'
end module Config
