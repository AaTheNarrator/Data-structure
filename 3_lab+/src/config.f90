module Config
    use Environment
    implicit none

    integer, parameter :: SURNAME_LEN = 25
    character(*), parameter :: IN_FILE = '../data/input.txt'
    character(*), parameter :: OUT_FILE = 'output.txt'
end module Config
