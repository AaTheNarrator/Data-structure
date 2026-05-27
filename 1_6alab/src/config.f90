module Config
    use Environment

    implicit none

    integer, parameter :: SURNAME_LEN = 15
    integer, parameter :: POSITION_LEN = 15

    character(*), parameter :: IN_FILE = '../data/input.txt'
    character(*), parameter :: ORDER_FILE = '../data/order.txt'
    character(*), parameter :: OUT_FILE = 'output.txt'

end module Config
