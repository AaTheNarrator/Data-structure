module Config
    use Environment

    implicit none

    integer, parameter :: SURNAME_LEN = 15
    integer, parameter :: POSITION_LEN = 15

    character(*), parameter :: S_FORMAT = '(a15, 1x, a15)'
    character(*), parameter :: IN_FILE = '../data/input.txt'
    character(*), parameter :: RECORD_FILE = 'input_records.bin'
    character(*), parameter :: ORDER_FILE = '../data/order.txt'
    character(*), parameter :: OUT_FILE = 'output.txt'

end module Config
