program lab_5
   use Environment
   use Module_Calculate
   implicit none

   character(*), parameter :: In_File = '../data/input.txt'
   character(*), parameter :: Out_File = 'output.txt'

   type(Expression) :: Work

   call Work%read(In_File)
   call Work%write_source(Out_File, 'rewind')
   call Work%convert()
   call Work%write_result(Out_File)
end program lab_5
