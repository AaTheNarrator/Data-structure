program lab_3_variant_11
   use Environment
   use Config
   use IO_Process

   implicit none

   type(Surname_List) :: Surnames
   character(kind=CH_, len=SURNAME_LEN) :: Target_Surname

   call Surnames%Read(IN_FILE)

   call Surnames%Write(OUT_FILE, 'Input list:', 'rewind')

   call Surnames%Remove_Last_And_Get(Target_Surname)

   call Surnames%Delete(Target_Surname)

   call Surnames%Write(OUT_FILE, 'Output list:', 'append')
end program lab_3_variant_11
