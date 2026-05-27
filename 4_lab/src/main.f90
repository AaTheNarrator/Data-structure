program lab_4_counting
   use Environment
   use Config
   use Process

   implicit none

   type(Counting_Game) :: Game

   call Game%Read(IN_FILE)

   call Game%Write_Input(OUT_FILE)

   call Game%Prepare()

   call Game%Write_Result(OUT_FILE)
end program lab_4_counting
