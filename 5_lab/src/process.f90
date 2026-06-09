module Module_Calculate
   use Environment
   implicit none
   private
   public :: Expression

   type :: Tree_Node
      private
      character :: ch = ' '
      type(Tree_Node), allocatable :: left
      type(Tree_Node), allocatable :: right
   end type Tree_Node

   type :: Tree
      private
      type(Tree_Node), allocatable :: root
      logical :: correct = .false.
   end type Tree

   type :: Expression
      private
      character(:), allocatable :: str
      character(:), allocatable :: result
      type(Tree) :: tree_exp
   contains
      procedure :: read => Read
      procedure :: write_source => Write_source
      procedure :: convert => Convert
      procedure :: write_result => Write_result
   end type Expression

contains

    subroutine Read(self, file)
       class(Expression), intent(inout) :: self
       character(*), intent(in) :: file
    
       character(1024) :: buf
       integer :: unit, iostat, last
    
       buf = ''
       open (newunit=unit, file=file, encoding=E_)
          read (unit, '(a)', iostat=iostat) buf
          call Handle_IO_status(iostat, 'reading prefix expression')
       close (unit)
    
       self%str = trim(buf)
    end subroutine Read

   subroutine Write_source(self, file, position)
      class(Expression), intent(in) :: self
      character(*), intent(in) :: file, position

      integer :: unit, iostat

      open (newunit=unit, file=file, position=position, encoding=E_)
         write (unit, '(a,1x,a)', iostat=iostat) 'The original prefix form:', self%str
         call Handle_IO_status(iostat, 'writing prefix expression')
      close (unit)
   end subroutine Write_source


   pure subroutine Convert(self)
      class(Expression), intent(inout) :: self

      integer :: pos

      if (allocated(self%tree_exp%root)) &
         deallocate (self%tree_exp%root)

      pos = 1
      self%tree_exp%correct = .false.

      call Parse_prefix(self%str, pos, self%tree_exp%root, self%tree_exp%correct)
      self%tree_exp%correct = self%tree_exp%correct .and. pos > len(self%str)

      if (self%tree_exp%correct .and. allocated(self%tree_exp%root)) then
         if (Check(self%tree_exp%root)) then
            self%result = Build_postfix(self%tree_exp%root)
         else
            self%tree_exp%correct = .false.
            self%result = 'Error: the prefix form is incorrect.'
         end if
      else
         self%tree_exp%correct = .false.
         self%result = 'Error: the prefix form is incorrect.'
      end if

   contains

      pure recursive function Check(nd) result(ok)
         type(Tree_Node), intent(in) :: nd
         logical :: ok

         if (nd%ch >= 'A' .and. nd%ch <= 'Z') then
            ok = .not. allocated(nd%left) .and. .not. allocated(nd%right)
         else if (nd%ch == '+' .or. nd%ch == '-' .or. nd%ch == '*' .or. nd%ch == '/') then
            ok = allocated(nd%left) .and. allocated(nd%right)
            if (ok) &
               ok = Check(nd%left) .and. Check(nd%right)
         else
            ok = .false.
         end if
      end function Check


      pure recursive function Build_postfix(nd) result(res)
         type(Tree_Node), intent(in) :: nd
         character(:), allocatable :: res

         character(:), allocatable :: left_part, right_part

         if (allocated(nd%left) .and. allocated(nd%right)) then
            left_part = Build_postfix(nd%left)
            right_part = Build_postfix(nd%right)
            res = left_part // ' ' // right_part // ' ' // nd%ch
         else
            res = nd%ch
         end if
      end function Build_postfix

   end subroutine Convert


   subroutine Write_result(self, file)
      class(Expression), intent(in) :: self
      character(*), intent(in) :: file

      integer :: unit, iostat

      open (newunit=unit, file=file, position='append', encoding=E_)
         if (self%tree_exp%correct) then
            write (unit, '(/a/a)', iostat=iostat) 'The postfix form:', self%result
         else
            write (unit, '(/a/a)', iostat=iostat) 'Error: the prefix form is incorrect:', self%str
         end if
         call Handle_IO_status(iostat, 'writing result')
      close (unit)
   end subroutine Write_result


   pure recursive subroutine Parse_prefix(s, pos, nd, ok)
      character(*), intent(in) :: s
      integer, intent(inout) :: pos
      type(Tree_Node), allocatable, intent(out) :: nd
      logical, intent(out) :: ok

      character :: current

      if (pos > len(s)) then
         ok = .false.
         return
      end if

      current = s(pos:pos)

      if (current >= 'A' .and. current <= 'Z') then
         allocate (nd)
         nd%ch = current
         pos = pos + 1
         ok = .true.
      else if (current == '+' .or. current == '-' .or. current == '*' .or. current == '/') then
         allocate (nd)
         nd%ch = current
         pos = pos + 1

         call Parse_prefix(s, pos, nd%left, ok)
         if (ok) &
            call Parse_prefix(s, pos, nd%right, ok)

         if (.not. ok) then
            if (allocated(nd)) &
               deallocate (nd)
         end if
      else
         ok = .false.
      end if
   end subroutine Parse_prefix
end module Module_Calculate
