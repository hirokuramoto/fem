subroutine check_solution(b, n)
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: b(n)
  integer(4) i

  write(*, *) 'solution vector b'
  write(*, '(8e10.3)') (b(i), i = 1, n)

  return
end subroutine check_solution
