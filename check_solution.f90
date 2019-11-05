subroutine check_solution(b, N)
  implicit none
  integer(4), intent(in) :: N
  real(8),    intent(inout) :: b(N+1)
  integer(4) i
  real(8) x

  write(*, *) 'solution vector b'

  do i = 1, N+1
      x = dfloat(i-1) / dfloat(N)
      write(*, '(8e10.3)') b(i), 0.5d0 * (x - x * x)
  end do

  return
end subroutine check_solution
