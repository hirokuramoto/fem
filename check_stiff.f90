subroutine check_stiff(a, n)
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: a(n, n)
  integer(4) i, j

  write(*, *) 'matrix A'
  do i = 1, n
    write(*, '(11e10.3)') (a(i, j), j = 1, n)
  end do

  return
end subroutine check_stiff
