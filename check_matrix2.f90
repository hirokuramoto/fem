subroutine check_matrix2(a, c, n)
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: a(n, n), c(n)
  integer(4) i, j

  write(*, *) 'matrix A'
  do i = 1, n
    write(*, '(11e12.4)') (a(i, j), j = 1, n)
  end do

  write(*, *) 'vector c'
  write(*, '(11e12.4)') (c(i), i = 1, n)

  return
end subroutine check_matrix2
