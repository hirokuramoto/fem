subroutine check_matrix(a, b, c, n)
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: a(n, n), b(n), c(n)
  integer(4) i, j

  write(*, *) 'matrix A'
  do i = 1, n
    write(*, '(8e10.3)') (a(i, j), j = 1, n)
  end do

  write(*, *) 'vector b'
  write(*, '(8e10.3)') (b(i), i = 1, n)

  write(*, *) 'vector c'
  write(*, '(8e10.3)') (c(i), i = 1, n)

  return
end subroutine check_matrix
