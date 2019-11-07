subroutine check_stiff(a, nnode)
  implicit none
  integer(4), intent(in) :: nnode
  real(8),    intent(inout) :: a(nnode, *)
  integer(4) i, j

  write(*, *) 'matrix A'
  do i = 1, nnode
    write(*, '(11e10.3)') (a(i, j), j = 1, nnode)
  end do

  return
end subroutine check_stiff
