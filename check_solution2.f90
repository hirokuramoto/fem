subroutine check_solution2(b, nnode, coords)
  implicit none
  integer(4), intent(in) :: nnode
  real(8),    intent(inout) :: b(nnode), coords(nnode)
  integer(4) i
  real(8) x

  write(*, *) 'solution vector b'

  do i = 1, nnode
      x = coords(i)
      write(*, '(11e10.3)') b(i), 0.5d0 * (x - x * x)
  end do

  return
end subroutine check_solution2
