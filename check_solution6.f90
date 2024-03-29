subroutine check_solution6(b, nnode, coords, icase)
  implicit none
  integer(4), intent(in)    :: nnode
  integer(4), intent(in)    :: icase
  real(8),    intent(inout) :: b(*)
  real(8),    intent(inout) :: coords(*)
  integer(4) i
  real(8) x, u

  write(*, *) 'solution vector b'

  do i = 1, nnode
      x = coords(i)
      if (icase <= 20) u = -0.5d0 * x * x - 0.5d0 * x + 1
      if (icase >= 20) u = -0.5d0 * x * x + 1.5d0 * x + 1
      write(*, '(11e13.6)') b(i), u
  end do

  return
end subroutine check_solution6
