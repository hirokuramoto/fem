subroutine datain3(nnode, coords, nelem, lnods, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero, icase)
  ! データファイルの読み込み

  implicit none
  integer(4), intent(in)  :: icase
  integer(4), intent(out) :: nnode, nelem, lnods(2, *), i_bc_given(*), n_bc_given, i_bc_nonzero(*), n_bc_nonzero
  real(8),    intent(out) :: coords(*), v_bc_nonzero(*)
  integer(4) i, j, itemp, ibc

  if (icase == 1) open(10, file='case1.dat')
  if (icase == 2) open(10, file='case2.dat')

  n_bc_given = 0
  read(10, *) nnode
  do i = 1, nnode
    read(10, *) itemp, ibc, coords(i)
    if (ibc == 1) then
      n_bc_given = n_bc_given + 1
      i_bc_given(n_bc_given) = i
    end if
  end do

  read(10, *) nelem
  do i = 1, nelem
    read(10, *) itemp, (lnods(j, i), j = 1, 2)
  end do

  read(10, *) n_bc_nonzero
  do i = 1, n_bc_nonzero
    read(10, *) itemp, i_bc_nonzero(i), v_bc_nonzero(i)
  end do

  close(10)

  return
end subroutine datain3
