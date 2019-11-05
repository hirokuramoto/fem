subroutine datain(nnode, coords, nelem, lnods)
  ! データファイルの読み込み

  implicit none
  integer(4), intent(out) :: nnode, nelem, lnods(2, *)
  real(8),    intent(out) :: coords(*)
  integer(4) i, j, itemp

  open(10, file='temp.dat')

  read(10, *) nnode
  do i = 1, nnode
    read(10, *) itemp, coords(i)
  end do

  read(10, *) nelem
  do i = 1, nelem
    read(10, *) itemp, lnods(1, i), lnods(2, i)
  end do

  close(10)

  return
end subroutine datain
