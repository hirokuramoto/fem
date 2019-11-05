subroutine stiff4(a, b, nnode, coords, nelem, lnods)
  ! 剛性行列と外力のベクトルの作成

  implicit none
  integer(4), intent(in) :: nnode, nelem
  integer(4), intent(in) :: lnods(2, nelem)
  real(8), intent(inout) :: a(nnode, nnode), b(nnode), coords(nnode)
  integer(4) i, j, ip1, ip2
  real(8) x, astiff(2, 2), c(2)

  do i = 1, nnode
    do j = 1, nnode
      a(j, i) = 0.d0
    end do
  end do

  do i = 1, nnode
    b(i) = 0.d0
  end do

  do i = 1, nelem
    x = coords(lnods(2, i)) - coords(lnods(1, i))
    astiff(1, 1) =   1.d0 / x
    astiff(1, 2) = - 1.d0 / x
    astiff(2, 1) = - 1.d0 / x
    astiff(2, 2) =   1.d0 / x
    c(1) = x / 2.d0
    c(2) = x / 2.d0

    ip1 = lnods(1, i)
    ip2 = lnods(2, i)
    a(ip1, ip1) = a(ip1, ip1) + astiff(1, 1)
    a(ip2, ip1) = a(ip2, ip1) + astiff(2, 1)
    a(ip1, ip2) = a(ip1, ip2) + astiff(1, 2)
    a(ip2, ip2) = a(ip2, ip2) + astiff(2, 2)
    b(ip1) = b(ip1) + c(1)
    b(ip2) = b(ip2) + c(2)
  end do

  return
end subroutine stiff4
