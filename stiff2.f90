subroutine stiff2(a, b, nnode, coords, nelem, lnods)
  ! 剛性行列と外力のベクトルの作成

  implicit none
  integer(4), intent(in) :: nnode, nelem
  integer(4), intent(in) :: lnods(2, nelem)
  real(8), intent(inout) :: a(nnode, nnode), b(nnode), coords(nnode)
  integer(4) i, j
  real(8) x

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
    a(i    , i    ) = a(i    , i    ) + 1.d0 / x
    a(i    , i + 1) = a(i    , i + 1) - 1.d0 / x
    a(i + 1, i    ) = a(i + 1, i    ) - 1.d0 / x
    a(i + 1, i + 1) = a(i + 1, i + 1) + 1.d0 / x

    b(i    ) = b(i    ) + x / 2.d0
    b(i + 1) = b(i + 1) + x / 2.d0
  end do

  return
end subroutine stiff2
