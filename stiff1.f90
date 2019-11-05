subroutine stiff1(a, b, N)
  ! 剛性行列と外力のベクトルの作成

  implicit none
  integer(4), intent(in) :: N
  real(8),    intent(inout) :: a(N+1, N+1), b(N+1)
  integer(4) i, j
  real(8) x

  do i = 1, N + 1
    do j = 1, N + 1
      a(j, i) = 0.d0
    end do
  end do

  x = 1.d0 / dfloat(N)

  do i = 1, N
    a(i    , i    ) = a(i    , i    ) + 1.d0 / x
    a(i    , i + 1) = a(i    , i + 1) - 1.d0 / x
    a(i + 1, i    ) = a(i + 1, i    ) - 1.d0 / x
    a(i + 1, i + 1) = a(i + 1, i + 1) + 1.d0 / x

    b(i    ) = b(i    ) + x / 2.d0
    b(i + 1) = b(i + 1) + x / 2.d0
  end do

  return
end subroutine stiff1
