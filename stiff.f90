subroutine stiff(a, n, nk, ak)
  implicit none
  integer(4), intent(in) :: n, nk
  real(8),    intent(in) :: ak
  real(8),    intent(inout) :: a(n, n)
  integer(4) i, j

  do i = 1, n
    do j = 1, n
      a(j, i) = 0.d0
    end do
  end do

  do i = 1, nk
    a(i    , i    ) = a(i    , i    ) + ak
    a(i    , i + 1) = a(i    , i + 1) - ak
    a(i + 1, i    ) = a(i + 1, i    ) - ak
    a(i + 1, i + 1) = a(i + 1, i + 1) + ak
  end do

  return
end subroutine stiff
