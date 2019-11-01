subroutine initial(a, b, c, n)
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: a(n, n), b(n), c(n)
  integer(4) i, j
  real(8) temp

  do i = 1, n
    do j = 1, i
      a(i, j) = dfloat(i)
      a(j, i) = dfloat(i)
    end do
  end do

  do i = 1, n
    b(i) = dfloat(i)
  end do

  do i = 1, n
    temp = 0.d0
    do j = 1, n
      temp = temp + a(i, j) * b(j)
    end do
    c(i) = temp
  end do
  
  return
end subroutine initial
