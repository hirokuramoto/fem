subroutine bound1(a, c, n, n_bc_given, i_bc_given, v_bc_given)
  implicit none
  integer(4), intent(in) :: n, n_bc_given, i_bc_given(*)
  real(8), intent(in)    :: v_bc_given(*)
  real(8), intent(inout) :: a(n, *), c(*)
  integer(4) i, k

  do i = 1, n_bc_given  ! 右辺の計算
    do k = 1, n
      c(k) = c(k) - v_bc_given(i) * a(k, i_bc_given(i))
    end do
  end do

  do i = 1, n_bc_given
    c(i_bc_given(i)) = v_bc_given(i)
  end do
  
  do i = 1, n_bc_given  ! 係数マトリックスの計算
    do k = 1, n
      a(k, i_bc_given(i)) = 0.d0
      a(i_bc_given(i), k) = 0.d0
    end do
    a(i_bc_given(i), i_bc_given(i)) = 1.d0
  end do
  return
end subroutine bound1
