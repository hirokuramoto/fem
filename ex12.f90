subroutine ex12(c, n_bc_given, i_bc_given, v_bc_given, n)
  implicit none
  integer(4), intent(in) :: n
  integer(4), intent(inout) :: n_bc_given, i_bc_given(n)
  real(8), intent(inout)    :: c(n), v_bc_given(n)
  integer(4) i


  do i = 1, n
    c(i) = 0.d0
  end do
  c(3) = 1.d0

  n_bc_given    = 2     ! 既知量の個数
  i_bc_given(1) = 1     ! 既知量のインデックス
  i_bc_given(2) = 5
  v_bc_given(1) = 0.d0  ! 既知量
  v_bc_given(2) = 0.d0

  return
end subroutine ex12
