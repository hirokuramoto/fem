subroutine ex13(c, n_bc_given, i_bc_given, v_bc_given, n)
  implicit none
  integer(4), intent(in) :: n
  integer(4), intent(inout) :: n_bc_given, i_bc_given(n)
  real(8), intent(inout)    :: c(n), v_bc_given(n)
  integer(4) i


  do i = 1, n
    c(i) = 0.d0
  end do

  n_bc_given    = 3     ! 既知量の個数
  i_bc_given(1) = 1     ! 既知量のインデックス
  i_bc_given(2) = 3
  i_bc_given(3) = 5
  v_bc_given(1) = 0.d0  ! 既知量
  v_bc_given(2) = 1.d0
  v_bc_given(3) = 0.d0

  return
end subroutine ex13
