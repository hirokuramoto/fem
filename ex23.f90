subroutine ex23(c, n_bc_given, i_bc_given, v_bc_given, n, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero)
  implicit none
  integer(4), intent(in) :: n
  integer(4), intent(inout) :: n_bc_given, i_bc_given(n), n_bc_nonzero, i_bc_nonzero(n)
  real(8), intent(inout)    :: c(n), v_bc_given(n), v_bc_nonzero(n)
  integer(4) i

  do i = 1, n
    c(i) = 0.d0
  end do

  ! 既知量の個数，インデックス，数値
  n_bc_given      = 3
  i_bc_given(1)   = 1
  i_bc_given(2)   = 3
  i_bc_given(3)   = 5
  v_bc_given(1)   = 0.d0
  v_bc_given(2)   = 1.d0
  v_bc_given(3)   = 0.d0

  ! 非零成分を抽出(個数，インデックス，数値)
  n_bc_nonzero    = 1
  i_bc_nonzero(1) = 3
  v_bc_nonzero(1) = 1.d0

  return
end subroutine ex23
