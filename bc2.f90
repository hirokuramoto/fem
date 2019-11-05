subroutine bc2(n_bc_given, i_bc_given, n_bc_nonzero, nnode)
  implicit none
  integer(4), intent(in) :: nnode
  integer(4), intent(inout) :: n_bc_given, i_bc_given(nnode), n_bc_nonzero
  integer(4) i

  ! 既知量の個数，インデックス
  n_bc_given      = 2
  i_bc_given(1)   = 1
  i_bc_given(2)   = nnode

  ! 非零成分を抽出(個数，インデックス，数値)
  n_bc_nonzero    = 0

  return
end subroutine bc2
