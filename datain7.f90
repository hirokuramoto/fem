subroutine datain7(nnode,coords,nelem,lnods,n_bc_given,i_bc_given,n_bc_nonzero,i_bc_nonzero,v_bc_nonzero,icase,nint,ntnoel)
  ! データファイルの読み込み

  implicit none
  integer(4), intent(in)  :: icase
  integer(4), intent(out) :: nnode            ! 総節点数
  integer(4), intent(out) :: nelem            ! 総要素数
  integer(4), intent(out) :: lnods(4, *)      ! コネクティビティ
  integer(4), intent(out) :: nint             ! ガウス積分のサンプル点数
  integer(4), intent(out) :: n_bc_given       ! 境界条件の全数
  integer(4), intent(out) :: i_bc_given(*)    ! 境界条件のインデックス
  integer(4), intent(out) :: n_bc_nonzero     ! 非零の境界条件の数
  integer(4), intent(out) :: i_bc_nonzero(*)  ! 非零の境界条件のインデックス
  integer(4), intent(out) :: ntnoel(*)        ! 各要素の節点数
  real(8),    intent(out) :: coords(*)        ! 節点座標
  real(8),    intent(out) :: v_bc_nonzero(*)  ! 非零の境界条件の値
  integer(4) i, j                             ! カウンタ
  integer(4) itemp                            ! データファイル上のデータインデックス
  integer(4) ibc                              ! 境界条件の有無（1=有り，0=無し）

  if (icase == 11) open(10, file='case11.dat')
  if (icase == 12) open(10, file='case12.dat')
  if (icase == 13) open(10, file='case13.dat')
  if (icase == 21) open(10, file='case21.dat')
  if (icase == 22) open(10, file='case22.dat')
  if (icase == 23) open(10, file='case23.dat')

  ! 境界条件の数をリセット
  n_bc_given = 0

  ! 総接点数の読み込み
  read(10, *) nnode

  ! 境界条件の有無と節点座標を読み込み
  do i = 1, nnode
    read(10, *) itemp, ibc, coords(i)
    ! 境界条件の数とインデックスを読み込み
    if (ibc == 1) then
      n_bc_given = n_bc_given + 1
      i_bc_given(n_bc_given) = i
    end if
  end do

  ! 総要素数の読み込み
  read(10, *) nelem

  ! コネクティビティの読み込み
  do i = 1, nelem
    read(10, *) itemp, ntnoel(i), (lnods(j, i), j = 1, ntnoel(i))
  end do

  ! 非零の境界条件の数を読み込み
  read(10, *) n_bc_nonzero

  ! 非零の境界条件のインデックスと値を読み込み
  do i = 1, n_bc_nonzero
    read(10, *) itemp, i_bc_nonzero(i), v_bc_nonzero(i)
  end do

  ! ガウス積分のサンプル点数を読み込み
  read(10, *) nint

  close(10)

  return

end subroutine datain7
