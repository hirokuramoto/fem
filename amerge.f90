subroutine amerge(a, lnods, astiff, ntnoel, nnode)
  !

  implicit none
  integer(4), intent(in) :: nnode           ! 総節点数
  integer(4), intent(in) :: lnods(*) ! コネクティビティ
  real(8), intent(inout) :: a(*) ! 全体剛性マトリックス
  real(8), intent(inout) :: astiff(ntnoel, *)        ! 全体Fベクトル
  integer(4) i, j, ielem  ! カウンタ
  integer(4) ntnoel       ! 各要素の節点数
  integer(4) ip(100)      !

  ! 全体合成マトリックスの作成
  do i = 1, ntnoel(ielem)
    ip(i) = lnods(i)
  end do

  do j = 1, ntnoel
    do i = 1, ntnoel
      a((ip(j) - 1) * nnode + ip(i)) = a((ip(j) - 1) * nnode + ip(i)) + astiff(i, j)
    end do
  end do

  return
end subroutine amerge
