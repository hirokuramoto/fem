subroutine stiff5(a, b, nnode, coords, nelem, lnods, astiff, c, nint)
  ! 剛性行列と外力のベクトルの作成

  implicit none
  integer(4), intent(in) :: nnode           ! 総節点数
  integer(4), intent(in) :: nelem           ! 総要素数
  integer(4), intent(in) :: lnods(2, nelem) ! コネクティビティ
  integer(4), intent(in) :: nint            ! ガウス積分のサンプル点数
  real(8), intent(inout) :: a(nnode, nnode) ! 全体剛性マトリックス
  real(8), intent(inout) :: b(nnode)        ! 全体Fベクトル
  real(8), intent(inout) :: coords(nnode)   ! 節点座標
  integer(4) i, j       ! カウンタ
  integer(4) ip1, ip2   ! 各区間毎の節点座標(1, i),(2, i)
  real(8) x             ! 節点間距離
  real(8) astiff(2, 2)  ! 要素マトリックス
  real(8) c(2)          ! 要素Fベクトル

  do i = 1, nnode
    do j = 1, nnode
      a(j, i) = 0.d0
    end do
  end do

  do i = 1, nnode
    b(i) = 0.d0
  end do

  do i = 1, nelem
    call element(coords, lnods(1, i), astiff, nint)

    x = coords(lnods(2, i)) - coords(lnods(1, i))

    c(1) = x / 2.d0
    c(2) = x / 2.d0

    ip1 = lnods(1, i)
    ip2 = lnods(2, i)
    a(ip1, ip1) = a(ip1, ip1) + astiff(1, 1)
    a(ip2, ip1) = a(ip2, ip1) + astiff(2, 1)
    a(ip1, ip2) = a(ip1, ip2) + astiff(1, 2)
    a(ip2, ip2) = a(ip2, ip2) + astiff(2, 2)
    b(ip1) = b(ip1) + c(1)
    b(ip2) = b(ip2) + c(2)
  end do

  return
end subroutine stiff5
