subroutine stiff7(a, b, nnode, coords, nelem, lnods, astiff, c, nint, ntnoel)
  ! 剛性行列と外力のベクトルの作成

  implicit none
  integer(4), intent(in) :: nnode           ! 総節点数
  integer(4), intent(in) :: nelem           ! 総要素数
  integer(4), intent(in) :: lnods(4, *)     ! コネクティビティ
  integer(4), intent(in) :: nint            ! ガウス積分のサンプル点数
  integer(4), intent(in) :: ntnoel(*)       ! 各要素の節点数
  real(8), intent(in)    :: coords(*)       ! 節点座標
  real(8), intent(out)   :: a(nnode, *)     ! 全体剛性マトリックス
  real(8), intent(out)   :: b(*)            ! 全体Fベクトル
  real(8), intent(out)   :: astiff(*)       ! 要素マトリックス
  real(8), intent(out)   :: c(*)            ! 要素Fベクトル
  integer(4) i, j, ielem  ! カウンタ
  integer(4) ip1, ip2     ! 各区間毎の節点座標(1, i),(2, i)
  integer(4) ip(100)      !
  real(8) x               ! 節点間距離


  ! 全体合成マトリックスの初期化
  do i = 1, nnode
    do j = 1, nnode
      a(j, i) = 0.d0
    end do
  end do

  ! 全体Fベクトルの初期化
  do i = 1, nnode
    b(i) = 0.d0
  end do

  do ielem = 1, nelem
    ! 要素マトリックスの作成
    call element(coords, lnods(1, ielem), astiff, nint, ntnoel(ielem))

    call amerge(a, lnods(1, ielem), astiff, ntnoel(ielem), nnode)

    do i = 1, ntnoel(ielem)
      ip(i) = lnods(i, ielem)
    end do

    ! 節点間距離の計算　x(i, 2) - x(1, 1)
    x = coords(lnods(2, ielem)) - coords(lnods(1, ielem))

    ! Fベクトルの計算．1次の補間関数の場合
    if (ntnoel(ielem) == 2) then
      c(1) = x / 2.d0
      c(2) = x / 2.d0
    end if

    ! Fベクトルの計算．2次の補間関数の場合
    if (ntnoel(ielem) == 3) then
      c(1) = x / 6.d0
      c(2) = x / 6.d0
      c(3) = x * 2.d0 / 3.d0
    end if

    ! Fベクトルの計算．3次の補間関数の場合
    if (ntnoel(ielem) == 4) then
      c(1) = x / 8.d0
      c(2) = x / 8.d0
      c(3) = x * 3.d0 / 8.d0
      c(4) = x * 3.d0 / 8.d0
    end if

    do i = 1, ntnoel(ielem)
      b(ip(i)) = b(ip(i)) + c(i)
    end do
  end do

  return
end subroutine stiff7
