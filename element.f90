subroutine element(coords, lnods, astiff, nint, ntnoel)
  implicit none

  integer(4), intent(in) :: lnods(*)          ! コネクティビティ
  integer(4), intent(in) :: nint              ! ガウス点
  integer(4), intent(in) :: ntnoel            ! 各要素の節点数
  real(8), intent(in)    :: coords(*)         ! 節点座標
  real(8), intent(inout) :: astiff(ntnoel, *) ! 要素マトリックス
  real(8) gsp(4, 4)         ! ガウス積分点の座標
  real(8) wgh(4, 4)         ! ガウス積分点の重み
  real(8) dndr(4), dndx(4)
  real(8) ajacob, detjac    ! ヤコビアン=dx/dr
  real(8) ajainv            ! ヤコビアンの逆数=dr/dx
  real(8) detwei            ! ガウス積分の w*Jの計算
  real(8) r                 ! ガウス積分点の座標
  integer(4) i, j, ir       ! カウンタ


  ! ガウス積分点の座標
  gsp(1:4, 1) = (/  0.d0               ,  0.d0               , 0.d0               , 0.d0                /)
  gsp(1:4, 2) = (/ -0.577350269189626d0,  0.577350269189626d0, 0.d0               , 0.d0                /)
  gsp(1:4, 3) = (/ -0.774596669241483d0,  0.d0               , 0.774596669241483d0, 0.d0                /)
  gsp(1:4, 4) = (/ -0.861136311594053d0, -0.339981043584856d0, 0.339981043584856d0, 0.861136311594053d0 /)

  ! ガウス積分点の重み
  wgh(1:4, 1) = (/ 2.d0               , 0.d0               , 0.d0               , 0.d0                /)
  wgh(1:4, 2) = (/ 1.d0               , 1.d0               , 0.d0               , 0.d0                /)
  wgh(1:4, 3) = (/ 0.555555555555556d0, 0.888888888888889d0, 0.555555555555556d0, 0.d0                /)
  wgh(1:4, 4) = (/ 0.347854845137454d0, 0.652145154862546d0, 0.652145154862546d0, 0.347854845137454d0 /)

  ! 要素マトリックスの初期化
  do j = 1, ntnoel
    do i = 1, ntnoel
      astiff(i, j) = 0.d0
    end do
  end do

  ! ガウス積分を使って要素マトリックスを作成
  do ir = 1, nint

    r = gsp(ir, nint)

    if (ntnoel == 2) then
      dndr(1) = -0.5d0
      dndr(2) =  0.5d0
    end if

    if (ntnoel == 3) then
      dndr(1) = r - 0.5d0
      dndr(2) = r + 0.5d0
      dndr(3) = -2.d0 * r
    end if

    if (ntnoel == 4) then
      dndr(1) = -(-1.d0 - 18.d0 * r + 27.d0 * r * r) / 16.d0
      dndr(2) = -( 1.d0 - 18.d0 * r - 27.d0 * r * r) / 16.d0
      dndr(3) = 9.d0 * (-3.d0 - 2.d0 * r + 9.d0 * r * r) / 16.d0
      dndr(4) = 9.d0 * ( 3.d0 - 2.d0 * r - 9.d0 * r * r) / 16.d0
    end if

    ! ヤコビアンの計算
    ajacob = 0.d0
    do i = 1, ntnoel
      ajacob = ajacob + dndr(i) * coords(lnods(i))
    end do

    detjac = ajacob
    ajainv = 1.d0 / ajacob

    do i = 1, ntnoel
      dndx(i) = dndr(i) * ajainv
    end do

    detwei = detjac * wgh(ir, nint)

    do j = 1, ntnoel
      do i = 1, ntnoel
        astiff(i, j) = astiff(i, j) + detwei * dndx(i) * dndx(j)
      end do
    end do
  end do

  return
end subroutine element
