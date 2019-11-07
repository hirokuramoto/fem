program main
  implicit none
  integer, parameter :: MXNODE = 100  ! 総接点数の最大値
  integer, parameter :: MXELEM = 100  ! 総要素数の最大値

  integer(4) n_bc_given           ! 境界条件の全数
  integer(4) n_bc_nonzero         ! 非零の境界条件の数
  integer(4) i_bc_given(MXNODE)   ! 境界条件のインデックス
  integer(4) i_bc_nonzero(MXNODE) ! 非零の境界条件のインデックス
  integer(4) nnode                ! 総節点数
  integer(4) nelem                ! 総要素数
  integer(4) lnods(2, MXELEM)     ! コネクティビティ
  integer(4) nint                 ! ガウス積分のサンプル点数
  integer(4) icase
  real(8)    a(MXNODE * MXNODE)   ! 全体剛性マトリックス
  real(8)    b(MXNODE)            ! 全体Fベクトル
  real(8)    coords(MXNODE)       ! 節点座標
  real(8)    v_bc_nonzero(MXNODE) ! 非零の境界条件の値
  real(8)    ntnoel(MXELEM)       ! 各要素の節点数
  real(8)    astiff(3, 3)         ! 要素マトリックス
  real(8)    c(3)                 ! 要素Fベクトル
  real(8)    t1, t2               ! 時間計測用

  call cpu_time(t1)

  icase = 2

  ! データファイルの読み込み
  call datain6(nnode, coords, nelem, lnods, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero, icase, nint, ntnoel)

  ! 剛性マトリックスの作成
  call stiff6(a, b, nnode, coords, nelem, lnods, astiff, c, nint, ntnoel)
  call check_stiff(a, nnode)

  ! 境界条件処理
  call bound2(a, b, nnode, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero)
  call check_matrix2(a, b, nnode)

  ! ガウスの消去法
  call gauss_ver4u(a, b, nnode)
  call check_solution6(b, nnode, coords, icase)

  call cpu_time(t2)
  write(*, *) "cpu time =", t2 - t1
  stop
end program main
