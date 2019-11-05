program main
  implicit none
  integer, parameter :: MXNODE = 100       ! parameter属性で初期値を指定
  integer, parameter :: MXELEM = 100

  integer(4) n_bc_given, n_bc_nonzero, i_bc_given(MXNODE), i_bc_nonzero(MXNODE)
  integer(4) nnode, nelem, lnods(2, MXELEM), icase
  real(8) a(MXNODE * MXNODE), b(MXNODE), coords(MXNODE), v_bc_nonzero(MXNODE)
  real(8) t1, t2  ! 時間計測用

  call cpu_time(t1)

  icase = 2

  call datain3(nnode, coords, nelem, lnods, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero, icase)

  call stiff2(a, b, nnode, coords, nelem, lnods)    ! 剛性マトリックスの作成
  call check_stiff(a, nnode)  ! 剛性マトリックスの確認

  call bound2(a, b, nnode, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero)
  call check_matrix2(a, b, nnode)

  call gauss_ver4u(a, b, nnode)
  call check_solution3(b, nnode, coords, icase)

  call cpu_time(t2)
  write(*, *) "cpu time =", t2 - t1
  stop
end program main
