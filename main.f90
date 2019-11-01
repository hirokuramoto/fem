program main
  implicit none
  integer, parameter :: n = 5       ! parameter属性で初期値を指定
  integer, parameter :: i_check = 3
  integer, parameter :: i_type  = 1
  integer, parameter :: nk = 4      ! n=4
  real, parameter    :: ak = 1.0d0     ! ばね定数

  integer(4) n_bc_given, n_bc_nonzero, i_bc_given(n), i_bc_nonzero(n)
  real(8) a(n, n), b(n), c(n), v_bc_given(n), v_bc_nonzero(n)
  real(8) t1, t2  ! 時間計測用

  call cpu_time(t1)

  call stiff(a, n, nk, ak)                  ! 剛性マトリックスの作成
  if (i_check >= 3) call check_stiff(a, n)  ! 剛性マトリックスの確認

  if (i_type == 1) call ex13(c, n_bc_given, i_bc_given, v_bc_given, n)
  if (i_type == 2) call ex23(c, n_bc_given, i_bc_given, v_bc_given, n, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero)

  if (i_type == 1) call bound1(a, c, n, n_bc_given, i_bc_given, v_bc_given)
  if (i_type == 2) call bound2(a, c, n, n_bc_given, i_bc_given, n_bc_nonzero, i_bc_nonzero, v_bc_nonzero)
  if (i_check >= 2) call check_matrix2(a, c, n)

  call gauss_ver4u(a, c, n)
  if (i_check >= 1) call check_solution(c, n)

  call cpu_time(t2)
  write(*, *) "cpu time =", t2 - t1
  stop
end program main
