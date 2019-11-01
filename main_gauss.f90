program main
  implicit none
  integer, parameter :: n = 4  ! parameter属性で初期値を指定
  integer, parameter :: i_check = 2
  real(8) a(n, n), b(n), c(n), al(n, n), ad(n, n), au(n, n), x(n), y(n), t1, t2

  call cpu_time(t1)

  call initial(a, b, c, n)
  if (i_check >= 2) call check_matrix(a, b, c, n)
  !call gauss_ver1(a, b, c, al, ad, au, x, y, n)
  !call gauss_ver21(a, c, al, ad, au, n)
  !call gauss_ver22(a, c, n)
  !call gauss_ver3(a, c, n)
  call gauss_ver4u(a, c, n)
  if (i_check >= 1) call check_solution(c, n)

  call cpu_time(t2)
  write(*, *) "cpu time =", t2 - t1
end program main
