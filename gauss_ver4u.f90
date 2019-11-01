subroutine gauss_ver4u(a, c, n) !三角分解の配列合理化
  implicit none
  integer(4), intent(in) :: n
  real(8),    intent(inout) :: a(n, n), c(n)
  integer(4) i, j, k
  real(8) temp, temp_u, temp_l

  ! 行列aから[L],[D],[U]に分解する
  ! [A]の対角項に[D],上下三角部分に[L],[U]の対角項以外を記憶する

  a(1, 2) = a(1, 2) / a(1, 1)

  a(2, 2) = a(2, 2) - a(1, 2) * a(1, 1) * a(1, 2)

  do j = 3, n
    do i = 2, j - 1
      temp_u = 0.d0
      do k = 1, i - 1
        temp_u = temp_u + a(k, i) * a(k, j)
      end do

      a(i, j) = a(i, j) - temp_u
    end do

    do i = 1, j - 1
      a(i, j) = a(i, j) / a(i, i)
    end do

    temp = 0.d0
    do k = 1, j - 1
      temp = temp + a(k, j) * a(k, k) * a(k, j)
    end do
    a(j, j) = a(j, j) - temp
  end do

  ! 分解した[L],[D],[U]を使って、未知ベクトル{b}を求める
  ! ①下三角行列（前進代入）
  do i = 2, n
    do j = 1 , i - 1
      c(i) = c(i) - a(j,i) * c(j)
    end do
  end do

  ! ②対角項
  do i = 1, n
      c(i) = c(i) / a(i, i)
  end do

  ! ③上三角行列（後退消去）
  do j = n, 2, -1
    do i = 1, j - 1
      c(i) = c(i) - a(i, j) * c(j)
    end do
  end do

  return

end subroutine gauss_ver4u
