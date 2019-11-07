program test

integer(4) i, lnods(2, 4)
integer(4) a, b, c, d

  do i = 1, 4
    lnods(1, i) = i
    lnods(2, i) = i + 1
  end do

  write(*, *) lnods
  write(*, *) lnods(1, 4) ! 2

  call test2(lnods(1, 4), a, b, c, d)

  write(*, *) size(lnods)
  write(*, *) a, b, c, d



end program
