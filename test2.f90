subroutine test2(lnods, a, b, c, d)

  integer(4), intent(in) :: lnods(*)
  integer(4), intent(out):: a, b, c, d


  a = lnods(1)
  b = lnods(2)
  c = lnods(3)
  d = lnods(4)


  return
end subroutine
