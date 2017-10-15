program extremes

  implicit none
  integer(kind=1)  :: i1
  integer(kind=2)  :: i2
  integer(kind=4)  :: i4
  integer(kind=8)  :: i8
  integer(kind=16) :: i16
  real(kind=4)     :: r4
  real(kind=8)     :: r8
  real(kind=10)    :: r10
  real(kind=16)    :: r16

  character(len=*), parameter :: IFMT = "(t2, a12, i10, i10, i15, i25, i45)"

  print *, "Integer data types:"

  print "(/, t14, a10, a10, a15, a25, a45, /)",                         &
        "1-byte", "2-byte", "4-byte", "8-byte", "16-byte"
  print IFMT, "digits",                                                 &
        digits(i1), digits(i2), digits(i4), digits(i8), digits(i16)
  print IFMT, "huge",                                                   &
        huge(i1), huge(i2), huge(i4), huge(i8), huge(i16)
  print IFMT, "radix",                 &
        radix(i1), radix(i2), radix(i4),  radix(i8), radix(i16)
  print IFMT,  "range",                 &
        range(i1), range(i2), range(i4),  range(i8), range(i16)

  print "(/, t2, a)", "Real data types:"
  print "(/, t14, a12, a12, a16, a16, /)",                             &
        "4-byte", "8-byte", "10-byte", "16-byte"
  print "(t2, a12, e12.4, e12.4, e16.4, e16.4)", "epsilon",                &
        epsilon(r4), epsilon(r8), epsilon(r10), epsilon(r16)
  print "(t2, a12, i12, i12, i16, i16)", "precision",                      &
        precision(r4), precision(r8), precision(r10), precision(r16)
!  print RFMT, "huge",                                                   &
!        huge(r4), huge(r8), huge(r10), huge(r16)
!  print RFMT, "tiny",                                                   &
!        tiny(r4), tiny(r8), tiny(r10), tiny(r16)
  print "(t2, a12, i12, i12, i16, i16)", "maxexponent",                    &
        maxexponent(r4), maxexponent(r8), maxexponent(r10), maxexponent(r16)
  print "(t2, a12, i12, i12, i16, i16)", "minexponent",                    &
        minexponent(r4), minexponent(r8), minexponent(r10), minexponent(r16)

end program extremes
