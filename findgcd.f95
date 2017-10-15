program findgcd

  implicit none
  integer :: a, b, c

  write(unit=*, fmt="(t2, a)", advance="no"), "Input two integers: "
  read *, a, b
  c = gcd(a, b)
  print "(t2, a, i0, a, i0, a, i0)", "gcd(", a, ", ", b, ") = ", c

contains

  integer function gcd(a, b)
    integer, intent(in) :: a, b
    !
    ! Returns the greatest common divisor of a and b.
    !
    integer   :: aa, bb, t
    intrinsic :: modulo
    
    aa = abs(a)
    bb = abs(b) 
    do
      if ( bb == 0 ) exit
      t = modulo(aa, bb)
      aa = bb
      bb = t
    end do
    gcd = aa

  end function gcd

end program findgcd
