program explicit_euler
  
  implicit none
  
  integer ::  i, n
  real :: y, t, h, f, sol
  
  print *, "enter number of iterations"
  read *, n
  h = 1.0/real(n)
  
  t = 0.0
  y = 0.0
  
  f = t*exp(3.0*t)-2.0*y
  sol = y + h*f
  t = t + h
  do i=1, n, 1
     f = t*exp(3.0*t)-2.0*sol
     sol = sol + h*f
     t = t + h
  end do
  
  print *, sol

end program explicit_euler
