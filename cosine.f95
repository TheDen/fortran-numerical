program cosine

  implicit none

  complex :: z, series_approx
  integer :: n
 
  print *, "Taylor series approximation of cos(z)."
  print *, "Input z:"
  read *, z
  print *, "Input number of terms:"
  read *, n
  series_approx = cosine_series(z, n)
  print *, "           Series = ", series_approx
  print *, "Truncation error  = ", series_approx - cos(z)
 
contains
 
  complex function cosine_series(z, n) 
  ! 
  ! Sums the first n terms of the Taylor series for cos(z).
  ! 
  complex, intent(in) :: z
  integer, intent(in) :: n

  complex :: s, t, zsqr
  integer :: k

  s = 1.0
  t = 1.0
  zsqr = z * z
  do k = 1, n
    t = - t * zsqr / ( (2*k-1)*(2*k) )
    s = s + t
  end do
  cosine_series = s
 
  end function cosine_series
 
end program cosine
