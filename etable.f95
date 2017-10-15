program etable

  implicit none

  integer, parameter :: DP = selected_real_kind(15)

  integer  :: n
  real(DP) :: Sn, t

  print "(/, t12, a, /)", "  n   approximation to e"
  Sn = 1.0_DP
  t  = 1.0_DP
  do n = 1, 20
    t = t / real(n)
    Sn = Sn + t
    print "(t12, i4, f20.15)", n, Sn
  end do
  print "(/, t2, a, f20.15)", "exp(1.0_DP) = ", exp(1.0_DP)

end program etable
