program ieuler

  use ivp
  use examples, f       => prob1_f,       &
                y0      => prob1_y0,      &
                start   => prob1_start,   &
                finish  => prob1_finish,  &
                y_exact => prob1_y_exact
  implicit none

  real(WP), allocatable :: y(:), t(:)
  integer :: maxN, argc, length, status, n, fp_its
  character(len=32) :: arg
  fp_its= 150
  argc = command_argument_count()
  if ( argc /= 1 ) then
      stop "You must provide a single command-line argument N"
  end if
  call get_command_argument(1, arg, length, status)
  if ( status /= 0 ) then
      stop "Error reading command-line argument"
  end if

  read(unit=arg, fmt=*) maxN
  allocate(y(0:maxN), t(0:maxN))
  call linspace(t, start, finish)
  call implicit_euler(y, t, y0, f, fp_its)
  print "(t1, a1, a8, 2(a20), /)", "#", "t", "y", "exact y"
  do n = 0, maxN
  print "(t2, f8.4, 2(e20.10))", t(n), y(n), y_exact(t(n)) !, y(n)-y_exact(t(n))
  end do
  deallocate(y, t)

end program ieuler
