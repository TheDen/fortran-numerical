program time_trisolve

  use linalg
  implicit none

  real(WP), allocatable :: A(:,:), x(:), row1(:)
  integer :: n, j
  real(WP) :: start, finish, elapsed

  print "(/, t2, a, /)", "CPU time to solve an n-by-n upper triangular system:"
  print "(t2, a, /)", "Results using DIY routine trisolve:"
  print "(t2, a6, a16, a16, /)", "n", "seconds", "seconds/n^2"
  do n = 1000, 8000, 1000
    allocate(A(n,n), x(n), row1(n))
    do j = 1, n
      row1(j) = 1.0 / ( 1 + (j-1)**2 )
    end do
    A = 0
    do j = 1, n
      A(1:j,j) = row1(j:1:-1)
    end do
    call random_number(x)
    call cpu_time(start)
    call trisolve('Upper triangular', A, x)
    call cpu_time(finish)
    elapsed = finish - start
    print "(t2, i6, e16.4, e16.4)", n, elapsed, elapsed/real(n,WP)**2
    deallocate(A, x, row1)
  end do

  print "(/, t2, a, /)", "Results using ATLAS routine dgtrs:"
  print "(t2, a6, a16, a16,/)", "n", "seconds", "seconds/n^2"
  do n = 1000,8000,1000
    allocate(A(n,n), x(n), row1(n))
    do j = 1, n
      row1(j) = 1.0 / ( 1 + (j-1)**2 )
    end do
    A = 0
    do j = 1, n
      A(1:j,j) = row1(j:1:-1)
    end do
    call random_number(x)
    call cpu_time(start)
    call dtrsv('U', 'N', 'N', n, A, n, x, 1)
    call cpu_time(finish)
    elapsed = finish - start
    print "(t2, i6, e16.4, e16.4)", n, elapsed, elapsed/real(n)**2
    deallocate(A, x, row1)
  end do
end program time_trisolve
