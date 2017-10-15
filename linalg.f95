module linalg

  implicit none

  integer, parameter :: WP = selected_real_kind(15)

  interface print_matrix
    module procedure print_real_matrix, print_integer_matrix
  end interface print_matrix
     
contains

  subroutine trisolve(uplo, a, x)
    character(len=*), intent(in) :: uplo
    real(WP),  intent(in)        :: a(:,:)
    real(WP),  intent(inout)     :: x(:)
    !
    ! Overwrites x with inverse(A) * x assuming A is upper triangular
    ! or unit lower-triangular, and non-singular.
    !
    ! If the first letter of uplo is 'u' or 'U' then the system
    ! is upper triangular, and the routine ignores a(i,j) for i > j.
    ! If the first letter of uplo is 'l' or 'L' then the system
    ! is unit lower triangular, and the routine ignores a(i,j) for i <= j.
    !
    integer  :: n, i, j
    real(WP) :: s

    n = size(a,1)
    if ( size(a,2) /= n ) stop "a is not square"
    if ( size(x)   /= n ) stop "shapes of a and x do not match"
    select case ( uplo(1:1) )
    case ('u','U')
      !
      ! Back substitution
      !
      do i = n, 1, -1
        s = x(i)
        do j = i+1, n
          s = s - a(i,j) * x(j)
        end do
        x(i) = s / a(i,i)
      end do
    case ('l','L')
      !
      ! Forward elimination
      !
      do i = 1, n
        s = x(i)
        do j = 1, i-1
          s = s - a(i,j) * x(j)
        end do
        x(i) = s
      end do

    case default
      print *, "Argument uplo has an illegal value", uplo
      stop
    end select

  end subroutine trisolve

  subroutine print_real_matrix(A, entry_fmt)
    real(WP),         intent(in) :: A(:,:)
    character(len=*), intent(in) :: entry_fmt

    integer :: i, j

    do i = 1, size(A, 1)
      do j = 1, size(A, 2)
        write(unit=*, fmt=entry_fmt, advance="no") A(i,j)
      end do
      print *
    end do

  end subroutine print_real_matrix

  subroutine print_integer_matrix(A, entry_fmt)
    integer,          intent(in) :: A(:,:)
    character(len=*), intent(in) :: entry_fmt

    integer :: i, j

    do i = 1, size(A, 1)
      do j = 1, size(A, 2)
        write(unit=*, fmt=entry_fmt, advance="no") A(i,j)
      end do
      print *
    end do

  end subroutine print_integer_matrix

end module linalg
