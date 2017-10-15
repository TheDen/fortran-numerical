module bvp

  implicit none

  integer, parameter :: DP = selected_real_kind(15)

contains

  subroutine solve_bvp(x, eqn_coeff, bc_coeff, forcing, U)
    real(DP), intent(in)  :: x(-1:), eqn_coeff(0:,0:), bc_coeff(0:,:), &
                             forcing(-1:)
    real(DP), intent(out) :: U(-1:)
    !
    ! We solve the boundary value problem
    !
    !      a_2 u'' + a_1 u' + a_0 u = f(x)      for x(0) < x < x(M)
    !
    !       bleft_1 u' +  bleft_0 u = Aleft     at x = x(0)
    !      bright_1 u' + bright_0 u = Aright    at x = x(M)
    !
    ! Put
    !
    !      eqn_coeff(0,m) = a_0(x_m)
    !      eqn_coeff(1,m) = a_1(x_m)
    !      eqn_coeff(2,m) = a_2(x_m)
    !       bc_coeff(0,1) = bleft_0            for 0 <= m <= M
    !       bc_coeff(1,1) = bleft_1
    !       bc_coeff(0,2) = bright_0
    !       bc_coeff(1,2) = bright_1
    !
    ! with
    !
    !            forcing(-1)  = Aleft
    !            forcing(m)   = f(x_m)      for 0 <= m <= M
    !            forcing(M+1) = Aright
    !
    real(DP), allocatable :: AB(:,:)
    integer,  allocatable :: ipiv(:)

    real(DP) :: Dx
    integer  :: M, j, info

    M = ubound(x,1) - 1
    Dx = x(1) - x(0)
    if ( ubound(eqn_coeff,1) /= 2 )  stop 1
    if ( ubound(eqn_coeff,2) /= M )  stop 2
    if ( ubound(bc_coeff,1)  /= 1 )  stop 3
    if ( ubound(bc_coeff,2)  /= 2 )  stop 4
    if ( ubound(forcing, 1)  /= M+1) stop 5
    if ( ubound(U, 1)        /= M+1) stop 6

    !
    ! Compute the entries of the coefficient matrix and store them
    ! in Lapack band storage mode.
    !
    allocate(AB(7,-1:M+1), ipiv(-1:M+1))
    AB = 0
    AB(3,1) = bc_coeff(1,1) / ( 2 * Dx )
    AB(4,0) = bc_coeff(0,1)
    do j = 0, M
      AB(4,j+1) =       eqn_coeff(2,j) / Dx**2 + eqn_coeff(1,j) / ( 2 * Dx )
      AB(5,j)   = - 2 * eqn_coeff(2,j) / Dx**2 + eqn_coeff(0,j)
      AB(6,j-1) =       eqn_coeff(2,j) / Dx**2 - eqn_coeff(1,j) / ( 2 * Dx )
    end do
    AB(5,-1) = -AB(3,1)
    AB(5,M+1) = bc_coeff(1,2) / ( 2 * Dx )
    AB(6,M) = bc_coeff(0,2)
    AB(7,M-1) = -AB(5,M+1)

    U = forcing
    call dgbsv(M+3, 2, 2, 1, AB, size(AB,1), ipiv, U, size(U), info)
    if ( info /= 0 ) then
      print *, "dgbsv failed with info = ", info
      stop
    end if

  end subroutine solve_bvp

  subroutine linspace(x, xl, xr)
    real(DP), intent(out) :: x(-1:)
    real(DP), intent(in)  :: xl, xr

    integer  :: steps, m
    real(DP) :: dx

    steps = ubound(x,1) - 1
    dx = ( xr - xl ) / steps
    if ( dx <= 0 ) stop "x_left >= x_right"
    x(-1) = xl - dx
    x(0)  = xl
    do m = 1, steps-1
      x(m) = xl + m * dx
    end do
    x(steps)   = xr
    x(steps+1) = xr + dx

  end subroutine linspace

  subroutine print_real_matrix(A, entry_fmt)
    real(DP),         intent(in) :: A(:,:)
    character(len=*), intent(in) :: entry_fmt

    integer :: i, j

    do i = 1, size(A, 1)
      do j = 1, size(A, 2)
        write(unit=*, fmt=entry_fmt, advance="no") A(i,j)
      end do
      print *
    end do

  end subroutine print_real_matrix

end module bvp
