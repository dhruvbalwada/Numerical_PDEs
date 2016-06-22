!
!//////////////////////////////////////////////////////////////////
!
!	subroutine : xDeriv(u,dudx,N,dx)
!	Module to compute derivatives using finite differences
!	u 		: Input array
! 	dudx 	: Output array
!	dx		: grid spacing
! 	N 		: number of intervals. x_j=jdx, where j = 0,1,...N 
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	21 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

      SUBROUTINE xDeriv( u, dudx, N, dx)
	  IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                                      :: N    ! The size of the vector
      DOUBLE PRECISION                             :: dx   ! the grid spacing
      DOUBLE PRECISION, DIMENSION(N+1), INTENT(IN) :: u    ! the input vector
      DOUBLE PRECISION, DIMENSION(N+1), INTENT(OUT):: dudx ! the result
!
!	---------------
!     Local Variables
!     ---------------
!
      INTEGER	:: j
      DOUBLE PRECISION :: dx2  ! To avoid computation of 2*dx everytime for center diff.
      dx2 = 2.0*dx
!
!     -----------------------
!	Compute derivatives now
!     -----------------------
!
!	---- j=0 - Forward Difference(by default arrays start at index 1)
!
	  dudx(1) = (u(2)-u(1))/dx
!	
!	---- j=1...N-1 - Centred Difference
!	
 	  DO j = 2, N
      	  dudx(j)= (u(j+1)-u(j-1))/(dx2)
	  ENDDO
!
!	---- j=N	- Backward Difference
!
	  dudx(N+1)=(u(N+1)-u(N))/dx

	  END SUBROUTINE xDeriv
	  	