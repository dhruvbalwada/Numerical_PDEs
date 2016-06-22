!
!//////////////////////////////////////////////////////////////////
!
!	SUBROUTINE : fwd_euler.f90
!	Subroutine to do the time stepping using the Forward Euler Scheme
!	Written as part of Homework 5 for Numerical PDE-I
! 	8 November, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

	SUBROUTINE fwd_euler(un1, un2, dudx1, Nx, dt)
		IMPLICIT NONE

!	  ---------
!     Arguments
!     ---------
!	
	  INTEGER		:: Nx
      DOUBLE PRECISION                             :: dt   ! the grid spacing
      DOUBLE PRECISION, DIMENSION(Nx+1), INTENT(IN) :: un1, dudx1   ! the input vector
      DOUBLE PRECISION, DIMENSION(Nx+1), INTENT(OUT):: un2 ! the result
     
      un2 = un1 - dt*dudx1
	  
	  END SUBROUTINE fwd_euler