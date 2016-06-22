!
!//////////////////////////////////////////////////////////////////
!
!	subroutine : rkLs(Q,neqn,N,time,dx,dt)
!	Module to time step ODE using 3rd order Runge Kutta with two level storage
!
!	Q 		: Input/Output array
!	neqn 	: Output array
!	dx		: grid spacing
!	dt      : time step
! 	N 		: number of intervals. x_j=jdx, where j = 0,1,...N 
!	time    : time of integration
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	21 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

      SUBROUTINE rkLs(Q, neqn, N, time, dx, dt)
	  IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                                      :: N, neqn        ! The size of the vector(0...N)
                                                                     ! The number of equations
      DOUBLE PRECISION, INTENT(IN)                 :: dx, dt, time   ! the grid spacing, time step,time
      DOUBLE PRECISION, DIMENSION(N+1,neqn)        :: Q              ! the input vector
!
!	  ---------------
!     Local Variables
!     ---------------
!
      INTEGER	:: j, kmax=3, k, l
	  DOUBLE PRECISION, DIMENSION(3) :: a = (/ 0.d0, -5.d0/9.d0, -153.d0/128.d0 /)
      DOUBLE PRECISION, DIMENSION(3) :: b = (/ 0.d0, 1.d0/3.d0, 3.d0/4.d0 /)
      DOUBLE PRECISION, DIMENSION(3) :: g = (/ 1.d0/3.d0, 15.d0/16.d0, 8.d0/15.d0 /)
      DOUBLE PRECISION, DIMENSION(N+1,neqn) :: dQdt, Gtemp
      DOUBLE PRECISION :: tk
      Gtemp = 0.d0
!	
!     -----------------------
!	  RK Integrator below
!     -----------------------

	  DO k = 1,kmax  		! Step through all the equations(u,v etc.) 
			tk = time + dt*b(k)
                  ! Line below when testing.
!			dQdt = 10.d0*tk**3
	  		CALL tDeriv_hw3(Q, dQdt, N , neqn, dx, dt)
			Gtemp = a(k)*Gtemp + dQdt
			Q = Q + g(k)*dt*Gtemp
	  ENDDO
	
	  END SUBROUTINE rkLs
