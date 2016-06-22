! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to set BC
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE boundary(N, zeta,eta,X,Y)
	IMPLICIT NONE

	INTEGER							::	N
	INTEGER							:: i
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(OUT)  :: X, Y
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: zeta, eta 
	DOUBLE PRECISION, DIMENSION(0:N)  :: theta
	REAL(8)							:: xmin = 0.31d0, xmax = 4.5d0
	REAL(8) 						:: h
	REAL(8), PARAMETER				:: PI = 3.1415926535897932384
	h = (xmax-xmin)/REAL(N)

	DO i =0,N
		! Left boundary 
		
		Y(0,i) = Y(1,i)

		! Bottom boundary 
		
		X(i,0) = X(i,1)

		! Right boundary linearly interpolate between 0 and 15 
		theta(i) = REAL(i)/REAL(N)*15.d0*2*PI/360.d0
		Y(N,i) = Y(N-1,i) + tan(theta(i))*(xmax - X(N-1,i))
	!	write(*,*) theta(i)

		! Top boundary just uses dirichlet. Alldirichlet conditions are set by 
		! the initilization and don't need to be updated. 

	END DO
	
	END SUBROUTINE boundary