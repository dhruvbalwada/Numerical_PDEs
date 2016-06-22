! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to set BC
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE boundary(N, X,Y,  Uout)
	IMPLICIT NONE

	INTEGER 						::	i,N
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(OUT)  :: Uout
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: X, Y 


	DO i = 0,N
		Uout(0,i) = COS(X(i))*SIN(Y(0))
		Uout(i,0) = COS(X(0))*SIN(Y(i))
		Uout(N,i) = COS(X(i))*SIN(Y(N))
		Uout(i,N) = COS(X(N))*SIN(Y(i))
	END DO
	
	END SUBROUTINE boundary