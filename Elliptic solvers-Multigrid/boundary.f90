! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to set BC
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE boundary(N, X,Y,  Uout)
	IMPLICIT NONE

	INTEGER 						::	i,N
	REAL(8), DIMENSION(0:,0:), INTENT(OUT)  :: Uout
	REAL(8), DIMENSION(0:),  INTENT(IN)  :: X, Y 


	DO i = lbound(X,1),ubound(X,1)

		Uout(0,i) = COS(X(i))*SIN(Y(0))
		Uout(i,0) = COS(X(0))*SIN(Y(i))
		Uout(N,i) = COS(X(i))*SIN(Y(N))
		Uout(i,N) = COS(X(N))*SIN(Y(i))
!		Uout(0,i) = COS(X(i))
!		Uout(i,0) = COS(X(0))
!		Uout(N,i) = COS(X(i))
!		Uout(i,N) = COS(X(N))
	END DO

	END SUBROUTINE boundary