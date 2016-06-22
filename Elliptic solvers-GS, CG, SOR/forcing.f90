! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to input the forcing F(i,j)
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE forcing(N,X,Y, Fout)
	IMPLICIT NONE

	INTEGER 						::	N, i, j
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(OUT)  :: Fout
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: X, Y

	DO j = 1,N-1
		DO i = 1,N-1
			Fout(i,j) = -2.d0*COS(X(j))*SIN(Y(i))
		END DO
	END DO
	
	END SUBROUTINE forcing