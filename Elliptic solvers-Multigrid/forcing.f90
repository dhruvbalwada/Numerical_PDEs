! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to input the forcing F(i,j)
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE forcing(N,X,Y, Fout)
	IMPLICIT NONE

	INTEGER 						::	N, i, j
	REAL(8), DIMENSION(0:,0:), INTENT(OUT)  :: Fout
	REAL(8), DIMENSION(0:), INTENT(IN)  :: X, Y

	Fout = 0.d0
	DO j = 1,N-1
		DO i = 1,N-1
			Fout(i,j) = -2.d0*COS(X(j))*SIN(Y(i))
		!	Fout(i,j) = -COS(X(j))
		END DO
	END DO

	
	END SUBROUTINE forcing