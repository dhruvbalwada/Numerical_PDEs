! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!  Code to do Conjugate Gradient pre conditioning 
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE pre_cond(N,r,Z,omg)
		IMPLICIT NONE

		INTEGER 			:: N, i, j
		DOUBLE PRECISION, DIMENSION(0:N,0:N)::  r, Z
		DOUBLE PRECISION		:: omg

		Z=r

		DO j = 1, N-1
			DO i =1, N-1
				Z(i,j) = -omg/4.d0*(2.d0-omg)*r(i,j) + omg/4.d0*(Z(i,j-1) + Z(i-1,j))
			END DO
		END DO

			Z = -4.d0*Z

		DO j= N-1, 1, -1
			DO i=N-1,1,-1
				Z(i,j) = omg/4.d0*(Z(i+1,j) + Z(i,j+1)) - 0.25d0*Z(i,j)
			END DO
		END DO

	END SUBROUTINE pre_cond