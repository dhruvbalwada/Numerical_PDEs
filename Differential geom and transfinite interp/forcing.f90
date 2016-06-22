! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to input the forcing F(i,j)
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE forcing(N,zeta,eta, P,Q)
	IMPLICIT NONE

	INTEGER 						::	N, i, j
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(OUT)  :: P, Q
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: zeta, eta
	REAL(8)						:: a1, b1, a2, b2, zeta0, eta0

	a1 = 0.2
	b1 = 0.5
	a2 = 2.0
	b2 = 0.5
	zeta0 = 0.5
	eta0 = 1.0

	DO j = 1,N-1 !eta
		DO i = 1,N-1 !zeta
			P(i,j) = SIGN(a1*exp(-b1*ABS(zeta(i)-zeta0)), -(zeta(i)-zeta0))
			Q(i,j) = SIGN(a2*exp(-b2*ABS(eta(j)-eta0)), -(eta(j)-eta0))
		END DO
	END DO
!	P= 0.d0
!	Q = 0.d0
	
	END SUBROUTINE forcing