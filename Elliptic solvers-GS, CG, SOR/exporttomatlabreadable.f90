! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE exporttomatlabreadable(N,X, Y, Uin, res,w, kmax)
	IMPLICIT NONE

	INTEGER 						::	N,i,j , out_unit = 150, kmax
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: X,Y
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)  :: Uin
	DOUBLE PRECISION , INTENT(IN) ::  w
	DOUBLE PRECISION, DIMENSION(kmax) :: res
	CHARACTER(len=32) :: myfmt, filname

	WRITE(myfmt, '(a, I0, a)') '(' , N+1 , 'f16.13)'
	WRITE(filname, '(a, I0, a, f3.1, a)') 'U_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 0, N 
		write(out_unit, myfmt) (Uin(i,j), j=0,N)
	END DO

	CLOSE(out_unit)

	WRITE(filname, '(a, I0, a, f3.1, a)') 'res_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 1, kmax
		WRITE(out_unit, '(1f14.8)') ABS(res(i))
	END DO
	CLOSE(out_unit)

	WRITE(filname, '(a, I0, a, f3.1, a)') 'X_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 0, N
		WRITE(out_unit, '(1f16.13)') X(i)
	END DO
	CLOSE(out_unit)

	WRITE(filname, '(a, I0, a, f3.1, a)') 'Y_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 0, N
		WRITE(out_unit, '(1f16.13)') Y(i)
	END DO
	CLOSE(out_unit)


	END SUBROUTINE exporttomatlabreadable