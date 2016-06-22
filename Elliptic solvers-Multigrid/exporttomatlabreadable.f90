! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE exporttomatlabreadable(N,X, Y, Uin, res, k)
	IMPLICIT NONE

	INTEGER 						::	N,i,j , k, out_unit = 150
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: X,Y
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)  :: Uin
	REAL(8), DIMENSION(100) 		:: res
	DOUBLE PRECISION  ::  w = 1.d0
	CHARACTER(len=32) :: myfmt, filname

	WRITE(myfmt, '(a, I0, a)') '(' , N+1 , 'f16.10)'
	WRITE(filname, '(a, I0, a, f3.1, a)') 'U_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 0, N 
		write(out_unit, myfmt) (Uin(i,j), j=0,N)
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


	WRITE(filname, '(a, I0, a, f3.1, a)') 'res_' , N , '_', w , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 1, k 
		WRITE(out_unit, '(1f20.13)') res(i)
	END DO
	CLOSE(out_unit)


	END SUBROUTINE exporttomatlabreadable