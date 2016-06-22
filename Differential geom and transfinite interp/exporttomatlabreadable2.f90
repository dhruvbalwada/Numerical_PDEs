! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE exporttomatlabreadable2(X, Y,N)
	IMPLICIT NONE

	INTEGER 						::	N,i,j , k, out_unit = 150
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: X,Y
	CHARACTER(len=32) :: myfmt, filname

	WRITE(filname, '(a, I0, a)') 'Xtop_' , N , '.txt'
	WRITE(myfmt, '(a, I0, a)') '(' , N+1 , 'f16.10)'

	OPEN(UNIT=out_unit, FILE = filname)
	
	DO i = 0, N 
		write(out_unit, myfmt) X(i)
	END DO
	
	CLOSE(out_unit)

	WRITE(filname, '(a, I0, a)') 'Ytop_' , N ,  '.txt'

	OPEN(UNIT=out_unit, FILE = filname)
	
	DO i = 0, N 
		write(out_unit, myfmt) Y(i)
	END DO
	
	CLOSE(out_unit)

	END SUBROUTINE exporttomatlabreadable2