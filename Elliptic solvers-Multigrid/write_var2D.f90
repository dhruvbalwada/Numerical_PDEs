! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE write_var2D(Uin)
	IMPLICIT NONE

	INTEGER 						::	N, i,j , out_unit = 150
	DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)  :: Uin
	CHARACTER(len=32) :: myfmt, filname

	N = ubound(Uin,2)
	

	WRITE(myfmt, '(a, I0, a)') '(' , N+1 , 'f16.10)'
	WRITE(filname, '(a, I0, a)') 'data' , N , '.txt'

	OPEN(UNIT=out_unit, FILE = filname)

	DO i = 1, N 
		write(out_unit, myfmt) (Uin(i,j), j=1,N)
	END DO

	CLOSE(out_unit)


	END SUBROUTINE write_var2D