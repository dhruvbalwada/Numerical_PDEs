! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE res_for(i,j,N,Uin, Fin, fac, resout)
	IMPLICIT NONE

	INTEGER 						::	i,j, N
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(IN)  :: Fin
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)  :: Uin
	DOUBLE PRECISION, INTENT(OUT) 		              :: resout
	DOUBLE PRECISION, INTENT(IN) 		              :: fac

	
	resout = Fin(i,j) - fac*(Uin(i+1,j) + Uin(i-1,j) + Uin(i,j+1) + Uin(i,j-1) - 4*Uin(i,j))
	

	END SUBROUTINE res_for