! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE res_for(i,j,N,Uin, Fin, fac, resout)
	IMPLICIT NONE

	INTEGER 						::	i,j, N
	REAL(8), DIMENSION(0:,0:), INTENT(IN)  :: Fin
	REAL(8), DIMENSION(0:,0:), INTENT(IN)  :: Uin
	REAL(8), INTENT(OUT) 		              :: resout
	REAL(8), INTENT(IN) 		              :: fac

	
	resout = Fin(i,j) - fac*(Uin(i+1,j) + Uin(i-1,j) + Uin(i,j+1) + Uin(i,j-1) - 4*Uin(i,j))
	!write(*,*) lbound(Fin,1)

	END SUBROUTINE res_for