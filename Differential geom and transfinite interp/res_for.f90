! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to calc residual in forward step
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	SUBROUTINE res_for(i,j,N,Xin, Yin, Pin, Qin, resout, num)
	IMPLICIT NONE

	INTEGER							:: N
	INTEGER 						::	i,j, num
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(IN)  :: Pin, Qin
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)  :: Xin, Yin
	DOUBLE PRECISION, INTENT(OUT) 		              :: resout
	REAL(8)											  :: RHS , LHS, Jac, alpha, beta, gamma, h

	h=1.d0/REAL(N)
	Jac = 1.d0/(4.d0*h**2)*((Xin(i+1,j)-Xin(i-1,j))*(Yin(i,j+1)-Yin(i,j-1)) - (Xin(i,j+1)-Xin(i,j-1))*(Yin(i+1,j)-Yin(i-1,j)))
	alpha = 1.d0/(4.d0*h**2)*((Xin(i,j+1)-Xin(i,j-1))**2 + (Yin(i,j+1)-Yin(i,j-1))**2)
	beta =1.d0/(4.d0*h**2)*((Xin(i+1,j)-Xin(i-1,j))*(Xin(i,j+1)-Xin(i,j-1)) + (Yin(i,j+1)-Yin(i,j-1))*(Yin(i+1,j)-Yin(i-1,j)))
	gamma = 1.d0/(4.d0*h**2)*((Xin(i+1,j)-Xin(i-1,j))**2 + (Yin(i+1,j)-Yin(i-1,j))**2)
	
	IF (num .EQ. 1) THEN

		RHS = -Jac**2*(Pin(i,j)*(Xin(i+1,j)-Xin(i-1,j)) + Qin(i,j)*(Xin(i,j+1)-Xin(i,j-1)))/(2*h)
		LHS = 1/h**2*(alpha*(Xin(i+1,j)-2.d0*Xin(i,j)+Xin(i-1,j)) &
				+ gamma*(Xin(i,j+1)-2.d0*Xin(i,j)+Xin(i,j-1))) &
		        - 2*beta*(Xin(i+1,j+1)-Xin(i-1,j+1)-Xin(i+1,j-1)+Xin(i-1,j-1))/(2*h)**2 

	ELSEIF (num .EQ. 2) THEN
		
		RHS = -Jac**2*(Pin(i,j)*(Yin(i+1,j)-Yin(i-1,j)) + Qin(i,j)*(Yin(i,j+1)-Yin(i,j-1)))/(2*h)
		LHS = 1/h**2*(alpha*(Yin(i+1,j)-2.d0*Yin(i,j)+Yin(i-1,j))&
				+ gamma*(Yin(i,j+1)-2.d0*Yin(i,j)+Yin(i,j-1))) &
			    - 2*beta*(Yin(i+1,j+1)-Yin(i-1,j+1)-Yin(i+1,j-1)+Yin(i-1,j-1))/(2*h)**2
	ENDIF


	resout = RHS - LHS
	

	END SUBROUTINE res_for