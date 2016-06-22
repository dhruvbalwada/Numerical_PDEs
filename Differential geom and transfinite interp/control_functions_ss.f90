! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Function to input the forcing F(i,j)
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
	SUBROUTINE control_functions_ss(N,zeta,eta, X,Y, Pold,Qold, P, Q)
	IMPLICIT NONE

	INTEGER 			:: N, i,j, jj
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(OUT)  :: P, Q
	DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(IN)  :: Pold, Qold
	DOUBLE PRECISION, DIMENSION(N-1)				  :: P1, Q1, PRHS, QRHS
	DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  :: zeta, eta
	DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)  :: X, Y 
	DOUBLE PRECISION		::	 omg, Jac, alpha, beta, gamma, R1, R2, etao, b, h

	omg = 0.1d0
	h=1.d0/REAL(N)
	j=N-1
	etao = eta(j)
	b = 0.5d0
	
	DO i = 1, N-1
		!DO j = 1,N-1

					Jac = 1.d0/(4.d0*h**2)*((X(i+1,j)-X(i-1,j))*(Y(i,j+1)-Y(i,j-1)) - (X(i,j+1)-X(i,j-1))*(Y(i+1,j)-Y(i-1,j)))
					alpha = 1.d0/(4.d0*h**2)*((X(i,j+1)-X(i,j-1))**2 + (Y(i,j+1)-Y(i,j-1))**2)
					beta =1.d0/(4.d0*h**2)*((X(i+1,j)-X(i-1,j))*(X(i,j+1)-X(i,j-1)) + (Y(i,j+1)-Y(i,j-1))*(Y(i+1,j)-Y(i-1,j)))
					gamma = 1.d0/(4.d0*h**2)*((X(i+1,j)-X(i-1,j))**2 + (Y(i+1,j)-Y(i-1,j))**2)

					R1 = -(1/h**2*(alpha*(X(i+1,j)-2.d0*X(i,j)+X(i-1,j)) &
						+ gamma*(X(i,j+1)-2.d0*X(i,j)+X(i,j-1))) &
		        		- 2*beta*(X(i+1,j+1)-X(i-1,j+1)-X(i+1,j-1)+X(i-1,j-1))/(2*h)**2)/Jac**2

					R2 = -(1/h**2*(alpha*(Y(i+1,j)-2.d0*Y(i,j)+Y(i-1,j))&
						+ gamma*(Y(i,j+1)-2.d0*Y(i,j)+Y(i,j-1))) &
			    		- 2*beta*(Y(i+1,j+1)-Y(i-1,j+1)-Y(i+1,j-1)+Y(i-1,j-1))/(2*h)**2)/Jac**2

					PRHS(i) = ((Y(i,j+1)-Y(i,j-1))/(2*h)*R1 - (X(i,j+1)-X(i,j-1))/(2*h)*R2)/Jac
					QRHS(i) = -((Y(i+1,j)-Y(i-1,j))/(2*h)*R1 - (X(i+1,j)-X(i-1,j))/(2*h)*R2)/Jac

					P1(i) = Pold(i,j) + omg*(PRHS(i) - Pold(i,j))
					Q1(i) = Qold(i,j) + omg*(QRHS(i) - Qold(i,j))

					DO jj=1,N-1
						P(i,jj) = P1(i)*exp(-b*ABS(eta(j)-etao))
						Q(i,jj) = Q1(i)*exp(-b*ABS(eta(j)-etao))
					ENDDO

		!ENDDO
	ENDDO

	END SUBROUTINE control_functions_ss




