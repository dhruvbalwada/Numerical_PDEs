! 


	SUBROUTINE initial_control_functions_ss(N,zeta,eta, P,Q, X,Y)
		IMPLICIT NONE

		INTEGER			:: N,i, j, jj  
		DOUBLE PRECISION, DIMENSION(N-1,N-1), INTENT(OUT)   :: P, Q
		DOUBLE PRECISION, DIMENSION(0:N,0:N), INTENT(IN)	:: X, Y
		DOUBLE PRECISION, DIMENSION(0:N), INTENT(IN)  		:: zeta, eta
		DOUBLE PRECISION, DIMENSION(N-1)					:: P1, Q1
		REAL(8)									     		  :: Jac, alpha, beta, gamma, h, R1, R2, etao,b 

			h=1.d0/REAL(N)
			j = N-1 ! choose this based on eta = eta_o
			etao = eta(j)
			b = 1
			DO i= 1,N-1
				
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

					P1(i) = ((Y(i,j+1)-Y(i,j-1))/(2*h)*R1 - (X(i,j+1)-X(i,j-1))/(2*h)*R2)/Jac
					Q1(i) = -((Y(i+1,j)-Y(i-1,j))/(2*h)*R1 - (X(i+1,j)-X(i-1,j))/(2*h)*R2)/Jac

					DO jj=1,N-1
						P(i,jj) = P1(i)*exp(-b*ABS(eta(j)-etao))
						Q(i,jj) = Q1(i)*exp(-b*ABS(eta(j)-etao))
					ENDDO
			ENDDO

		END SUBROUTINE initial_control_functions_ss




