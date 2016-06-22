!! Main program to calculate the grid


	PROGRAM main_tfi
		IMPLICIT NONE 

		INTEGER, PARAMETER 		:: M=32
		INTEGER					:: i,j,k
		REAL(8), DIMENSION(0:M,0:M) :: X, Y
		REAL(8), DIMENSION(0:M)		:: zeta, eta
		REAL(8) 				    :: quar_annulus
		REAL(8)						:: dx=1.d0/M
		! Initialize
		X =0.d0
		Y = 0.d0

		DO i =0,M
			zeta(i) = i*dx
			eta(i)  = i*dx
		END DO


		DO i = 0,M !columns, zeta
			DO j = 0,M ! rows, eta
				X(j,i) = (1.d0-zeta(i))*quar_annulus(zeta(0),eta(j),1) + zeta(i)*quar_annulus(zeta(M),eta(j),1) &
				 + (1.d0-eta(j))*quar_annulus(zeta(i),eta(0),1) + eta(j)*quar_annulus(zeta(i),eta(M),1)  &
				 - (1.d0-zeta(i))*((1.d0-eta(j))*quar_annulus(zeta(0),eta(0),1)+eta(j)*quar_annulus(zeta(0),eta(M),1)) &
				 - (zeta(i))*((1.d0-eta(j))*quar_annulus(zeta(M),eta(0),1)+eta(j)*quar_annulus(zeta(M),eta(M),1)) 
				Y(j,i) = (1.d0-zeta(i))*quar_annulus(zeta(0),eta(j),2) + zeta(i)*quar_annulus(zeta(M),eta(j),2) &
				 + (1.d0-eta(j))*quar_annulus(zeta(i),eta(0),2) + eta(j)*quar_annulus(zeta(i),eta(M),2)  &
				 - (1.d0-zeta(i))*((1.d0-eta(j))*quar_annulus(zeta(0),eta(0),2)+eta(j)*quar_annulus(zeta(0),eta(M),2)) &
				 - (zeta(i))*((1.d0-eta(j))*quar_annulus(zeta(M),eta(0),2)+eta(j)*quar_annulus(zeta(M),eta(M),2)) 
			END DO
		END DO

		CALL exporttomatlabreadable(X,Y,M)

	END PROGRAM main_tfi