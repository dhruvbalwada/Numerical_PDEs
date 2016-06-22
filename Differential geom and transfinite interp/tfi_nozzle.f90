! Main program to generate grid for nozzle 


	SUBROUTINE tfi_nozzle(M,X,Y)
		IMPLICIT NONE 

		INTEGER, PARAMETER 		::  N =100000
		INTEGER					:: i,j,k, M
		REAL(8), DIMENSION(0:M,0:M), INTENT(OUT) :: X, Y
		REAL(8), DIMENSION(0:M)		:: zeta, eta
		REAL(8), DIMENSION(0:M,2)		:: G1, G2, G3, G4
		REAL(8) 				    :: quar_annulus
		REAL(8)						:: dx, xmax, xmin, Lx, HL, HR, Ls, dt, ds,h, k1, k2, k3, k4, tl, tr, tm
		REAL(8)						:: area

		!write(*,*) 13
		! Initialize
		X = 0.d0
		Y = 0.d0
		dx=1.d0/M
		! Generate zeta and eta 
		DO i =0,M
			zeta(i) = i*dx
			eta(i)  = i*dx
		END DO

		! Generate the edges of the nozzle 
		xmax = 4.5d0 
		xmin = 0.31d0
		Lx = xmax - xmin
		dt = Lx/REAL(N)
		HL = area(xmin, 1)
		HR = area(xmax, 1)

		DO i = 0, M
			G1(i,1) = Lx*zeta(i)+xmin
			G1(i,2) = 0.d0
 			
 			G4(i,1) = xmin
 			G4(i,2) = HL*eta(i)

 			G2(i,1) = xmax
 			G2(i,2) = HR*eta(i)

 		ENDDO


 		Ls = 0.d0 

 		DO i = 0,N-1
 			tl = REAL(i)*dt+xmin
 			tr = REAL(i+1)*dt+xmin
 			tm = (tl+tr)/2.d0
 			ds = dt/6.d0*((sqrt(1.d0+area(tl,2)**2.d0))+ & 
 				(SQRT(1.d0+area(tr,2)**2.d0))+4.d0*(sqrt(1.d0+area(tm,2)**2.d0)))
 			Ls = Ls + ds
 		ENDDO

 		write(*,*) Ls

 		G3(0,1) = xmin
 		G3(0,2) = area(G3(0,1),1)
 		h = dx
 		DO i = 1, M
 			k1 = Ls/SQRT(1.d0+area(G3(i-1,1),2)**2.d0)
 			k2 = Ls/SQRT(1.d0+area(G3(i-1,1)+h/2.d0*k1,2)**2.d0)
 			k3 = Ls/SQRT(1.d0+area(G3(i-1,1)+h/2.d0*k2,2)**2.d0)
 			k4 = Ls/SQRT(1.d0+area(G3(i-1,1)+h*k3,2)**2.d0)

 			G3(i,1) = G3(i-1,1) + h/6.d0*(k1+ 2.d0*k2 +2.d0*k3 + k4)
 			G3(i,2) = area(G3(i,1),1)
 		ENDDO
 			
 	!	write(*,*) G3(:,2)			


		DO i = 0,M !columns, zeta
			DO j = 0,M ! rows, eta
				X(i,j) = (1.d0-zeta(i))*G4(j,1) + zeta(i)*G2(j,1) &
				 + (1.d0-eta(j))*G1(i,1) + eta(j)*G3(i,1)  &
				 - (1.d0-zeta(i))*((1.d0-eta(j))*G1(0,1)+eta(j)*G3(0,1)) &
				 - (zeta(i))*((1.d0-eta(j))*G1(M,1)+eta(j)*G3(M,1)) 
				Y(i,j) = (1.d0-zeta(i))*G4(j,2) + zeta(i)*G2(j,2) &
				 + (1.d0-eta(j))*G1(i,2) + eta(j)*G3(i,2)  &
				 - (1.d0-zeta(i))*((1.d0-eta(j))*G1(0,2)+eta(j)*G3(0,2)) &
				 - (zeta(i))*((1.d0-eta(j))*G1(M,2)+eta(j)*G3(M,2)) 
			END DO
		END DO

	!CALL exporttomatlabreadable(X,Y,M)

	END SUBROUTINE tfi_nozzle