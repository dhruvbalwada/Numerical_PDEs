! Elliptic grid generation for a nozzle 

	PROGRAM nozzle_elliptic 

			IMPLICIT NONE

!	======= Arguments =========
			INTEGER							:: i, j, k
			INTEGER, PARAMETER 				:: N = 64, kmax = 400000, SYMflag =1.d0
			DOUBLE PRECISION, PARAMETER		:: omg = 0.5d0
			DOUBLE PRECISION				:: resmax, tol = 1d-5, h = 1.0/REAL(N), start, finish, D, alpha, gamma
			DOUBLE PRECISION, DIMENSION(0:N,0:N):: X, Y
			DOUBLE PRECISION, DIMENSION(N-1,N-1):: P, Q
			DOUBLE PRECISION, DIMENSION(0:N) :: zeta, eta 
			DOUBLE PRECISION, DIMENSION(kmax) :: resx, resy
			DOUBLE PRECISION				:: ss_flag =1.d0, control_flag=1.d0


			CALL CPU_TIME(start)

!	====== Asign B.C. and other starting array values =========
	DO i = 0, N
		zeta(i) = REAL(i)*h
		eta(i) = REAL(i)*h
	END DO
 	

	X = 0.d0
	Y = 0.d0
	!CALL RANDOM_NUMBER(U)

	!write(*,*) 10

	CALL tfi_nozzle(N, X,Y)

	!write(*,*) 12
	CALL boundary(N, zeta, eta, X,Y)
!	====== Asign Forcing =========
	IF (ss_flag==0) THEN
		CALL forcing(N,zeta,eta, P,Q)
	ELSE
		CALL initial_control_functions_ss(N,zeta,eta, P,Q, X,Y)
	ENDIF
	IF (control_flag==0) THEN
		P = 0.d0
		Q = 0.d0
	ENDIF

	

!	write(*,*) P

	

! 	====== The main iteration loop below ========

	DO k=1,kmax
		
		resmax = 0.d0

		


		DO j = 1, N-1
			DO i = 1, N-1

				alpha = 1.d0/(4.d0*h**2)*((X(i,j+1)-X(i,j-1))**2 + (Y(i,j+1)-Y(i,j-1))**2)
				gamma = 1.d0/(4.d0*h**2)*((X(i+1,j)-X(i-1,j))**2 + (Y(i+1,j)-Y(i-1,j))**2)

				D = -2.d0*(alpha+gamma)/h**2

				CALL res_for(i,j,N, X, Y, P, Q, resx(k),1)
				X(i,j) = X(i,j)  + 1/D*omg*resx(k)

				CALL res_for(i,j,N, X, Y, P, Q, resy(k),2)
				Y(i,j) = Y(i,j)  + 1/D*omg*resy(k)

				resmax = MAX(ABS(resx(k)),ABS(resy(k)), resmax)
			END DO
		END DO
		CALL boundary(N, zeta, eta, X,Y)

		IF (SYMflag == 1.d0) THEN
			DO j =  N-1, 1, -1
				DO i = N-1, 1, -1

					alpha = 1.d0/(4.d0*h**2)*((X(i,j+1)-X(i,j-1))**2 + (Y(i,j+1)-Y(i,j-1))**2)
					gamma = 1.d0/(4.d0*h**2)*((X(i+1,j)-X(i-1,j))**2 + (Y(i+1,j)-Y(i-1,j))**2)

					D = -2.d0*(alpha+gamma)/h**2

					CALL res_for(i,j,N, X, Y, P, Q, resx(k),1)
					X(i,j) = X(i,j)  + 1/D*omg*resx(k)

					CALL res_for(i,j,N, X, Y, P, Q, resy(k),2)
					Y(i,j) = Y(i,j)  + 1/D*omg*resy(k)

					resmax = MAX(ABS(resx(k)),ABS(resy(k)), resmax)
				END DO
			END DO 
			CALL boundary(N, zeta, eta, X,Y)
		END IF

		write(*,*) resmax
		IF (abs(resmax) < tol) EXIT

		IF (ss_flag==1 .AND. control_flag==1) THEN 
			CALL control_functions_ss(N,zeta,eta, X,Y, P,Q, P, Q)
		ENDIF
!			IF (MOD(k,2)==0 .AND. k<20) THEN
!				CALL exporttomatlabreadable2(N,U,k)
!			END IF


!	write(*,*) k
	END DO

	CALL CPU_TIME(finish)

	write(*,*) finish-start

	CALL exporttomatlabreadable(X,Y,N)

	END PROGRAM nozzle_elliptic
