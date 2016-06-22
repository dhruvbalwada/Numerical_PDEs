! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Main code to do SOR
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

		PROGRAM SOR_main

			IMPLICIT NONE

!	======= Arguments =========
			INTEGER							:: i, j, k
			INTEGER, PARAMETER 				:: N =64, kmax = 20000, SYMflag =0
			DOUBLE PRECISION, PARAMETER		:: omg = 1.0d0
			DOUBLE PRECISION				:: resmax, tol = 1d-10, h = 1.0/REAL(N), start, finish
			DOUBLE PRECISION, DIMENSION(0:N,0:N):: U
			DOUBLE PRECISION, DIMENSION(N-1,N-1):: F
			DOUBLE PRECISION, DIMENSION(0:N) :: X, Y 
			DOUBLE PRECISION, DIMENSION(kmax) :: res


			CALL CPU_TIME(start)

!	====== Asign B.C. and other starting array values =========
	DO i = 0, N
		X(i) = i*h
		Y(i) = i*h
	END DO
 	

	U = 0.d0
	CALL RANDOM_NUMBER(U)



	CALL boundary(N, X, Y, U)
!	====== Asign Forcing =========
	CALL forcing(N,X,Y, F)


! 	====== The main iteration loop below ========

	DO k=1, kmax
		resmax = 0.d0
		DO j = 1, N-1
			DO i = 1, N-1


				CALL res_for(i,j,N, U, F, (1.0/h**2), res(k))

				U(i,j) = U(i,j) - 0.25*omg*(h**2)*res(k)

				resmax = MAX(ABS(res(k)), resmax)
			END DO
		END DO

		IF (SYMflag == 1) THEN
		DO j =  N-1, 1, -1
			DO i = N-1, 1, -1
				CALL res_for(i,j,N, U, F, 1.0/h**2, res(k))
				U(i,j) = U(i,j) + 0.25*omg*h**2*res(k)
				resmax = MAX(ABS(res(k)), resmax)
			END DO
		END DO
		END IF

		write(*,*) resmax
		IF (abs(resmax) < tol) EXIT

			IF (MOD(k,2)==0 .AND. k<20) THEN
				CALL exporttomatlabreadable2(N,U,k)
			END IF

	END DO
	write(*,*) k

	CALL CPU_TIME(finish)

	write(*,*) finish-start
	CALL exporttomatlabreadable(N,X,Y,U,res,omg,kmax)

END PROGRAM
