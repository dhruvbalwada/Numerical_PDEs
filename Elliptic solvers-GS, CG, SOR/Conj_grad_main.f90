! Dhruv Balwada
! 1/31/2015
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!   Main code to do Conjugate Gradient
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

		PROGRAM Conj_grad_main

			IMPLICIT NONE

!	======= Arguments =========
		INTEGER							:: i, j, k, M
		INTEGER, PARAMETER 				:: N = 128, kmax = 20000, PC_flag =0
		DOUBLE PRECISION				:: resmax, tol = 1d-10, h = 1.d0/REAL(N), c , w, d , omg = 1.d0
		DOUBLE PRECISION, DIMENSION(0:N) :: X, Y
		DOUBLE PRECISION, DIMENSION(0:N,0:N):: U, b,  r, Z, V
		DOUBLE PRECISION, DIMENSION(N-1,N-1):: F
		DOUBLE PRECISION 					::	DDOT, DNRM2, start, finish
		DOUBLE PRECISION, DIMENSION(kmax) 	:: res, maxnormr

!  -------- Generate the matrices and start the process ------
		CALL CPU_time(start)
		res = 0.d0
		M = (N+1)**2
		DO i = 0, N
			X(i) = i*h
			Y(i) = i*h
		END DO

		r = 0.d0
		V = 0.d0
		U = 0.d0
		

		! Generate forcing array and boundary conditions
		CALL boundary(N,X,Y,U)
		CALL forcing(N,X,Y,F)
	! Start of with some initial values of the matrices 

	! r = Y - Ax
	
		DO i =1, N-1
			DO j = 1,N-1
				r(i,j) = (h**2)*F(i,j)- (U(i+1,j)+U(i-1,j)+U(i,j+1)+U(i,j-1)-4*U(i,j))
			END DO
		END DO
				IF (PC_flag == 0) THEN
			Z= r
		ELSE IF (PC_flag == 1) THEN
			CALL pre_cond(N,r,Z,omg)
		END IF
		
		V = Z 									! V <- Z
		

 		c = DDOT(M,r, 1, Z,1)					! c = (r,Z)
		
! Run the main iteration loops 
		DO k = 1, kmax
			
			! Z<-AV
			DO i = 1, N-1
				DO j =1 ,N-1
					Z(i,j) = (V(i+1,j)+V(i-1,j)+V(i,j+1)+V(i,j-1)-4*V(i,j))
				END DO
			END DO
			
			w = c/DDOT(M,V,1,Z,1)					 ! w<- C/(V,Z)
				
			CALL DAXPY(M,w,V,1,U,1)				 ! X <- X + wV


			CALL DAXPY(M,-w,Z,1,r,1)				 ! r <- r - wZ
			
		
			res(k) = DNRM2(M, r, 1)
			maxnormr(k) = MAXVAL(ABS(r))
		
			!write(*,*) res(k)
			IF ( maxnormr(k)<= tol) EXIT

			! Solve HZ = r for Z 

			IF (PC_flag == 0) THEN
				Z= r
			ELSE IF (PC_flag == 1) THEN
				CALL pre_cond(N,r,Z,omg)
			END IF

			d = DDOT(M, r, 1, Z, 1)					! d <- (r,Z)
			

			CALL DSCAL(M, d/c, V, 1) 				! V <- (d/c)V
			CALL DAXPY(M, 1.d0, Z, 1, V, 1)			! V <- z + V

			c = d			 						! c <- d

	
		END DO

		CALL CPU_time(finish)



		CALL exporttomatlabreadable(N,X, Y, U, res, omg, kmax)
		write(*,*) k
		write(*,*) finish-start
		END PROGRAM Conj_grad_main

