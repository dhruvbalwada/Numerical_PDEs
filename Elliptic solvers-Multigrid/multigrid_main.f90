! //////////////////////////////////////////////////////
!////////////////// Multigrid Solver////////////////////////
! //////////// Dhruv Balwada //////////////////////////////
! ////////////////// 2/9/2015//////////////////////////////
! ////////////////////////////////////////////////////////////


	PROGRAM multigrid_main
		USE vars
		IMPLICIT NONE

		INTERFACE 
			SUBROUTINE boundary(N,X,Y, Uout)
			INTEGER 						::	i,N
			REAL(8), DIMENSION(0:,0:), INTENT(OUT)  :: Uout
			REAL(8), DIMENSION(0:),  INTENT(IN)  :: X, Y 
			END SUBROUTINE boundary

			SUBROUTINE forcing(N,X,Y, Fout)
			INTEGER 						::	N, i, j
			REAL(8), DIMENSION(0:,0:), INTENT(OUT)  :: Fout
			REAL(8), DIMENSION(0:), INTENT(IN)  :: X, Y
			END SUBROUTINE forcing

			SUBROUTINE write_var2D(Uin)
			INTEGER 						::	N, i,j , out_unit = 150
			DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)  :: Uin
			CHARACTER(len=32) :: myfmt, filname
			END SUBROUTINE write_var2D


			SUBROUTINE relax_gauss_seidel(GRID, resmax)
			USE vars
			INTEGER 					:: k, N, i, j
			REAL(8)						:: omg = 1,h, res, resmax
			TYPE (ARRAYS) 	, INTENT(INOUT)	:: GRID
			END SUBROUTINE relax_gauss_seidel

			SUBROUTINE FtoC(GRIDf, GRIDc, kf, kc)
			USE vars
			INTEGER 			:: kf, kc, Nf, Nc , i, j
			TYPE (ARRAYS) , INTENT(IN) :: GRIDf
			TYPE (ARRAYS) , INTENT(INOUT) :: GRIDc
			REAL(8), DIMENSION(:,:),ALLOCATABLE :: A, B
			REAL(8) 			 :: hf, hc
			END SUBROUTINE
			
			SUBROUTINE CtoF(GRIDF, GRIDC, kf, kc)
			USE vars
			INTEGER 			:: kf, kc, Nf, Nc 
			TYPE (ARRAYS) :: GRIDc, GRIDf
			REAL(8), DIMENSION(:,:),ALLOCATABLE :: A, B
			REAL(8) 			 :: hf, hc
			END SUBROUTINE CtoF
		END INTERFACE

		INTEGER		, PARAMETER		:: M=5
		INTEGER						:: N, i, j, k,l , ld =3, lu=3
		REAL(8)						:: tol = 10.d-10, rnorm, start, finish
		REAL(8), DIMENSION(:), ALLOCATABLE :: X, Y
		REAL(8), DIMENSION(100)     :: res
		TYPE (ARRAYS), DIMENSION(M) :: GRID

		CALL CPU_time(start)
		res = 0.d0
		DO k = 1, M
			N = 2**k
			ALLOCATE(GRID(k)%U(0:N,0:N), GRID(k)%F(0:N,0:N))
			GRID(k)%U = 0.d0
			GRID(k)%F = 0.d0
		ENDDO
					
		ALLOCATE(X(0:N), Y(0:N))
		
		DO i = 0,N 
			X(i) = 1.d0/REAL(N)*REAL(i)
			Y(i) = 1.d0/REAL(N)*REAL(i)
		END DO

			! Initialize and set boundary conditions etc.
		CALL boundary(N,X,Y,GRID(M)%U)
		
		CALL forcing(N, X, Y, GRID(M)%F)

	
 		rnorm = 1.d0

 		i = 0
 		DO WHILE (tol < rnorm)
 			DO k = M, 1,-1
 				
 				DO l = 1,ld
 					
 					CALL relax_gauss_seidel(GRID(k), rnorm)
 				ENDDO
 				
 				IF (k > 1) THEN
 					CALL FtoC(GRID(k), GRID(k-1),k , k-1)
 				END IF
 				
 			ENDDO


 			DO k = 2, M
 				
 				CALL CtoF(GRID(k), GRID(k-1),k , k-1)
 			
 				DO l = 1, lu
 					CALL relax_gauss_seidel(GRID(k),rnorm)
 				ENDDO
 			ENDDO

 		write(*,*) rnorm
 			i = i+1
 			res(i) = rnorm
  		ENDDO

  		CALL CPU_time(finish)
  		write(*,*) finish-start, i

  		CALL exporttomatlabreadable(N, X, Y, GRID(M)%U, res, i)

  	!	DEALLOCATE(X, Y)
  !		DO k = 1, M
!			N = 2**k
!				DEALLOCATE(GRID(k)%U, GRID(k)%F)
!		ENDDO


	END PROGRAM multigrid_main