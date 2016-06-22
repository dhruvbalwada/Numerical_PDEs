! //////////////////////////////////////////
! ////// Subroutine to relax using ////////////
! ////// Gauss-Seidel ////////////////////////
! //////////////////////////////////////////

	SUBROUTINE FtoC(GRIDf, GRIDc, kf, kc)
		USE vars

		IMPLICIT NONE
		INTEGER 			:: kf, kc, Nf, Nc , i, j
		TYPE (ARRAYS) , INTENT(IN) :: GRIDf
		TYPE (ARRAYS) , INTENT(INOUT) :: GRIDc
		REAL(8), DIMENSION(:,:),ALLOCATABLE :: A, B
		REAL(8) 			 :: hf, hc

		INTERFACE 
			SUBROUTINE restriction(Uf, Uc, kf,kc)
			INTEGER				:: kf, kc, Nf, Nc, i,j
			REAL(8), DIMENSION(0:,0:), INTENT(IN) :: Uf
			REAL(8), DIMENSION(0:,0:), INTENT(OUT) :: Uc
			END SUBROUTINE restriction

			SUBROUTINE write_var2D(Uin)
			INTEGER 						::	N, i,j , out_unit = 150
			DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)  :: Uin
			CHARACTER(len=32) :: myfmt, filname
			END SUBROUTINE write_var2D
		END INTERFACE

		Nf = 2**kf
		hf = 1.d0/REAL(Nf)
		Nc = 2**kc
		hc = 1.d0/REAL(Nc)		
		
		!CALL write_var2D(GRIDc%U)

		CALL restriction(GRIDf%U, GRIDc%U, kf,kc)
		!CALL write_var2D(GRIDc%U)
	
		
		! Fk-1 = Lk-1Uk-1 + R(Fk-LkUk)
		ALLOCATE(A(0:Nc,0:Nc))
		A = 0.d0 ! Lk-1Uk-1
		DO j = 1, Nc-1 
			DO i = 1, Nc-1
				A(i,j) = (1.d0/hc**2)*(GRIDc%U(i+1,j)+GRIDc%U(i-1,j)+GRIDc%U(i,j+1)+GRIDc%U(i,j-1) - 4.d0*GRIDc%U(i,j))
				!write(*,*) A 
			END DO
		END DO

		
		ALLOCATE(B(0:Nf, 0:Nf))
		B = 0.d0
		DO j = 1, Nf-1 
			DO i = 1, Nf-1
				B(i,j) = GRIDf%F(i,j) -(1.d0/hf**2)*(GRIDf%U(i+1,j)+GRIDf%U(i-1,j)+GRIDf%U(i,j+1)+GRIDf%U(i,j-1) - 4.d0*GRIDf%U(i,j))
			END DO
		END DO
		
		GRIDc%F = A

		CALL restriction(B,A,kf,kc)

		GRIDc%F = GRIDc%F + A

		DEALLOCATE ( A, B)
		
	END SUBROUTINE FtoC
