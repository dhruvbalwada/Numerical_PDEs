! //////////////////////////////////////////
! ////// Subroutine to relax using ////////////
! ////// Gauss-Seidel ////////////////////////
! //////////////////////////////////////////

	SUBROUTINE CtoF(GRIDF, GRIDC, kf, kc)
		USE vars

		IMPLICIT NONE
		
		INTEGER 			:: kf, kc, Nf, Nc 
		TYPE (ARRAYS) :: GRIDc, GRIDf
		REAL(8), DIMENSION(:,:),ALLOCATABLE :: A, B
		REAL(8) 			 :: hf, hc

		INTERFACE
			SUBROUTINE restriction(Uf, Uc, kf,kc)
			INTEGER				:: kf, kc, Nf, Nc, i,j
			REAL(8), DIMENSION(0:,0:), INTENT(IN) :: Uf
			REAL(8), DIMENSION(0:,0:), INTENT(OUT) :: Uc
			END SUBROUTINE restriction

			SUBROUTINE interpolation(Uf, Uc, kf, kc)
			INTEGER				:: kf, kc, Nf, Nc, i,j
			REAL(8), DIMENSION(0:,0:), INTENT(IN)::  Uc
			REAL(8), DIMENSION(0:,0:), INTENT(OUT)::  Uf
			END SUBROUTINE interpolation

		END INTERFACE

		Nf = 2**kf
		hf = 1.d0/REAL(Nf)
		Nc = 2**kc
		hc = 1.d0/REAL(Nc)		

		ALLOCATE(A(0:Nc,0:Nc))

		CALL restriction(GRIDf%U, A, kf,kc)
	
		A = GRIDc%U - A

		ALLOCATE(B(0:Nf,0:Nf))
		
		CALL interpolation(B, A, kf, kc)

		GRIDf%U = GRIDf%U + B
		
		DEALLOCATE(A, B)

	END SUBROUTINE CtoF
