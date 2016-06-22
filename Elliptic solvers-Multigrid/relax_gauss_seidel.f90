! //////////////////////////////////////////
! ////// Subroutine to relax using ////////////
! ////// Gauss-Seidel ////////////////////////
! //////////////////////////////////////////

	SUBROUTINE relax_gauss_seidel(GRID, resmax)
		USE vars

		IMPLICIT NONE
		INTERFACE 
			SUBROUTINE res_for(i,j,N,Uin, Fin, fac, resout)
			INTEGER 						::	i,j, N
			REAL(8), DIMENSION(0:,0:), INTENT(IN)  :: Fin
			REAL(8), DIMENSION(0:,0:), INTENT(IN)  :: Uin
			REAL(8), INTENT(OUT) 		              :: resout
			REAL(8), INTENT(IN) 		              :: fac
			END SUBROUTINE

			SUBROUTINE write_var2D(Uin)
			INTEGER 						::	N, i,j , out_unit = 150
			DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN)  :: Uin
			CHARACTER(len=32) :: myfmt, filname
			END SUBROUTINE write_var2D
		END INTERFACE

		INTEGER 					:: k, N, i, j
		REAL(8)						:: omg = 1.d0,h, res, resmax
		TYPE (ARRAYS) , INTENT(INOUT) :: GRID

		N = ubound(GRID%U,1)
		h= 1.d0/N

		resmax = 0.d0
	
		DO j = 1, N-1
			DO i = 1, N-1
				
				CALL res_for(i,j,N, GRID%U, GRID%F, (1.0/h**2), res)
				GRID%U(i,j) = GRID%U(i,j) - 0.25*omg*(h**2)*res

				resmax = MAX(ABS(res),resmax)
			END DO
		END DO
		

	

	END SUBROUTINE relax_gauss_seidel

