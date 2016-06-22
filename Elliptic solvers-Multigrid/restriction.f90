! //////////////////////////////////////////
! ////// Subroutine to relax using ////////////
! ////// Gauss-Seidel ////////////////////////
! //////////////////////////////////////////

	SUBROUTINE restriction(Uf, Uc, kf,kc)

		IMPLICIT NONE
		INTEGER				:: kf, kc, Nf, Nc, i,j
		REAL(8), DIMENSION(0:,0:), INTENT(IN) :: Uf
		REAL(8), DIMENSION(0:,0:), INTENT(OUT) :: Uc

		Nf = 2**kf
		Nc = 2**kc
		!CALL write_var2D(Uf)
		
		DO i = 0,Nc
			DO j = 0,Nc
				Uc(j,i) = Uf(2*j,2*i)
			!	write(*,*) Uc(j,i)
			END DO
		END DO

		!CALL write_var2D(Uf)

	END SUBROUTINE restriction