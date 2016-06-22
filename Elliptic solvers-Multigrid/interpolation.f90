! //////////////////////////////////////////
! ////// Subroutine to relax using ////////////
! ////// Gauss-Seidel ////////////////////////
! //////////////////////////////////////////

	SUBROUTINE interpolation(Uf, Uc, kf, kc)

		IMPLICIT NONE
		INTEGER				:: kf, kc, Nf, Nc, i,j
		REAL(8), DIMENSION(0:,0:), INTENT(IN)::  Uc
		REAL(8), DIMENSION(0:,0:), INTENT(OUT)::  Uf

		Nf = 2**kf
		Nc = 2**kc
				
		! Copy the corner points as is
		DO i = 0,Nc
			DO j = 0,Nc
				Uf(2*j,2*i) = Uc(j,i)
			END DO
		END DO

		! Do the half way points
		! columns
		DO i = 0,Nf
			DO j = 1,Nf-1,2
				Uf(j,i) = 0.5d0*(Uf(j-1,i)+Uf(j+1,i))
			END DO
		END DO

	   ! rows
		DO i = 1,Nf-1,2
			DO j = 0,Nf
				Uf(j,i) = 0.5d0*(Uf(j,i-1)+Uf(j,i+1))
			END DO
		END DO

		! middle points
		DO i = 1,Nf-1,2
			DO j = 1,Nf-1,2
				Uf(j,i) = 0.25d0*(Uf(j,i-1)+Uf(j,i+1)+Uf(j-1,i)+Uf(j+1,i))
			END DO
		END DO
		
	END SUBROUTINE interpolation