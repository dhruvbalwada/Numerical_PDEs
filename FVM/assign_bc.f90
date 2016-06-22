SUBROUTINE assign_bc(normal, boundary)
	IMPLICIT NONE

			REAL(8), DIMENSION(2) :: leftnorm, rightnorm, topnorm, bottomnorm, normal
			CHARACTER(len=72) :: boundary
			REAL(8) :: temp

			leftnorm = (/ -1.d0, 0.d0/)
			rightnorm = (/ 1.d0, 0.d0/)
			topnorm = (/ 0.d0, 1.d0/)
			bottomnorm = (/ 0.d0, -1.d0/)

			temp = leftnorm(1)*normal(1) + leftnorm(2)*normal(2)

			IF(temp ==1.d0) THEN
				boundary = 'left'
			ELSEIF(temp == -1.d0) THEN
				boundary = 'right'
			ENDIF

			temp = topnorm(1)*normal(1) + topnorm(2)*normal(2)


			IF(temp ==1.d0) THEN
				boundary = 'top'
			ELSEIF(temp == -1.d0) THEN
				boundary = 'bottom'
			ENDIF

END SUBROUTINE assign_bc
