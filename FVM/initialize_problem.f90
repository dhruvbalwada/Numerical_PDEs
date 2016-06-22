SUBROUTINE initialize_problem(Q,Ymesh)
	
	USE MESH_CLASS
	IMPLICIT NONE

	TYPE(MESH) :: Ymesh
	REAL(8), DIMENSION(:,:,:) :: Q
	INTEGER :: i, j
	REAL(8), DIMENSION(4) :: X, Y, temp
	! initiailze u and v = 0
	

	Q(:,2,:) = 0.0d0
	Q(:,3,:) = 0.0d0



	DO i=1, size(Q,1)
		DO j =1,4 
			X(j)= Ymesh%nodes(Ymesh%elements(i)%nodeids(j))%X(1)
			Y(j)= Ymesh%nodes(Ymesh%elements(i)%nodeids(j))%X(2)
			temp(j) = exp(-log(2.d0)*(((X(j)-0.5d0)**2+(Y(j)-0.5d0)**2)/0.06d0**2))
		ENDDO

		Q(i,1,1) = 0.25d0*(sum(temp))
	ENDDO

END SUBROUTINE
