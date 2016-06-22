SUBROUTINE write_solution(Q, N,dt)

	USE MESH_CLASS
	IMPLICIT NONE
! 	TYPE(MESH) :: Y 
	REAL(8), DIMENSION(:,:) :: Q
	CHARACTER(len=32) :: myfmt, filename
	REAL(8) :: dt, T
	INTEGER :: out_unit = 150,i,j, N 
	! Still working here
	
	T = dt*N
	write(filename, '(a, F5.3, a)') './solution/Sol_', T ,'.txt'

	OPEN(UNIT=out_unit, FILE = filename)
	
	DO i = 1, size(Q,1)
		write(out_unit, '(3f25.10)') (Q(i,j), j=1,3)
	END DO

END SUBROUTINE write_solution
