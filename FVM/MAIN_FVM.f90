! The main function that controls the Finite Volume Code

PROGRAM MAIN_FVM

	USE MESH_CLASS

	IMPLICIT NONE

	INTERFACE 
	SUBROUTINE initialize_problem(Q,Ymesh)
	
		USE MESH_CLASS
		IMPLICIT NONE

		TYPE(MESH) :: Ymesh
		REAL(8), DIMENSION(:,:,:) :: Q
		INTEGER :: i, j
		REAL(8), DIMENSION(4) :: X, Y, temp
	END SUBROUTINE initialize_problem

	SUBROUTINE write_solution(Q, N,dt)

	USE MESH_CLASS
	IMPLICIT NONE
! 	TYPE(MESH) :: Y 
	REAL(8), DIMENSION(:,:) :: Q
	CHARACTER(len=32) :: myfmt, filename
	REAL(8) :: dt, T
	INTEGER :: out_unit = 150,i,j, N 
	END SUBROUTINE write_solution

	END INTERFACE

	TYPE(MESH) :: Y
	INTEGER :: elemid = 2, Nt, k,i, j, v, p, o
! 	REAL(8), DIMENSION(3) :: uin, uext, F 
 	REAL(8), DIMENSION(3,3) :: A, modA
 	REAL(8), DIMENSION(3) :: flux, Qext,sumflux 
	REAL(8) :: dt
	REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: Q
	REAL(8)		:: Tmax = 1.d0, Nx, Ny, len, CFL=0.5

	write(*,*) 'blah'
	CALL Y%READ_MESH('./grid_generator/nodes_128.txt', './grid_generator/connections_128.txt')
	write(*,*) 'blah'
	CALL find_edge(Y)
	CALL calc_volumes(Y)

	


	dt = CFL * (Y%nodes(Y%elements(1)%nodeids(2))%X(1) - Y%nodes(Y%elements(1)%nodeids(1))%X(1))
	Nt = Tmax/dt
! 	CFL = 1.d0*dt/(Y%nodes(Y%elements(1)%nodeids(2))%X(1) - Y%nodes(Y%elements(1)%nodeids(1))%X(1) )! - &

	write(*,*) CFL

	IF (CFL>=1.d0) STOP
	write(*,*) size(Y%elements)

	ALLOCATE(Q(size(Y%elements),3,2)) ! number of elements, number of variables, number of timesteps
! 	write(*,*) ' test'

	Q= 0.d0
	
	CALL initialize_problem(Q,Y)
	
!  	CALL write_solution(Q(:,:,1),0)
! 	CALL assign_equations(A,B)
	DO k = 1,Nt

		DO i =1,size(Y%elements)
			
			sumflux =0.d0
			DO j =1,4 ! calculate the 4 fluxes
				Nx = Y%elements(i)%normals(j,1)
				Ny = Y%elements(i)%normals(j,2)
				len = Y%elements(i)%length(j)

				CALL assign_equations(A, modA, Nx, Ny)
! 				C = Nx*A + Ny*B
! 				do p =1,3
! 					write(*,'(3f16.10)') (C(p,o), o=1,3)
! 				ENDDO
! 				write(*,*) Y%elements(i)%nbrelemids(j)
! 				IF(i==44)	write(*,*) Y%elements(i)%nbrelemids(j)
				IF (Y%elements(i)%nbrelemids(j)==0) THEN
					
					CALL assign_external_bc(Y%elements(i)%bcnames(j), Qext, Q(i,:,1), Nx, Ny)
				ELSEIF (Y%elements(i)%nbrelemids(j)/=0) THEN
					Qext = Q(Y%elements(i)%nbrelemids(j),:,1)
				ENDIF

! 			IF(i==44)	write(*,*) 'External'
! 			IF(i==44)	write(*,*) Qext
! 			IF(i==44)	write(*,*) 'Internal'
! 			IF(i==44)	write(*,*) Q(i,:,1)

! 			IF(i==44)	write(*,*) 'blah'
				CALL riemann_solver_linear(Q(i,:,1), Qext , A, modA, flux)
!  			IF(i==44)	write(*,*) flux
				sumflux = sumflux+flux*len
! 				write(*,*) sumflux
			ENDDO
! 			IF(i==44)write(*,*) 'WHAAT'

			! do the time time stepping 
! 			write(*,*) Y%elements(i)%volume

			DO v = 1,3
				Q(i,v,2) = Q(i,v,1) - dt*sumflux(v)/Y%elements(i)%volume
			ENDDO

		ENDDO
		! Write to file 
		IF (mod(k,1) == 0) THEN
			CALL write_solution(Q(:,:,2),k,dt)
		ENDIF

		! Move the matrix in time 
		Q(:,:,1) = Q(:,:,2)
	ENDDO

	WRITE(*,*) 'I made it'

END PROGRAM
