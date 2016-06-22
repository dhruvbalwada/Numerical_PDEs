!
!//////////////////////////////////////////////////////////////////
!
!	PROGRAM: solve_hyp.f90
!	Main program to solve the hyperbolic wave equation
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	26 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

	PROGRAM solve_hyp
		IMPLICIT NONE

!	------------------
!    Local Variables
!   ------------------

	INTEGER, PARAMETER		:: Nt=308, N=256, neqn=2  	! Number of time steps, x space steps
	DOUBLE PRECISION		:: dt =1/REAL(Nt), dx = 1/REAL(N), start, finish
	DOUBLE PRECISION, DIMENSION(N+1,neqn,Nt+1) :: Q
	DOUBLE PRECISION, DIMENSION(N+1, neqn) :: Qtemp
	DOUBLE PRECISION, DIMENSION(Nt+1) :: T
	DOUBLE PRECISION, DIMENSION(N+1)  :: X
	INTEGER 				:: i, j, out_file = 100
	CHARACTER(LEN=1024) :: filename
!   Set Initial Conditions
	T(1) = 0.d0
	
	DO i=1,N+1
		X(i) = dx*REAL(i-1)
		Q(i,1,1) = EXP(-100.0 * (X(i)-0.5)**2)
		Q(i,2,1) = 0.0
	ENDDO

	CALL exact_solution(N, Nt, neqn,dx,dt)

! 	Do numerical computations
	CALL CPU_TIME(start)
	DO i=2, Nt+1

		Qtemp = Q(:,:,i-1)
		CALL rkLs(Qtemp,neqn, N, T(i-1), dx, dt)
		Q(:,:,i) = Qtemp
		T(i) = T(i-1) + dt

	ENDDO

	CALL CPU_TIME(finish)

	WRITE(*,*) finish-start

    WRITE(filename, "(A8, I0,A1, I0, A4)") "fileunum", N, "_", Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
	WRITE(out_file,*) Q(:,1,i)
	ENDDO
	CLOSE(out_file)

    WRITE(filename, "(A8, I0,A1, I0, A4)") "filevnum", N, "_", Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
	WRITE(out_file,*) Q(:,2,i)
	ENDDO
	CLOSE(out_file)

	END PROGRAM solve_hyp
