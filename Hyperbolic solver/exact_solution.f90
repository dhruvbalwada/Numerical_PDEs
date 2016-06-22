!
!//////////////////////////////////////////////////////////////////
!
!	subroutine : exact
!	Module to put in numbers and generate the exact analytical solution
!
!   
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	21 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

      SUBROUTINE exact_solution(N, Nt, neqn,dx,dt)
      IMPLICIT NONE
!     ---------
!	ARGUMENTS
!	---------
      INTEGER			:: N, neqn, Nt
      DOUBLE PRECISION		:: dx, dt
!	----------------
!	Local Variables
!	----------------
	INTEGER 				:: i, j
      DOUBLE PRECISION 			::  ul, ur
      DOUBLE PRECISION, DIMENSION(N+1,neqn,Nt+1) :: Qtrue
      DOUBLE PRECISION, DIMENSION(N+1) :: X
      DOUBLE PRECISION, DIMENSION(Nt+1) :: T
      INTEGER 				:: out_file = 100
	CHARACTER(LEN=1024)           :: filename

      ! Calculate at later times
      DO i=1, Nt+1
      	DO j=1, N+1
      		X(j) = (j-1)*dx
      		T(i) = (i-1)*dt
      		! Region 1
      			IF (X(j)-3.d0*T(i)>= 0.d0 .AND. X(j)+T(i)<= 1.d0  ) THEN
      			CALL uo(X(j)-3.d0*T(i), ul)
      			CALL uo(X(j)+T(i), ur)
      			Qtrue(j,1,i) = 1.d0/2.d0*(ul+ur)
      			Qtrue(j,2,i) = 1.d0/2.d0*(ul-ur)

      		! Region 2
      			ELSEIF (X(j)-3.d0*T(i)>= 0.d0 .AND. X(j)+T(i)> 1.d0  ) THEN
      			CALL uo(X(j)-3.d0*T(i), ul)
      			Qtrue(j,1,i) = 1.d0/2.d0*(ul)
      			Qtrue(j,2,i) = 1.d0/2.d0*(ul)

      		! Region 3
      			ELSEIF (X(j)-3.d0*T(i)< 0.0 .AND. X(j)+T(i)<= 1.0  ) THEN
      			CALL uo(X(j)+T(i), ur)
      			CALL uo(T(i)-X(j)/3.d0, ul)
      			Qtrue(j,1,i) = 1.d0/2.d0*(-ul+ur)
      			Qtrue(j,2,i) = 1.d0/2.d0*(-ul-ur)

      		! Region 4
      		    ELSEIF (X(j)-3.d0*T(i)< 0.0 .AND. X(j)+T(i) > 1.0 .AND. T(i)-X(j)/3.0-1.d0 <= 0.d0  ) THEN
      			CALL uo(T(i)-X(j)/3.d0, ul)
      			Qtrue(j,1,i) = 1.d0/2.d0*(-ul)
      			Qtrue(j,2,i) = 1.d0/2.d0*(-ul)

      		! Region 5 
      		ELSEIF (T(i)-X(j)/3.0-1.d0 <= 0.d0   ) THEN
      			CALL uo(T(i)-X(j)/3.d0, ul)
      			Qtrue(j,1,i) = 0.d0
      			Qtrue(j,2,i) = 0.d0
      		ENDIF
      	ENDDO
      ENDDO

      WRITE(filename, "(A8, I0,A1, I0, A4)") "fileuext", N, "_", Nt, ".txt"
      OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
		WRITE(out_file,*) Qtrue(:,1,i)
	ENDDO
	CLOSE(out_file)

      WRITE(filename, "(A8, I0,A1, I0, A4)") "filevext", N, "_", Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
		WRITE(out_file,*) Qtrue(:,2,i)
	ENDDO
	CLOSE(out_file)

      WRITE(filename, "(A5, I0,A1, I0, A4)") "filex", N, "_", Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, N+1
		WRITE(out_file,*) X(i)
	ENDDO
	CLOSE(out_file)

      WRITE(filename, "(A5, I0,A1, I0, A4)") "filet", N, "_", Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
		WRITE(out_file,*) T(i)
	ENDDO
	CLOSE(out_file)

    END SUBROUTINE exact_solution
