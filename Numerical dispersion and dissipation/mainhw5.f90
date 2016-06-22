!
!//////////////////////////////////////////////////////////////////
!
!	PROGRAM: mainhw5.f90
!	Main program to solve the hyperbolic wave equation
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	26 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

	PROGRAM mainhw5
		IMPLICIT NONE

!	------------------
!    Local Variables
!   ------------------
	REAL, PARAMETER :: pi = 3.1415927, CFL= 1.5
	INTEGER, PARAMETER		:: Nx=31
	DOUBLE PRECISION, PARAMETER		:: dx = 2*pi/REAL(Nx+1) , dt=CFL*2*pi/REAL(Nx+1)  ! dt = CFL/REAL(160), dx = 1/REAL(160)!dx = 2*pi/REAL(Nx+1) , dt=CFL*2*pi/REAL(Nx+1) 
	INTEGER, PARAMETER 		:: Nt = 10/dt  
	DOUBLE PRECISION, DIMENSION(Nx+1,Nt+1) :: u
	DOUBLE PRECISION, DIMENSION(Nx+1) :: utemp, utemp1, utemp2, utemp3
	DOUBLE PRECISION, DIMENSION(Nt+1) :: T
	DOUBLE PRECISION, DIMENSION(Nx+1)  :: X
	INTEGER 				:: i, j, out_file = 100, k
	CHARACTER(LEN=1024) :: filename

!   Set Initial Conditions
	k = 2 ! set problem number
	T(1) = 0.d0
	
	DO i=1,Nx+1
		X(i) = dx*REAL(i-1)
	ENDDO

	CALL uo(X, utemp, k, Nx)
	u(:,1) = utemp

!   Solve the equation with LW and CIR schemes 
!   CIR 
	DO j =1,Nt 
		utemp = 0
		CALL xderiv(u(:,j), utemp, Nx, dx,-1)
		CALL fwd_euler(u(:,j), u(:,j+1), utemp, Nx, dt)
		T(j+1) = T(j) + dt
	ENDDO

	WRITE(filename, "(A9, I0, A1, I0, I0, A4)") "fileucir_", INT(CFL*10), "_", Nx, Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
	WRITE(out_file,*) u(:,i)
	ENDDO
	CLOSE(out_file)

	WRITE(filename, "(A6, I0, A1, I0, I0, A4)") "filex_", INT(CFL*10), "_", Nx, Nt,  ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nx+1
	WRITE(out_file,*) X(i)
	ENDDO
	CLOSE(out_file)

	WRITE(filename, "(A6, I0, A1, I0, I0,  A4)") "filet_", INT(CFL*10), "_", Nx, Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
	WRITE(out_file,*) T(i)
	ENDDO
	CLOSE(out_file)	

	u(:,2:Nt) = 0

	!   LW
	DO j =1,Nt
		utemp = 0 
		CALL xderiv(u(:,j), utemp, Nx, dx, 0) ! Calculate the centered diff for LW
		CALL xderiv(u(:,j), utemp1, Nx, dx, -1) ! Calculate the backward diff for LW
		CALL xderiv(utemp1, utemp2, Nx, dx, 1) ! Calculate the forward diff of the backward diff 
		utemp3 = utemp - dt/2*utemp2
		CALL fwd_euler(u(:,j), u(:,j+1), utemp3, Nx, dt)
	ENDDO

	WRITE(filename, "(A8, I0,A1, I0, I0,  A4)") "fileulw_", INT(CFL*10), "_", Nx, Nt, ".txt"
	OPEN(UNIT = out_file, FILE=filename)
	DO i=1, Nt+1
	WRITE(out_file,*) u(:,i)
	ENDDO
	CLOSE(out_file)

	END PROGRAM mainhw5


