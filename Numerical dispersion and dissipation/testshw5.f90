!
!//////////////////////////////////////////////////////////////////
!
!	PROGRAM: testshw5.f90
!	Main program to solve the hyperbolic wave equation
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	26 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

	PROGRAM testshw5
		IMPLICIT NONE

!	------------------
!    Local Variables
!   ------------------
	REAL, PARAMETER :: pi = 3.1415927, CFL= 0.4
	INTEGER, PARAMETER		:: Nx=8	
	DOUBLE PRECISION, PARAMETER		:: dt=0.1, dx = 2*pi/REAL(Nx)
	INTEGER, PARAMETER 		:: Nt = 10/dt  
	DOUBLE PRECISION, DIMENSION(Nx+1) :: utemp
	DOUBLE PRECISION, DIMENSION(Nt+1) :: T, v
	DOUBLE PRECISION, DIMENSION(Nx+1)  :: X, u1, du1, du2, du3
	INTEGER 				:: i, j, out_file = 100, k, cas
	CHARACTER(LEN=1024) :: filename

	cas = 1
	SELECT CASE(cas)
! Test xderiv ()
		CASE(1)
			DO i=1,Nx+1
				X(i) = dx*REAL(i-1)
			ENDDO

			u1 = X

			CALL xderiv(u1, du1, Nx, dx,-1)
			CALL xderiv(u1, du2, Nx, dx,0)
			CALL xderiv(u1, du3, Nx, dx,1)

			WRITE(filename, "(A13, F3.1, A4)") "linear_deriv_",  dx, ".txt"
			OPEN(UNIT = out_file, FILE=filename)
			DO i=1,Nx+1
				WRITE(out_file,*) X(i), du1(i), du2(i), du3(i)
			ENDDO
			CLOSE(out_file)

			u1 = X**2
			CALL xderiv(u1, du1, Nx, dx,-1)
			CALL xderiv(u1, du2, Nx, dx,0)
			CALL xderiv(u1, du3, Nx, dx,1)

			WRITE(filename, "(A16, F3.1, A4)") "quadratic_deriv_",  dx, ".txt"
			OPEN(UNIT = out_file, FILE=filename)
			DO i=1,Nx+1
				WRITE(out_file,*) X(i), du1(i), du2(i), du3(i)
			ENDDO
			CLOSE(out_file)

			u1 = X**3
			CALL xderiv(u1, du1, Nx, dx,-1)
			CALL xderiv(u1, du2, Nx, dx,0)
			CALL xderiv(u1, du3, Nx, dx,1)

			WRITE(filename, "(A12, F3.1, A4)") "cubic_deriv_",  dx, ".txt"
			OPEN(UNIT = out_file, FILE=filename)
			DO i=1,Nx+1
				WRITE(out_file,*) X(i), du1(i), du2(i), du3(i)
			ENDDO
			CLOSE(out_file)

! Test CIR scheme 
		CASE(2)


! Test fwd_euler	
		CASE(3)
! test for du/dt =1 

			DO j=1, Nt+1
				T(j) = dt*(j-1)
			ENDDO

			v(1) = 2
			DO j =1, Nt 
				CALL fwd_euler(v(j), v(j+1), -1.d0, 0, dt)
	!	write(*,*) v(j)
			ENDDO

			WRITE(filename, "(A7, F3.1, A4)") "linear_",  dt, ".txt"
			OPEN(UNIT = out_file, FILE=filename)
			DO j=1,Nt+1
				WRITE(out_file,*) T(j), v(j)
				ENDDO
			CLOSE(out_file)
			v = 0

! test for du/dt = t 
			v(1) = 1 
			DO j =1, Nt 
				CALL fwd_euler(v(j), v(j+1), -T(j), 0, dt)
			ENDDO
			
			WRITE(filename, "(A10, F3.1, A4)") "quadratic_",  dt, ".txt"
			OPEN(UNIT = out_file, FILE=filename)
			DO j=1,Nt+1
				WRITE(out_file,*) T(j), v(j)
			ENDDO
			CLOSE(out_file)

	END SELECT

	END PROGRAM testshw5