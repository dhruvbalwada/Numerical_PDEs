SUBROUTINE assign_equations(Aout,modA, Nx, Ny)

	IMPLICIT NONE

	REAL(8) :: A(3,3), B(3,3), Aout(3,3), modA(3,3)
	REAL(8) :: rho = 1.d0, c = 1.d0, Nx, Ny
	
	A = 0.d0
! 	A(1,1) = -c**2
	A(1,2) = rho*c**2
	A(2,1) = 1.d0/rho

	B = 0.d0
! 	B(1,1) = -c**2
	B(1,3) = rho*c**2
	B(3,1) = 1.d0/rho

	Aout = Nx*A + Ny*B

	modA = 0.d0
	modA(1,1)  = c
	modA(2,2)  = c*Nx**2
	modA(3,3)  = c*Ny**2
	modA(2,3)  = c*Nx*Ny
	modA(3,2)  = c*Nx*Ny

END SUBROUTINE assign_equations