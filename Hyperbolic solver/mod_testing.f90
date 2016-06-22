! 
! ///////////////////////////////////////////////////////////////////////////
!
! -------- mod_testing.f90 -------
! -------- for HW 3 --------------
! -----To do code testing etc.----
!
! Created by Dhruv Balwada
! Created on 9/22/2014
!
! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    PROGRAM mod_testing
      IMPLICIT NONE 

    INTEGER :: mod_num =1
    INTEGER, PARAMETER                ::  N=16
	INTEGER                             :: i, out_file=101, case_num=2
	CHARACTER(LEN=1024)                 :: filename 
	DOUBLE PRECISION, DIMENSION(N+1)    :: u, dudx, x
	DOUBLE PRECISION, PARAMETER     	:: a=16, b=-16 ,c=3, dx=1.d0/DBLE(N)
	INTEGER, PARAMETER 					:: Nt = 128
	DOUBLE PRECISION, DIMENSION(Nt+1)   :: T, V
	DOUBLE PRECISION				    :: Vtemp, dt =1.d0/DBLE(Nt)

      SELECT CASE(mod_num)
      	CASE(0)
!	----------------------
!   Variable Declarations
!   ----------------------
	

	WRITE(filename, "(A6, I0, A4)") "outdux", N , ".txt"
!   ///////// Write Tests for xDeriv //////////
!   We will define functions(whose derivatives are known)
!   such as a linear polynomial(u=ax+b), second degree polynomial
!   (u = ax^2+bx+c) and check if xDeriv gives the expected results
!   and if the accuracy is close to the expected accuracy.
!   We try different cases with degree 1(linear) and 2(quadratic) polynomials

	SELECT CASE(case_num)
		CASE(0)
			DO i = 1, (N+1)
				u(i) = b*(i-1)*dx + c
				x(i) = dx*(i-1)
			ENDDO
		CASE(1)
			DO i = 1, (N+1)
				u(i) = a*((i-1)*dx)**2 + b*(i-1)*dx + c
				x(i) = dx*(i-1)
			ENDDO
		CASE(2)
			DO i = 1, (N+1)
				u(i) = a*((i-1)*dx)**3 + b*(i-1)*dx + c
				x(i) = dx*(i-1)
			ENDDO
	END SELECT

	
	CALL xDeriv(u, dudx, N, dx)

	OPEN(UNIT=out_file, FILE= filename)
	DO i=1, (N+1)	
		WRITE(out_file,*) x(i), u(i) , dudx(i)
	ENDDO
	CLOSE(out_file)

	CASE(1)
!   ///////// Write Tests for rkLS //////////
!	We run this test on a simple equations u_t = at+b with 
!	initial conditions u(0) = 0
	

	WRITE(filename, "(A6, I0, A4)") "outdut", Nt , ".txt"
	V=0.d0
	T(1)=0.d0

	DO i=2, Nt+1
		Vtemp = V(i-1)
		
		CALL rkLs(Vtemp, 1, 0, T(i-1), 0, dt)
		V(i) = Vtemp
		T(i) = (i-1)*dt

	ENDDO

	OPEN(UNIT=out_file, FILE= filename)
	DO i=1, (Nt+1)	
		WRITE(out_file,*) T(i), V(i)
	ENDDO
	CLOSE(out_file)

	END SELECT
	END PROGRAM mod_testing


!   ///////// Write Tests for rkLs   //////////      