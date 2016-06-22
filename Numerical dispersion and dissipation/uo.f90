!
!//////////////////////////////////////////////////////////////////
!
!	subroutine : uo
!	Module to generate initial condition
!
!   
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	8 November, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

      SUBROUTINE uo(X, u, k, Nx)
      	IMPLICIT NONE 

            REAL, PARAMETER :: pi = 3.1415927
      	INTEGER							:: Nx	! size of the X axis
      	DOUBLE PRECISION, DIMENSION(Nx+1)	:: X, u ! X axis and u array 
      	INTEGER 						:: k, j ! k is case number and j is for loopss


      	SELECT CASE(k)
                  CASE(1)
                  u = sin(1*X)

      		CASE(2)
      		u = sin(4*X)

      		CASE(3)
      		DO j = 1, Nx+1
      			u(j) = (-1)**j
      		ENDDO

      		CASE(4) 
      		u = EXP(-20.0 * (X-1.5)**2)*sin(40.0*pi*X) 

      	END SELECT 
      END SUBROUTINE uo