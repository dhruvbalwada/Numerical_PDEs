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

      SUBROUTINE uo(X, u)
      	IMPLICIT NONE 

      	DOUBLE PRECISION 				:: X, u

      	u = EXP(-100.0 * (X-0.5)**2) 

      END SUBROUTINE uo