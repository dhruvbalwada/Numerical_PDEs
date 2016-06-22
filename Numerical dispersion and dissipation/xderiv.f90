!
!//////////////////////////////////////////////////////////////////
!
!	SUBROUTINE : xderiv
!	Program to calculate the proper derivative using periodic bc
!	Written as part of Homework 5 for Numerical PDE-I
! 	8 November, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

	SUBROUTINE xderiv( u, dudx, Nx, dx, cas)
		IMPLICIT NONE

!	  ---------
!     Arguments
!     ---------
!
      INTEGER                                      :: Nx, cas   ! The size of the vector
      DOUBLE PRECISION                             :: dx   ! the grid spacing
      DOUBLE PRECISION, DIMENSION(Nx+1), INTENT(IN) :: u    ! the input vector
      DOUBLE PRECISION, DIMENSION(Nx+1), INTENT(OUT):: dudx ! the result
!
!	---------------
!     Local Variables
!     ---------------
!
      INTEGER	:: j

      SELECT CASE(cas)
      
      case(-1)     
!	---- j=1	- Using periodic BC 

		dudx(1) = (u(1)-u(Nx+1))/(dx)

!	---- j=2:N+1- Backward Difference

            DO j = 2, Nx+1
      	  dudx(j)= (u(j)-u(j-1))/(dx)
	     ENDDO

      case(0) ! centered
!     ---- j=1    - Using periodic BC 

            dudx(1) = (u(2)-u(Nx+1))/(2*dx)

!     ---- j=2:N- Backward Difference

            DO j = 2, Nx
              dudx(j)= (u(j+1)-u(j-1))/(2*dx)
           ENDDO
!     ----- j = Nx+1           
             dudx(Nx+1) = (u(1)-u(Nx))/(2*dx)

      case(1) ! forward

!     ---- j=2:N+1- Backward Difference

            DO j = 1, Nx
              dudx(j)= (u(j+1)-u(j))/(dx)
           ENDDO
            dudx(Nx+1) = (u(1)-u(Nx+1))/(dx)


	  END SELECT

	  END SUBROUTINE xderiv