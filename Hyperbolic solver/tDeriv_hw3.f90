!
!//////////////////////////////////////////////////////////////////
!
!	subroutine : tDeriv_hw3(dQdt)
!	Module to calculate the time derivative(tendency) for the system
!
!	Written as part of Homework 3 for Numerical PDE-I
! 	21 September, 2014
!	Dhruv Balwada
! 
!//////////////////////////////////////////////////////////////////

      SUBROUTINE tDeriv_hw3(Q, dQdt, N, neqn, dx, dt)
	  IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER                                      :: N, neqn    	         ! The size of the vector
      DOUBLE PRECISION                             :: dx, dt  ! the grid spacing
      DOUBLE PRECISION, DIMENSION(N+1, neqn), INTENT(IN) :: Q              ! the input vector
      DOUBLE PRECISION, DIMENSION(N+1, neqn), INTENT(OUT) :: dQdt              ! the output vector

!	---------------
!     Local Variables
!     ---------------
!
      INTEGER	:: i, j, k
      DOUBLE PRECISION, DIMENSION(neqn,neqn) :: A 
      DOUBLE PRECISION, DIMENSION(N+1,neqn) :: dQdx 
!     
!     --------------------
!     Do Computations here 	
!     --------------------
!
      DO j = 1,neqn
            CALL xDeriv(Q(:,j),dQdx(:,j),N, dx)
      ENDDO

!     This definition of A can be changed based on the problem at hand
      A(1,1) = -1.0 
      A(2,1) = -2.0
      A(1,2) = -2.0
      A(2,2) = -1.0
      dQdt = 0

      DO j = 1,neqn ! Steps to find du/dt and dv/dt
       DO i = 2,N   ! Steps at all the interior points
            DO k = 1,neqn ! Does the matrix sum du_j/dt = A_jk*u_k(vector notation)
                  dQdt(i,j) =  dQdt(i,j) + A(j,k)*dQdx(i,k)
            ENDDO
       ENDDO
      ENDDO


!     Compatibility conditions at i = 1, N+1
!     u = 0 at i=1
      dQdt(1,1) = 0.d0
      dQdt(1,2) = -dQdx(1,1) + dQdx(1,2)
!     u = v - Radiation condition at i=N+1      
      dQdt(N+1,1) = -3.d0/2.d0*(dQdx(N+1,1)+dQdx(N+1,2))
      dQdt(N+1,2) = dQdt(N+1,1)

      END SUBROUTINE tDeriv_hw3



