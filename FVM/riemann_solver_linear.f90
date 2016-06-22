SUBROUTINE riemann_solver_linear(uin, uext, A, modA, F)
	IMPLICIT NONE

	REAL(8) , DIMENSION(3,3) :: A, modA, Aplus, Aminus
	REAL(8) , DIMENSION(3)	 :: uin, uext , F
! 	REAL(8) 				 :: 
	INTEGER 				:: i,p,o
	Aplus =  0.5d0*(A + modA)
	Aminus = 0.5d0*(A - modA)

! 	write(*,*) 'plus'
! 	do p =1,3
! 					write(*,'(3f16.10)') (Aplus(p,o), o=1,3)
! 				ENDDO

! 	write(*,*) 'minus'
! 	do p =1,3
! 					write(*,'(3f16.10)') (Aminus(p,o), o=1,3)
! 				ENDDO				
!  	write(*,*) Aplus
	
	DO i =1,3
		F(i) = Aplus(i,1)*uin(1)+ Aplus(i,2)*uin(2) + Aplus(i,3)*uin(3) & 
			+Aminus(i,1)*uext(1)+ Aminus(i,2)*uext(2) + Aminus(i,3)*uext(3)
	ENDDO
! 	CALL DGEMV('N',3,3,1.d0,Aplus,3, uin,1 ,0.d0, F,1)
! 	CALL DGEMV('N',3,3,1.d0,Aminus,3, uext,1 ,1.d0, F,1)

END SUBROUTINE riemann_solver_linear