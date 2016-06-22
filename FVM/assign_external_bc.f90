SUBROUTINE assign_external_bc(bc,Qext,Qin, alpha, beta)

	INTEGER :: i 
	CHARACTER(len=72) :: bc 
	REAL(8) :: Qext(3) , Qin(3)
	REAL(8) :: alpha, beta, c=1.d0

! 	write(*,*) bc
! 	write(*,*) bc == 'right'

!  	Qin(1) = 0.d0
	IF (bc == 'left' .OR. bc == 'top' .OR. bc == 'bottom') THEN
		Qext = 0.d0
! 		Qext(1) = Qin(1)
! 		Qext(2) = (alpha*Qin(1)/c + beta**2*Qin(2) - alpha*beta*Qin(3))/(alpha**2 + beta**2)
! 		Qext(3) = (beta*Qin(1)/c + alpha**2*Qin(3) - alpha*beta*Qin(2))/(alpha**2 + beta**2)
	ELSEIF (bc == 'right') THEN 
!  		Qext = 0.d0
		Qext(1) = Qin(1)
		Qext(2) = -((alpha**2-beta**2)*Qin(2) + 2*alpha*beta*Qin(3))/(alpha**2 + beta**2)
		Qext(3) = -((beta**2-alpha**2)*Qin(3) + 2*alpha*beta*Qin(2))/(alpha**2 + beta**2)
	ENDIF

END SUBROUTINE assign_external_bc
