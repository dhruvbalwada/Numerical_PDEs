MODULE LLIST_CLASS 
	USE LL_RECORD_CLASS

	TYPE LLIST
		TYPE(LL_RECORD), pointer :: head , tail

		CONTAINS
			PROCEDURE, PASS(this) :: construct, destruct, add, modify, count_list

	END TYPE LLIST

	CONTAINS

		SUBROUTINE construct(this)
			IMPLICIT NONE
			CLASS(LLIST) :: this 
			ALLOCATE(this%head)
			ALLOCATE(this%tail)
			NULLIFY(this%head)
			NULLIFY(this%tail)
		END SUBROUTINE construct

		SUBROUTINE destruct(this)
			IMPLICIT NONE
			CLASS(LLIST) :: this 
			DEALLOCATE(this%head)
			DEALLOCATE(this%tail)
		END SUBROUTINE destruct

		SUBROUTINE add(this, nodeid1, nodeid2, k ,s, edgeid)
			IMPLICIT NONE
			CLASS(LLIST) :: this 
			TYPE(LL_RECORD), pointer :: newdata
			INTEGER :: nodeid1, nodeid2, k ,s, edgeid

			ALLOCATE(newdata)

			CALL newdata%construct_edge_record( nodeid1, nodeid2, k ,s, edgeid)


			IF(ASSOCIATED(this%tail)) THEN
		!		write(*,*) this%head%dataedge%edgeid
		!		write(*,*) this%tail%dataedge%edgeid
			 	this%tail%next => newdata 
			 	this%tail => newdata 
			ELSE
				this%head => newdata
				this%tail => newdata 
			ENDIF

		!	DEALLOCATE(newdata)
		END SUBROUTINE add 

		SUBROUTINE modify(this, k, s, edgeid)
			IMPLICIT NONE
			CLASS(LLIST) :: this 
			TYPE(LL_RECORD), pointer :: current
			INTEGER		:: k, s, edgeid

		!	ALLOCATE(current)
		!	write(*,*) this%tail%dataedge%edgeid

			current => this%head

		!	write(*,*) current%dataedge%edgeid
		!   write(*,*) 'here'

			DO 
				IF(current%dataedge%edgeid == edgeid) THEN
					current%dataedge%elementids(2) = k
					current%dataedge%elementsides(2) = s

					EXIT
				ELSE
					current => current%next
				ENDIF
			ENDDO

!			DEALLOCATE(current)
		END SUBROUTINE modify

		SUBROUTINE count_list(this)
			CLASS(LLIST) :: this
			TYPE(LL_RECORD), pointer :: current
			INTEGER :: countnum 
			
			countnum = 0 
			current => this%head

			DO 
				write(*,*) current%dataedge%edgeid
				
				IF (ASSOCIATED(current%next)) THEN
					countnum = countnum+1
				ELSE
					EXIT
				ENDIF
				current => current%next
			ENDDO
! 			DO 
! 				IF (ASSOCIATED(current)) THEN
! 					countnum = countnum+1 
! 					write(*,*) countnum
! 				ELSE
! 					EXIT
! 				ENDIF	
! 				current => current%next
! 			ENDDO

			write(*,*) current%dataedge%edgeid
			write(*,*) this%head%dataedge%edgeid

		END SUBROUTINE count_list

			
! 		SUBROUTINE delete(this,edgeid)
! 			CLASS(LLIST) :: this
! 			TYPE(LL_RECORD) :: tmp, current
! 			INTEGER :: edgeid
! 			current => this%head
	
! 			! if the record sits at the head 
! 			IF (current%data%edgeid == edgeid) THEN
! 				this%head => current%next 
! 				DEALLOCATE(current)

! 			ELSEIF (ASSOCIATED(current, this%tail))
				
					
! 					IF(current%next%data%edgeid == edgeid) THEN 
! 						tmp => current%next
! 						current%next => tmp%next 
! 						DEALLOCATE(tmp)

END MODULE LLIST_CLASS



