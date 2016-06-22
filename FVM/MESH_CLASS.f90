! 

MODULE MESH_CLASS

USE NODE_CLASS
USE ELEMENT_CLASS


TYPE MESH
	TYPE(NODE), ALLOCATABLE :: nodes(:)
	TYPE(ELEMENT), ALLOCATABLE :: elements(:)
		CONTAINS
			PROCEDURE, PASS(this) :: construct_mesh
			PROCEDURE, PASS(this) :: destruct_mesh
			PROCEDURE, PASS(this)		:: READ_MESH
		!	FINAL, PASS(this) :: destruct_mesh
END TYPE MESH

CONTAINS
		SUBROUTINE construct_mesh(this, nnodes, nelem)
			!IMPORT MESH
			IMPLICIT NONE
			CLASS(MESH) :: this
			INTEGER :: nnodes, nelem
			
				ALLOCATE(this%nodes(nnodes))
				ALLOCATE(this%elements(nelem))
		END SUBROUTINE construct_mesh

		SUBROUTINE destruct_mesh(this)
				IMPLICIT NONE
			!IMPORT MESH
				CLASS(MESH) :: this
				DEALLOCATE(this%nodes)
				DEALLOCATE(this%elements)
		END SUBROUTINE destruct_mesh

		SUBROUTINE READ_MESH(this, filenamenodes, filenameconn)
			IMPLICIT NONE
			CLASS(MESH) :: this
			INTEGER :: out_id = 150, InputStatus, nnodes,nelems, i, j
			CHARACTER(len=*) :: filenamenodes, filenameconn
			REAL(8) , DIMENSION(2) ::  nodex
			REAL(8) , DIMENSION(4) :: elems

			OPEN(UNIT=out_id, FILE = filenamenodes)

			nnodes = 0
			DO 
				READ(out_id, '(2f20.10)', IOSTAT = InputStatus) nodex

				IF(InputStatus>0) STOP 
				IF(InputStatus<0) EXIT 
			 	nnodes = nnodes + 1
			ENDDO

			OPEN(UNIT=out_id, FILE = filenameconn)
			nelems = 0
			DO 
				READ(out_id, '(4F20.10)', IOSTAT = InputStatus) elems
				IF(InputStatus>0) STOP 
				IF(InputStatus<0) EXIT 
			 	nelems = nelems + 1
			ENDDO

			write(*,*) nelems
			write(*,*) nnodes

 			CALL this%construct_mesh(nnodes,nelems)

			OPEN(UNIT=out_id, FILE = filenamenodes)
			DO i = 1,nnodes
				READ(out_id, '(2f20.10)') this%nodes(i)%X
			ENDDO

			OPEN(UNIT=out_id, FILE = filenameconn)
			DO i = 1,nelems
				READ(out_id, '(4f20.10)') elems
				this%elements(i)%nodeids = int(elems)
			ENDDO


		END SUBROUTINE READ_MESH


END MODULE MESH_CLASS

