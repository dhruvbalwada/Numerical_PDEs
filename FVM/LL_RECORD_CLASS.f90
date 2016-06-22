MODULE LL_RECORD_CLASS 
		USE EDGE_CLASS
	
	TYPE LL_RECORD
		TYPE(EDGE) :: dataedge
		TYPE(LL_RECORD), pointer :: next => null()

			CONTAINS
				PROCEDURE, PASS(this) :: construct_edge_record
	END TYPE LL_RECORD

	CONTAINS	
		SUBROUTINE construct_edge_record(this, nodeid1, nodeid2, k ,s,edgeid)
			CLASS(LL_RECORD) this
			INTEGER :: nodeid1, nodeid2, k ,s, edgeid
			
			ALLOCATE(this%next)
			NULLIFY(this%next)

			CALL this%dataedge%construct_edge(nodeid1, nodeid2, k ,s, edgeid)

		END SUBROUTINE construct_edge_record



END MODULE LL_RECORD_CLASS