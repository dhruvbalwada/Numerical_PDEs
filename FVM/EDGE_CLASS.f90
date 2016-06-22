MODULE EDGE_CLASS

TYPE EDGE 
	INTEGER :: nodeids(2)
	INTEGER :: elementids(2) =0 
	INTEGER :: elementsides(2) = 0
	
	INTEGER :: edgeid

	CONTAINS
		PROCEDURE, PASS(this) :: construct_edge 
END TYPE EDGE

CONTAINS
		SUBROUTINE construct_edge(this, nodeid1, nodeid2, k ,s, edgeid)
			CLASS(EDGE) :: this 
			INTEGER :: nodeid1, nodeid2, k ,s, edgeid
			
			this%nodeids(1) = nodeid1
			this%nodeids(2) = nodeid2
			this%elementids(1) = k
			this%elementsides(1) = s
			this%edgeid = edgeid

		END SUBROUTINE construct_edge


END MODULE EDGE_CLASS