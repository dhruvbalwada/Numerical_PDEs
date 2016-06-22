		SUBROUTINE find_edge(mesh_var)
			USE EDGE_CLASS
			USE MESH_CLASS
			USE LLIST_CLASS

			IMPLICIT NONE
			
			TYPE(LLIST) :: listedge
			TYPE(MESH) :: mesh_var
			INTEGER, ALLOCATABLE :: exists(:,:) 
			INTEGER :: edgeid, eid, nodeid, k, s, i,j, nodeid1, nodeid2
			INTEGER :: nodemap(2,4)
			TYPE(LL_RECORD), pointer :: current
			INTEGER :: eid1, eid2, esid1, esid2
			REAL(8) :: magnor


			CALL listedge%construct()


			ALLOCATE(exists(size(mesh_var%nodes), size(mesh_var%nodes)))
			exists = 0
! 			write(*,*) size(exists)
			edgeid = 0
			nodemap = RESHAPE([1,2,2,3,3,4,4,1],[2,4])


		!	write(*,*) size(exists)
			DO k = 1,size(mesh_var%elements)
				
! 				write(*,*) 'test1'
! 				nodemap = RESHAPE([mesh_var%elements%nodeids(1), mesh_var%elements%nodeids(2), mesh_var%elements%nodeids(2), &
! 				mesh_var%elements%nodeids(3), mesh_var%elements%nodeids(3), mesh_var%elements%nodeids(4), mesh_var%elements%nodeids(4), &
! 				mesh_var%elements%nodeids(1) ],[2,4])
				
				DO s= 1,4
					nodeid1 = mesh_var%elements(k)%nodeids(nodemap(1,s))
					nodeid2 = mesh_var%elements(k)%nodeids(nodemap(2,s))
! 					write(*,*) nodeid1
! 					write(*,*) nodeid2
					i = max(nodeid1,nodeid2)
					j = min(nodeid1,nodeid2)

					IF (exists(i,j)== 0) THEN
						edgeid = edgeid+1
	!					write(*,*) edgeid
						CALL listedge%add(nodeid1, nodeid2, k, s, edgeid)
! 						write(*,*) listedge%head%dataedge%edgeid
! 						write(*,*) listedge%tail%dataedge%edgeid
						!CALL listedge%count()
						exists(i,j) = edgeid
					ELSE

						eid = exists(i,j)
						
! 						write(*,*) edgeid
! 						write(*,*) 20
! 						write(*,*) eid
						CALL listedge%modify(k,s,eid)
					ENDIF
				ENDDO
			ENDDO

			!CALL listedge%count_list()

! 			ALLOCATE(current)
! 			NULLIFY(current)
			current => listedge%head

		!	write(*,*) 'here'
		!	write(*,*) current%dataedge%edgeid

			DO k=1,edgeid
				
				eid1 = current%dataedge%elementids(1)
				eid2 = current%dataedge%elementids(2)
				esid1 = current%dataedge%elementsides(1)
				esid2 = current%dataedge%elementsides(2)
! 				write(*,*) eid1
! 				write(*,*) eid2

				IF ( eid1/= 0 .AND. eid2 /= 0) THEN

					mesh_var%elements(eid1)%nbrelemids(esid1) = eid2
					mesh_var%elements(eid2)%nbrelemids(esid2) = eid1

					mesh_var%elements(eid1)%bcnames(esid1) = 'none'
					mesh_var%elements(eid2)%bcnames(esid2) = 'none'

					mesh_var%elements(eid1)%normals(esid1,1) = mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(2,esid1)))%X(2) & 
													- mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(1,esid1)))%X(2)
					mesh_var%elements(eid1)%normals(esid1,2) = -(mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(2,esid1)))%X(1) &
													- mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(1,esid1)))%X(1))
					
					magnor = sqrt(mesh_var%elements(eid1)%normals(esid1,1)**2 + mesh_var%elements(eid1)%normals(esid1,2)**2)
					mesh_var%elements(eid1)%length(esid1) = magnor
					mesh_var%elements(eid1)%normals(esid1,1) = mesh_var%elements(eid1)%normals(esid1,1)/magnor
					mesh_var%elements(eid1)%normals(esid1,2) = mesh_var%elements(eid1)%normals(esid1,2)/magnor

					mesh_var%elements(eid2)%normals(esid2,1) = mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(2,esid2)))%X(2) &
															- mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(1,esid2)))%X(2)
					mesh_var%elements(eid2)%normals(esid2,2) = -(mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(2,esid2)))%X(1) &
												- mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(1,esid2)))%X(1))
					magnor = sqrt(mesh_var%elements(eid2)%normals(esid2,1)**2 + mesh_var%elements(eid2)%normals(esid2,2)**2)
					mesh_var%elements(eid2)%length(esid2) = magnor
					mesh_var%elements(eid2)%normals(esid2,1) = mesh_var%elements(eid2)%normals(esid2,1)/magnor
					mesh_var%elements(eid2)%normals(esid2,2) = mesh_var%elements(eid2)%normals(esid2,2)/magnor

				ELSEIF (eid2 /= 0 .AND. eid1 == 0) THEN
					mesh_var%elements(eid2)%nbrelemids(esid2) = 0

					mesh_var%elements(eid2)%normals(esid2,1) = mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(2,esid2)))%X(2) &
													- mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(1,esid2)))%X(2)
					mesh_var%elements(eid2)%normals(esid2,2) = -(mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(2,esid2)))%X(1) &
											 - mesh_var%nodes(mesh_var%elements(eid2)%nodeids(nodemap(1,esid2)))%X(1))
					magnor = sqrt(mesh_var%elements(eid2)%normals(esid2,1)**2 + mesh_var%elements(eid2)%normals(esid2,2)**2)
					mesh_var%elements(eid2)%length(esid2) = magnor
					mesh_var%elements(eid2)%normals(esid2,1) = mesh_var%elements(eid2)%normals(esid2,1)/magnor
					mesh_var%elements(eid2)%normals(esid2,2) = mesh_var%elements(eid2)%normals(esid2,2)/magnor

					CALL assign_bc(mesh_var%elements(eid2)%normals(esid2,:), mesh_var%elements(eid2)%bcnames(esid2))

				ELSEIF (eid2 == 0 .AND. eid1 /= 0) THEN

! 					write(*,*) 'here'
! 					write(*,*) esid1

! 					write(*,*) ' Element and edge'
! 					write(*,*) eid1
! 					write(*,*) esid1

					mesh_var%elements(eid1)%nbrelemids(esid1) = 0

					mesh_var%elements(eid1)%normals(esid1,1) = mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(2,esid1)))%X(2) &
												- mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(1,esid1)))%X(2)
					mesh_var%elements(eid1)%normals(esid1,2) = -(mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(2,esid1)))%X(1) &
											 - mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(1,esid1)))%X(1))
				
					magnor = sqrt(mesh_var%elements(eid1)%normals(esid1,1)**2 + mesh_var%elements(eid1)%normals(esid1,2)**2)
					
					mesh_var%elements(eid1)%length(esid1) = magnor
! 					write(*,*) ' Y pos of node 2 and node 1'
! 					write(*,*) mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(2,esid1)))%X(1) 
! 					write(*,*) mesh_var%nodes(mesh_var%elements(eid1)%nodeids(nodemap(1,esid1)))%X(1) 

! 					write(*,*) mesh_var%elements(eid1)%normals(esid1,2)
					!write(*,*) magnor

					mesh_var%elements(eid1)%normals(esid1,1) = mesh_var%elements(eid1)%normals(esid1,1)/magnor
					mesh_var%elements(eid1)%normals(esid1,2) = mesh_var%elements(eid1)%normals(esid1,2)/magnor

					CALL assign_bc(mesh_var%elements(eid1)%normals(esid1,:), mesh_var%elements(eid1)%bcnames(esid1))
				ENDIF

				current => current%next
			ENDDO

! 			DEALLOCATE(current)

write(*,*) 'test1'



		END SUBROUTINE find_edge