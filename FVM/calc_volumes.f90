SUBROUTINE calc_volumes(Y)
	
	USE MESH_CLASS
	IMPLICIT NONE

	TYPE(MESH):: Y 
	INTEGER  :: i 
	REAL(8)		::  dx24,dx31, dy31, dy24

		DO i =1, size(Y%elements)
			dx24 = Y%nodes(Y%elements(i)%nodeids(4))%X(1) - Y%nodes(Y%elements(i)%nodeids(2))%X(1)   
			dy24 = Y%nodes(Y%elements(i)%nodeids(4))%X(2) - Y%nodes(Y%elements(i)%nodeids(2))%X(2)   
			dy31 = Y%nodes(Y%elements(i)%nodeids(1))%X(2) - Y%nodes(Y%elements(i)%nodeids(3))%X(2)   
			dx31 = Y%nodes(Y%elements(i)%nodeids(1))%X(1) - Y%nodes(Y%elements(i)%nodeids(3))%X(1)   

			Y%elements(i)%volume = 0.5d0*(dx24*dy31 - dx31*dy24)

		ENDDO

	END SUBROUTINE calc_volumes