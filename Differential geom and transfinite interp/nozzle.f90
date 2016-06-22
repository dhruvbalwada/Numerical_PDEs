! function for nozzle shape 
! Transonic Flowfield in a Supersonic Nozzle with Small Throat 
! radius of Curvature AIAA J., 1969, pp. 1364-1366
! Cuffel, R.F. , L.F. Back and P.F. Massier

 	 FUNCTION nozzle(zeta,eta,num)
 		IMPLICIT NONE

 		integer				  :: num
 		REAL(8)               :: quar_annulus
 		REAL(8)				  :: zeta, eta
 		REAL(8), PARAMETER		:: PI = 3.1415926535897932385
 		REAL(8)				  :: area

 		IF zeta == 0 THEN
			IF (num .eq. 1)  THEN
 			quar_annulus = 0.d0
 			ELSEIF (num .eq. 2) THEN
 			quar_annulus = area(0.d0)*eta
 			ENDIF
 		ENDIF



 	END FUNCTION nozzle


