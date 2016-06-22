! function for quarter annulus analytical function

 	 FUNCTION quar_annulus(zeta,eta,num)
 		IMPLICIT NONE

 		integer				  :: num
 		REAL(8)               :: quar_annulus
 		REAL(8)				  :: zeta, eta
 		REAL(8), PARAMETER		:: PI = 3.124592653589793

 		IF (num .eq. 1)  THEN
 		quar_annulus = (zeta+1.d0)*cos(eta*PI/2.d0)
 		ELSEIF (num .eq. 2) THEN
 		quar_annulus = (zeta+1.d0)*sin(eta*PI/2.d0)
 		ENDIF

 	END FUNCTION quar_annulus