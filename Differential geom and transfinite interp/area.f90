 	FUNCTION area(x, num)
 		IMPLICIT NONE

 		integer 			:: i, j, k , num
 		REAL(8)				:: area, darea, x , t

 		IF (x .LE. 0.31d0) THEN
 			area = 2.5d0
 			darea = 0.d0
 		ELSEIF (x .LE. 0.874697d0) THEN 
 			t = ASIN((x - 0.31d0)/0.8d0)/0.7836528d0
 			area = 2.5d0 + 0.8d0*(COS(0.7836528d0*t)-1.d0)
 			darea = (-SIN(0.7836528d0*t))/SQRT(1-((x - 0.31d0)/0.8d0)**2)
 		ELSEIF (x .LE. 2.20016d0) THEN 
 			t = (x- 0.874697d0)/1.325463d0
 			area = 2.26667d0 - TAN(0.7836d0)*t*1.325463d0
 			darea = - TAN(0.7836d0)
 		ELSEIF (x .LE. 2.55309d0) THEN
 			t = 1.d0 + ASIN((x-2.55309d0)/0.5d0)/0.78365d0
 			area = 0.8d0 + 0.5d0*(1.d0 - COS((t-1.d0)*0.78365d0))
 			darea = (SIN((t-1.d0)*0.78365d0))/SQRT(1-((x-2.55309d0)/0.5d0)**2)
 		ELSEIF (x .LE. 2.68250d0) THEN
 			t = ASIN((x-2.55309d0)/0.5d0)/0.2617994d0
 			area = 0.8d0 + 0.5d0*(1.d0 - COS(0.2617994d0*t))
 			darea = (SIN(0.2617994d0*t))/SQRT(1-((x-2.55309d0)/0.5d0)**2)
 		ELSE
 			t = (x - 2.68250d0)/(4.5d0 - 2.68250d0)
 			area = 0.817037d0 + 0.26795d0*1.8175d0*t 
 			darea = 0.26795d0*1.8175d0/(4.5d0 - 2.68250d0)
 		ENDIF

 		IF (num == 2) THEN
 			area = darea 
 		ENDIF 
 	END FUNCTION area