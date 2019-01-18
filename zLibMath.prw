
/* =======================================================

Funções Matemáticas 

======================================================= */

STATIC Function zFatorial(nNum)
Local nI := nNum - 1
While nI > 1 
	nNum *= nI 
	nI--
Enddo
Return nNum
                         

