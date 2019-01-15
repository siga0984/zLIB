/* ======================================================

Funcao de Comparação zCompare()

Comparação de tipo e conteúdo exatamente igual
Compara também arrays.

Retorno :

0 = Conteúdos idênticos
-1 = Tipos diferentes
-2 = Conteúdo diferente
-3 = Numero de elementos (Array) diferente

====================================================== */

STATIC Function zCompare(xValue1,xValue2)
Local cType1 := valtype(xValue1)
Local nI, nT, nRet := 0

If cType1 == valtype(xValue2)
	If cType1 = 'A'
		// Comparação de Arrays 
		nT := len(xValue1)
		If nT <> len(xValue2)
			// Tamanho do array diferente
			nRet := -3
		Else
			// Compara os elementos
			For nI := 1 to nT
				nRet := zCompare(xValue1[nI],xValue2[nI])
				If nRet < 0
					// Achou uma diferença, retorna 
					EXIT
				Endif
			Next
		Endif
	Else
		If !( xValue1 == xValue2 )
			// Conteudo diferente
			nRet := -2
		Endif
	Endif
Else
	// Tipos diferentes
	nRet := -1
Endif

Return nRet
