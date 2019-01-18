#include 'Protheus.ch'
#include 'zLibMath.ch'

/* ======================================================

Classe       ZCOMBINE
Autor        Júlio Wittwer
Data         12/2018
Descrição    Classe de combinação de elementos de Array 

-- Conbinação Simples
-- Geração de Combinações ordenadas não repetidas 
-- Mesmo principio de combinação de loterias

====================================================== */

CLASS ZCOMBINE FROM LONGNAMECLASS

  DATA nCols         // Numero de elementos do resultado 
  DATA aElements     // Elementos do array a serem combinados
  DATA nSize         // Tamanho do Array de elementos
  DATA aControl      // Array de controle de conbinações
  DATA aMaxVal       // Array de controle de contadores máximos por coluna
  DATA nPos          // Posição de incremento para obter próximo valor 

  METHOD NEW()       // Construtor, recebe numero de colunas e array a combinar 
  METHOD GETCOMB()   // Retorna um array com os elementos da combinação atual 
  METHOD NEXTCOMB()  // Process a próxima combinação 
  METHOD RUNCOMB()   // Executa um bloco de código para cada combinação processada 
  METHOD GETTOTAL()  // Calcula o numero total de combinações 
 
ENDCLASS


// ------------------------------------------------------
// Construtor 
// Recebe em nCols o numero de elementos por combinação 
// aElements é o array com os elementos a combinar. 

METHOD NEW( nCols , aElements ) CLASS ZCOMBINE
Local nI , nMax

::nCols     := nCols
::aElements := aElements
::nSize     := len(aElements)
::nPos      := nCols
::aControl  := {}
::aMaxVal   := {}

nMax := ::nSize - ::nCols + 1 

For nI := 1 to ::nCols
	aadd(::aControl,nI)
	aadd(::aMaxVal, nMax )
	nMax++
Next

Return self

// ------------------------------------------------------
// GetComb -- Recupera um array de nCols com os 
// elementos combinados. 

METHOD GETCOMB() CLASS ZCOMBINE
Local nI , aRet := array(6)
For nI := 1 to ::nCols
	aRet[nI] := ::aElements[ ::aControl[nI] ]
Next 
Return aRet 


// ------------------------------------------------------
// Geta a proxima combinação . Quando todos os elementos
// já foram combinados, ela retorna .F. 

METHOD NEXTCOMB() CLASS ZCOMBINE
While .T.
	::aControl[::nPos]++
	If ::aControl[::nPos] > ::aMaxVal[::nPos]
		::nPos--
		If ::nPos < 1
			Return .F.
		Endif
		LOOP
	Endif
	EXIT
Enddo
While ::nPos < ::nCols
	::nPos++
	::aControl[::nPos] := ::aControl[::nPos-1]+1
Enddo
Return .T.


// ------------------------------------------------------
// Cria todas as combinações, e chamada um codeblock 
// informado como parametro o array com cada combinação 

METHOD RUNCOMB(bBLock) CLASS ZCOMBINE
Local nI , aRet := array(::nCols)
Local nResult := 0

While .T.
	nResult++ 
	If bBLock != NIL
		For nI := 1 to ::nCols
			aRet[nI] := ::aElements[ ::aControl[nI] ] 
		Next
		Eval(bBLock,aRet)
	Endif
	While .T.
		::aControl[::nPos]++
		If ::aControl[::nPos] > ::aMaxVal[::nPos]
			::nPos--
			If ::nPos < 1
				Return
			Endif
			LOOP
		Endif
		EXIT
	Enddo
	While ::nPos < ::nCols
		::nPos++
		::aControl[::nPos] := ::aControl[::nPos-1]+1
	Enddo
Enddo
Return nResult

// ------------------------------------------------------
// Calcula o numero total de conbinações únicas
// baseado nos parametros ( numero de colunas resultante 
// e numero de elementos a combinar ) 

METHOD GETTOTAL() CLASS ZCOMBINE
Local nFat1 := zFatorial( ::nSize )
Local nFat2 := zfatorial( ::nCols )
Local nFat3 := zFatorial( ::nSize - ::nCols )
Local nTot := nFat1 / ( nFat2 * nFat3 ) 
Return nTot

