/* -------------------------------------------------------------------------------------------

Copyright 2015-2019 Júlio Wittwer ( siga0984@gmail.com | http://siga0984.wordpress.com/ )

É permitido, gratuitamente, a qualquer pessoa que obtenha uma cópia deste software 
e dos arquivos de documentação associados (o "Software"), para negociar o Software 
sem restrições, incluindo, sem limitação, os direitos de uso, cópia, modificação, fusão,
publicação, distribuição, sublicenciamento e/ou venda de cópias do Software, 
SEM RESTRIÇÕES OU LIMITAÇÕES. 

O SOFTWARE É FORNECIDO "TAL COMO ESTÁ", SEM GARANTIA DE QUALQUER TIPO, EXPRESSA OU IMPLÍCITA,
INCLUINDO MAS NÃO SE LIMITANDO A GARANTIAS DE COMERCIALIZAÇÃO, ADEQUAÇÃO A UMA FINALIDADE
ESPECÍFICA E NÃO INFRACÇÃO. EM NENHUM CASO OS AUTORES OU TITULARES DE DIREITOS AUTORAIS
SERÃO RESPONSÁVEIS POR QUALQUER REIVINDICAÇÃO, DANOS OU OUTRA RESPONSABILIDADE, SEJA 
EM AÇÃO DE CONTRATO OU QUALQUER OUTRA FORMA, PROVENIENTE, FORA OU RELACIONADO AO SOFTWARE. 

                    *** USE A VONTADE, POR SUA CONTA E RISCO ***

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without 
restriction, including without limitation the rights to use, copy, modify, merge, publish, 
distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom 
the Software is furnished to do so, WITHOUT RESTRICTIONS OR LIMITATIONS. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT 
OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE. 

                    ***USE AS YOU WISH , AT YOUR OWN RISK ***

------------------------------------------------------------------------------------------- */



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

