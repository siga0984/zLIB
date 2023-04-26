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

USER Function zCompare(	xValue1	,;	//01 xValue1
						xValue2	);	//02 xValue2
							AS Numeric

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cType1 := valtype(xValue1)		AS Character
	Local nI, nT, nRet := 0					AS Numeric

	//---------------------------------------------------------------------------------------------
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
					nRet := U_zCompare(xValue1[nI],xValue2[nI])
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
