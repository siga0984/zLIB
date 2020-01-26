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



#include 'protheus.ch'

/* ==================================================

Classe      ZMEMINDEX
Autor       Julio Wittwer
Data        05/01/2019
Descrição   A partir de um objeto ZISAMFILE, permite 
            a criação de um índice em memória 

================================================== */

CLASS ZMEMINDEX FROM LONGNAMECLASS

   DATA oDBF			// Objeto ZISAMFILE relacionado ao índice 
   DATA cIndexExpr      // Expressão AdvPL original do índice
   DATA bIndexBlock     // CodeBlock para montar uma linha de dados do índice
   DATA aIndexData      // Array com os dados do índice ordenado pela chave 
   DATA nCurrentRow     // Numero da linha atual do índice 
   DATA lVerbose        // Modo Verbose (echo em console ligado)

   METHOD NEW(oDBF)     // Cria o objeto do índice
   METHOD CREATEINDEX(cIndexExpr) // Cria o índice baseado na chave fornecida 
   METHOD CLOSE()       // Fecha o índice e limpa os dados da memória 

   METHOD GetFirstRec() // Retorna o RECNO do primeiro registro do índice
   METHOD GetPrevRec()  // Retorna o RECNO do Registro anterior do índice
   METHOD GetNextRec()  // Retorna o RECNO do próximo registro do índice
   METHOD GetLastRec()  // Retorna o RECNO do último registro do índice 
   
   METHOD GetIndexExpr()  // Rertorna a expressão de indexação 
   METHOD GetIndexValue() // Retorna o valor da chave de indice do registro atual 
   METHOD GetIndexRecno() // REtorna o numero do RECNO da posição do índice atual 
   METHOD IndexSeek()     // Realiza uma busca ordenada por um valor informado 
   METHOD RecordSeek()    // REaliza uma busca no indice pelo RECNO 
   METHOD InsertKey()     // Insere uma nova chave no indice ao inserir um registro
   METHOD UpdateKey()     // Atualiza uma chave de indice ( em implementação ) 
   
   METHOD CheckSync()    // Verifica a necessidade de sincronizar o indice 
   METHOD SetVerbose()    // Seta modo verbose com echo em console ( em implementação 
   
ENDCLASS

// ----------------------------------------
// Construtor do indice em memoria
// Recebe o objeto da tabela

METHOD NEW(oDBF) CLASS ZMEMINDEX
::oDBF := oDBF
::cIndexExpr := ''
::bIndexBlock := NIL
::aIndexData := {}
::nCurrentRow := 0
::lVerbose   := .T.
Return self

// ----------------------------------------
// Permite ligar ou desligar o modo verbose da classe de indice
METHOD SetVerbose( lSet ) CLASS ZMEMINDEX
::lVerbose := lSet
Return


// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se existe sincronismo pendente antes de fazer uma movimentacao
// Caso tenha, efetua o sincronismo da posicao do indice com a posicao do RECNO

METHOD CheckSync() CLASS ZMEMINDEX
Local nRecno

If ::oDBF:Eof()
	// Nao posso sincronizar em EOF()
	Return
Endif
	
// Pega o numero do RECNO atual do DBF
nRecno := ::oDBF:Recno()
	
IF ::aIndexData[::nCurrentRow][2] != nRecno
	
	// Se o RECNO da posicao de indice nao está sincronizado,
	// Busca pela posicao correta do indice de addos ordenados
	// correspondente ao RECNO atual
	
	::nCurrentRow := ::RecordSeek(nRecno)
	
	If ::nCurrentRow <= 0
		UserException("*** INDEX RESYNC FAILED - RECNO "+cValToChar(nRecno)+" ***")
	Endif
	
Endif

Return

// ----------------------------------------
// Cria um indice na memoria usando a expressao
// enviada como parametro

METHOD CREATEINDEX( cIndexExpr ) CLASS ZMEMINDEX
Local cIndexBlk

// Guarda a expressão original do indice
::cIndexExpr := cIndexExpr

// Monta o CodeBlock para a montagem da linha de dados
// com a chave de indice
cIndexBlk := ::oDbf:_BuildFieldBlock( cIndexExpr )

// Faz a macro da Expressao
::bIndexBlock := &(cIndexBlk)

// Agora varre a tabela montando o o set de dados para criar o índice
::aIndexData := {}

// Coloca a tabela em ordem de regisrtros para a criação do indice
::oDBF:SetOrder(0)
::oDBF:ClearFilter()
::oDBF:GoTop()

While !::oDBF:Eof()
	// Array de dados
	// [1] Chave do indice
	// [2] RECNO
	aadd( ::aIndexData , { Eval( ::bIndexBlock , ::oDBF ) , ::oDBF:Recno() } )
	::oDBF:Skip()
Enddo

// Sorteia pela chave de indice, usando o RECNO como criterio de desempate
// Duas chaves iguais, prevalesce a ordem fisica ( o menor recno vem primeiro )
aSort( ::aIndexData ,,, { |x,y| ( x[1] < y[1] ) .OR. ( x[1] == y[1] .AND. x[2] < y[2] ) } )

Return .T.

// ----------------------------------------
// Retorna o primeiro RECNO da ordem atual
// Caso nao tenha dados, retorna zero

METHOD GetFirstRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := 1
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o RECNO anterior da ordem atual
// Caso já esieta no primeiro registro ou
// nao tenha dados, retorna zero

METHOD GetPrevRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0 .and. ::nCurrentRow > 1
	::CheckSync()
	::nCurrentRow--
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o próximo RECNO da ordem atual
// Caso nao tenha dados ou tenha chego em EOF
// retorna zero

METHOD GetNextRec() CLASS ZMEMINDEX
If ::nCurrentRow < Len(::aIndexData)
	::CheckSync()
	::nCurrentRow++
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna o numero do ultimo RECNO da ordem atual
// Caso nao tenha dados retorna zero

METHOD GetLastRec() CLASS ZMEMINDEX
If Len(::aIndexData) > 0
	::nCurrentRow := Len(::aIndexData)
	Return ::aIndexData[::nCurrentRow][2]
Endif
Return 0

// ----------------------------------------
// Retorna a expressao de indice original
METHOD GetIndexExpr() CLASS ZMEMINDEX
return ::cIndexExpr

// ----------------------------------------
// REtorna o valor da chave de indice do registro atual
// Que a tabela esta posicionada
// Em AdvPL, seria o equivalente a &(Indexkey())

METHOD GetIndexValue() CLASS ZMEMINDEX
Return Eval( ::bIndexBlock , ::oDBF )

// ----------------------------------------
// REtorna o numero do RECNO da posição de indice atual

METHOD GetIndexRecno() CLASS ZMEMINDEX
Return ::aIndexData[::nCurrentRow][2]


// ----------------------------------------
// Um registro do dbf foi inserido 
// Preciso inserir uma nova chave no indice 
METHOD InsertKey() CLASS ZMEMINDEX
Local cKeyDBF, nRecDBF
Local nTop := 1
Local nBottom := Len(::aIndexData)
Local nPos

// Valores da chave atual do DBF 
cKeyDBF := ::GetIndexValue()
nRecDBF := ::oDBF:Recno()

// Busca a posição correta para inserir a chave
For nPos := nTop to nBottom
	if ( cKeyDBF >= ::aIndexData[nPos][1])
	   LOOP
	Endif
Next

// aIndexData
// [1] Chave de ordenação 
// [2] Numero do recno 

If nPos > nBottom
	// Nova chave acrescentada no final
	aadd(::aIndexData , { cKeyDBF, nRecDBF , nPos } )
Else
  // Nova chave acrescentada na ordem 
	aadd(::aIndexData,NIL)
	aIns(::aIndexData,nPos)
	::aIndexData[nPos] :=  { cKeyDBF, nRecDBF , nPos} 
Endif

// Atualiza  Posicao atual do indice 
::nCurrentRow := nPos

Return

// ----------------------------------------
// Um registro do dbf foi alterado.
// Preciso ver se houve alteração nos valores dos campos chave de indice
// Caso tenha havido, preciso remover a antiga e inserir a nova
// na ordem certa.

METHOD UpdateKey() CLASS ZMEMINDEX
Local cKeyDBF, nRecDBF
Local cKeyIndex , nRecIndex
Local nPos

// Valores da chave atual do registro no DBF
cKeyDBF := ::GetIndexValue()
nRecDBF := ::oDBF:Recno()

// Valores da chave atual do indice
cKeyIndex := ::aIndexData[::nCurrentRow][1]
nRecIndex := ::aIndexData[::nCurrentRow][2]

IF nRecDBF == nRecIndex
	IF cKeyDBF == cKeyIndex
		// Nenhum campo chave alterado
		// Nada para fazer
		Return
	Endif
Endif

// Demove o elemento atual do array de indices
aDel(::aIndexData,::nCurrentRow)

// Acrescenta na ordem certa 
For nPos := 1 to len(::aIndexData)-1
	If cKeyDBF > ::aIndexData[nPos][1]
		LOOP
	Endif
	If cKeyDBF == ::aIndexData[nPos][1]
		IF nRecDBF > ::aIndexData[nPos][2]
			LOOP
		Endif
	Endif
	EXIT
Next

// Insere na posição correta
aIns(::aIndexData,nPos)
::aIndexData[nPos] := { cKeyDBF , nRecDBF }
::nCurrentRow := nPos

Return

// ----------------------------------------
// Realiza uma busca exata pela chave de indice informada

METHOD IndexSeek(cSeekKey) CLASS ZMEMINDEX
Local nTop := 1
Local nBottom := Len(::aIndexData)
Local nMiddle
Local lFound := .F.

If nBottom > 0
	
	If cSeekKey < ::aIndexData[nTop][1]
		// Chave de busca é menor que a primeira chave do indice
		Return 0
	Endif
	
	If cSeekKey > ::aIndexData[nBottom][1]
		// Chave de busca é maior que a última chave
		Return 0
	Endif
	
	While nBottom >= nTop
		
		// Procura o meio dos dados ordenados
		nMiddle := Int( ( nTop + nBottom ) / 2 )
		
		If ::aIndexData[nMiddle][1] = cSeekKey
			// Operador de igualdade ao comparar a chave do indice
			// com a chave informada para Busca. O Advpl opr default
			// considera que ambas sao iguais mesmo que a chave de busca
			// seja menor, desde que os caracteres iniciais até o tamanho da
			// chave de busca sejam iguais.
			lFound := .T.
			EXIT
		ElseIf cSeekKey < ::aIndexData[nMiddle][1]
			// Chave menor, desconsidera daqui pra baixo
			nBottom := nMiddle-1
		ElseIf cSeekKey > ::aIndexData[nMiddle][1]
			// Chave maior, desconsidera daqui pra cima
			nTop := nMiddle+1
		Endif
		
	Enddo
	
	If lFound
		
		// Ao encontrar uma chave, busca pelo menor RECNO
		// entre chaves repetidas, do ponto atual para cima
		// enquanto a chave de busca for a mesma.
		// Compara sempre a chave do indice com a chave de busca
		// com igualdade simples
		
		While ::aIndexData[nMiddle][1] = cSeekKey
			nMiddle--
			If nMiddle == 0
				EXIT
			Endif
		Enddo
		
		// A posicao encontrada é a próxima, onde a
		// chave ainda era igual
		::nCurrentRow := nMiddle+1
		
		// Retorna o RECNO correspondente a esta chave
		Return ::aIndexData[::nCurrentRow][2]
		
	Endif
	
Endif

Return 0

// ----------------------------------------
// Retorna a posicao do array de indice 
// que contem este registro
METHOD RecordSeek(nRecno) CLASS ZMEMINDEX
Return ascan(::aIndexData , {|x| x[2] == nRecno })

// ----------------------------------------
// Fecha o indice aberto
// limpa flags e dados da memoria

METHOD CLOSE() CLASS ZMEMINDEX

::oDBF := NIL
::cIndexExpr := ''
::bIndexBlock := NIL
::nCurrentRow := 0
::lSetResync := .F.

// Zera o array do indice
aSize( ::aIndexData,0 )

Return

