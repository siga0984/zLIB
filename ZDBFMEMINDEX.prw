#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'

/* ==================================================
Classe      ZDBFMEMINDEX
Autor       Julio Wittwer
Data        05/01/2019
Descri��o   A partir de um objeto ZDBFFILE, permite 
            a cria��o de um �ndice em mem�ria 
================================================== */

CLASS ZDBFMEMINDEX

   PUBLIC DATA oDBF				AS Object		// Objeto ZDBFFILE relacionado ao �ndice 
   PUBLIC DATA cIndexExpr 		AS Character	// Express�o AdvPL original do �ndice
   PUBLIC DATA bIndexBlock		AS CodeBlock	// CodeBlock para montar uma linha de dados do �ndice
   PUBLIC DATA aIndexData 		AS Array		// Array com os dados do �ndice ordenado pela chave 
   PUBLIC DATA aRecnoData 		AS Array		// Array com os dados do �ndice ordenado pelo RECNO 
   PUBLIC DATA nCurrentRow		AS Numeric		// Numero da linha atual do �ndice 
   PUBLIC DATA lSetResync 		AS Logical		// Flag de resincronismo pendente da posi��o do �ndice
   PUBLIC DATA lVerbose			AS Logical

   PUBLIC METHOD NEW()			CONSTRUCTOR			// Cria o objeto do �ndice
   PUBLIC METHOD CREATEINDEX() 		AS Logical		// Cria o �ndice baseado na chave fornecida 
   PUBLIC METHOD CLOSE()							// Fecha o �ndice e limpa os dados da mem�ria 

   PUBLIC METHOD GetFirstRec()		AS Numeric		// Retorna o RECNO do primeiro registro do �ndice
   PUBLIC METHOD GetPrevRec()		AS Numeric		// Retorna o RECNO do Registro anterior do �ndice
   PUBLIC METHOD GetNextRec()		AS Numeric		// Retorna o RECNO do pr�ximo registro do �ndice
   PUBLIC METHOD GetLastRec()		AS Numeric		// Retorna o RECNO do �ltimo registro do �ndice 

   PUBLIC METHOD GetIndexExpr()		AS Character	// Rertorna a express�o de indexa��o 
   PUBLIC METHOD GetIndexValue()					// Retorna o valor da chave de indice do registro atual 
   PUBLIC METHOD GetIndexRecno()	AS Numeric		// REtorna o numero do RECNO da posi��o do �ndice atual 
   PUBLIC METHOD IndexSeek()		AS Numeric		// Realiza uma busca ordenada por um valor informado 
   PUBLIC METHOD RecordSeek()		AS Numeric		// REaliza uma busca no indice pelo RECNO 
   PUBLIC METHOD UpdateKey()						// Atualiza uma chave de indice ( em implementa��o ) 
   
   PUBLIC METHOD SetResync()						// Seta flag de resincronismo pendente
   PUBLIC METHOD SetVerbose()						// Seta modo verbose com echo em console ( em implementa��o 
   
   PROTECTED METHOD _BuildIndexBlock()				// Cria o codeblock da chave de indice 
   PROTECTED METHOD _CheckSync()					// Verifica a necessidade de sincronizar o indice 

ENDCLASS

// ----------------------------------------
// Construtor do indice em memoria
// Recebe o objeto da tabela 

METHOD NEW(oDBF	AS Object) CLASS ZDBFMEMINDEX

	//Inicializa vari�veis-------------------------------------------------------------------------
	::oDBF := oDBF
	::cIndexExpr := ''
	::bIndexBlock := NIL
	::aIndexData := {}
	::aRecnoData := {}
	::nCurrentRow := 0
	::lSetResync := .F. 
	::lVerbose   := .F. 

Return self

// ----------------------------------------
// Chamado pela ZDBFFILE para indicar que o registro ou chave atuais 
// precisam ser resincronizados devido a mudan�a de indice ativo
// ou reposicionamento de registro direto por DBGoto()
METHOD SetResync() CLASS ZDBFMEMINDEX

	::lSetResync := .T.

Return      

// ----------------------------------------
// Permite ligar ou desligar o modo verbose da classe de indice
METHOD SetVerbose( lSet	AS Logical	) CLASS ZDBFMEMINDEX

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lSet		AS Logical

	::lVerbose := lSet

Return


// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se existe sincronismo pendente antes de fazer uma movimentacao 
// Caso tenha, efetua o sincronismo da posicao do indice com a posicao do RECNO 

METHOD _CheckSync() CLASS ZDBFMEMINDEX

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nRecno		AS Numeric

	If ::lSetResync

		If ::oDBF:Eof()
			// Nao posso sincronizar em EOF()
			Return
		Endif

		// Desliga flag de resync
		::lSetResync := .F. 

		// Pega o numero do RECNO atual do DBF 
		nRecno := ::oDBF:Recno()

		IF  ::aIndexData[::nCurrentRow][2] != nRecno
		
			// Se o RECNO da posicao de indice nao est� sincronizado,
			// Busca pela posicao correta do indice de addos ordenados
			// correspondente ao RECNO atual

			::nCurrentRow := ::RecordSeek(nRecno)
			
			If ::nCurrentRow <= 0 
				UserException("*** INDEX RESYNC FAILED - RECNO "+cValToChar(nRecno)+" ***")
			Endif
			
		Endif

	Endif

Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// A partir da express�o de indexa��o fornecida, 
// cria um codeblock para gerar uma linha de dados

METHOD _BuildIndexBlock(cIndexExpr	AS Character) CLASS ZDBFMEMINDEX

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aCampos := {}			AS Array
	Local cTemp					AS Character
	Local nI, nPos				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cIndexExpr		AS Character
	
	// Cria lista de campos
	aEval( ::oDBF:aStruct , {|x| aadd(aCampos , x[1]) } )

	// Ordena pelos maiores campos primeiro
	aSort( aCampos ,,, {|x,y| alltrim(len(x)) > alltrim(len(y)) } )

	// Copia a expressao de �ndice
	cTemp := cIndexExpr

	// Troca os campos por o:Fieldget(nCpo)
	// Exemplo : CAMPO1 + CAMPO2 ser� trocado para o:FieldGet(1) + o:FieldGet(2)

	For nI := 1 to len(aCampos)
		cCampo := alltrim(aCampos[nI])
		nPos   := ::oDBF:Fieldpos(cCampo)
		cTemp  := StrTran( cTemp , cCampo,"o:FieldGet(" +cValToChar(nPos)+ ")")
	Next

	// Monta a string com o codeblock para indice
	cTemp := "{|o| "+cTemp+"}"

	// Monta efetivamente o codeblock de indice
	::bIndexBlock := &(cTemp)

Return

// ----------------------------------------
// Cria um indice na memoria usando a expressao 
// enviada como parametro

METHOD CREATEINDEX( cIndexExpr	AS Character ) CLASS ZDBFMEMINDEX

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cIndexExpr		AS Character

	// Guarda a express�o original do indice
	::cIndexExpr := cIndexExpr

	// Monta o CodeBlock para a montagem da linha de dados
	// com a chave de indice
	::_BuildIndexBlock( cIndexExpr )

	// Agora varre a tabela montando o o set de dados para criar o �ndice
	::aIndexData := {}
	::aRecnoData := {}

	// Coloca a tabela em ordem de regisrtros para a cria��o do indice
	::oDBF:SetOrder(0)
	::oDBF:ClearFilter()
	::oDBF:GoTop()

	While !::oDBF:Eof()
		// Array de dados 
		// [1] Chave do indice
		// [2] RECNO
		// [3] Numero do elemento do array aIndexData que cont�m este RECNO
		aadd( ::aIndexData , { Eval( ::bIndexBlock , ::oDBF ) , ::oDBF:Recno() , NIL } )
		::oDBF:Skip()
	Enddo

	// Sorteia pela chave de indice, usando o RECNO como criterio de desempate
	// Duas chaves iguais, prevalesce a ordem fisica ( o menor recno vem primeiro )
	aSort( ::aIndexData ,,, { |x,y| ( x[1] < y[1] ) .OR. ( x[1] == y[1] .AND. x[2] < y[2] ) } )

	// Guardando a posicao do array ordenado pelos dados na terceira coluna do array 
	aEval( ::aIndexData , {| x,y| x[3] := y })

	// Agora, eu preciso tambem de um indice ordenado por RECNO 
	// Porem fazendo referencia a todos os elementos do array, mudandi apenas a ordena��o 

	// Para fazer esta magica, cria um novo array, referenciando 
	// todos os elementos do array principal , ent�o ordena
	// este array pelo RECNO
	::aRecnoData := Array(len(::aIndexData))
	aEval(::aIndexData , {|x,y| ::aRecnoData[y] := x })
	aSort( ::aRecnoData ,,, { |x,y| x[2] < y[2] } )

Return .T.

// ----------------------------------------
// Retorna o primeiro RECNO da ordem atual 
// Caso nao tenha dados, retorna zero

METHOD GetFirstRec() CLASS ZDBFMEMINDEX

	If Len(::aIndexData) > 0
		::nCurrentRow := 1
		Return ::aIndexData[::nCurrentRow][2]
	Endif

Return 0

// ----------------------------------------
// Retorna o RECNO anterior da ordem atual 
// Caso j� esieta no primeiro registro ou 
// nao tenha dados, retorna zero

METHOD GetPrevRec() CLASS ZDBFMEMINDEX

	If Len(::aIndexData) > 0 .and. ::nCurrentRow > 1
		::_CheckSync()
		::nCurrentRow--
		Return ::aIndexData[::nCurrentRow][2]
	Endif

Return 0

// ----------------------------------------
// Retorna o pr�ximo RECNO da ordem atual 
// Caso nao tenha dados ou tenha chego em EOF 
// retorna zero 

METHOD GetNextRec() CLASS ZDBFMEMINDEX

	If ::nCurrentRow < Len(::aIndexData)
		::_CheckSync()
		::nCurrentRow++
		Return ::aIndexData[::nCurrentRow][2]
	Endif

Return 0 

// ----------------------------------------
// Retorna o numero do ultimo RECNO da ordem atual 
// Caso nao tenha dados retorna zero 

METHOD GetLastRec() CLASS ZDBFMEMINDEX

	If Len(::aIndexData) > 0
		::nCurrentRow := Len(::aIndexData)
		Return ::aIndexData[::nCurrentRow][2]
	Endif

Return 0

// ----------------------------------------
// Retorna a expressao de indice original 
METHOD GetIndexExpr() CLASS ZDBFMEMINDEX
return ::cIndexExpr

// ----------------------------------------
// REtorna o valor da chave de indice do registro atual 
// Que a tabela esta posicionada 
// Em AdvPL, seria o equivalente a &(Indexkey())

METHOD GetIndexValue() CLASS ZDBFMEMINDEX
Return Eval( ::bIndexBlock , ::oDBF )

// ----------------------------------------
// REtorna o numero do RECNO da posi��o de indice atual 

METHOD GetIndexRecno() CLASS ZDBFMEMINDEX
Return ::aIndexData[::nCurrentRow][2]

// ----------------------------------------
// Um registro do dbf foi alterado. 
// Preciso ver se houve altera��o nos valores dos campos chave de indice 
// Caso tenha havido, preciso remover a antiga e inserir a nova 
// na ordem certa. 

METHOD UpdateKey() CLASS ZDBFMEMINDEX

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cKeyDBF			AS Character
	Local cKeyIndex			AS Character
	Local nRecDBF 			AS Numeric
	Local nRecIndex			AS Numeric

	// Valores da chave atual do DBF 
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

	// [TODO] Atualiza��o de campo chave em 
	// inclusao ou altera��o de registro com 
	// o indice aberto 

	conout("")
	conout("*** PENDING ZDBFMEMINDEX::UpdateKey() ***")
	conout("... DBF KeyValue   ["+cValToChar(cKeyDBF)+"]")
	conout("... DBF Recno      ["+cValToChar(nRecDBF)+"]")
	conout("... Index KeyValue ["+cValToChar(cKeyIndex)+"]")
	conout("... Index Recno    ["+cValToChar(nRecIndex)+"]")
	conout("")

	UserException("*** UPDATEKEY() ON ZDBFMEMINDEX NOT AVAILABLE YET ***")

Return

// ----------------------------------------
// Realiza uma busca exata pela chave de indice informada 
// Leva em considera��o chaves repetidas buscando 
// sempre a com menor RECNO 

METHOD IndexSeek(cSeekKey	AS Character) CLASS ZDBFMEMINDEX

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lFound := .F. 				AS Logical
	Local nTop := 1 					AS Numeric
	Local nBottom := Len(::aIndexData)	AS Numeric
	Local nMiddle 						AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cSeekKey		AS Character
	
	If nBottom > 0

		If cSeekKey < ::aIndexData[nTop][1]
			// Chave de busca � menor que a primeira chave do indice
			Return 0
		Endif

		If cSeekKey > ::aIndexData[nBottom][1]
			// Chave de busca � maior que a �ltima chave
			Return 0
		Endif

		While nBottom >= nTop

			// Procura o meio dos dados ordenados
			nMiddle := Int( ( nTop + nBottom ) / 2 )

			If ::aIndexData[nMiddle][1] = cSeekKey
				// Operador de igualdade ao comparar a chave do indice 
				// com a chave informada para Busca. O Advpl opr default 
				// considera que ambas sao iguais mesmo que a chave de busca
				// seja menor, desde que os caracteres iniciais at� o tamanho da 
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
			
			// A posicao encontrada � a pr�xima, onde a
			// chave ainda era igual
			::nCurrentRow := nMiddle+1

			// Retorna o RECNO correspondente a esta chave 
			Return ::aIndexData[::nCurrentRow][2]
			
		Endif
			
	Endif

Return 0


// ----------------------------------------
// Realiza uma busca ordenada pelo RECNO no indice 
// retorna a posicao do array de dados ordenado 
// ( aIndexData ) que aponta para este RECNO

METHOD RecordSeek(nRecno	AS Numeric	) CLASS ZDBFMEMINDEX

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lFound := .F. 				AS Logical
	Local nTop := 1 					AS Numeric
	Local nBottom := Len(::aRecnoData)	AS Numeric
	Local nMiddle 						AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nRecno		AS Numeric

	If nBottom > 0

		If nRecno < ::aRecnoData[nTop][2]
			// Chave de busca � menor que a primeira chave do indice
			Return 0
		Endif

		If nRecno > ::aRecnoData[nBottom][2]
			// Chave de busca � maior que a �ltima chave
			Return 0
		Endif

		While nBottom >= nTop

			// Procura o meio dos dados ordenados
			nMiddle := Int( ( nTop + nBottom ) / 2 )

			If ::aIndexData[nMiddle][2] == nRecno
				// Achou 
				lFound := .T. 
				EXIT
			ElseIf nRecno < ::aRecnoData[nMiddle][2]
				// RECNO menor, desconsidera daqui pra baixo 
				nBottom := nMiddle-1
			ElseIf nRecno > ::aRecnoData[nMiddle][2]
				// RECNO maior, desconsidera daqui pra cima
				nTop := nMiddle+1
			Endif
		
		Enddo

		If lFound
			// Retorna a posi��o do array de dados 
			// ordenados (aIndexData) que contem este RECNO 
			Return ::aRecnoData[nMiddle][3]
		Endif
		
	Endif

Return 0


// ----------------------------------------
// Fecha o indice aberto 
// limpa flags e dados da memoria

METHOD CLOSE() CLASS ZDBFMEMINDEX

	::oDBF := NIL
	::cIndexExpr := ''
	::bIndexBlock := NIL
	::nCurrentRow := 0
	::lSetResync := .F. 

	// Zera os arrays ordenados pela CHAVE de Indice e pelo RECNO 
	aSize( ::aIndexData,0 )
	aSize( ::aRecnoData,0 )

Return
