#include 'protheus.ch'

/* ======================================================================================
Classe       ZISAMFILE
Autor        Julio Wittwer
Data         01/2019
Descrição    A Classe ZISAMFILE serve de base para implementação de tabeas ISAM 
             através de herança. Atualmente é herdada pelas classes ZDBFFILE e ZMEMFILE\
             
Ela serve para unificar os métodos comuns de processamento e lógica de acesso a 
registros em tabela ISAM 
             
====================================================================================== */

CLASS ZISAMFILE FROM LONGNAMECLASS

  DATA nLastError			// Ultimo erro ocorrido 
  DATA cLastError			// Descrição do último erro 
  DATA lVerbose             // Modo Verbose (echo em console ligado)
  DATA bFilter              // Codeblock de filtro 
  DATA nIndexOrd            // Ordem de indice atual 
  DATA aIndexes             // Array com objetos de indice 
  DATA oCurrentIndex        // Objeto do indice atual 
  DATA nLastRec				// Ultimo registro do arquivo - Total de registros
  DATA aStruct		   		// Array com a estrutura do DBF 
  DATA nFldCount			// Quantidade de campos do arquivo 
  DATA lBOF					// Flag de inicio de arquivo 
  DATA lEOF					// Flag de final de arquivo 
  DATA lOpened              // Indica se o arquivo está aberto 
  DATA lCanWrite            // Arquivo aberto para gravacao 

  METHOD GoTo(nRec)		    // Posiciona em um registro informado. 
  METHOD GoTop()			// Posiciona no RECNO 1 da tabela 
  METHOD GoBottom()   	    // Posiciona em LASTREC da tabela 
  METHOD Skip()             // Navegação de registros ISAM 
  METHOD SetFilter()        // Permite setar um filtro para os dados 
  METHOD ClearFilter()      // Limpa o filtro 
  METHOD BOF()				// Retorna .T. caso tenha se tentado navegar antes do primeiro registro 
  METHOD EOF()				// Retorna .T, caso o final de arquivo tenha sido atingido 
  METHOD Lastrec()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD RecCount()			// Retorna o total de registros / numero do ultimo registro da tabela 
  METHOD GetStruct()		// Retorna CLONE da estrutura de dados da tabela 
  METHOD FCount()           // Retorna o numero de campo / colunas da tabela
  METHOD FieldName( nPos )	// Recupera o nome da coluna informada 
  METHOD FieldPos( cField ) // Retorna a posicao de um campo na estrutura da tabela ( ID da Coluna )

  METHOD SetOrder()         // Seta um indice / ordem ativa 
  METHOD IndexOrd()         // Retorna a ordem ativa
  METHOD IndexKey()         // Retorna a expressao de indice ativa 
  METHOD IndexValue()       // Retorna o valor da chave de indice do registro atual 
  METHOD Seek(cKeyExpr)     // Realiza uma busca usando o indice ativo 
  METHOD CreateIndex()      // Cria um Indice ( em memoria ) para a tabela 
  METHOD ClearIndex()       // Fecha todos os indices

  METHOD CreateFrom()       // Cria tabela a partir da estrutura do objeto inforado 
  METHOD AppendFrom()       // Apenda dados do objeto infdormado na tabela atual 
  METHOD Export()           // Exporta o arquivo para um outro formato

  METHOD GetError() 		// Retorna o Codigo e Descricao por referencia do ultimo erro 
  METHOD GetErrorCode()     // Retorna apenas oCodigo do ultimo erro ocorrido
  METHOD GetErrorStr()		// Retorna apenas a descrição do último erro ocorrido

  METHOD SetVerbose()       // Liga ou desliga o modo "verbose" da classe
  METHOD IsVerbose()        // Consulta ao modo verbose


  // ========================= Metodos de uso interno da classe

  METHOD _ResetError()		// Limpa a ultima ocorrencia de erro 
  METHOD _SetError()        // Seta uma nova ocorrencia de erro 
  METHOD _InitVars() 		// Inicializa propriedades  

  METHOD _CheckFilter()     // Verifica se o registro atual está contemplado no filtro 
  METHOD _SkipNext()		// Le o proximo registro da tabela 
  METHOD _SkipPrev()        // Le o registro anterior da tabela 
  METHOD _ClearRecord()     // Limpa o conteudo do registro em memoria 
  METHOD _BuildFieldBlock(cFieldExpr) // Cria codeblock com expressao de campos 
 
ENDCLASS


// ----------------------------------------
// Retorna .T. caso a ultima movimentação de registro 
// tentou ir antes do primeiro registro 
METHOD BOF() CLASS ZISAMFILE 
Return ::lBOF

// ----------------------------------------\
// Retorna .T. caso a tabela esteja em EOF
METHOD EOF() CLASS ZISAMFILE
Return ::lEOF

// ----------------------------------------------------------
// Posiciona diretamente em um regsitro 

METHOD GoTo(nRec)  CLASS ZISAMFILE

// Verifica se o registro é válido 
// Se não for, vai para EOF

If nRec > ::nLastRec .or. nRec < 1
	::lEOF := .T.
	::_ClearRecord()
	Return
Endif

// ----------------------------------------
// Atualiza o numero do registro atual 
::nRecno := nRec

If ::nIndexOrd > 0 
	// Eu tenho indice ativo, sincroniza a posicao do indice 
	// com a posicao do registro atual 
	::oCurrentIndex:SetResync()
Endif

// Traz o registro atual para a memória
::_ReadRecord()

Return

// ----------------------------------------------------------
// Movimenta a tabela para o primeiro registro 
// Release 20190105 : Contempla uso de indice

METHOD GoTop() CLASS ZISAMFILE 

IF ::nLastRec == 0 
	// Nao há registros 
	::lBOF := .T. 
	::lEOF := .T. 
	::nRecno   := 0
	::_ClearRecord()
	Return
Endif

If ::nIndexOrd > 0 
	// Se tem indice ativo, pergunta pro indice
	// quanl é o primeiro registro da ordem 
	::nRecno := ::oCurrentIndex:GetFirstRec()
Else
	// Ordem fisica 
	// Atualiza para o primeiro registtro 
	::nRecno     := 1
Endif

// Traz o registro atual para a memória
::_ReadRecord()

If ( !::_CheckFilter() )
	// Nao passou na verificacao do filtro
	// busca o proximo registro que atenda
	::_SkipNext()
Endif

Return

// ----------------------------------------------------------
// Movimenta a tabela para o último registro

METHOD GoBottom() CLASS ZISAMFILE 

IF ::nLastRec == 0 
	// Nao há registros 
	::lBOF := .T. 
	::lEOF := .T. 
	::nRecno   := 0
	::_ClearRecord()
	Return
Endif

If ::nIndexOrd > 0 
	// Se tem indice ativo, pergunta pro indice
	// quanl é o primeiro registro da ordem 
	::nRecno := ::oCurrentIndex:GetLastRec()
Else
	// Ordem fisica 
	// Atualiza o RECNO para o ultimo registro 
	::nRecno     := ::nLastRec
Endif

// Traz o registro atual para a memória
::_ReadRecord()

If ( !::_CheckFilter() )
	// Nao passou na verificacao do filtro
	// busca nos registros anteriores o primeiro que atende
	::_SkipPrev()
Endif

Return

// ----------------------------------------------------------
// Avança ou retrocede o ponteiro de registro 
// No caso de DBSkip(0), apenas faz refresh do registro atual   
// Default = 1 ( Próximo Registro ) 

METHOD Skip( nQtd ) CLASS ZISAMFILE
Local lForward := .T. 

If nQtd  == NIL
	nQtd := 1
ElseIF nQtd < 0 
	lForward := .F. 
Endif

// Quantidade de registros para mover o ponteiro
// Se for negativa, remove o sinal 
nQtd := abs(nQtd)

While nQtd > 0 
	If lForward
		IF ::_SkipNext()
			nQtd--
		Else
			// Bateu EOF()
			::_ClearRecord()
			Return
		Endif
	Else
		IF ::_SkipPrev()
			nQtd--
		Else
			// Bateu BOF()
			Return
		Endif
	Endif
Enddo

// Traz o registro atual para a memória
::_ReadRecord()

Return


// ----------------------------------------------------------
// Permite setar um filtro para a navegação de dados 
// Todos os campos devem estar em letras maiusculas 

METHOD SetFilter( cFilter ) CLASS ZISAMFILE
Local cFilterBlk

// retorna string com codebloc para expressao de campos 
cFilterBlk := ::_BuildFieldBlock(cFilter)

// Monta efetivamente o codeblock 
::bFilter := &(cFilterBlk)

Return .T. 

// ----------------------------------------------------------
// Limpa a expressao de filtro atual 

METHOD ClearFilter() CLASS ZISAMFILE
::bFilter := NIL
Return


// ----------------------------------------------------------
// Retorna o numero do ultimo registro da tabela 

METHOD Lastrec() CLASS ZISAMFILE
Return ::nLastRec

// ----------------------------------------------------------
// Colocado apenas por compatibilidade 
// 

METHOD Reccount() CLASS ZISAMFILE
Return ::nLastRec

// ----------------------------------------------------------
// Retorna um clone do Array da estrutura da tabela 

METHOD GetStruct() CLASS ZISAMFILE
Return aClone( ::aStruct )

// ----------------------------------------------------------
// Retorna o numero de campo / colunas da tabela
METHOD FCount()  CLASS ZISAMFILE
Return ::nFldCount

// ----------------------------------------------------------
// Recupera o nome de um campo da tabela 
// a partir da posicao do campo na estrutura

METHOD FieldName(nPos) CLASS ZISAMFILE
If nPos > 0 .and. nPos <= ::nFldCount 
	Return ::aStruct[nPos][1]
Endif
Return NIL

// ----------------------------------------------------------
// Recupera o numero do campo na estrutura da tabela 
// a partir do nome do campo 

METHOD FieldPos( cField ) CLASS ZISAMFILE
Return ASCAN( ::aStruct , {|x| x[1] = cField })

// ----------------------------------------
// Permite trocar a ordedm atual usando 
// um indice aberto 

METHOD SetOrder(nOrd) CLASS ZISAMFILE
If nOrd < 0 .OR.  nOrd > len( ::aIndexes )
	UserException("DbSetOrder - Invalid Order "+cValToChar(nOrd))
Endif
::nIndexOrd := nOrd
If ::nIndexOrd > 0 
	::oCurrentIndex := ::aIndexes[::nIndexOrd]
	::oCurrentIndex:SetResync()
Else
	::oCurrentIndex := NIL
Endif
Return

// ----------------------------------------
// Retorna o numero da ordem do indce ativo 

METHOD IndexOrd() CLASS ZISAMFILE
Return ::nIndexOrd

// ----------------------------------------
// Retorna a expressao da chave de indice atual 
// Caso nao haja indice ativo, retorna ""

METHOD IndexKey() CLASS ZISAMFILE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexExpr()
Endif
Return ""

// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD IndexValue() CLASS ZISAMFILE
IF ::nIndexOrd > 0 
	Return ::oCurrentIndex:GetIndexValue()
Endif
Return NIL


// ----------------------------------------
// Retorna o numero da ordem do indce ativo 
METHOD Seek(cKeyExpr) CLASS ZISAMFILE
Local nRecFound := 0

IF ::nIndexOrd <= 0
	UserException("DBSeek Failed - No active Index")
Endif

nRecFound := ::oCurrentIndex:IndexSeek(cKeyExpr)

If nRecFound > 0
	// NAo precisa resincronizar o indice
	// Eu já fiz a busca pelo indice
	::nRecno := nRecFound
	::_ReadRecord()
	Return .T.
Endif

// Nao achou nada, vai para EOF 
::lEOF := .T.
::_ClearRecord()

Return .F.
	
  
// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Cria uma instancia de um indice em memoria 
// Acrescenta na lista de indices 
// Torna o indice ativo e posiciona no primeiro 
// registro da nova ordem 

METHOD CreateIndex(cIndexExpr) CLASS ZISAMFILE
Local oMemIndex
Local nLastIndex

// Cria o objeto do indice passando a instancia
// do arquivo DBF atual 
oMemIndex := ZMEMINDEX():New(self)

// Cria o indice com a expressao informada
oMemIndex:CreateIndex(cIndexExpr) 

// Acrescenta o indice criado na tabela 
aadd(::aIndexes,oMemIndex)

// E torna este indice atual 
nLastIndex := len(::aIndexes)
::SetOrder( nLastIndex ) 

// Posiciona no primeiro registro da nova ordem 
::GoTop()

Return

// ----------------------------------------
// Fecha todos os indices

METHOD ClearIndex()  CLASS ZISAMFILE
Local nI

For nI := 1 to len(::aIndexes)
	::oCurrentIndex := ::aIndexes[nI]
	::oCurrentIndex:Close()
	FreeObj(::oCurrentIndex)
Next

Return

// ----------------------------------------------------------
// Cria um arquivo de dados na instancia atual usando a estrutura 
// do objeto de arquivo de dados informado como parametro 
// Pode ser infomado um Alias / WorkArea
// Caso lAppend seja .T., a tabela é aberta em modo exclusivo e para gravação 
// e os dados são importados

METHOD CreateFrom( _oDBF , lAppend  ) CLASS ZISAMFILE
Local lFromAlias := .F. 
Local cAlias := ""
Local aStruct := {}

If lAppend = NIL ; lAppend := .F. ; Endif

If valtype(_oDBF) == 'C'

	// Se a origem é caractere, só pode ser um ALIAS 
	lFromAlias := .T. 
	cAlias := alltrim(upper(_oDBF))
	If Select(cAlias) < 1 
		UserException("Alias does not exist - "+cAlias)
	Endif

	aStruct := (cAlias)->(DbStruct())
	
Else

	aStruct := _oDBF:GetStruct()

Endif

If !::Create(aStruct)
	Return .F.
Endif

IF lAppend

	// Dados serão apendados na criação 
	// Abre para escrita exclusiva 
	
	If !::Open(.T.,.T.)
		Return .F.
	Endif

	// Apenda os dados	
	IF !::AppendFrom(_oDBF)
		Return .F.
	Endif

	// E posiciona no primeiro registro 	
	::GoTop()
	
Endif

Return .T.


// ----------------------------------------------------------
// Apena os dados da tabela informada na atual 
// Origem = _oDBF
// Destino = self

METHOD AppendFrom( _oDBF , lAll, lRest , cFor , cWhile ) CLASS ZISAMFILE
Local aFromTo := {}
Local aFrom := {}
Local nI, nPos, cField
Local lFromAlias := .F. 
Local cAlias := ""

DEFAULT lAll  := .T. 
DEFAULT lRest := .F.
DEFAULT cFor := ''
DEFAULT cWhile := ''
              
// Primeiro, a tabela tem qye estar aberta
IF !::lOpened
	UserException("AppendFrom Failed - Table not opened")
	Return .F.
Endif

IF !::lCanWrite
	UserException("AppendFrom Failed - Table opened for READ ONLY")
	Return .F.
Endif

If valtype(_oDBF) == 'C'

	// Se a origem é caractere, só pode ser um ALIAS 
	lFromAlias := .T. 
	cAlias := alltrim(upper(_oDBF))
	If Select(cAlias) < 1 
		UserException("Alias does not exist - "+cAlias)
	Endif

	aFrom := (cAlias)->(DbStruct())
	
Else

	aFrom := _oDBF:GetStruct()

Endif

// Determina match de campos da origem no destino 
For nI := 1 to len(aFrom)
	cField :=  aFrom[nI][1]
	nPos := ::FieldPos(cField)
	If nPos > 0 
		aadd( aFromTo , { nI , nPos })
	Endif
Next

IF lFromAlias
	
	// Dados de origem a partir de uma WorkArea
	
	If lAll 
		// Se é para importar tudo, pega desde o primeiro registro 
		(cAlias)->(DbGoTop())
	Endif
	
	While !(cAlias)->(EOF())

		// Insere um novo registro na tabela atual
		::Insert()

		// Preenche os campos com os valores da origem
		For nI := 1 to len(aFromTo)
			::FieldPut(  aFromTo[nI][2] , (cAlias)->(FieldGet(aFromTo[nI][1]))  )
		Next

		// Atualiza os valores
		::Update()

		// Vai para o procimo registro
		(cAlias)->(DbSkip())
	Enddo
	
Else
	
	If lAll 
		// Se é para importar tudo, pega desde o primeiro registro 
		_oDBF::GoTop()
	Endif
	
	While !_oDBF:EOF()

		// Insere um novo registro na tabela atual
		::Insert()

		// Preenche os campos com os valores da origem
		For nI := 1 to len(aFromTo)
			::FieldPut(  aFromTo[nI][2] , _oDBF:FieldGet(aFromTo[nI][1])  )
		Next

		// Atualiza os valores
		::Update()

		// Vai para o procimo registro
		_oDBF:Skip()

	Enddo
	
Endif

Return .T. 

// ----------------------------------------------------------
// Exporta o arquivo para um outro formato
// cFormat = Formato a exportar 
//    SDF
//    CSV 
//    JSON
// cFileOut = Arquivo de saída 

METHOD Export( cFormat, cFileOut ) CLASS ZISAMFILE
Local nHOut
Local nPos
Local cBuffer := ''
Local lFirst := .T. 

// Primeiro, a tabela tem qye estar aberta
IF !::lOpened
	UserException("ZISAMFILE:EXPORT() Failed - Table not opened")
	Return .F.
Endif

cFormat := alltrim(Upper(cFormat))

If cFormat == "SDF" 
	
	// Formato SDF
	// Texto sem delimitador , Campos colocados na ordem da estrutura
	// CRLF como separador de linhas 
	// Campo MEMO não é exportado
	
	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(-12,"Output SDF File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif
	
	::GoTop()
	
	While !::Eof()
		
		// Monta uma linha de dados
		cRow := ""
		
		For nPos := 1 TO ::nFldCount
			cTipo := ::aStruct[nPos][2]
			nTam  := ::aStruct[nPos][3]
			nDec  := ::aStruct[nPos][4]
			If cTipo = 'C'
				cRow += ::FieldGet(nPos)
			ElseIf cTipo = 'N'
				cRow += Str(::FieldGet(nPos),nTam,nDec)
			ElseIf cTipo = 'D'
				cRow += DTOS(::FieldGet(nPos))
			ElseIf cTipo = 'L'
				cRow += IIF(::FieldGet(nPos),'T','F')
			Endif
		Next
		
		cRow += CRLF
		cBuffer += cRow
		
		If len(cBuffer) > 32000
			// A cada 32 mil bytes grava em disco
			fWrite(nHOut,cBuffer)
			cBuffer := ''
		Endif
		
		::Skip()
		
	Enddo

	// Grava flag de EOF
	cBuffer += Chr(26)

	// Grava resto do buffer que falta 
	fWrite(nHOut,cBuffer)
	cBuffer := ''
	
	fClose(nHOut)
	
ElseIf cFormat == "CSV" 
	
	// Formato CSV
	// Strings entre aspas duplas, campos colocados na ordem da estrutura
	// Virgula como separador de campos, CRLF separador de linhas 
	// Gera o CSV com Header
	// Campo MEMO não é exportado
	
	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(-12,"Output CSV File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif
	
	// Primeira linha é o "header" com o nome dos campos 
	For nPos := 1 TO ::nFldCount
		If nPos > 1 
			cBuffer += ','
		Endif
		cBuffer += '"'+Alltrim(::aStruct[nPos][1])+'"'
	Next
	cBuffer += CRLF

	::GoTop()
	
	While !::Eof()
		
		// Monta uma linha de dados
		cRow := ""
		
		For nPos := 1 TO ::nFldCount
			cTipo := ::aStruct[nPos][2]
			nTam  := ::aStruct[nPos][3]
			nDec  := ::aStruct[nPos][4]
			If nPos > 1
				cRow += ","
			Endif
			If cTipo = 'C'
				// Dobra aspas duplas caso exista dentro do conteudo 
				cRow += '"' + StrTran(rTrim(::FieldGet(nPos)),'"','""') + '"'
			ElseIf cTipo = 'N'
				// Numero trimado 
				cRow += cValToChar(::FieldGet(nPos))
			ElseIf cTipo = 'D'
				// Data em formato AAAAMMDD entre aspas 
				cRow += '"'+Alltrim(DTOS(::FieldGet(nPos)))+'"'
			ElseIf cTipo = 'L'
				// Boooleano true ou false
				cRow += IIF(::FieldGet(nPos),'true','false')
			Endif
		Next
		
		cRow += CRLF
		cBuffer += cRow
		
		If len(cBuffer) > 32000
			// A cada 32 mil bytes grava em disco
			fWrite(nHOut,cBuffer)
			cBuffer := ''
		Endif
		
		::Skip()
		
	Enddo

	// Grava resto do buffer que falta 
	If len(cBuffer) > 0 
		fWrite(nHOut,cBuffer)
		cBuffer := ''
	Endif
	
	fClose(nHOut)
	
ElseIf cFormat == "JSON" 

	// Formato JSON - Exporta estrutura e dados   
	// Objeto com 2 propriedades 
	// header : Array de Arrays, 4 colunas, estrutura da tabela
	// data : Array de Arrays, cada linha é um registro da tabela, 
	// campos na ordem da estrutura
	// -- Campo Memo não é exportado 
	
	/*
	{
	"header": [
		["cCampo", "cTipo", nTam, nDec], ...
	],
	"data": [
	    ["José", 14, true], ...
	]
	}
	*/

	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(-12,"Output JSON File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif


	cBuffer += '{' + CRLF
	cBuffer += '"header": [' + CRLF

	For nPos := 1 to len(::aStruct)
		If nPos = 1 
			cBuffer += "["
		Else
			cBuffer += '],'+CRLF+'['
		Endif
		cBuffer += '"'+Alltrim(::aStruct[nPos][1])+'","'+;
			::aStruct[nPos][2]+'",'+;
			cValToChar(::aStruct[nPos][3])+','+;
			cValToChar(::aStruct[nPos][4])
	Next

	cBuffer += ']'+CRLF
	cBuffer += ']' + CRLF
	cBuffer += ',' + CRLF
	cBuffer += '"data": [' + CRLF
		
	::GoTop()
	
	While !::Eof()
		
		// Monta uma linha de dados
		if lFirst
			cRow := "["
			lFirst := .F. 
		Else
			cRow := "],"+CRLF+"["
		Endif
				
		For nPos := 1 TO ::nFldCount
			cTipo := ::aStruct[nPos][2]
			nTam  := ::aStruct[nPos][3]
			nDec  := ::aStruct[nPos][4]
			If nPos > 1
				cRow += ","
			Endif
			If cTipo = 'C'
				// Usa Escape sequence de conteudo 
				// para astas duplas. -- 
				cRow += '"' + StrTran(rTrim(::FieldGet(nPos)),'"','\"') + '"'
			ElseIf cTipo = 'N'
				// Numero trimado 
				cRow += cValToChar(::FieldGet(nPos))
			ElseIf cTipo = 'D'
				// Data em formato AAAAMMDD como string
				cRow += '"'+Alltrim(DTOS(::FieldGet(nPos)))+'"'
			ElseIf cTipo = 'L'
				// Boooleano = true ou false
				cRow += IIF(::FieldGet(nPos),'true','false')
			Endif
		Next
		
		cBuffer += cRow
		
		If len(cBuffer) > 32000
			// A cada 32 mil bytes grava em disco
			fWrite(nHOut,cBuffer)
			cBuffer := ''
		Endif
		
		::Skip()
		
	Enddo

	// Termina o JSON
	cBuffer += ']' + CRLF
	cBuffer += ']' + CRLF
	cBuffer += '}' + CRLF

	// Grava o final do buffer
	fWrite(nHOut,cBuffer)
	cBuffer := ''
	
	// Fecha o Arquivo 
	fClose(nHOut)
	

Else

	UserException("Formato ["+cFormat+"] não suportado. ")

Endif

Return


// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Verifica se o registro atual está contemplado pelo filtro 
// Release 20190106 -- Contempla filtro de registros deletados

METHOD _CheckFilter() CLASS ZISAMFILE

If ::lSetDeleted .AND. ::lDeleted
	// Filtro de deletados está ligado 
	// e este registro está deletado .. ignora
	Return .F. 
Endif

If ::bFilter != NIL 
	// Existe uma expressao de filtro 
	// Roda a expressão para saber se este registro 
	// deve estar  "Visivel" 
	Return Eval(::bFilter , self )	
Endif

Return .T. 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Le e posiciona no proximo registro, considerando filtro 

METHOD _SkipNext() CLASS ZISAMFILE
Local nNextRecno

While (!::lEOF)

	If ::nIndexOrd > 0 
		// Se tem indice ativo, pergunta pro indice
		// qual é o próximo registro
		nNextRecno := ::oCurrentIndex:GetNextRec()
	Else
		// Estou na ordem fisica
		// Parte do registro atual , soma 1 
		nNextRecno := ::Recno() + 1 
	Endif
	
	// Retornou ZERO ou 
	// Passou do final de arquivo, esquece
	If nNextRecno == 0 .OR. nNextRecno > ::nLastRec
		::lEOF := .T.
		::_ClearRecord()
		Return .F. 
	Endif

	// ----------------------------------------
	// Atualiza o numero do registro atual 
	::nRecno := nNextRecno

	// Traz o registro atual para a memória
	::_ReadRecord()

	// Passou na checagem de filtro ? Tudo certo 
	// Senao , continua lendo ate achar um registro valido 
	If ::_CheckFilter()
		Return .T. 
	Endif

Enddo

Return .F. 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Le e posiciona no registro anmterior, considerando filtro 

METHOD _SkipPrev() CLASS ZISAMFILE
Local nPrevRecno

While (!::lBOF)

	If ::nIndexOrd > 0 
		// Se tem indice ativo, pergunta pro indice
		// qual é o registro anterior
		nPrevRecno := ::oCurrentIndex:GetPrevRec()
	Else
		// Estou na ordem fisica
		// Parte do registro atual , subtrai 1
		nPrevRecno := ::Recno() - 1 
    Endif
    
	// Tentou ler antes do primeiro registro 
	// Bateu em BOF()
	If nPrevRecno < 1 
		::lBOF := .T.
		Return .F. 
	Endif

	// ----------------------------------------
	// Atualiza o numero do registro atual 
	::nRecno := nPrevRecno

	// Traz o registro atual para a memória
	::_ReadRecord()

	// Passou na checagem de filtro ? Tudo certo 
	// Senao , continua lendo ate achar um registro valido 
	If ::_CheckFilter()
		Return .T. 
	Endif

Enddo

// Chegou no topo. 
// Se tem filtro, e o registro nao entra no filtro, localiza 
// o primeir registro válido 
If ( !::_CheckFilter() )
	::GoTop()
	::lBOF := .T. 
Endif

Return .F. 

// ----------------------------------------------------------
// Retorna o código do ultimo erro e a descrição por referencia

METHOD GetError( cRefError ) CLASS ZISAMFILE 
cRefError := ::cLastError
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas o código do ultimo erro 

METHOD GetErrorCode() CLASS ZISAMFILE 
Return ::nLastError

// ----------------------------------------------------------
// Retorna apenas a descrição do ultimo erro 

METHOD GetErrorStr() CLASS ZISAMFILE 
Return ::cLastError

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa o registro do ultimo erro 

METHOD _ResetError() CLASS ZISAMFILE 
::cLastError := ''
::nLastError := 0 
Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Seta uma nova ocorrencia de erro

METHOD _SetError(nCode,cErrorMsg) CLASS ZISAMFILE 
::cLastError := cErrorMsg
::nLastError := nCode
Return


// ----------------------------------------------------------
// Permite setar o modo "verbose" da classe

METHOD SetVerbose( lSet ) CLASS ZISAMFILE 
::lVerbose := lSet
Return

// ----------------------------------------------------------
// Retorna  .T. se o modo verbose está ligado 

METHOD IsVerbose() CLASS ZISAMFILE 
Return ::lVerbose

// ----------------------------------------------------------
// Inicializa as propriedades da classe base

METHOD _InitVars() CLASS ZISAMFILE 

::lOpened       := .F. 
::lCanWrite     := .F. 
::nLastError    := 0
::cLastError    := ''
::lVerbose      := .F. 
::bFilter       := NIL
::lBof          := .F. 
::lEof          := .F. 
::nIndexOrd     := 0
::aIndexes      := {}
::oCurrentIndex := NIL
::nLastRec      := 0
::aStruct       := {}
::nFldCount     := 0

Return


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZISAMFILE
Local nI , cTipo , nTam

// Inicializa com o valor default os campos da estrutura 
For nI := 1 to ::nFldCount
	cTipo := ::aStruct[nI][2]
	nTam  := ::aStruct[nI][3]
	If cTipo == 'C'
		::aGetRecord[nI] := space(nTam)
	ElseIf cTipo == 'N'
		::aGetRecord[nI] := 0
	ElseIf cTipo == 'D'
		::aGetRecord[nI] := ctod('')
	ElseIf cTipo == 'L'
		::aGetRecord[nI] := .F.
	ElseIf cTipo == 'M'
		::aGetRecord[nI] := 0
	Endif
Next

Return

// ----------------------------------------------------------
// Cria uma string para criar codeblock dinamico 
// baseado em expressao usando camposa da tabela
// Os campos devem estar em letras maiúsculas. Cada campo será 
// trocado por o:FieldGet(nPos), o codeblock deve ser usado 
// com Eval() passando como argumento o objeto da tabela 

METHOD _BuildFieldBlock(cFieldExpr) CLASS ZISAMFILE
Local aCampos := {}
Local cBlockStr
Local nI, nPos

// Cria lista de campos
aEval( ::aStruct , {|x| aadd(aCampos , x[1]) } )

// Ordena pelos maiores campos primeiro
aSort( aCampos ,,, {|x,y| alltrim(len(x)) > alltrim(len(y)) } )

// Copia a expressao 
cBlockStr := cFieldExpr

// Troca os campos por o:Fieldget(nCpo)
// Exemplo : CAMPO1 + CAMPO2 será trocado para o:FieldGet(1) + o:FieldGet(2)

For nI := 1 to len(aCampos)
	cCampo := alltrim(aCampos[nI])
	nPos   := ::Fieldpos(cCampo)
	cBlockStr  := StrTran( cBlockStr , cCampo,"o:FieldGet(" +cValToChar(nPos)+ ")")
Next

// Monta a string com o codeblock para indice
cBlockStr := "{|o| "+cBlockStr+"}"

Return cBlockStr

