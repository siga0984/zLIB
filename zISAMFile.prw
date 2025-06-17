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


#INCLUDE "FileIO.ch"
#include 'Protheus.ch'
#include "zLibDateTime.ch"
#include "zLibZCompare.ch"
#INCLUDE 'ParmType.ch'

Static nST_CAMPO	:= 1	AS Integer
Static nST_TIPO		:= 2	AS Integer
Static nST_TAMANHO	:= 3	AS Integer
Static nST_DECIMAL	:= 4	AS Integer

/* ======================================================================================
Classe       ZISAMFILE
Autor        Julio Wittwer
Data         01/2019
Descrição    A Classe ZISAMFILE serve de base para implementação de tabeas ISAM 
             através de herança. Atualmente é herdada pelas classes ZDBFFILE e ZMEMFILE\
             
Ela serve para unificar os métodos comuns de processamento e lógica de acesso a 
registros em tabela ISAM 
             
====================================================================================== */

CLASS ZISAMFILE //FROM LONGNAMECLASS

	PROTECTED DATA nError			AS Numeric		// Último código de erro
	PROTECTED DATA cError			AS Character	// Descrição do último erro 
	PROTECTED DATA lVerbose			AS Logical		// Modo Verbose (echo em console ligado)
	PROTECTED DATA bFilter			AS CodeBlock	// Codeblock de filtro 
	PROTECTED DATA nIndexOrd		AS Numeric		// Ordem de indice atual 
	PROTECTED DATA aIndexes			AS Array		// Array com objetos de indice 
	PROTECTED DATA oCurrentIndex	AS Object		// Objeto do indice atual 
	PROTECTED DATA nRecno			AS Numeric		// Número do registro (RECNO) atualmnete posicionado 
	PROTECTED DATA nLastRec			AS Numeric		// Ultimo registro do arquivo - Total de registros
	PROTECTED DATA aStruct			AS Array		// Array com a estrutura do DBF 
	PROTECTED DATA jStruct			AS JSON			// Json com a estrutura do DBF
	PROTECTED DATA nFldCount		AS Logical		// Quantidade de campos do arquivo 
	PROTECTED DATA lBOF				AS Logical		// Flag de inicio de arquivo 
	PROTECTED DATA lEOF				AS Logical		// Flag de final de arquivo 
	PROTECTED DATA lOpened			AS Logical		// Indica se o arquivo está aberto 
	PROTECTED DATA lCanWrite		AS Logical		// Arquivo aberto para gravacao 
	PROTECTED DATA aGetRecord		AS Logical		// Array com todas as colunas do registro atual 
	PROTECTED DATA aPutRecord		AS Logical		// Array com campos para update 
	PROTECTED DATA oISAMLogger		AS Object		// Objeto de log 
	PROTECTED DATA oFileDef			AS Object		// Definição extendida do arquivo 

	PUBLIC METHOD New()								// *** O Construtor nao pode ser chamado diretamente ***
	PUBLIC METHOD GoTo()							// Posiciona em um registro informado. 
	PUBLIC METHOD GoTop()							// Posiciona no RECNO 1 da tabela 
	PUBLIC METHOD GoBottom()						// Posiciona em LASTREC da tabela 
	PUBLIC METHOD Skip()							// Navegação de registros ISAM 
	PUBLIC METHOD SetFilter()		AS Logical		// Permite setar um filtro para os dados 
	PUBLIC METHOD ClearFilter()						// Limpa o filtro 
	PUBLIC METHOD BOF()				AS Logical		// Retorna .T. caso tenha se tentado navegar antes do primeiro registro 
	PUBLIC METHOD EOF()				AS Logical		// Retorna .T, caso o final de arquivo tenha sido atingido 
	PUBLIC METHOD Lastrec()			AS Numeric		// Retorna o total de registros / numero do ultimo registro da tabela 
	PUBLIC METHOD RecCount()		AS Numeric		// Retorna o total de registros / numero do ultimo registro da tabela 
	PUBLIC METHOD GetStruct()						// Retorna CLONE da estrutura de dados da tabela (array ou json)
	PUBLIC METHOD dbStruct()						// Retorna CLONE da estrutura de dados da tabela (array ou json)
	PUBLIC METHOD FCount()			AS Numeric		// Retorna o numero de campo / colunas da tabela
	PUBLIC METHOD FieldName()		AS Character	// Recupera o nome da coluna informada 
	PUBLIC METHOD FieldPos()		AS Numeric		// Retorna a posicao de um campo na estrutura da tabela ( ID da Coluna )
	PUBLIC METHOD FieldType()		AS Character	// Recupera o tipo da coluna informada 

	PUBLIC METHOD SetOrder()						// Seta um indice / ordem ativa 
	PUBLIC METHOD IndexOrd()		AS Numeric		// Retorna a ordem ativa
	PUBLIC METHOD IndexKey()		AS Character	// Retorna a expressao de indice ativa 
	PUBLIC METHOD IndexValue()		// Retorna o valor da chave de indice do registro atual 
	PUBLIC METHOD Seek()			AS Logical		// Realiza uma busca usando o indice ativo 
	PUBLIC METHOD CreateIndex()						// Cria um Indice ( em memoria ) para a tabela 
	PUBLIC METHOD ClearIndex()						// Fecha todos os indices
	PUBLIC METHOD Search()			AS Logical		// Busca um registro que atenda os criterios informados

	PUBLIC METHOD CreateFrom()		AS Logical		// Cria tabela a partir da estrutura do objeto ou alias informado
	PUBLIC METHOD AppendFrom()		AS Logical		// Apenda dados do objeto ou alias informado na tabela atual 
	PUBLIC METHOD Export()			AS Logical		// Exporta o arquivo para um outro formato
	PUBLIC METHOD Import()			AS Logical		// Importa dados de arquivo externo em outro formato ( SDF,CSV,JSON )

	PUBLIC METHOD GetErrorStr()		AS Character	// Retorna apenas a descrição do último erro ocorrido
	PUBLIC METHOD GetErrorCode()	AS Numeric		// Retorna apenas o número do último erro ocorrido

	PUBLIC METHOD SetVerbose()						// Liga ou desliga o modo "verbose" da classe
	PUBLIC METHOD IsVerbose()		AS Logical		// Consulta ao modo verbose

	PUBLIC METHOD SetFileDef()		AS Logical		// Guarda o objeto da definição do arquivo 
	PUBLIC METHOD Destroy()							// Destrutor da classe

	// ========================= Metodos de uso interno da classe

	PROTECTED METHOD _ResetError()					// Limpa a ultima ocorrencia de erro 
	PROTECTED METHOD _SetError()        			// Seta uma nova ocorrencia de erro 
	PROTECTED METHOD _InitVars() 					// Inicializa propriedades  

	PROTECTED METHOD _CheckFilter()     AS Logical	// Verifica se o registro atual está contemplado no filtro 
	PROTECTED METHOD _SkipNext()		AS Logical	// Le o proximo registro da tabela 
	PROTECTED METHOD _SkipPrev()        AS Logical	// Le o registro anterior da tabela 
	PROTECTED METHOD _ClearRecord()     			// Limpa o conteudo do registro em memoria 
	PROTECTED METHOD _BuildFieldBlock()	AS Character// Cria codeblock com expressao de campos 

	PROTECTED METHOD _ExportSDF()       AS Logical	// Exporta dados para arquivo SDF
	PROTECTED METHOD _ExportCSV()       AS Logical	// Exporta dados para arquivo CSV
	PROTECTED METHOD _ExportJSON()      AS Logical	// Exporta dados para arquivo JSON
	PROTECTED METHOD _ExportXML()       AS Logical	// Exporta dados para arquivo XML 

	PROTECTED METHOD _ImportSDF()       AS Logical	// Importa dados de arquivo SDF
	PROTECTED METHOD _ImportCSV()       AS Logical	// Importa dados de arquivo CSV
	PROTECTED METHOD _ImportJSON()      AS Logical	// Importa dados de arquivo JSON
 
ENDCLASS


// ----------------------------------------
METHOD New(oParent	AS Object) CLASS ZISAMFILE

	::oISAMLogger := ZLOGGER():New("ZISAMFILE")
	::oISAMLogger:Write("NEW","IsamFile based on "+GetClassName(oParent))

Return

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

METHOD GoTo(nRec	AS Numeric	)  CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nRec		AS Numeric
	
	::oISAMLogger:Write("GoTo","Record "+cValToChar(nRec))

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

	// Traz o registro atual para a memória
	::_ReadRecord()

Return

// ----------------------------------------------------------
// Movimenta a tabela para o primeiro registro 
// Release 20190105 : Contempla uso de indice

METHOD GoTop() CLASS ZISAMFILE 

	::oISAMLogger:Write("GoToP")

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

	::oISAMLogger:Write("GoBottom")

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

METHOD Skip( nQtd	AS Numeric	) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lForward := .T. 	AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nQtd		AS Numeric		Optional Default 1

	::oISAMLogger:Write("Skip")

	IF nQtd < 0 
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

METHOD SetFilter( cFilter	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cFilterBlk		AS Character

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFilter		AS Character

	::oISAMLogger:Write("SetFilter",cFilter)

	// retorna string com codebloc para expressao de campos 
	cFilterBlk := ::_BuildFieldBlock(cFilter)

	// Monta efetivamente o codeblock 
	::bFilter := &(cFilterBlk)

Return .T. 

// ----------------------------------------------------------
// Limpa a expressao de filtro atual 
METHOD ClearFilter() CLASS ZISAMFILE

	::oISAMLogger:Write("ClearFilter")
	::bFilter := NIL

Return


// ----------------------------------------------------------
// Retorna o numero do ultimo registro da tabela 
METHOD Lastrec() CLASS ZISAMFILE
Return ::nLastRec

// ----------------------------------------------------------
// Colocado apenas por compatibilidade 
METHOD Reccount() CLASS ZISAMFILE
Return ::nLastRec

// ----------------------------------------------------------
// Retorna um clone do Array da estrutura da tabela 
METHOD GetStruct(lJson	AS Logical	) CLASS ZISAMFILE
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local uRet		AS Variant

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lJson		AS Logical		Optional Default .F.

	If lJson
		uRet	:= ::jStruct
	Else
		uRet	:= aClone( ::aStruct )
	EndIf

Return uRet

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| dbStruct                        | Autor | Cirilo Rocha       | Data | 10/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Função análoga a função padrão de mesmo nome (para facilitar o uso)               |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD dbStruct(lJson	AS Logical	) CLASS ZISAMFILE
Return ::GetStruct(lJson)

// ----------------------------------------------------------
// Retorna o numero de campo / colunas da tabela
METHOD FCount()  CLASS ZISAMFILE
Return ::nFldCount

// ----------------------------------------------------------
// Recupera o nome de um campo da tabela 
// a partir da posicao do campo na estrutura
METHOD FieldName(nPos	AS Numeric	) CLASS ZISAMFILE

	If nPos > 0 .and. nPos <= ::nFldCount 
		Return ::aStruct[nPos][nST_CAMPO]
	Endif

Return NIL

// ------------------------------------------------------------------------------------------------
// Recupera o numero do campo na estrutura da tabela a partir do nome do campo 
METHOD FieldPos( cField	AS Character) CLASS ZISAMFILE
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local nPos	:= 0		AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cField		AS Character

	//---------------------------------------------------------------------------------------------
	cField	:= AllTrim(Upper(cField))
	If ::jStruct:hasProperty(cField)
		nPos	:= ::jStruct[cField]['POS']
	EndIf

Return nPos

// ----------------------------------------------------------
// Recupera o tipo do campo na estrutura da tabela 
// a partir da posicao do campo na estrutura

METHOD FieldType(uPos) CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR uPos		AS Character,Numeric
	
	If valtype(uPos) = 'C'
		uPos := ::FieldPos(uPos)
	Endif

	If uPos > 0 .and. uPos <= ::nFldCount 
		Return ::aStruct[uPos][nST_TIPO]
	Endif

Return NIL

// ----------------------------------------
// Permite trocar a ordedm atual usando 
// um indice aberto 
METHOD SetOrder(nOrd	AS Numeric	) CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nOrd		AS Numeric

	::oISAMLogger:Write("SetOrder","Order: "+cValToChar(nOrd))

	If nOrd < 0 .OR.  nOrd > len( ::aIndexes )
		UserException("DbSetOrder - Invalid Order "+cValToChar(nOrd))
	Endif
	::nIndexOrd := nOrd
	If ::nIndexOrd > 0 
		::oCurrentIndex := ::aIndexes[::nIndexOrd]
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
METHOD Seek(cKeyExpr	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nRecFound := 0

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cKeyExpr		AS Character

	::oISAMLogger:Write("Seek","Key: "+cKeyExpr)

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

METHOD CreateIndex(cIndexExpr	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local oMemIndex		AS Object
	Local nLastIndex	AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cIndexExpr		AS Character
	
	::oISAMLogger:Write("CreateIndex","Expression: "+cIndexExpr)

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

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nI		AS Numeric

	If ::oISAMLogger <> NIL
		::oISAMLogger:Write("ClearIndex")
	EndIf

	If ::aIndexes <> NIL
		For nI := 1 to len(::aIndexes)
			::oCurrentIndex := ::aIndexes[nI]
			If ValType(aIndexes[nI]) == 'O'
				::oCurrentIndex:Close()
			EndIf
			FreeObj(::oCurrentIndex)
		Next
	EndIf

Return

// ----------------------------------------------------------
// Cria um arquivo de dados na instancia atual usando a estrutura 
// do objeto de arquivo de dados informado como parametro 
// Pode ser infomado um Alias / WorkArea
// Caso lAppend seja .T., a tabela é aberta em modo exclusivo e para gravação 
// e os dados são importados

METHOD CreateFrom( 	_oDBF ,;
					lAppend	AS Logical  ) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aStruct		:= {}		AS Array
	Local cAlias		:= ""		AS Character
	Local lFromAlias	:= .F. 		AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR _oDBF		AS Character,Object
	ParamType 1		VAR lAppend		AS Logical				Optional Default .F.

	::oISAMLogger:Write("CreateFrom")

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

METHOD AppendFrom( 	_oDBF ,;
					lAll	AS Logical	,;
					lRest	AS Logical	,;
					cFor 	AS Character,;
					cWhile	AS Character ) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aFromTo	:= {}		AS Array
	Local aFrom		:= {}		AS Array
	Local cAlias	:= ""		AS Character
	Local cField				AS Character
	Local nI					AS Numeric
	Local nPos					AS Numeric
	Local lFromAlias := .F. 	AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR _oDBF			AS Character,Object
	ParamType 1		VAR lAll			AS Logical				Optional Default .T.
	ParamType 2		VAR lRest			AS Logical				Optional Default .F.
	ParamType 3		VAR cFor			AS Character			Optional Default ''
	ParamType 4		VAR cWhile			AS Character			Optional Default ''

	::oISAMLogger:Write("AppendFrom")
				
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
			_oDBF:GoTop()
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
//    XML
// cFileOut = Arquivo de saída 

METHOD Export( 	cFormat		AS Character,;
				cFileOut 	AS Character,;
				bBlock 					) CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFormat		AS Character
	ParamType 1		VAR cFileOut	AS Character
	//ParamType 1		VAR bBlock		AS CodeBlock DESUSO!

	::oISAMLogger:Write("Export")

	// Primeiro, a tabela tem qye estar aberta
	IF !::lOpened
		UserException("ZISAMFILE:EXPORT() Failed - Table not opened")
		Return .F.
	Endif

	cFormat := alltrim(Upper(cFormat))

	If cFormat == "SDF" 
		lOk := ::_ExportSDF(cFileOut)	
	ElseIf cFormat == "CSV" 
		lOk := ::_ExportCSV(cFileOut)	
	ElseIf cFormat == "JSON" 
		lOk := ::_ExportJSON(cFileOut)
	ElseIf cFormat == "XML"
		lOk := ::_ExportXML(cFileOut)
	Else
		UserException("Export() ERROR - Formato ["+cFormat+"] não suportado. ")
	Endif

Return lOk


// ----------------------------------------------------------
// Recebe a definicao extendida da tabela 
// Com isso eu já tenho a estrutura 
METHOD SetFileDef(oDef	AS Object	)  CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR oDef		AS Object

	::oISAMLogger:Write("SetFileDef")

	IF ::lOpened
		UserException("SetFileDef Failed - Table already opened")
		Return .F.
	Endif

	// Recebe a definição do arquivo 
	::oFileDef := oDef

Return .T. 

// ----------------------------------------------------------
// Formato SDF
// Texto sem delimitador , Campos colocados na ordem da estrutura
// CRLF como separador de linhas
// Campo MEMO não é exportado
METHOD _ExportSDF( cFileOut	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer := ''			AS Character
	Local cRow					AS Character
	Local cTipo					AS Character
	Local nHOut					AS Numeric
	Local nPos					AS Numeric
	Local nTam,nDec				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileOut		AS Character

	::oISAMLogger:Write("_ExportSDF")

	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(ProcLine(),"Output SDF File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif

	::GoTop()

	While !::Eof()
		
		// Monta uma linha de dados
		cRow := ""
		
		For nPos := 1 TO ::nFldCount
			cTipo := ::aStruct[nPos][nST_TIPO	]
			nTam  := ::aStruct[nPos][nST_TAMANHO]
			nDec  := ::aStruct[nPos][nST_DECIMAL]

			IF cTipo = 'M'
				Loop
			Endif

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

Return

// ----------------------------------------------------------
// Formato CSV
// Strings entre aspas duplas, campos colocados na ordem da estrutura
// Virgula como separador de campos, CRLF separador de linhas 
// Gera o CSV com Header
// Campo MEMO não é exportado

METHOD _ExportCSV( cFileOut	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer := ''				AS Character
	Local cTipo						AS Character
	Local cRow						AS Character
	Local nHOut						AS Numeric
	Local nPos						AS Numeric
	Local nTam,nDec					AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileOut		AS Character

	::oISAMLogger:Write("_ExportCSV")

	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(ProcLine(),"Output CSV File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif

	// Primeira linha é o "header" com o nome dos campos 
	For nPos := 1 TO ::nFldCount
		If ::aStruct[nPos][nST_TIPO] == 'M'
			Loop
		Endif	
		If nPos > 1 
			cBuffer += ','
		Endif
		cBuffer += '"'+Alltrim(::aStruct[nPos][nST_CAMPO])+'"'
	Next
	cBuffer += CRLF

	::GoTop()

	While !::Eof()
		
		// Monta uma linha de dados
		cRow := ""
		
		For nPos := 1 TO ::nFldCount
			cTipo := ::aStruct[nPos][nST_TIPO	]
			nTam  := ::aStruct[nPos][nST_TAMANHO]
			nDec  := ::aStruct[nPos][nST_DECIMAL]

			IF cTipo = 'M'
				Loop
			Endif

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

Return .T. 


// ----------------------------------------------------------
// Formato JSON - Exporta estrutura e dados   
// Objeto com 2 propriedades 
// header : Array de Arrays, 4 colunas, estrutura da tabela
// data : Array de Arrays, cada linha é um registro da tabela, 
// campos na ordem da estrutura
// -- Campo Memo não é exportado 

/* 	{ 	
"header": [
	["cCampo", "cTipo", nTam, nDec], ...
],
"data": [
    ["José", 14, true], ...
] 	}
*/

METHOD _ExportJSON( cFileOut	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer := ''			AS Character
	Local cTipo					AS Character
	Local cRow					AS Character
	Local lFirst := .T.			AS Logical
	Local nHOut					AS Numeric
	Local nPos					AS Numeric
	Local nTam,nDec				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileOut		AS Character

	::oISAMLogger:Write("_ExportJSON")

	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(ProcLine(),"Output JSON File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif

	cBuffer += '{' + CRLF
	cBuffer += '"header": [' + CRLF

	For nPos := 1 to len(::aStruct)
		If ::aStruct[nPos][nST_TIPO] == 'M'
			LOOP
		Endif
		If nPos = 1
			cBuffer += "["
		Else
			cBuffer += '],'+CRLF+'['
		Endif
		cBuffer += 	'"'+Alltrim(::aStruct[nPos][nST_CAMPO])+'","'+;
					::aStruct[nPos][nST_TIPO]+'",'+;
					cValToChar(::aStruct[nPos][nST_TAMANHO])+','+;
					cValToChar(::aStruct[nPos][nST_DECIMAL])
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

			cTipo := ::aStruct[nPos][nST_TIPO	]
			nTam  := ::aStruct[nPos][nST_TAMANHO]
			nDec  := ::aStruct[nPos][nST_DECIMAL]

			IF cTipo = 'M'
				Loop
			Endif

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

Return .T.


// ----------------------------------------------------------
// Formato XML - Exporta estrutura e dados   
// Objeto com 2 array de propriedades : header e data
// Para economizar espaço, as colunas de dados são nomeadas com as tags col1, col2 ... n

METHOD _ExportXML( cFileOut	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer := ''			AS Character
	Local cCampo,cTipo			AS Character
	Local cRow					AS Character
	Local nHOut					AS Numeric
	Local nPos					AS Numeric
	Local nTam,nDec				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileOut		AS Character
	
	::oISAMLogger:Write("_ExportXML")

	nHOut := fCreate(cFileOut)
	If nHOut == -1
		::_SetError(ProcLine(),"Output XML File Create Error - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif

	cBuffer += '<?xml version="1.0" encoding="windows-1252" ?>' + CRLF
	cBuffer += '<table>' + CRLF

	cBuffer += '<header>' + CRLF

	For nPos := 1 to len(::aStruct)

		If ::aStruct[nPos][nST_TIPO] == 'M'
			LOOP
		Endif
					
		cBuffer += '<field>'
		cBuffer += '<name>' +lower(Alltrim(::aStruct[nPos][nST_CAMPO]))+ '</name>'
		cBuffer += '<type>' +::aStruct[nPos][nST_TIPO]+ '</type>'
		cBuffer += '<size>' +cValToChar(::aStruct[nPos][nST_TAMANHO])+ '</size>'
		cBuffer += '<decimal>' +cValToChar(::aStruct[nPos][nST_DECIMAL])+ '</decimal>'
		cBuffer += '</field>' + CRLF 
		
	Next

	cBuffer += '</header>' + CRLF
	cBuffer += '<data>' + CRLF

	::GoTop()

	While !::Eof()
		
		// Monta uma linha de dados
		cRow := '<record id="'+cValToChar(::Recno())+'">'
		
		For nPos := 1 TO ::nFldCount
		
			cCampo := ::aStruct[nPos][nST_CAMPO]
			cTipo  := ::aStruct[nPos][nST_TIPO	]
			nTam   := ::aStruct[nPos][nST_TAMANHO]
			nDec   := ::aStruct[nPos][nST_DECIMAL]

			IF cTipo = 'M'
				Loop
			Endif

			cRow += '<'+lower(alltrim(cCampo))+'>'

			If cTipo = 'C'
				// Usa Escape sequence de conteudo
				// para aspas duplas. --
				cRow += StrTran(rTrim(::FieldGet(nPos)),'"','&quot;')
			ElseIf cTipo = 'N'
				// Numero trimado, com "." ponto deecimal 
				cRow += cValToChar(::FieldGet(nPos))
			ElseIf cTipo = 'D'
				// Data em formato AAAAMMDD 
				cRow += Alltrim(DTOS(::FieldGet(nPos)))
			ElseIf cTipo = 'L'
				// Boooleano = true ou false
				cRow += IIF(::FieldGet(nPos),'true','false')
			Endif

			cRow += '</'+lower(alltrim(cCampo))+'>'

		Next
		
		cRow += '</record>' + CRLF 

		cBuffer += cRow
		
		If len(cBuffer) > 32000
			// A cada 32 mil bytes grava em disco
			fWrite(nHOut,cBuffer)
			cBuffer := ''
		Endif
		
		::Skip()
		
	Enddo

	// Termina o XML
	cBuffer += '</data>' + CRLF
	cBuffer += '</table>' + CRLF

	// Grava o final do buffer
	fWrite(nHOut,cBuffer)
	cBuffer := ''

	// Fecha o Arquivo
	fClose(nHOut)

Return .T.

// --------------------------------------------------------------------
// Importacao de dados de arquivo externo -- Formatos SDF,CDV e JSON   
METHOD Import(	cFileIn	AS Character,;	//01 cFileIn
				cFormat	AS Character);	//02 cFormat
					CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lOk		AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileIn		AS Character
	ParamType 1		VAR cFormat		AS Character

	::oISAMLogger:Write("Import")

	// Primeiro, a tabela tem qye estar aberta
	IF !::lOpened
		UserException("Import Failed - Table not opened")
		Return .F.
	Endif

	IF !::lCanWrite
		UserException("Import Failed - Table opened for READ ONLY")
		Return .F.
	Endif

	// Ajusta formato 
	cFormat := alltrim(Upper(cFormat))

	If cFormat == "SDF"
		lOk := ::_ImportSDF(cFileIn)
	ElseIf 	cFormat == "CSV"
		lOk := ::_ImportCSV(cFileIn)
	ElseIf 	cFormat == "JSON"
		lOk := ::_ImportJSON(cFileIn)
	Else
		UserException("Export() ERROR - Formato ["+cFormat+"] não suportado. ")
	Endif

Return lOk 


// --------------------------------------------------------------------
// Importacao de arquivo SDF 
// A estrutura tem que ser a mesma que o arquivo foi gerado 
// Nao tengo como validar os campos, mas tenho como fazer uma consistencia 
// Baseado no tamanho de cada linha com a estrutura atual da tabela. 

METHOD _ImportSDF(cFileIn	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cOneRow	:= ''		AS Character
	Local cTipo					AS Character
	Local cValue				AS Character
	Local nH ,nFSize			AS Numeric
	Local nRowSize	:= 0		AS Numeric
	Local nRows		:= 0 		AS Numeric
	Local nCheck				AS Numeric
	Local nOffset 				AS Numeric
	Local nTam, nPos			AS Numeric
	Local xValue				AS Variant

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileIn		AS Character

	::oISAMLogger:Write("_ImportSDF")

	// Abre o arquivo SDF para leitura 
	nH := FOpen(cFileIn)

	If nH == -1
		::_SetError(ProcLine(),"_ImportSDF() ERROR - File Open Failed - FERROR "+cValToChar(ferror()) )
		Return .F. 
	Endif

	// Pega tamanho do arquivo no disco 
	nFSize := fSeek(nH,0,FS_END)
	FSeek(nH,0)
			
	// Calcula o tamanho de cada linha baseado na estrutura
	For nPos := 1 TO ::nFldCount
		cTipo := ::aStruct[nPos][nST_TIPO]
		If cTipo = 'M' 
			// Ignora campos MEMO 
			LOOP
		Endif
		nTam  := ::aStruct[nPos][nST_TAMANHO]
		nRowSize += nTam
	Next

	// Cada linha do SDF deve ter o numero de bytes 
	// de acordo com a estrutura da tabela, mais CRLF 

	nRowSize += 2

	// O resto da divisao ( Modulo ) do tamanho do arquivo 
	// pelo tamanho da linha deve ser 1 -- devido 
	// ao ultimo byte (0x1A / Chr)26)) indicando EOF

	nCheck := nFsize % nRowSize

	If nCheck <> 1

		::_SetError(ProcLine(),"_ImportSDF() ERROR - SDF File Size FERROR MISMATCH" )
		FClose(nH)
		Return .F. 

	Endif

	// Calcula quantas linhas tem no arquivo 
	nRows :=  (nFsize-1) / nRowSize

	While nRows > 0 

		// Le uma linha do arquivo 
		fRead(nH,@cOneRow,nRowSize)
		
		// Insere nova linha em branco 
		::Insert()

		// Le os valores de cOneRow
		nOffset := 1
		For nPos := 1 TO ::nFldCount

			cTipo := ::aStruct[nPos][nST_TIPO]
			
			If cTipo = 'M' 
				// Ignora campos MEMO 
				LOOP
			Endif
			
			nTam  := ::aStruct[nPos][nST_TAMANHO]

			cValue	:= substr(cOneRow,nOffset,nTam)
			nOffset += nTam
			
			If cTipo == "C"
				::Fieldput(nPos,cValue)
			ElseIf cTipo == "N"
				xValue := Val(cValue)
				::Fieldput(nPos,xValue)
			ElseIf cTipo == "D"
				xValue := STOD(cValue)
				::Fieldput(nPos,xValue)
			ElseIf cTipo == "L"
				xValue := ( cValue = 'T' )
				::Fieldput(nPos,xValue)
			Endif		

		Next
		
		::Update()

		nRows--

	Enddo

	FClose(nH)

Return


// ----------------------------------------
// Importacao de arquivo CSV
// Calculo o tamanho maximo da linha baseado na estrutura da tabela 
// e passo a ler o arquivo em blocos, parseando o conteúdo lido em memória
// Comparo o Header com os campos da estrutura

METHOD _ImportCSV(cFileIn	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aHeadCpos := {}		AS Array
	Local aFileCpos := {}		AS Array
	Local cBuffer	:= ''		AS Character
	Local cTemp		:= ''		AS Character
	Local cValue				AS Character
	Local cTipo					AS Character
	Local nH , nFSize			As Numeric
	Local nMaxSize	:= 0		AS Numeric
	Local nTam, nPos			AS Numeric
	Local nToRead 				AS Numeric
	Local nLidos , nI			AS Numeric
	Local xValue				AS Variant

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFileIn		AS Character

	::oISAMLogger:Write("_ImportCSV")

	// Abre o arquivo CSV para leitura 
	nH := FOpen(cFileIn)

	If nH == -1
		::_SetError(ProcLine(),"_ImportCSV() ERROR - File Open Failed - FERROR "+cValToChar(ferror()) )
		Return .F. 
	Endif

	// Pega tamanho do arquivo no disco 
	nFSize := fSeek(nH,0,FS_END)
	FSeek(nH,0)
			
	// Calcula o tamanho máximo de uma linha baseado na estrutura da tabela 
	For nPos := 1 TO ::nFldCount
		cCampo  := ::aStruct[nPos][nST_CAMPO]
		cTipo := ::aStruct[nPos][nST_TIPO]
		
		If cTipo = 'M' 
			// Ignora campos MEMO 
			LOOP
		Endif
		nTam  := ::aStruct[nPos][nST_TAMANHO] 
		// Soma 3 ao tamanho de cada coluna
		// DElimitadores + separador 
		nMaxSize += ( nTam + 3 )

		// Monta a lista de campos baseado na estrutura atual 
		aadd(aFileCpos , alltrim(upper(cCampo)) )
	Next

	// Acrescenta um final de linha
	nMaxSize += 2

	// Le a primeira linha - HEader com os campos 
	// Logo de cara lê 512 bytes 

	nLidos := fRead(nH , @cBuffer , 512 )
	nFSize -= nLidos

	// Acha a quebra de linha e remove ela do buffer
	nPos := AT( CRLF , cBuffer )
	cOneRow := left(cBuffer , nPos-1)
	cBuffer := substr(cBuffer,nPos+2)

	// Cria array com os campos considerando a virgula como separador
	aHeader := StrTokArr2(cOneRow,",",.F.)

	For nI := 1 to len(aHeader)
		cField := aHeader[nI]
		NoQuotes(@cField)

		// Monta a lista de campos baseado no header
		aadd(aHeadCpos, Alltrim(upper(cField)) )
	Next

	// Comparação de Arrays usando zCompare()
	// 0 = Conteúdos idênticos
	// < 0 = Diferentes ( -1 tipo , -2 conteudo ou -3 tamanho de array ) 

	If zCompare( aFileCpos , aHeadCpos ) < 0 
		fClose(nH)	
		::_SetError(ProcLine(),"_ImportCSV() ERROR - Header Fields Mismatch." )
		Return .F. 
	Endif

	// Uma linha deste arquivo NUNCA deve chegar em nMaxSize
	// Ele é calculado assumindo que todas as colunas tem delimitador 
	// e um separador, ele soma isso inclusive na ultima coluna 

	While nFSize > 0 .or. !empty(cBuffer)
		
		IF len(cBuffer) < nMaxSize .and. nFSize > 0 
			// SE o buffer em memoria 
			nToRead := MIN ( nMaxSize * 5 , nFSize ) 
			nLidos := fRead(nH , @cTemp , nToRead )
			cTemp := left(cTemp,nLidos)
			nFSize -= nLidos
			cBuffer += cTemp
		Endif	

		// Agora identifica uma linha e faz parser de conteudo 

		nPos := AT( CRLF , cBuffer )
		cOneRow := left(cBuffer , nPos-1)
		cBuffer := substr(cBuffer,nPos+2)
		
		// Insere nova linha em branco 
		::Insert()

		For nPos := 1 to ::nFldCount
		
			cTipo := ::aStruct[nPos][nST_TIPO	]
			nTam  := ::aStruct[nPos][nST_TAMANHO]

			If cTipo = 'M' 
				// Ignora campos MEMO 
				LOOP
			Endif

			// Pega procimo valor de campo e remove da linha 
			cValue := GetNextVal(@cOneRow)
			
			If cTipo == "C"
				// Tipo caractere, coloca valor direto 
				::Fieldput(nPos,cValue)
			ElseIf cTipo == "N"
				// Numérico, converte para numero 
				xValue := Val(cValue)
				::Fieldput(nPos,xValue)
			ElseIf cTipo == "D"
				// Data , string em formato AAAMMDD , converte para Data 
				xValue := STOD(cValue)
				::Fieldput(nPos,xValue)
			ElseIf cTipo == "L"
				// Booleano , pode ser Y, T , 1 ou TRUE
				xValue := Upper(cValue)
				If xValue = 'Y' .or. xValue = 'T' .or. xValue = '1' .or. xValue = 'TRUE'
					::Fieldput(nPos,.T.)
				Endif
			Endif		

		Next
		
		::Update()

	Enddo	

	FClose(nH)

Return

// ----------------------------------------

METHOD _ImportJSON(cFileIn	AS Character) CLASS ZISAMFILE
	
	UserException("ZISAMFILE:_ImportJSON() NOT IMPLEMENTED.")

Return .F. 


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

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nNextRecno		AS Numeric

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

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nPrevRecno		AS Numeric

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
// Retorna apenas a descrição do ultimo erro 

METHOD GetErrorStr() CLASS ZISAMFILE 
Return ::cError

// ----------------------------------------------------------
// Retorna apenas o código do ultimo erro 

METHOD GetErrorCode() CLASS ZISAMFILE 
Return ::nError

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa o registro do ultimo erro 

METHOD _ResetError() CLASS ZISAMFILE 

	::cError	:= ''
	::nError	:= 0

Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Seta uma nova ocorrencia de erro

METHOD _SetError(	nError		AS Numeric	,;
					cErrorMsg	AS Character) CLASS ZISAMFILE 

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nError		AS Numeric		Optional Default -999
	ParamType 1		VAR cErrorMsg	AS Character	Optional Default 'ERRO DESCONHECIDO!'

	::oISAMLogger:Write("_SetError",LTrim(Str(nError))+' '+cErrorMsg)
	::cError 	:= cErrorMsg
	::nError	:= nError

Return

// ----------------------------------------------------------
// Permite setar o modo "verbose" da classe

METHOD SetVerbose( lSet	AS Logical	) CLASS ZISAMFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lSet		AS Logical

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
	::cError        := ''
	::nError        := 0
	::lVerbose      := .T. 
	::bFilter       := NIL
	::lBof          := .F. 
	::lEof          := .F. 
	::nIndexOrd     := 0
	::aIndexes      := {}
	::oCurrentIndex := NIL
	::nLastRec      := 0
	::aStruct       := {}
	::jStruct       := JsonObject():New()
	::nFldCount     := 0
	::aGetRecord    := {}
	::aPutRecord    := {}

Return


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual de leitura
// ( Inicializa todos com os valores DEFAULT ) 
// Limpa campos de gravação / update 
// ( seta NIL nos elementos ) 

METHOD _ClearRecord() CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cTipo				AS Character
	Local nI , nTam			AS Numeric

	::oISAMLogger:Write("_ClearRecord")

	// Inicializa com o valor default os campos da estrutura 
	For nI := 1 to ::nFldCount
		cTipo := ::aStruct[nI][nST_TIPO]
		nTam  := ::aStruct[nI][nST_TAMANHO]
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

	// Zera também registro de granação
	::aPutRecord := Array(::nFldCount)

Return

// ----------------------------------------------------------
// Cria uma string para criar codeblock dinamico 
// baseado em expressao usando campos da tabela
// Os campos devem estar em letras maiúsculas. Cada campo será 
// trocado por o:FieldGet(nPos), o codeblock deve ser usado 
// com Eval() passando como argumento o objeto da tabela 

METHOD _BuildFieldBlock(cFieldExpr	AS Character) CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aCampos := {}		AS Array
	Local cBlockStr			AS Character
	Local nI, nPos			AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFieldExpr	AS Character

	::oISAMLogger:Write("_BuildFieldBlock",cFieldExpr)

	// Cria lista de campos
	aCampos	:= ::jStruct:GetNames()

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

// Remove aspas duplas delimitadoras por referencia
// Retorna por referencia se a string estava 
// delimitada por aspas duplas 
STATIC Function NoQuotes(	cQuotStr	AS Character,;
							lQuoted		AS Logical	)

	lQuoted := left(cQuotStr,1) = '"' .and. right(cQuotStr,1) = '"'
	If lQuoted
		cQuotStr := Substr(cQuotStr,2,len(cQuotStr)-2)	
		cQuotStr := StrTran(cQuotStr,'""','"')
	Endif

Return 

// ------------------------------------------------------------------------------------------------
STATIC Function GetNextVal(cCSVLine	AS Character)	AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cRet		:= ''				AS Character
	Local lQuoted	:= .F.				AS Logical
	Local lInAspas	:= .F.				AS Logical
	Local nI , nT	:= len(cCSVLine)	AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cCSVLine		AS Character

	If left(cCSVLine,1) == '"'
		lQuoted := .T.
	Endif

	For nI := 1 to nT
		cChar := substr(cCSVLine,nI,1)
		If cChar == ','
			IF lInAspas
				cRet += cChar
			Else
				cCSVLine := substr(cCSVLine,nI+1)
				EXIT
			Endif
		ElseIF cChar == '"'
			lInAspas := !lInAspas
			cRet += cChar
		Else
			cRet += cChar
		Endif
	Next

	IF  nI >  nT
		// Saou do loop sem achar o separador  ","
		// Logo, a linha acabou
		cCSVLine := ""
	Endif

	If lQuoted
		// Remove aspas antes e depois
		// Troca escape sequence de aspas [""] por ["]
		NoQuotes(@cRet)
	Endif

Return cRet

// ----------------------------------------------------------
// Busca um registro que atenda os criterios informados
// aRecord recebe os dados a procurar no formato [1] Campo [2][ Conteudo 
// aFound retorna o registro encontrado por referencia ( todos os campos ) 
// no mesmo formato do aRecord, acrescido do RECNO 
// Por padrao a busca é feita por substring 
// Caso seja especificada busca EXATA, os conteudos dos campos 
// informados devem ter correspondencia exata com a base de dados

METHOD Search(	aRecord	AS Array	,;
				aFound	AS Array	,;
				lExact	AS Logical	)  CLASS ZISAMFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aFldPos := {}				AS Array
	Local nCnt						AS Numeric
	Local nI						AS Numeric
	Local nFound := 0				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aRecord		AS Array
	ParamType 1		VAR aFound		AS Array				Optional Default NIL
	ParamType 1		VAR lExact		AS Logical				Optional Default .F.

	::oISAMLogger:Write("Search")

	nCnt 	:= len(aRecord)
	aFound	:= {}

	// Sempre posiciona no topo 

	If nCnt <= 0 
		
		// Sem condições especificadas, pega 
		// o primeiro registro 
		::GoTop()

	Else

		// Mapeia campos informados com a posição 
		// do campo no arquivo 	
		For nI := 1 to nCnt
			aadd( aFldPos , ::fieldpos(aRecord[nI][1]) )
		Next
		
		// Começa a busca sempre no inicio do arquivo 
		::GoTop()

		// FAz busca sequencial	
		While !::Eof()
			nFound := 0 
			For nI := 1 to nCnt 
				IF lExact
					// Busca exata
					IF ::FieldGet(aFldPos[nI]) == aRecord[nI][2]
						nFound++
					Endif
				Else
					// Busca por substring ( ou "like %content%" ) 
					If alltrim(aRecord[nI][2]) $ ::FieldGet(aFldPos[nI])  
						nFound++
					Endif
				Endif
			Next
			If nFound == nCnt
				EXIT
			Endif
			::Skip()
		Enddo
		
	Endif

	If !::Eof()  
		// Nao estou em EOF = achei um registro 
		For nI := 1 to ::nFldCount
			aadd(aFound , {  ::FieldName(nI) , ::FieldGet(nI)  })
		Next
		// Acrescenta o RECNO no campo
		aadd(aFound,{"RECNO",::Recno()})
		Return .T.
	Endif

	::_SetError(ProcLine(), "Nenhum registro foi encontrado baseado nos dados informados" )

Return .F. 

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| Destroy                         | Autor | Cirilo Rocha       | Data | 09/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Método para liberar a memória do objeto                                           |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD Destroy()  CLASS ZISAMFILE 

	//---------------------------------------------------------------------------------------------
	::ClearIndex()
	FwFreeArray(::aIndexes)
	FwFreeArray(::aStruct)
	FreeObj(::jStruct)
	FreeObj(::oISAMLogger)
	FreeObj(::oFileDef)
	FreeObj(::oCurrentIndex)

Return
