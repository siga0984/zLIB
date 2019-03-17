#include 'protheus.ch'
#include 'zlib.ch'

/*
  #define ZDEF_ONNEWREC    1
  #define ZDEF_ONINSERT    2
  #define ZDEF_ONUPDATE    4
  #define ZDEF_ONDELETE    8
  #define ZDEF_ONSEARCH    16
  #define ZDEF_ONCANCEL    32
  #define ZDEF_ONGETDATA   64
*/


/* ======================================================

		Classe do Modelo Padrao

Model Layer:

This is the data layer which consists of the business logic of the system.
It consists of all the data of the application
It also represents the state of the application.
It consists of classes which have the connection to the database.
The controller connects with model and fetches the data and sends to the view layer.
The model connects with the database as well and stores the data into a database which is connected to it.

O Modelo trabalha com um buffer de registro corrente no array ::aRecord
Isso permite alterações e validações de conteudo sem acessar a camada de dados 

====================================================== */

CLASS ZMVCMODEL FROM LONGNAMECLASS

  DATA oEnv                     // Environment em uso 
  DATA oDBConn                  // Conexao com o banco de dados 
  DATA cTable                   // Nome da tabela deste modelo 
  DATA oObjectDef               // Definição do Componente em uso 
  DATA oObjectTable             // Tabela relacionada ao componente
  DATA lInited                  // Flag indicando que o modelo já foi inicializado 
  DATA cError                   // Registro do ultimo erro 
  DATA aRecord                  // Registro em uso pelo modelo
  DATA oLogger                  // Objeto de log 
    
  METHOD NEW()                  // Construtor do Modelo 
  METHOD INIT()                 // Inicialização do modelo 
  METHOD DONE()                 // Finalizacao do Modelo 
	
  METHOD GetZLIBEnv()                 // Retorna o objeto do Ambiente da execução atual 
  METHOD GetErrorStr()          // Recupera ultima mensagem de erro 
  METHOD SetError()             // Seta mensagem de erro 
  METHOD ClearError()           // Limpa ultima ocorrencia de erro 
  METHOD GetObjectDef()         // Retorna o objeto com a definição do componente
  METHOD GetTableObj()          // Recupera a tabela relacionada ao compoentne
  METHOD RunEvents()            // Roda um ou mais eventos relacionados do compoennte     
  METHOD RunAction()            // Executa uma determinada ação do componente

  METHOD VldConstraints()       // Roda Validação de Constraints dos campos 

  METHOD FieldGet()             // Recupera o valor de um campo do registro atual 
  METHOD FieldPut()             // Seta valor de um campo no registro atual 
 
  METHOD NewRecord()            // Cria registro em branco para inserção 
  METHOD Write()                // Insere um novo registro na base de dados 
  METHOD Search()               // Busca o primeiro registro a atender os criterios 
  METHOD Update()               // Atualiza um registro da tabela 
  METHOD GetData()              // Recupera os dados da tabela 

ENDCLASS

// ----------------------------------------------------------

METHOD NEW(cTable,oDef) CLASS ZMVCMODEL

::cTable := cTable
::oObjectDef := oDef

::lInited := .F. 
::cError := ''
::aRecord := {}

::oLogger := ZLOGGER():New("ZMVCMODEL")
::oLogger:Write("NEW","Model based on definition "+GetClassName(oDef))

Return self

// ----------------------------------------------------------
// Recupera ultima ocorrencia de erro 

METHOD GetErrorStr()  CLASS ZMVCMODEL
Return ::cError
             
// ----------------------------------------------------------
// Seta ocorrencia de erro 

METHOD SetError(cError)  CLASS ZMVCMODEL
::cError := cError
::oLogger:Write("SetError","Error="+cError)
Return

// ----------------------------------------------------------
// Limpa ultima ocorrencia de erro 

METHOD ClearError()  CLASS ZMVCMODEL
::cError := ''
Return

// ----------------------------------------------------------
// Inicializa o modelo 

METHOD INIT(oEnv) CLASS ZMVCMODEL
Local lOk 

::oLogger:Write("INIT")

::ClearError()

// O Init do modelo recebe o environment 
::oEnv   := oEnv

If ::lInited
	::SetError( 'MODEL ALREADY INITIALYZED' )
	Return .F. 
Endif

::oDBConn := ::oEnv:GetObject("DBCONN")

If ::oDBConn = NIL 
	::SetError( '*** DATABASE DEFAULT CONNECTION NOT FOUND ***' )
	Return .F. 
Endif

// Verifica a estrutura da tabela de dados  
lOk := ::oDBConn:Connect()

If !lOk   
	::SetError( ::oDBConn:GetErrorStr() )
	Return .F. 
Endif

// Recupera o objeto da tabela 
::GetTableObj()

// Verifica se a tabela existe. Se nao existir, cria

If !::oObjectTable:Exists()

	// Cria a tabela baseado na estrutura da definicao
	lOk := ::oObjectTable:Create( ::oObjectDef:GetStruct() )

Else

	// Verifica a estutura fisica da tabela versus
	// a estrutura da definição da tabela 
	lOk := ::oObjectTable:UpdStruct(::oObjectDef)

Endif

If !lOk
	::SetError( ::oObjectTable:GetErrorStr() )
	Return .F. 
Endif

// Desconecta 
::oDBConn:Disconnect()

// Flag de objeto inicializado 
::lInited := .T. 

Return .T. 

// ----------------------------------------------------------
// Retorna o objeto do ambiente do processo atual 

METHOD GetZLIBEnv() CLASS ZMVCMODEL
::oLogger:Write("GetZLIBEnv")
Return ::oEnv

// ----------------------------------------------------------
// Finaliza o contexto de execução do modelo 

METHOD DONE() CLASS ZMVCMODEL

::oLogger:Write("Done")

// Fecha e limpa a tabela 
If ::oObjectTable != NIL
	::oObjectTable:Close()
	freeObj(::oObjectTable)
Endif

Return


// ----------------------------------------------------------
// Retorna a definicao do componente

METHOD GetObjectDef() CLASS ZMVCMODEL
Return ::oObjectDef

// ----------------------------------------------------------
// Retorna o objeto da tabela relacionado ao modelo 
// --- Criado sob demanda ---

METHOD GetTableObj() CLASS ZMVCMODEL
If ::oObjectTable = NIL
	// Peço pro driver montar meu objeto de acesso a tabela 
	::oObjectTable := ::oDBConn:GetTable(::cTable,::oObjectDef)
	::oLogger:Write("GetTableObj","Get Table "+::cTable)
Endif
Return ::oObjectTable


// ----------------------------------------------------------
// Quando um modelo chama um evento, 
// ele precisa passar-se como parametro 

METHOD RunEvents(nEvent) CLASS ZMVCMODEL
::oLogger:Write("RunEvents","Event="+cValToChar(nEvent))
Return ::oObjectDef:RunEvents(nEvent,self)


// ----------------------------------------------------------
// Quando um modelo executa uma ação 
// ele precisa passar-se como parametro 
// Passa o registro em foco também como parametro

METHOD RunAction(cAction) CLASS ZMVCMODEL
::oLogger:Write("RunAction","Action="+cAction)
Return ::oObjectDef:RunAction(cAction,self)

// ----------------------------------------------------------
// Cria registro em branco para inserção 
// Parte da definição dos campos

METHOD NEWRECORD( aRecord )  CLASS ZMVCMODEL
Local aFieldsDef
Local nFldCount
Local oFldDef
Local nI

::oLogger:Write("NewRecord")

// Limpa o registro atual 
aSize(::aRecord,0)
aSize(aRecord,0)

aFieldsDef := ::oObjectDef:GetFields()
nFldCount  := len( aFieldsDef ) 

For nI := 1 to nFldCount
	oFldDef := aFieldsDef[nI]
	aadd(::aRecord,{ oFldDef:GetField() , oFldDef:DefaultValue() } )
Next

// Roda os eventos apos preencher os valores dos campos 
// Os valores de ::aRecord podem ser alterados
IF ::RunEvents( ZDEF_ONNEWREC )
    aEval( ::aRecord , {|x| aadd(aRecord,x) })
	Return .T. 
Endif

Return .F. 


// ----------------------------------------------------------
// Insere um novo registro na base 

METHOD Write(aRecord) CLASS ZMVCMODEL
Local nFld  , nTot  , lOk

::ClearError()
::oLogger:Write("Write")

lOk := ::oDBConn:Connect()

IF !lOk   
	::SetError( ::oDBConn:GetErrorStr() )
   Return .F. 
Endif

::GetTableObj()

// Shared, Write 
lOk := ::oObjectTable:Open(.F.,.T.)

If !lOk
  ::SetError( ::oObjectTable:GetErrorStr() )
   Return .F. 
Endif

// Antes de rodar os eventos, atualiza o 
// array de dados do registro da classe

::aRecord := aClone(aRecord)

// Roda os eventos apos preencher os valores dos campos   
// Podem haver validações -- se algum evento 
// retornar .F. a inserção não deve ser feita 
// Os campos da tabela sao acessados usando  ::aRecord
lOk := ::RunEvents( ZDEF_ONINSERT )

IF lOk
	
	// Passou dos eventos
	// Realiza as validações das constraints dos campos 
	// -- Campo de preenchimento obrigatório 

	lOk := ::VldConstraints()

Endif

If lOk

	// VERBOSE
	conout(padc(' ZMVCMODEL:WRITE() ',79,'-'))	
	aeval( ::aRecord , {|x|  conout(x[1]+" = "+cValToChar( x[2] )  ) })
	conout(replicate('-',79)+CRLF)

	// Inicia a inserção
	::oObjectTable:Insert()

	// Preenche os campos 
	nTot := len(::aRecord)
	For nFld := 1 to nTot
		::oObjectTable:FieldPut(::aRecord[nFld][1],::aRecord[nFld][2])
	Next

	// Efetiva a inserção 
	lOk := ::oObjectTable:Update()

	If !lOk
	   ::SetError( ::oObjectTable:GetErrorStr() )
	Endif

Endif

// Fecha a tabela 
::oObjectTable:Close()

// e Desconecta do banco 
::oDBConn:Disconnect()

Return lOk


// ----------------------------------------------------------
// Recupera o valor de um campo do registro atual 
// O registro corrente é mantido em memoria em um array 
// no formato [1] campo [2] conteudo 

METHOD FieldGet(xPos) CLASS ZMVCMODEL

If valtype(xPos) = 'C'
	xPos := Ascan(::aRecord , {|x| x[1] == xPos })
Endif

If xPos > 0 .and. xPos <= Len(::aRecord)
	Return ::aRecord[xPos][2]
Endif

Return NIL

// ----------------------------------------------------------
// Seta o valor de um campo do registro atual em memoria
// O registro corrente é mantido em memoria em um array 
// no formato [1] campo [2] conteudo 

METHOD FieldPut(xPos,xValue) CLASS ZMVCMODEL

If valtype(xPos) = 'C'
	xPos := Ascan(::aRecord , {|x| x[1] == xPos })
Endif

If xPos > 0 .and. xPos <= Len(::aRecord)
	::aRecord[xPos][2] := xValue
Endif

Return

// ----------------------------------------------------------
// Busca o primeiro registro a atender os criterios 
// informados. 

METHOD Search(aRecord,aFound)  CLASS ZMVCMODEL
Local nI

::ClearError()
::oLogger:Write("Search")

// Limpa o registro atual da memoria 
aSize(::aRecord,0)

lOk := ::oDBConn:Connect()

IF !lOk   
   ::SetError( ::oDBConn:GetErrorStr() )
   Return .F. 
Endif

::GetTableObj()

// Shared, Read 
lOk := ::oObjectTable:Open()

If !lOk
  ::SetError( ::oObjectTable:GetErrorStr() )
   Return .F. 
Endif

// Roda evento na hora da busca 
lOk := ::RunEvents( ZDEF_ONSEARCH ) 

If lOk

	For nI := 1 to len(aRecord)
		::oLogger:Write("Search","Search for "+aRecord[nI][1]+"="+cValToChar(aRecord[nI][2]))
	Next
	
	// Realiza a busca na tabela 
	lOk := ::oObjectTable:Search(aRecord,aFound)
    
	If lOk
		// Registro encontrado, atualiza registro atual do modelo
		::aRecord := aClone(aFound)
	Endif

	If !lOk
	   ::SetError( ::oObjectTable:GetErrorStr() )
	Endif

Endif

// Fecha a tabela 
::oObjectTable:Close()

// e Desconecta do banco 
::oDBConn:Disconnect()

Return lOk

// ----------------------------------------------------------
// Atualiza um registro da tabela 

METHOD Update(aRecord)  CLASS ZMVCMODEL
Local nFld , nTot
Local lOk
Local nRecno := 0
Local nPos
Local aUpdate := {}


::ClearError()
::oLogger:Write("Update")

// Zera o registro atual da memoria 
aSize(::aRecord,0)

nPos := ascan(aRecord,{|x| x[1] == 'RECNO' })
If nPos > 0 
	// Identifica o RECNO 
	nRecno := aRecord[nPos][2]
Else
	::SetError( "RECNO não informado para UPDATE" )
	Return .F. 
Endif

lOk := ::oDBConn:Connect()

IF !lOk   
	::SetError( ::oDBConn:GetErrorStr() )
   Return .F. 
Endif

::GetTableObj()

// Shared, Write 
lOk := ::oObjectTable:Open(.F.,.T.)

If !lOk
   ::SetError( ::oObjectTable:GetErrorStr() )
   Return .F. 
Endif

// Posiciona no Registro
::oObjectTable:Goto(nRecno)

If ::oObjectTable:EOF()
    
	lOk := .F. 
	::SetError( "Update Failed -- Recno ["+cValToChar(nRecno)+"] not found " )

Endif

If lOk

	// Antes de rodar os eventos, atualiza o 
	// array de dados do registro da classe
	
	::aRecord := aClone(aRecord)
	
	// Roda os eventos apos preencher os valores dos campos   
	// Podem haver validações -- se algum evento 
	// retornar .F. a inserção não deve ser feita 
	// Os campos da tabela sao acessados usando  ::aRecord
	lOk := ::RunEvents( ZDEF_ONUPDATE )
	
Endif 

IF lOk
	
	// Passou dos eventos
	// Realiza as validações das constraints dos campos 
	// -- Campo de preenchimento obrigatório 

	lOk := ::VldConstraints()

Endif

If lOk

	// Atualiza os campos -- Mas somente os campos diferentes 
	nTot := len(::aRecord)
	For nFld := 1 to nTot
		nPos := ::oObjectTable:FieldPos(::aRecord[nFld][1])
		If nPos <= 0 
			::oLogger:Write("Update","Warning: Field ["+::aRecord[nFld][1]+"] not found.")
			Loop
		Endif
		If ::oObjectTable:FieldGet(nPos) <> ::aRecord[nFld][2]
			::oObjectTable:FieldPut(::aRecord[nFld][1],::aRecord[nFld][2])
			aadd(aUpdate , { ::aRecord[nFld][1],::aRecord[nFld][2] } )
		Endif
	Next

	if len(aUpdate) > 0 

		// VERBOSE -- apenas campos alterados 
		conout(padc(' ZMVCMODEL:UPDATE() ',79,'-'))	
		aeval( aUpdate , {|x|  conout(x[1]+" = "+cValToChar( x[2] )  ) })
		conout(replicate('-',79)+CRLF)
	
		// Efetiva as alterações
		lOk := ::oObjectTable:Update()
	
		If !lOk
		   ::SetError( ::oObjectTable:GetErrorStr() )
		Endif

	Endif
	
Endif
            
// Fecha a tabela 
::oObjectTable:Close()

// e Desconecta do banco 
::oDBConn:Disconnect()

Return lOk


// ----------------------------------------------------------
// Recupera os dados da tabela

METHOD GetData(aCols,aData) CLASS ZMVCMODEL
Local aRow := {}
Local lOk
Local nFlds
Local nI

::ClearError()
::oLogger:Write("GetData")

lOk := ::oDBConn:Connect()

IF !lOk   
	::SetError( ::oDBConn:GetErrorStr() )
   Return .F. 
Endif

::GetTableObj()

// Shared, Read
lOk := ::oObjectTable:Open(.F.,.F.)

If !lOk
   ::SetError( ::oObjectTable:GetErrorStr() )
   Return .F. 
Endif

// Roda evento antes da busca por dados 
lOk := ::RunEvents( ZDEF_ONGETDATA ) 

IF lOk

	// Beleza, agora vamos pegar os dados que interessam 
	nFlds := ::oObjectTable:FCount()
	::oObjectTable:GoTop()

	For nI := 1 to nFlds
		aadd(aCols,::oObjectTable:FieldName(nI))
	Next
	
	While !::oObjectTable:EOF()

		aRow := {}
		For nI := 1 to nFlds
			aadd(aRow,::oObjectTable:FieldGet(nI))
		Next
		aadd(aRow,::oObjectTable:Recno())
		aadd(aRow,::oObjectTable:Deleted())
		
		AADD(aData,aRow)
		
		::oObjectTable:Skip()
	
	Enddo

Endif

// Fecha a tabela 
::oObjectTable:Close()

// e Desconecta do banco 
::oDBConn:Disconnect()

Return lOk


// ----------------------------------------------------------
// Valida as constraints dos campos
// Por exemplo, se o campo é obrigatório e nao está preenchido 

METHOD VldConstraints() CLASS ZMVCMODEL
Local aFieldsDef
Local nFldCount , nFld
Local oFldDef , cFldName
Local nPos , xValue 
Local lOk := .T.
Local cMsgVld := ''

aFieldsDef := ::oObjectDef:GetFields()
nFldCount  := len( aFieldsDef )

For nFld := 1 to nFldCount
	
	oFldDef := aFieldsDef[nFld]
	cFldName := oFldDef:GetField()
	
	If oFldDef:IsRequired()
		nPos := ascan(::aRecord,{|x| x[1] == cFldName })
		If nPos > 0
			xValue := ::aRecord[nPos][2]
		Else
			xValue := NIL
		Endif
		If Empty(xValue)
			cMsgVld += 'O campo ['+oFldDef:GetLabel()+'] é obrigatório e deve ser informado.'+CRLF
		Endif
	Endif
	
Next

If !empty(cMsgVld)
	lOk := .F.
	::SetError( cMsgVld )
Endif

Return lOk

