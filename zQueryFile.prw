#include 'Protheus.ch'

/* ===========================================================================

Classe		ZQUERYFILE
Autor		Júlio Wittwer
Data		01/2019

Descrição   Classe de consulta ao DBAccess por Queries 

Versao		1.0 

Observações 

Por baixo usa o mecanismo de Queries do DBAccess. 
Exige uma conexão ativa com o banco de dados -- TCLink()
Usa sempre a conexão corrente. Cria um alias temporario 
para mascarar a Query. Ainda não herda do ZISAMFILE, somente possui 
os métodos suportados por uma Query : OPEN,CLOSE, SKIP, GOTOP 
Possui SetField() para permitir trocar o tratamento de campos 
usando por baixo a TCSetField()
		
=========================================================================== */

STATIC __QrySeq := 0    // Sequenciador de aberturas de Query na thread atual 

CLASS ZQUERYFILE FROM LONGNAMECLASS

  DATA cQryAlias		    // Alias da Query no AdvPL 
  DATA cQryStr              // String da Query 
  DATA lOpened              // Flag indicando se a Query está aberta
  DATA aSetField            // Array com os tratamentos diferenciados de campos

  // ========================= Metodos de uso público da classe

  METHOD NEW()	            // Construtor
  METHOD OPEN()	   	        // Abertura da Query
  METHOD CLOSE()			// Fecha a Query 

  METHOD GetFileType()      // Tipo do arquivo ("QUERY")

  METHOD FieldGet()         // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPos()         // Retorna a posicao de um campo na estrutura da tabela ( ID da Coluna )
  METHOD FieldName()        // Recupera o nome da coluna informada 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD GetStruct()		// Retorna estrutura de dados da Query
  METHOD SetField()         // Seta tratamento diferenciado de campo 
  METHOD FCount()           // Retorna o numero de campo / colunas da Query
  METHOD GoTop()            // Sumbete novamente a Query 
  METHOD Skip()             // lê o proximo registro 
  METHOD EOF()				// Retorna .T. caso acabou  os registros da Query 
  
ENDCLASS

// ----------------------------------------------------------
// Retorna o tipo do arquivo 

METHOD GetFileType() CLASS ZQUERYFILE 
Return "QUERY"

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades

METHOD NEW(cQuery) CLASS ZQUERYFILE 

::cQryStr     := cQuery
::lOpened     := .F. 
::aSetField   := {}

// Calcula proxima sequencia 
__QrySeq++
IF __QrySeq > 99999
	__QrySeq := 1
Endif

// Monta o alias para a Query atual
::cQryAlias   := "QRY"+StrZero(__QrySeq,5)

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN() CLASS ZQUERYFILE 

If ::lOpened
	UserException("ZQUERYFILE:OPEN() ERROR - Query ALREADY opened")
	Return .F.
Endif

// Abre a Query 
USE (TCGenQry(,,::cQryStr)) ALIAS (::cQryAlias) SHARED NEW VIA "TOPCONN"

// Seta que a Query está aberta
::lOpened := .T. 

// Caso já existam SetFields definidos, aplica
aEval( ::aSetField , {|x| TCSetField(::cQryAlias,x[1],x[2],x[3],x[4]) } )

Return .T. 


// ----------------------------------------------------------
// Submete novamente a Query, para ela reposicionar no 
// primeiro registro 

METHOD GoTop() CLASS ZQUERYFILE 
If !::lOpened
	UserException("ZQUERYFILE:GOTOP() ERROR - Query not opened")
Endif

(::cQryAlias)->(DBGoTop())

Return


// ----------------------------------------------------------
// Fecha a Query 
// O alias é fechado , o flag de aberto é resetado
// Mas a Query pode ser sumbetida novamente 

METHOD CLOSE() CLASS ZQUERYFILE 

// Fecha o alias 
If ::lOpened
	(::cQryAlias)->(DbCloseArea())
Endif

::lOpened     := .F. 

Return 

// ----------------------------------------------------------
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura

METHOD FieldGet(nPos) CLASS ZQUERYFILE 
Return (::cQryAlias)->(FieldGet(nPos))

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZQUERYFILE 
Return (::cQryAlias)->(Recno())

// ----------------------------------------
// Retorna a estrutura de dados da Query 

METHOD GetStruct() CLASS ZQUERYFILE
If !::lOpened
	UserException("ZQUERYFILE:GETSTRUCT() ERROR - Query not opened.")
	Return
Endif
Return (::cQryAlias)->(DBStruct())

// ----------------------------------------------------------
// Recupera o numero do campo na estrutura da tabela 
// a partir do nome do campo 

METHOD FieldPos( cField ) CLASS ZQUERYFILE
If !::lOpened
	UserException("ZQUERYFILE:FIELDPOS() ERROR - Query not opened.")
Endif
Return (::cQryAlias)->(FieldPos(cField))

// ----------------------------------------------------------
// Recupera o nome da coluna informada 

METHOD FieldName( nPos ) CLASS ZQUERYFILE
If !::lOpened
	UserException("ZQUERYFILE:FIELDNAME() ERROR - Query not opened.")
Endif
Return (::cQryAlias)->(FieldName(nPos))

// ----------------------------------------
// Permite setar tratamento diferenciado para campo da Query 
// O SetField atualiza a estrutuda a Query na memoria 
METHOD SetField(cCampo,cTipo,nTam,nDec)  CLASS ZQUERYFILE
Local nPos 

If !::lOpened
	UserException("ZQUERYFILE:SETFIELD() ERROR - Query not opened.")
	Return
Endif

// Ajusta nmome do campo 
cCampo := alltrim(upper(cCampo))

// Aplica TcSetField
TCSetField(::cQryAlias,cCampo,cTipo,nTam,nDec)

// Armazena o SetField Utilizado 
// Caso a query seja Fechada e re-aberta 
nPos := ascan(::aSetField , {|x|  x[1] == cCampo })
If nPos > 0 
	::aSetField[nPos] := { cCampo,cTipo,nTam,nDec } 
Else
	aadd(::aSetField , { cCampo,cTipo,nTam,nDec } )
Endif

Return

// ----------------------------------------
// Retorna o numero de campo / colunas da Query

METHOD FCount() CLASS ZQUERYFILE
If !::lOpened
	UserException("ZQUERYFILE:SETFIELD() ERROR - Query not opened.")
	Return
Endif
Return (::cQryAlias)->(FCount())

// ----------------------------------------
// Pula um numero de registros

METHOD Skip( nQtd ) CLASS ZQUERYFILE

If nQtd = NIL ; nQtd := 1 ; Endif

If !::lOpened
	UserException("ZQUERYFILE:SKIP() ERROR - Query not opened.")
	Return .F.
Endif

If nQtd < 1 
	UserException("ZQUERYFILE:SKIP() ERROR - Invalid SKIP argument ("+cValToChar(nQtd)+")")
Endif

(::cQryAlias)->(DbSkip(nQtd))

Return
           
// ----------------------------------------
// Retorna .T. caso os registros da Query tenham acabado 
METHOD EOF() CLASS ZQUERYFILE
If !::lOpened
	UserException("ZQUERYFILE:EOF() ERROR - Query not opened.")
Endif
Return (::cQryAlias)->(EOF())

