#include 'Protheus.ch'

/* ===========================================================================

Classe		ZTOPFILE
Autor		Júlio Wittwer
Data		01/2019

Descrição   Classe de encapsulamento de acecsso a arquivos ISAM do DBAccess

Versao		1.0 

Pendencias :

1) Reimplementar alguns metodos da ZISAMFILE 
2) Rever mecanismo de LOCK ou trabalhar sob demanda 
3) Implementar controle de transacionamento 
			
=========================================================================== */

STATIC __TopSeq := 0    // Sequenciador de aberturas de Tabelas para montar o alias 

CLASS ZTOPFILE FROM ZISAMFILE

  DATA cFileName		    // Nome / Identificador do arquivo no DBAccess
  DATA cAlias               // Alias do Arquivo em AdvPL

  DATA lExclusive           // Arquivo aberto em modo exclusivo ?
  DATA lInInsert            // Alias em modo de inserção de registro 
  DATA lUpdPend             // Flag indicando update pendente 

  // ========================= Metodos de uso público da classe

  METHOD NEW()              // Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela no disco 

  METHOD GetFileType()      // Tipo do arquivo ("MEMORY")

  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPut( nPos )   // Faz update em uma coluna do registro atual 
  METHOD FileName()         // Retorna nome do arquivo aberto 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
  
  METHOD Insert()           // Insere um registro em branco no final da tabela
  METHOD Update()           // Atualiza o registro atual na tabela 

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD FileSize()         // Retorna o tamanho ocupado pelo arquivo em bytes 
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 
 
  // ========================= Metodos de uso interno da classe

  METHOD _InitVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 

ENDCLASS

// ----------------------------------------------------------
// Retorna o tipo do arquivo 

METHOD GetFileType() CLASS ZTOPFILE 
Return "TOPCONN"

// ----------------------------------------------------------
// Construtor do objeto TOP
// Apenas recebe o nome do arquivo e inicializa as propriedades

METHOD NEW(cFile) CLASS ZTOPFILE 

::_InitVars() 

// Guarda o nome da tabela 
::cFileName   := UPPER(cFile)

// Calcula proxima sequencia para alias 
__TopSeq++
IF __TopSeq > 99999
	__TopSeq := 1
Endif

// Monta o alias para a tabela atual 
::cAlias   := "TOP"+StrZero(__TopSeq,5)

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZTOPFILE 

::_ResetError()

If ::lOpened
	::_SetError(-1,"File Already Open")
	Return .F.
Endif

IF !::Exists()
	::_SetError(-6,"Unable to OPEN - TOP File ["+::cFileName+"] DOES NOT EXIST")
	Return .F.
Endif

If lExclusive = NIL ; 	lExclusive := .F. ; Endif
If lCanWrite = NIL ; 	lCanWrite := .F.  ; Endif

// Por enquanto faz escrita apenas em modo exclusivo
If lCanWrite .AND. !lExclusive
	::_SetError(-6,"Unable to OPEN for WRITE in SHARED MODE -- Use Exclusive mode or OPEN FOR READ")
	Return .F.
Endif

// Define modo de abertura de acordo com os parametros

IF lExclusive

	IF lCanWrite
		USE (::cFileName) ALIAS (::cAlias) EXCLUSIVE NEW VIA "TOPCONN"
	Else 
		USE (::cFileName) ALIAS (::cAlias) EXCLUSIVE READONLY NEW VIA "TOPCONN"
	Endif

Else

	IF lCanWrite
		USE (::cFileName) ALIAS (::cAlias) SHARED NEW VIA "TOPCONN"
	Else
		USE (::cFileName) ALIAS (::cAlias) SHARED READONLY NEW VIA "TOPCONN"
	Endif
Endif

If NetErr()
	::_SetError(-15,"Unable to OPEN - TOP File ["+::cFileName+"] IN "+IIf( lExclsuive,"EXCLUSIVE","SHARED") +" MODE ")
	Return .F. 
Endif

// Atualiza propriedades de controle da classe
::lExclusive   := lExclusive
::lCanWrite    := lCanWrite

// Cria o array de campos do registro atual 
::aPutRecord := Array(::nFldCount)

// Seta que o arquivo está aberto 
::lOpened := .T. 

// Pega a estrutura e o numero de campos
::aStruct := (::cAlias)->(DBStruct())
::nFldCount := len(::aStruct)

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZTOPFILE 

If !::lOpened
	// Arquivo ' já está fechado, não faz nada. 
	Return 
Endif

IF ::lUpdPend    
	conout("--- WARNING - ZTOPFILE:CLOSE() REQUESTED WITH PENDING UDPATE ---")
	::Update()
ElseIf ::lInInsert  
	conout("--- WARNING - ZTOPFILE:CLOSE() REQUESTED WITH PENDING INSERT ---")
	::Update()
Endif

// Fecha todos os indices 
::ClearIndex()

// Agora fecha a tabela 
(::cAlias)->(DBCommit()) 
(::cAlias)->(DbCloseAreA()) 

// Limpa as propriedades -- inclusive flag de abertura 
::_InitVars()

Return 

// ----------------------------------------------------------\
// Verifica se a tabela existe no banco de dados 
METHOD EXISTS() CLASS ZTOPFILE 
Return TCCanOpen(::cFileName)

// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome já foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 

METHOD CREATE( aStru ) CLASS ZTOPFILE
Local nFields := 0
Local nI

If ::EXISTS()
	::_SetError(-7,"CREATE ERROR - File Already Exists")
Endif

If ::lOpened
	::_SetError(-8,"CREATE ERROR - File Already Opened")
Endif

// Valida a estrutura informada
nFields := len(aStru)
For nI := 1 to nFields
	If !aStru[nI][2]$"CNDLM"
		UserException("ZTOPFILE:CREATE() ERROR - INVALID FIELD TYPE "+aStru[nI][2]+ " ("+aStru[nI][1]+")" )
	Endif
	// Apenas Ajusta nome do campo 
	aStru[nI][1] := Upper(padr(aStru[nI][1],10))
Next

DBCreate(::cFileName , aStru , "TOPCONN")

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZTOPFILE 

// Inicialização das propriedades da classe pai
_Super:_InitVars()

// Inicializa demais propriedades da ZTOPFILE
::lOpened     := .F. 
::lExclusive  := .F. 
::lCanWrite   := .F.
::aPutRecord  := {}
::lUpdPend    := .F. 
::lInInsert   := .F. 

Return

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZTOPFILE
UserException("ZTOPFILE:LUPDATE() NOT IMPLEMENTED")
Return 

// ----------------------------------------------------------
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura
// TODO - Inserir tratamento caso eu esteja em inserção 

METHOD FieldGet(nPos) CLASS ZTOPFILE 
If nPos > 0 .and. nPos <= ::nFldCount 
	Return (::cAlias)->(FieldGet(nPos))
Endif
Return NIL

// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 
// Faz isso no array em memoria 

METHOD FieldPut(nPos,xValue) CLASS ZTOPFILE 

If ( !::lCanWrite )
	UserException("ZTOPFILE::FieldPut() Error -- File NOT OPEN for WRITING")
Endif

If ( ::lEOF )
	UserException("ZTOPFILE::FieldPut() Error -- File is in EOF")
Endif

If nPos > 0 .and. nPos <= ::nFldCount 
	If ::aStruct[nPos][2] == 'C'
		// Ajusta tamanho de string com espaços a direita
		// ( Preenche ou trima ) 
		xValue := PadR(xValue,::aStruct[nPos][3])
	Endif
	
	::aPutRecord[nPos] := xValue

	// Liga flag de update pendente
	::lUpdPend := .T. 
	
Endif

Return NIL

// ----------------------------------------------------------
// Recupera o nome do arquivo no disco 
METHOD FileName() CLASS ZTOPFILE 
Return ::cFileName

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado  
// Rever tratamento durante inserção de registro 
METHOD DELETED() CLASS ZTOPFILE 
If !::lEOF
	Return (::cAlias)->(Deleted())
Endif
Return .F. 

// ----------------------------------------
// Retorna o tamanho estimado do HEader
// Baseado na estrutura  o tamanho seria 32 * o tamanho da estrutura 
// Mais 32 bytes do Header, mais 2 do final da estrutura 

METHOD HEADER() CLASS ZTOPFILE 
Return ( len(::aStruct) * 32 ) + 32 + 2 

// ----------------------------------------
// Retorna o tamanho aproximado do arquivo 
// -- Desconsidera campos MEMO 

METHOD FileSize() CLASS ZTOPFILE
UserException("ZTOPFILE:FileSize() NOT IMPLEMENTED")
Return 

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RecSize() CLASS ZTOPFILE 
UserException("ZTOPFILE:RecSize() NOT IMPLEMENTED")
Return

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZTOPFILE 
If ::lInInsert  
	conout("--- WARNING - ZTOPFILE:RECNO() REQUESTED WITH PENDING INSERT ---")
	::Update()
Endif
Return (::cAlias)->(Recno())

// ----------------------------------------
// Faz o alias atual entrar em modo de inserção pendente 
// A confirmação da inserção é feita pelo Update()
METHOD Insert() CLASS ZTOPFILE

If ::lInInsert
	conout("--- WARNING - ZTOPFILE:Insert() REQUESTED WITH PENDING INSERT ---")
	::Update()
Endif

// Estou em modo de inserção 
::lInInsert  := .T. 

// Limpa o conteudo do registro em memoria 
::_ClearRecord()

// Nao estou em BOF ou EOF, 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 

// ----------------------------------------
// Grava as alterações do registro atual na tabela 

METHOD Update() CLASS ZTOPFILE
Local nI
Local lHasLock := .F. 

If ( ::lEOF )
	UserException("ZTOPFILE::Update() ERROR -- File is in EOF")
	Return
Endif

If ::lInInsert

	// Estou em modo de inserção, insere o registro
	(::cAlias)->(DBAppend())
	lHasLock := .T. 

	// Informa que existe update pendente e desliga inserção 
	::lUpdPend  := .T. 
	::lInInsert := .F. 

Endif

If !::lUpdPend
	// Nao tem update pendente, nao faz nada
	Return
Endif

If !lHasLock

	If !(::cAlias)->(DbrLock(Recno()))
		UserException("LOCK FAILED")	
		Return
	Endif
	
	lHasLock := .T. 
	
Endif

For nI := 1 to ::nFldCount

	// Atualiza apenas os campos que receberam conteudo 
	// Atualiza o registro no SGDB 
	// E limpa o elemento do array de update pendente 
	If ::aPutRecord[nI] != NIL 
		(::cAlias)->(FieldPut( nI , ::aPutRecord[nI] ))
		::aPutRecord[nI] := NIL 
	Endif

	// Ao fazer o update, manda o Flush dos dados 
	(::cAlias)->(DbCommit())
	
Next

// Agora que o registro está atualizado, atualiza os indices 
aEval(::aIndexes , {|oIndex| oIndex:UpdateKey() })

// Desliga flag de update pendente 
::lUpdPend := .F. 

// solta o lock -- por enquanto nao está preparado para transação 
(::cAlias)->(DBRUnlock(Recno()))

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZTOPFILE

// Inicializa com o valor default os campos da estrutura 
_Super:_ClearRecord()

Return

