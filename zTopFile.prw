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
#include 'zlib.ch'

/* ===========================================================================

Classe		ZTOPFILE
Autor		Júlio Wittwer
Data		01/2019

Descrição   Classe de encapsulamento de acesso a arquivos ISAM do DBAccess

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
  DATA oDBConn              // Driver de conexao com o banco  
  DATA cSqlOrderBy          // Order By para acesso SQL 

  DATA oLogger              // Objeto de log 

  // ========================= Metodos de uso público da classe

  METHOD NEW()              // Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela no SGDB
  METHOD DROP()             // Apaga a tabela do SGDB

  METHOD GetFileType()      // Tipo do arquivo ("MEMORY")
  METHOD SetDBConn()        // Conexao com o Banco para uso desta tabela 

  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPut( nPos )   // Faz update em uma coluna do registro atual 
  METHOD FileName()         // Retorna nome do arquivo aberto 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
  METHOD LastRec()          // Retorna o ultimo registro da tabela 
  
  METHOD Insert()           // Insere um registro em branco no final da tabela
  METHOD Update()           // Atualiza o registro atual na tabela 
  METHOD UpdStruct()        // Verifica estrutura fisica versus definição 
  METHOD UpdIndex()         // Verifica se os indices precisam ser atualizados
  METHOD Search()           // Busca um registro que atenda os criterios informados

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD FileSize()         // Retorna o tamanho ocupado pelo arquivo em bytes 
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 

  METHOD GoTop()
  METHOD Skip()
  METHOD GoTo()	     	    // Posiciona em um registro informado. 

  // Metodos exclusivos para TOP FILE 
  
  METHOD SetSQLOrderBy()
  METHOD GetSQLOrderBy()
 
 
  // ========================= Metodos de uso interno da classe

  METHOD _InitVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 


ENDCLASS

// ----------------------------------------------------------
// Retorna o tipo do arquivo 

METHOD GetFileType() CLASS ZTOPFILE 
Return "TOPCONN"

// ----------------------------------------------------------
// Construtor do objeto TOP
// Apenas recebe o nome do arquivo e inicializa as propriedades
// Inicializa o ZISAMFILE passando a instancia atual 

METHOD NEW(cFile,oFileDef) CLASS ZTOPFILE 
_Super:New(self)

::oLogger := ZLOGGER():New("ZTOPFILE")
::oLogger:Write("NEW","File: "+cFile)

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

If oFileDef != NIL 
	// Passa a definição pro IsamFile 
	::SetFileDef(oFileDef)
Endif

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZTOPFILE 

::oLogger:Write("OPEN")

::_ResetError()

If ::oDBConn = NIL 
	UserException("ZTOPFILE:OPEN() -- DBCONN NOT SET")
Endif

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:OPEN() -- DBCONN NOT CONNECTED")
Endif

If ::lOpened
	::_SetError("File Already Open")
	Return .F.
Endif

IF !::Exists()
	::_SetError("Unable to OPEN - TOP File ["+::cFileName+"] DOES NOT EXIST")
	Return .F.
Endif

If lExclusive = NIL ; 	lExclusive := .F. ; Endif
If lCanWrite = NIL ; 	lCanWrite := .F.  ; Endif

// O TopFile permite abrir arquivo para escrita sem estar em modo exclusivo

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
	::_SetError("Unable to OPEN - TOP File ["+::cFileName+"] IN "+IIf( lExclsuive,"EXCLUSIVE","SHARED") +" MODE ")
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

// Cria o array de campos em memoria do registro atual 
::aGetRecord := Array(::nFldCount)
::aPutRecord := Array(::nFldCount)

// Atualiza o LastRec
::nLastRec := 	(::cAlias)->(LastRec())

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZTOPFILE 

::oLogger:Write("CLOSE")

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
If Select(::cAlias) > 0 
	(::cAlias)->(DBCommit()) 
	(::cAlias)->(DbCloseAreA()) 
Else
	conout("--- WARNING - ZTOPFILE:CLOSE() ALIAS UNEXPECTEDLY CLOSED ---")
Endif

// Limpa as propriedades -- inclusive flag de abertura 
::_InitVars()

Return 

// ----------------------------------------------------------\
// Verifica se a tabela existe no banco de dados 
METHOD EXISTS() CLASS ZTOPFILE 

::oLogger:Write("EXISTS")

If ::oDBConn = NIL 
	UserException("ZTOPFILE:EXISTS() -- DBCONN NOT SET")
Endif

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:EXISTS() -- DBCONN NOT CONNECTED")
Endif

Return TCCanOpen(::cFileName)

// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome já foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 

METHOD CREATE( aStru ) CLASS ZTOPFILE
Local nFields := 0
Local nI

::oLogger:Write("CREATE")

If ::oDBConn = NIL 
	UserException("ZTOPFILE:Create() -- DBCONN NOT SET")
Endif

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:Create() -- DBCONN NOT CONNECTED")
Endif

If ::EXISTS()
	::_SetError("CREATE ERROR - File Already Exists")
Endif

If ::lOpened
	::_SetError("CREATE ERROR - File Already Opened")
Endif

If aStru = NIL .AND. ::oFileDef != NIL 
	// Se a erstrutura nao foi informada 
	// Mas a tabela tem a definição , 
	// pega a estrutura da definicao 
	aStru := ::oFileDef:GetStruct()
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

// ----------------------------------------------------------\

METHOD DROP() CLASS ZTOPFILE
Local lOk := .T. 

::oLogger:Write("DROP")

If ::oDBConn = NIL 
	UserException("ZTOPFILE:Drop() -- DBCONN NOT SET")
Endif

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:Drop() -- DBCONN NOT CONNECTED")
Endif

If ::lOpened
	::_SetError("DROP ERROR - File is Opened")
	Return .F.
Endif

// Dropa a tabela
lOk := TCDelFile(::cFileName)

If !lOk
	::_SetError(TCSqlError())
Endif

Return lOk

// ----------------------------------------------------------\
// Seta o objeto da conexao na tabela 
METHOD SetDBConn(oDB) CLASS ZTOPFILE

::oLogger:Write("SetDBConn")

::oDBConn := oDB
Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZTOPFILE 

::oLogger:Write("_InitVars")

// Inicialização das propriedades da classe pai
_Super:_InitVars()

// Inicializa demais propriedades da ZTOPFILE
::lOpened     := .F. 
::lExclusive  := .F. 
::lCanWrite   := .F.
::aPutRecord  := {}
::lUpdPend    := .F. 
::lInInsert   := .F. 
::cSqlOrderBy := ''

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
If valtype(nPos) = 'C'
	nPos := ::FieldPos(nPos)
Endif         

If nPos < 1 .or. nPos > ::nFldCount 
	Return NIL 
Endif
	
If ::lInInsert  .AND. ::aPutRecord[nPos] != NIL 
	// Estou em inserçào, pega valores da memória 
	// Se eu já atualizei algum campo, pega o valor 
	// senão pega o valor default 
	Return ::aPutRecord[nPos]
Endif

Return ::aGetRecord[nPos]

// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 
// Faz isso no array em memoria 

METHOD FieldPut(nPos,xValue) CLASS ZTOPFILE 

If valtype(nPos) = 'C'
	nPos := ::FieldPos(nPos)
Endif

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
// Atualiza LastRec do ZISAMFILE e retorna 

METHOD Lastrec() CLASS ZTOPFILE
::nLastRec := 	(::cAlias)->(LastRec())
Return ::nLastRec



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

::oLogger:Write("Insert")

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

::oLogger:Write("Update")

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
	::oLogger:Write("Update","Nothing to do...")
	Return .T. 
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

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lê o registro atual para a memoria 

METHOD _ReadRecord() CLASS ZTOPFILE
Local nI

::oLogger:Write("_ReadRecord","Recno: "+cValToChar(::Recno()))

For nI := 1 to ::nFldCount
	::aGetRecord[nI] := (::cAlias)->(FieldGet(nI))
Next

// Reseta flags de BOF e EOF 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZTOPFILE

::oLogger:Write("_ClearRecord()")

// Inicializa com o valor default os campos da estrutura 
_Super:_ClearRecord()

Return

// ----------------------------------------------------------
// Vai para o topo da tabela atual na ordem atual 

METHOD GoTop() CLASS ZTOPFILE

::oLogger:Write("GoTop")

(::cAlias)->(DBGoTop())

::lBOF := (::cAlias)->(Bof())
::lEOF := (::cAlias)->(Eof())
::nRecno := (::cAlias)->(Recno())

IF !::lEof
	::_ReadRecord()
Else
    ::_ClearRecord()
Endif

Return


// ----------------------------------------------------------
// 

METHOD Skip(nRecs) CLASS ZTOPFILE

If nRecs = NIL
	nRecs := 1 
Endif

(::cAlias)->(DBSkip(nRecs))

::lBOF := (::cAlias)->(Bof())
::lEOF := (::cAlias)->(Eof())
::nRecno := (::cAlias)->(Recno())

IF !::lEof
	::_ReadRecord()
Else
    ::_ClearRecord()
Endif

Return


// ----------------------------------------------------------
//

METHOD GoTo(nRec)	CLASS ZTOPFILE

::oLogger:Write("GoTo")

(::cAlias)->(DBGoto(nRec))

::lBOF := (::cAlias)->(Bof())
::lEOF := (::cAlias)->(Eof())
::nRecno := (::cAlias)->(Recno())

IF !::lEof
	::_ReadRecord()
Else
    ::_ClearRecord()
Endif

Return

// ----------------------------------------------------------
// Verifica estrutura fisica versus definição 

METHOD UpdStruct(oFileDef) CLASS ZTOPFILE
Local aDBStru := {}   // Estrutura da tabela no banco de dados 
Local aDefStru := {}  // Estrutura segundo a definição da tabela 
Local lOk

::oLogger:Write("UpdStruct")

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:UpdStruct() -- DBCONN NOT CONNECTED")
Endif

If empty(::aStruct)
	// Se eu ainda nao tenho a estrutura fisica, busca no banco 
	// Abertura Shared, Read Only
	IF ::Open(.F.,.F.)
		aDBStru := aClone(::aStruct)
		::Close()
	Else
		Return .F. 
	Endif
Endif

// Agora vamos ver a estrutura segundo a definicao 
aDefStru := oFileDef:GetStruct()

// Sorteia as estruturas por nome para comparar 
aSort(aDbStru,,,{|x1,x2| x1[1] < x2[1] })
aSort(aDefStru,,,{|x1,x2| x1[1] < x2[1] })

If zCompare(aDbStru,aDefStru) <> 0 

	If MsgYesNo("Definição diferente da estrutura do Banco de Dados -- Deseja atualizar ?")

		// Se nao tem campos novos, mas as estruturas estao diferentes
		// pode ter havido alteração de parametros. Verificar o que fazer...
		lOk := TCAlter(::cFileName , aDbStru , aDefStru )
	
		IF !lOk
			::_SetError(TCSqlError())
			Return .F. 
		Endif
	
		MsgInfo("Estrutura da Tabela ["+::cFileName+"] ajustada automaticamente.")
	
	Else 

		QUIT
		
	Endif
	
Endif


Return .T. 
               

// ----------------------------------------------------------
// Verifica indices no BAnco Versus definição 
// Por hora apenas cria os índices caso nao existam 
// TODO -- Verificar chaves e recriar em caso de diferença

METHOD UpdIndex(oFileDef) CLASS ZTOPFILE
Local aIndex 
Local cIndex
Local nI

::oLogger:Write("UpdIndex")

If !::oDBConn:IsConnected()
	UserException("ZTOPFILE:UpdIndex() -- DBCONN NOT CONNECTED")
Endif

// Pega os indices da definição 
aIndex := oFileDef:GetIndex()

For nI := 1 to len(aIndex)

	cIndex := ::cFileName+cvaltochar(nI)
	cExp := aIndex[nI]

    IF !TCCanOpen(::cFileName,cIndex)

    	// Indice nao existe 
		IF ::Open(.F.,.F.)
			INDEX ON &(cExp) TO (cIndex)
			::Close()
		Else
			Return .F. 
		Endif

	Endif
	
Next

Return  .T.

// ----------------------------------------------------------
// Busca um registro que atenda os criterios informados
// aRecord recebe os dados a procurar no formato [1] Campo [2][ Conteudo 
// aFound retorna o registro encontrado por referencia ( todos os campos ) 
// no mesmo formato do aRecord, acrescido do RECNO 
// -- Optimização no TOPFILE para usar QUERY 

METHOD Search(aRecord,aFound,lExact)  CLASS ZTOPFILE
Local nCnt := len(aRecord)
Local nI
Local nPos
Local cQuery
Local cType
Local nRecFound := 0
Local oQuery

::oLogger:Write("Search")

IF lExact = NIL
	lExact := .F.
Endif

aSize(aFound,0)

cQuery := 'SELECT '
If TCGetDB() == 'MSSQL'
	cQuery += 'TOP 1'
Endif
cQuery += ' R_E_C_N_O_ AS RECNO FROM '
cQuery += ::cFileName
cQuery += ' WHERE '

For nI := 1 to nCnt
	
	nPos := ::FieldPos(aRecord[nI][1])
	cType := ::FieldType(nPos)
    
	If cType = 'M'
		// Campo memo por hora nao suportado 
		// na busca generica
		Loop
	Endif

	If nI > 1
		cQuery += ' AND '
	Endif
	
	IF cType = 'C'
		IF lExact
			cQuery += ::FieldName(nPos)
			cQuery += " = '"+aRecord[nI][2]+"' "
		Else
			cQuery += " lower("+::FieldName(nPos)+") "
			cQuery += " LIKE '"+Lower(Alltrim(aRecord[nI][2]))+"' "
		Endif
	ElseIF cType = 'D'
		cQuery += ::FieldName(nPos)
		cQuery += " = '"+DTOS(aRecord[nI][2])+"' "
	ElseIF cType = 'N'
		cQuery += ::FieldName(nPos)
		cQuery += " = "+cValToChar(aRecord[nI][2])+" "
	ElseIF cType = 'L'
		cQuery += ::FieldName(nPos)
		cQuery += " = '"+IIF(aRecord[nI][2],'T','F')+"' "
	Endif
	
Next

If nCnt > 0
	cQuery += ' AND '
Endif	

cQuery += " D_E_L_E_T_ != '*' "

If !Empty(::cSqlOrderBy)
	cQuery += ' ORDER BY '+::cSqlOrderBy
Endif

oQuery := ZQUERYFILE():New(cQuery)
oQuery:Open()
oQuery:SetField('RECNO','N',16,0)
If !oQuery:Eof()
	nRecFound := oQuery:Fieldget(1)
Endif
oQuery:Close()
FreeObj(oQuery)

IF nRecFound > 0
	
	// Achou , posiciona no registro e traz o conteudo
	::Goto(nRecFound)
	
	For nI := 1 to ::nFldCount
		aadd(aFound , {  ::FieldName(nI) , ::FieldGet(nI)  })
		// conout("Found ["+::FieldName(nI)+"] = ["+cValToChar(::FieldGet(nI))+"]")
	Next
	
	// Acrescenta o RECNO no array
	aadd(aFound,{"RECNO",::Recno() })
	Return .T.
	
Endif

::_SetError( "Nenhum registro foi encontrado baseado nos dados informados" )

Return .F.


// ----------------------------------------------------------
// Permite acrescentar um ORDER BY direto na query do componente de busca 

METHOD SetSQLOrderBy(cOrderBy) CLASS ZTOPFILE 
::oLogger:Write("SetSQLOrderBy",cOrderBy)
::cSqlOrderBy := cOrderBy
Return

// ----------------------------------------------------------
// Recupera o ORDER BY setado para a query de busca (search) 

METHOD GetSQLOrderBy() CLASS ZTOPFILE 
Return ::cSqlOrderBy

