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

#define QRY_BATCHSIZE 50 // Batch Size Default ( prefetch ) 

/* ======================================================

Mongo DB Client API 

*** CLASSE AINDA NAO TERMINADA -- FALTA MUITA COISA *** 

===================================================== */

STATIC __QryID := 0

CLASS ZMONGOQUERY FROM LONGNAMECLASS
               
   DATA oMongoDB
   DATA cTable
   DATA oCmdFind
   DATA oCmdGetMore
   DATA oJsonFilter
   DATA oJsonOrder
   DATA oResponse
   DATA cCursorID    
   DATA aRecords
   DATA iRecPos
   DATA lEOF
   DATA nQryID
   DATA lVerbose

   METHOD New()
   METHOD SetVerbose()
   METHOD FromSQL()
   METHOD SetProperty()
   METHOD AddFilter()
   METHOD AddOrder()
   METHOD Open()
   METHOD GetObject()
   METHOD Eof()
   METHOD Skip()
   METHOD Close()
   
ENDCLASS

// ----------------------------------------

METHOD NEW(cTable,oMongoDB) CLASS ZMONGOQUERY
::oMongoDB   := oMongoDB
::oCmdFind   := JSONOBJECT():New()
::cTable     := cTable
::aRecords   := {}
::lEOF       := .T.
Return self

// ----------------------------------------

METHOD SetVerbose(lSet) CLASS ZMONGOQUERY
::lVerbose := lSet
Return

// ----------------------------------------

/*
SELECT FROM <collection> WHERE 
ORDER BY <cols,...>
LIMIT <nRows>
*/

METHOD FromSQL(cSql) CLASS ZMONGOQUERY

Return


// ----------------------------------------

METHOD SetProperty(cProp,xValue) CLASS ZMONGOQUERY
::oCmdFind[cProp] := xValue
Return

// ----------------------------------------
/*
$eq		Matches values that are equal to a specified value.
$gt		Matches values that are greater than a specified value.
$gte	Matches values that are greater than or equal to a specified value.
$in		Matches any of the values specified in an array.
$lt		Matches values that are less than a specified value.
$lte	Matches values that are less than or equal to a specified value.
$ne		Matches all values that are not equal to a specified value.
$nin	Matches none of the values specified in an array.
*/

METHOD AddFilter(cField,cOperator,xValue) CLASS ZMONGOQUERY

If ::oJsonFilter = NIL
	::oJsonFilter := JSonObject():New()
Endif

If cOperator == '$eq'
	::oJsonFilter[cField] := xValue
Else                         
	::oJsonFilter[cField] := JSonObject():New()
	::oJsonFilter[cField][cOperator] := xValue
Endif

Return

// ----------------------------------------

METHOD AddOrder(cField,bDesc) CLASS ZMONGOQUERY
If ::oJsonOrder = NIL
	::oJsonOrder := JsonObject():New()
Endif                                 
IF bDesc 
	::oJsonOrder[cField] := -1 // Descending order
Else
	::oJsonOrder[cField] := 1 // Ascending order
Endif
Return

// ----------------------------------------
// Abre  a Query / Cursor no Banco de Dados

METHOD Open() CLASS ZMONGOQUERY

If ::oMongoDB = NIL
	UserException("ZMONGOQUERY OPEN ERROR - DBCLIENT NOT SET")
Endif

IF empty(::cTable )
	UserException("ZMONGOQUERY OPEN ERROR - TABLE NOT SET")
Endif

IF ::oResponse != NIL
	UserException("ZMONGOQUERY OPEN ERROR - CURSOR ALREADY OPEN")
Endif

// Monta o comando find()
::oCmdFind['find'] := ::cTable

// Coloca o BatchSize DEFAULT  
If ::oCmdFind['batchSize'] = NIL
	::oCmdFind['batchSize'] := QRY_BATCHSIZE
Endif

// Seta o database para rodar o comando 
// Usa o database da conexão
::oCmdFind['$db'] := ::oMongoDB:cDatabase

// Aplica os filtros caso setados 
If ::oJsonFilter != NIL
	::oCmdFind['filter'] := ::oJsonFilter
Endif

// Aplica a ordem , caso setada 
If ::oJsonOrder != NIL 
	::oCmdFind['sort'] := ::oJsonOrder
Endif

// Incrementa o contador de requisiçoes de Query / Cursor
__QryID++

// Guarda o identificador da requisição
::nQryID 	:= __QryID                 

// Executa o find() no MongoDB
::oResponse := ::oMongoDB:RunCommand('find',::oCmdFind,::nQryID)

If ::oResponse['ok'] == 1

	::cCursorID :=  ::oResponse['cursor']['id']

	If ::cCursorID = '#INT64_0000000000000000'
		// Nao tem mais cursor, acabou os dados
		::cCursorID := ''
	Endif

	// Recupera os registros da requisição de abertura... 
	::aRecords    := ::oResponse['cursor']['firstBatch']
	::iRecPos     := 1
	
	If len(::aRecords) > 0 
		::lEOF        := .F.
    Else
   		::lEOF        := .T.
	Endif                                                             `
	
	Return .T. 
	
Endif

conout(::oResponse:ToJson())

nErrCode := ::oResponse['code']
cCodeName := ::oResponse['codeName']
cErrMsg := ::oResponse['errmsg']

conout('ERROR CODE  .... '+cValToChar(nErrCode))
conout('ERROR CODENAME . '+cValToChar(cCodeName))
conout('ERROR MESSAGE .. '+cValToChar(cErrMsg))

UserException("ZMONGOQUERY::OPEN ERROR")

Return .F.

// ----------------------------------------
// Retorna o Objeto JSON atual do cursor
// REtorna NIL Caso a Query nao esteja aberta ou em EOF()

METHOD GetObject() CLASS ZMONGOQUERY
IF ::lEOF
	Return NIL
Endif
Return ::aRecords[::iRecPos]

// ----------------------------------------
// Retorna .T. se os dados do cursor acabaram 

METHOD Eof() CLASS ZMONGOQUERY
Return ::lEOF

// ----------------------------------------
// Posiciona no proximo registro do cursor
// Caso nao existam mais registros no cache, verifica se 
// os dados nao acabaram e pede mais para o banco 

METHOD Skip() CLASS ZMONGOQUERY

IF ::lEOF
	Return .F. 
Endif

::iRecPos++

IF ::iRecPos > len(::aRecords)
   
	::iRecPos := 1
	
	If !empty(::cCursorID)

		// Monta a requisição para mais dados	

		IF ::oCmdGetMore = NIL 
			::oCmdGetMore := JSONOBJECT():new()
			::oCmdGetMore['getMore']    := ::cCursorID
			::oCmdGetMore['collection'] := ::cTable
			::oCmdGetMore['batchSize']  := ::oCmdFind['batchSize']
    	Endif
        
		IF ::oResponse != NIL 
			FreeObj(::oResponse)
		Endif
				
		::oResponse := ::oMongoDB:RunCommand('getMore',::oCmdGetMore,::nQryID)

		If ::oResponse['ok'] == 0 

			conout(::oResponse:ToJson())
			
			nErrCode := ::oResponse['code']
			cCodeName := ::oResponse['codeName']
			cErrMsg := ::oResponse['errmsg']
			
			conout('ERROR CODE  .... '+cValToChar(nErrCode))
			conout('ERROR CODENAME . '+cValToChar(cCodeName))
			conout('ERROR MESSAGE .. '+cValToChar(cErrMsg))
			
			UserException("ZMONGOQUERY::SKIP ERROR")
			
		Endif

		::cCursorID := ::oResponse['cursor']['id']
		
		If ::cCursorID = '#INT64_0000000000000000'
			// Nao tem mais cursor, acabou os dados
			::cCursorID := ''
		Endif
		
		::aRecords  := ::oResponse['cursor']['nextBatch']

		IF len(::aRecords) > 0 
			::lEOF := .F. 
		Else			
			::lEOF := .T. 
			::aRecords := {}
		Endif

	Else
	
		::lEOF := .T. 
		::aRecords := {}
		
	Endif

Endif

Return  !::lEOF


METHOD Close() CLASS ZMONGOQUERY
Local oCmdKill

IF !::lEOF
	conout("WARNING - ZMONGOQUERY CLOSED BEFORE END")
Endif                                              

If !empty(::cCursorID)
	
	// Se o cursor ainda tem dados, fecha ele no Banco de Dado
	// db.runCommand( { killCursors: "restaurants", cursors: [ NumberLong("18314637080") ] } )

	oCmdKill := JsonObject():New()
	oCmdKill['killCursors']    := ::cTable
	oCmdKill['cursors']    := { ::cCursorID } 
	
	::oResponse := ::oMongoDB:RunCommand('killCursors',oCmdKill,::nQryID)
	conout(::oResponse:ToJSON())		
	
	FreeObj(oCmdKill)
	FreeObj(::oResponse)
	::cCursorID := ''
	::aRecords   := {}
	::lEOF       := .T.
	
Endif

Return
