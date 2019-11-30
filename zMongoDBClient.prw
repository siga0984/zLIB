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

// Classe de Conexao SSL ainda nao implementada
// #define SSL_SUPPORT

/* ======================================================

Mongo DB Client API 

*** CLASSE AINDA NAO TERMINADA -- FALTA MUITA COISA *** 

===================================================== */

CLASS ZMONGODBCLIENT FROM LONGNAMECLASS

   DATA cServer
   DATA nPort
   DATA cDatabase
   DATA oSocket
   DATA lVerbose
   DATA lSecure
   DATA cUserName    
   DATA cUserPsw
   DATA cAuthDB  
   DATA oIsMasterInfo
   
   METHOD New()
   METHOD Connect()
   METHOD Disconnect()
   METHOD SetDB()
   METHOD SetVerbose()
   METHOD SetSecure()
   METHOD SetUserName()
   METHOD SetUSerPsw()
   METHOD SetAuthDB()
   METHOD AuthScramSha1()
   
   METHOD RunCommand()

ENDCLASS


// ----------------------------------------

METHOD New() CLASS ZMONGODBCLIENT 
::lVerbose := .F.
::lSecure  := .F.
::cDatabase := 'db'
::cUserName := ''
::cUserPsw  := ''
::cAuthDB   := ''
Return self

// ----------------------------------------
METHOD SetUserName(cUsr) CLASS ZMONGODBCLIENT 
::cUserName := cUsr
Return

// ----------------------------------------

METHOD SetUSerPsw(cPsw) CLASS ZMONGODBCLIENT 
::cUserPsw := cPsw
Return

// ----------------------------------------

METHOD SetAuthDB(cDb) CLASS ZMONGODBCLIENT 
::cAuthDb := cDB
Return

// ----------------------------------------

METHOD Connect(cServer,nPort) CLASS ZMONGODBCLIENT 
Local iStat
Local oIsMaster
Local oResponse
Local nI , aAuthM
Local lOk := .T.

If ::oSocket != NIL 
	UserException("ZMONGODBCLIENT ALREADY CONNECTED") 
Endif

If empty(cServer)
	::cServer := 'localhost'
Else
	::cServer := cServer
Endif

If empty(nPort)
	::nPort := 	27017
Else
	::nPort := 	nPort
Endif

If ::lSecure
	::oSocket   := tSSLClient():New()
Else
	::oSocket   := tSocketClient():New()
Endif

// Abre uma conexão TCP com o MongoDB
iStat := ::oSocket:Connect( ::nPort , ::cServer , 1000 )

IF iStat < 0 
	UserException("ZMONGODBCLIENT - Connection ERROR ("+cValToChar(iStat)+")") 
	freeobj(::oSocket)
	Return .F.	
Endif

// Primeira etapa da conexão -- enviar um isMaster com os detalhes de quem está conectando 

oIsMaster := JSONOBJECT():new()
oIsMaster['isMaster'] := 1

If !empty(::cUserName) 
	If Empty(::cAuthDB)
		oIsMaster['saslSupportedMechs'] := ::cDatabase + '.' + ::cUserName
	Else
		oIsMaster['saslSupportedMechs'] := ::cAuthDB + '.' + ::cUserName
	Endif
Endif

oIsMaster['client'] := JSONOBJECT():new()
oIsMaster['client']['application'] := JSONOBJECT():new()
oIsMaster['client']['application']['name'] := 'TOTVS Application Server'

oIsMaster['client']['driver'] := JSONOBJECT():new()
oIsMaster['client']['driver']['name'] := 'AdvPL'
oIsMaster['client']['driver']['version'] := '1.1.1'

oIsMaster['client']['os'] := JSONOBJECT():new()
oIsMaster['client']['os']['type'] := 'Windows'
oIsMaster['client']['os']['name'] := 'Microsoft Windows 10'
oIsMaster['client']['os']['architecture'] := 'x86_64'
oIsMaster['client']['os']['version'] := '10.0 (build 18362)'

// Submete o comando
oResponse := ::RunCommand('isMaster',oIsMaster)

If oResponse['ok'] == 1

	// Guarda as informações retornadas 
	::oIsMasterInfo := oResponse        

	/*
	{ 
	  "ismaster":true,
	  "maxMessageSizeBytes":48000000,
	  "logicalSessionTimeoutMinutes":30,
	  "maxWriteBatchSize":100000,
	  "localTime":"#UTC64_33BEDEBA6E010000",
	  "connectionId":35,
	  "maxWireVersion":8,
	  "saslSupportedMechs":["SCRAM-SHA-256","SCRAM-SHA-1"],
	  "maxBsonObjectSize":16777216,
	  "readOnly":false,
	  "ok":1,
	  "minWireVersion":0
	}
	*/

	// Se foi informado usuario, deve chegar a informação dos métodos 
	// de autenticaçao suportados. 
	aAuthM := oResponse['saslSupportedMechs']
	
	If !Empty(aAuthM)
		
		// Se foi retornada alugma informação de autenticação, 
		// eu informei usuario para autentica e pedi os metodos
		// disponives. Varre o retorno procurando por um metodo implemetado

		For nI := 1 to len(aAuthM)
			IF aAuthM[nI] == 'SCRAM-SHA-1'
				lOk := ::AuthScramSha1()
			Endif
		Next

    Endif

Else

	conout('ERROR : '+oResponse:ToJson())
	lOk := .F.

Endif

IF !lOk
	::Disconnect()
Endif

Return lOk


// ----------------------------------------

METHOD Disconnect() CLASS ZMONGODBCLIENT 
If ::oSocket != NIL 
	::oSocket:CloseConnection()
	Freeobj(::oSocket)
Endif
Return

// ----------------------------------------

METHOD SetDB(cDB) CLASS ZMONGODBCLIENT 
::cDatabase := cDB
Return

// ----------------------------------------

METHOD SetVerbose(lSet) CLASS ZMONGODBCLIENT 
::lVerbose := lSet
Return

// ----------------------------------------

METHOD SetSecure(lSet) CLASS ZMONGODBCLIENT 
#ifdef SSL_SUPPORT
	::lSecure := lSet
#else
	If lSet
		UserException("ZMONGODBCLIENT -- SSL SUPPORT NOT AVAILABLE")
	Endif	
#endif
Return

// ----------------------------------------
// Execuçao de um comando no MongoDB 
// Recebe o nome do comando -- que deve ser o primeiro nó do BSON 
// E o Objeto JSON com os dados do comando a ser executado 
// Recebe opcionalmente o ID de request e Flags...

METHOD RunCommand(cCommand,oJsonCmd,iReqID,iReqFlags) CLASS ZMONGODBCLIENT 
Local cSection := ''
Local cSendCmd := '' , nSend
Local cRecvMsg := '' , nRecv
Local oResponse
Local nSecType

If ::oSocket = NIL 
	UserException("ZMONGODBCLIENT NOT CONNECTED") 
Endif

IF empty(::cDatabase)
	UserException("ZMONGODBCLIENT - DATABASE NOT SET") 
Endif

If iReqID = NIL
	// Request ID Default = 0 
	iReqID := 0 
Endif

IF iReqFlags = NIL
	// Request Flags default = 0 
	iReqFlags := 0
Endif

// Insere o $db -- necessário para executar os comandos
If oJsonCmd['$db'] = NIL 
	oJsonCmd['$db'] := ::cDatabase  
Endif

// Cria uma seção para uma requisição ( Section 0 ) 
// Usa um objeto JSON gerado e converte para BSON 
// o comando deve ser o primeiro node do BSON 
cSection := chr(0) + JTOBSON( oJsonCmd , cCommand )

// Cria a mensagem com a requisição para o MongoFB
cSendCmd += L2Bin( len(cSection) + (5*4) ) // Tamanho total da requisição, Header + Flags + Seção 0 
cSendCmd += L2Bin(iReqID)    // Request ID, pode ser zero 
cSendCmd += L2Bin(0)         // REsponeId , usado apenas pelo Mongo na resposta 
cSendCmd += L2Bin(2013)      // OpCode para envio de comando por mensagem 
cSendCmd += L2Bin(iReqFlags) // Flags 
cSendCmd += cSection         // Seção 0 com o comando a ser executado 

// Manda pro MongoDB 
nSend := ::oSocket:Send(cSendCmd)   

// Mostra no console os dados enviados
IF ::lVerbose       
	conout('')
	conout("ZMONGODBCLIENT::RunCommand("+cCommand+") : "+oJsonCmd:ToJson())
	conout("ZMONGODBCLIENT::RunCommand | Sent Bytes ["+cValToChar(nSend)+"]")
	conout(STR2HexDmp(cSendCmd))
Endif

// Agora verifica o resultado 
// Espera até 2 segundos pelo resutado 

nRecv := ::oSocket:Receive(@cRecvMsg,2000)

IF ::lVerbose
	conout("ZMONGODBCLIENT | Recv Bytes      ["+cValToChar(nRecv)+"]")
	conout(STR2HexDmp(cRecvMsg))
Endif

// Agora vamos tratar a resposta                                            r
// o MondoGB responde também com um OP_MSG 

iRetLen    := bin2l( substr(cRecvMsg,01,4) )
iRetId     := bin2l( substr(cRecvMsg,05,4) )
iRetTo     := bin2l( substr(cRecvMsg,09,4) )
iRetCode   := bin2l( substr(cRecvMsg,13,4) )
iRetFlags  := bin2l( substr(cRecvMsg,17,4) )

// Isola a(s) seção(ões) de resposta 
cRetSections := substr(cRecvMsg,21)

nSecType := ASC(left(cRetSections,1))

IF ::lVerbose
	conout("Repsonse Length ... "+cValToChar(iRetLen))
	conout("Request ID ........ "+cValToChar(iRetId))
	conout("ResponseTo ........ "+cValToChar(iRetTo))
	conout("Return Op Code..... "+cValToChar(iRetCode))
	conout("Return Flags ...... "+cValToChar(iRetFlags))
	conout('Section Type ...... '+cValToChaR(nSecType))
Endif

If nSecType == 0 

	// Seção tipo 0 = um documento BSON de resposta 
    // Transforma o BSON em um objeto JSON 
    
	oResponse := BSONTOJ(substr(cRetSections,2))

	IF ::lVerbose
		conout('JSON Response ..... '+oResponse:ToJsOn())
		conout('')
    Endif
	
Else 

	// Retornou Mais de uma seção ?! 
	// Opa ... 
	
	UserException("ZMONGODBCLIENT:RunCommand() -- MULTIPLE SECTIONS NOT IMPLEMENTED")

Endif

Return oResponse

// ----------------------------------------

METHOD AuthScramSha1() CLASS ZMONGODBCLIENT 
Local oAuth , oResponse
Local lDone 
Local cPayLoad  := '' 
Local nCvId, aTmp, nI 
Local cBuff := ''
Local cSalt := ''
Local cSrvNonce := ''
Local nInteract := 0 
Local cAuthMsg := ''
Local cCliNonce := ''
Local cCliProf := ''
Local cSaltedPsw := ''
Local cHashedPsw := ''
Local cCliKey := ''
Local cStoredKey := ''

// Cria um Client Nonce ( 24 bytes ) 
// Ja encodado em Base64
cCliNonce := NewNonce()

// Salva a primeira parte da mensagem de autenticacao 
cAuthMsg := 'n='+::cUserName+',r='+cCliNonce

oAuth := JSONOBJECT():new()
oAuth['saslStart'] := 1
oAuth['mechanism'] := 'SCRAM-SHA-1'
oAuth['payload'] := '#BIN00_' + 'n,,'+cAuthMsg

// Primeira etapa do Handshake de autenticação 
// Envia o nome do usuario e o Client "nonce"
oResponse := ::RunCommand('saslStart',oAuth)

IF oResponse['ok'] == 1

	nCvId := oResponse['conversationId']
	lDone := oResponse['done']
	cPayLoad := oResponse['payload']
	cPayLoad := substr(cPayLoad,8)

Else

	conout('ERROR : '+oResponse:ToJson())
	UserException("AUTHSCRAMSHA1 - STEP 1 ERROR")

Endif

// Segunda etapa, recebe o Server "nonce" apos o client Nonce, 
// recebe o SALT para usar com a senha 

// Acrescenta a resposta do servidor para gerar a "Client Proof"
cAuthMsg +=  ',' + cPayLoad

// Desmonta o payload para pegar os parametros

aTmp := StrTokarr2(cPayLoad,',')
For nI := 1 to len(aTmp)
	cBuff := aTmp[nI]
	IF left(cBuff,2) == 'r='
		cSrvNonce := substr(cBuff,3)
	ElseIF left(cBuff,2) == 's='
		cSalt := decode64(substr(cBuff,3))
	ElseIf left(cBuff,2) == 'i='
		nInteract  := val(substr(cBuff,3))
	Endif
Next

// TODO 
// Implementar um cache de SALT, para evitar recalcular 
// o Salted Password a cada conexão 
// O MongoDB gera o mesmo SALT por um periodo de tempo 

// Hash sobre o usuario e a senha 
cHashedPsw := MD5(  ::cUserName + ":mongo:" + ::cUserPsw )

// Aplica o Salt informado sobre o Hash 
cSaltedPsw := SaltPsw( cHashedPsw , cSalt , nInteract)

// Gera o Client Key
cCliKey := HMAC( "Client Key", cSaltedPsw ,  3 , 1  ) // SHA1, RAW

// Gera o Stored Key 
cStoredKey := SHA1(cCliKey,1)
                                       
// Acrescenta mais uma parte do retorno na mensagem de autenticacao 
cAuthMsg += ',c=biws,r='
cAuthMsg += cSrvNonce

// Cria a Client Signature
cClientSig := HMAC(  cAuthMsg , cStoredKey ,  3 , 1 ) // SHA1  RAW

// Monta o Client Proof sobre a Client Key e a Client Signature
For nI := 1 to len( cCliKey )
	cCliPRof += chr(  NXOR(  asc(substr(cCliKey,nI,1)) , asc(substr(cClientSig,nI,1)) )  )
Next  

FreeObj(oAuth)

// Envia a Client PRoof para o Mongo DB Validar

oAuth := JSONOBJECT():new()
oAuth['saslContinue'] := 1
oAuth['conversationId'] := nCvId
oAuth['payload'] := '#BIN00_' + 'c=biws,r='+cSrvNonce+',p='+encode64(cCliPRof)	

// Submete o comando
oResponse := ::RunCommand('saslContinue',oAuth)

// TODO  : Ver se o código de verificação retornado está OK 

oAuth := JSONOBJECT():new()
oAuth['saslContinue'] := 1
oAuth['conversationId'] := nCvId
oAuth['payload'] := '#BIN00_' 

// Submete o comando
oResponse := ::RunCommand('saslContinue',oAuth)

// {"conversationId":1,"done":true,"payload":"#BIN00_","ok":1}

IF oResponse['ok'] == 1
	Return .T. 
Endif

conout('ERROR : '+oResponse:ToJson())
UserException("AUTHSCRAMSHA1 - STEP 2 ERROR")

Return .F. 


// ----------------------------------------
// Funcão auxiliar JTOBSON
// Retorna uma string binária BSON representando o Objeto JsonObject usado como parâmetro
// Recebe o nome do primeiro nó no BSON a ser gerado 
// Caso nao seja especificado, gera na ordem que o GetNames() devolver 
// Tratamento para tipos nativos do BSON representados como STring em AdvPL 
// #INT64_<hex8>       -- Inteiro 64 bits, 8 bytes codificados em hexadecimal
// #OBJID_<hex12>      -- Object ID, 12 bytes codificados em hexadecimal 
// #UTC64_<hex8>       -- UTC Datetime (int64) , 8 bytes codificados em hexadecimal
// #TSU64_<hex8>       -- TimeStamp (uint64)  , 8 bytes codificados em hexadecimal
// #JSSTR_<javascript> -- JavaSCript Code como String
// ----------------------------------------

STATIC Function JTOBSON(oJson, cFirst)
Local cBson := ''
Local nI , nJ , oTmp, aTmp, cTmp
Local cName , xValue, cValType     
Local aNames := oJson:GetNames()
Local nPos

If cFirst != NIL
	nPos := ascan(aNames,cFirst)
	If nPos > 1
		cTmp := aNames[1]
		aNames[1] := cFirst
		aNames[nPos] := cTmp
	Endif 	
Endif

For nI := 1 to len(aNames)

	cName := aNames[nI]
	xValue := oJson[aNames[nI]]
	cValType := valtype(xValue)

	If cValType == 'C'   

		// Reverter tratamentos da BSONTOJ
		
		If left(xValue,7) == '#OBJID_'
			cBson += chr(7) 
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
		ElseIf left(xValue,7) == '#UTC64_'
			cBson += chr(9) 
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
		ElseIf left(xValue,7) == '#JSSTR_'
			cBson += chr(13)
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			cBson += l2bin( len(xValue)+1 )
			cBson += xValue
			cBson += chr(0)
		ElseIf left(xValue,7) == '#TSU64_'
			cBson += chr(17)
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
		ElseIf left(xValue,7) == '#INT64_'
			cBson += chr(18) 
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
		ElseIf left(xValue,7) == '#BIN00_'
			cBson += chr(5) // Binary subtype 0 - generic
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			cBson += l2bin( len(xValue) )
			cBson += chr(0)
			cBson += xValue
		Else
			// String mesmo...
			cBson += chr(02) // BSON UTF8 String
			cBson += ( cName + chr(0) )
			cBson += l2bin( len(xValue)+1 )
			cBson += xValue
			cBson += chr(0)
		Endif

	ElseIf cValType == 'N'

		IF ( xValue - Int(xValue)) != 0 
			// Numero com decimais, 
			cBson += chr(1) // BSON 64-bit binary floating point
			cBson += ( cName + chr(0) )
			cBson += d2bin( xValue )
		Else              
			// Inteiro 32 bits 
			cBson += chr(16) // BSON int32
			cBson += ( cName + chr(0) )
			cBson += l2bin( xValue )
		Endif

	ElseIf cValType == 'L'

		cBson += chr(8) // BSON Boolean
		cBson += ( cName + chr(0) )
		If 	xValue
			cBson += chr(1)
		Else
			cBson += chr(0)
		Endif

	ElseIf cValType == 'D' // Data AdvPL por enquanto vira string AAAAMMDD

		cBson += chr(02) // BSON UTF8 String
		cBson += ( cName + chr(0) )
		cBson += l2bin( 9 )
		cBson += dtos(xValue)
		cBson += chr(0)

	ElseIf cValType == 'U'

		cBson += chr(10) // BSON NULL Value 
		cBson += ( cName + chr(0) )

	ElseIf cValType == 'J'

		// Outro JSON Object 
		cBson += chr(03) // BSON embedded document
		cBson += ( cName + chr(0) )
		cBson += JTOBSON( xValue )

	ElseIf cValType == 'A'

		// Array ...
		// Quebra em um documento onde cada propriedade 
		// é um elemento do array, iniciando em '0'
		cBson += chr(04) // BSON array
		cBson += ( cName + chr(0) )
		oTmp := JSONOBJECT():New()
		aTmp := {}
		For nJ := 1 to len(xValue) 
			oTmp[ cValTochar(nJ-1) ] := xValue[nJ]
			aadd(aTmp,cValTochar(nJ-1))
		Next            
		cBson += JTOBSON(oTmp,aTmp)
		oTmp := NIL               
		aTmp := NIL 

	Else

		USerException("JTOBSON ERROR - UNSUPPORTED Argument Type ["+cValType+"]")

	Endif

Next

// Monta / Envelopa a string BSON final  : 
// 4 bytes ( total size ), o conteúdo e o terminador ( ASCII 0  )

Return l2bin( len(cBson) + 5 ) + cBson + chr(0)


// ----------------------------------------
// Função auxiliar BSONTOJ
// Recebe uma string BSON e retorna um objeto JSON correspondente
// Tipos nativos do BSON sem suporte direto em AdvPL viram string com prefixo diferenciado:
// #INT64_<hex8>       -- Inteiro 64 bits, 8 bytes codificados em hexadecimal
// #OBJID_<hex12>      -- Object ID, 12 bytes codificados em hexadecimal 
// #UTC64_<hex8>       -- UTC Datetime (int64) , 8 bytes codificados em hexadecimal
// #TSU64_<hex8>       -- TimeStamp (uint64)  , 8 bytes codificados em hexadecimal
// #JSSTR_<javascript> -- JavaSCript Code como String
// ----------------------------------------


STATIC Function BSONTOJ(cBSON)
Local oRet := JsonObject():new()
Local nSize := bin2l( substr(cBSON,1,4) ) // Tamanho do objeto JSON
Local cSaved , oChild
Local cName , cValue, iSize
Local nI , aNames

IF nSize < 5
	conout("Unexpected BSON size "+cValTochar(nSize))
	return NIL
ElseIF nSize == 5
	// JSON vazio ... 
	// So tem o tamanho ( 4 bytes ) mais o ASCII 0
	cBSON := substr(cBSON,6)
	return NIL 
Endif

// Salva o que tem pra frente ..
cSaved := substr(cBSON,nSize+1)

// Remove o tamanho do objeto e pega apenas o que interessa
cBSON := substr(cBSON,5,nSize-5)

While len(cBSON) > 0

	// Pega o tipo do elemento 
	nElement := asc( substr(cBSON,1,1)) 
    
	// Remove o tipo do buffer
	cBSON := substr(cBSON,2)  
	
	// Recupera o nome do elemento, removendo do buffer
	cName := GetCString(@cBSON)

	If nElement == 1 
		
		// 	"\x01" e_name double	64-bit binary floating point
		// Converte para valor numérico direto em AdvPL 
		
		oRet[cName] := bin2d( substr(cBSON,1,8) )
		cBSON := substr(cBSON,9)
	
	ElseIf nElement == 2 

		// "\x02" e_name string	UTF-8 string
		// Recupera a string no BSON 
		
		cValue := GetBString(@cBSON)
		oRet[cName] := cValue                   
		
	ElseIF nElement == 3 
	                     
		// "\x03" e_name document	Embedded document
		// Um objeto BSON dentro de outro ...
				
		oChild := BSONTOJ(@cBSON)
		oRet[cName] := oChild
	
	ElseIf nElement == 4
        
		// "\x04" e_name document	Array
		
		oChild := BSONTOJ(@cBSON)
		oRet[cName] := { }

		If oChild != NIL      

			// Notação de array em BSON
			// objeto json com propriedades numericas ordenadas
			// Varre as propriedades e acrescenta
			
			aNames := oChild:GetNames()
			For nI := 1 to len(aNames) 
				aadd( oRet[cName] , oChild[aNames[nI]] ) 
			Next
			
		Endif

	ElseIf nElement == 5 
	
		// "\x05" e_name binary	-- Binary data
		// binary	::=	int32 subtype (byte*)	Binary - The int32 is the number of bytes in the (byte*).
		// Implementar pelo menos o subtipo 00 - generic

		iSize := bin2l( substr(cBSON,1,4) )		
		if ASC(substr(cBSON,5,1)) == 0
			cBSON := substr(cBSON,6)
			oRet[cName] := '#BIN00_'+substr(cBSON,1,iSize)
			cBSON := substr(cBSON,iSize+1)
		Else
			UserException("BSONTOJ ERROR -- BINARY DATA -- SUBTYPE "+cValToChar(ASC(substr(cBSON,5,1))) +" -- NOT IMPEMENTED")
		Endif
		
	
	ElseIF nElement == 6
	
		// 0x06 Undefined (value) — Deprecated
		// Por compatibilidade gera um valor NIL no JSON 
		oRet[cName] := NIL 

	ElseIf nElement == 7 

		// "\x07" e_name (byte*12)	ObjectId
		// Recuepera tring prefixada em hexadecimal 	
		oRet[cName] := '#OBJID_'+Str2Hex(substr(cBSON,1,12))
		cBSON := substr(cBSON,13)

	ElseIf nElement == 8

		// "\x08" e_name "\x00"	Boolean "false"
		// "\x08" e_name "\x01"	Boolean "true"

		If asc( substr(cBSON,1,1) ) == 1
			oRet[cName] := .T.
		Else
			oRet[cName] := .F.
		Endif	
		cBSON := substr(cBSON,2)

	ElseIf nElement == 9 

		// 	"\x09" e_name int64	UTC datetime
		// -- por hora string prefixada em hexadecimal

		oRet[cName] := '#UTC64_'+Str2Hex(substr(cBSON,1,8))
		cBSON := substr(cBSON,9)

	ElseIF nElement == 10
	
		// "\x0A" e_name	Null value
		
		oRet[cName] := NIL 

	ElseIf nElement == 11 

		// "\x0B" e_name cstring cstring
		// Regular expression - The first cstring is the regex pattern, the second is the regex options string. 
		// Options are identified by characters, which must be stored in alphabetical order. 
		// Valid options are 'i' for case insensitive matching, 'm' for multiline matching, 'x' for verbose mode, 
		// 'l' to make \w, \W, etc. locale dependent, 's' for dotall mode ('.' matches everything), 
		// and 'u' to make \w, \W, etc. match unicode.
		 
		UserException("BSONTOJ ERROR -- REGEXP DATA NOT IMPEMENTED")
		 
	ElseIF nElement == 12 
	
		// "\x0C" e_name string (byte*12)	DBPointer — Deprecated
		UserException("BSONTOJ ERROR -- DBPOINTER DATA DEPRECATED | NOT IMPEMENTED")

	ElseIf nElement == 13 
	
		// 	"\x0D" e_name string	JavaScript code		

		cValue := GetBString(@cBSON)
		oRet[cName] := '#JSSTR_'+cValue                   

	ElseIf nElement == 14
	
		// "\x0E" e_name string	Symbol. Deprecated
		UserException("BSONTOJ ERROR -- SYMBOL DATA DEPRECATED | NOT IMPEMENTED")
		
	ElseIf nElement == 15
	
		// "\x0F" e_name code_w_s	JavaScript code w/ scope
		UserException("BSONTOJ ERROR -- JAVASCRIPT CODE W/ SCOPE NOT IMPEMENTED")

	ElseIf nElement == 16 
        
        // "\x10" e_name int32	32-bit integer
	    // Converte para valor numérico em AdvPL

		oRet[cName] := bin2l( substr(cBSON,1,4) )
		cBSON := substr(cBSON,5)
		
	ElseIF nElement == 17
	
		// "\x11" e_name uint64	Timestamp
		
		oRet[cName] := '#TSU64_'+Str2Hex(substr(cBSON,1,8))
		cBSON := substr(cBSON,9)
		
	ElseIf nElement == 18 
	                      
		// "\x12" e_name int64	64-bit integer
		// -- por hora string prefixada em hexadecimal

		oRet[cName] := '#INT64_'+Str2Hex(substr(cBSON,1,8))
		cBSON := substr(cBSON,9)

	ElseIF nElement == 19 
	
		// 	"\x13" e_name decimal128	128-bit decimal floating point
		UserException("BSONTOJ ERROR -- 128-BIT DECIMAL FLOATING POINT NOT IMPEMENTED")

	Else

		USerException("Element ["+cName+"] Type 0x"+DEC2HEX(nElement)+" UNKNOW / NOT IMPLEMENTED")

	Endif

Enddo
         
// Restaura o que ainda tem pra frente
cBSON := cSaved

Return oRet

// ----------------------------------------
// Recupera o nome de um elemento do BSON
// C String, terminada com ASCII 0 

STATIC Function GetCString(cBSON)
Local cRet := ''
Local nPos := 1 
Local cChar := substr(cBSON,nPos,1)

// Le o nome do elemento do BSON 
While asc(cChar) > 0 
	cRet +=  cChar
	nPos++
	cChar := substr(cBSON,nPos,1)
Enddo

// Remove o nome do elemento do BSON
cBSON := substr(cBSON,nPos+1)

Return cRet

// ----------------------------------------
// Recupera uma string do BSON 
// Os primeiros 4 bytes = tamanho total ( size64 + payload + ascii 0 ) 

STATIC Function GetBString(cBSON)
Local nSize, cRet

// Pega o tamanho da string
nSize := bin2l(substr(cBSON,1,4))

// Recupera a string desconsiderando o ultimo ASCII 0 
cRet := substr(cBSON,5,nSize-1) 

// Corta fora a string recuperada
// inclusive o ASCII 0 final 
cBSON := substr(cBSON,nSize+5)

Return cRet 

// Cria um Client "nonce" de 24 bytes
// Já retorna encodado em Base64
STATIC Function NewNonce()
Local cNonce := ''
While len(cNonce) < 24
	cNonce += chr(randomize(0,256))
Enddo
Return Encode64(cNonce)


/* ---------------------------------------------------------------
"Salga" o Hashed Password com o SALT que o MongoDB retornou 
--------------------------------------------------------------- */

STATIC Function SaltPsw( cHashedPsw , cSaltStr , nInt)
Local cStartKey, cOutput, aOutPut, nI , nJ , cIntermed
Local cSaltedPdw := ''

// Monta o Salt de 20 bytes 
// 16 bytes do Salt, mais "0001"
cStartKey := cSaltStr+chr(0)+chr(0)+chr(0)+chr(1)

// Primeiro Hash
cOutput := HMAC(  cStartKey , cHashedPsw , 3 , 1  ) // SHA1, RAW

// Começa a montar o retorno 
aOutPut := {}
For nI := 1 to len(cOutput)
	aadd(aOutPut,asc(substr(cOutput,nI,1)))
Next

// Salva o primeiro hash como base para as interações
cIntermed := cOutput

// Aplica o Hash pelo numero de interações informado
For nI := 2 to nInt
	cIntermed := HMAC( cIntermed, cHashedPsw ,  3 , 1  ) // SHA1, RAW
	For nJ := 1 to len(cIntermed)
		aOutPut[nJ]	:= NXOR( aOutPut[nJ] , asc(substr(cIntermed,nJ,1)) )
	Next
Next

// Monta o Salted Password
For nI := 1 to len(aOutPut)
	cSaltedPdw += chr(aOutPut[nI])
Next

Return cSaltedPdw

