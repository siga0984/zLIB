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
   
   METHOD New()
   METHOD Connect()
   METHOD Disconnect()
   METHOD SetDB()
   METHOD SetVerbose()
   
   METHOD MsgSend()

ENDCLASS


// ----------------------------------------

METHOD New() CLASS ZMONGODBCLIENT 
::lVerbose := .F.
Return self

// ----------------------------------------

METHOD Connect(cServer,nPort) CLASS ZMONGODBCLIENT 
Local iStat

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

::oSocket   := tSocketClient():New()

// Abre uma conexão TCP com o MongoDB
iStat := ::oSocket:Connect( ::nPort , ::cServer , 1000 )

IF iStat < 0 
	UserException("ZMONGODBCLIENT - Connection ERROR ("+cValToChar(iStat)+")") 
	freeobj(::oSocket)
	Return .F.	
Endif

Return .T.


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

METHOD MsgSend(oJsonCmd,aCmds,iReqID,iReqFlags) CLASS ZMONGODBCLIENT 
Local cSection := ''
Local cSendCmd := '' , nSend
Local cRecvMsg := '' , nRecv
Local oResponse

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

aadd(aCmds,'$db')

// Cria uma seção para uma requisição ( Section 0 ) 
// Usa um objeto JSON gerado e converte para BSON 
cSection := chr(0) + JTOBSON( oJsonCmd , aCmds )

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
	conout("ZMONGODBCLIENT | Sent Bytes      ["+cValToChar(nSend)+"]")
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

IF ::lVerbose
	conout("Repsonse Length ... "+cValToChar(iRetLen))
	conout("Request ID ........ "+cValToChar(iRetId))
	conout("ResponseTo ........ "+cValToChar(iRetTo))
	conout("Return Op Code..... "+cValToChar(iRetCode))
	conout("Return Flags ...... "+cValToChar(iRetFlags))
Endif

nSecType := ASC(left(cRetSections,1))

conout('Section Type ...... '+cValToChaR(nSecType))

If nSecType == 0 

	// Seção tipo 0 = um documento BSON de resposta 
	// Passa o BSON pra frente para gerar um objeto JSON 
	
    // Transforma o BSON em um objeto JSON 
	oResponse := BSONTOJ(substr(cRetSections,2))
	
Else 

	// Mais de uma seção 
	UserException("Multiple Sections not implemented")

Endif

Return oResponse


// ----------------------------------------
// Funcão auxiliar JTOBSON
// Retorna uma string binária BSON representando
// o Objeto JsonObject usado como parâmetro
// Recebe a lista de propriedades a considerar
// ----------------------------------------

STATIC Function JTOBSON(oJson , aNames )
Local cBson := ''
Local nI , nJ , oTmp, aTmp
Local cName , xValue, cValType     

If aNames = NIL
	aNames := oJson:GetNames()
Endif

For nI := 1 to len(aNames)

	cName := aNames[nI]
	xValue := oJson[aNames[nI]]
	cValType := valtype(xValue)

	If cValType == 'C'   

		// Reverter tratamentos da BSONTOJ
		
		If left(xValue,7) == '#INT64_'
			cBson += chr(18) // BSON INT64 - 8 bytes
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
		ElseIf left(xValue,7) == '#OBJID_'
			cBson += chr(7) // BSON Object ID - 12 bytes 
			cBson += ( cName + chr(0) )
			xValue := substr(xValue,8)
			For nJ := 1 to len(xValue) STEP 2 
				cBson += chr( __hextodec( substr(xValue,nJ,2)) )
			Next		
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
	ElseIf cValType == 'D' // Data por enquanto vira string AAAAMMDD
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

// Monta a string final BSON : 
// 4 bytes com o tamanho total, mais o conteúdo
// finaliza com ASCII 0 
Return l2bin( len(cBson) + 5 ) + cBson + chr(0)


// ----------------------------------------
// Função auxiliar BSONTOJ
// Recebe uma string BSON
// e retorna um objeto JSON correspondente

STATIC Function BSONTOJ(cBSON)
Local oRet := JsonObject():new()
Local nSize := bin2l( substr(cBSON,1,4) ) // Tamanho do objeto JSON
Local cSaved , oChild
Local cName , cValue
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

	If nElement == 1 // 	64-bit binary floating point

		oRet[cName] := bin2d( substr(cBSON,1,8) )
		cBSON := substr(cBSON,9)
	
	ElseIf nElement == 2 // BSON UTF8 String

		// Recupera a string no BSON 
		cValue := GetBString(@cBSON)

		oRet[cName] := cValue                   
		
	ElseIF nElement == 3 // Embedded document
	
		oChild := BSONTOJ(@cBSON)

		oRet[cName] := oChild
	
	ElseIf nElement == 4 // BSON ARRAY 

		oChild := BSONTOJ(@cBSON)
        
		// Iniciaiza com Array vazio 
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

	ElseIf nElement == 7 

		// 12 bytes Object ID
		// Por hora string em hexadecimal 	

		cObjId := substr(cBSON,1,12)
		cHexID := ''
		For nI := 1 to 12 
			cHexID += DEC2HEX(asc(substr(cObjId,nI,1)))
		Next		                        
		oRet[cName] := '#OBJID_'+cHexID
		cBSON := substr(cBSON,13)

	ElseIf nElement == 8 // Boolean

		If asc( substr(cBSON,1,1) ) == 1
			oRet[cName] := .T.
		Else
			oRet[cName] := .F.
		Endif	
		cBSON := substr(cBSON,2)

	ElseIf nElement == 16 // INT32

		oRet[cName] := bin2l( substr(cBSON,1,4) )
		cBSON := substr(cBSON,5)
		
	ElseIf nElement == 18 // INT64 -- por hora em hexadecimal
                       
		cINT64 := substr(cBSON,1,8)
		cHex64 := ''
		For nI := 1 to 8 
			cHex64 += DEC2HEX(asc(substr(cINT64,nI,1)))
		Next		                        
		oRet[cName] := '#INT64_'+cHex64
		
		cBSON := substr(cBSON,9)

	Else

		USerException("Element ["+cName+"] Type ["+cValToChar(nElement)+"] NOT IMPLEMENTED")

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



