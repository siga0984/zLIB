#include "protheus.ch"
#include "zLibStr2HexDmp.ch"
#include "zLibVarStream.ch"

/* ----------------------------------------------------------------------------

Classe      zMemCached
Autor       Júlio Wittwer
Data        01/2019
Descrição   API Client para o aplicativo MEMCACHED 
Versão      1.01 

Suporta armazenar e recuperar variáveis AdvPL do cache 
dos tipos "C" Caractere, "N" Numérico, "D" Data, "L" Lógico , 
e array contento todos estes tipos, inclusive "NIL"

---------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------

Blog con instrucoes sobre MEMCACHED para Windows 

https://commaster.net/content/installing-memcached-windows

Links de Download

http://downloads.northscale.com/memcached-1.4.5-x86.zip
http://downloads.northscale.com/memcached-1.4.5-amd64.zip

Referencias do protocolo texto

https://github.com/bfad/Lasso-Memcached-Connector/blob/master/protocol.txt
https://docs.oracle.com/cd/E17952_01/mysql-5.0-en/ha-memcached-interfaces-protocol.html

---------------------------------------------------------------------------- */

CLASS ZMEMCACHED FROM LONGNAMECLASS
 
   DATA cMemCacheIP     // IP da instancia Memcached
   DATA nMemCachePort   // Porta da instancia Memcached
   DATA nRvcTimeOut	    // Timeout de recebimento em milissegundos ( default 1000 ) 
   DATA oTCPConn        // Objeto Socket Client
   DATA cError		    // Ultimo erro da API 
   DATA cResponse	    // Response header da ultima requisicao
   DATA lVerbose        // Modo verbose de operação 
   
   METHOD New()         // Construtor da classe
   METHOD Connect()     // Estabelece conexão com o MemCached
   METHOD GetVersion()  // Recupera a versao do MemCached
   METHOD GetStats()    // Recupera estatisticas da intancia do memcached
   METHOD Disconnect()  // Desconecta do MemCAched
   METHOD IsConnected() // Retorna .T. caso o cache esteja conectado 

   METHOD GetErrorStr() // Recupera string com informações de erro 

   METHOD Add()         // Acrescenta uma chave / valor ( apenas caso nao exista ) 
   METHOD Replace()     // Troca o valor de uma chave existente
   METHOD Set()         // Armazena uma chave / valor no MemCached 
   METHOD Get()         // Recupera o valor de uma chave armazeanda 
   METHOD Delete()      // Remove do cache um valor pela chave 
   METHOD Increment()   // Incrementa um contador pela chave -- valor em string numerica
   METHOD Decrement()   // Decrementa um contador pela chave -- valor em string numerica
   METHOD Flush()       // Limpa todas as variáveis do cache 
   

   // ********* METODOS DE USO INTERNO *********

   METHOD _Store()
   METHOD _GetTCPError()
  
ENDCLASS


// ----------------------------------------------------------
// Constroi o objeto principal do client, ja recebe aqui IP e Porta de conexao

Method NEW( cIp , nPorta ) CLASS ZMEMCACHED
::cMemCacheIP    := cIp
::nMemCachePort  := nPorta
::nRvcTimeOut := 1000
::oTCPConn       := tSocketClient():New()
::cError      := ''
::cResponse   := ''
::lVerbose    := .F.
Return self

// ----------------------------------------------------------
// Estabelece a conexao com a instancia do memcached

METHOD Connect() CLASS ZMEMCACHED
Local iStat

::cError := ''
::cResponse := ''

IF ::lVerbose
	Conout("zMemCached:Connect() to "+::cMemCacheIP+" Port "+cValToChar(::nMemCachePort))
Endif

If ::oTCPConn:Isconnected()
	::cError := "Memcached client already connected."
	Return .F.
Endif

// Estabelece a conexao com o memcache DB
iStat := ::oTCPConn:Connect( ::nMemCachePort , ::cMemCacheIP, 100 )

If iStat != 0
	::cError := "Memcached connection Error ("+cValToChar(iStat)+")"
	::_GetTCPError()
	Return .F.
Endif

Return .T.

// ----------------------------------------------------------
// Fecha a conexao e finaliza operacoes com a instancia

METHOD Disconnect() CLASS ZMEMCACHED
Local cSendCmd := 'quit'+CRLF
Local nSend

::cError := ''
::cResponse := ''

If ::oTCPConn == NIL
	::cError := "Memcached client already Done."
	Return .F.
Endif

if( ::oTCPConn:IsConnected() )
	// Se ainda está conectado, manda um "quit"
	// para fechar a conexao de modo elegante
	IF ::lVerbose
		Conout("zMemCached:DONE() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
		Conout(Str2HexDmp(cSendCmd))
	Endif
	nSend := ::oTCPConn:Send( cSendCmd )

	If nSend <= 0 
		::cError := "Memcached client SEND Error."
		::_GetTCPError()
		Return .F.
	Endif
	
Endif

::oTCPConn:CloseConnection()
::oTCPConn := NIL

Return .T.

// ----------------------------------------------------------
// Verifica se o objeto está conectado ao MemCached

METHOD IsConnected() CLASS ZMEMCACHED
If ::oTCPConn != NIL 
	Return ::oTCPConn:Isconnected()
Endif
Return .F. 
   
// ----------------------------------------------------------
// Recupera a versao da instancia conectada por referencia

METHOD GetVersion( cVersion ) CLASS ZMEMCACHED
Local nRecv, cRecvBuff := ''
Local cSendCmd := "version" + CRLF
Local nSend

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:GetVersion() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Receive Error"
	::_GetTCPError()
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:GetVersion() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

If Left(cRecvBuff,8)!='VERSION '
	::cError := "Response Error : " + cRecvBuff
	Return .F.
Endif

// Recupera a versão por referencia 
cVersion := ::cResponse

Return .T.

// ----------------------------------------------------------
/// Recupera estatisticas globais  da instancia

METHOD GetStats( aStats ) CLASS ZMEMCACHED
Local nRecv, cRecvBuff := ''
Local nI , nT , aTmp
Local cSendCmd := "stats" + CRLF
Local nSend

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:GetStats() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Receive stats error"
	::_GetTCPError()
	Return .F.
Endif

If nRecv == 0
	::cError := "Receive stats time-out
	::_GetTCPError()
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:GetStats() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

// Recupera estatisticas
aTmp := strtokarr2( strtran(cRecvBuff,CRLF,chr(10)) , chr(10) )

nT := Len(aTmp)
For nI := 1 to nT
	If Left(aTmp[nI],5)=='STAT '
		aadd(aStats , substr(aTmp[nI],6) )
	Endif
Next

// Limpa o array temporario
aSize(aTmp,0)

Return .T.

// ----------------------------------------------------------
// Guarda um valor no memcache
// Mode = set, add, replace, append, prepend
// cas ainda nao implementado 

METHOD _Store( cMode, cKey , xValue, nOptFlag, nOptExpires ) CLASS ZMEMCACHED
Local cSendCmd := ''
Local nRecv
Local cRecvBuff := ''
Local nSend
Local cBuffer

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

If !( ('.'+cMode+'.') $ ('.set.add.replace.append.prepend.cas.') )
	::cError := "Invalid Store mode ["+cMode+"]"
	Return .F.
Endif

// Tratamento de valores a armazenar
If valtype(xValue) == 'C'
	// Valores em String grava no formato original 
	// Para inclusive permitir Increment() e Decrement() 
	// em string numérica
	cBuffer := xValue
Else
	// Demais valores sao codificados em Binary String 
	Var2BinStr(xValue,cBuffer)
Endif

// <mode> <key> <flags> <exptime> <bytes>
// ------------------------------------------
cSendCmd += cMode + ' '
cSendCmd += cKey + ' '
If nOptFlag == NIL
	cSendCmd += '0 '
else
	cSendCmd += cValToChar(nOptFlag)+' '
Endif
If nOptExpires == NIL
	cSendCmd += '0 '
else
	cSendCmd += cValToChar(nOptExpires)+' '
Endif
cSendCmd += cValToChar(len(cBuffer))
cSendCmd += CRLF
// ------------------------------------------

IF ::lVerbose
	Conout("zMemCached:_Store() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

// Etapa 01 Envia o comando 
nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif


// Etapa 02
// Envia o valor a ser armazenado 

nSend := ::oTCPConn:Send(cBuffer+CRLF)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

If ::lVerbose
	Conout("zMemCached:Store("+cMode+") SEND VALUE ")
	Conout(Str2HexDmp(cBuffer+CRLF))
Endif

// Se tudo der certo, aqui eu devo receber um "stored"
nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Store("+cMode+") failed - connection error" + cValTochar(nRecv)
	::_GetTCPError()
	Return .F.
Endif

If nRecv == 0
	::cError := "Store("+cMode+") failed - response time-out"
	::_GetTCPError()
	Return .F.
Endif

If ::lVerbose
	Conout("zMemCached:Store("+cMode+") RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

cRecvBuff := strtran(cRecvBuff,CRLF,'')

If cRecvBuff != 'STORED'
	::cError := "Store ["+cMode+"] failed: "+cRecvBuff
	Return .F.
Endif

Return .T.


// ----------------------------------------------------------
// Deleta uma chave da instancia conectada                                            

METHOD Delete( cKey ) CLASS ZMEMCACHED
Local cSendCmd 
Local nRecv
Local cRecvBuff := ''
Local nSend

::cError    := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

cSendCmd := 'delete ' + cKey + CRLF

// ------------------------------------------

If ::lVerbose
	Conout("zMemCached:Delete() SEND")
	Conout(Str2HexDmp(cSendCmd))
Endif

// Manda o comando 
nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

// Se tudo der certo, aqui eu devo receber DELETED 
nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

// Pega apenas a primeira linha do resultado 
::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Delete() failed - connection error" + cValTochar(nRecv)
	::_GetTCPError()
	Return .F.
Endif

If nRecv == 0
	::cError := "Delete() failed - response time-out"
	::_GetTCPError()
	Return .F.
Endif

If ::lVerbose
	Conout("zMemCached:Delete() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

cRecvBuff := strtran(cRecvBuff,CRLF,'')

If cRecvBuff != 'DELETED'
	::cError := "Delete failed - Error: "+cRecvBuff
	Return .F.
Endif

Return .T.

// Todos os comandos de STORE tem a mesma sintaxe 
// Logo, serão tratados dentro do mesmo metodo interno 
// que recebe como parametro a instrução de armazenamento 

METHOD Add( cKey , xValue, nOptExpires ) CLASS ZMEMCACHED
Return ::_Store("add", cKey , xValue, NIL, nOptExpires)

METHOD Replace( cKey , xValue, nOptExpires ) CLASS ZMEMCACHED
Return ::_Store("replace", cKey , xValue, NIL, nOptExpires)

METHOD Set( cKey , xValue, nOptExpires ) CLASS ZMEMCACHED
Return ::_Store("set", cKey , xValue, NIL, nOptExpires)


// ----------------------------------------------------------
// Recupera o valor de uma chave por referencia em cBuffer
// Retorna .T. em caso de sucesso na operação 
// O valor recuperado é colocado por referencia em cBuffer

METHOD Get( cKey , xValue ) CLASS ZMEMCACHED

Local cSendCmd := ''
Local nRecv
Local cRecvBuff := ''
Local nPos
Local cLine
Local aTmp
Local cTeco
Local nSize
Local nSend

::cError := ''
::cResponse := ''

// Limpa o retorno por referencia 
xValue := NIL

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	return .F. 
Endif

// Monta o comando de recuperacao 
cSendCmd += 'get '+cKey + CRLF

If ::lVerbose
	Conout("zMemCached:Get() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

// Manda o comando
nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

// Se tudo der certo, aqui eu devo receber os dados ...
nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

If nRecv < 0
	::cError := "Get() failed - connection error" + cValTochar(nRecv)
	::_GetTCPError()
	return .F. 
Endif

If nRecv == 0
	::cError := "Get() failed - response time-out"
	::_GetTCPError()
	return .F. 
Endif

If ::lVerbose
	Conout("zMemCached:Get() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

// Parser do retorno

While !empty(cRecvBuff)
	
	// Primeiro pega a linha de status
	nPos := at(CRLF,cRecvBuff)
	If nPos < 1
		::cError := "Get() failed - missing CRLF"
		return .F. 
	Endif
	
	cLine := left(cRecvBuff,nPos-1)
	cRecvBuff := substr(cRecvBuff,nPos+2)
	
	If cLine == "END"
		// acabaram os dados
		// Sai do loop
		EXIT
	Endif
	
	If Left(cLine,6) == "VALUE "
		
		// Tem valor ... opa ... legal
		aTmp := strtokarr2(cLine,' ')
		
		// varinfo("aTmp",aTmp)
		// [1] "VALUE"
		// [2] <key>
		// [3] <flags>
		// [4] <size> 
		// [5] Optional [uniqueid]
		
		nSize := val(aTmp[4])
		
		While len(cRecvBuff) < nSize
			
			// Se ainda falta coisa pra receber, recebe mais um teco
			// e acrescenta no buffer
			cTeco := ''
			nRecv := ::oTCPConn:Receive(@cTeco,::nRvcTimeOut)
			
			If nRecv < 0
				::cError := "Get() failed - connection error" + cValTochar(nRecv)
				::_GetTCPError()
				return .F. 
			Endif
			
			If nRecv == 0
				::cError := "Get() failed - response time-out"
				::_GetTCPError()
				return .F. 
			Endif
			
			If ::lVerbose
				Conout("zMemCached:Get() RECV "+cValToChar(nRecv)+" Byte(s)")
				Conout(Str2HexDmp(cTeco))
			Endif
			
			// So acrescenta o que recebeu
			cRecvBuff += substr(cTeco,1,nRecv)
			
			If ::lVerbose
				Conout("zMemCached:Get() Total ReceivedBuffer ")
				Conout(Str2HexDmp(cRecvBuff))
			Endif
			
		Enddo
		
		// Valor ja foi recebido na integra
		// Coloca o valor recebido no retorno
		cBuffer := left(cRecvBuff,nSize) 
		
		// Arranca valor recebido do buffer
		// Ja desconsiderando o CRLF
		cRecvBuff := substr(cRecvBuff,nSize+3)
		
		// Limpa o array temporário
		aSize(aTmp,0)

		// Eu só espero recener um valor 		
		EXIT 
		
	Else
		
		// Se nao tem o valor, ou nao tem o "END", deu merda ?!
		::cError := "Get() failed - Unexpected Buffer ["+cLine+"]"
		return .F. 
		
	Endif
	
Enddo

If empty(cRecvBuff)
	// Se nao sobrou nada do buffer, 
	// A operação de GET foi feita com sucesso, 
	// Nao houve erro, apenas o valor nao foi encontrado. 
	Return .T. 
Endif


If left(cBuffer,4) == chr(1)+chr(0)+chr(0)+chr(0)
    // Se o valor tem a assinatura de uma String Binaria
    // converte ela para a variavel de tipo correspondente
	BinStr2Var( cBuffer , xValue )
Else
	// Senao , o valor recebido é String
	// e já está no buffer
	xValue := cBuffer
Endif

If left(cRecvBuff,5) == "END" + CHR(13)+Chr(10)
	// Depois do valor, eu espero um END (CRLF) 
	// Se nao chegou um END, tem mais de um valor na chave ? ....
	Return .T. 
Endif

::cError := "Get() failed - Unexpected Multiple Value(s)"
return .F. 

// ----------------------------------------------------------
// Incrementa um contador com registro numerico
// Permite informar o fator de incremento ( default = 1 )
//

Method Increment( cKey , nValue , nStep ) CLASS ZMEMCACHED

Local cSendCmd := ''
Local nRecv
Local cRecvBuff := ''     
Local nSend

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

// Monta o comando de recuperacao
cSendCmd += 'incr '+cKey+' '
If nStep == NIL
	cSendCmd += '1'
Else
	cSendCmd += cValToChar(nStep)
Endif

cSendCmd += CRLF 

If ::lVerbose
	Conout("zMemCached:Increment() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

// Manda o comando
nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

// Se tudo der certo, aqui eu devo receber o valor apos o incremento
nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Increment() FAILED - connection error" + cValTochar(nRecv)
	::_GetTCPError()
	Return .F.
Endif

If nRecv == 0
	::cError := "Increment() FAILED - response time-out"
	::_GetTCPError()
	Return .F.
Endif

If ::lVerbose
	Conout("zMemCached:Increment() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

// Parser do retorno
cRecvBuff := strtran(cRecvBuff,CRLF,'')

If !(left(cRecvBuff,1)$'0123456789')
	::cError := "Increment() FAILED - Unexpected Buffer "+cRecvBuff
	Return .F.
Endif

// Pega e retorna o valor apos o incremento
nValue := val(cRecvBuff)

Return .T.


// ----------------------------------------------------------
// Dencrementa um contador com registro numerico
// Permite informar o fator de incremento ( default = 1 )

Method Decrement( cKey , nValue , nStep ) CLASS ZMEMCACHED

Local cSendCmd := ''
Local cRecvBuff := ''
Local nRecv
Local nSend

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

// Monta o comando de recuperacao
cSendCmd += 'decr '+cKey+' '
If nStep == NIL
	cSendCmd += '1'
Else
	cSendCmd += cValToChar(nStep)
Endif

cSendCmd += CRLF

If ::lVerbose
	Conout("zMemCached:Decrement() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

// Manda o comando
nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

// Se tudo der certo, aqui eu devo receber o valor apos o decremento
nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv < 0
	::cError := "Decrement() FAILED - connection error" + cValTochar(nRecv)
	::_GetTCPError()
	Return .F.
Endif

If nRecv == 0
	::cError := "Decrement() FAILED - response time-out"
	::_GetTCPError()
	Return .F.
Endif

If ::lVerbose
	Conout("zMemCached:Decrement() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

// Parser do retorno

cRecvBuff := strtran(cRecvBuff,CRLF,'')

If !(left(cRecvBuff,1)$'0123456789')
	::cError := "Decrement() FAILED - Uexpected Buffer "+cRecvBuff
	Return .F.
Endif

// Pega e retorna o valor apos o decremento
nValue := val(cRecvBuff)

Return .T.


// ----------------------------------------------------------
// Apaga todos os registros de memoria da instancia

METHOD Flush() CLASS ZMEMCACHED
Local nRecv, cRecvBuff := ''
Local cSendCmd := "flush_all" + CRLF
Local nSend

::cError := ''
::cResponse := ''

If !::oTCPConn:Isconnected()
	::cError := "Memcached client not connected."
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:Flush() SEND "+cValToChar(len(cSendCmd))+" byte(s).")
	Conout(Str2HexDmp(cSendCmd))
Endif

nSend := ::oTCPConn:Send(cSendCmd)

If nSend <= 0 
	::cError := "Memcached client SEND Error."
	::_GetTCPError()
	Return .F.
Endif

nRecv := ::oTCPConn:Receive(@cRecvBuff,::nRvcTimeOut)

::cResponse := substr(cRecvBuff,1,at(CRLF,cRecvBuff)-1)

If nRecv == 0
	::cError := "Receive timed-out"
	::_GetTCPError()
	Return .F.
Endif

If nRecv < 0
	::cError := "Receive Error"
	::_GetTCPError()
	Return .F.
Endif

IF ::lVerbose
	Conout("zMemCached:Flush() RECV "+cValToChar(nRecv)+" Byte(s)")
	Conout(Str2HexDmp(cRecvBuff))
Endif

If Left(cRecvBuff,2)!='OK'
	::cError := "Response Error : " + cRecvBuff
	Return .F.
Endif

Return .T.


// ----------------------------------------------------------
// Acrescenta detalhes de erro TCP na propriedade ::cError

METHOD _GetTCPError() CLASS ZMEMCACHED
Local cTCPError := '' 
Local nTCPCode := ::oTCPConn:GetError(@cTCPError)

::cError += " (TCP ERROR "+cValToChar(nTCPCode)+" : "+cTCPError+" )"

Return

// ----------------------------------------------------------
// Recupera informações sobre o último erro ocorrido 

METHOD GetErrorStr()  CLASS ZMEMCACHED
Return ::cError


