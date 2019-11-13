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



#include 'Protheus.ch'
#include 'zLib.ch'

/* ===================================================================

Classe     ZSHIELDRPC
Autor      Julio Wittwer
Data       02/2019
Descricao  Encapsula a classe ZRPC com tratamento de erro 

Reimplementa o CallProc e CallProcEX para rodar a função chamada 
com encapsulamento para retorno de erro remoto. Em caso de erro 
remoto, apos o retorno é feita a desconexao -- este RPC nao deve 
mais ser utilizado ... Na conexao, ele troca informações entre ele e 
o server RPC , para que um registro de erro no server possa identificar 
a origem da conexão 

=================================================================== */

// Variaveis para guardar no lado RPC Server os dados do 
// Client e do Server RPC 

STATIC _aCliInfo
STATIC _aSrvInfo

// Chasse client de RPC com encapsulamento de execução 

CLASS ZSHIELDRPC FROM ZRPC

  DATA aClientInfo
  DATA aServerInfo

  METHOD New(cIp,nPort,cEnv)  // Construtor
  METHOD Connect()
  METHOD CallProc()           // Executa uma função no servidor remoto -- Síncrona
  METHOD CallProcEX()         // Executa uma função no servidor remoto -- Síncrona
  
  METHOD GetServerInfo()      // Informacoes do servidor RPC 
  METHOD GetClientInfo()      // Informacoes do cliente RRC 

ENDCLASS

// -------------------------------------------------------------------
// Construtor do Objeto ZSHIELDRPC 
 
METHOD NEW(cIp,nPort,cEnv)  CLASS ZSHIELDRPC
_Super:New(cIp,nPort,cEnv)
::aClientInfo := {}
::aServerInfo := {}
Return self

// -------------------------------------------------------------------
// Coenxao do Objeto ZSHIELDRPC 
// Manda algumas informações do processo client do RPC 
// para guardar no lado server da conexão 
METHOD Connect() CLASS ZSHIELDRPC
Local lOk := _Super:Connect()

If lOk
	If !_Super:Callproc("FindFunction","U_ZRPCCALLER")
	    ::_SetError(-6,"ZSHIELDRPC not supported on RPC Server")
		_Super:Disconnect()	
		lOk := .F. 
	Endif
Endif

If lOk
	
	// Minhas informacoes na hora da conexao 
	::aClientInfo := _RPCInfo()
	
	// Manda as minhas informacoes ao Server 
	// e recupera as informações dele 
	::aServerInfo := _Super:Callproc("U_ZRPCCALLER",::aClientInfo)

	conout(padc(' RPC Server Info Received ',79,'-'))
	aEval(::aServerInfo,{|x| conout(cValToChar(x))})
	conout(replicate('-',79))
	conout('')

Endif
Return lOk

// -------------------------------------------------------------------
// Realiza uma chamada de processo no servidor remoto 
// A chamada é sempre síncrona -- aguarda retorno 
// Somente podem ser chamadas funções que não acessam a Interface ( SmartClient ) 

METHOD CALLPROC(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZSHIELDRPC
Local xRet
Local nParms := pCount()-1

::_ClearError()

If !::bIsConnected
    ::_SetError(-1,"RPC NOT CONNECTED")
	Return NIL
Endif

if nParms == 0 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn)
ElseIf nParms == 1 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1)
ElseIf nParms == 2 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2)
ElseIf nParms == 3 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3)
ElseIf nParms == 4 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4)
ElseIf nParms == 5 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5)
ElseIf nParms == 6 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6)
ElseIf nParms == 7 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7)
ElseIf nParms == 8 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8)
ElseIf nParms == 9 
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9)
ElseIf nParms = 10  
	xRet := ::oRpcConn:CallProc("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
Else
    ::_SetError(-2,"EXCEEDED NUMBER OF PARAMETERS : MAX = 10")
	Return NIL
Endif

If valtype(xRet) != "A"
    ::_SetError(-3,"Unexpected RPCShield Return Type ("+valtype(xRet)+")")
	Return NIL 
Endif

If !xRet[1]
	// Em caso de falha, seta o erro com os detalhes 
	// Desconecta e retorna NIL 
	::_SetError(-4,xRet[2])
	::Disconnect()
	Return NIL 
Endif

// Em caso de sucesso, repassa o retorno 
Return xRet[2]

// -------------------------------------------------------------------
// Realiza uma chamada de processo no servidor remoto 
// Utiliza internamente CALLPROCEX()

METHOD CALLPROCEX(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZSHIELDRPC
Local xRet
Local nParms := pCount()-1

::_ClearError()

If !::bIsConnected
    ::_SetError(-1,"RPC NOT CONNECTED")
	Return NIL
Endif

if nParms == 0 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn)
ElseIf nParms == 1 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1)
ElseIf nParms == 2 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2)
ElseIf nParms == 3 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3)
ElseIf nParms == 4 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4)
ElseIf nParms == 5 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5)
ElseIf nParms == 6 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6)
ElseIf nParms == 7 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7)
ElseIf nParms == 8 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8)
ElseIf nParms == 9 
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9)
ElseIf nParms = 10
	xRet := ::oRpcConn:CallProcEx("U_RPCShield",cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
Else
    ::_SetError(-2,"EXCEEDED NUMBER OF PARAMETERS : MAX = 10")
	Return NIL
Endif

If valtype(xRet) != "A"
	// Retorno inesperado ? 
	// Desconecta e retorna NIL 
    ::_SetError(-3,"Unexpected RPCShield Return Type ("+valtype(xRet)+")")
	::Disconnect()
	Return NIL 
Endif

If !xRet[1]
	// Em caso de falha, seta o erro com os detalhes 
	// Desconecta e retorna NIL 
	::_SetError(-4,xRet[2])
	::Disconnect()
	Return NIL 
Endif

// Em caso de sucesso, repassa o retorno 
Return xRet[2]


// -------------------------------------------------------------------

METHOD GetServerInfo()  CLASS ZSHIELDRPC
Return aClone( ::aServerInfo )

// -------------------------------------------------------------------

METHOD GetClientInfo()  CLASS ZSHIELDRPC
Return aClone( ::aClientInfo )

/* ---------------------------------------------------------
Funcao de encapsulamento de execução remota com recuperacao 
de informações de erro 
----------------------------------------------------------*/

User Function RPCShield(cFnRun,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
Local aRet := {}
Local oOldBLock 
Local xRet , cMsg
Local nParms := pCount()-1

IF !FindFunction(cFnRun)

	// Funcao nao encontrada no RPO 
	cMsg := "[RPC] Function does not exits - " + cFnRun
	Aadd(aRet,.F.)
	Aadd(aRet , cMsg )
	
Else
	
	// Roda protegido por bloco de erro 
	oOldBLock := Errorblock( { |oError| ZRPCErr(oError,@aRet) } )
	
	BEGIN SEQUENCE
	
	If nParms = 0
		xRet := &cFnRun.()
	ElseIf nParms = 1
		xRet := &cFnRun.(p1)
	ElseIf nParms = 2
		xRet := &cFnRun.(p1,p2)
	ElseIf nParms = 3
		xRet := &cFnRun.(p1,p2,p3)
	ElseIf nParms = 4
		xRet := &cFnRun.(p1,p2,p3,p4)
	ElseIf nParms = 5
		xRet := &cFnRun.(p1,p2,p3,p4,p5)
	ElseIf nParms = 6
		xRet := &cFnRun.(p1,p2,p3,p4,p5,p6)
	ElseIf nParms = 7
		xRet := &cFnRun.(p1,p2,p3,p4,p5,p6,p7)
	ElseIf nParms = 8
		xRet := &cFnRun.(p1,p2,p3,p4,p5,p6,p7,p8)
	ElseIf nParms = 9
		xRet := &cFnRun.(p1,p2,p3,p4,p5,p6,p7,p8,p9)
	ElseIf nParms = 10
		xRet := &cFnRun.(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
	Endif
	
	// Deu tudo certo, acrescenta retorno com sucesso
	Aadd(aRet,.T.)
	Aadd(aRet , xRet )
	
	END SEQUENCE
	ErrorBlock(oOldBLock)
	
Endif

Return aRet


/* -------------------------------------------
Tratamento de erro para RPC 
Recupera a propriedade ERRORSTACK, que contém a descrição 
e a pilha de chamadas do erro 
------------------------------------------- */

STATIC Function ZRPCErr(oError,aRet)
Local cError := oError:ERRORSTACK

// Se o erro comeca com quebra de linha, remove 
While left(cError,1) == chr(13)
	cError := Substr(cError,2)
Enddo
While left(cError,1) == chr(10)
	cError := Substr(cError,2)
Enddo

// Acrescentar detalhes do contexto atual no erro 
// e do contexto client do erro obtidas na conexao 

cError += CRLF 
cError += padc(" RPC Server Info ",40,'-') + CRLF  
aEval(_aSrvInfo, {|x| cError += x + CRLF  }  )

cError += CRLF 
cError += padc(" RPC Client Info ",40,'-') + CRLF  
aEval(_aCliInfo, {|x| cError += x + CRLF  }  )

// Alimenta Retormo com informações de erro 
aRet := { .F. , cError }

// Registra o erro no log de console
// TODO -- Criar evento de log de erro 
conout( CRLF + padc(" RPC SERVER ERROR ",79,"*") + CRLF + ;
        cError + CRLF + ;
        replicate("*",79) + CRLF )

BREAK

Return NIL

// Recebe informacoes do cliente deste RPC 
// e devolve as informacoes do meu ambiente 
USER Function ZRPCCALLER(aInfo)

// Guarda as informacoes de quem chamou 
_aCliInfo := aClone(aInfo)

conout(padc(' RPC Client Info Received ',79,'-'))
aEval(aInfo,{|x| conout(cValToChar(x))})
conout(replicate('-',79))
conout('')

// Retorna as minhas informacoes 
// e guarda no ambiente do RPC Server
_aSrvInfo := _RPCInfo()

Return aClone(_aSrvInfo)

// Monta array com informacoes basicas do contexto atual 
// Usado nas pontas server e client no momento da conexao 

STATIC Function _RPCInfo()
Local aInfo := {}
aadd(aInfo,"Build: "   + GetBuild())           // Build deste servidor
aadd(aInfo,"Release: " + GetSrvVersion())      // Versao novo formato (x.x.x.x)
aadd(aInfo,"Hostname: "+_HostName())            // HostName (configurado na seção General)
aadd(aInfo,"Port: "+cValToChar(GetPort(1)))   // Porta TCP do Application Server
aadd(aInfo,"Environment: "+getenvserver())     // Environment
aadd(aInfo,"Thread: "+cValToChar(ThreadID()))   // Minha thread
aadd(aInfo,"Platform: "+_SrvType())            // Plataforma do Protheus Server
aadd(aInfo,"ConnDate: "+dtos(date()))          // Data da conexao
aadd(aInfo,"ConnTime: "+time())			       // Hora da conexao
Return aInfo


// Plataforma do Protheus Server 
STATIC Function _SrvType()
Local cInfo := ''
If IsSrvUnix()
	cInfo += 'Linux'
Else
	cInfo += 'Windows'
Endif
If IsSrv64()
	cInfo += ' 64 Bits'
Else
	cInfo += ' 32 Bits'
Endif
Return cInfo

// Nome do host onde este serviço esta sendo executado 
// Deve ser configurado explicitamente no INI
STATIC Function _HostName()
Local cHost
cHost := GetPVProfString("GENERAL","HOSTNAME","",GETSRVININAME())
Return cHost


