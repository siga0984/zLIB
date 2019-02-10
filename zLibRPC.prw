#include 'Protheus.ch'
#include 'zLib.ch'

/* ===================================================================

Classe     ZRPC
Autor      Julio Wittwer
Data       09/2018
Descricao  Encapsula um objeto TRPC() do AdvPL 

Observacoes

- Nao tem proteção de execução ou tratamento de erro nativos
  :: É responsabilidade da função de criação de ambiente do lado 
  :: server do RPC montar uma proteção 
- Retorno direto do CallProc() / CallProcEX()
- Conecta com Connect(),descocecta com Disconnect() 
- Usa a versao Básica de classes AdvPL
- Limite de passagem de até 10 parâmetros

[TODO] Implementar modo verbose / Logger

=================================================================== */

CLASS ZRPC FROM LONGNAMECLASS

  DATA oRPCConn          // Objeto AdvPL TRPC
  DATA cRPCServer        // IP ou host do servidor remoto 
  DATA nRPCPort          // Porta do servidor remoto 
  DATA cRPCEnv           // Ambiente do Servidor Remoto 
  DATA bIsConnected      // Flag indicador de conexão 
  DATA lVerbose          // Flag de modo verbose
  
  DATA nLastErr          // Numero do ultimo erro ocorrido 
  DATA cLastErr          // Striong com ultimo erro ocorrido 

  METHOD New(cIp,nPort,cEnv)  // Construtor
  METHOD Connect()            // Estabelece conexao com o servidor remoto 
  METHOD Disconnect()         // Desconecta do servidor remoto 
  METHOD IsConnected()        // Retorna .T. caso esteja conectado ao servidor remoto 
  METHOD CallProc()           // Executa uma função no servidor remoto -- Síncrona
  METHOD CallProcEX()         // Executa uma função no servidor remoto -- Síncrona
  METHOD GetError(cMsg)       // Retorna ultimo erro ocorrigo 
  METHOD Destroy()            // Limpa as propriedades do objeto da memoria 
  METHOD Verbose(lSet)        // Seta ou consulta modo de trabalho verbose -- echo em console 

  /****** METODOS DE USO PRIVADO ********/
  
  METHOD _SetError(nErr,cErr) // Seta ocorrencia de erro 
  METHOD _ClearError()        // Limpa ultima ocorrencia de erro 

ENDCLASS

// -------------------------------------------------------------------
// Construtor do Objeto ZRPC 
// cIP e nPort sao obrigatorios. 
// Se o envionment não for informado, usa o nome do ambiente atual
 
METHOD NEW(cIp,nPort,cEnv)  CLASS ZRPC

::cRPCServer   := cIP
::nRPCPort     := nPort
::bIsConnected := .F.
::lVerbose     := .T. 

If empty(cEnv)
	::cRPCEnv := GetEnvServer()  
Else
	::cRPCEnv := cEnv
Endif

// Cria o objeto da conexao usando a classe de RPC do AdvPL 
::oRPCConn := TRPC():New( ::cRPCEnv )

// Reseta informações de erro 
::nLastErr := 0
::cLastErr := ''

Return self

// -------------------------------------------------------------------
// Estabelece conexão RPC com o servidor remoto 

METHOD Connect() CLASS ZRPC
::_ClearError()
If ::bIsConnected
	Return .T.
Endif
::bIsConnected := ::oRpcConn:Connect( ::cRPCServer,::nRpcPort )
Return ::bIsConnected

// -------------------------------------------------------------------
// Desconecta do servidor remoto 

METHOD Disconnect() CLASS ZRPC
If ::bIsConnected
	::oRpcConn:Disconnect()
	::bIsConnected := .F.
Endif
Return

// -------------------------------------------------------------------
// Retorna .T. caso a conexão com o servidor remoto 
// já tenha sido realizada .

METHOD IsConnected() CLASS ZRPC
Return ::bIsConnected

// -------------------------------------------------------------------
// Realiza uma chamada de processo no servidor remoto 
// A chamada é sempre síncrona -- agiarda retorno 
// Somente podem ser chamadas funções que não acessam a Interface ( SmartClient ) 

METHOD CALLPROC(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZRPC
Local xRet
Local nParms := pCount()-1

::_ClearError()

If !::bIsConnected
    ::_SetError(-1,"RPC NOT CONNECTED")
	Return NIL
Endif

if nParms == 0 
	xRet := ::oRpcConn:CallProc(cFn)
ElseIf nParms == 1 
	xRet := ::oRpcConn:CallProc(cFn,p1)
ElseIf nParms == 2 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2)
ElseIf nParms == 3 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3)
ElseIf nParms == 4 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4)
ElseIf nParms == 5 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5)
ElseIf nParms == 6 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5,p6)
ElseIf nParms == 7 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5,p6,p7)
ElseIf nParms == 8 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5,p6,p7,p8)
ElseIf nParms == 9 
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9)
ElseIf nParms = 10  
	xRet := ::oRpcConn:CallProc(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
Else
    ::_SetError(-2,"EXCEEDED NUMBER OF PARAMETERS : MAX = 10")
	Return NIL
Endif

Return xRet

// -------------------------------------------------------------------
// Realiza uma chamada de processo no servidor remoto 
// Utiliza internamente CALLPROCEX()

METHOD CALLPROCEX(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZRPC
Local xRet
Local nParms := pCount()-1

::_ClearError()

If !::bIsConnected
    ::_SetError(-1,"RPC NOT CONNECTED")
	Return NIL
Endif

if nParms == 0 
	xRet := ::oRpcConn:CallProcEx(cFn)
ElseIf nParms == 1 
	xRet := ::oRpcConn:CallProcEx(cFn,p1)
ElseIf nParms == 2 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2)
ElseIf nParms == 3 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3)
ElseIf nParms == 4 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4)
ElseIf nParms == 5 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5)
ElseIf nParms == 6 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5,p6)
ElseIf nParms == 7 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5,p6,p7)
ElseIf nParms == 8 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5,p6,p7,p8)
ElseIf nParms == 9 
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9)
ElseIf nParms = 10
	xRet := ::oRpcConn:CallProcEx(cFn,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
Else
    ::_SetError(-2,"EXCEEDED NUMBER OF PARAMETERS : MAX = 10")
	Return NIL
Endif

Return xRet

// -------------------------------------------------------------------
// Recupera o ultimo erro registrado pela classe ZRPC

METHOD GETERROR(cMsg) CLASS ZRPC
cMsg := ::cLastErr
Return ::nLastErr

// -------------------------------------------------------------------
// Limpa o objeto -- deve ser chamada antes de destruir o objeto 

METHOD DESTROY() CLASS ZRPC
::_ClearError()
::Disconnect()
FreeObj(oRPCConn)
Return

// -------------------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Seta o ultimo erro ocorrido 

METHOD _SetError(nErr,cErr) CLASS ZRPC
::cLastErr := cErr
::nLastErr := nErr
Return

// -------------------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa registro de ultimo erro 

METHOD _ClearError() CLASS ZRPC
::cLastErr := ''
::nLastErr := 0
Return

// -------------------------------------------------------------------
// Permite ligar ou desligar o modo "verbose"
// de operação com echo em console

METHOD Verbose(lSet) CLASS ZRPC
Local lOldSet := ::lVerbose
If pCount() > 0 
	::lVerbose := lSet
Endif
Return lOldSet

