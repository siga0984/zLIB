#include  'Protheus.ch'
#include  'zLib.ch'

/* ======================================================

Classe de encapsulamento de acesso ao DBAccess 

Cria ou recupera do Pool uma conexão para uso. 
Usa contador de referencias para reaproveitar a conexao na aplicação 

Observação : Cada GetDBConn deve estar acompanhado do seu ReleaseDBConn
             para não furar o contador e a conexão ser desfeita 
             antes da hora, ou não ser desfeita apos o uso 
             
O objetivo desta classe, além de compartilhar conexões usando um POOL 
é fazer com que a aplicação apenas obtenha uma conexão quando 
seja realmente necessário, evitando conexões ociosas. 

====================================================== */

CLASS ZDBACCESS FROM LONGNAMECLASS

   DATA nTopConn      // Handler da conexao com o DBAccess
   DATA cDatabase     // Database da conexao ( MSSQL, ORACLE, MYSQL, etc ) 
   DATA cDbAlias      // Alias / DSN da Conexão 
   DATA cTopServer    // IP ou Host do DBACcess
   DATA nTopPort      // Porta do DBAccess
   DATA lUsePool      // Usa pool de conexão do Protheus ?
   DATA cPoolId       // Nome do identificador do POOL 
   DATA nRefs         // Contador de referências
   DATA cErrorStr     // Ultimo erro registrado 

   METHOD NEW()           // Construtor
   METHOD SETPOOL()       // LIga ou desliga o Pool 
   METHOD GETDBConn()     // Pega uma conexão para uso 
   METHOD RELEASEDBConn() // Devolve uma conexão 
   METHOD GetErrorStr()   // Retorna string com ultimo erro 

ENDCLASS

// ------------------------------------------------------
// Construtor 
// Recebe os dados para estabelecer a conexao com o DBACCESS
// Obrigatorio database e alias 
// Server e porta do dbaccess DEFAULT = localhost

METHOD NEW( cDatabase , cAlias, cTopServer, nTopPort) CLASS ZDBACCESS
::nTopConn   := -1
::cDatabase  := cDatabase
::cDbAlias   := cAlias
::cTopServer := "localhost"
::nTopPort   := 7890
::lUsePool   := .F. 
::cPoolId    := ''
::nRefs      := 0 

IF cTopServer != NIL
	::cTopServer := cTopServer
Endif
If nTopPort != NIL 
	::nTopPort   := nTopPort
Endif

Return

// ------------------------------------------------------
// Permite habilitar ou desabilitar o pool de conexões
// usando no Protheus a função TCGetPool e TCSetPool

METHOD SETPOOL(lSet,cPoolId) CLASS ZDBACCESS
::lUsePool   := lSet
::cPoolId    := cPoolId
Return

// ------------------------------------------------------
// Conecta ou recupera do Pool uma conexão com o DBAccess

METHOD GETDBConn() CLASS ZDBACCESS

::cErrorStr := ''

If ::nTopConn >= 0 

	// Já estou conectado, verifica se a conexão está OK 
	If !TCISCONNECTED(::nTopConn)
		UserException("*** DBACCESS CONNECTION LOST ***")
	Endif

	// Seta a conexcao como ativa e incrementa contador 
	TCSetConn(::nTopConn)
	::nRefs++
	Return .T. 

Endif

// Ainda nao estou conectado 
// Verifica se recupera conexao do pool ou cria nova 

IF ::lUsePool

	// Tenta recupera do POOL
	::nTopConn := TCGetPool(::cPoolId)
	
	If ::nTopConn >= 0 
		// Em caso de sucesso, 
		// Seta a conexcao como ativa e incrementa contador 
		conout("[ZDBACCESS] DBConnection from POOL - "+cValToChar(::nTopConn))
		TCSetConn(::nTopConn)
		::nRefs++
		Return .T. 
	Endif
	
Endif

// Se eu nao estou usando o pool, ou nao deu pra recuperar 
// uma conexão do pool ... Cria uma nova 

::nTopConn := TCLink(::cDatabase+"/"+::cDBAlias,::cTopServer,::nTopPort)
	
If ::nTopConn >= 0 
	// Em caso de sucesso, 
	// Seta a conexcao como ativa e incrementa contador 
	conout("[ZDBACCESS] New DBConnection Created - "+cValToChar(::nTopConn))
	TCSetConn(::nTopConn)
	::nRefs++
	Return .T.
Endif

::cErrorStr := "TCLINK ERROR ("+cValToChar(::nTopConn)+")"

Return .F. 

// ------------------------------------------------------
// Desconecta ou devolve ao Pool uma conexão com o DBAccess

METHOD ReleaseDBConn() CLASS ZDBACCESS

::cErrorStr := ''

IF ::nTopConn < 0
	// Já está desconectado
	Return
Endif

// Decrementa contador de referencias 
::nRefs--

If ::nRefs < 1

	// Não há mais uso da conexão, 
	// devolve ao POOL ou desconecta 
	
	If ::lUsePool
		
		conout("[ZDBACCESS] SEND DBConnection TO POOL - "+cValToChar(::nTopConn))
		tcSetConn(::nTopConn)
		xRet := TCSetPool(::cPoolId)
		conout(xRet)
		
	Else
		
		conout("[ZDBACCESS] UNLINK DBConnection - "+cValToChar(::nTopConn))
		tcUnlink(::nTopConn)
		
	Endif
	
	::nTopConn := -1
	
Endif

Return


METHOD GetErrorStr() CLASS ZDBACCESS
Return ::cErrorStr

