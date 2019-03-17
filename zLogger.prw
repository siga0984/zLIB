#include 'protheus.ch'
#include 'zlib.ch' 


/* ======================================================

          Classe de LOG em modo console

Autor      Julio Wittwer
Data       01/2019   

Deve ser instanciada por componente, onde o identificador do componente é 
informado no construtor. O log em console pode conter a thread, data 
e horario. Cada chamada deve identificar a operação em execução 
e ter uma mensagem com detalhes. 

====================================================== */

CLASS ZLOGGER FROM LONGNAMECLASS

   DATA cComponent
   DATA lDateTime
   DATA lThread
   DATA lConsole
   DATA oLogWriter
   
   METHOD NEW(cComponent)
   METHOD WRITE(cRun,cMsg)
   METHOD SETWRITER()
   METHOD SETECHO()
   
ENDCLASS 


// ------------------------------------------------------
// Cria uma instancia de geração de log para uma classe ou componente

METHOD NEW(cComponent) CLASS ZLOGGER
::cComponent := cComponent
::lDateTime := .T. 
::lThread   := .T. 
::lConsole  := .F. 
::oLogWriter := NIL
Return self


// ------------------------------------------------------
// Permite associar ao log uma classe de escrita adicional 

METHOD SETWRITER(oWriter)  CLASS ZLOGGER
::oLogWriter := oWriter
Return

// ------------------------------------------------------
// Habilita ou desabilita o echo em console

METHOD SETECHO(lSet)  CLASS ZLOGGER
::lConsole  := lSet
Return


// ------------------------------------------------------
// Escreve algo no log 
// Requer a operação ou função, e a mensagem com os detalhes de execução 

METHOD WRITE(cRun,cMsg) CLASS ZLOGGER
Local cEcho := ''
Local nMS

If !::lConsole .AND. ::oLogWriter = NIL
	// Sem saida de console, sem escrita 
	// em nenhum objeto, nao faz nada 
	Return
Endif

If ::lDateTime 
	// Acrescenta data e hora, com milissegundos
	nMS := round( seconds()-int(seconds()) , 3 )  * 1000
	cEcho += "["+dtos(date())+"]["+time()+"."+StrZero(nMS,3)+"]"
Endif

If ::lThread   
	// Acrescenta Numero da Thread
	cEcho += "[Thread "+cValToChar(ThreadID())+"]"
Endif

// Acrescenta Componente 
cEcho += "["+::cComponent
cEcho += ":"
cEcho += cRun
cEcho += "] "

// Acrescenta o valor informado
cEcho += cValToChar(cMsg)

// Mostra o echo no log de console
If ::lConsole
	Conout(cEcho)
Endif

// SE eu tenho um objeto escritor de log relacionado
// Chamo o escritor para fazer as honras
If ::oLogWriter != NIL
	::oLogWriter:WRITE(cRun,cMsg)
Endif

Return 

