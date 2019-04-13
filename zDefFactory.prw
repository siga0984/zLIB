#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

				Factory de Definições 

	criado e armazenado como um objeto do ZENV()

====================================================== */

CLASS ZDEFFACTORY FROM LONGNAMECLASS

  DATA oLogger         // Objeto de log 
  DATA aObjects
  DATA aRPODefs

  METHOD NEW()
  METHOD GetaDefs()
  METHOD GetNewDef()
  METHOD Done()

ENDCLASS 


// ------------------------------------------------------
//

METHOD NEW() CLASS ZDEFFACTORY

::oLogger := ZLOGGER():New("ZDEFFACTORY")
::oLogger:Write("NEW","Create Definition Factory")

// Popula array de strings com as classes compiladas no RPO 
// que herdam a classe ZTABLEDEF

::aRpoDefs := ChdClsArr("ZTABLEDEF")

// Array de controle de instancias 
::aObjects := {}

Return self

// ------------------------------------------------------
// Retorna array com os nomes das classes 

METHOD GetaDefs() CLASS ZDEFFACTORY

::oLogger:Write("GETADEFS","Retrieve Definition Class List")

Return ::aRpoDefs


// ------------------------------------------------------
// Retorna um objeto com a instancia montada da classe

METHOD GetNewDef(cDefName,p1,p2,p3,p4,p5)  CLASS ZDEFFACTORY
Local oRet
Local bBlock
Local cBlock

::oLogger:Write("GetNewDef","Create Definition ["+cDefName+"]")

// Monta codeblock para criar instancia dinamicamente 
cBlock := "{|p1,p2,p3,p4,p5| "+cDefName+"():New(p1,p2,p3,p4,p5) }"
bBlock := &(cBlock)
oRet := Eval(bBlock,p1,p2,p3,p4,p5)

// Limpa codeblock para quebrar referencias 
bBlock := NIL

// Guarda a referencia deste objeto 
aadd(::aObjects,oRet)

Return oRet

// ------------------------------------------------------

METHOD Done() CLASS ZDEFFACTORY
Local nI, oObj
::oLogger:Write("Done")

For nI := 1 to len(::aObjects)
	oObj := ::aObjects[nI]
	If oObj != NIL \
		oObj:Done()
		 FreeObj(oObj)
	Endif
Next
aSize(::aObjects,0)

Return .T. 


// ===================================================================

User function clstst()
Local nI

// Retorna array de strings contendo o nome dos fontes que tem classe
// ax := GETCLSARRAY('*') 

// REtorna array de strings contendo as classes 
// compiladas no RPO  que herdam a classe ZTABLEDEF
// -- No caso, todos os componentes -- 
ay := ChdClsArr( "ZTABLEDEF" )

For nI := 1 to len(ay)
	conout(ay[nI])
Next

return

