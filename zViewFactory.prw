#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

				Factory de Views 

	Criado e armazenado como um objeto do ZENV()

====================================================== */

CLASS ZVIEWFACTORY FROM LONGNAMECLASS

  DATA oLogger         // Objeto de log 
  DATA aObjects

  METHOD NEW()
  METHOD GetNewView()
  METHOD Done()

ENDCLASS 


// ------------------------------------------------------
//

METHOD NEW() CLASS ZVIEWFACTORY

::oLogger := ZLOGGER():New("ZVIEWFACTORY")
::oLogger:Write("NEW","Create View Factory")

// Array de controle de instancias 
::aObjects := {}

Return self

// ------------------------------------------------------
// Retorna um objeto com a instancia montada da View 

METHOD GetNewView(oDefObj,p1,p2,p3,p4,p5)  CLASS ZVIEWFACTORY
Local oRet
Local bBlock
Local cBlock

::oLogger:Write("GetNewView","Create View based on Definition ["+GetClassName(oDefObj)+"]")

// Monta codeblock para criar instancia dinamicamente 
cBlock := "{|oDef,p1,p2,p3,p4,p5| ZMVCVIEW():New(oDef,p1,p2,p3,p4,p5) }"
bBlock := &(cBlock)
oRet := Eval(bBlock,oDefObj, p1,p2,p3,p4,p5)

// Limpa codeblock para quebrar referencias 
bBlock := NIL

// Guarda a referencia deste objeto 
aadd(::aObjects,oRet)

Return oRet

// ------------------------------------------------------

METHOD Done() CLASS ZVIEWFACTORY
Local nI, oObj
::oLogger:Write("Done")

For nI := 1 to len(::aObjects)
	oObj := ::aObjects[nI]
	If oObj != NIL 
		oObj:Done()
		 FreeObj(oObj)
	Endif
Next
aSize(::aObjects,0)

Return .T. 


