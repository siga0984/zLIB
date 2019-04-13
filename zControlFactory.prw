#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

				Factory de Control

	Criado e armazenado como um objeto do ZENV()

====================================================== */

CLASS ZCONTROLFACTORY FROM LONGNAMECLASS

  DATA oLogger         // Objeto de log 
  DATA aObjects

  METHOD NEW()
  METHOD GetNewControl()
  METHOD Done()

ENDCLASS 


// ------------------------------------------------------
//

METHOD NEW() CLASS ZCONTROLFACTORY

::oLogger := ZLOGGER():New("ZCONTROLFACTORY")
::oLogger:Write("NEW","Create Control Factory")

// Array de controle de instancias 
::aObjects := {}

Return self

// ------------------------------------------------------
// Retorna um objeto com a instancia montada da Control 

METHOD GetNewControl(oDefObj,p1,p2,p3,p4,p5)  CLASS ZCONTROLFACTORY
Local oRet
Local bBlock
Local cBlock

::oLogger:Write("GetNewControl","Create Control based on Definition ["+GetClassName(oDefObj)+"]")

// Monta codeblock para criar instancia dinamicamente 
cBlock := "{|oView,p1,p2,p3,p4,p5| ZMVCCONTROL():New(oView,p1,p2,p3,p4,p5) }"
bBlock := &(cBlock)
oRet := Eval(bBlock,oDefObj, p1,p2,p3,p4,p5)

// Limpa codeblock para quebrar referencias 
bBlock := NIL

// Guarda a referencia deste objeto 
aadd(::aObjects,oRet)

Return oRet

// ------------------------------------------------------

METHOD Done() CLASS ZCONTROLFACTORY
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


