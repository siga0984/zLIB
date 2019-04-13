#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

				Factory de Modelos 

	Criado e armazenado como um objeto do ZENV()

====================================================== */

CLASS ZMODELFACTORY FROM LONGNAMECLASS

  DATA oLogger         // Objeto de log 
  DATA aObjects

  METHOD NEW()
  METHOD GetNewModel()
  METHOD Done()

ENDCLASS 


// ------------------------------------------------------
//

METHOD NEW() CLASS ZMODELFACTORY

::oLogger := ZLOGGER():New("ZMODELFACTORY")
::oLogger:Write("NEW","Create Definition Factory")

// Array de controle de instancias 
::aObjects := {}

Return self

// ------------------------------------------------------
// Retorna um objeto com a instancia montada do Modelo 

METHOD GetNewModel(oDefObj,p1,p2,p3,p4,p5)  CLASS ZMODELFACTORY
Local oRet
Local bBlock
Local cBlock

::oLogger:Write("GetNewModel","Create Model based on Definition ["+GetClassName(oDefObj)+"]")

// Monta codeblock para criar instancia dinamicamente 
cBlock := "{|oDef,p1,p2,p3,p4,p5| ZMVCMODEL():New(oDef,p1,p2,p3,p4,p5) }"
bBlock := &(cBlock)
oRet := Eval(bBlock,oDefObj, p1,p2,p3,p4,p5)

// Limpa codeblock para quebrar referencias 
bBlock := NIL

// Guarda a referencia deste objeto 
aadd(::aObjects,oRet)

Return oRet

// ------------------------------------------------------

METHOD Done() CLASS ZMODELFACTORY
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


