#include 'Protheus.ch'
#include 'zLib.ch'

/* ==========================================================

Classe de montagem de Ambiente 

Por hora, ela seta o formato de data e habilita acentuação

========================================================== */

CLASS ZLIBENV

  DATA lVerbose
  DATA aObjList     // Lista de objetos relacionados ao ambiente 

  METHOD NEW()      // Construtor
  METHOD DONE()     // Finalizador / Destrutor

  METHOD SETENV()      // Seta o minimo do ambiente ( formato de data e acentuação ) 
  METHOD SetObject()   // Guarda um objeto do ambiente
  METHOD GetObject()   // Recupera um objeto do ambiente 

ENDCLASS

// ----------------------------------------------------------
//

METHOD NEW() CLASS ZLIBENV
::aObjList := {}
::lVerbose := .F. 
Return

// ----------------------------------------------------------
// Metodo responsavel pela montagem do ambiente 
// Formato de data e habilita acentuação no SmartClient

METHOD SETENV() CLASS ZLIBENV

If ::lVerbose
	conout("ZLIBENV:SETENV()")	
Endif		

SET DATE BRITISH
SET CENTURY ON
SET EPOCH TO 1950

PTSETACENTO(.T.)

Return 

// ----------------------------------------------------------
// Finaliza o contexto do ambiente   
// Limpa os objetos relacionados ao ambiente 

METHOD DONE() CLASS ZLIBENV
Local nI , cId , oObj

If ::lVerbose
	conout("ZLIBENV:Done() -- Begin ")	
Endif		

For nI := 1 to len(::aObjList)
	
	cId  := ::aObjList[nI][1]
	oObj := ::aObjList[nI][2]
	
	If oObj != NIL
	
		If ::lVerbose
			conout("ZLIBENV:Done() -- FreeObj ["+cId+"]")
		Endif
		
		oObj:Done()
		FreeObj(oObj)
		
	Endif
	
Next

aSize(::aObjList,0)

If ::lVerbose
	conout("ZLIBENV:Done() -- End")	
Endif		

Return


// ----------------------------------------------------------
//

METHOD SetObject(cObjId,oObject) CLASS ZLIBENV
Local nPos

If ::lVerbose
	conout("ZLIBENV:SetObject("+cObjId+") -- "+GETCLASSNAME(oObject))
Endif		

nPos := ascan(::aObjList,{|x| x[1] == cObjId})
If nPos == 0 
	aadd(::aObjList , { cObjId,oObject } )	
Else
	::aObjList[nPos][2] := oObject
Endif
Return


// ----------------------------------------------------------
//

METHOD GetObject(cObjId) CLASS ZLIBENV
Local nPos
nPos := ascan(::aObjList,{|x| x[1] == cObjId})
If nPos > 0 
	Return ::aObjList[nPos][2]
Endif
Return NIL

