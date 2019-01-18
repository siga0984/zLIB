#include 'Protheus.ch'
#include 'zLib.ch'

/* ==========================================================

Classe de montagem de Ambiente 

Por hora, ela seta o formato de data e habilita acentiação

========================================================== */

CLASS ZLIBENV

  DATA aObjList

  METHOD NEW()
  METHOD DONE()

  METHOD SETENV()
  METHOD SETOBJ()
  METHOD GETOBJ()

ENDCLASS


METHOD NEW() CLASS ZLIBENV
::aObjList := {}
Return

METHOD SETENV() CLASS ZLIBENV

SET DATE BRITISH
SET CENTURY ON
SET EPOCH TO 1950

PTSETACENTO(.T.)

Return 

METHOD DONE() CLASS ZLIBENV

aEval(::aObjList,{|x| Freeobj(x)})
aSize(::aObjList,0)

Return


METHOD SETOBJ(cObjId,oObject) CLASS ZLIBENV
Local nPos
nPos := ascan(::aObjList,{|x| x[1] == cObjId})
If nPos == 0 
	aadd(::aObjList , { cObjId,oObject } )	
Else
	::aObjList[nPos][2] := oObject
Endif
Return


METHOD GETOBJ(cObjId) CLASS ZLIBENV
Local nPos
nPos := ascan(::aObjList,{|x| x[1] == cObjId})
If nPos > 0 
	Return ::aObjList[nPos][2]
Endif
Return NIL

