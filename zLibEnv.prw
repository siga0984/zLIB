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

/* ==========================================================

Classe de montagem de Ambiente 

Por hora, ela seta o formato de data e habilita acentuação
                                                              
TODO 

- Usar HASH na lista de objetos do Ambiente 

========================================================== */

CLASS ZLIBENV

  DATA lVerbose
  DATA aObjList     // Lista de objetos relacionados ao ambiente 

  METHOD NEW()      // Construtor
  METHOD DONE()     // Finalizador / Destrutor

  METHOD SETENV()          // Seta o minimo do ambiente ( formato de data e acentuação ) 
  METHOD SetObject()       // Guarda um objeto do ambiente
  METHOD GetObject()       // Recupera um objeto do ambiente 
  METHOD InitDBConn()      // Inicia objeto de Pool com DBAccess
  METHOD InitMVCFactory()  // Inicia Factories de MVC
  METHOD InitMemCache()    // Inicializa memcade -- caso habilitado 
  
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
Local cId , oObj , oNode

If ::lVerbose
	conout("ZLIBENV:Done() -- Begin ")	
Endif		

While len(::aObjList) > 0 

    oNode := aTail(::aObjList)
	
	cId  := oNode[1]
	oObj := oNode[2]
	
	If oObj != NIL
	
		If ::lVerbose
			conout("ZLIBENV:Done() -- FreeObj ["+cId+"] of class ["+GetClassName(oObj)+"]")
		Endif
		
		oObj:Done()
		FreeObj(oObj)
		
	Endif

	aSize(::aObjList, len(::aObjList)-1 )
	
Enddo

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


// ----------------------------------------------------------
//

METHOD InitDBConn()  CLASS ZLIBENV
Local oDBConn
oDBConn  := ZDBACCESS():New()
// oDBConn:SETPOOL(.T. , "DB_POOL")
::SetObject("DBCONN",oDBConn)
Return .T. 


// ----------------------------------------------------------
//

METHOD InitMVCFactory(bDefBlock)  CLASS ZLIBENV
Local oDefFactory
Local oModelFactory
Local oViewFactory
Local oCtrlFactory

// Cria e Guarda o FACTORY de Definições no ambiente
oDefFactory := ZDEFFACTORY():New( bDefBlock )
::SetObject("ZDEFFACTORY",oDefFactory)

// Factory de Modelos 
oModelFactory := ZMODELFACTORY():New()
::SetObject("ZMODELFACTORY",oModelFactory)

// Factory de Views 
oViewFactory := ZVIEWFACTORY():New()
::SetObject("ZVIEWFACTORY",oViewFactory)

// Factory de Controles
oCtrlFactory := ZCONTROLFACTORY():New()
::SetObject("ZCONTROLFACTORY",oCtrlFactory)

Return .T. 



METHOD InitMemCache()  CLASS ZLIBENV
Local oMemCache

// Cria um objeto de cache em memoria 
// e guarda no environment
IF Val(GetSrvProfString("UseMemCache","0")) > 0 
	oMemCache := ZMEMCACHED():New( "127.0.0.1" , 11211 )
	::SetObject("MEMCACHED",oMemCache)
Endif

Return .T. 

