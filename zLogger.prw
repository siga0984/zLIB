/* -------------------------------------------------------------------------------------------

Copyright 2015-2019 J�lio Wittwer ( siga0984@gmail.com | http://siga0984.wordpress.com/ )

� permitido, gratuitamente, a qualquer pessoa que obtenha uma c�pia deste software 
e dos arquivos de documenta��o associados (o "Software"), para negociar o Software 
sem restri��es, incluindo, sem limita��o, os direitos de uso, c�pia, modifica��o, fus�o,
publica��o, distribui��o, sublicenciamento e/ou venda de c�pias do Software, 
SEM RESTRI��ES OU LIMITA��ES. 

O SOFTWARE � FORNECIDO "TAL COMO EST�", SEM GARANTIA DE QUALQUER TIPO, EXPRESSA OU IMPL�CITA,
INCLUINDO MAS N�O SE LIMITANDO A GARANTIAS DE COMERCIALIZA��O, ADEQUA��O A UMA FINALIDADE
ESPEC�FICA E N�O INFRAC��O. EM NENHUM CASO OS AUTORES OU TITULARES DE DIREITOS AUTORAIS
SER�O RESPONS�VEIS POR QUALQUER REIVINDICA��O, DANOS OU OUTRA RESPONSABILIDADE, SEJA 
EM A��O DE CONTRATO OU QUALQUER OUTRA FORMA, PROVENIENTE, FORA OU RELACIONADO AO SOFTWARE. 

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



#include 'protheus.ch'
#INCLUDE 'ParmType.ch'
//#include 'zlib.ch' 


/* ======================================================

          Classe de LOG em modo console

Autor      Julio Wittwer
Data       01/2019   

Deve ser instanciada por componente, onde o identificador do componente � 
informado no construtor. O log em console pode conter a thread, data 
e horario. Cada chamada deve identificar a opera��o em execu��o 
e ter uma mensagem com detalhes. 

====================================================== */

CLASS ZLOGGER //FROM LONGNAMECLASS

   PUBLIC DATA cComponent			AS Character
   PUBLIC DATA lDateTime			AS Logical
   PUBLIC DATA lThread				AS Logical 
   PUBLIC DATA lConsole				AS Logical 
   PUBLIC DATA oLogWriter			AS Object
   
   PUBLIC METHOD NEW()
   PUBLIC METHOD WRITE()
   PUBLIC METHOD SETWRITER()
   PUBLIC METHOD SETECHO()
   
ENDCLASS 


// ------------------------------------------------------
// Cria uma instancia de gera��o de log para uma classe ou componente

METHOD NEW(cComponent	AS Character) CLASS ZLOGGER

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cComponent		AS Character

	::cComponent := cComponent
	::lDateTime := .T. 
	::lThread   := .T. 
	::lConsole  := .F. 
	::oLogWriter := NIL

Return self


// ------------------------------------------------------
// Permite associar ao log uma classe de escrita adicional 

METHOD SETWRITER(oWriter	AS Object	)  CLASS ZLOGGER
	
	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR oWriter		AS Object

	::oLogWriter := oWriter

Return

// ------------------------------------------------------
// Habilita ou desabilita o echo em console

METHOD SETECHO(lSet	AS Logical	)  CLASS ZLOGGER

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lSet		AS Logical
	
	::lConsole  := lSet

Return


// ------------------------------------------------------
// Escreve algo no log 
// Requer a opera��o ou fun��o, e a mensagem com os detalhes de execu��o 

METHOD WRITE(	cRun	AS Character,;
				cMsg	AS Character) CLASS ZLOGGER

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cEcho := ''		AS Character
	Local nMS				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cRun		AS Character
	ParamType 1		VAR cMsg		AS Character		Optional Default NIL

	//---------------------------------------------------------------------------------------------
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
