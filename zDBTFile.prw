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



#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'
#include 'fileio.ch'

/* ==========================================================

Classe 		ZDBTFILE
Autor		Julio Wittwer
Data		01/2019
Descri��o 	Classe de manuten��o de arquivo DBF MEMO
			Formato DBT 
			
Observa��o  Somente leitura implementada 

========================================================== */

CLASS ZDBTFILE //FROM LONGNAMECLASS

   PUBLIC DATA oDBF			AS Object
   PUBLIC DATA cFileName	AS Character
   PUBLIC DATA nHMemo		AS Numeric

   PUBLIC METHOD NEW()
   PUBLIC METHOD OPEN()
   PUBLIC METHOD CLOSE()
   PUBLIC METHOD READMEMO()
   PUBLIC METHOD WRITEMEMO()

ENDCLASS
              
// ----------------------------------------------------------

METHOD NEW(_oDBF		AS Object	,;
			_cFileName	AS Character) CLASS ZDBTFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR _oDBF			AS Object
	ParamType 1		VAR _cFileName		AS Character

	//---------------------------------------------------------------------------------------------
	::oDBF      := _oDBF
	::cFileName := _cFileName
	::nHMemo    := -1

Return self


// ----------------------------------------------------------

METHOD OPEN() CLASS ZDBTFILE

	// Abre o arquivo MEMO 
	::nHMemo := FOpen(::cFileName)

	IF ::nHMemo == -1
		Return .T. 
	Endif

Return .T. 

// ----------------------------------------------------------

METHOD CLOSE() CLASS ZDBTFILE

	IF ::nHMemo != -1
		fClose(::nHMemo)
		::nHMemo := -1
	Endif

Return


// ----------------------------------------------------------

METHOD READMEMO(nBlock	AS Numeric	) CLASS ZDBTFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cMemo   := ''					AS Character
	Local cBlock  := space(512)			AS Character
	Local nFilePos := nBlock * 512		AS Numeric
	Local nEndPos						AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nBlock		AS Numeric
	
	fSeek(::nHMemo , nFilePos)

	While .T.
		fRead(::nHMemo,@cBlock,512)
		nEndPos := at(chr(26),cBlock)
		If nEndPos > 0
			cBlock := left(cBlock,nEndPos-1)
			cMemo += cBlock
			EXIT
		Else
			cMemo += cBlock
		Endif
	Enddo

	// -- Quebra de linha "soft" = 8D 0A
	// -- Remove a quebra
	cMemo := strtran(cMemo , chr(141)+chr(10) , '' )

Return cMemo


METHOD WRITEMEMO( 	nBlock 	AS Numeric	,;
					cMemo	AS Character) CLASS ZDBTFILE

	UserException("*** WRITEMEMO NOT AVAILABLE FOR DBT MEMO FILE ***")

Return




