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



#include 'protheus.ch'
#include 'fileio.ch'

/* ==========================================================

Classe 		ZDBTFILE
Autor		Julio Wittwer
Data		01/2019
Descrição 	Classe de manutenção de arquivo DBF MEMO
			Formato DBT 
			
Observação  Somente leitura implementada 

========================================================== */

CLASS ZDBTFILE FROM LONGNAMECLASS

   DATA oDBF
   DATA cFileName
   DATA nHMemo

   METHOD NEW()
   METHOD OPEN()
   METHOD CLOSE()
   METHOD READMEMO()
   METHOD WRITEMEMO()

ENDCLASS
              
// ----------------------------------------------------------

METHOD NEW(_oDBF,_cFileName) CLASS ZDBTFILE

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

METHOD READMEMO(nBlock) CLASS ZDBTFILE
Local cMemo   := ''
Local cBlock  := space(512)
Local nFilePos := nBlock * 512
Local nEndPos

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


METHOD WRITEMEMO( nBlock , cMemo ) CLASS ZDBTFILE
UserException("*** WRITEMEMO NOT AVAILABLE FOR DBT MEMO FILE ***")
Return




