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



/* ======================================================

Funcoes de Conversao String / Hexadecimal 

====================================================== */

#include "zLibDec2Hex.ch"

// ----------------------------------------
// Recebe uma string binária
// Retorna uma uma string ASCII em formato de DUMP de memíoria
// em Hexadecimal --  Cada linha com 16 bytes

STATIC Function STR2HexDmp(cBuffer,nOffset,nLength)
Local cHexLine := ''
Local cCharLine := ''
Local nI , cChar , nChar , cRet := ''
Local nTamBuff := len(cBuffer)

// Valores DEFAULT
IF nOffset = NIL ; nOffset := 1 ; Endif
IF nLength = NIL ; nLength := nTamBuff ; Endif

cBuffer := Substr(cBuffer,nOffset,nLength)

For nI := 1 to nTamBuff
	cChar := substr(cBuffer,nI,1)
	nChar := Asc(cChar)
	If nChar < 32 
		cCharLine += "." 
	Else
		cCharLine += cChar
	Endif
	cHexLine += DEC2HEX(nChar) + ' '
	If (nI%16) == 0
		cRet += cHexLine+' | '+cCharLine + Chr(13)+Chr(10)
		cCharLine := ''
		cHexLine  := ''
	Endif
Next

If !empty(cHexLine)
	cRet += padr(cHexLine,48)+' | '+ padr(cCharLine,16) + Chr(13)+Chr(10)
Endif

Return cRet


