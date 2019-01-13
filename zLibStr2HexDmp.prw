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


