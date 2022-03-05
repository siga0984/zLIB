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

/* ============================================================
Arquivo    zLibNBin.prw
Autor      Julio Wittwer
Data       01/2019
Descrição  Arquivo de funções auxiliares de conversao entre
           valores numericos em ASCII ( Binary String ) , com 2 e 4 bytes 
           
Referência

Um valor numérico dentro de um arquivo não necessariamente possui a mesma 
representação deste valor na memória. A plataforma IBM/PC e compatíveis 
(Intel, AMD64) usa uma disposição de bytes na memória chamada Little Endian, 
o que significa que o BIT do byte com o maior valor está no final -- lado direito
da sequencia. 

Com 2 bytes da memória, podemos representar 65536 valores distintos, 
do valor 0 ao 65535.  O valor 16737 é representado em hexadecimal 
( base 16 ) pela sequencia 0x4161. Cada par de digitos pode ser colocado 
em um byte. Logo, temos dois pares: 0x41 e 0x61

Usando a representação Big Endian, este número seria armazenado 
nesta ordem, começando da esquerda para a direita, o primeiro 
byte é o 0x41, e o segundo é 0x51. 

A representação binária deste numero em Little Endian é ao contrário, 
o byte com o bit de maior valor ( 0x41 ) vai no final da sequência. Logo, ele 
é representado na sequência 0x61 0x41 
                                      
As funções nativas da linguagem AdvPL para conversão de valores inteiros de 16 bits ( 2 Bytes ) 
e 32 Bits ( 4 Bytes ) entre os formatos numérico decimal e string binária são : 

I2Bin e Bin2I -- Número 16 bits para string binária e vice-versa 
L2Bin e Bin2L -- Número 32 bits para string binária e vice-versa 

As funções nativas de conversão do ADvPL trabalham com a representação 
binária em Little Endian. Já as funções de conversão feitas nesta lib 
trabalham com big Endian -- bit de maior valor primeiro ( a esquerda ) 

TODO
Implementar conversao de string binaria para decimal
Estudar metodos rapidos de conversao 

https://gacaffe.net/en/2019/02/24/fast-binary-conversion-part-i/

============================================================ */

User Function ZLibNBin()

cValor := chr(65)+chr(97)    // 0x41 0x61

// 24897 -- valor esperado considerando a sequencia em little endian
nValor := Bin2I(cValor)
conout(nValor)             

// 16737 -- valor esperado considerando a sequencia como big-endian
nValor := Bin2toN(cValor)
conout(nValor)             

Return


// ------------------------------------------------------------
// Converte buffer de 4 bytes ( 32 Bits ) Big-Endian
// ( high bit first ) no seu valor numerico  

USER Function Bin4toN(cBin4)
Local nByte1,nByte2,nByte3,nByte4
Local nResult := 0

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))
nByte3 := asc(substr(cBin4,3,1))
nByte4 := asc(substr(cBin4,4,1))

nResult += ( nByte1 * 16777216 )
nResult += ( nByte2 * 65536 )
nResult += ( nByte3 * 256 )
nResult += nByte4

Return nResult


// ------------------------------------------------------------
// Converte valor numérico em buffer de 4 bytes ( 32 Bits ) 
// ( High Byte First )

USER Function NtoBin4(nNum)
Local cBin4 := '' , nTmp
While nNum > 0
	nTmp := nNum % 256 
	cBin4 := chr(nTmp) + cBin4
	nNum := ( ( nNum - nTmp ) / 256 )
Enddo
While len(cBin4) < 4
	cBin4 := CHR(0) + cBin4
Enddo
Return cBin4


// ------------------------------------------------------------
// Converte buffer de 2 bytes ( 16 Bits ) no seu valor numerico  
// ( High Byte First ) 

USER Function Bin2toN(cBin4)
Local nByte1,nByte2

nByte1 := asc(substr(cBin4,1,1))
nByte2 := asc(substr(cBin4,2,1))

If nByte1 > 0
	nByte2 += ( nByte1 * 256 )
Endif

Return nByte2


// ------------------------------------------------------------
// Converte valor numérico (base 10 ) em buffer de 2 bytes ( 16 Bits ) 
// ( High Byte First ) 

USER Function NtoBin2(nNum)
Local nL := ( nNum % 256 ) 
Local nH := ( nNum-nL ) / 256 
Return chr(nH) + chr(nL)


// Funcao de Conversao de um numero entre 0 e 255 
// para uma string binaria de 8 bytes contendo "0" e "1"
// Tempo O(1) 

USER Function NTOBIT8(nI)
Return _Bits[nI+1]

// Converte um numero entre 0 e 65535 
// para uma string binaria de 16 bytes contendo "0" e "1"
USER Function NTOBIT16(nI)
Local n1,n2
If nI > 255
	n2 := ( nI%256 )
	n1 := ( nI - n2 ) / 256 
Else
	n1 := 0 
	n2 := nI
Endif  
Return _Bits[n1+1]+_Bits[n2+1]

// Converte qualquer numero deciml de até 14 digitos inteiros 
// do AdvPL para formato binario em blocos de 8 bits 
USER Function NTOBITS(nI)
Local nTmp
Local cBitsRet  := ''
While nI >= 256  
	nTmp := nI % 256
	cBitsRet := _Bits[nTmp+1] + cBitsRet
	nI := ( nI - nTmp ) / 256
Enddo
cBitsRet := _Bits[nI+1] + cBitsRet
Return cBitsRet


// Converte uma string em formato binario para
// um número em AdvPL  
USER Function BitsToN(cBitStr)
Local nI, nT := len(cBitStr)
Local nMult := 1
Local nResult := 0
For nI := nT to 1 STEP -1
	IF substr(cBitStr,nI) = '1'
		nResult += nMult
	Endif
	nMult *= 2
Next
Return nResult
                      

STATIC _HexSeq := '0123456789ABCDEF'

USER Function Bit4ToHex(cBit4)
Local nRet := 0 
If substr(cBit4,1,1) == '1'
	nRet += 8
Endif
If substr(cBit4,2,1) == '1'
	nRet += 4
Endif
If substr(cBit4,3,1) == '1'
	nRet += 2
Endif
If substr(cBit4,4,1) == '1'
	nRet += 1
Endif
Return substr(_HexSeq,nRet-1,1)



// Funcoes auxiliares da NTOBIT8(), NTOBIT16() e NTOBITS()
// Cria um cache da representacao binaria em array 

STATIC _Bits := GetBits()

STATIC Function GetBits()
Local aBits := {}

AADD(aBits,'00000000')
AADD(aBits,'00000001')
AADD(aBits,'00000010')
AADD(aBits,'00000011')
AADD(aBits,'00000100')
AADD(aBits,'00000101')
AADD(aBits,'00000110')
AADD(aBits,'00000111')
AADD(aBits,'00001000')
AADD(aBits,'00001001')
AADD(aBits,'00001010')
AADD(aBits,'00001011')
AADD(aBits,'00001100')
AADD(aBits,'00001101')
AADD(aBits,'00001110')
AADD(aBits,'00001111')
AADD(aBits,'00010000')
AADD(aBits,'00010001')
AADD(aBits,'00010010')
AADD(aBits,'00010011')
AADD(aBits,'00010100')
AADD(aBits,'00010101')
AADD(aBits,'00010110')
AADD(aBits,'00010111')
AADD(aBits,'00011000')
AADD(aBits,'00011001')
AADD(aBits,'00011010')
AADD(aBits,'00011011')
AADD(aBits,'00011100')
AADD(aBits,'00011101')
AADD(aBits,'00011110')
AADD(aBits,'00011111')
AADD(aBits,'00100000')
AADD(aBits,'00100001')
AADD(aBits,'00100010')
AADD(aBits,'00100011')
AADD(aBits,'00100100')
AADD(aBits,'00100101')
AADD(aBits,'00100110')
AADD(aBits,'00100111')
AADD(aBits,'00101000')
AADD(aBits,'00101001')
AADD(aBits,'00101010')
AADD(aBits,'00101011')
AADD(aBits,'00101100')
AADD(aBits,'00101101')
AADD(aBits,'00101110')
AADD(aBits,'00101111')
AADD(aBits,'00110000')
AADD(aBits,'00110001')
AADD(aBits,'00110010')
AADD(aBits,'00110011')
AADD(aBits,'00110100')
AADD(aBits,'00110101')
AADD(aBits,'00110110')
AADD(aBits,'00110111')
AADD(aBits,'00111000')
AADD(aBits,'00111001')
AADD(aBits,'00111010')
AADD(aBits,'00111011')
AADD(aBits,'00111100')
AADD(aBits,'00111101')
AADD(aBits,'00111110')
AADD(aBits,'00111111')
AADD(aBits,'01000000')
AADD(aBits,'01000001')
AADD(aBits,'01000010')
AADD(aBits,'01000011')
AADD(aBits,'01000100')
AADD(aBits,'01000101')
AADD(aBits,'01000110')
AADD(aBits,'01000111')
AADD(aBits,'01001000')
AADD(aBits,'01001001')
AADD(aBits,'01001010')
AADD(aBits,'01001011')
AADD(aBits,'01001100')
AADD(aBits,'01001101')
AADD(aBits,'01001110')
AADD(aBits,'01001111')
AADD(aBits,'01010000')
AADD(aBits,'01010001')
AADD(aBits,'01010010')
AADD(aBits,'01010011')
AADD(aBits,'01010100')
AADD(aBits,'01010101')
AADD(aBits,'01010110')
AADD(aBits,'01010111')
AADD(aBits,'01011000')
AADD(aBits,'01011001')
AADD(aBits,'01011010')
AADD(aBits,'01011011')
AADD(aBits,'01011100')
AADD(aBits,'01011101')
AADD(aBits,'01011110')
AADD(aBits,'01011111')
AADD(aBits,'01100000')
AADD(aBits,'01100001')
AADD(aBits,'01100010')
AADD(aBits,'01100011')
AADD(aBits,'01100100')
AADD(aBits,'01100101')
AADD(aBits,'01100110')
AADD(aBits,'01100111')
AADD(aBits,'01101000')
AADD(aBits,'01101001')
AADD(aBits,'01101010')
AADD(aBits,'01101011')
AADD(aBits,'01101100')
AADD(aBits,'01101101')
AADD(aBits,'01101110')
AADD(aBits,'01101111')
AADD(aBits,'01110000')
AADD(aBits,'01110001')
AADD(aBits,'01110010')
AADD(aBits,'01110011')
AADD(aBits,'01110100')
AADD(aBits,'01110101')
AADD(aBits,'01110110')
AADD(aBits,'01110111')
AADD(aBits,'01111000')
AADD(aBits,'01111001')
AADD(aBits,'01111010')
AADD(aBits,'01111011')
AADD(aBits,'01111100')
AADD(aBits,'01111101')
AADD(aBits,'01111110')
AADD(aBits,'01111111')
AADD(aBits,'10000000')
AADD(aBits,'10000001')
AADD(aBits,'10000010')
AADD(aBits,'10000011')
AADD(aBits,'10000100')
AADD(aBits,'10000101')
AADD(aBits,'10000110')
AADD(aBits,'10000111')
AADD(aBits,'10001000')
AADD(aBits,'10001001')
AADD(aBits,'10001010')
AADD(aBits,'10001011')
AADD(aBits,'10001100')
AADD(aBits,'10001101')
AADD(aBits,'10001110')
AADD(aBits,'10001111')
AADD(aBits,'10010000')
AADD(aBits,'10010001')
AADD(aBits,'10010010')
AADD(aBits,'10010011')
AADD(aBits,'10010100')
AADD(aBits,'10010101')
AADD(aBits,'10010110')
AADD(aBits,'10010111')
AADD(aBits,'10011000')
AADD(aBits,'10011001')
AADD(aBits,'10011010')
AADD(aBits,'10011011')
AADD(aBits,'10011100')
AADD(aBits,'10011101')
AADD(aBits,'10011110')
AADD(aBits,'10011111')
AADD(aBits,'10100000')
AADD(aBits,'10100001')
AADD(aBits,'10100010')
AADD(aBits,'10100011')
AADD(aBits,'10100100')
AADD(aBits,'10100101')
AADD(aBits,'10100110')
AADD(aBits,'10100111')
AADD(aBits,'10101000')
AADD(aBits,'10101001')
AADD(aBits,'10101010')
AADD(aBits,'10101011')
AADD(aBits,'10101100')
AADD(aBits,'10101101')
AADD(aBits,'10101110')
AADD(aBits,'10101111')
AADD(aBits,'10110000')
AADD(aBits,'10110001')
AADD(aBits,'10110010')
AADD(aBits,'10110011')
AADD(aBits,'10110100')
AADD(aBits,'10110101')
AADD(aBits,'10110110')
AADD(aBits,'10110111')
AADD(aBits,'10111000')
AADD(aBits,'10111001')
AADD(aBits,'10111010')
AADD(aBits,'10111011')
AADD(aBits,'10111100')
AADD(aBits,'10111101')
AADD(aBits,'10111110')
AADD(aBits,'10111111')
AADD(aBits,'11000000')
AADD(aBits,'11000001')
AADD(aBits,'11000010')
AADD(aBits,'11000011')
AADD(aBits,'11000100')
AADD(aBits,'11000101')
AADD(aBits,'11000110')
AADD(aBits,'11000111')
AADD(aBits,'11001000')
AADD(aBits,'11001001')
AADD(aBits,'11001010')
AADD(aBits,'11001011')
AADD(aBits,'11001100')
AADD(aBits,'11001101')
AADD(aBits,'11001110')
AADD(aBits,'11001111')
AADD(aBits,'11010000')
AADD(aBits,'11010001')
AADD(aBits,'11010010')
AADD(aBits,'11010011')
AADD(aBits,'11010100')
AADD(aBits,'11010101')
AADD(aBits,'11010110')
AADD(aBits,'11010111')
AADD(aBits,'11011000')
AADD(aBits,'11011001')
AADD(aBits,'11011010')
AADD(aBits,'11011011')
AADD(aBits,'11011100')
AADD(aBits,'11011101')
AADD(aBits,'11011110')
AADD(aBits,'11011111')
AADD(aBits,'11100000')
AADD(aBits,'11100001')
AADD(aBits,'11100010')
AADD(aBits,'11100011')
AADD(aBits,'11100100')
AADD(aBits,'11100101')
AADD(aBits,'11100110')
AADD(aBits,'11100111')
AADD(aBits,'11101000')
AADD(aBits,'11101001')
AADD(aBits,'11101010')
AADD(aBits,'11101011')
AADD(aBits,'11101100')
AADD(aBits,'11101101')
AADD(aBits,'11101110')
AADD(aBits,'11101111')
AADD(aBits,'11110000')
AADD(aBits,'11110001')
AADD(aBits,'11110010')
AADD(aBits,'11110011')
AADD(aBits,'11110100')
AADD(aBits,'11110101')
AADD(aBits,'11110110')
AADD(aBits,'11110111')
AADD(aBits,'11111000')
AADD(aBits,'11111001')
AADD(aBits,'11111010')
AADD(aBits,'11111011')
AADD(aBits,'11111100')
AADD(aBits,'11111101')
AADD(aBits,'11111110')
AADD(aBits,'11111111')

Return aBits

// ----------------------------------------
// Montagem do cache interno com HASH 
// para conversão de números decimais de 0 a 255
// em string binária de 8 bits, e vice-versa

STATIC _BitHash := BuildBitHash()

STATIC Function BuildBitHash()
Local nI
Local _BitHash := HMNew()
For nI := 0 to 255
  // Seta uma tupla chave , valor 
  HMSet(_BitHash, U_NTOBIT8(nI) , nI )
Next
Return _BitHash

                       
// ----------------------------------------
// Conversão optimizada de uma sequencia de 
// string de 8 bits para numérico 
USER Function BIT8TON(cStrBit,nNum)
Return HMGET( _BitHash,cStrBit,@nNum)
                                                          
