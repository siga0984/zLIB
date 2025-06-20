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



#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'

//aBits[nI+1] USO DIRETAMENTE ASSIM PARA SER MAIS OTIMIZADO!

/* ================================================================================================
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

=================================================================================================== */

User Function ZLibNBin()

	cValor := chr(65)+chr(97)    // 0x41 0x61

	// 24897 -- valor esperado considerando a sequencia em little endian
	nValor := Bin2I(cValor)
	conout(nValor)             

	// 16737 -- valor esperado considerando a sequencia como big-endian
	nValor := Bin2toN(cValor)
	conout(nValor)             

Return


// ------------------------------------------------------------------------------------------------
// Converte buffer de 4 bytes ( 32 Bits ) Big-Endian ( high bit first ) no seu valor numerico  
USER Function Bin4ToN(cBin4	AS Character)	AS Numeric

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nByte1,nByte2,nByte3,nByte4	AS Numeric
	Local nResult := 0					AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cBin4		AS Character
	
	nByte1 := asc(substr(cBin4,1,1))
	nByte2 := asc(substr(cBin4,2,1))
	nByte3 := asc(substr(cBin4,3,1))
	nByte4 := asc(substr(cBin4,4,1))

	nResult += ( nByte1 * 16777216 )
	nResult += ( nByte2 * 65536 )
	nResult += ( nByte3 * 256 )
	nResult += nByte4

Return nResult


// ------------------------------------------------------------------------------------------------
// Converte valor numérico em buffer de 4 bytes ( 32 Bits ) ( High Byte First )
USER Function NToBin4(nNum	AS Numeric	)		AS Character
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBin4 := ''		AS Character
	Local nTmp				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nNum		AS Numeric

	While nNum > 0
		nTmp := nNum % 256 
		cBin4 := chr(nTmp) + cBin4
		nNum := ( ( nNum - nTmp ) / 256 )
	Enddo

	While len(cBin4) < 4
		cBin4 := CHR(0) + cBin4
	Enddo

Return cBin4

// ------------------------------------------------------------------------------------------------
// Converte buffer de 2 bytes ( 16 Bits ) no seu valor numerico ( High Byte First ) 
USER Function Bin2toN(cBin4)	AS Numeric

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nByte1,nByte2		AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cBin4		AS Character
	
	nByte1 := asc(substr(cBin4,1,1))
	nByte2 := asc(substr(cBin4,2,1))

	If nByte1 > 0
		nByte2 += ( nByte1 * 256 )
	Endif

Return nByte2

// ------------------------------------------------------------------------------------------------
// Converte valor numérico (base 10 ) em buffer de 2 bytes ( 16 Bits ) ( High Byte First ) 
USER Function NToBin2(nNum)		AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nL ,nH		AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nNum		AS Numeric

	//---------------------------------------------------------------------------------------------
	nL := ( nNum % 256 ) 
	nH := ( nNum-nL ) / 256 

Return chr(nH) + chr(nL)

// Funcao de Conversao de um numero entre 0 e 255 para uma string binaria de 8 bytes contendo "0" e "1"
// Tempo O(1)
USER Function NToBit8(nI)	AS Character

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nI		AS Numeric

Return aBits[nI+1]

// Converte um numero entre 0 e 65535 
// para uma string binaria de 16 bytes contendo "0" e "1"
USER Function NToBit16(nI)	AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local n1,n2			AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nI		AS Numeric

	//---------------------------------------------------------------------------------------------
	If nI > 255
		n2 := ( nI%256 )
		n1 := ( nI - n2 ) / 256 
	Else
		n1 := 0 
		n2 := nI
	Endif 

Return aBits[n1+1]+aBits[n2+1]

// Converte qualquer numero deciml de até 14 digitos inteiros 
// do AdvPL para formato binario em blocos de 8 bits 
USER Function NToBits(nI)	AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBitsRet  := ''	AS Character
	Local nTmp				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nI		AS Numeric

	//---------------------------------------------------------------------------------------------
	While nI >= 256  
		nTmp := nI % 256
		cBitsRet := aBits[nTmp+1] + cBitsRet
		nI := ( nI - nTmp ) / 256
	Enddo
	cBitsRet := aBits[nI+1] + cBitsRet

Return cBitsRet


// Converte uma string em formato binario para um número em AdvPL
USER Function BitsToN(cBitStr)	AS Numeric

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nI, nT	:= len(cBitStr)		AS Numeric
	Local nMult		:= 1				AS Numeric
	Local nResult	:= 0				AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cBitStr		AS Character

	//---------------------------------------------------------------------------------------------
	For nI := nT to 1 STEP -1
		IF substr(cBitStr,nI) = '1'
			nResult += nMult
		Endif
		nMult *= 2
	Next

Return nResult
                      
//-------------------------------------------------------------------------------------------------
STATIC cHexSeq := '0123456789ABCDEF'		AS Character
//-------------------------------------------------------------------------------------------------
USER Function Bit4ToHex(cBit4)	AS Character

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nRet := 0 			AS Numeric

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cBit4		AS Character

	//---------------------------------------------------------------------------------------------
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

Return substr(cHexSeq,nRet-1,1)


// Funcoes auxiliares da NTOBIT8(), NTOBIT16() e NTOBITS()
// Cria um cache da representacao binaria em array 
STATIC aBits := {;
			;//   00        01          02         03            04          05        06         07
/*+000*/	'00000000','00000001','00000010','00000011',	'00000100','00000101','00000110','00000111',;
/*+008*/	'00001000','00001001','00001010','00001011',	'00001100','00001101','00001110','00001111',;
/*+016*/	'00010000','00010001','00010010','00010011',	'00010100','00010101','00010110','00010111',;
/*+024*/	'00011000','00011001','00011010','00011011',	'00011100','00011101','00011110','00011111',;
;
/*+032*/	'00100000','00100001','00100010','00100011',	'00100100','00100101','00100110','00100111',;
/*+040*/	'00101000','00101001','00101010','00101011',	'00101100','00101101','00101110','00101111',;
/*+048*/	'00110000','00110001','00110010','00110011',	'00110100','00110101','00110110','00110111',;
/*+056*/	'00111000','00111001','00111010','00111011',	'00111100','00111101','00111110','00111111',;
;
/*+064*/	'01000000','01000001','01000010','01000011',	'01000100','01000101','01000110','01000111',;
/*+072*/	'01001000','01001001','01001010','01001011',	'01001100','01001101','01001110','01001111',;
/*+080*/	'01010000','01010001','01010010','01010011',	'01010100','01010101','01010110','01010111',;
/*+088*/	'01011000','01011001','01011010','01011011',	'01011100','01011101','01011110','01011111',;
;
/*+096*/	'01100000','01100001','01100010','01100011',	'01100100','01100101','01100110','01100111',;
/*+104*/	'01101000','01101001','01101010','01101011',	'01101100','01101101','01101110','01101111',;
/*+112*/	'01110000','01110001','01110010','01110011',	'01110100','01110101','01110110','01110111',;
/*+120*/	'01111000','01111001','01111010','01111011',	'01111100','01111101','01111110','01111111',;
;
/*+128*/	'10000000','10000001','10000010','10000011',	'10000100','10000101','10000110','10000111',;
/*+136*/	'10001000','10001001','10001010','10001011',	'10001100','10001101','10001110','10001111',;
/*+144*/	'10010000','10010001','10010010','10010011',	'10010100','10010101','10010110','10010111',;
/*+152*/	'10011000','10011001','10011010','10011011',	'10011100','10011101','10011110','10011111',;
;
/*+160*/	'10100000','10100001','10100010','10100011',	'10100100','10100101','10100110','10100111',;
/*+168*/	'10101000','10101001','10101010','10101011',	'10101100','10101101','10101110','10101111',;
/*+176*/	'10110000','10110001','10110010','10110011',	'10110100','10110101','10110110','10110111',;
/*+184*/	'10111000','10111001','10111010','10111011',	'10111100','10111101','10111110','10111111',;
;
/*+192*/	'11000000','11000001','11000010','11000011',	'11000100','11000101','11000110','11000111',;
/*+200*/	'11001000','11001001','11001010','11001011',	'11001100','11001101','11001110','11001111',;
/*+208*/	'11010000','11010001','11010010','11010011',	'11010100','11010101','11010110','11010111',;
/*+216*/	'11011000','11011001','11011010','11011011',	'11011100','11011101','11011110','11011111',;
;
/*+224*/	'11100000','11100001','11100010','11100011',	'11100100','11100101','11100110','11100111',;
/*+232*/	'11101000','11101001','11101010','11101011',	'11101100','11101101','11101110','11101111',;
/*+240*/	'11110000','11110001','11110010','11110011',	'11110100','11110101','11110110','11110111',;
/*+248*/	'11111000','11111001','11111010','11111011',	'11111100','11111101','11111110','11111111'};
			AS Array

// ------------------------------------------------------------------------------------------------
// Montagem do cache interno com HASH para conversão de números decimais de 0 a 255
// em string binária de 8 bits, e vice-versa
//-------------------------------------------------------------------------------------------------
STATIC Function BuildBitHash()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nI					AS Numeric

	//---------------------------------------------------------------------------------------------
	jBitHash	:= JsonObject():New()
	For nI := 0 to 255
		jBitHash[aBits[nI+1]]	:= nI
	Next

Return
                       
// ------------------------------------------------------------------------------------------------
// Conversão optimizada de uma sequencia de string de 8 bits para numérico 
STATIC jBitHash 					AS Json
// ------------------------------------------------------------------------------------------------
USER Function Bit8ToN(	cStrBit	,;	//01 cStrBit
						nNum	);	//02 @nNum
							AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local lRet	:= .F.		AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cStrBit		AS Character
	
	If ValType(jBitHash) == 'U'
		BuildBitHash()
	EndIf

	If jBitHash:hasProperty(cStrBit)
		lRet	:= .T.
		nNum	:= jBitHash[cStrBit]
	EndIf

Return lRet
