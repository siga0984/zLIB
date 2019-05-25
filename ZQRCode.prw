#include 'protheus.ch'
#include 'zlib.ch'


/*

=== Referencias da implementação ===

https://www.qr-code-generator.com/
https://www.qrcode.com/en/codes/
https://en.wikipedia.org/wiki/QR_code
https://www.thonky.com/qr-code-tutorial/introduction
https://www.thonky.com/qr-code-tutorial/module-placement-matrix
https://www.thonky.com/qr-code-tutorial/alignment-pattern-locations
https://www.thonky.com/qr-code-tutorial/format-version-information 
https://www.thonky.com/qr-code-tutorial/data-masking
https://www.thonky.com/qr-code-tutorial/mask-patterns

Referencias de correção de erro 

https://www.thonky.com/qr-code-tutorial/error-correction-coding
https://www.thonky.com/qr-code-tutorial/generator-polynomial-tool?degree=10
https://github.com/zxing/zxing/tree/00f634024ceeee591f54e6984ea7dd666fab22ae/cpp
https://github.com/ArashPartow/schifra
https://archive.codeplex.com/?p=schifra
https://stackoverflow.com/questions/11243272/error-correcting-codes
http://www.drdobbs.com/cpp/reed-solomon-error-correction/184410107

*/

// ===============================================================


CLASS ZQRCODE FROM LONGNAMECLASS


    DATA nVersion         // Versao do QR Code ( 1-40 )
    DATA nSize            // Aresta do QR Code
    DATA aGrid            // Grid de modulos do QR Code
    DATA cQRData          // Dados da mensagem 
    DATA cErrCorr         // Correção de erro ( LMQH )
    DATA nMask            // Data Mask ( 0-7 )
    DATA nMode            // Encoding Mode ( Alfanumerico=2 )
    DATA aBitStream       // CodeWord final ( Dados + Error Correction )
    DATA oLogger
    DATA lVerbose

    METHOD NEW()
    METHOD SetEC()
    METHOD SetData()
    METHOD BuildData()
    METHOD DeployBits()
    METHOD SelectMask()
    METHOD PutAlign()
	METHOD BuildErrData()
	METHOD BestVersion()
    METHOD PutFVInfo()
    METHOD GetCountSize()
    METHOD PrintQR()
    METHOD PrintBits()
    METHOD ApplyMask()
    

ENDCLASS 


// ------------------------------------------
// Cria um objeto para manipulação de um QR Code

METHOD NEW() CLASS ZQRCODE 
::nVersion := 0
::nSize := 0
::aGrid := {}
::cQRData := ''
::cErrCorr := 'L'
::nMode   := 0
::lVerbose := .F.
::oLogger := ZLOGGER():New("ZQRCODE")
::oLogger:Write("NEW","New QRCode Object")
Return self


// ------------------------------------------
// Para a criação de um QR Code, o primeiro métoo a ser chamado 
// é a definição do nivel de correção de erro 
// Caso nao chamado, o default é "L" ( Low ) 

METHOD SetEC(cErrCorr) CLASS ZQRCODE
IF ::lVerbose
	::oLogger:Write("SetEC","Error Correction set to ["+cErrCorr+"]")
Endif
::cErrCorr := cErrCorr
Return          


// ------------------------------------------
// A Criação do QR Code envolve a codificaçao 
// de dados, logo devemos informar a string
// a ser representada no QR Code, e opcionalmente
// o modo de codificacão. Atualmente são suportados 
// os métodos 2 ( alfanumérico ) e 3 ( ASCII Bytes / UTF8 )  

METHOD SetData(cStr,nMode) CLASS ZQRCODE

::oLogger:Write("SETDATA","Set Data ["+cValToChar(cStr)+"]")

::cQRData := cStr

IF nMode != NIL 
	// Modo informado como parametro
	::nMode := nMode
Else
	// TODO - chumbado por enquando, alfanumerico 
	// O Correto é verificar se a mensagem cabe dentro de um determnado 
	// modo de codificacao ( numerico, alfa, bytes ) segundo o Error Correction definido
	::nMode := 2
Endif

::oLogger:Write("SETDATA","Data Mode ["+cValToChar(nMode)+"]")

Return

// ------------------------------------------
// Imprime QR Code em modo de depuração no console

METHOD PrintBits() CLASS ZQRCODE 
Local nL , nC , cRow
conout('')
For nL := 1 to len(::aGrid)
	cRoW := ''
	For nC := 1 to len(::aGrid[nL])
		IF ::aGrid[nL][nC] > 0
			IF ::aGrid[nL][nC] = 1
				cRow += chr(178)+chr(178)
			ElseIF ::aGrid[nL][nC] = 2
				cRow += '  '
			ElseIF ::aGrid[nL][nC] = 3
				cRow += chr(219)+chr(219)
			Endif
		ElseIf ::aGrid[nL][nC] = 0
			cRow += chr(176)+chr(176)
		Else
			cRow += '--'
		Endif
	Next
	conout(cRow)
Next
conout('')
Return


// ------------------------------------------
// Imprime QR Code no console em modo pseudo-grafico 

METHOD PrintQR(aGrid) CLASS ZQRCODE 
Local nL , nC , cRow
Local lSet1
Local lSet2

conout(replicate('-',::nSize))
Conout('')

IF pcount() == 0 
	aGrid := ::aGrid
Endif

For nL := 1 to len(aGrid) step 2
	cRoW := ''
	For nC := 1 to len(aGrid[nL])
		lSet1 := aGrid[nL][nC] == 1 .or. aGrid[nL][nC] == 3 
		IF nL+1>::nSize
			lSet2 := .F.
		Else		
			lSet2 := aGrid[nL+1][nC] == 1 .or. aGrid[nL+1][nC] == 3 
		Endif
		if lset1 .and. lset2
			cRow += chr(219) // full block
   		ElseIf lset1 .and. !lset2
			cRow += chr(223) // upper half
   		ElseIf !lset1 .and. lset2
			cRow += chr(220) // lower half
		Else   		
			cRow += ' ' // empty
		Endif
	Next
	conout(cRow)
Next
conout('')
Return

// ------------------------------------------------------------
// Função auxiliar PutArray()
// Plota dentro do array de destino, nas coordenadas informadas
// em nLine e nCol o array de origem. se e somente se 
// o array de origem na posição informada appenas 
// atualizar coordenadas nao inicializadas ou usadas (-1)      
// Util para colocar os patterns de localização e alinhamento 

STATIC Function PutArray(aTarget,aSource,nLine,nCol)
Local nL , nC
Local lOk := .T. 
// Primeiro verifica se algum ponto já está preenchido 
For nL := 1 to len(aSource)               
	For nC := 1 to len(aSource[nL])
		IF aTarget[nLine+nL-1][nCol+nC-1] >= 0 
			lOk := .F.
			EXIT
		Endif
	Next    
	IF !lOk
		EXIT
	Endif
Next
If lOk
	// Todas as coordenadas livres, aplica o array 
	For nL := 1 to len(aSource)
		For nC := 1 to len(aSource[nL])
			aTarget[nLine+nL-1][nCol+nC-1] := aSource[nL][nC]
		Next
	Next
Endif
Return lOk


// ------------------------------------------------------------
// Monta e inicializa um Grid de um determinado tamanho 
// Já plota no Grid os Patterns de Localização,
// as Time Lines e o Dark Pattern

STATIC Function BuildGrid(nSize)
Local aGrid := {}, aRow := {}
For nI := 1 to nSize
	aadd(aRow,-1)
Next             
For nI := 1 to nSize
	aadd(aGrid,aClone(aRow))
Next             
return aGrid


// ------------------------------------------------------------
// Calcula o tamanho da aresta do QR Code de acordo com a versao 
// 21 , 25 , 29 ... ( cresce em passos de 4 ) 
STATIC Function VerSize(nVersion)
Return ( 17 + ( 4*nVersion ) ) 


// ------------------------------------------------------------
// Plota os patterns de alinhamento no Grid 
// de acordo com as coordenadas infomadas
METHOD PutAlign( aAlignPos , aAlign ) CLASS ZQRCODE 
Local nL , nC 
Local nT := len(aAlignPos)

For nL := 1 TO nT
	For nC := 1 to nT
		::oLogger:Write("BUILDDATA","Put Alignment pattern at ["+cValToChar(aAlignPos[nL]-1)+","+cValToChar(aAlignPos[nC]-1)+"]")
    	PutArray(::aGrid,aAlign,aAlignPos[nL]-1,aAlignPos[nC]-1 )
	Next
Next

Return


// ------------------------------------------------------------
// Obtem as coordenadas dos padroes de alinhamento por versao
// Coordenadas em {Linha,Coluna}

STATIC __CenterAlign 

STATIC Function GetCenterAlign(nVersion)

IF __CenterAlign = NIL
	
	_CenterAlign := {}
	
	aadd(_CenterAlign,{6,18})  // Version 2
	aadd(_CenterAlign,{6,22})  // Version 3
	aadd(_CenterAlign,{6,26})  // Version 4
	aadd(_CenterAlign,{6,30})  // Version 5
	aadd(_CenterAlign,{6,34})  // Version 6
	aadd(_CenterAlign,{6,22,38})  // Version 7
	aadd(_CenterAlign,{6,24,42})  // Version 8
	aadd(_CenterAlign,{6,26,46})  // Version 9
	aadd(_CenterAlign,{6,28,50})  // Version 10
	aadd(_CenterAlign,{6,30,54})  // Version 11
	aadd(_CenterAlign,{6,32,58})  // Version 12
	aadd(_CenterAlign,{6,34,62})  // Version 13
	aadd(_CenterAlign,{6,26,46,66})  // Version 14
	aadd(_CenterAlign,{6,26,48,70})  // Version 15
	aadd(_CenterAlign,{6,26,50,74})  // Version 16
	aadd(_CenterAlign,{6,30,54,78})  // Version 17
	aadd(_CenterAlign,{6,30,56,82})  // Version 18
	aadd(_CenterAlign,{6,30,58,86})  // Version 19
	aadd(_CenterAlign,{6,34,62,90})  // Version 20
	aadd(_CenterAlign,{6,28,50,72,94})  // Version 21
	aadd(_CenterAlign,{6,26,50,74,98})  // Version 22
	aadd(_CenterAlign,{6,30,54,78,102})  // Version 23
	aadd(_CenterAlign,{6,28,54,80,106})  // Version 24
	aadd(_CenterAlign,{6,32,58,84,110})  // Version 25
	aadd(_CenterAlign,{6,30,58,86,114})  // Version 26
	aadd(_CenterAlign,{6,34,62,90,118})  // Version 27
	aadd(_CenterAlign,{6,26,50,74,98,122})  // Version 28
	aadd(_CenterAlign,{6,30,54,78,102,126})  // Version 29
	aadd(_CenterAlign,{6,26,52,78,104,130})  // Version 30
	aadd(_CenterAlign,{6,30,56,82,108,134})  // Version 31
	aadd(_CenterAlign,{6,34,60,86,112,138})  // Version 32
	aadd(_CenterAlign,{6,30,58,86,114,142})  // Version 33
	aadd(_CenterAlign,{6,34,62,90,118,146})  // Version 34
	aadd(_CenterAlign,{6,30,54,78,102,126,150})  // Version 35
	aadd(_CenterAlign,{6,24,50,76,102,128,154})  // Version 36
	aadd(_CenterAlign,{6,28,54,80,106,132,158})  // Version 37
	aadd(_CenterAlign,{6,32,58,84,110,136,162})  // Version 38
	aadd(_CenterAlign,{6,26,54,82,110,138,166})  // Version 39
	aadd(_CenterAlign,{6,30,58,86,114,142,170})  // Version 40
	
Endif

Return _CenterAlign[nVersion-1]


STATIC _Alpha := GetAlpha()

// ------------------------------------------------------------
// Obtém o alfabeto Alfanumérico ( restrito ) para codificação

STATIC Function GetAlpha()

IF _Alpha = NIL
	
	_Alpha := {}
	
	aadd(_Alpha,{'0', 0 })
	aadd(_Alpha,{'1', 1 })
	aadd(_Alpha,{'2', 2 })
	aadd(_Alpha,{'3', 3 })
	aadd(_Alpha,{'4', 4 })
	aadd(_Alpha,{'5', 5 })
	aadd(_Alpha,{'6', 6 })
	aadd(_Alpha,{'7', 7 })
	aadd(_Alpha,{'8', 8 })
	aadd(_Alpha,{'9', 9 })
	aadd(_Alpha,{'A', 10 })
	aadd(_Alpha,{'B', 11 })
	aadd(_Alpha,{'C', 12 })
	aadd(_Alpha,{'D', 13 })
	aadd(_Alpha,{'E', 14 })
	aadd(_Alpha,{'F', 15 })
	aadd(_Alpha,{'G', 16 })
	aadd(_Alpha,{'H', 17 })
	aadd(_Alpha,{'I', 18 })
	aadd(_Alpha,{'J', 19 })
	aadd(_Alpha,{'K', 20 })
	aadd(_Alpha,{'L', 21 })
	aadd(_Alpha,{'M', 22 })
	aadd(_Alpha,{'N', 23 })
	aadd(_Alpha,{'O', 24 })
	aadd(_Alpha,{'P', 25 })
	aadd(_Alpha,{'Q', 26 })
	aadd(_Alpha,{'R', 27 })
	aadd(_Alpha,{'S', 28 })
	aadd(_Alpha,{'T', 29 })
	aadd(_Alpha,{'U', 30 })
	aadd(_Alpha,{'V', 31 })
	aadd(_Alpha,{'W', 32 })
	aadd(_Alpha,{'X', 33 })
	aadd(_Alpha,{'Y', 34 })
	aadd(_Alpha,{'Z', 35 })
	aadd(_Alpha,{' ', 36 }) // (space)
	aadd(_Alpha,{'$', 37 })
	aadd(_Alpha,{'%', 38 })
	aadd(_Alpha,{'*', 39 })
	aadd(_Alpha,{'+', 40 })
	aadd(_Alpha,{'-', 41 })
	aadd(_Alpha,{'.', 42 })
	aadd(_Alpha,{'/', 43 })
	aadd(_Alpha,{':', 44 })
Endif

Return _Alpha
       
// ----------------------------------------------------------
// Modos de codificação e sequencia binária correspondente

STATIC _Modes := GetAModes()

STATIC Function GetAModeS()
Local aModes := {}

// Mode Name	Mode Indicator
aadd(aModes,{'Numeric Mode'     ,'0001'})
aadd(aModes,{'Alphanumeric Mode','0010'})
aadd(aModes,{'Byte Mode'        ,'0100'})
aadd(aModes,{'Kanji Mode'       ,'1000'})

// missing info ....
// aadd(_Modes,{'ECI Mode'         ,'0111'})

return aModes


// ------------------------------------------------------------
// Obtem o numero de bits necessarios para representar 
// o tamanho dos dados do QR Code de acordo com a versão

METHOD GetCountSize() CLASS ZQRCODE
Local nBits := 0

IF ::nVersion >= 1 .and. ::nVersion <= 9 
	IF ::nMode == 1
		nBits := 10	
	ElseIF ::nMode == 2
		nBits := 9
	ElseIF ::nMode == 3
		nBits := 8
	ElseIF ::nMode == 4
		nBits := 8
	Endif
ElseIF ::nVersion >= 10 .and. ::nVersion <= 26 
	IF ::nMode == 1
		nBits := 12
	ElseIF ::nMode == 2
		nBits := 11
	ElseIF ::nMode == 3
		nBits := 16
	ElseIF ::nMode == 4
		nBits := 10
	Endif
ElseIF ::nVersion >= 10 .and. ::nVersion <= 26 
	IF ::nMode == 1
		nBits := 14
	ElseIF ::nMode == 2
		nBits := 13
	ElseIF ::nMode == 3
		nBits := 16
	ElseIF ::nMode == 4
		nBits := 12
	Endif
Endif

::oLogger:Write("GETCOUNTSIZE","Bit Size for Version ["+cValToChar(::nVersion)+"] Mode ["+cValTochar(::nMode)+"] is "+cValToChar(nBits))

Return nBits


// Encoding modes

// https://www.thonky.com/qr-code-tutorial/numeric-mode-encoding
// https://www.thonky.com/qr-code-tutorial/alphanumeric-mode-encoding
// https://www.thonky.com/qr-code-tutorial/byte-mode-encoding


// --------------------------------------------------------------
// Monta a tabela de formato e versao em cache 

// https://www.thonky.com/qr-code-tutorial/format-version-tables
// List of all Format Information Strings
// [1] ECC Level 	[2] Mask Pattern	[3] Type Information Bits

STATIC _InfoStr := GetFmtInfo()

STATIC Function GetFmtInfo()
Local aInfoStr := {}

If _InfoStr = NIL


	aadd(aInfoStr,{'L',0,'111011111000100'})
	aadd(aInfoStr,{'L',1,'111001011110011'})
	aadd(aInfoStr,{'L',2,'111110110101010'})
	aadd(aInfoStr,{'L',3,'111100010011101'})
	aadd(aInfoStr,{'L',4,'110011000101111'})
	aadd(aInfoStr,{'L',5,'110001100011000'})
	aadd(aInfoStr,{'L',6,'110110001000001'})
	aadd(aInfoStr,{'L',7,'110100101110110'})
	aadd(aInfoStr,{'M',0,'101010000010010'})
	aadd(aInfoStr,{'M',1,'101000100100101'})
	aadd(aInfoStr,{'M',2,'101111001111100'})
	aadd(aInfoStr,{'M',3,'101101101001011'})
	aadd(aInfoStr,{'M',4,'100010111111001'})
	aadd(aInfoStr,{'M',5,'100000011001110'})
	aadd(aInfoStr,{'M',6,'100111110010111'})
	aadd(aInfoStr,{'M',7,'100101010100000'})
	aadd(aInfoStr,{'Q',0,'011010101011111'})
	aadd(aInfoStr,{'Q',1,'011000001101000'})
	aadd(aInfoStr,{'Q',2,'011111100110001'})
	aadd(aInfoStr,{'Q',3,'011101000000110'})
	aadd(aInfoStr,{'Q',4,'010010010110100'})
	aadd(aInfoStr,{'Q',5,'010000110000011'})
	aadd(aInfoStr,{'Q',6,'010111011011010'})
	aadd(aInfoStr,{'Q',7,'010101111101101'})
	aadd(aInfoStr,{'H',0,'001011010001001'})
	aadd(aInfoStr,{'H',1,'001001110111110'})
	aadd(aInfoStr,{'H',2,'001110011100111'})
	aadd(aInfoStr,{'H',3,'001100111010000'})
	aadd(aInfoStr,{'H',4,'000011101100010'})
	aadd(aInfoStr,{'H',5,'000001001010101'})
	aadd(aInfoStr,{'H',6,'000110100001100'})
	aadd(aInfoStr,{'H',7,'000100000111011'})
	
	_InfoStr := aInfoStr
	
Endif

Return _InfoStr


// --------------------------------------------------------------
// Para montar a sequencia reservada de 15 bits de um QRCode
// informe o error correcion e o Mask utilizad, e a função
// retorna os 15 bits ( como string de "0" e "1" a acrecentar

STATIC Function GetTypeBits(cErrCorr,nMask)
Local nPos 
nPos := ascan( _InfoStr , {|x| x[1] == cErrCorr .and. x[2] == nMask })
If nPos > 0
	Return _InfoStr[nPos][3]
Endif
Return ''


// ----------------------------------------------------------------
// A partir dos 15 bis de informaçáo do Error Code e Mask 
// disposta em um QRCode, determina o error code e a mascara em uso
// retorna ambos por referencia 
// Usado em leitura de QRCode 

STATIC Function GetBitsEM(cFmtInfo,cErrCorr,nMask)
Local nPos 
nPos := ascan( _InfoStr , {|x| x[3] == cFmtInfo })
If nPos > 0
	cErrCorr := _InfoStr[nPos][1]
	nMAsk := _InfoStr[nPos][2]
	Return .T.
Endif
Return .F.


// --------------------------------------------------------------
// Na geração de um QRCode, após definir os dados
// esta função gera o array de bytes em string binaria
// codificando os dados e fazendo o "padding" de acordo 
// com as propriedades do QRCode

METHOD BuildData() CLASS ZQRCODE
Local aPattern1 := {}  // top left pattern position
Local aPattern2 := {}  // top right pattern position
Local aPattern3 := {}  // bottom left pattern position
Local aAlignMask := {}     
Local aAlignPos := {}
Local nL , nC
Local aAlphaTab 
Local aModes
Local nI 
Local cChar, nPos, nVal
Local cDataBits := ''
Local cBitSize , nCntBits, nDataSize
Local nErrCW
Local aQRErrData := {}
Local aFinalData := {}
Local nInt

If ::nVersion < 1
	// Se até agora nao tinha a versão, escolhe 
	// a melhor de acordo com o error correction 
	// e tamanho dos dados de acordo com o modo de codificaçcão
	::nVersion := ::BestVersion()
Endif

::oLogger:Write("BUILDDATA","Building Data for Version ["+cValToChar(::nVersion)+"]")

// Pega o tamanho da aresta do QR Code baseado na versão 
::nSize := VerSize(::nVersion)

::oLogger:Write("BUILDDATA","QR Code Size ["+cValToChar(::nSize)+"]")

// Monta o Grid em array bi-dimensional com elementos 
// inicializados com -1 ( elemento livre) 
::aGrid := BuildGrid( ::nSize )

// Localizador superior esquerdo
aadd(aPattern1,{1,1,1,1,1,1,1,0})
aadd(aPattern1,{1,0,0,0,0,0,1,0})
aadd(aPattern1,{1,0,1,1,1,0,1,0})
aadd(aPattern1,{1,0,1,1,1,0,1,0})
aadd(aPattern1,{1,0,1,1,1,0,1,0})
aadd(aPattern1,{1,0,0,0,0,0,1,0})
aadd(aPattern1,{1,1,1,1,1,1,1,0})
aadd(aPattern1,{0,0,0,0,0,0,0,0})

// Localizador superior direito 
aadd(aPattern2,{0,1,1,1,1,1,1,1})
aadd(aPattern2,{0,1,0,0,0,0,0,1})
aadd(aPattern2,{0,1,0,1,1,1,0,1})
aadd(aPattern2,{0,1,0,1,1,1,0,1})
aadd(aPattern2,{0,1,0,1,1,1,0,1})
aadd(aPattern2,{0,1,0,0,0,0,0,1})
aadd(aPattern2,{0,1,1,1,1,1,1,1})
aadd(aPattern2,{0,0,0,0,0,0,0,0})

// Localizador inferior esquerdo 
aadd(aPattern3,{0,0,0,0,0,0,0,0})
aadd(aPattern3,{1,1,1,1,1,1,1,0})
aadd(aPattern3,{1,0,0,0,0,0,1,0})
aadd(aPattern3,{1,0,1,1,1,0,1,0})
aadd(aPattern3,{1,0,1,1,1,0,1,0})
aadd(aPattern3,{1,0,1,1,1,0,1,0})
aadd(aPattern3,{1,0,0,0,0,0,1,0})
aadd(aPattern3,{1,1,1,1,1,1,1,0})

// Coloca os Finder Patterns no lugar
// de acordo com o tamanho do QRCode
PutArray(::aGrid,aPattern1,01,01 )
PutArray(::aGrid,aPattern2,01,::nSize-7)
PutArray(::aGrid,aPattern3,::nSize-7,01 )

// Seta o Dark Pointer
// ( primeiro byte apos o localizador inferior esquerdo ) 
nL := ::nSize-7
nC := 09
::aGrid[nL,nC] := 1

::oLogger:Write("BUILDDATA","Set Dark Pointer at ["+cValToChar(nL-1)+","+cValToChar(nC-1)+"]")

// Cria e coloca os Align Patterns 
// Para versão 2 em diante 
// Os padroes de alinhamento devem ser distribuidos 
// por versao em coordenadas centrais pré-estabelecidas
// e não podem "colidir" com os localizadores 
        
If ::nVersion > 1
	
	// Cria o padrao de alinhamento 
	aadd(aAlignMask,{1,1,1,1,1})
	aadd(aAlignMask,{1,0,0,0,1})
	aadd(aAlignMask,{1,0,1,0,1})
	aadd(aAlignMask,{1,0,0,0,1})
	aadd(aAlignMask,{1,1,1,1,1})
	
	// Versa 2 ou maior, exige alinhamento
	aAlignPos := GetCenterAlign(::nVersion)
	
	// Coloca os padroes de alinhamento no Grid
	::PutAlign( aAlignPos , aAlignMask )
	
Endif

// Coloca as Time Lines
// TimeLine vertical, liga os patterns 1 e 3
For nL := 9 to ::nSize-8 STEP 2
	::aGrid[nL,7] := 1
Next
For nL := 8 to ::nSize-7 STEP 2
	::aGrid[nL,7] := 0
Next

// TimeLine Horizontal, liga os patterns 1 e 2 
For nC := 9 to ::nSize-8 STEP 2
	::aGrid[7,nC] := 1
Next
For nC := 8 to ::nSize-7 STEP 2
	::aGrid[7,nC] := 0
Next

// Recupera o array de modos 
// [1] DEscricao [2] Bits 

// agora vamos mastigar os dados e gerar bits 
// primeiro acrescenta o modo em uso nos dados

aModes := GetAModes()

// OS 4 primeiros bits do stream começam com 
// o modo ou tipo da codificaçao ( alfanumerica, bytes, ... )
cDataBits := aModes[::nMode][2]      

::oLogger:Write("BUILDDATA","Encoding Mode Bits ........ "+aModes[::nMode][2]+ " ("+aModes[::nMode][1]+")")

// Determina tamanho dos dados 
nDataSize := len(::cQRData)

::oLogger:Write("BUILDDATA","Data Buffer ............... ["+::cQRData+"] ")
::oLogger:Write("BUILDDATA","Data Size  ................ "+cValToChar(nDataSize)  )

// Converte o tamanho pra binario em 16 bits 
cBitSize := NTOBIT16(nDataSize)

// Agora determina quantos bits devem ser usados para
// representar o tamanho 
nCntBits := ::GetCountSize()

// Corta o tamanho no nimero de bits necessarios 
cBitSize := substr(cBitSize,17-nCntBits)

::oLogger:Write("BUILDDATA","Data Size Bits ............ "+cBitSize+" ("+cValToChar(nCntBits)+" bits long)")

// E acrescenta no stream de bits de dados 
cDataBits += cBitSize

If ::nMode == 2 
    
	// Modo Alfanumerico 
	// Codificação Alfanumerica com Alfabeto Restrito 

	// Recupera o alfabeto 
	aAlphaTab := GetAlpha()
	aChars := {-1,-1}

	// Varre os dados para codificar 
	For nI := 1 to nDataSize 
	
		// Pega o caactere, busca a posicao no alfabeto 
		cChar := upper(substr(::cQRData,nI,1))
		nPos := ascan(aAlphaTab,{|x| x[1] == cChar })
		IF nPos == 0
			USerException("Char ["+cChar+"] not found on alphanumeric table.")
		Endif                            
		// Pega o valor a ser codificado 
		nVal := aAlphaTab[nPos][2]

		::oLogger:Write("BUILDDATA","Char #"+cValToChar(nI)+ " ["+cChar+"] Alpha Value ["+cValToChar(nVal)+"]")

		// Monta um par de valores 
		IF aChars[1] == -1 
			aChars[1] := nVal
		Else
			aChars[2] := nVal              
			nTuple := ( aChars[1] * 45 ) + aChars[2]
			cCodeBits := Substr( NTOBIT16( nTuple ) , 6 )
			::oLogger:Write("BUILDDATA","Tuple ("+cValToChar(nTuple)+") Char Bits ["+cCodeBits+"]")
			cDataBits += cCodeBits
			aChars := {-1,-1}
		Endif
	
	Next
                      
	IF aChars[1] > 0 

		// ultimo cactere, sem par 
		// Converte direto para binario em 6 bits 
		cCodeBits := Substr(NTOBIT8(aChars[1]),3)
		::oLogger:Write("BUILDDATA","Last Char 6 Bits ["+cCodeBits+"]")
		cDataBits += cCodeBits

	Endif
		
ElseIf ::nMode == 3 // Codificação em Bytes ( ASCII / UTF8 ) 

	// Codificação em Bytes, a mais simples
	For nI := 1 to nDataSize 
		cChar := substr(::cQRData,nI,1)
		nChar := asc(cChar)
		cCodeBits := NTOBIT8( nChar )
		::oLogger:Write("BUILDDATA","Char #"+cValToChar(nI)+ " ["+cChar+"] Bits ["+cCodeBits+"]")
		cDataBits += cCodeBits
	Next


Else

	UserException("TODO")

Endif

::oLogger:Write("BUILDDATA","Encode Buffer Bits ........ "+cDataBits)
nBitSize := len(cDataBits)
::oLogger:Write("BUILDDATA","Encode Bits Size .......... "+cValToChar(nBitSize))

/*

GetECCW()

1) Version and EC Level
2) Total Number of Data Codewords for this Version and EC Level
3) EC Codewords Per Block
4) Number of Blocks in Group 1
5) Number of Data Codewords in Each of Group 1's Blocks
6) Number of Blocks in Group 2
7) Number of Data Codewords in Each of Group 2's Blocks
8) Total Data Codewords

*/

// Recupera as definições de Correção de erro
// baseado na versao atual e correção de erro desejada
aECCW := GetECCW(::nVersion,::cErrCorr)

IF aECCW = NIL
	USerException("Invalid Version or Error Correction")
Endif

nTotBlocks := val(aECCW[2])
nErrCW     := val(aECCW[3])
nBlocks    := val(aECCW[4])
aBlock8 := {}

If nBlocks > 1
	UserException("More than 1 Data Block not implemented YET")
Endif

// Informa o total de CodeWords de correção de erro necessários
::oLogger:Write("BUILDDATA","Total Data CodeWords ......... "+cValToChar(nTotBlocks))

nReqBits := nTotBlocks*8
::oLogger:Write("BUILDDATA","Data Required Bits ........... "+cValToChar(nReqBits))

If nBitSize < nReqBits
	::oLogger:Write("BUILDDATA","Add 0000 Data Bits Terminator")
	cDataBits += '0000'	
Endif

// Break Data int 8 bytes blocks 
cBitsRow := ''
while !empty(cDataBits)
	cBitsRow += left(cDataBits,1)
	cDataBits := substr(cDataBits,2)
	If len(cBitsRow) == 8
		aadd(aBlock8,cBitsRow)
		::oLogger:Write("BUILDDATA","Build CodeWord #"+cValToChar(len(aBlock8))+" ["+cBitsRow+"] ")
		cBitsRow := ''
	Endif
Enddo

// Complementa a sequencia de bits em multiplos de 8
IF !empty(cBitsRow)
	cBitsRow := left(cBitsRow+"00000000",8)
	aadd(aBlock8,cBitsRow)
	::oLogger:Write("BUILDDATA","Padding Last CodeWord #"+cValToChar(len(aBlock8))+" ["+cBitsRow+"] ")
Endif

// Preenche os blocos finais ( caso vazios )
// com um padrao de preenchimento ( filler )
While len(aBlock8) < nTotBlocks
	If len(aBlock8) < nTotBlocks
		aadd(aBlock8,"11101100")
		::oLogger:Write("BUILDDATA","Filler CodeWord #"+cValToChar(len(aBlock8))+" ["+"11101100"+"] ")
	Endif
	If len(aBlock8) < nTotBlocks
		aadd(aBlock8,"00010001")
		::oLogger:Write("BUILDDATA","Filler CodeWord #"+cValToChar(len(aBlock8))+" ["+"00010001"+"] ")
	Endif
Enddo

// Inicializa o array de inteiros que vai guardar a mensagem
// ou CodeWords da Mensagem

::oLogger:Write("BUILDDATA",padc(" Build Message CodeWord ",79,'-'))

aQRIntData := {}
cTmpCode := ''
conout("")
For nI := 1 to nTotBlocks
	BIT8TON( aBlock8[nI] , nInt )
	::oLogger:Write("BUILDDATA","CodeWord ["+str(nI,3)+"] Data ["+aBlock8[nI]+"] ("+cValToChar(nInt)+")")
	aadd( aQRIntData , nInt )
	IF nI>1
		cTmpCode += ','
	Endif
	cTmpCode += cValToChar(nInt)
Next
::oLogger:Write("BUILDDATA","Message Data .... "+cTmpCode)

::oLogger:Write("BUILDDATA","Error Correction CodeWords ... "+cValToChar(nErrCW))

// Com os dados obtidos em aQRIntData, agora vamso gerar o array de Error Correction
aQRErrData := ::BuildErrData(aQRIntData,nErrCW)

// TODO
// Agora faz o "interleave" dos dados baseado nos gupos
// por hora, sem interleave

aEval(aQRIntData,{|x| aadd(aFinalData,x) })
aEval(aQRErrData,{|x| aadd(aFinalData,x) })

::oLogger:Write("BUILDDATA","Final Data Size ........... "+cValToChar(len(aFinalData)))

// Guarda o array de Bytes de dados e Correção de Erro
// na propriedade aBitStream
::aBitStream := aClone(aFinalData)

// Limpa arrays nao mais utilizados
aSize(aFinalData,0)

Return .T.


// --------------------------------------------------------------
// Coloca nas posicoes adequadas as informacoes
// de versao e formato . Elas entram duas vezes
// no QR Code em posicoes especificas em torno
// dos Patterns de localização

METHOD PutFVInfo(cVerFmtBits,aGrid)  CLASS ZQRCODE

If aGrid = NIL
	aGrid := ::aGrid
Endif

::oLogger:Write("PUTFVINFO","Put Version and Format Bits ["+cVerFmtBits+"]")

aGrid[09][01] := val(substr(cVerFmtBits,01,1))
aGrid[09][02] := val(substr(cVerFmtBits,02,1))
aGrid[09][03] := val(substr(cVerFmtBits,03,1))
aGrid[09][04] := val(substr(cVerFmtBits,04,1))
aGrid[09][05] := val(substr(cVerFmtBits,05,1))
aGrid[09][06] := val(substr(cVerFmtBits,06,1))
aGrid[09][08] := val(substr(cVerFmtBits,07,1))
aGrid[09][09] := val(substr(cVerFmtBits,08,1))
aGrid[08][09] := val(substr(cVerFmtBits,09,1))
aGrid[06][09] := val(substr(cVerFmtBits,10,1))
aGrid[05][09] := val(substr(cVerFmtBits,11,1))
aGrid[04][09] := val(substr(cVerFmtBits,12,1))
aGrid[03][09] := val(substr(cVerFmtBits,13,1))
aGrid[02][09] := val(substr(cVerFmtBits,14,1))
aGrid[01][09] := val(substr(cVerFmtBits,15,1))

aGrid[::nSize  ][09] := val(substr(cVerFmtBits,01,1))
aGrid[::nSize-1][09] := val(substr(cVerFmtBits,02,1))
aGrid[::nSize-2][09] := val(substr(cVerFmtBits,03,1))
aGrid[::nSize-3][09] := val(substr(cVerFmtBits,04,1))
aGrid[::nSize-4][09] := val(substr(cVerFmtBits,05,1))
aGrid[::nSize-5][09] := val(substr(cVerFmtBits,06,1))
aGrid[::nSize-6][09] := val(substr(cVerFmtBits,07,1))

aGrid[09][::nSize-7] := val(substr(cVerFmtBits,08,1))
aGrid[09][::nSize-6] := val(substr(cVerFmtBits,09,1))
aGrid[09][::nSize-5] := val(substr(cVerFmtBits,10,1))
aGrid[09][::nSize-4] := val(substr(cVerFmtBits,11,1))
aGrid[09][::nSize-3] := val(substr(cVerFmtBits,12,1))
aGrid[09][::nSize-2] := val(substr(cVerFmtBits,13,1))
aGrid[09][::nSize-1] := val(substr(cVerFmtBits,14,1))
aGrid[09][::nSize  ] := val(substr(cVerFmtBits,15,1))

Return

/* --------------------------------------------------------------
BestVersion()
Encontra a melhor versão baseado na codificação escolhida
-------------------------------------------------------------- */

METHOD BestVersion() CLASS ZQRCODE 
Local nTamData 
Local nPos 
Local aLimits
Local nColOrder
Local nVersion                      
Local cErrCorr

If Empty(::cQRData) 
	UserException("QRData not set -- Unable to compute QR Version.") 
Endif

If empty(::cErrCorr)
	UserException("Error Correction not set -- Unable to compute QR Version.") 
Endif
            
IF ::nMode < 1 
	UserException("Encoding Mode not set -- Unable to compute QR Version.") 
Endif

nTamData := len(::cQRData)
nColOrder := ::nMode+2                            
cErrCorr := ::cErrCorr

// Ordena por menor tamanho de acordo com o encoding
aLimits :=  aClone(_QRLimts)
aSort(aLimits,,,{|x1,x2| x1[nColOrder] < x2[nColOrder]  })
                            
// Busca pelo menor tamanho usando o codigo de erro 
nPos := ascan(aLimits,{|x| x[2] == cErrCorr .and. x[nColOrder] >= nTamData })

nVersion := aLimits[nPos][1]

::oLogger:Write("BESTVERSION","Version ["+cValToChar(nVersion)+"] "+;
				"Limit ["+cValToChar(aLimits[nPos][nColOrder])+"] "+;
				"best for ["+cValToChar(nTamData)+"] data length.")

Return nVersion

// -----------------------------------------------
// Calcula as CodeWords de correção de erro 
// a partir do array de dados da mensagem
// Retorna Array com os Bytes de Error Correction

METHOD BuildErrData(aQRIntData,nErrCW) CLASS ZQRCODE 
Local aQRErrData := {}
Local nMsgSize 
Local aMsgPoli
Local aGenPoli
Local nFirstV 
Local nFirstA 
Local aTmpPoli 

// Pega o tamanho da mensagem 
nMsgSize := len(aQRIntData)

// Monta o polinomial para as mensagens
// [1] Valor do elemento 
// [2] expoente de x
// notacao : aQRIntData[1] * x ^ aQRIntData[2]

aMsgPoli := BuildPoli(aQRIntData)

// Multiplica o Message Polinomial usando o numero de 
// error connection codewords como expoente
MultPoliX(aMsgPoli,nErrCW)

//ShowPoli("Message",aMsgPoli)

// Usando o gerador de polinomiais, monta a equacao para 10 codigos de erro 
// 32x15 + 91x14 + 11x13 + 120x12 + 209x11 + 114x10 + 220x9 + 77x8 + 67x7 + 64x6 + 236x5 + 17x4 + 236x3 + 17x2 + 236x1 + 17(x0)
// "pow(a0*x,10)+pow(a251*x,9)+pow(a67*x,8)+pow(a46*x,7)+pow(a61*x,6)+pow(a118*x,5)+"
// "pow(a70*x,4)+pow(a64*x,3)+pow(a94*x,2)+pow(a32*x,1)+pow(a45*x,0)"
// 
// Notacao a ^ aGen[1] *  x ^ aGen[2]

aGenPoli := GetPolyGen(nErrCW)

// Multiplica o Generator Polinomial em X pelo 
// maior expoente da mesagem ( 15 , tamanho da mensagem - 1) 
nExp := len(aQRIntData)-1
MultPoliX(aGenPoli,nExp)

// ShowPoli("Base Generator",aGenPoli)
                                 
// Step 1a: Multiply the Generator Polynomial by the Lead Term of the Message Polynomial
// Usar a tabea aGFLog de log e antilog para usar a notação "alpha"
// 1o termo Mensagem  - pow(32*x,25) -> transformar 32*x em notacao alpha. 
// usando a tabela de log-antilog, "32" equivale a "a5" ( ou a^5 )
// logo o gerador polinomial deve ser multiplicado por a^5
// Pega o valor do primeiro item
// e converte para expoente alpha 

nFirstV := aMsgPoli[1][1] // 32
nFirstA := xToAlpha(nFirstV) // 5
aTmpPoli := aClone(aGenPoli)
StepGen(aGenPoli)
MultPoliA(aTmpPoli,nFirstA)

//ShowPoli("(Step 1) Generator x A("+cValToChar(nFirstA)+")",aTmpPoli)
PoliA2I(aTmpPoli)

//ShowPoli("Step (1) - Result as Integers",aTmpPoli)

// Step 1b: XOR the result with the message polynomial
// Agora faz XOR dos valores usando a mensagem contra o resultado 

aResult := PoliXOR(aMsgPoli,aTmpPoli)
//ShowPoli("Step (1) Result = XOR with Message",aResult)

For nStep := 2 to nMsgSize

	// Step 2a: Multiply the Generator Polynomial by the Lead Term of the XOR result from the previous step
	nFirstV := aResult[1][1]     
	If nFirstV > 0 
		nFirstA := xToAlpha(nFirstV) 
		aTmpPoli := aClone(aGenPoli)
		StepGen(aGenPoli)
		MultPoliA(aTmpPoli,nFirstA)
		//ShowPoli("Step ("+cValToChar(nStep)+") - Generator x A("+cValToChar(nFirstA)+")",aTmpPoli)
		PoliA2I(aTmpPoli)
		//ShowPoli("Step ("+cValToChar(nStep)+") - Result as Integers",aTmpPoli)
	Else 
		// Valor multiplicador zero 
		// Multiplicar por zero deve zerar os valores de A 
    	aEval(aTmpPoli,{|x,y| aTmpPoli[y][1] := 0})                      
	Endif

	// Step 2b: XOR the result with the result from step 1b
	aResult := PoliXOR(aResult,aTmpPoli)
	//ShowPoli("Step ("+cValToChar(nStep)+") Result = XOR with Previous Result",aResult)

Next

::oLogger:Write("BUILDDATA","===== ERROR CORRECTION CODEWORDS ======")

For nI := 1 to Len(aResult)
	nInt := aResult[nI][1]
  	cBits := NTOBIT8(nInt)
   	::oLogger:Write("BUILDDATA","CodeWord ["+str(nI,3)+"] Data ["+cBits+"] ("+cValToChar(nInt)+")")
	aadd(aQRErrData,aResult[nI][1])
Next

Return aQRErrData


/*

Error Correction Code Words and Block Information

The following table lists the number of error correction code words 
that you are required to generate for each version and error correction level 
of QR code. These values can be used to determine how many data bytes 
and error correction bytes are required for a given Reed-Solomon block.

1) Version and EC Level
2) Total Number of Data Codewords for this Version and EC Level
3) EC Codewords Per Block
4) Number of Blocks in Group 1
5) Number of Data Codewords in Each of Group 1's Blocks
6) Number of Blocks in Group 2
7) Number of Data Codewords in Each of Group 2's Blocks
8) Total Data Codewords
*/

STATIC _aECCW

// Retorna as informações reelevantes para realizar 
// a codificação dos dados e os tamanhos e limites 
// de palavras ( CodeWords ) que podem ser utilizados 

STATIC Function GetECCW(nVersion,cErrCorr)
Local cKey := cvaltochar(nVersion)+'-'+cErrCorr

If _aECCW = NIL
	
	_aECCW := {}
	
	aadd(_aECCW,{'1-L','19','7','1','19','','','(19*1) = 19'})
	aadd(_aECCW,{'1-M','16','10','1','16','','','(16*1) = 16'})
	aadd(_aECCW,{'1-Q','13','13','1','13','','','(13*1) = 13'})
	aadd(_aECCW,{'1-H','9','17','1','9','','','(9*1) = 9'})
	aadd(_aECCW,{'2-L','34','10','1','34','','','(34*1) = 34'})
	aadd(_aECCW,{'2-M','28','16','1','28','','','(28*1) = 28'})
	aadd(_aECCW,{'2-Q','22','22','1','22','','','(22*1) = 22'})
	aadd(_aECCW,{'2-H','16','28','1','16','','','(16*1) = 16'})
	aadd(_aECCW,{'3-L','55','15','1','55','','','(55*1) = 55'})
	aadd(_aECCW,{'3-M','44','26','1','44','','','(44*1) = 44'})
	aadd(_aECCW,{'3-Q','34','18','2','17','','','(17*2) = 34'})
	aadd(_aECCW,{'3-H','26','22','2','13','','','(13*2) = 26'})
	aadd(_aECCW,{'4-L','80','20','1','80','','','(80*1) = 80'})
	aadd(_aECCW,{'4-M','64','18','2','32','','','(32*2) = 64'})
	aadd(_aECCW,{'4-Q','48','26','2','24','','','(24*2) = 48'})
	aadd(_aECCW,{'4-H','36','16','4','9','','','(9*4) = 36'})
	aadd(_aECCW,{'5-L','108','26','1','108','','','(108*1) = 108'})
	aadd(_aECCW,{'5-M','86','24','2','43','','','(43*2) = 86'})
	aadd(_aECCW,{'5-Q','62','18','2','15','2','16','(15*2) + (16*2) = 62'})
	aadd(_aECCW,{'5-H','46','22','2','11','2','12','(11*2) + (12*2) = 46'})
	aadd(_aECCW,{'6-L','136','18','2','68','','','(68*2) = 136'})
	aadd(_aECCW,{'6-M','108','16','4','27','','','(27*4) = 108'})
	aadd(_aECCW,{'6-Q','76','24','4','19','','','(19*4) = 76'})
	aadd(_aECCW,{'6-H','60','28','4','15','','','(15*4) = 60'})
	aadd(_aECCW,{'7-L','156','20','2','78','','','(78*2) = 156'})
	aadd(_aECCW,{'7-M','124','18','4','31','','','(31*4) = 124'})
	aadd(_aECCW,{'7-Q','88','18','2','14','4','15','(14*2) + (15*4) = 88'})
	aadd(_aECCW,{'7-H','66','26','4','13','1','14','(13*4) + (14*1) = 66'})
	aadd(_aECCW,{'8-L','194','24','2','97','','','(97*2) = 194'})
	aadd(_aECCW,{'8-M','154','22','2','38','2','39','(38*2) + (39*2) = 154'})
	aadd(_aECCW,{'8-Q','110','22','4','18','2','19','(18*4) + (19*2) = 110'})
	aadd(_aECCW,{'8-H','86','26','4','14','2','15','(14*4) + (15*2) = 86'})
	aadd(_aECCW,{'9-L','232','30','2','116','','','(116*2) = 232'})
	aadd(_aECCW,{'9-M','182','22','3','36','2','37','(36*3) + (37*2) = 182'})
	aadd(_aECCW,{'9-Q','132','20','4','16','4','17','(16*4) + (17*4) = 132'})
	aadd(_aECCW,{'9-H','100','24','4','12','4','13','(12*4) + (13*4) = 100'})
	aadd(_aECCW,{'10-L','274','18','2','68','2','69','(68*2) + (69*2) = 274'})
	aadd(_aECCW,{'10-M','216','26','4','43','1','44','(43*4) + (44*1) = 216'})
	aadd(_aECCW,{'10-Q','154','24','6','19','2','20','(19*6) + (20*2) = 154'})
	aadd(_aECCW,{'10-H','122','28','6','15','2','16','(15*6) + (16*2) = 122'})
	aadd(_aECCW,{'11-L','324','20','4','81','','','(81*4) = 324'})
	aadd(_aECCW,{'11-M','254','30','1','50','4','51','(50*1) + (51*4) = 254'})
	aadd(_aECCW,{'11-Q','180','28','4','22','4','23','(22*4) + (23*4) = 180'})
	aadd(_aECCW,{'11-H','140','24','3','12','8','13','(12*3) + (13*8) = 140'})
	aadd(_aECCW,{'12-L','370','24','2','92','2','93','(92*2) + (93*2) = 370'})
	aadd(_aECCW,{'12-M','290','22','6','36','2','37','(36*6) + (37*2) = 290'})
	aadd(_aECCW,{'12-Q','206','26','4','20','6','21','(20*4) + (21*6) = 206'})
	aadd(_aECCW,{'12-H','158','28','7','14','4','15','(14*7) + (15*4) = 158'})
	aadd(_aECCW,{'13-L','428','26','4','107','','','(107*4) = 428'})
	aadd(_aECCW,{'13-M','334','22','8','37','1','38','(37*8) + (38*1) = 334'})
	aadd(_aECCW,{'13-Q','244','24','8','20','4','21','(20*8) + (21*4) = 244'})
	aadd(_aECCW,{'13-H','180','22','12','11','4','12','(11*12) + (12*4) = 180'})
	aadd(_aECCW,{'14-L','461','30','3','115','1','116','(115*3) + (116*1) = 461'})
	aadd(_aECCW,{'14-M','365','24','4','40','5','41','(40*4) + (41*5) = 365'})
	aadd(_aECCW,{'14-Q','261','20','11','16','5','17','(16*11) + (17*5) = 261'})
	aadd(_aECCW,{'14-H','197','24','11','12','5','13','(12*11) + (13*5) = 197'})
	aadd(_aECCW,{'15-L','523','22','5','87','1','88','(87*5) + (88*1) = 523'})
	aadd(_aECCW,{'15-M','415','24','5','41','5','42','(41*5) + (42*5) = 415'})
	aadd(_aECCW,{'15-Q','295','30','5','24','7','25','(24*5) + (25*7) = 295'})
	aadd(_aECCW,{'15-H','223','24','11','12','7','13','(12*11) + (13*7) = 223'})
	aadd(_aECCW,{'16-L','589','24','5','98','1','99','(98*5) + (99*1) = 589'})
	aadd(_aECCW,{'16-M','453','28','7','45','3','46','(45*7) + (46*3) = 453'})
	aadd(_aECCW,{'16-Q','325','24','15','19','2','20','(19*15) + (20*2) = 325'})
	aadd(_aECCW,{'16-H','253','30','3','15','13','16','(15*3) + (16*13) = 253'})
	aadd(_aECCW,{'17-L','647','28','1','107','5','108','(107*1) + (108*5) = 647'})
	aadd(_aECCW,{'17-M','507','28','10','46','1','47','(46*10) + (47*1) = 507'})
	aadd(_aECCW,{'17-Q','367','28','1','22','15','23','(22*1) + (23*15) = 367'})
	aadd(_aECCW,{'17-H','283','28','2','14','17','15','(14*2) + (15*17) = 283'})
	aadd(_aECCW,{'18-L','721','30','5','120','1','121','(120*5) + (121*1) = 721'})
	aadd(_aECCW,{'18-M','563','26','9','43','4','44','(43*9) + (44*4) = 563'})
	aadd(_aECCW,{'18-Q','397','28','17','22','1','23','(22*17) + (23*1) = 397'})
	aadd(_aECCW,{'18-H','313','28','2','14','19','15','(14*2) + (15*19) = 313'})
	aadd(_aECCW,{'19-L','795','28','3','113','4','114','(113*3) + (114*4) = 795'})
	aadd(_aECCW,{'19-M','627','26','3','44','11','45','(44*3) + (45*11) = 627'})
	aadd(_aECCW,{'19-Q','445','26','17','21','4','22','(21*17) + (22*4) = 445'})
	aadd(_aECCW,{'19-H','341','26','9','13','16','14','(13*9) + (14*16) = 341'})
	aadd(_aECCW,{'20-L','861','28','3','107','5','108','(107*3) + (108*5) = 861'})
	aadd(_aECCW,{'20-M','669','26','3','41','13','42','(41*3) + (42*13) = 669'})
	aadd(_aECCW,{'20-Q','485','30','15','24','5','25','(24*15) + (25*5) = 485'})
	aadd(_aECCW,{'20-H','385','28','15','15','10','16','(15*15) + (16*10) = 385'})
	aadd(_aECCW,{'21-L','932','28','4','116','4','117','(116*4) + (117*4) = 932'})
	aadd(_aECCW,{'21-M','714','26','17','42','','','(42*17) = 714'})
	aadd(_aECCW,{'21-Q','512','28','17','22','6','23','(22*17) + (23*6) = 512'})
	aadd(_aECCW,{'21-H','406','30','19','16','6','17','(16*19) + (17*6) = 406'})
	aadd(_aECCW,{'22-L','1006','28','2','111','7','112','(111*2) + (112*7) = 1006'})
	aadd(_aECCW,{'22-M','782','28','17','46','','','(46*17) = 782'})
	aadd(_aECCW,{'22-Q','568','30','7','24','16','25','(24*7) + (25*16) = 568'})
	aadd(_aECCW,{'22-H','442','24','34','13','','','(13*34) = 442'})
	aadd(_aECCW,{'23-L','1094','30','4','121','5','122','(121*4) + (122*5) = 1094'})
	aadd(_aECCW,{'23-M','860','28','4','47','14','48','(47*4) + (48*14) = 860'})
	aadd(_aECCW,{'23-Q','614','30','11','24','14','25','(24*11) + (25*14) = 614'})
	aadd(_aECCW,{'23-H','464','30','16','15','14','16','(15*16) + (16*14) = 464'})
	aadd(_aECCW,{'24-L','1174','30','6','117','4','118','(117*6) + (118*4) = 1174'})
	aadd(_aECCW,{'24-M','914','28','6','45','14','46','(45*6) + (46*14) = 914'})
	aadd(_aECCW,{'24-Q','664','30','11','24','16','25','(24*11) + (25*16) = 664'})
	aadd(_aECCW,{'24-H','514','30','30','16','2','17','(16*30) + (17*2) = 514'})
	aadd(_aECCW,{'25-L','1276','26','8','106','4','107','(106*8) + (107*4) = 1276'})
	aadd(_aECCW,{'25-M','1000','28','8','47','13','48','(47*8) + (48*13) = 1000'})
	aadd(_aECCW,{'25-Q','718','30','7','24','22','25','(24*7) + (25*22) = 718'})
	aadd(_aECCW,{'25-H','538','30','22','15','13','16','(15*22) + (16*13) = 538'})
	aadd(_aECCW,{'26-L','1370','28','10','114','2','115','(114*10) + (115*2) = 1370'})
	aadd(_aECCW,{'26-M','1062','28','19','46','4','47','(46*19) + (47*4) = 1062'})
	aadd(_aECCW,{'26-Q','754','28','28','22','6','23','(22*28) + (23*6) = 754'})
	aadd(_aECCW,{'26-H','596','30','33','16','4','17','(16*33) + (17*4) = 596'})
	aadd(_aECCW,{'27-L','1468','30','8','122','4','123','(122*8) + (123*4) = 1468'})
	aadd(_aECCW,{'27-M','1128','28','22','45','3','46','(45*22) + (46*3) = 1128'})
	aadd(_aECCW,{'27-Q','808','30','8','23','26','24','(23*8) + (24*26) = 808'})
	aadd(_aECCW,{'27-H','628','30','12','15','28','16','(15*12) + (16*28) = 628'})
	aadd(_aECCW,{'28-L','1531','30','3','117','10','118','(117*3) + (118*10) = 1531'})
	aadd(_aECCW,{'28-M','1193','28','3','45','23','46','(45*3) + (46*23) = 1193'})
	aadd(_aECCW,{'28-Q','871','30','4','24','31','25','(24*4) + (25*31) = 871'})
	aadd(_aECCW,{'28-H','661','30','11','15','31','16','(15*11) + (16*31) = 661'})
	aadd(_aECCW,{'29-L','1631','30','7','116','7','117','(116*7) + (117*7) = 1631'})
	aadd(_aECCW,{'29-M','1267','28','21','45','7','46','(45*21) + (46*7) = 1267'})
	aadd(_aECCW,{'29-Q','911','30','1','23','37','24','(23*1) + (24*37) = 911'})
	aadd(_aECCW,{'29-H','701','30','19','15','26','16','(15*19) + (16*26) = 701'})
	aadd(_aECCW,{'30-L','1735','30','5','115','10','116','(115*5) + (116*10) = 1735'})
	aadd(_aECCW,{'30-M','1373','28','19','47','10','48','(47*19) + (48*10) = 1373'})
	aadd(_aECCW,{'30-Q','985','30','15','24','25','25','(24*15) + (25*25) = 985'})
	aadd(_aECCW,{'30-H','745','30','23','15','25','16','(15*23) + (16*25) = 745'})
	aadd(_aECCW,{'31-L','1843','30','13','115','3','116','(115*13) + (116*3) = 1843'})
	aadd(_aECCW,{'31-M','1455','28','2','46','29','47','(46*2) + (47*29) = 1455'})
	aadd(_aECCW,{'31-Q','1033','30','42','24','1','25','(24*42) + (25*1) = 1033'})
	aadd(_aECCW,{'31-H','793','30','23','15','28','16','(15*23) + (16*28) = 793'})
	aadd(_aECCW,{'32-L','1955','30','17','115','','','(115*17) = 1955'})
	aadd(_aECCW,{'32-M','1541','28','10','46','23','47','(46*10) + (47*23) = 1541'})
	aadd(_aECCW,{'32-Q','1115','30','10','24','35','25','(24*10) + (25*35) = 1115'})
	aadd(_aECCW,{'32-H','845','30','19','15','35','16','(15*19) + (16*35) = 845'})
	aadd(_aECCW,{'33-L','2071','30','17','115','1','116','(115*17) + (116*1) = 2071'})
	aadd(_aECCW,{'33-M','1631','28','14','46','21','47','(46*14) + (47*21) = 1631'})
	aadd(_aECCW,{'33-Q','1171','30','29','24','19','25','(24*29) + (25*19) = 1171'})
	aadd(_aECCW,{'33-H','901','30','11','15','46','16','(15*11) + (16*46) = 901'})
	aadd(_aECCW,{'34-L','2191','30','13','115','6','116','(115*13) + (116*6) = 2191'})
	aadd(_aECCW,{'34-M','1725','28','14','46','23','47','(46*14) + (47*23) = 1725'})
	aadd(_aECCW,{'34-Q','1231','30','44','24','7','25','(24*44) + (25*7) = 1231'})
	aadd(_aECCW,{'34-H','961','30','59','16','1','17','(16*59) + (17*1) = 961'})
	aadd(_aECCW,{'35-L','2306','30','12','121','7','122','(121*12) + (122*7) = 2306'})
	aadd(_aECCW,{'35-M','1812','28','12','47','26','48','(47*12) + (48*26) = 1812'})
	aadd(_aECCW,{'35-Q','1286','30','39','24','14','25','(24*39) + (25*14) = 1286'})
	aadd(_aECCW,{'35-H','986','30','22','15','41','16','(15*22) + (16*41) = 986'})
	aadd(_aECCW,{'36-L','2434','30','6','121','14','122','(121*6) + (122*14) = 2434'})
	aadd(_aECCW,{'36-M','1914','28','6','47','34','48','(47*6) + (48*34) = 1914'})
	aadd(_aECCW,{'36-Q','1354','30','46','24','10','25','(24*46) + (25*10) = 1354'})
	aadd(_aECCW,{'36-H','1054','30','2','15','64','16','(15*2) + (16*64) = 1054'})
	aadd(_aECCW,{'37-L','2566','30','17','122','4','123','(122*17) + (123*4) = 2566'})
	aadd(_aECCW,{'37-M','1992','28','29','46','14','47','(46*29) + (47*14) = 1992'})
	aadd(_aECCW,{'37-Q','1426','30','49','24','10','25','(24*49) + (25*10) = 1426'})
	aadd(_aECCW,{'37-H','1096','30','24','15','46','16','(15*24) + (16*46) = 1096'})
	aadd(_aECCW,{'38-L','2702','30','4','122','18','123','(122*4) + (123*18) = 2702'})
	aadd(_aECCW,{'38-M','2102','28','13','46','32','47','(46*13) + (47*32) = 2102'})
	aadd(_aECCW,{'38-Q','1502','30','48','24','14','25','(24*48) + (25*14) = 1502'})
	aadd(_aECCW,{'38-H','1142','30','42','15','32','16','(15*42) + (16*32) = 1142'})
	aadd(_aECCW,{'39-L','2812','30','20','117','4','118','(117*20) + (118*4) = 2812'})
	aadd(_aECCW,{'39-M','2216','28','40','47','7','48','(47*40) + (48*7) = 2216'})
	aadd(_aECCW,{'39-Q','1582','30','43','24','22','25','(24*43) + (25*22) = 1582'})
	aadd(_aECCW,{'39-H','1222','30','10','15','67','16','(15*10) + (16*67) = 1222'})
	aadd(_aECCW,{'40-L','2956','30','19','118','6','119','(118*19) + (119*6) = 2956'})
	aadd(_aECCW,{'40-M','2334','28','18','47','31','48','(47*18) + (48*31) = 2334'})
	aadd(_aECCW,{'40-Q','1666','30','34','24','34','25','(24*34) + (25*34) = 1666'})
	aadd(_aECCW,{'40-H','1276','30','20','15','61','16','(15*20) + (16*61) = 1276'})
	
Endif

// Busca as informações reelevantes baseado na chave
// montada com a versao e o nivel de correção de erro 
nPos := ascan( _aECCW , {|x| x[1] == cKey })
If nPos > 0
	Return aClone(_aECCW[nPos])
Endif

Return NIL

// ------------------------------------------------------------
// Converte GF com Notacao inteiro para notação alpha 
// Array de Cache inicia em base 1
STATIC Function xToAlpha(nX)
Return _GFInt2A[nX]


// ------------------------------------------------------------
// Converte GF de Notacao alpha para inteiro 
// Array em cache inicia em base 0 
STATIC Function AlphaToX(nA)
Return _GFA2Int[nA+1]

// ------------------------------------------------------------
// Tabela de logs e anti-logs de conversao 
// de inteiro * x para a^n 

STATIC _GFA2Int 
STATIC _GFInt2A 

// ------------------------------------------------------------
// Inicializa os caches de conversao de inteiro para notação 
// alpha e vice-versa
// ( QR Code Logs and Antilogs for Galois Field 256 ) 

STATIC _Dummy_ := InitGFA() 
                            
STATIC Function InitGFA()
_GFA2Int := GetGFA2Int()
_GFInt2A := GetGFInt2A()
Return

// ------------------------------------------------------------
// Tabela de conversão de notação Alpha para numérico

STATIC Function GetGFA2Int()
Local aGFA2Int := {}
       
aadd(aGFA2Int,1) // a0 = 1
aadd(aGFA2Int,2) // a1 = 2 ...
aadd(aGFA2Int,4)
aadd(aGFA2Int,8)
aadd(aGFA2Int,16)
aadd(aGFA2Int,32)
aadd(aGFA2Int,64)
aadd(aGFA2Int,128)
aadd(aGFA2Int,29)
aadd(aGFA2Int,58)
aadd(aGFA2Int,116)
aadd(aGFA2Int,232)
aadd(aGFA2Int,205)
aadd(aGFA2Int,135)
aadd(aGFA2Int,19)
aadd(aGFA2Int,38)
aadd(aGFA2Int,76)
aadd(aGFA2Int,152)
aadd(aGFA2Int,45)
aadd(aGFA2Int,90)
aadd(aGFA2Int,180)
aadd(aGFA2Int,117)
aadd(aGFA2Int,234)
aadd(aGFA2Int,201)
aadd(aGFA2Int,143)
aadd(aGFA2Int,3)
aadd(aGFA2Int,6)
aadd(aGFA2Int,12)
aadd(aGFA2Int,24)
aadd(aGFA2Int,48)
aadd(aGFA2Int,96)
aadd(aGFA2Int,192)
aadd(aGFA2Int,157)
aadd(aGFA2Int,39)
aadd(aGFA2Int,78)
aadd(aGFA2Int,156)
aadd(aGFA2Int,37)
aadd(aGFA2Int,74)
aadd(aGFA2Int,148)
aadd(aGFA2Int,53)
aadd(aGFA2Int,106)
aadd(aGFA2Int,212)
aadd(aGFA2Int,181)
aadd(aGFA2Int,119)
aadd(aGFA2Int,238)
aadd(aGFA2Int,193)
aadd(aGFA2Int,159)
aadd(aGFA2Int,35)
aadd(aGFA2Int,70)
aadd(aGFA2Int,140)
aadd(aGFA2Int,5)
aadd(aGFA2Int,10)
aadd(aGFA2Int,20)
aadd(aGFA2Int,40)
aadd(aGFA2Int,80)
aadd(aGFA2Int,160)
aadd(aGFA2Int,93)
aadd(aGFA2Int,186)
aadd(aGFA2Int,105)
aadd(aGFA2Int,210)
aadd(aGFA2Int,185)
aadd(aGFA2Int,111)
aadd(aGFA2Int,222)
aadd(aGFA2Int,161)
aadd(aGFA2Int,95)
aadd(aGFA2Int,190)
aadd(aGFA2Int,97)
aadd(aGFA2Int,194)
aadd(aGFA2Int,153)
aadd(aGFA2Int,47)
aadd(aGFA2Int,94)
aadd(aGFA2Int,188)
aadd(aGFA2Int,101)
aadd(aGFA2Int,202)
aadd(aGFA2Int,137)
aadd(aGFA2Int,15)
aadd(aGFA2Int,30)
aadd(aGFA2Int,60)
aadd(aGFA2Int,120)
aadd(aGFA2Int,240)
aadd(aGFA2Int,253)
aadd(aGFA2Int,231)
aadd(aGFA2Int,211)
aadd(aGFA2Int,187)
aadd(aGFA2Int,107)
aadd(aGFA2Int,214)
aadd(aGFA2Int,177)
aadd(aGFA2Int,127)
aadd(aGFA2Int,254)
aadd(aGFA2Int,225)
aadd(aGFA2Int,223)
aadd(aGFA2Int,163)
aadd(aGFA2Int,91)
aadd(aGFA2Int,182)
aadd(aGFA2Int,113)
aadd(aGFA2Int,226)
aadd(aGFA2Int,217)
aadd(aGFA2Int,175)
aadd(aGFA2Int,67)
aadd(aGFA2Int,134)
aadd(aGFA2Int,17)
aadd(aGFA2Int,34)
aadd(aGFA2Int,68)
aadd(aGFA2Int,136)
aadd(aGFA2Int,13)
aadd(aGFA2Int,26)
aadd(aGFA2Int,52)
aadd(aGFA2Int,104)
aadd(aGFA2Int,208)
aadd(aGFA2Int,189)
aadd(aGFA2Int,103)
aadd(aGFA2Int,206)
aadd(aGFA2Int,129)
aadd(aGFA2Int,31)
aadd(aGFA2Int,62)
aadd(aGFA2Int,124)
aadd(aGFA2Int,248)
aadd(aGFA2Int,237)
aadd(aGFA2Int,199)
aadd(aGFA2Int,147)
aadd(aGFA2Int,59)
aadd(aGFA2Int,118)
aadd(aGFA2Int,236)
aadd(aGFA2Int,197)
aadd(aGFA2Int,151)
aadd(aGFA2Int,51)
aadd(aGFA2Int,102)
aadd(aGFA2Int,204)
aadd(aGFA2Int,133)
aadd(aGFA2Int,23)
aadd(aGFA2Int,46)
aadd(aGFA2Int,92)
aadd(aGFA2Int,184)
aadd(aGFA2Int,109)
aadd(aGFA2Int,218)
aadd(aGFA2Int,169)
aadd(aGFA2Int,79)
aadd(aGFA2Int,158)
aadd(aGFA2Int,33)
aadd(aGFA2Int,66)
aadd(aGFA2Int,132)
aadd(aGFA2Int,21)
aadd(aGFA2Int,42)
aadd(aGFA2Int,84)
aadd(aGFA2Int,168)
aadd(aGFA2Int,77)
aadd(aGFA2Int,154)
aadd(aGFA2Int,41)
aadd(aGFA2Int,82)
aadd(aGFA2Int,164)
aadd(aGFA2Int,85)
aadd(aGFA2Int,170)
aadd(aGFA2Int,73)
aadd(aGFA2Int,146)
aadd(aGFA2Int,57)
aadd(aGFA2Int,114)
aadd(aGFA2Int,228)
aadd(aGFA2Int,213)
aadd(aGFA2Int,183)
aadd(aGFA2Int,115)
aadd(aGFA2Int,230)
aadd(aGFA2Int,209)
aadd(aGFA2Int,191)
aadd(aGFA2Int,99)
aadd(aGFA2Int,198)
aadd(aGFA2Int,145)
aadd(aGFA2Int,63)
aadd(aGFA2Int,126)
aadd(aGFA2Int,252)
aadd(aGFA2Int,229)
aadd(aGFA2Int,215)
aadd(aGFA2Int,179)
aadd(aGFA2Int,123)
aadd(aGFA2Int,246)
aadd(aGFA2Int,241)
aadd(aGFA2Int,255)
aadd(aGFA2Int,227)
aadd(aGFA2Int,219)
aadd(aGFA2Int,171)
aadd(aGFA2Int,75)
aadd(aGFA2Int,150)
aadd(aGFA2Int,49)
aadd(aGFA2Int,98)
aadd(aGFA2Int,196)
aadd(aGFA2Int,149)
aadd(aGFA2Int,55)
aadd(aGFA2Int,110)
aadd(aGFA2Int,220)
aadd(aGFA2Int,165)
aadd(aGFA2Int,87)
aadd(aGFA2Int,174)
aadd(aGFA2Int,65)
aadd(aGFA2Int,130)
aadd(aGFA2Int,25)
aadd(aGFA2Int,50)
aadd(aGFA2Int,100)
aadd(aGFA2Int,200)
aadd(aGFA2Int,141)
aadd(aGFA2Int,7)
aadd(aGFA2Int,14)
aadd(aGFA2Int,28)
aadd(aGFA2Int,56)
aadd(aGFA2Int,112)
aadd(aGFA2Int,224)
aadd(aGFA2Int,221)
aadd(aGFA2Int,167)
aadd(aGFA2Int,83)
aadd(aGFA2Int,166)
aadd(aGFA2Int,81)
aadd(aGFA2Int,162)
aadd(aGFA2Int,89)
aadd(aGFA2Int,178)
aadd(aGFA2Int,121)
aadd(aGFA2Int,242)
aadd(aGFA2Int,249)
aadd(aGFA2Int,239)
aadd(aGFA2Int,195)
aadd(aGFA2Int,155)
aadd(aGFA2Int,43)
aadd(aGFA2Int,86)
aadd(aGFA2Int,172)
aadd(aGFA2Int,69)
aadd(aGFA2Int,138)
aadd(aGFA2Int,9)
aadd(aGFA2Int,18)
aadd(aGFA2Int,36)
aadd(aGFA2Int,72)
aadd(aGFA2Int,144)
aadd(aGFA2Int,61)
aadd(aGFA2Int,122)
aadd(aGFA2Int,244)
aadd(aGFA2Int,245)
aadd(aGFA2Int,247)
aadd(aGFA2Int,243)
aadd(aGFA2Int,251)
aadd(aGFA2Int,235)
aadd(aGFA2Int,203)
aadd(aGFA2Int,139)
aadd(aGFA2Int,11)
aadd(aGFA2Int,22)
aadd(aGFA2Int,44)
aadd(aGFA2Int,88)
aadd(aGFA2Int,176)
aadd(aGFA2Int,125)
aadd(aGFA2Int,250)
aadd(aGFA2Int,233)
aadd(aGFA2Int,207)
aadd(aGFA2Int,131)
aadd(aGFA2Int,27)
aadd(aGFA2Int,54)
aadd(aGFA2Int,108)
aadd(aGFA2Int,216)
aadd(aGFA2Int,173)
aadd(aGFA2Int,71)
aadd(aGFA2Int,142)
aadd(aGFA2Int,1)

Return aGFA2Int
                  
// ------------------------------------------------------------
// Tabela de conversão de numérico para notação Alpha 

STATIC Function GetGFInt2A()
Local aGFInt2A := {}
                
aadd(aGFInt2A,0) // 1 = a0 
aadd(aGFInt2A,1) // 2 = a1
aadd(aGFInt2A,25)
aadd(aGFInt2A,2)
aadd(aGFInt2A,50)
aadd(aGFInt2A,26)
aadd(aGFInt2A,198)
aadd(aGFInt2A,3)
aadd(aGFInt2A,223)
aadd(aGFInt2A,51)
aadd(aGFInt2A,238)
aadd(aGFInt2A,27)
aadd(aGFInt2A,104)
aadd(aGFInt2A,199)
aadd(aGFInt2A,75)
aadd(aGFInt2A,4)
aadd(aGFInt2A,100)
aadd(aGFInt2A,224)
aadd(aGFInt2A,14)
aadd(aGFInt2A,52)
aadd(aGFInt2A,141)
aadd(aGFInt2A,239)
aadd(aGFInt2A,129)
aadd(aGFInt2A,28)
aadd(aGFInt2A,193)
aadd(aGFInt2A,105)
aadd(aGFInt2A,248)
aadd(aGFInt2A,200)
aadd(aGFInt2A,8)
aadd(aGFInt2A,76)
aadd(aGFInt2A,113)
aadd(aGFInt2A,5)
aadd(aGFInt2A,138)
aadd(aGFInt2A,101)
aadd(aGFInt2A,47)
aadd(aGFInt2A,225)
aadd(aGFInt2A,36)
aadd(aGFInt2A,15)
aadd(aGFInt2A,33)
aadd(aGFInt2A,53)
aadd(aGFInt2A,147)
aadd(aGFInt2A,142)
aadd(aGFInt2A,218)
aadd(aGFInt2A,240)
aadd(aGFInt2A,18)
aadd(aGFInt2A,130)
aadd(aGFInt2A,69)
aadd(aGFInt2A,29)
aadd(aGFInt2A,181)
aadd(aGFInt2A,194)
aadd(aGFInt2A,125)
aadd(aGFInt2A,106)
aadd(aGFInt2A,39)
aadd(aGFInt2A,249)
aadd(aGFInt2A,185)
aadd(aGFInt2A,201)
aadd(aGFInt2A,154)
aadd(aGFInt2A,9)
aadd(aGFInt2A,120)
aadd(aGFInt2A,77)
aadd(aGFInt2A,228)
aadd(aGFInt2A,114)
aadd(aGFInt2A,166)
aadd(aGFInt2A,6)
aadd(aGFInt2A,191)
aadd(aGFInt2A,139)
aadd(aGFInt2A,98)
aadd(aGFInt2A,102)
aadd(aGFInt2A,221)
aadd(aGFInt2A,48)
aadd(aGFInt2A,253)
aadd(aGFInt2A,226)
aadd(aGFInt2A,152)
aadd(aGFInt2A,37)
aadd(aGFInt2A,179)
aadd(aGFInt2A,16)
aadd(aGFInt2A,145)
aadd(aGFInt2A,34)
aadd(aGFInt2A,136)
aadd(aGFInt2A,54)
aadd(aGFInt2A,208)
aadd(aGFInt2A,148)
aadd(aGFInt2A,206)
aadd(aGFInt2A,143)
aadd(aGFInt2A,150)
aadd(aGFInt2A,219)
aadd(aGFInt2A,189)
aadd(aGFInt2A,241)
aadd(aGFInt2A,210)
aadd(aGFInt2A,19)
aadd(aGFInt2A,92)
aadd(aGFInt2A,131)
aadd(aGFInt2A,56)
aadd(aGFInt2A,70)
aadd(aGFInt2A,64)
aadd(aGFInt2A,30)
aadd(aGFInt2A,66)
aadd(aGFInt2A,182)
aadd(aGFInt2A,163)
aadd(aGFInt2A,195)
aadd(aGFInt2A,72)
aadd(aGFInt2A,126)
aadd(aGFInt2A,110)
aadd(aGFInt2A,107)
aadd(aGFInt2A,58)
aadd(aGFInt2A,40)
aadd(aGFInt2A,84)
aadd(aGFInt2A,250)
aadd(aGFInt2A,133)
aadd(aGFInt2A,186)
aadd(aGFInt2A,61)
aadd(aGFInt2A,202)
aadd(aGFInt2A,94)
aadd(aGFInt2A,155)
aadd(aGFInt2A,159)
aadd(aGFInt2A,10)
aadd(aGFInt2A,21)
aadd(aGFInt2A,121)
aadd(aGFInt2A,43)
aadd(aGFInt2A,78)
aadd(aGFInt2A,212)
aadd(aGFInt2A,229)
aadd(aGFInt2A,172)
aadd(aGFInt2A,115)
aadd(aGFInt2A,243)
aadd(aGFInt2A,167)
aadd(aGFInt2A,87)
aadd(aGFInt2A,7)
aadd(aGFInt2A,112)
aadd(aGFInt2A,192)
aadd(aGFInt2A,247)
aadd(aGFInt2A,140)
aadd(aGFInt2A,128)
aadd(aGFInt2A,99)
aadd(aGFInt2A,13)
aadd(aGFInt2A,103)
aadd(aGFInt2A,74)
aadd(aGFInt2A,222)
aadd(aGFInt2A,237)
aadd(aGFInt2A,49)
aadd(aGFInt2A,197)
aadd(aGFInt2A,254)
aadd(aGFInt2A,24)
aadd(aGFInt2A,227)
aadd(aGFInt2A,165)
aadd(aGFInt2A,153)
aadd(aGFInt2A,119)
aadd(aGFInt2A,38)
aadd(aGFInt2A,184)
aadd(aGFInt2A,180)
aadd(aGFInt2A,124)
aadd(aGFInt2A,17)
aadd(aGFInt2A,68)
aadd(aGFInt2A,146)
aadd(aGFInt2A,217)
aadd(aGFInt2A,35)
aadd(aGFInt2A,32)
aadd(aGFInt2A,137)
aadd(aGFInt2A,46)
aadd(aGFInt2A,55)
aadd(aGFInt2A,63)
aadd(aGFInt2A,209)
aadd(aGFInt2A,91)
aadd(aGFInt2A,149)
aadd(aGFInt2A,188)
aadd(aGFInt2A,207)
aadd(aGFInt2A,205)
aadd(aGFInt2A,144)
aadd(aGFInt2A,135)
aadd(aGFInt2A,151)
aadd(aGFInt2A,178)
aadd(aGFInt2A,220)
aadd(aGFInt2A,252)
aadd(aGFInt2A,190)
aadd(aGFInt2A,97)
aadd(aGFInt2A,242)
aadd(aGFInt2A,86)
aadd(aGFInt2A,211)
aadd(aGFInt2A,171)
aadd(aGFInt2A,20)
aadd(aGFInt2A,42)
aadd(aGFInt2A,93)
aadd(aGFInt2A,158)
aadd(aGFInt2A,132)
aadd(aGFInt2A,60)
aadd(aGFInt2A,57)
aadd(aGFInt2A,83)
aadd(aGFInt2A,71)
aadd(aGFInt2A,109)
aadd(aGFInt2A,65)
aadd(aGFInt2A,162)
aadd(aGFInt2A,31)
aadd(aGFInt2A,45)
aadd(aGFInt2A,67)
aadd(aGFInt2A,216)
aadd(aGFInt2A,183)
aadd(aGFInt2A,123)
aadd(aGFInt2A,164)
aadd(aGFInt2A,118)
aadd(aGFInt2A,196)
aadd(aGFInt2A,23)
aadd(aGFInt2A,73)
aadd(aGFInt2A,236)
aadd(aGFInt2A,127)
aadd(aGFInt2A,12)
aadd(aGFInt2A,111)
aadd(aGFInt2A,246)
aadd(aGFInt2A,108)
aadd(aGFInt2A,161)
aadd(aGFInt2A,59)
aadd(aGFInt2A,82)
aadd(aGFInt2A,41)
aadd(aGFInt2A,157)
aadd(aGFInt2A,85)
aadd(aGFInt2A,170)
aadd(aGFInt2A,251)
aadd(aGFInt2A,96)
aadd(aGFInt2A,134)
aadd(aGFInt2A,177)
aadd(aGFInt2A,187)
aadd(aGFInt2A,204)
aadd(aGFInt2A,62)
aadd(aGFInt2A,90)
aadd(aGFInt2A,203)
aadd(aGFInt2A,89)
aadd(aGFInt2A,95)
aadd(aGFInt2A,176)
aadd(aGFInt2A,156)
aadd(aGFInt2A,169)
aadd(aGFInt2A,160)
aadd(aGFInt2A,81)
aadd(aGFInt2A,11)
aadd(aGFInt2A,245)
aadd(aGFInt2A,22)
aadd(aGFInt2A,235)
aadd(aGFInt2A,122)
aadd(aGFInt2A,117)
aadd(aGFInt2A,44)
aadd(aGFInt2A,215)
aadd(aGFInt2A,79)
aadd(aGFInt2A,174)
aadd(aGFInt2A,213)
aadd(aGFInt2A,233)
aadd(aGFInt2A,230)
aadd(aGFInt2A,231)
aadd(aGFInt2A,173)
aadd(aGFInt2A,232)
aadd(aGFInt2A,116)
aadd(aGFInt2A,214)
aadd(aGFInt2A,244)
aadd(aGFInt2A,234)
aadd(aGFInt2A,168)
aadd(aGFInt2A,80)
aadd(aGFInt2A,88)
aadd(aGFInt2A,175)
     
Return aGFInt2A

// ------------------------------------------------------------
// Cria o polinomio baseado nos dados representados como inteiros 
// polinomio com valores (1) e expoente de x (2)  
 
STATIC Function BuildPoli(aCodeNum)
Local nTam := len(aCodeNum)
Local nI
Local aPoli := {}
For nI := 1 to nTam
	aadd(aPoli,{aCodeNum[nI],nTam-nI})
Next
Return aPoli

                       
// Cria o array de Error Correction para os numeros informmados
// Recebe tambem o numero de Error Correction Codewords a ser criado 

// 
// Generator polinomial para 10 codewords de Error correction
//
// x10 + a251x9 + a67x8 + a46x7 + a61x6 + a118x5 + a70x4 + a64x3 + a94x2 + a32x + a45
// ou 
// pow(x,10) + pow(a251*x,9) + pow(a67*x,8) + pow(a46*x,7) + pow(a61*x,6) + pow(a118*x,5) + 
// pow(a70*x,4) + pow(a64*x,3) + pow(a94*x,2) + (a32*x) + a45
// 
// Message polinomial
// 
// pow(32*x,15)+pow(91*x,14)+pow(11*x,13)+pow(120*x,12)+pow(209*x,11)+pow(114*x,10)+pow(220*x,9)+
// pow(77*x,8)+pow(67*x,7)+pow(64*x,6)+pow(236*x,5)+pow(17*x,4)+pow(236*x,3)+pow(17*x,2)+(236*x)+(17)

// The lead term of the generator polynomial should also have the same exponent, so multiply by x15 to get
// a primeira potencia do message polinomial é 15, logo multiplica o message polinomial pelo expoente 15

// ------------------------------------------------------------
// multiplicacao de polinomio adiconando o expoente em X 

STATIC Function MultPoliX(aPoli,nExp)
Local nI
For nI := 1 to len(aPoli)
	aPoli[nI][2] += nExp
Next
Return 


/*
STATIC Function ShowPoli(cMsg,aPoli)
Local nI
conout(padc(cMSg,79,'-'))
For nI := 1 to len(aPoli)
	conout('a('+cValToChar(aPoli[nI][1])+') x('+cValToChar(aPoli[nI][2])+')')
Next
Return
*/
           
// ------------------------------------------------------------
// Realiza multiplicação polinomial 
// em notação Alpha

STATIC Function MultPoliA(aPoli,nExp)
Local nI
For nI := 1 to len(aPoli)
	aPoli[nI][1] += nExp
	If aPoli[nI][1] > 255
		aPoli[nI][1] := aPoli[nI][1] % 255
	Endif
Next
Return 


// ------------------------------------------------------------
// Converte polinomio de representação Alpha para Inteiro 

STATIC Function PoliA2I(aPoli)
Local nI, nA 
For nI := 1 to len(aPoli)
	nA := aPoli[nI][1]
    nV := AlphaToX(nA)
    aPoli[nI][1] := nV
Next
Return 



// ------------------------------------------------------------
// Realiza o XOR de dois polinomios em notação Alpha

STATIC Function PoliXOR(aMsgPoli,aResPoli)
Local aNewPoli := {}
Local nI, nV , nX 
Local nMax :=  MAX(len(aMsgPoli),len(aResPoli))
For nI := 1 to nMax
	IF nI > len(aResPoli)    
		// Nao tem o elemento, N XOR 0 mantem o valor original 
		nV := aMsgPoli[nI][1]
		nX := aMsgPoli[nI][2]
	ElseIf nI > len(aMsgPoli)
		// Nao tem o elemento, N XOR 0 mantem o valor original 
		nV := aResPoli[nI][1]
		nX := aResPoli[nI][2]
	Else
		// Faz o XOR com o valor do elemento para gerar o novo polinomio
		nV := NXOR(aMsgPoli[nI][1] ,aResPoli[nI][1])
		nX := aMsgPoli[nI][2]
	Endif
	If nI == 1 .and. nV <> 0 
		UserException("*** POLYONMIAL XOR ERROR ***")
	Endif
	IF nI > 1
		// Sempre descarta o primeiro termo
		// Que deve ser zero  
		aadd(aNewPoli,{nV,nX})
	Endif
Next
Return aNewPoli

// ------------------------------------------------------------
// Faz um Step Down no Base Generator, apenas 
// diminuindo um expoente de x de cada elemento 
// Na pratica nao faz diferença para o calculo, pois 
// todo o resto da aritmetica depois de montado o generator 
// polinomial é baseada no expoente em notacao alpha (A) 

STATIC Function StepGen(aGenPoli)
Local nI
For nI := 1 to len(aGenPoli)
	aGenPoli[nI][2]--	
Next
Return

/* -----------------------------------------------------------------
	Retorna um Generator Polynomial para um determinado 
	numero de Error Correction CodeWords 
	Dados obtigos usando o Generator Polynomial Tool
	https://www.thonky.com/qr-code-tutorial/generator-polynomial-tool
----------------------------------------------------------------- */

STATIC Function GetPolyGen(nECWords)
Local aGenPoli := {}

If nECWords == 7

    // x7 + a87x6 + a229x5 + a146x4 + 
    // a149x3 + a238x2 + a102x + a21
    
	aadd(aGenPoli,{0,7})
 	aadd(aGenPoli,{87,6})
 	aadd(aGenPoli,{229,5})
 	aadd(aGenPoli,{146,4})
 	aadd(aGenPoli,{149,3})
 	aadd(aGenPoli,{238,2})
 	aadd(aGenPoli,{102,1})
 	aadd(aGenPoli,{21,0})

ElseIf nECWords == 10
	
	// x10 + a251x9 + a67x8 + a46x7 + 
	// a61x6 + a118x5 + a70x4 + a64x3 + 
	// a94x2 + a32x + a45

	aadd(aGenPoli,{0,10})
	aadd(aGenPoli,{251,9})
	aadd(aGenPoli,{67,8})
	aadd(aGenPoli,{46,7})
	aadd(aGenPoli,{61,6})
	aadd(aGenPoli,{118,5})
	aadd(aGenPoli,{70,4})
	aadd(aGenPoli,{64,3})
	aadd(aGenPoli,{94,2})
	aadd(aGenPoli,{32,1})
	aadd(aGenPoli,{45,0})

ElseIf nECWords == 13

	// x13 + a74x12 + a152x11 + a176x10 + 
	// a100x9 + a86x8 + a100x7 + a106x6 + 
	// a104x5 + a130x4 + a218x3 + a206x2 + 
	// a140x + a78
	
	aadd(aGenPoli,{0  ,13})
	aadd(aGenPoli,{74 ,12})
	aadd(aGenPoli,{152,11})
	aadd(aGenPoli,{176,10})
	aadd(aGenPoli,{100,9})
	aadd(aGenPoli,{86 ,8})
	aadd(aGenPoli,{100,7})
	aadd(aGenPoli,{106,6})
	aadd(aGenPoli,{104,5})
	aadd(aGenPoli,{130,4})
	aadd(aGenPoli,{218,3})
	aadd(aGenPoli,{206,2})
	aadd(aGenPoli,{140,1})
	aadd(aGenPoli,{78 ,0})

ElseIf nECWords == 15

	// x15 + a8x14 + a183x13 + a61x12 + 
	// a91x11 + a202x10 + a37x9 + a51x8 + 
	// a58x7 + a58x6 + a237x5 + a140x4 + 
	// a124x3 + a5x2 + a99x + a105                    

	aadd(aGenPoli,{0,15})
	aadd(aGenPoli,{8,14})
	aadd(aGenPoli,{183,13})
	aadd(aGenPoli,{61,12})
	aadd(aGenPoli,{91,11})
	aadd(aGenPoli,{202,10})
	aadd(aGenPoli,{37,9})
	aadd(aGenPoli,{51,8})
	aadd(aGenPoli,{58,7})
	aadd(aGenPoli,{58,6})
	aadd(aGenPoli,{237,5})
	aadd(aGenPoli,{140,4})
	aadd(aGenPoli,{124,3})
	aadd(aGenPoli,{5,2})
	aadd(aGenPoli,{99,1})
	aadd(aGenPoli,{105,0})

ElseIf nECWords == 16

	// x16 + a120x15 + a104x14 + a107x13 + 
	// a109x12 + a102x11 + a161x10 + a76x9 + 
	// a3x8 + a91x7 + a191x6 + a147x5 + 
	// a169x4 + a182x3 + a194x2 + a225x + a120

	aadd(aGenPoli,{0,16})
	aadd(aGenPoli,{120,15})
	aadd(aGenPoli,{104,14})
	aadd(aGenPoli,{107,13})
	aadd(aGenPoli,{109,12})
	aadd(aGenPoli,{102,11})
	aadd(aGenPoli,{161,10})
	aadd(aGenPoli,{76,9})
	aadd(aGenPoli,{3,8})
	aadd(aGenPoli,{91,7})
	aadd(aGenPoli,{191,6})
	aadd(aGenPoli,{147,5})
	aadd(aGenPoli,{169,4})
	aadd(aGenPoli,{182,3})
	aadd(aGenPoli,{194,2})
	aadd(aGenPoli,{225,1})
	aadd(aGenPoli,{120,0})
	
ElseIf nECWords == 17

	// x17 + a43x16 + a139x15 + a206x14 + 
	// a78x13 + a43x12 + a239x11 + a123x10 + 
	// a206x9 + a214x8 + a147x7 + a24x6 + 
	// a99x5 + a150x4 + a39x3 + a243x2 + 
	// a163x + a136

	aadd(aGenPoli,{0,17})
	aadd(aGenPoli,{43,16})
	aadd(aGenPoli,{139,15})
	aadd(aGenPoli,{206,14})
	aadd(aGenPoli,{78,13})
	aadd(aGenPoli,{43,12})
	aadd(aGenPoli,{239,11})
	aadd(aGenPoli,{123,10})
	aadd(aGenPoli,{206,9})
	aadd(aGenPoli,{214,8})
	aadd(aGenPoli,{147,7})
	aadd(aGenPoli,{24,6})
	aadd(aGenPoli,{99,5})
	aadd(aGenPoli,{150,4})
	aadd(aGenPoli,{39,3})
	aadd(aGenPoli,{243,2})
	aadd(aGenPoli,{163,1})
	aadd(aGenPoli,{136,0})

	
ElseIf nECWords == 18

	// x18 + a215x17 + a234x16 + a158x15 + 
	// a94x14 + a184x13 + a97x12 + a118x11 + 
	// a170x10 + a79x9 + a187x8 + a152x7 + 
	// a148x6 + a252x5 + a179x4 + a5x3 + 
	// a98x2 + a96x + a153
	
	aadd(aGenPoli,{0,18})
	aadd(aGenPoli,{215,17})
	aadd(aGenPoli,{234,16})
	aadd(aGenPoli,{158,15})
	aadd(aGenPoli,{94,14})
	aadd(aGenPoli,{184,13})
	aadd(aGenPoli,{97,12})
	aadd(aGenPoli,{118,11})
	aadd(aGenPoli,{170,10})
	aadd(aGenPoli,{79,9})
	aadd(aGenPoli,{187,8})
	aadd(aGenPoli,{152,7})
	aadd(aGenPoli,{148,6})
	aadd(aGenPoli,{252,5})
	aadd(aGenPoli,{179,4})
	aadd(aGenPoli,{5,3})
	aadd(aGenPoli,{98,2})
	aadd(aGenPoli,{96,1})
	aadd(aGenPoli,{153,0})

ElseIf nECWords == 20

	// x20 + a17x19 + a60x18 + a79x17 + 
	// a50x16 + a61x15 + a163x14 + a26x13 + 
	// a187x12 + a202x11 + a180x10 + a221x9 + 
	// a225x8 + a83x7 + a239x6 + a156x5 + 
	// a164x4 + a212x3 + a212x2 + a188x + a190
	
	aadd(aGenPoli,{0,20})
	aadd(aGenPoli,{17,19})
	aadd(aGenPoli,{60,18})
	aadd(aGenPoli,{79,17})
	aadd(aGenPoli,{50,16})
	aadd(aGenPoli,{61,15})
	aadd(aGenPoli,{163,14})
	aadd(aGenPoli,{26,13})
	aadd(aGenPoli,{187,12})
	aadd(aGenPoli,{202,11})
	aadd(aGenPoli,{180,10})
	aadd(aGenPoli,{221,9})
	aadd(aGenPoli,{225,8})
	aadd(aGenPoli,{83,7})
	aadd(aGenPoli,{239,6})
	aadd(aGenPoli,{156,5})
	aadd(aGenPoli,{164,4})
	aadd(aGenPoli,{212,3})
	aadd(aGenPoli,{212,2})
	aadd(aGenPoli,{188,1})
	aadd(aGenPoli,{190,0})

	
ElseIf nECWords == 22

	// x22 + a210x21 + a171x20 + a247x19 + 
	// a242x18 + a93x17 + a230x16 + a14x15 + 
	// a109x14 + a221x13 + a53x12 + a200x11 + 
	// a74x10 + a8x9 + a172x8 + a98x7 + 
	// a80x6 + a219x5 + a134x4 + a160x3 + 
	// a105x2 + a165x + a231
	
	aadd(aGenPoli,{0,22})
	aadd(aGenPoli,{210,21})
	aadd(aGenPoli,{171,20})
	aadd(aGenPoli,{247,19})
	aadd(aGenPoli,{242,18})
	aadd(aGenPoli,{93,17})
	aadd(aGenPoli,{230,16})
	aadd(aGenPoli,{14,15})
	aadd(aGenPoli,{109,14})
	aadd(aGenPoli,{221,13})
	aadd(aGenPoli,{53,12})
	aadd(aGenPoli,{200,11})
	aadd(aGenPoli,{74,10})
	aadd(aGenPoli,{8,9})
	aadd(aGenPoli,{172,8})
	aadd(aGenPoli,{98,7})
	aadd(aGenPoli,{80,6})
	aadd(aGenPoli,{219,5})
	aadd(aGenPoli,{134,4})
	aadd(aGenPoli,{160,3})
	aadd(aGenPoli,{105,2})
	aadd(aGenPoli,{165,1})
	aadd(aGenPoli,{231,0})

ElseIf nECWords == 24

	// x24 + a229x23 + a121x22 + a135x21 + 
	// a48x20 + a211x19 + a117x18 + a251x17 + 
	// a126x16 + a159x15 + a180x14 + a169x13 + 
	// a152x12 + a192x11 + a226x10 + a228x9 + 
	// a218x8 + a111x7 + x6 + a117x5 + 
	// a232x4 + a87x3 + a96x2 + a227x + a21

	aadd(aGenPoli,{0,24})
	aadd(aGenPoli,{229,23})
	aadd(aGenPoli,{121,22})
	aadd(aGenPoli,{135,21})
	aadd(aGenPoli,{48,20})
	aadd(aGenPoli,{211,19})
	aadd(aGenPoli,{117,18})
	aadd(aGenPoli,{251,17})
	aadd(aGenPoli,{126,16})
	aadd(aGenPoli,{159,15})
	aadd(aGenPoli,{180,14})
	aadd(aGenPoli,{169,13})
	aadd(aGenPoli,{152,12})
	aadd(aGenPoli,{191,11})
	aadd(aGenPoli,{226,10})
	aadd(aGenPoli,{228,9})
	aadd(aGenPoli,{218,8})
	aadd(aGenPoli,{111,7})
	aadd(aGenPoli,{0,6})
	aadd(aGenPoli,{117,5})
	aadd(aGenPoli,{232,4})
	aadd(aGenPoli,{87,3})
	aadd(aGenPoli,{96,2})
	aadd(aGenPoli,{227,1})
	aadd(aGenPoli,{21,0})
	
ElseIf nECWords == 26

	// x26 + a173x25 + a125x24 + a158x23 + 
	// a2x22 + a103x21 + a182x20 + a118x19 + 
	// a17x18 + a145x17 + a201x16 + a111x15 + 
	// a28x14 + a165x13 + a53x12 + a161x11 + 
	// a21x10 + a245x9 + a142x8 + a13x7 + 
	// a102x6 + a48x5 + a227x4 + a153x3 + 
	// a145x2 + a218x + a70
	
	aadd(aGenPoli,{0,26})
	aadd(aGenPoli,{173,25})
	aadd(aGenPoli,{125,24})
	aadd(aGenPoli,{158,23})
	aadd(aGenPoli,{2,22})
	aadd(aGenPoli,{103,21})
	aadd(aGenPoli,{182,20})
	aadd(aGenPoli,{118,19})
	aadd(aGenPoli,{17,18})
	aadd(aGenPoli,{145,17})
	aadd(aGenPoli,{201,16})
	aadd(aGenPoli,{111,15})
	aadd(aGenPoli,{28,14})
	aadd(aGenPoli,{165,13})
	aadd(aGenPoli,{53,12})
	aadd(aGenPoli,{161,11})
	aadd(aGenPoli,{21,10})
	aadd(aGenPoli,{245,9})
	aadd(aGenPoli,{142,8})
	aadd(aGenPoli,{13,7})
	aadd(aGenPoli,{102,6})
	aadd(aGenPoli,{48,5})
	aadd(aGenPoli,{227,4})
	aadd(aGenPoli,{153,3})
	aadd(aGenPoli,{145,2})
	aadd(aGenPoli,{218,1})
	aadd(aGenPoli,{70,0})


ElseIf nECWords == 28

	// x28 + a168x27 + a223x26 + a200x25 + 
	// a104x24 + a224x23 + a234x22 + a108x21 + 
	// a180x20 + a110x19 + a190x18 + a195x17 + 
	// a147x16 + a205x15 + a27x14 + a232x13 + 
	// a201x12 + a21x11 + a43x10 + a245x9 + 
	// a87x8 + a42x7 + a195x6 + a212x5 + 
	// a119x4 + a242x3 + a37x2 + a9x + a123
	   
	aadd(aGenPoli,{0,28})
	aadd(aGenPoli,{168,27})
	aadd(aGenPoli,{223,26})
	aadd(aGenPoli,{200,25})
	aadd(aGenPoli,{104,24})
	aadd(aGenPoli,{224,23})
	aadd(aGenPoli,{234,22})
	aadd(aGenPoli,{108,21})
	aadd(aGenPoli,{180,20})
	aadd(aGenPoli,{110,19})
	aadd(aGenPoli,{190,18})
	aadd(aGenPoli,{195,17})
	aadd(aGenPoli,{147,16})
	aadd(aGenPoli,{205,15})
	aadd(aGenPoli,{27,14})
	aadd(aGenPoli,{232,13})
	aadd(aGenPoli,{201,12})
	aadd(aGenPoli,{21,11})
	aadd(aGenPoli,{43,10})
	aadd(aGenPoli,{245,9})
	aadd(aGenPoli,{87,8})
	aadd(aGenPoli,{42,7})
	aadd(aGenPoli,{195,6})
	aadd(aGenPoli,{212,5})
	aadd(aGenPoli,{119,4})
	aadd(aGenPoli,{242,3})
	aadd(aGenPoli,{37,2})
	aadd(aGenPoli,{9,1})
	aadd(aGenPoli,{123,0})

ElseIf nECWords == 30

	// x30 + a41x29 + a173x28 + a145x27 + 
	// a152x26 + a216x25 + a31x24 + a179x23 + 
	// a182x22 + a50x21 + a48x20 + a110x19 + 
	// a86x18 + a239x17 + a96x16 + a222x15 + 
	// a125x14 + a42x13 + a173x12 + a226x11 + 
	// a193x10 + a224x9 + a130x8 + a156x7 + 
	// a37x6 + a251x5 + a216x4 + a238x3 + 
	// a40x2 + a192x + a180

	aadd(aGenPoli,{0,30})
	aadd(aGenPoli,{41,29})
	aadd(aGenPoli,{173,28})
	aadd(aGenPoli,{145,27})
	aadd(aGenPoli,{152,26})
	aadd(aGenPoli,{216,25})
	aadd(aGenPoli,{31,24})
	aadd(aGenPoli,{179,23})
	aadd(aGenPoli,{182,22})
	aadd(aGenPoli,{50,21})
	aadd(aGenPoli,{48,20})
	aadd(aGenPoli,{110,19})
	aadd(aGenPoli,{86,18})
	aadd(aGenPoli,{239,17})
	aadd(aGenPoli,{96,16})
	aadd(aGenPoli,{222,15})
	aadd(aGenPoli,{125,14})
	aadd(aGenPoli,{42,13})
	aadd(aGenPoli,{173,12})
	aadd(aGenPoli,{226,11})
	aadd(aGenPoli,{193,10})
	aadd(aGenPoli,{224,9})
	aadd(aGenPoli,{130,8})
	aadd(aGenPoli,{156,7})
	aadd(aGenPoli,{37,6})
	aadd(aGenPoli,{251,5})
	aadd(aGenPoli,{216,4})
	aadd(aGenPoli,{238,3})
	aadd(aGenPoli,{40,2})
	aadd(aGenPoli,{192,1})
	aadd(aGenPoli,{180,0})

Endif

If len(aGenPoli) < 1 
	UserException("POLINOMIAL GENERATOR EC("+cValToChar(nECWords)+") NOT IMPLEMENTED")
Endif

Return aGenPoli

// ------------------------------------------------------------
// Character Capacities by Version, Mode, and Error Correction
// https://www.thonky.com/qr-code-tutorial/character-capacities

STATIC _QRLimts := GetQRLimits()

// ------------------------------------------------------------
// Retorna o limite de caracteres para um determinado modo

STATIC Function GetLimit(nVersion,cErrCorr,nMode)
Local nPos := ascan(_QRLimts,{|x| x[1] == nVersion .and. x[2] == cErrCorr })
Return _QRLimts[nPos][nMode+2]

STATIC Function GetQRLimits()
Local aQRLimits := {}

/*
[1] Version	
[2] Error Correction Level	
[3] Numeric Mode Limit
[4] Alphanumeric Mode Limit	
[5] Byte Mode Limit
[6] Kanji Mode Limit
*/

aadd(aQRLimits,{1,'L',41,25,17,10})
aadd(aQRLimits,{1,'M',34,20,14,8})
aadd(aQRLimits,{1,'Q',27,16,11,7})
aadd(aQRLimits,{1,'H',17,10,7,4})
aadd(aQRLimits,{2,'L',77,47,32,20})
aadd(aQRLimits,{2,'M',63,38,26,16})
aadd(aQRLimits,{2,'Q',48,29,20,12})
aadd(aQRLimits,{2,'H',34,20,14,8})
aadd(aQRLimits,{3,'L',127,77,53,32})
aadd(aQRLimits,{3,'M',101,61,42,26})
aadd(aQRLimits,{3,'Q',77,47,32,20})
aadd(aQRLimits,{3,'H',58,35,24,15})
aadd(aQRLimits,{4,'L',187,114,78,48})
aadd(aQRLimits,{4,'M',149,90,62,38})
aadd(aQRLimits,{4,'Q',111,67,46,28})
aadd(aQRLimits,{4,'H',82,50,34,21})
aadd(aQRLimits,{5,'L',255,154,106,65})
aadd(aQRLimits,{5,'M',202,122,84,52})
aadd(aQRLimits,{5,'Q',144,87,60,37})
aadd(aQRLimits,{5,'H',106,64,44,27})
aadd(aQRLimits,{6,'L',322,195,134,82})
aadd(aQRLimits,{6,'M',255,154,106,65})
aadd(aQRLimits,{6,'Q',178,108,74,45})
aadd(aQRLimits,{6,'H',139,84,58,36})
aadd(aQRLimits,{7,'L',370,224,154,95})
aadd(aQRLimits,{7,'M',293,178,122,75})
aadd(aQRLimits,{7,'Q',207,125,86,53})
aadd(aQRLimits,{7,'H',154,93,64,39})
aadd(aQRLimits,{8,'L',461,279,192,118})
aadd(aQRLimits,{8,'M',365,221,152,93})
aadd(aQRLimits,{8,'Q',259,157,108,66})
aadd(aQRLimits,{8,'H',202,122,84,52})
aadd(aQRLimits,{9,'L',552,335,230,141})
aadd(aQRLimits,{9,'M',432,262,180,111})
aadd(aQRLimits,{9,'Q',312,189,130,80})
aadd(aQRLimits,{9,'H',235,143,98,60})
aadd(aQRLimits,{10,'L',652,395,271,167})
aadd(aQRLimits,{10,'M',513,311,213,131})
aadd(aQRLimits,{10,'Q',364,221,151,93})
aadd(aQRLimits,{10,'H',288,174,119,74})
aadd(aQRLimits,{11,'L',772,468,321,198})
aadd(aQRLimits,{11,'M',604,366,251,155})
aadd(aQRLimits,{11,'Q',427,259,177,109})
aadd(aQRLimits,{11,'H',331,200,137,85})
aadd(aQRLimits,{12,'L',883,535,367,226})
aadd(aQRLimits,{12,'M',691,419,287,177})
aadd(aQRLimits,{12,'Q',489,296,203,125})
aadd(aQRLimits,{12,'H',374,227,155,96})
aadd(aQRLimits,{13,'L',1022,619,425,262})
aadd(aQRLimits,{13,'M',796,483,331,204})
aadd(aQRLimits,{13,'Q',580,352,241,149})
aadd(aQRLimits,{13,'H',427,259,177,109})
aadd(aQRLimits,{14,'L',1101,667,458,282})
aadd(aQRLimits,{14,'M',871,528,362,223})
aadd(aQRLimits,{14,'Q',621,376,258,159})
aadd(aQRLimits,{14,'H',468,283,194,120})
aadd(aQRLimits,{15,'L',1250,758,520,320})
aadd(aQRLimits,{15,'M',991,600,412,254})
aadd(aQRLimits,{15,'Q',703,426,292,180})
aadd(aQRLimits,{15,'H',530,321,220,136})
aadd(aQRLimits,{16,'L',1408,854,586,361})
aadd(aQRLimits,{16,'M',1082,656,450,277})
aadd(aQRLimits,{16,'Q',775,470,322,198})
aadd(aQRLimits,{16,'H',602,365,250,154})
aadd(aQRLimits,{17,'L',1548,938,644,397})
aadd(aQRLimits,{17,'M',1212,734,504,310})
aadd(aQRLimits,{17,'Q',876,531,364,224})
aadd(aQRLimits,{17,'H',674,408,280,173})
aadd(aQRLimits,{18,'L',1725,1046,718,442})
aadd(aQRLimits,{18,'M',1346,816,560,345})
aadd(aQRLimits,{18,'Q',948,574,394,243})
aadd(aQRLimits,{18,'H',746,452,310,191})
aadd(aQRLimits,{19,'L',1903,1153,792,488})
aadd(aQRLimits,{19,'M',1500,909,624,384})
aadd(aQRLimits,{19,'Q',1063,644,442,272})
aadd(aQRLimits,{19,'H',813,493,338,208})
aadd(aQRLimits,{20,'L',2061,1249,858,528})
aadd(aQRLimits,{20,'M',1600,970,666,410})
aadd(aQRLimits,{20,'Q',1159,702,482,297})
aadd(aQRLimits,{20,'H',919,557,382,235})
aadd(aQRLimits,{21,'L',2232,1352,929,572})
aadd(aQRLimits,{21,'M',1708,1035,711,438})
aadd(aQRLimits,{21,'Q',1224,742,509,314})
aadd(aQRLimits,{21,'H',969,587,403,248})
aadd(aQRLimits,{22,'L',2409,1460,1003,618})
aadd(aQRLimits,{22,'M',1872,1134,779,480})
aadd(aQRLimits,{22,'Q',1358,823,565,348})
aadd(aQRLimits,{22,'H',1056,640,439,270})
aadd(aQRLimits,{23,'L',2620,1588,1091,672})
aadd(aQRLimits,{23,'M',2059,1248,857,528})
aadd(aQRLimits,{23,'Q',1468,890,611,376})
aadd(aQRLimits,{23,'H',1108,672,461,284})
aadd(aQRLimits,{24,'L',2812,1704,1171,721})
aadd(aQRLimits,{24,'M',2188,1326,911,561})
aadd(aQRLimits,{24,'Q',1588,963,661,407})
aadd(aQRLimits,{24,'H',1228,744,511,315})
aadd(aQRLimits,{25,'L',3057,1853,1273,784})
aadd(aQRLimits,{25,'M',2395,1451,997,614})
aadd(aQRLimits,{25,'Q',1718,1041,715,440})
aadd(aQRLimits,{25,'H',1286,779,535,330})
aadd(aQRLimits,{26,'L',3283,1990,1367,842})
aadd(aQRLimits,{26,'M',2544,1542,1059,652})
aadd(aQRLimits,{26,'Q',1804,1094,751,462})
aadd(aQRLimits,{26,'H',1425,864,593,365})
aadd(aQRLimits,{27,'L',3517,2132,1465,902})
aadd(aQRLimits,{27,'M',2701,1637,1125,692})
aadd(aQRLimits,{27,'Q',1933,1172,805,496})
aadd(aQRLimits,{27,'H',1501,910,625,385})
aadd(aQRLimits,{28,'L',3669,2223,1528,940})
aadd(aQRLimits,{28,'M',2857,1732,1190,732})
aadd(aQRLimits,{28,'Q',2085,1263,868,534})
aadd(aQRLimits,{28,'H',1581,958,658,405})
aadd(aQRLimits,{29,'L',3909,2369,1628,1002})
aadd(aQRLimits,{29,'M',3035,1839,1264,778})
aadd(aQRLimits,{29,'Q',2181,1322,908,559})
aadd(aQRLimits,{29,'H',1677,1016,698,430})
aadd(aQRLimits,{30,'L',4158,2520,1732,1066})
aadd(aQRLimits,{30,'M',3289,1994,1370,843})
aadd(aQRLimits,{30,'Q',2358,1429,982,604})
aadd(aQRLimits,{30,'H',1782,1080,742,457})
aadd(aQRLimits,{31,'L',4417,2677,1840,1132})
aadd(aQRLimits,{31,'M',3486,2113,1452,894})
aadd(aQRLimits,{31,'Q',2473,1499,1030,634})
aadd(aQRLimits,{31,'H',1897,1150,790,486})
aadd(aQRLimits,{32,'L',4686,2840,1952,1201})
aadd(aQRLimits,{32,'M',3693,2238,1538,947})
aadd(aQRLimits,{32,'Q',2670,1618,1112,684})
aadd(aQRLimits,{32,'H',2022,1226,842,518})
aadd(aQRLimits,{33,'L',4965,3009,2068,1273})
aadd(aQRLimits,{33,'M',3909,2369,1628,1002})
aadd(aQRLimits,{33,'Q',2805,1700,1168,719})
aadd(aQRLimits,{33,'H',2157,1307,898,553})
aadd(aQRLimits,{34,'L',5253,3183,2188,1347})
aadd(aQRLimits,{34,'M',4134,2506,1722,1060})
aadd(aQRLimits,{34,'Q',2949,1787,1228,756})
aadd(aQRLimits,{34,'H',2301,1394,958,590})
aadd(aQRLimits,{35,'L',5529,3351,2303,1417})
aadd(aQRLimits,{35,'M',4343,2632,1809,1113})
aadd(aQRLimits,{35,'Q',3081,1867,1283,790})
aadd(aQRLimits,{35,'H',2361,1431,983,605})
aadd(aQRLimits,{36,'L',5836,3537,2431,1496})
aadd(aQRLimits,{36,'M',4588,2780,1911,1176})
aadd(aQRLimits,{36,'Q',3244,1966,1351,832})
aadd(aQRLimits,{36,'H',2524,1530,1051,647})
aadd(aQRLimits,{37,'L',6153,3729,2563,1577})
aadd(aQRLimits,{37,'M',4775,2894,1989,1224})
aadd(aQRLimits,{37,'Q',3417,2071,1423,876})
aadd(aQRLimits,{37,'H',2625,1591,1093,673})
aadd(aQRLimits,{38,'L',6479,3927,2699,1661})
aadd(aQRLimits,{38,'M',5039,3054,2099,1292})
aadd(aQRLimits,{38,'Q',3599,2181,1499,923})
aadd(aQRLimits,{38,'H',2735,1658,1139,701})
aadd(aQRLimits,{39,'L',6743,4087,2809,1729})
aadd(aQRLimits,{39,'M',5313,3220,2213,1362})
aadd(aQRLimits,{39,'Q',3791,2298,1579,972})
aadd(aQRLimits,{39,'H',2927,1774,1219,750})
aadd(aQRLimits,{40,'L',7089,4296,2953,1817})
aadd(aQRLimits,{40,'M',5596,3391,2331,1435})
aadd(aQRLimits,{40,'Q',3993,2420,1663,1024})
aadd(aQRLimits,{40,'H',3057,1852,1273,784})

Return aQRLimits


STATIC _RemBits := InitRemBits()

// Recupera a quantidade de bits remanescentes que devem ser acrescentados 
// a sequencia binaria de dados e EC CodeWords           
// https://www.thonky.com/qr-code-tutorial/structure-final-message
// Add Remainder Bits if Necessary
     
STATIC Function GetRemBits(nVersion)
Return _RemBits[nVersion][2]

STATIC Function InitRemBits()
Local aRemBits := {}

aadd(aRemBits,{1,0})
aadd(aRemBits,{2,7})
aadd(aRemBits,{3,7})
aadd(aRemBits,{4,7})
aadd(aRemBits,{5,7})
aadd(aRemBits,{6,7})
aadd(aRemBits,{7,0})
aadd(aRemBits,{8,0})
aadd(aRemBits,{9,0})
aadd(aRemBits,{10,0})
aadd(aRemBits,{11,0})
aadd(aRemBits,{12,0})
aadd(aRemBits,{13,0})
aadd(aRemBits,{14,3})
aadd(aRemBits,{15,3})
aadd(aRemBits,{16,3})
aadd(aRemBits,{17,3})
aadd(aRemBits,{18,3})
aadd(aRemBits,{19,3})
aadd(aRemBits,{20,3})
aadd(aRemBits,{21,4})
aadd(aRemBits,{22,4})
aadd(aRemBits,{23,4})
aadd(aRemBits,{24,4})
aadd(aRemBits,{25,4})
aadd(aRemBits,{26,4})
aadd(aRemBits,{27,4})
aadd(aRemBits,{28,3})
aadd(aRemBits,{29,3})
aadd(aRemBits,{30,3})
aadd(aRemBits,{31,3})
aadd(aRemBits,{32,3})
aadd(aRemBits,{33,3})
aadd(aRemBits,{34,3})
aadd(aRemBits,{35,0})
aadd(aRemBits,{36,0})
aadd(aRemBits,{37,0})
aadd(aRemBits,{38,0})
aadd(aRemBits,{39,0})
aadd(aRemBits,{40,0})

Return aRemBits

// ------------------------------------------------------------
// Monta alguns QRCodes para escolher qual o melhor
// Data Mask a ser utilizado. Os valores numéricos 
// dos bytes estao em ::aBitStream

METHOD DeployBits() CLASS ZQRCODE 
Local nByteSize := len(::aBitStream)
Local nI
Local cBitSeq := ''
Local lUp
Local nPlaceRow
Local nPlaceCol
Local lFirstBit
Local nDeploy := 0
  
// Reserva a área de 15 bytes para formato e versao 
::PutFVInfo("000000000000000")
               
// Cria a sequencia de bits para deploy

::oLogger:Write("DEPLOYBITS",'Build Bit Stream of '+cValToChar(nByteSize)+" byte(s)")

For nI := 1 to nByteSize
	cBitSeq += NTOBIT8(::aBitStream[nI])
Next
       
nRemBits := GetRemBits(::nVersion)
If nRemBits > 0 
	::oLogger:Write("DEPLOYBITS",'ADD '+cValToChar(nRemBits)+' Remain Bitsd')
	cBitSeq += replicate('0',nRemBits)
Endif

::oLogger:Write("DEPLOYBITS","Bits Sequence Size. "+cValToChar(len(cBitSeq)))
::oLogger:Write("DEPLOYBITS","Bits Sequence ..... ")
::oLogger:Write("DEPLOYBITS",cBitSeq)

// -------------------------------------------------------------
// https://www.thonky.com/qr-code-tutorial/module-placement-matrix
// Step 6: Place the Data Bits

// A plotagem dos dados começa na ultima coluna 
// da ultima linha, de baixo pra cima, sempre 
// em pares de bits preenchidos da direita para a esquerda ( <-- ) 
// pulando as áreas reservadas

lUp := .T.
nPlaceRow := ::nSize
nPlaceCol := ::nSize
lFirstBit := .T.             
nDeploy := 0 

While !empty(cBitSeq)

	If ::aGrid[nPlaceRow][nPlaceCol] == -1
		// Posicao livre
		// Remove um bit 
		cBit := left(cBitSeq,1)
		cBitSeq := substr(cBitSeq,2)
		// e plota o bit                            
		// O bit de dados é plotado com os valores
		// 2 = espaço vazio
		// 3 = módulo preenchido 
		// Justamente para diferenciar a área de dados 
		// das áreas reservadas
		::aGrid[nPlaceRow][nPlaceCol] := val(cBit)+2
        
        nDeploy++            
		::oLogger:Write("DEPLOYBITS","Deploy Bit #"+cValToChar(nDeploy)+" ("+cBit+") ("+IIF(lUp,"UP","DW")+") at "+cValTochar(nPlaceRow-1)+","+cValTochar(nPlaceCol-1))
		If ::lVerbose
			::PrintQR()
		Endif
		
	Else

		::oLogger:Write("DEPLOYBITS","Skip Reserved Module at ["+cValTochar(nPlaceRow-1)+","+cValTochar(nPlaceCol-1)+"]")

	Endif

	// Procura a proxima posicao para plotagem    

	IF lFirstBit
		lFirstBit := .F.
		nPlaceCol-- 
	Else
		lFirstBit := .T.
		nPlaceCol++
		IF lUp
			IF nPlaceRow-1<1
				lUp := .F.
				nPlaceCol -= 2
			Else
				nPlaceRow--
			Endif
		Else 
			IF nPlaceRow+1>::nSize
				lUp := .T.
				nPlaceCol -= 2
			Else
				nPlaceRow++
			Endif
		Endif 
	Endif
                  
	If nPlaceCol == 7  
		// Pula a coluna da Time Line vertical 
		nPlaceCol := 6
	Endif

Enddo

If !empty(cBitSeq)
	::oLogger:Write("BUILDDATA","Left Bits ...... ["+cBitSeq+"]")
	UserException("Internal Error -- Bits remain after DeployBits")
Endif	

Return  .T.


// -------------------------------------------------------------
// Com o QR Code "base" plotado, depois do Deploy 
// Vamos aplica as 8 mascaras nos dados, plotar
// os resultados, e contabilizar as penalidades
// para escolher a melhor

METHOD SelectMask(nSetMask) CLASS ZQRCODE 
Local nMask
Local aNewGrid
Local aPenalty := {}
Local nPenalty
Local cVerFmtBits

IF nSetMask != NIL 

	::nMask := nSetMask
	::oLogger:Write("SELECTMASK","Set Mask "+cValToChar(::nMask))
     
	// Clona o grid original para fazer a projeção 
	aNewGrid := aClone(::aGrid)

	// Recupera os bits de versao e formato 	
	cVerFmtBits := GetTypeBits(::cErrCorr , nSetMask )
	
	::oLogger:Write("SELECTMASK","Version and Format Bits ["+cVerFmtBits+"]")

	// plota as informacoes de formato e versao
	::PutFVInfo(cVerFmtBits,aNewGrid)

	// Aplicar mascara na cópia do Grid
	// Deve ser aplicado apenas nas áreas de dados
	::ApplyMask(aNewGrid,nSetMask)
	
	// Elege o resultado como oficial 
	::aGrid := aClone(aNewGrid)

	Return .T. 

Endif
                                            
::oLogger:Write("SELECTMASK","Choosing Best Data Masking")

For nMask := 0 to 7

	::oLogger:Write("SELECTMASK","Test Mask "+cValToChar(nMask))

	// Clona o grid original para fazer a projeção 
	aNewGrid := aClone(::aGrid)

	// Recupera os bits de versao e formato 	
	cVerFmtBits := GetTypeBits(::cErrCorr , nMask )
	::oLogger:Write("SELECTMASK","Version and Format Bits ["+cVerFmtBits+"]")
	
	// plota as informacoes de formato e versao
	::PutFVInfo(cVerFmtBits,aNewGrid)

	// Aplicar mascara na cópia do Grid
	// Deve ser aplicado apenas nas áreas de dados
	::ApplyMask(aNewGrid,nMask)
	
	::oLogger:Write("SELECTMASK","Apply Mask "+cValToChar(nMask))
    ::PrintQR(aNewGrid)
    
	// Agora calcula os fatores de penalidade
	// nPenalty := ::GetPenalty(aNewGrid)

	// Guarda o fator de penalidade para esta máscara	
	// aadd( aPenalty , {nask,nPenalty} )	
	
Next

UserException("*** TODO ***")

/* ---------------------------------

// ordena pela menor penalidade    
aSort(aPenalty,,,{|x1,x2|  x1[2] < x2[2] })

// Escolhe a máscara com a menor penalidade
::nMask := aPenalty[1][1]
--------------------------------- */

Return

// ------------------------------------------------------------
// Aplica uma mascara de dados no grid informado 
// O objetivo da máscara é inverter os módulos
// das posições onde a fórmula retornar .T. 

METHOD ApplyMask(aNewGrid,nMask) CLASS ZQRCODE
Local nRow , nCol             
Local lInvert

::oLogger:Write("APPLYMASK","Data Mask ["+cValToChar(nMask)+"]")
                                        
// Primeiro aplica a máscara
// Somente inverte posicoes de dados
// 2 = modulo vazio
// 3 = modulo preenchido
For nRow := 0 to len(aNewGrid)-1
	For nCol := 0 to len(aNewGrid)-1
		
		If nMask == 0
			lInvert := ((nRow + nCol) % 2) = 0
		ElseIf nMask == 1
			lInvert := (nRow % 2) = 0
		ElseIf nMask == 2
			lInvert := (nCol % 3) = 0
		ElseIf nMask == 3
			lInvert :=  ((nRow + nCol) % 3) = 0
		ElseIf nMask == 4
			lInvert :=  (( int(nRow / 2) + int(nCol / 3) ) % 2 ) = 0
		ElseIf nMask == 5
			lInvert := ( (nRow * nCol) % 2) + ((nRow * nCol) % 3) = 0
		ElseIf nMask == 6
			lInvert :=  ( ((nRow * nCol) % 2) + ((nRow * nCol) % 3) ) % 2 = 0
		ElseIf nMask == 7
			lInvert := ( ((nRow + nCol) % 2) + ((nRow * nCol) % 3) ) % 2 = 0
		Endif
		
		IF lInvert
			If aNewGrid[nRow+1][nCol+1] == 3
				aNewGrid[nRow+1][nCol+1] := 2
				::oLogger:Write("APPLYMASK","Invert "+cValToChar(nRow)+","+cValToChar(nCol)+" From 1 to 0")
			ElseIf ::aGrid[nRow+1][nCol+1] == 2
				aNewGrid[nRow+1][nCol+1] := 3
				::oLogger:Write("APPLYMASK","Invert "+cValToChar(nRow)+","+cValToChar(nCol)+" From 0 to 1")
			Endif
		Endif

	Next
Next

// ::PrintQR(aNewGrid)

// Agora calcula os 4 indices de penalidade

/*
The second rule gives the QR code a penalty for each 2x2 area of same-colored modules in the matrix.
The third rule gives the QR code a large penalty if there are patterns that look similar to the finder patterns.
The fourth rule gives the QR code a penalty if more than half of the modules are dark or light, with a larger penalty for a larger difference.
*/

/*

The first rule gives the QR code a penalty for each group of five or more same-colored modules in a row (or column).

For the first evaluation condition, check each row one-by-one. If there are five consecutive modules of the same color, 
add 3 to the penalty. If there are more modules of the same color after the first five, add 1 for each additional 
module of the same color. Afterward, check each column one-by-one, checking for the same condition. Add the horizontal 
and vertical total to obtain penalty score #1.

*/

// Penalidade 1 
// Se uma linha ou coluna tem 5 ou mais modulos da mesma cor , adicione 3 na penalidade, 
// mais uma unidade de penalidade para cada módulo extra da mesma cor 

Return                                  

