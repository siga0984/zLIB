#include 'protheus.ch'
#include 'zlib.ch'

#DEFINE PI 3.14159265 // ACos(-1)

#TRANSLATE ZSWAP( <X> , <Y> , <S> ) =>  ( <S> := <X> , <X> := <Y> , <Y> := <S> )

/* ===========================================================================

Classe		ZBITMAP
Autor		Júlio Wittwer
Data		05/2019

Leitura e edição de Bitmap monocromatico em AdvPL 

Referencia do formato do arquivo 
https://en.wikipedia.org/wiki/BMP_file_format

=========================================================================== */

CLASS ZBITMAP FROM LONGNAMECLASS

    DATA cFileName     // Nome do arquivo de imagem 
    DATA nFileSize     // Tamanho em bytes total do arquivo
    DATA nHeight       // Altura da imagem em pixels
    DATA nWidth        // largura da imagem em pixels
    DATA nBPP          // Bits por Pixel ( 1,4,...)
    DATA cERROR        // String com ultima ocorrencia de erro da classe         
    DATA aMatrix       // Matrix de pontos do Bitmap ( cores ) 
    DATA aTrans        // Area interna de transferencia
    DATA aColors       // Tabela de cores do Bitmap
    DATA cFormat       // Identificador do formato do arquivo 
    DATA nOffSet       // Offset de inico dos dados 
    DATA nRawData      // Tamanho dos dados 
    DATA nRowSize      // Tamanho em bytes de uma linha da imagem 
    DATA nHeadSize     // Tamanho do Info Header 
    DATA nCompress     // Nivel de compressao 
    DATA nColorPlanes  // Numero de planos de cor ( 1 ) 
    DATA nHRes         // Resolucao horizontal 
    DATA nVRes         // Resolucao vertical 
    DATA nColorPal     // Paleta de cores
    DATA nImpColors    // Cores importantes
    DATA nFRColor      // Cor de pintural atual ( default =0 / preto ) 
    DATA nBgColor      // Cor de fundo ( default = branco ) 
    DATA nMargin       // Margem da imagem   
    DATA nPenSize      // Grossura da "caneta" de desenho ( default=1 ) 
    
    METHOD New()           // Cria uma imagem vazia 
    METHOD LoadFromFile()  // Le imagem de arquivo 
    METHOD GetErrorStr()   // Recupera ultimo erro da classe
    METHOD Clear()         // Limpa a imagem ( preenche com a cor de fundo ) ou uma parte dela  
    METHOD SetPixel()      // Seta a cor de um ponto 
    METHOD GetPixel()      // Recupera a cor de um ponto 
    METHOD BgColor()       // Seta ou Recupera a cor de fundo 
    METHOD Negative()      // Inverte as cores da imagem ( = Negativo ) 
    METHOD SaveToBMP()     // Salva a imagem em disco como Bitmap
    METHOD SaveToJPG()     // Salva a imagem em disco como JPEG
    METHOD Rectangle()     // Desenha um retângulo na imagem
    METHOD Line()          // Desenha uma linha na imagem entre dois pontos
    METHOD Circle()        // Desenha um círculo 
    METHOD SetBPP()        // Troca a resoluçao de cores
    METHOD Paint()         // pintura de área da imagem delimitada
    METHOD FlipH()         // Inverte horizontalmente uma área da imagem ou a imagem inteira
    METHOD FlipV()         // Inverte verticalmente uma área da imagem ou a imagem inteira
    METHOD Cut()           // Copia uma parte da imagem para a área interna de transferencia e limpa a área da imagem
    METHOD Copy()          // Copia uma parte da imagem para a área interna de transferencia
    METHOD Paste()         // Plota a imagem da area interna de transferencia na coordenada indicada

ENDCLASS


// Cria um objeto BMP
// Pode ser criado sem parametros, para carregar uma imagem do disco 

METHOD NEW(nWidth,nHeight,nBPP) CLASS ZBITMAP
Local nL
Local nC
Local aRow

IF nWidth = NIL
	::nWidth := 32
Else
	::nWidth := nWidth
Endif

If nHeight = NIL 
	::nHeight := 32
Else
	::nHeight := nHeight
Endif

If nBPP = NIL
	::nBPP := 1
Else
	::nBPP := nBPP
Endif

::aColors := GetColorTab(nBPP)

::cFileName := ''
::nPenSize  := 1               

::nFRColor := 0 // Preto 

IF ::nBPP = 1
	::nOffSet  := 62
	::nBgColor := 1 // Branco 
ElseIf ::nBPP = 4
	::nOffSet  := 118
	::nBgColor := 15 // Branco 
ElseIf ::nBPP = 8
	::nOffSet  := 1078
	::nBgColor := 255 // Branco 
Endif        
aRow := {}

// Inicializa matrix com a cor de fundo
::aMatrix := {}
For nC := 1 to ::nWidth
	aadd(aRow,::nBgColor)
Next
For nL := 1 to ::nHeight
	aadd(::aMatrix,aClone(aRow))
Next

::nHeadSize  := 40
::nColorPlanes := 1
::nCompress  := 0
::nHRes      := 0
::nVRes      := 0
::nColorPal  := 0
::nImpColors := 0

// Tamanho calculado de cada linha em bytes
::nRowSize   := int( ( (::nBPP*::nWidth) + 31 ) / 32 ) * 4 

// Tamando da area de dados da imagem 
::nRawData   := ::nRowSize  * ::nHeight

// Tamanho final da imagem 
::nFileSize :=   ::nRawData + ::nOffSet

Return self


// Carrega uma imagem BMP do disco 

METHOD LOADFROMFILE(cFile)  CLASS ZBITMAP
Local nH
Local cBuffer
Local cBinSize  
Local nBmpSize
Local nL , nC, nI
Local nByte
Local nReadOffset 
Local aRow := {}
Local cBits      
Local lTopDown := .F. 
Local nPos, nRed, nGreen,nBlue,nAlpha

::cFileName := cFile

nH := Fopen(cFile)

If  nH < 0 
	UserException("Fopen error ("+cValToChar(Ferror())+")")
Endif

::nFileSize := fSeek(nH,0,2)

fSeek(nH,0)

If ::nFileSize < 54
	::cError := "Invalid BMP -- file too small"
	Return .F.
Endif

// Aloca o Buffer e lê o arquivo intero 
// para arquivos maiores que 1MB, aumentar o tamanho 
// máximo de string no AdvPL -- MaxStringSize

cBuffer := space(::nFileSize) 
fRead(nH,@cBuffer,::nFileSize)
fClose(nH)

::cFormat := substr(cBuffer,0,2)

If ::cFormat <> "BM"
	::cError := "Unknow BMP Format ["+::cFormat+"]"
	Return .F.
Endif

cBinSize := substr(cBuffer,3,4)
nBmpSize := bin2l(cBinSize)

If ::nFileSize <> nBmpSize
	::cError := "BMP Incorrect Format -- File size mismatch"
	Return .F.
Endif


/*
                                                
Windows BITMAPINFOHEADER[1]

Offset (hex)	Offset (dec)	Size (bytes)	Information 
0E	14	4	the size of this header (40 bytes)
12	18	4	the bitmap width in pixels (signed integer)
16	22	4	the bitmap height in pixels (signed integer)
1A	26	2	the number of color planes (must be 1)
1C	28	2	the number of bits per pixel, which is the color depth of the image. Typical values are 1, 4, 8, 16, 24 and 32.
1E	30	4	the compression method being used. See the next table for a list of possible values
22	34	4	the image size. This is the size of the raw bitmap data; a dummy 0 can be given for BI_RGB bitmaps.
26	38	4	the horizontal resolution of the image. (pixel per metre, signed integer)
2A	42	4	the vertical resolution of the image. (pixel per metre, signed integer)
2E	46	4	the number of colors in the color palette, or 0 to default to 2n
32	50	4	the number of important colors used, or 0 when every color is important; generally ignored

*/

::nOffSet    := bin2l(substr(cBuffer,11,4))
::nHeadSize  := bin2l(substr(cBuffer,15,4))
::nWidth     := bin2l(substr(cBuffer,19,4))
::nHeight    := bin2l(substr(cBuffer,23,4))
::nColorPlanes := bin2i(substr(cBuffer,27,2))
::nBPP       := bin2i(substr(cBuffer,29,2))
::nCompress  := bin2l(substr(cBuffer,31,4))
::nRawData   := bin2l(substr(cBuffer,35,4))
::nHRes      := bin2l(substr(cBuffer,39,4))
::nVRes      := bin2l(substr(cBuffer,43,4))
::nColorPal  := bin2l(substr(cBuffer,47,4))
::nImpColors := bin2l(substr(cBuffer,51,4))

If ::nHeight < 0 
	// Linhas do Bitmap "de cima pra baixo"
	// Normalmente o formato padrao é "Bottom-Up" ( de baixo pra cima ) 
	lTopDown := .T.
	::nHeight := abs(::nHeight)
Endif

// Tamanho calculado de cada linha em bytes
::nRowSize   := int( ( (::nBPP*::nWidth) + 31 ) / 32 ) * 4 

// Tabela de cores para 4 bytes por pixel ( 16 cores ) 
::aColors := {}
If ::nBPP = 1
	nCores := 2
ElseIF ::nBPP = 4
	nCores := 16
ElseIF ::nBPP = 8
	nCores := 256
Endif
	
nPos := 55
For nCor := 0 to nCores-1
	
	nBlue  := asc(substr(cBuffer,nPos,1))
	nGreen := asc(substr(cBuffer,nPos+1,1))
	nRed   := asc(substr(cBuffer,nPos+2,1))
	nAlpha := asc(substr(cBuffer,nPos+3,1))
	
	aadd(::aColors,{nBlue,nGreen,nRed,nAlpha})
	
	nPos += 4
	
	// Conout("BGRA("+cValToChar(nCor)+") = ("+cValToChar(nBlue)+","+cValToChar(nGreen)+","+cValToChar(nRed)+","+cValToChar(nAlpha)+")")
	
Next

// Leitura dos dados na matriz

::aMatrix := {}

For nL := 0 to ::nHeight-1
	nReadOffset := ::nOffSet + ( ::nRowSize * nL ) + 1
	For nC := 0 to ::nWidth-1
		nByte := asc(substr(cBuffer,nReadOffset,1))
		If ::nBPP == 1 
			// Bitmap monocromatico 
			// um bit por pixel, converte para binario direto 
			// 0 = preto, 1 = branco
			cBits := NTOBIT8(nByte) 
			For nI := 1 to 8
				aadd(aRow,IIF(substr(cBits,nI,1)=='0',0,1))
			Next
		ElseIf ::nBPP == 4
			// Bitmap de 16 cores
			// 2 pixels por byte
			cBits := NTOBIT8(nByte) 
			aadd(aRow, BITSTON(Substr(cBits,1,4)))
			aadd(aRow, BITSTON(Substr(cBits,5,4)))
		ElseIf ::nBPP == 8
			// Bitmap de 256 cores
			// 1 pixels por byte
			cBits := NTOBIT8(nByte) 
			aadd(aRow, BITSTON(cBits))
		Else
			UserException("Unsupported ("+cValToChar(::nBPP)+") Bytes per Pixel")
		Endif
		nReadOffset++
	Next
	aSize(aRow,::nWidth)
	If lTopDown
		// Armazenamento de cima pra baixo
		aadd(::aMatrix,aClone(aRow))
	Else
		// Windows BMP normalmente é armazenado de baixo pra cima
		// Somente é de cima pra baixo caso a altura seja negativa
		aadd(::aMatrix,NIL)
		aIns(::aMatrix,1)
		::aMatrix[1] := aClone(aRow)
	Endif
	aSize(aRow,0)
Next

Return .T.

// Recupera a última informação de erro da Classe

METHOD GetErrorStr()  CLASS ZBITMAP 
Return ::cError

// Limpa a imagem preenchendo os pontos com 
// a cor de fundo 

METHOD Clear(L1,C1,L2,C2) CLASS ZBITMAP
Local nL, nC

IF pCount() == 0
	// Limpa a imagem inteira
	L1 := 0
	C1 := 0
	L2 := ::nHeight-1
	C2 := ::nWidth-1
Else
	// Valida coordenadas informadas para limpeza
	IF L1 < 0 .or. L1 >= ::nHeight
		::cError := "Invalid 1o Line -- Out Of Image Area"
		Return .F.
	ElseIF L2 < 0 .or. L2 >= ::nHeight
		::cError := "Invalid 2o Line -- Out Of Image Area"
		Return .F.
	ElseIf C1 < 0 .or. C1 >= ::nWidth
		::cError := "Invalid 1o Column -- Out Of Image Area"
		Return .F.
	ElseIf C2 < 0 .or. C2 >= ::nWidth
		::cError := "Invalid 2o Column -- Out Of Image Area"
		Return .F.
	ElseIf L1 > L2
		::cError := "Invalid Lines -- Order mismatch"
		Return .F.
	ElseIf C1 > C2
		::cError := "Invalid Columns -- Order mismatch"
		Return .F.
	Endif
Endif

// Limpa a área informada
For nL := L1+1 to L2+1
	For nC := C1+1 to C2+1
		::aMatrix[nL][nC] := ::nBGColor
	Next
Next

Return

// -------------------------------------------------
// Seta um pixel com uma cor em uma coordenada 
// linha e coluna, base 0 , cor é opcional 
// Diametro da caneta (Pen) opcional

METHOD SETPIXEL(nL,nC,nColor,nPen) CLASS ZBITMAP
Local nRow := nL+1
Local nCol := nC+1

If nPen = NIL
	nPen := ::nPenSize
Endif

IF nColor = NIL 
	nColor := ::nFRColor
Endif

IF nRow < 1 .or. nRow > ::nHeight
	return
ElseIF nCol < 1 .or. nCol > ::nWidth
	return
Endif

::aMatrix[nRow][nCol] := nColor

IF nPen > 1
	// 2x2
	::aMatrix[nRow+1][nCol  ] := nColor
	::aMatrix[nRow  ][nCol+1] := nColor
	::aMatrix[nRow+1][nCol+1] := nColor
Endif

If nPen > 2
	// 3x3
	::aMatrix[nRow-1][nCol  ] := nColor
	::aMatrix[nRow+1][nCol  ] := nColor
	::aMatrix[nRow  ][nCol-1] := nColor
	::aMatrix[nRow  ][nCol+1] := nColor
Endif

If nPen > 3
	// 4x4
	::aMatrix[nRow+2][nCol  ] := nColor
	::aMatrix[nRow  ][nCol+2] := nColor
	::aMatrix[nRow+2][nCol+2] := nColor
Endif

IF nPen > 4
	// Caneta Acima de 4 ..
	// Verifica limites da imagem 
	nHalf := nPen/2
	For nRow := nL+1-nHalf TO nL+1+nHalf
		If nRow > 0 .and. nRow <= ::nHeight
			For nCol := nC+1-nHalf TO nC+1+nHalf
				IF nCol > 0 .and. nCol <= ::nWidth
					::aMatrix[nRow][nCol] := nColor
				Endif
			Next
		Endif
	Next
Endif

Return
              
// -------------------------------------------------
// Retorna a cor de um pixel 
// linha e coluna, base 0 
METHOD GETPIXEL(nRow,nCol) CLASS ZBITMAP
Return ::aMatrix[nRow+1][nCol+1]

// -------------------------------------------------
// Retorna a cor de fundo da imagem 
// se informado um parametro, seta a cor de fundo 
METHOD BgColor(nSet) CLASS ZBITMAP
If pCount()>0
	::nBgColor := nSet
Endif
Return ::nBgColor

// --------------------------------------------------------
// Faz o "negativo" da imagem 
// Recalcula as cores complementares da tabela de cores

METHOD Negative()  CLASS ZBITMAP
Local nI
For nI := 1 to len(::aColors)
	::aColors[nI][1] := 255-::aColors[nI][1]
	::aColors[nI][2] := 255-::aColors[nI][2]
	::aColors[nI][3] := 255-::aColors[nI][3]
Next
Return

// ---------------------------------------------------
// Salva o arquivo em disco                           

METHOD SaveToBMP(cFile)  CLASS ZBITMAP
Local nH, nI
Local cHeader := ''
Local cHeadInfo := ''

If ::nBPP <> 1	.and. ::nBPP <> 4 .and. ::nBPP <> 8
	UserException("Format not implemented (yet) to save")
Endif

nH := fCreate(cFile)
           
cHeader := "BM"               // 2 bytes
cHeader += L2bin(::nFileSize) // 4 bytes 
cHeader += chr(0)+chr(0)      // 2 bytes
cHeader += chr(0)+chr(0)      // 2 bytes
cHeader += L2bin(::nOffset)   // 4 bytes

// Grava o primeiro Header
fWrite(nH,cHeader,len(cHEader)) // 14 bytes 

cHeadInfo += L2bin(40)             // This Header Size
cHeadInfo += L2bin(::nWidth)       // 
cHeadInfo += L2bin(::nHeight)      // 
cHeadInfo += I2Bin(::nColorPlanes) // Color planes
cHeadInfo += I2Bin(::nBPP)         // Bits Per Pixel
cHeadInfo += L2bin(::nCompress)    // Compression Method ( 0 = No Compression ) 
cHeadInfo += L2bin(::nRawData)     // RAW Data Size 
cHeadInfo += L2bin(::nHRes)        // Resolucao Horizontal
cHeadInfo += L2bin(::nVRes)        // Resolucao vertical
cHeadInfo += L2bin(::nColorPal)    // Color Pallete
cHeadInfo += L2bin(::nImpColors)   // Important Colors

// Até aqui o Header ocupou 54 bytes

// BMP Monocromatico 
// Para o Offset 62, ainda faltam 8 bytes 
// a tabela de cores tem apenas duas entradas, preto e branco 

// BMP de 16 cores
// Até o offset 118 sao 64 bytes
// Tabela de Cores em BGRA - ( Blue Green Red Alpha ) 4 bytes por cor

For nI := 1 to len(::aColors)
	cHeadInfo += chr(::aColors[nI][1])+chr(::aColors[nI][2])+chr(::aColors[nI][3])+chr(::aColors[nI][4])
Next

fWrite(nH,cHeadInfo,len(cHeadInfo))
            
// Armazena por default a imagem de baixo pra cima 

If ::nBPP == 1
	
	// Gravação de imagem monocromática
	For nL := ::nHeight to 1 STEP -1
		cBinRow := ''
		cBitRow := ''
		For nC := 1 to ::nWidth
			cBitRow += chr( 48 + ::aMatrix[nL][nC])
		Next
		While len(cBitRow)%32 > 0
			// Padding Bits to 32 ( 4 bytes )
			cBitRow += '1'
		Enddo
		For nC := 0 to len(cBitRow)-1 STEP 8
			cByteBit := Substr(cBitRow,nC+1,8)
			nByte    := BitsToN(cByteBit)
			cBinRow  += Chr(nByte)
		Next
		while len(cBinRow) < ::nRowSize
			// Padding Bytes ( ASCII 0 )
			cBinRow += Chr(0)
		Enddo
		// Grava os bytes da linha no arquivo
		fWrite(nH,cBinRow)
	Next
	
ElseIf ::nBPP == 4

	// Gravação de imagem 16 cores
	// 4 bits por cor

	For nL := ::nHeight to 1 STEP -1
		cBinRow := ''
		cBitRow := ''
		For nC := 1 to ::nWidth 
			cBitRow += Right(NTOBITS(::aMatrix[nL][nC]),4)
			IF len(cBitRow) == 8 
				cBinRow += chr(BITSToN(cBitRow))
				cBitRow := ''
			Endif
		Next
		If !Empty(cBitRow)
			cBitRow += '0000'
			cBinRow += chr(BITSToN(cBitRow))
			cBitRow := ''
		Endif
		while len(cBinRow) < ::nRowSize
			// Padding Bytes ( ASCII 0 )
			cBinRow += Chr(0)
		Enddo
		// Grava os bytes da linha no arquivo
		fWrite(nH,cBinRow)
	Next
	
ElseIf ::nBPP == 8

	// Gravação de imagem 256 cores
	// 8 bits por cor

	For nL := ::nHeight to 1 STEP -1
		cBinRow := ''
		For nC := 1 to ::nWidth 
			cBinRow += chr(::aMatrix[nL][nC])
		Next
		while len(cBinRow) < ::nRowSize
			// Padding Bytes ( ASCII 0 )
			cBinRow += Chr(0)
		Enddo
		// Grava os bytes da linha no arquivo
		fWrite(nH,cBinRow)
	Next
	

Else
	UserException("TODO")
	
Endif
fClose(nH)

Return .T.

// ---------------------------------------------------
// Desenha um retängulo na cor e espessura especificadas
// Cor e espessura sao opcionais
// Espessura > 1 , preenche a área interna do retängulo 

METHOD Rectangle(L1,C1,L2,C2,nColor,nPen)  CLASS ZBITMAP
Local nL , nC

If nPen = NIL 
	nPen := ::nPenSize
Endif

For nPen := 0 to nPen-1

	// Espessura de linha de retangulo sempre 
	// para a área interna do Retangulo 

	// Traça linhas horizontais
	For nC := C1+nPen to C2-nPen
		::SetPixel(L1+nPen,nC,nColor,1)
		::SetPixel(L2-nPen,nC,nColor,1)
	Next
	
	// Traca as linhas verticais
	For nL := L1+nPen to L2-nPen
		::SetPixel(nL,C1+nPen,nColor,1)
		::SetPixel(nL,C2-nPen,nColor,1)
	Next
	
Next

Return

// ---------------------------------------------------
// Traça uma linha entre as coordenadas informadas
// pode ser horizontal, vertical ou diagonal 

METHOD Line(L1,C1,L2,C2,nColor,nPen)  CLASS ZBITMAP
Local nDH , nDV 
Local nStepH , nStepV 
Local nPoints 
Local nRow,nCol

If nPen = NIL
	nPen := ::nPenSize
Endif
	
	// Calcula as distancias entre os pontos
	nDH := C2 - C1 
	nDV := L2 - L1

	nStepH := 1
	nStepV := 1
	
	// Calcula a maior distancia e o passo
	// decimal
	If abs(nDH) > abs(nDV)
		nStepV := nDV / nDH
	ElseIf abs(nDV) > abs(nDH)
		nStepH := nDH / nDV
	Endif
	
	// Pontos que vao compor a reta
	nPoints := Max(abs(nDV),abs(nDH))
	
	// Traça a reta ponto a ponto , menos o ultimo
	nRow := L1 
	nCol := C1 
	For nX := 0 to nPoints-1
		::SetPixel(Round(nRow,1),round(nCol,1),nColor,nPen)
		nRow += nStepV
		nCol += nStepH
	Next
	
	// O ultimo ponto seta com as coordenadas informadas
	// Pode haver perda de precisao na aritmetica dos passos
	::SetPixel(L2,C2,nColor)

Return

// ---------------------------------------------------
// Desenha um círculo com o centro na coordenada informada

METHOD Circle(nL , nC , nRadius , nColor, nPen ) CLASS ZBITMAP
Local nRow , nCol
Local nAngle
Local nPoints
Local nStep
Local nI,nR

If nPen = NIL
	nPen := ::nPenSize
Endif

For nR := 1 to nPen
	// Seno e cosseno em Radianos
	// Faz o loop de 0 a 2*PI para calcular as coordenadas
	// dos pontos para desenhar o círculo
	nAngle := 0
	nPoints := 2 * PI * nRadius
	nStep   := (2*PI) / nPoints
	For nI := 0 to nPoints
		nRow := round(Sin(nAngle) * nRadius,1)
		nCol := round(Cos(nAngle) * nRadius,1)
		::SetPixel(nL-nRow,nC+nCol,nColor)
		nAngle += nStep
	Next
	nRadius--
Next

Return


METHOD SetBPP(nBPP) CLASS ZBITMAP

IF ::nBPP == 1 .and. nBPP == 4

	// Trocou de preto e branco para 16 cores 
	// Nao mexe em nada 
	
	::nBPP := 4
	
Endif

Return


STATIC Function GetColorTab(nBPP)
Local aColors := {}
    
If nBPP = 1 

	aadd(aColors,{ 0   , 0   , 0   , 0 })  // 0  Black
	aadd(aColors,{ 255 , 255 , 255 , 0 })  // 15 White

ElseIf nBPP = 4
	
	// Paleta de Cores Padrao ( 16 cores )	// Azul Verde Vermelho Alpha ( BGRA - Blue , Green , Red , Alpha ) 
	
	aadd(aColors,{ 0   , 0   , 0   , 0 })  // 0  Black
	aadd(aColors,{ 0   , 0   , 128 , 0 })  // 1  Maroon
	aadd(aColors,{ 0   , 128 , 0   , 0 })  // 2  Green
	aadd(aColors,{ 0   , 128 , 128 , 0 })  // 3  Olive
	aadd(aColors,{ 128 , 0   , 0   , 0 })  // 4  Navy
	aadd(aColors,{ 128 , 0   , 128 , 0 })  // 5  Magenta or Purple
	aadd(aColors,{ 128 , 128 , 0   , 0 })  // 6  Teal
	aadd(aColors,{ 128 , 128 , 128 , 0 })  // 7  Gray
	aadd(aColors,{ 192 , 192 , 192 , 0 })  // 8  Silver
	aadd(aColors,{ 0   , 0   , 255 , 0 })  // 9  Red
	aadd(aColors,{ 0   , 255 , 0   , 0 })  // 10 Lime Green
	aadd(aColors,{ 0   , 255 , 255 , 0 })  // 11 Yelow
	aadd(aColors,{ 255 , 0   , 0   , 0 })  // 12 Blue
	aadd(aColors,{ 255 , 0   , 255 , 0 })  // 13 Fuchsia
	aadd(aColors,{ 255 , 255 , 0   , 0 })  // 14 Cyan or Aqua
	aadd(aColors,{ 255 , 255 , 255 , 0 })  // 15 White
	
Endif

Return aColors

// Metodo de pintura 
// Dado um ponto e uma cor, todos os pontos ligados a ele
// na horizontal e vertical serão pintados, caso a cor de pintura
// seja diferente da cor do ponto, sendo apenas considerados 
// os pontos adjacentes com cor igual ao ponto original 

METHOD Paint(nL,nC,nColor)  CLASS ZBITMAP
Local aPaint := {}                   
Local nCurrent
Local nLNext, nCNext
Local nPL, nPC, nPColor

IF nColor = NIL 
	nColor := ::nFRColor
Endif

// Pega a cor do ponto atual 
nPColor := ::GetPixel(nL,nC)

aadd(aPaint,{nL,nC})

While len(aPaint) > 0 

	// Pega o primeiro ponto pendente
	nPL := aPaint[1][1]
	nPC := aPaint[1][2]

	// Remove das pendencias
	aDel(aPaint,1)
	aSize(aPaint,len(aPaint)-1)
	
	// Pega a cor desta coordenada
	nCurrent := ::GetPixel(nPL,nPC)
	
	If nCurrent <> nColor .and. nCurrent == nPColor

		// Se o ponto nao tem a cor final, e é da cor 
		// do ponto original, pinta ele 
		::SetPixel(nPL,nPC,nColor,1)      
		
		// ao pintar um ponto, seta os pontos adjacentes
		// como pendencias caso eles tambem precisem ser pintados
 
		// Ponto superior
		nLNext := nPL-1
		nCNext := nPC
		If nLNext >= 0 
			nCurrent := ::GetPixel(nLNext,nCNext)
			If nCurrent <> nColor .and. nCurrent == nPColor
				aadd(aPAint,{nLNext,nCNext}) 
			Endif
		Endif
		
		// Ponto inferior
		nLNext := nPL+1
		nCNext := nPC
		If nLNext < ::nHeight 
			nCurrent := ::GetPixel(nLNext,nCNext)
			If nCurrent <> nColor .and. nCurrent == nPColor
				aadd(aPaint,{nLNext,nCNext}) 
			Endif
		Endif

		// Ponto a direita
		nLNext := nPL
		nCNext := nPC+1
		If nLNext < ::nWidth
			nCurrent := ::GetPixel(nLNext,nCNext)
			If nCurrent <> nColor .and. nCurrent == nPColor
				aadd(aPaint,{nLNext,nCNext}) 
			Endif
		Endif

		// Ponto a esquerda
		nLNext := nPL
		nCNext := nPC-1
		If nLNext >= 0
			nCurrent := ::GetPixel(nLNext,nCNext)
			If nCurrent <> nColor .and. nCurrent == nPColor
				aadd(aPaint,{nLNext,nCNext}) 
			Endif
		Endif
		
	Endif

Enddo

Return

// Inverte horizontalmente uma área da imagem
// Ou a imagem inteira caso a área nao seja especificada
METHOD FlipH(L1,C1,L2,C2) CLASS ZBITMAP
Local nL  , nC            
Local nCol, nSwap
IF pCount() == 0
	// Faz flip horizontal da imagem inteira
	L1 := 0
	C1 := 0
	L2 := ::nHeight-1
	C2 := ::nWidth-1
Else
	// Valida coordenadas informados
	IF L1 < 0 .or. L1 >= ::nHeight
		::cError := "Invalid 1o Line -- Out Of Image Area"
		Return .F.
	ElseIF L2 < 0 .or. L2 >= ::nHeight
		::cError := "Invalid 2o Line -- Out Of Image Area"
		Return .F.
	ElseIf C1 < 0 .or. C1 >= ::nWidth
		::cError := "Invalid 1o Column -- Out Of Image Area"
		Return .F.
	ElseIf C2 < 0 .or. C2 >= ::nWidth
		::cError := "Invalid 2o Column -- Out Of Image Area"
		Return .F.
	ElseIf L1 > L2
		::cError := "Invalid Lines -- Order mismatch"
		Return .F.
	ElseIf C1 > C2
		::cError := "Invalid Columns -- Order mismatch"
		Return .F.
	Endif
Endif

For nL := L1+1 to L2+1
	nCol := C2+1
	For nC := C1 + 1 TO C1 + INT( ( C2-C1 ) / 2 ) + 1
		ZSWAP( ::aMatrix[nL][nC] , ::aMatrix[nL][nCol] , nSwap )
		nCol--
	Next
Next

Return .T. 

// Inverte verticalmente uma área da imagem
// Ou a imagem inteira caso a área nao seja especificada
METHOD FlipV(L1,C1,L2,C2) CLASS ZBITMAP
Local nL  , nC            
Local nRow, nSwap
IF pCount() == 0
	// Faz flip vertical da imagem inteira
	L1 := 0
	C1 := 0
	L2 := ::nHeight-1
	C2 := ::nWidth-1
Else
	// Valida coordenadas informados
	IF L1 < 0 .or. L1 >= ::nHeight
		::cError := "Invalid 1o Line -- Out Of Image Area"
		Return .F.
	ElseIF L2 < 0 .or. L2 >= ::nHeight
		::cError := "Invalid 2o Line -- Out Of Image Area"
		Return .F.
	ElseIf C1 < 0 .or. C1 >= ::nWidth
		::cError := "Invalid 1o Column -- Out Of Image Area"
		Return .F.
	ElseIf C2 < 0 .or. C2 >= ::nWidth
		::cError := "Invalid 2o Column -- Out Of Image Area"
		Return .F.
	ElseIf L1 > L2
		::cError := "Invalid Lines -- Order mismatch"
		Return .F.
	ElseIf C1 > C2
		::cError := "Invalid Columns -- Order mismatch"
		Return .F.
	Endif
Endif

// Troca os pontos da primeira linha com a ultima
// depois da segunda com a penultima 
// até chegar na linha central da área a ser invertida      
nRow := L2+1
For nL := L1+1 to L1 + INT( ( L2-L1 ) / 2 ) + 1
	For nC := C1+1 to C2+1
		ZSWAP( ::aMatrix[nL][nC] , ::aMatrix[nRow][nC] , nSwap )
	Next
	nRow--
Next

Return .T. 

// ----------------------------------------------------
// Copia uma parte da imagem para a área interna 
// de transferencia e limpa a área da imagem

METHOD Cut(L1,C1,L2,C2)            CLASS ZBITMAP
::Copy(L1,C1,L2,C2)
::Clear(L1,C1,L2,C2)
Return .T. 

// ----------------------------------------------------
// Copia uma parte da imagem para a área interna de transferencia

METHOD Copy(L1,C1,L2,C2)           CLASS ZBITMAP
Local nL  , nC            
Local aRow := {}

IF pCount() == 0
	// Copia a imagem inteira para a area de transferencia
	::aTrans := aClone(::aMatrix)
    Return .T.
Endif

// Valida coordenadas informados
IF L1 < 0 .or. L1 >= ::nHeight
	::cError := "Invalid 1o Line -- Out Of Image Area"
	Return .F.
ElseIF L2 < 0 .or. L2 >= ::nHeight
	::cError := "Invalid 2o Line -- Out Of Image Area"
	Return .F.
ElseIf C1 < 0 .or. C1 >= ::nWidth
	::cError := "Invalid 1o Column -- Out Of Image Area"
	Return .F.
ElseIf C2 < 0 .or. C2 >= ::nWidth
	::cError := "Invalid 2o Column -- Out Of Image Area"
	Return .F.
ElseIf L1 > L2
	::cError := "Invalid Lines -- Order mismatch"
	Return .F.
ElseIf C1 > C2
	::cError := "Invalid Columns -- Order mismatch"
	Return .F.
Endif

::aTrans := {}

// Copia a área informada para a area de transferencia interna
For nL := L1+1 to L2+1
	For nC := C1+1 to C2+1
		aadd(aRow,::aMatrix[nL][nC])
	Next
	aadd(::aTrans,aClone(aRow))
	aSize(aRow,0)
Next

Return .T.

// ----------------------------------------------------
// Plota a imagem da area interna de transferencia na coordenada indicada

METHOD Paste(L1,C1)                CLASS ZBITMAP
Local nL , nC

// Valida a area de transferencis
If empty(::aTrans)
	::cError := "Empty Transfer Area"
	Return .F.
Endif

// Valida as cordenadas
IF L1 < 0 .or. L1 >= ::nHeight
	::cError := "Invalid Target Line -- Out Of Image Area"
	Return .F.
ElseIf C1 < 0 .or. C1 >= ::nWidth
	::cError := "Invalid Target Column -- Out Of Image Area"
	Return .F.
Endif
                       
// Plota a imagem da area de transferencia
// Validando as coordenadas de colagem caso 
// a imagem colada nas coordenadas saia 
// "fora" da área total da imagem 
For nL := 0 to len(::aTrans)-1
	IF L1+nL < ::nHeight
		For nC := 0 to len(::aTrans[nL+1])-1
			If C1+nC < ::nWidth
				::aMatrix[L1+nL+1][C1+nC+1] := ::aTrans[nL+1][nC+1]
			Else
				EXIT
			Endif
		Next
	Else
		EXIT
	Endif
Next

Return .T. 

// Salva a imagem em disco como JPEG
METHOD SaveToJPG(cJpgFile)         CLASS ZBITMAP
Local cTmpFile := "\tmpbitmap.bmp"

If file(cTmpFile)
	Ferase(cTmpFile)
Endif

::SaveToBMP(cTmpFile)

nRet := BMPTOJPG(cTmpFile,cJpgFile)

conout(nRet)

Return


