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
#include 'zlib.ch' 

/* ======================================================

Classe		zBarCode128
Autor		Julio Wittwer
Data 		19/07/2019
Descrição	Permite gerar um Codigo de Barras Code 128 

====================================================== */

Class zBarCode128 FROM LONGNAMECLASS

  METHOD NEW()
  METHOD Generate()  
  METHOD BuildBmp(aSeq)

Endclass

METHOD NEW() CLASS zBarCode128
Return self

METHOD Generate(cBarData) CLASS zBarCode128
Local lOnlyNum
Local nTam
Local cBarData
Local nCheck
Local aSequence := {}                                   

If empty(cBarData)
	Return NIL
Endif

nTam := len(cBarData)
lOnlyNum := IsNumbers(cBarData)

If lOnlyNum
	
	// Geração numérica
	// pode alternar Code A e Code C para zeros
	// Code C trabalha sempre em blocos de 2 numeros
	// Boa pratica : Com numero de digitos impar, coloca o primeiro
	// digito com Code A , e as demaps tuplas em Code C
	IF (nTam % 2) != 0
		// Impar, comeca com CODE A
		aadd(aSequence , 103 )
		aadd(aSequence , val(left(cBarData,1))+16 )
		cBarData := substr(cBarData,2)
	Else
		// Par, codifica direto em Code C
		aadd(aSequence , 105 )
	Endif

	// Codifica os numeros em pares
	While len(cBarData) > 0 
		nTupla := val(left(cBarData,2))
		aadd(aSequence , nTupla )
		cBarData := substr(cBarData,3)
	Enddo
	
Else

	// Alfanumérico
	// por enquanto chumbado direto com Code B ( alfabeto maior ) 
	aadd(aSequence , 104 )
	
	While len(cBarData) > 0 
		cChar := left(cBarData,1)
		nValor := GetCodeB(cChar)
		aadd(aSequence , nValor )
		cBarData := substr(cBarData,2)
	Enddo
	
Endif

// Com a sequencia montada, agora gera o checksum 
nCheck := CheckSum(aSequence)

// Acrescena o Checksum 
aadd(aSequence,nCheck)

// Finaliza com o STOP Pattern
aadd(aSequence,108)

Return aSequence



METHOD BuildBmp(aSeq) CLASS zBarCode128
Local nI
Local cPlot := ''
Local oBmp

// Sequencia em Base 0 na tabela                    
// Soma 1 para pegar o elemento certo no alfabeto
for nI := 1 to len(aSeq)
	// conout(str(aSeq[nI],3)+' | '+GetBitBar(aSeq[nI]))
	cPlot += GetBitBar(aSeq[nI])
Next

// Cria o Bitmap Monocromatico na memória 
oBMP := ZBITMAP():NEW( len(cPlot)+20,40, 1 )
oBMP:nPenSize := 1

// Pinta as barras
For nI := 1 to len(cPlot)
	If substr(cPlot,nI,1) == '1'
		oBmp:Line(5,nI+9,35,nI+9,0,1)
	Endif
Next

Return oBmp


STATIC Function IsNumbers(cData)
Local nI , nT
Local cChar
nT := len(cData)
For nI := 1 to  nT
	cChar := substr(cData,nI,1)
	If cChar < '0' .or. cChar > '9'
		Return .F.
	Endif
Next
Return .T.


STATIC _aBar128 := LoadBar128()

STATIC Function LoadBar128()
Local aBar128 := {}

// Value,128A,128B,128C,Pattern,Widths

aadd( aBar128 , {  0,' ',' ','00','11011001100','212222'})
aadd( aBar128 , {  1,'!','!','01','11001101100','222122'})
aadd( aBar128 , {  2,'"','"','02','11001100110','222221'})
aadd( aBar128 , {  3,'#','#','03','10010011000','121223'})
aadd( aBar128 , {  4,'$','$','04','10010001100','121322'})
aadd( aBar128 , {  5,'%','%','05','10001001100','131222'})
aadd( aBar128 , {  6,'&','&','06','10011001000','122213'})
aadd( aBar128 , {  7,"'","'",'07','10011000100','122312'})
aadd( aBar128 , {  8,'(','(','08','10001100100','132212'})
aadd( aBar128 , {  9,')',')','09','11001001000','221213'})
aadd( aBar128 , { 10,'*','*','10','11001000100','221312'})
aadd( aBar128 , { 11,'+','+','11','11000100100','231212'})
aadd( aBar128 , { 12,',',',','12','10110011100','112232'})
aadd( aBar128 , { 13,'-','-','13','10011011100','122132'})
aadd( aBar128 , { 14,'.','.','14','10011001110','122231'})
aadd( aBar128 , { 15,'/','/','15','10111001100','113222'})
aadd( aBar128 , { 16,'0','0','16','10011101100','123122'})
aadd( aBar128 , { 17,'1','1','17','10011100110','123221'})
aadd( aBar128 , { 18,'2','2','18','11001110010','223211'})
aadd( aBar128 , { 19,'3','3','19','11001011100','221132'})
aadd( aBar128 , { 20,'4','4','20','11001001110','221231'})
aadd( aBar128 , { 21,'5','5','21','11011100100','213212'})
aadd( aBar128 , { 22,'6','6','22','11001110100','223112'})
aadd( aBar128 , { 23,'7','7','23','11101101110','312131'})
aadd( aBar128 , { 24,'8','8','24','11101001100','311222'})
aadd( aBar128 , { 25,'9','9','25','11100101100','321122'})
aadd( aBar128 , { 26,':',':','26','11100100110','321221'})
aadd( aBar128 , { 27,';',';','27','11101100100','312212'})
aadd( aBar128 , { 28,'<','<','28','11100110100','322112'})
aadd( aBar128 , { 29,'=','=','29','11100110010','322211'})
aadd( aBar128 , { 30,'>','>','30','11011011000','212123'})
aadd( aBar128 , { 31,'?','?','31','11011000110','212321'})
aadd( aBar128 , { 32,'@','@','32','11000110110','232121'})
aadd( aBar128 , { 33,'A','A','33','10100011000','111323'})
aadd( aBar128 , { 34,'B','B','34','10001011000','131123'})
aadd( aBar128 , { 35,'C','C','35','10001000110','131321'})
aadd( aBar128 , { 36,'D','D','36','10110001000','112313'})
aadd( aBar128 , { 37,'E','E','37','10001101000','132113'})
aadd( aBar128 , { 38,'F','F','38','10001100010','132311'})
aadd( aBar128 , { 39,'G','G','39','11010001000','211313'})
aadd( aBar128 , { 40,'H','H','40','11000101000','231113'})
aadd( aBar128 , { 41,'I','I','41','11000100010','231311'})
aadd( aBar128 , { 42,'J','J','42','10110111000','112133'})
aadd( aBar128 , { 43,'K','K','43','10110001110','112331'})
aadd( aBar128 , { 44,'L','L','44','10001101110','132131'})
aadd( aBar128 , { 45,'M','M','45','10111011000','113123'})
aadd( aBar128 , { 46,'N','N','46','10111000110','113321'})
aadd( aBar128 , { 47,'O','O','47','10001110110','133121'})
aadd( aBar128 , { 48,'P','P','48','11101110110','313121'})
aadd( aBar128 , { 49,'Q','Q','49','11010001110','211331'})
aadd( aBar128 , { 50,'R','R','50','11000101110','231131'})
aadd( aBar128 , { 51,'S','S','51','11011101000','213113'})
aadd( aBar128 , { 52,'T','T','52','11011100010','213311'})
aadd( aBar128 , { 53,'U','U','53','11011101110','213131'})
aadd( aBar128 , { 54,'V','V','54','11101011000','311123'})
aadd( aBar128 , { 55,'W','W','55','11101000110','311321'})
aadd( aBar128 , { 56,'X','X','56','11100010110','331121'})
aadd( aBar128 , { 57,'Y','Y','57','11101101000','312113'})
aadd( aBar128 , { 58,'Z','Z','58','11101100010','312311'})
aadd( aBar128 , { 59,'[','[','59','11100011010','332111'})
aadd( aBar128 , { 60,'\','\','60','11101111010','314111'})
aadd( aBar128 , { 61,']',']','61','11001000010','221411'})
aadd( aBar128 , { 62,'^','^','62','11110001010','431111'})
aadd( aBar128 , { 63,'_','_','63','10100110000','111224'})
aadd( aBar128 , { 64,chr(0),'`','64','10100001100','111422'})
aadd( aBar128 , { 65,chr(1),'a','65','10010110000','121124'})
aadd( aBar128 , { 66,chr(2),'b','66','10010000110','121421'})
aadd( aBar128 , { 67,chr(3),'c','67','10000101100','141122'})
aadd( aBar128 , { 68,chr(4),'d','68','10000100110','141221'})
aadd( aBar128 , { 69,chr(5),'e','69','10110010000','112214'})
aadd( aBar128 , { 70,chr(6),'f','70','10110000100','112412'})
aadd( aBar128 , { 71,chr(7),'g','71','10011010000','122114'})
aadd( aBar128 , { 72,chr(8),'h','72','10011000010','122411'})
aadd( aBar128 , { 73,chr(9),'i','73','10000110100','142112'})
aadd( aBar128 , { 74,chr(10),'j','74','10000110010','142211'})
aadd( aBar128 , { 75,chr(11),'k','75','11000010010','241211'})
aadd( aBar128 , { 76,chr(12),'l','76','11001010000','221114'})
aadd( aBar128 , { 77,chr(13),'m','77','11110111010','413111'})
aadd( aBar128 , { 78,chr(14),'n','78','11000010100','241112'})
aadd( aBar128 , { 79,chr(15),'o','79','10001111010','134111'})
aadd( aBar128 , { 80,chr(16),'p','80','10100111100','111242'})
aadd( aBar128 , { 81,chr(17),'q','81','10010111100','121142'})
aadd( aBar128 , { 82,chr(18),'r','82','10010011110','121241'})
aadd( aBar128 , { 83,chr(19),'s','83','10111100100','114212'})
aadd( aBar128 , { 84,chr(20),'t','84','10011110100','124112'})
aadd( aBar128 , { 85,chr(21),'u','85','10011110010','124211'})
aadd( aBar128 , { 86,chr(22),'v','86','11110100100','411212'})
aadd( aBar128 , { 87,chr(23),'w','87','11110010100','421112'})
aadd( aBar128 , { 88,chr(24),'x','88','11110010010','421211'})
aadd( aBar128 , { 89,chr(25),'y','89','11011011110','212141'})
aadd( aBar128 , { 90,chr(26),'z','90','11011110110','214121'})
aadd( aBar128 , { 91,chr(27),'{','91','11110110110','412121'})
aadd( aBar128 , { 92,chr(28),'|','92','10101111000','111143'})
aadd( aBar128 , { 93,chr(29),'}','93','10100011110','111341'})
aadd( aBar128 , { 94,chr(30),'~','94','10001011110','131141'})
aadd( aBar128 , { 95,chr(31),NIL,'95','10111101000','114113'}) // DEL
aadd( aBar128 , { 96, NIL   ,NIL,'96','10111100010','114311'}) // FNC 3','FNC 3'
aadd( aBar128 , { 97, NIL   ,NIL,'97','11110101000','411113'}) // 'FNC 2','FNC 2'
aadd( aBar128 , { 98, NIL   ,NIL,'98','11110100010','411311'}) // 'Shift B','Shift A'
aadd( aBar128 , { 99, NIL   ,NIL,'99','10111011110','113141'}) // 'Code C','Code C'
aadd( aBar128 , {100, NIL   ,NIL, NIL,'10111101110','114131'}) // 'Code B','FNC 4','Code B'
aadd( aBar128 , {101, NIL   ,NIL, NIL,'11101011110','311141'}) // 'FNC 4','Code A','Code A'
aadd( aBar128 , {102, NIL   ,NIL, NIL,'11110101110','411131'}) // 'FNC 1','FNC 1','FNC 1'
aadd( aBar128 , {103, NIL   ,NIL, NIL,'11010000100','211412'}) // 3x'Start Code A'
aadd( aBar128 , {104, NIL   ,NIL, NIL,'11010010000','211214'}) // 3x'Start Code B'
aadd( aBar128 , {105, NIL   ,NIL, NIL,'11010011100','211232'}) // 3x'Start Code C'
aadd( aBar128 , {106, NIL   ,NIL, NIL,'11000111010','233111'}) // 'Stop','—','—'
aadd( aBar128 , {NIL, NIL   ,NIL, NIL,'11010111000','211133'})    // Reverse Stop
aadd( aBar128 , {NIL, NIL   ,NIL, NIL,'1100011101011','2331112'}) // Stop pattern (7 bars/spaces)

Return aBar128


STATIC Function CheckSum(aSequence)
Local nI , nT , nSum := 0 , nCheck
nT := len(aSequence)
nSum := aSequence[1]
For nI := 2 to nT           
	nSum += ( aSequence[nI] * (nI-1) )
Next
nCheck := nSum % 103                            
Return nCheck

STATIC Function GetBitBar(nValue)
Return _aBar128[nValue+1][5]

STATIC Function GetCodeB(cChar)
Local nPos := ascan(_aBar128,{|x| x[3] == cChar })
Return _aBar128[nPos][1]

