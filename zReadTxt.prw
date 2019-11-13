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



#include 'msobject.ch'
#include 'zlib.ch'

#ifndef CRLF
   #define CRLF chr(13)+chr(10)
#endif   

/* ======================================================================
Classe      zReadTxt
Autor       Júlio Wittwer
Data        10/2015
Descrição   Classe de leitura de arquivo TXT

Permite com alto desempenho a leitura de arquivos TXT 
utilizando o identificador de quebra de linha definido            

Buffer padrao de leitura e cache = 4 KB

====================================================================== */

#define DEFAULT_FILE_BUFFER	4096

CLASS zReadTxt FROM LONGNAMECLASS

	DATA nHnd       as Integer
	DATA cFName     as String     
	DATA cFSep      as String
	DATA nFerror    as Integer
	DATA nOsError   as Integer
	DATA cFerrorStr as String
	DATA nFSize     as Integer
	DATA nFReaded   as Integer
	DATA nFBuffer   as Integer
	DATA nFLine     as Integer

	DATA _Buffer    as Array
	DATA _PosBuffer as Integer
	DATA _Resto     as String
        
	// Metodos Pubicos
	METHOD New()
	METHOD Open()       
	METHOD Close()
	METHOD GetFSize()
	METHOD GetFLine() 
	METHOD GetError() 
	METHOD GetOSError()
	METHOD GetErrorStr()
	METHOD ReadLine()
	METHOD GoTop()

	// Metodos privados 	     
  METHOD _CleanLastErr() 
  METHOD _SetError()
  METHOD _SetOSError()

ENDCLASS
                   
/* --------------------------------------------------------------
zReadTxt::New( <cFName> , [cFSep] , [nFBuffer])

cFName   = Nome do arquivo a ser aberto 
cFSep    = String considerada como final de linha. Default = CRLF
nFBuffer = Tamanho do Buffer/Cache de leitura. Deve ser maior que 
           a maior linha a ser lida no arquivo. Default = 4 KB

--------------------------------------------------------------*/
                           
METHOD New( cFName , cFSep , nFBuffer ) CLASS zReadTxt

DEFAULT cFSep := CRLF 
DEFAULT nFBuffer := DEFAULT_FILE_BUFFER

::nHnd       := -1
::cFName     := cFName
::cFSep      := cFSep
::_Buffer    := {}
::_Resto     := ''
::nFSize     := -1
::nFLine     := 0 
::nFReaded   := 0 
::nFerror    := 0   
::nOsError   := 0 
::cFerrorStr := ''
::_PosBuffer := 0 
::nFBuffer   := nFBuffer

Return self


/* --------------------------------------------------------------
zReadTxt::Open( [iFMode] )

iFmode   = Modo de acesso ao arquivo, usando internamente na abertura
           em baixo nível -- usando FOpen() . Default = 0 

--------------------------------------------------------------*/
METHOD Open( iFMode ) CLASS zReadTxt

DEFAULT iFMode := 0

::_CleanLastErr()

If ::nHnd != -1
	_SetError(-1,"Open Error - File already open")
	Return .F.
Endif
 
// Abre o arquivo 
::nHnd := FOpen( ::cFName , iFMode )

// Testa a abertura
If ::nHnd < 0 
	::_SetOSError(-2,"Open File Error (OS)",ferror())
	Return .F.
Endif

// Pega o tamanho do Arquivo  
::nFSize := fSeek(::nHnd,0,2)

If ::nFSize < 0 
	::_SetOSError(-2,"FSeek File Error (OS)",ferror())
	fClose(::nHnd)
	::nHnd := -1
	Return .F.
Endif

// Reposiciona no inicio do arquivo 
fSeek(::nHnd,0)

Return .T.

              
/* --------------------------------------------------------------
zReadTxt::Close()

Fecha o arquivo para leitura atualmente aberto
--------------------------------------------------------------*/

METHOD Close() CLASS zReadTxt

::_CleanLastErr()

If ::nHnd == -1
	_SetError(-3,"Close Error - File already closed")
	Return .F.
Endif

// Clean file read cache
aSize(::_Buffer,0)

// Cleanup other properties
::_Resto     := ''
::nHnd       := -1
::nFSize     := -1
::nFReaded   := 0 
::_PosBuffer := 0 
::nFLine     := 0 

Return .T.


/* --------------------------------------------------------------
zReadTxt::GoTop()

Volta o arquivo para a primeira linha. 
--------------------------------------------------------------*/

Method GoTop() CLASS zReadTxt

::_CleanLastErr()

If ::nHnd == -1
	_SetError(-4,"GoTop Error - File not opened")
	Return .F.
Endif

// Clean file read cache
aSize(::_Buffer,0)

// Clear read properties
::_Resto     := ''
::nFReaded   := 0 
::_PosBuffer := 0 

If ::nHnd == -1
	_SetError(-4,"GoTop Error - File not opened")
	Return .F.
Endif

Return .T. 


/* --------------------------------------------------------------
zReadTxt::GoTop()

Lê uma linha do arquivo. 
--------------------------------------------------------------*/

METHOD ReadLine( /*@*/ cReadLine ) CLASS zReadTxt
Local cTmp := ''
Local cBuffer
Local nRPos
Local nRead

::_CleanLastErr()

If ::nHnd == -1
	_SetError(-4,"ReadLine Error - File not opened")
	Return .F.
Endif

// Incrementa o contador da posição do Buffer
::_PosBuffer++

If ( ::_PosBuffer <= len(::_Buffer) )

	// A proxima linha já está no Buffer ...
	// recupera a linha e retorna
	// incrementando o numero de linhas lido
	cReadLine := ::_Buffer[::_PosBuffer]
	::nFLine++
	Return .T.

Endif

If ( ::nFReaded < ::nFSize ) 

	// Nao tem linha no Buffer, mas ainda tem partes 
	// do arquivo para ler. Lê mais um pedaço
                                   
	cTmp := ''
	nRead := fRead(::nHnd , @cTmp, ::nFBuffer)
	if nRead < 0 
		::_SetOSError(-5,"Read File Error (OS)",ferror())
		Return .F.
	Endif
     
	// Soma a quantidade de bytes lida no acumulador
	::nFReaded += nRead

	// Considera no buffer de trabalho o resto 
	// da ultima leitura mais o que acabou de ser lido 
	cBuffer := ::_Resto + cTmp

	// Determina a ultima quebra
	nRPos := Rat(::cFSep,cBuffer)
	If nRPos > 0 
		// Pega o que sobrou apos a ultima quebra e guarda no resto
		// E Isola o resto do buffer atual 
		::_Resto := substr(cBuffer , nRPos + len(::cFSep))
		cBuffer := left(cBuffer , nRPos-1 )
	Else
		// Nao tem resto, o buffer de trabalho será considerado inteiro 
		// ( pode ser final de arquivo sem o ultimo separador ) 
		::_Resto := ''
	Endif

	// Limpa e Recria o array de cache                
	// Por default linhas vazias são ignoradas
	aSize(::_Buffer,0)
	::_Buffer := StrTokArr( cBuffer , ::cFSep )

	// Reseta posicionamento de buffer para o primeiro elemento
	// E Retorna a primeira linha do buffer
	::_PosBuffer := 1
	cReadLine := ::_Buffer[::_PosBuffer]

	Return .T.

Endif

// Chegou no final do arquivo ...           
::_SetError(-6,"File is in EOF")
Return .F. 

// --- Recupera o codigo de erro da ultima operação 
METHOD GetError() CLASS zReadTxt
Return ::nFerror

// --- Recupera o ultimo codigo de erro de SO da ultima operação 
METHOD GetOSError() CLASS zReadTxt
Return ::nOSError

// Recupera a ultima string com descrição do erro da ultima operação
METHOD GetErrorStr() CLASS zReadTxt
Local cErrStr := ''
cErrStr += "(ERR "+cValToChar(::nFerror)+") "
If ::nOSError != 0 
	cErrStr += "(OS_ERR "+cValToChar(::nOSError)+") "
Endif
cErrStr += ::cFerrorStr
Return cErrStr
   
// Recupera o tamanho do arquivo
// retorna -1 caso o arquivo esteja fechado
METHOD GetFSize() CLASS zReadTxt
Return ::nFSize
               
// Recupera o numero da linha atual lida do arquivo 
// retorna 0 caso o arquivo esteja fechado
// ou nenhuma linha tenha sido lida
METHOD GetFLine() CLASS zReadTxt
Return ::nFLine

// Metodo conceitualmente privado
// Seta um codigo de erro de operação da classe
METHOD _SetError(nCode,cStr) CLASS zReadTxt
::nFerror    := nCode
::cFerrorStr := cStr
Return

// Metodo conceitualmente privado
// Seta um codigo de erro de operação da classe
// associado a uma falha da API de arquivos 
// recebendo também um codigo de sistema operacional

METHOD _SetOSError(nCode,cStr,nOsError) CLASS zReadTxt
::nFerror    := nCode
::cFerrorStr := cStr
::nOsError   := nOsError
Return

// Metodo conceitualmente privado
// Limpa o registro da ultima ocorrencia de erro 

METHOD _CleanLastErr() CLASS zReadTxt
::nFerror    := 0
::cFerrorStr := ''
::nOsError   := 0 
Return


