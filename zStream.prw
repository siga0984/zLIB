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



#include  'protheus.ch'
#include  'zLib.ch'

/* ============================================================================

CLASSE         ZSTREAM
Autor          Júlio Wittwer
Data           01/2019
Descrição      O Objeto Stream permite gravar sequencias de dados e de objetos 
               em uma string binária, que pode ser armazenada e carregada 

============================================================================ */

CLASS ZSTREAM FROM LONGNAMECLASS

   DATA aStream          // Array com os elementos do Stream
   DATA nPos             // Elemento atual do Stream 
   DATA nSize            // Tamanho atual do Stream ( elementos ) 
   
   METHOD NEW()          // Concstrutor 
   METHOD LOADSTR()      // Carrega um stream da binary String gerada pelo ::SaveStr()
   METHOD SAVESTR()      // Salva o Stream em uma Binary String 
   METHOD CLEAR()        // Limpa o Stream 
   METHOD READ()         // Lê a informação da posição atual do Stream e posiciona na próxima
   METHOD WRITE()        // Acrescenta uma variável no Stream
   METHOD EOF()          // Verifica se o stream está no final 

ENDCLASS

// ----------------------------------------
// Construtor, apenas inicializa variaáveis 
METHOD NEW() CLASS ZSTREAM 
::aStream := {}
::nPos := 1
::nSize := 0 
Return SELF

// ----------------------------------------
// Recebe cBuffer com o Binary Stream gerado pelo Save()
// Lê o Buffer e popula o Stream.
// Se o tream é valido -- foi gerado pela SaveStr, carrega 
METHOD LOADSTR(cBuffer) CLASS ZSTREAM 
If left(cBuffer,11) == "#_ZSTREAM_#"
	cBuffer := Substr(cBuffer,12)
	BinStr2Var( cBuffer, ::aStream ) 
	::nPos := 1
	::nSize := len(::aStream)
	Return .T. 
Endif
Return .F. 

// ----------------------------------------
// Recebe cBuffer por referencia 
// Salva o stream nele como Binary String
// Coloca um prefixo na BinaryString gerada
METHOD SAVESTR(cBuffer) CLASS ZSTREAM 
Var2BinStr( ::aStream , cBuffer )
cBuffer := Stuff(cBuffer , 1 , 0 , "#_ZSTREAM_#")
Return .T. 

// ----------------------------------------
// Limpa e reinicializa o Stream 

METHOD CLEAR() CLASS ZSTREAM 
::aStream := {}
::nPos := 1
::nSize := 0
Return

// ----------------------------------------
// Leitura por referência do stream , na ordem de gravação 

METHOD READ(xValue) CLASS ZSTREAM 

// Recupero o elemento atual 
BinStr2Var( ::aStream[nPos][2] , xValue  )

// Posiciona na próxima posição para leitura 
::nPos++

Return 

// ----------------------------------------
// Escreve um valor no Stream. 
// Pode até ser um objeto 

METHOD WRITE(xValue) CLASS ZSTREAM 
Local cBuffer := ''
Local cType := Valtype(xValue)
If cType $ 'CNDLMAU'
	// Acrescenta o valor no Array para Stream 
	// Coloca o tipo junto 
	Var2BinStr(xValue,cBuffer)
	aadd(::aStream,{cType,cBuffer})
	::nSize := len(::aStream)
	::nPos := ::nSize + 1 
	cBuffer := ''
Else
	UserException("ZSTREAM:Write() -- Unsupported Type "+cType)
Endif
Return 

// ----------------------------------------
// Verifica se o stream acabou ou está no final 

METHOD EOF() CLASS ZSTREAM 
Return ::nPos > ::nSize



