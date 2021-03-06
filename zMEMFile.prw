/* -------------------------------------------------------------------------------------------

Copyright 2015-2019 J�lio Wittwer ( siga0984@gmail.com | http://siga0984.wordpress.com/ )

� permitido, gratuitamente, a qualquer pessoa que obtenha uma c�pia deste software 
e dos arquivos de documenta��o associados (o "Software"), para negociar o Software 
sem restri��es, incluindo, sem limita��o, os direitos de uso, c�pia, modifica��o, fus�o,
publica��o, distribui��o, sublicenciamento e/ou venda de c�pias do Software, 
SEM RESTRI��ES OU LIMITA��ES. 

O SOFTWARE � FORNECIDO "TAL COMO EST�", SEM GARANTIA DE QUALQUER TIPO, EXPRESSA OU IMPL�CITA,
INCLUINDO MAS N�O SE LIMITANDO A GARANTIAS DE COMERCIALIZA��O, ADEQUA��O A UMA FINALIDADE
ESPEC�FICA E N�O INFRAC��O. EM NENHUM CASO OS AUTORES OU TITULARES DE DIREITOS AUTORAIS
SER�O RESPONS�VEIS POR QUALQUER REIVINDICA��O, DANOS OU OUTRA RESPONSABILIDADE, SEJA 
EM A��O DE CONTRATO OU QUALQUER OUTRA FORMA, PROVENIENTE, FORA OU RELACIONADO AO SOFTWARE. 

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



#include 'Protheus.ch'

STATIC _aMemFiles := {}

/* ===========================================================================

Classe		ZMEMFILE
Autor		J�lio Wittwer
Data		01/2019

Descri��o   Classe de persistencia de arquivos em mem�ria 

Versao		1.0 
			-- Arquivos em mem�ria n�o s�o compartilhados entre Threads
			
Observa��o  A classe usa a implementa��o de indices em memoria 

=========================================================================== */

CLASS ZMEMFILE FROM ZISAMFILE

  DATA cMemFile			    // Nome / Identificador do arquivo de dados na mem�ria 

  DATA aFileData			// Array com os registros do arquivo 

  DATA dLastUpd				// Data registrada dentro do arquivo como ultimo UPDATE 
  DATA nRecLength			// Tamanho de cada registro 

  DATA lExclusive           // Arquivo aberto em modo exclusivo ?
  DATA lUpdPend             // Flag indicando update pendente 
  DATA lSetDeleted          // Filtro de registros deletados ativo 
  DATA oLogger              // Objeto de log 

  // ========================= Metodos de uso p�blico da classe

  METHOD NEW()    			// Construtor 
  METHOD OPEN()				// Abertura da tabela 
  METHOD CLOSE()			// Fecha a tabela 
  METHOD EXISTS()           // Verifica se a tabela existe 
  METHOD CREATE()           // Cria a tabela na mem�ria
  METHOD DROP()             // Apaga os dados e estrutura da tabela da memoria

  METHOD GetFileType()      // Tipo do arquivo ("MEMORY")

  METHOD FieldGet( nPos )   // Recupera o conteudo da coluna informada do registro atual 
  METHOD FieldPut( nPos )   // Faz update em uma coluna do registro atual 
  METHOD FileName()         // Retorna nome do arquivo aberto 
  METHOD Recno()			// Retorna o numero do registro (RECNO) posicionado 
  METHOD Deleted()			// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para dele��o ) 
  METHOD SetDeleted()       // Liga ou desliga filtro de registros deletados
  
  METHOD Insert()           // Insere um registro em branco no final da tabela
  METHOD Update()           // Atualiza o registro atual na tabela 

  METHOD Header() 			// Retorna tamanho em Bytes do Header da Tabela
  METHOD FileSize()         // Retorna o tamanho ocupado pelo arquivo em bytes 
  METHOD RecSize()			// Retorna o tamanho de um registro da tabela 
  METHOD LUpdate()			// Retorna a data interna do arquivo que registra o ultimo update 
 
  // ========================= Metodos de uso interno da classe

  METHOD _InitVars() 		// Inicializa propriedades do Objeto, no construtor e no CLOSE
  METHOD _ReadStruct()		// L� a estrutura do arquivo de dados 
  METHOD _ReadRecord()		// Le um registro do arquivo de dados
  METHOD _ClearRecord()		// Limpa o registro da memoria (EOF por exemplo) 

ENDCLASS

// ----------------------------------------------------------
// Retorna o tipo do arquivo 

METHOD GetFileType() CLASS ZMEMFILE 
Return "MEMORY"

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades
// Inicializa o ZISAMFILE passando a instancia atual 

METHOD NEW(cFile,oFileDef) CLASS ZMEMFILE 
_Super:New(self)

::oLogger := ZLOGGER():New("ZMEMFILE")
::oLogger:Write("NEW","File: "+cFile)

::_InitVars() 
::cMemFile   := lower(cFile)

If oFileDef != NIL 
	// Passa a defini��o pro IsamFile 
	::SetFileDef(oFileDef)
Endif

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZMEMFILE 

::_ResetError()

If ::lOpened
	::_SetError(-1,"File Already Open")
	Return .F.
Endif

IF !::Exists()
	::_SetError(-6,"Unable to OPEN - MEM File ["+::cMemFile+"] DOES NOT EXIST")
	Return .F.
Endif

If lExclusive = NIL ; 	lExclusive := .F. ; Endif
If lCanWrite = NIL ; 	lCanWrite := .F.  ; Endif

// Por enquanto faz escrita apenas em modo exclusivo
If lCanWrite .AND. !lExclusive
	::_SetError(-6,"Unable to OPEN for WRITE in SHARED MODE -- Use Exclusive mode or OPEN FOR READ")
	Return .F.
Endif

// Atualiza propriedades de controle da classe
::lExclusive   := lExclusive
::lCanWrite    := lCanWrite

If !::_ReadStruct()
	// Em caso de falha na leitura da estrutura 
	Return .F.
Endif

// Cria o array de campos do registro atual 
// Aloca uma coluna a mais para o Flag de deletado 
::aGetRecord := Array(::nFldCount+1)
::aPutRecord := Array(::nFldCount+1)

// Seta que o arquivo est� aberto 
::lOpened := .T. 

// Vai para o topo do arquivo 
// e L� o primeiro registro f�sico 
::GoTop()

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZMEMFILE 
Local nPos

// Localiza a estrutura da tabela 
// e some com ela 
nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })
If nPos > 0 
	_aMemFiles[nPos] := NIL
	aDel(_aMemFiles,nPos)
	aSize(_aMemFiles,len(_aMemFiles)-1)
Endif

// TODO - Revisar este comportamento
// Ao fechar, evapora com os dados da tabela 
aSize( ::aFileData , 0 ) 

// Fecha tamb�m, todos os indices 
::ClearIndex()

// Limpa as propriedades
::_InitVars()


Return 


// ----------------------------------------------------------\
// Verifica se a tabela existe na memoria
METHOD EXISTS() CLASS ZMEMFILE 
Local nPos
nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })
Return nPos > 0 


// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome j� foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 

METHOD CREATE( aStru ) CLASS ZMEMFILE
Local nFields := 0
Local nI

If ::EXISTS()
	::_SetError(-7,"CREATE ERROR - File Already Exists")
Endif

If ::lOpened
	::_SetError(-8,"CREATE ERROR - File Already Opened")
Endif

If aStru = NIL .AND. ::oFileDef != NIL 
	// Se a erstrutura nao foi informada 
	// Mas a tabela tem a defini��o , 
	// pega a estrutura da definicao 
	aStru := ::oFileDef:GetStruct()
Endif

// Valida a estrutura informada
nFields := len(aStru)
For nI := 1 to nFields
	If !aStru[nI][2]$"CNDLM"
		UserException("CREATE ERROR - INVALID FIELD TYPE "+aStru[nI][2]+ " ("+aStru[nI][1]+")" )
	Endif
	// Apenas Ajusta nome do campo 
	aStru[nI][1] := Upper(padr(aStru[nI][1],10))
Next

// Guarda a estrutura do arquivo em array est�tico 
// Visivel apenas na Thread / Processo atual 
aadd( _aMemFiles , { ::cMemFile , aClone(aStru) }  )

Return .T. 

// ----------------------------------------------------------
// Permite ligar filtro de navega��o de registros deletados
// Defaul = desligado

METHOD SetDeleted( lSet ) CLASS ZMEMFILE 
Local lOldSet := ::lSetDeleted
If pCount() > 0 
	::lSetDeleted := lSet
Endif
Return lOldSet


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZMEMFILE 

// Inicializa��o das propriedades da classe pai
_Super:_InitVars()

// Inicializa demais propriedades da ZMEMFILE
::aFileData   := {}
::lOpened     := .F. 
::lExclusive  := .F. 
::lCanWrite   := .F. 
::dLastUpd    := ctod("")
::aPutRecord  := {}
::lUpdPend    := .F. 
::lSetDeleted := .F. 
::nRecno      := 0

Return

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZMEMFILE 
Return ::dLastUpd

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// L� a estrutura de campos da tabela 

METHOD _ReadStruct() CLASS ZMEMFILE 
Local nPos

nPos := ascan(_aMemFiles,{|x| x[1] == ::cMemFile })

If nPos > 0 
	::aStruct := aClone(_aMemFiles[nPos][2])
	::nFldCount := len(::aStruct)
	Return .T. 
Endif

Return .F. 

// ----------------------------------------------------------
// Recupera o conte�do de um campo da tabela 
// a partir da posi�ao do campo na estrutura

METHOD FieldGet(nPos) CLASS ZMEMFILE 

If valtype(nPos) = 'C'
	nPos := ::FieldPos(nPos)
Endif

If nPos > 0 .and. nPos <= ::nFldCount 
	Return ::aGetRecord[nPos]
Endif
Return NIL


// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 

METHOD FieldPut(nPos,xValue) CLASS ZMEMFILE 

If valtype(nPos) = 'C'
	nPos := ::FieldPos(nPos)
Endif

If ( !::lCanWrite )
	UserException("Invalid FieldPut() -- File NOT OPEN for WRITING")
Endif

If ( ::lEOF )
	UserException("Invalid FieldPut() -- File is in EOF")
Endif

If nPos > 0 .and. nPos <= ::nFldCount 
	If ::aStruct[nPos][2] == 'C'
		// Ajusta tamanho de string com espa�os a direita
		xValue := PadR(xValue,::aStruct[nPos][3])
	Endif
	::aPutRecord[nPos] := xValue
	::lUpdPend := .T. 
Endif

Return NIL

// ----------------------------------------------------------
// Recupera o nome do arquivo no disco 
METHOD FileName() CLASS ZMEMFILE 
Return ::cMemFile

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado 
METHOD DELETED() CLASS ZMEMFILE 
If !::lEOF
	Return ::aGetRecord[::nFldCount+1]
Endif
Return .F. 

// ----------------------------------------
// Retorna o tamanho do HEader
// Baseado na estrutura DBF, o tamanho seria 32 * o tamanho da estrutura 
// Mais 32 bytes do Header, mais 2 do final da estrutura 

METHOD HEADER() CLASS ZMEMFILE 
Return ( len(::aStruct) * 32 ) + 32 + 2 

// ----------------------------------------
// Retorna o tamanho aproximado do arquivo na memoria 
// -- Desconsidera campos MEMO 

METHOD FileSize() CLASS ZMEMFILE 
Local nFileSize := 0 
nFileSize := ( ::nLastRec * ::nRecLength )  
Return nFileSize

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RECSIZE() CLASS ZMEMFILE 
Return ::nRecLength

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZMEMFILE 
If ::lEOF
	Return ::nLastRec+1
Endif
Return ::nRecno 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// L� o registro posicionado no offset de dados atual 

METHOD _ReadRecord() CLASS ZMEMFILE 

// Copia os dados do array para o registro atual 
aCopy( ::aFileData[::nRecno], ::aGetRecord )

// Reseta flags de BOF e EOF 
::lBOF := .F. 
::lEOF := .F. 

Return .T. 


// ----------------------------------------
// Insere um registro em branco no final da tabela
// Apos a inser��o, voce pode fazer fieldput 
// e confirmar tudo com UPDATE 
METHOD Insert() CLASS ZMEMFILE

// Limpa o conteudo do registro em memoria 
::_ClearRecord()

// Insere o registro em branco 
aadd( ::aFileData , aClone(::aGetRecord) )

// Nao estou em BOF ou EOF, 
// Estou em modo de inser��o de registro
::lBOF := .F. 
::lEOF := .F. 

// Atualiza contador de registros 
::nLastRec := len( ::aFileData )

// Recno atual = registro novo 
::nRecno := ::nLastRec

Return .T. 

// ----------------------------------------
// Grava as altera��es do registro atual na tabela 

METHOD Update() CLASS ZMEMFILE
Local nI

If ( ::lEOF )
	UserException("ZMEMFILE::Update() ERROR -- File is in EOF")
	Return
Endif

If !::lUpdPend
	// Nao tem update pendente, nao faz nada
	Return
Endif

For nI := 1 to ::nFldCount
	// Atualiza apenas os campos que receberam conteudo 
	// Atualiza tambel o registro atual na memoria
	// E limpa o elemento do array de update pendente 
	If ::aPutRecord[nI] != NIL 
		::aFileData[::nRecno][nI] := ::aPutRecord[nI]
		::aGetRecord[nI] := ::aPutRecord[nI]
		::aPutRecord[nI] := NIL 
	Endif
Next

// Agora que o registro est� atualizado, atualiza os indices 
aEval(::aIndexes , {|oIndex| oIndex:UpdateKey() })

// Desliga flag de update pendente 
::lUpdPend := .F. 

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZMEMFILE

// Inicializa com o valor default os campos da estrutura 
_Super:_ClearRecord()

// Limpa Flag de deletado 
::aGetRecord[::nFldCount+1] := .F. 

Return

