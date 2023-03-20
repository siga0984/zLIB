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


#include 'fileio.ch'
#include "zLibDec2Hex.ch"
#include "CXStruct.ch"
#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'
#INCLUDE 'CXInclude.ch'

/* ===========================================================================

Classe		ZDBFFILE
Autor		Júlio Wittwer
Data		04/01/2019

Descrição   Classe de acesso a arquivos DBF - SOMENTE LEITURA 
            Suporte DBF DBASE III / Clipper / ADS 
			Suporte a leitura de campos MEMO dos formatos DBT e FPT

Observações Embora todas as propriedades sejam públicas, o uso da classe 
            deve ser feito totalmente pelos MÉTODOS implementados
            Os métodos de uso interno e/ou restritos da classe são 
            iniciados com "_"

			O objeto gerado exige apenas o nome do arquivo no construtor. 
			O arquivo é aberto em modo de leitura compartilhado. 
			Pode ser usado inclusive para ler arquivos no SmartClient 
			( Porém o desempenho pode ser prejudicado devido ao tráfego 
			de rede entre APPServer e SmartClient a cada leitura de registro


Release 20190105
- Implementação de filtro -- DbSetFilter e DBCleanFilter()

Release 20190106 
- Implementação de indice em memória 
- Implementação de filtro de registros deletados

Debitos Tecnicos

1) Melhorar a inserção. Atualmente é inserido um registro em branco 
direto no final da tabela, e então ele é alterado. Verificar forma 
de postergar a inserção, para escrever os novos dados de uma vez 

Referências do Formato de Arquivos DBF / DBT / FPT 

http://web.tiscali.it/SilvioPitti/
https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm
http://dbfviewer.com/dbf-file-structure/
https://www.loc.gov/preservation/digital/formats/fdd/fdd000325.shtml
https://en.wikipedia.org/wiki/.dbf
http://www.dbfree.org/webdocs/1-documentation/b-dbf_header_specifications.htm
http://www.independent-software.com/dbase-dbf-dbt-file-format.html
http://www.idea2ic.com/File_Formats/DBF%20FILE%20STRUCTURE.pdf
http://www.oocities.org/geoff_wass/dBASE/GaryWhite/dBASE/FAQ/qformt.htm
http://www.dbfree.org/webdocs/1-documentation/a-about_indexes.htm


=========================================================================== */

// Pseudo-comando para trabalhar com OFFSET de Base 0 em string AdvPL 
// Recebe STring, Offset Base 0 e Tamanho do bloco 
#xtranslate DBF_OFFSET(<cBuffer>,<nOffset>,<nSize>) => Substr(<cBuffer>,<nOffset>+1,<nSize>)

CLASS ZDBFFILE FROM ZISAMFILE

	PROTECTED DATA cDataFile			AS Character	// Nome do arquivo de dados
	PROTECTED DATA cMemoFile			AS Character	// Nome do arquivo memo (DBT/FPT) 
	PROTECTED DATA lOpened              AS Logical		// Indica se o arquivo está aberto 

	PROTECTED DATA cDBFType				AS Character	// Identificador hexadecimal do tipo do DBF 
	PROTECTED DATA dLastUpd				AS Date			// Data registrada dentro do arquivo como ultimo UPDATE 
	PROTECTED DATA nRecLength			AS Numeric		// Tamanho de cada registro 
	PROTECTED DATA nDataPos 			AS Numeric		// Offset de inicio dos dados 
	PROTECTED DATA lHasMemo				AS Logical		// Tabela possui campo MEMO ?
	PROTECTED DATA nMemoType            AS Numeric		// Tipo de campo MEMO da RDD ( 1 = DBT, 2 = FPT ) 
	PROTECTED DATA cMemoExt             AS Character	// Identificador (extensao) do tipo do campo MEMO
	PROTECTED DATA aGetRecord			AS Array		// Array com todas as colunas do registro atual 
	PROTECTED DATA aPutRecord           AS Array		// Array com campos para update 
	PROTECTED DATA lExclusive           AS Logical		// Arquivo aberto em modo exclusivo ?
	PROTECTED DATA lCanWrite            AS Logical		// Arquivo aberto para gravacao 
	PROTECTED DATA lUpdPend             AS Logical		// Flag indicando update pendente 
	PROTECTED DATA lNewRecord           AS Logical		// Flag indicando a inserção de um registro
	PROTECTED DATA lDeleted				AS Logical		// Indicador de registro corrente deletado (marcado para deleção ) 
	PROTECTED DATA lSetDeleted          AS Logical		// Filtro de registros deletados ativo 
	PROTECTED DATA nRecno				AS Numeric		// Número do registro (RECNO) atualmnete posicionado 

	PROTECTED DATA nHData				AS Numeric		// Handler do arquivo de dados
	PROTECTED DATA oMemoFile			AS Object		// Objeto para lidar com campo Memo 
	PROTECTED DATA oLogger              AS Object		// Objeto de log

  // ========================= Metodos de uso público da classe

	PUBLIC METHOD New()    				CONSTRUCTOR		// Construtor 
	PUBLIC METHOD Destroy()								// Destrutor da classe
	PUBLIC METHOD Open()				AS Logical		// Abertura da tabela 
	PUBLIC METHOD Close()								// Fecha a tabela 
	PUBLIC METHOD Exists()           	AS Logical		// Verifica se a tabela existe 
	PUBLIC METHOD Create()           	AS Logical		// Cria a tabela no disco 
	PUBLIC METHOD Drop()             	AS Logical		// Apaga a tabela do disco 

	PUBLIC METHOD GetFileType()      	AS Character	// Tipo do arquivo ("DBF")
	PUBLIC METHOD GetDBType()			AS Character	// REtorna identificador hexadecimal do tipo da tabela 
	PUBLIC METHOD GetDBTypeStr() 		AS Character	// Retorna string identificando o tipo da tabela 
	PUBLIC METHOD GetMemoType()      	AS Numeric		// Tipo do MEMO usado, 1 = DBT , 2 = FPT

	PUBLIC METHOD FieldGet()							// Recupera o conteudo da coluna informada do registro atual 
	PUBLIC METHOD FieldPut()							// Faz update em uma coluna do registro atual 
	PUBLIC METHOD FieldBulk()							// Faz a gravação dos campos passados (observar a mesma estrutura)
	PUBLIC METHOD FileName()			AS Character	// Retorna nome do arquivo aberto 
	PUBLIC METHOD Recno()				AS Numeric		// Retorna o numero do registro (RECNO) posicionado 
	PUBLIC METHOD Deleted()				AS Logical		// REtorna .T. caso o registro atual esteja DELETADO ( Marcado para deleção ) 
	PUBLIC METHOD dbDelete()			AS Logical		// Marca registro para deleção
	PUBLIC METHOD SetDeleted()       	AS Logical		// Liga ou desliga filtro de registros deletados

	PUBLIC METHOD Insert()				AS Logical		// Insere um registro em branco no final da tabela
	PUBLIC METHOD Update()				AS Logical		// Atualiza o registro atual na tabela 

	PUBLIC METHOD Header()				AS Numeric		// Retorna tamanho em Bytes do Header da Tabela
	PUBLIC METHOD FileSize()			AS Numeric		// Retorna o tamanho ocupado pelo arquivo em bytes 
	PUBLIC METHOD RecSize()				AS Numeric		// Retorna o tamanho de um registro da tabela 
	PUBLIC METHOD LUpdate()				AS Date			// Retorna a data interna do arquivo que registra o ultimo update 
 
  // ========================= Metodos de uso interno da classe

  PROTECTED METHOD _InitVars()							// Inicializa propriedades do Objeto, no construtor e no CLOSE
  PROTECTED METHOD _ReadHeader()		AS Logical		// Lê o Header do arquivo  de dados
  PROTECTED METHOD _ReadStruct()		AS Logical		// Lê a estrutura do arquivo de dados 
  PROTECTED METHOD _SetLUpdate()						// Atualiza data do Last Update no Header do Arquivo 
  PROTECTED METHOD _ReadRecord()		AS Logical		// Le um registro do arquivo de dados
  PROTECTED METHOD _ClearRecord()						// Limpa o registro da memoria (EOF por exemplo) 
  PROTECTED METHOD _ReadMemo()			AS Character	// Recupera um conteudo de campo memo por OFFSET
  PROTECTED METHOD _WriteTrailer()						// Grava informações do trailer do arquivo (final de arquivo)
  PROTECTED METHOD _WriteLastRec()						// Grava quantidade de registros no Header

ENDCLASS

// ----------------------------------------------------------
// Retorna o tipo do arquivo 

METHOD GetFileType() CLASS ZDBFFILE 
Return "DBF"

// ----------------------------------------------------------
// Construtor do objeto DBF 
// Apenas recebe o nome do arquivo e inicializa as propriedades
// Inicializa o ZISAMFILE passando a instancia atual 

METHOD NEW(cFile,oFileDef) CLASS ZDBFFILE 

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR cFile		AS Character
	ParamType 1		VAR oFileDef	AS Object				Optional Default NIL

	//---------------------------------------------------------------------------------------------
	_Super:New(self)

	::oLogger := zLOGGER():New("ZDBFFILE")
	::oLogger:Write("NEW","File: "+cFile)

	::_InitVars()
	::cDataFile   := lower(cFile)

	If oFileDef != NIL 
		// Passa a definição pro IsamFile 
		::SetFileDef(oFileDef)
	Endif

Return self


// ----------------------------------------------------------
// Abertura da tabela -- READ ONLE 
// Caso retorne .F. , consulte o ultimo erro usando GetErrorStr() / GetErrorCode()
// Por hora apenas a abertura possui tratamento de erro 

METHOD OPEN(lExclusive,lCanWrite) CLASS ZDBFFILE 

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nFMode := 0		AS Numeric

	//---------------------------------------------------------------------------------------------
	::_ResetError()

	If ::lOpened
		::_SetError(-1,"File Already Open")
		Return .F.
	Endif

	IF !::Exists()
		::_SetError(-6,"Unable to OPEN - File ["+::cDataFile+"] DOES NOT EXIST")
		Return .F.
	Endif

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lExclusive		AS Logical				Optional Default .F.
	ParamType 1		VAR lCanWrite		AS Logical				Optional Default .F.

	If lExclusive
		nFMode += FO_EXCLUSIVE
	Else
		nFMode += FO_SHARED
	Endif

	If lCanWrite
		nFMode += FO_READWRITE
	Else
		nFMode += FO_READ
	Endif

	// Por enquanto faz escrita apenas em modo exclusivo
	If lCanWrite .AND. !lExclusive
		::_SetError(-6,"Unable to OPEN for WRITE in SHARED MODE -- Use Exclusive mode or OPEN FOR READ")
		Return .F.
	Endif

	// Atualiza propriedades de controle da classe
	::lExclusive   := lExclusive
	::lCanWrite    := lCanWrite

	// Abre o arquivo de dados
	::nHData := Fopen(::cDataFile,nFMode)

	If ::nHData == -1
		::_SetError(-2,"Open Error - File ["+::cDataFile+"] Mode ["+cValToChar(nFMode)+"] - FERROR "+cValToChar(Ferror()))
		Return .F.
	Endif

	// Lê o Header do arquivo 
	If !::_ReadHEader()
		FClose(::nHData)
		::nHData := -1
		Return .F. 
	Endif

	If ::lHasMemo

		// Se o header informa que a tabela possui campo MEMO 
		// Determina o nome do arquivo MEMO 

		::cMemoFile := substr(::cDataFile,1,rat(".",::cDataFile)-1)
		::cMemoFile += ::cMemoExt
		
		If !file(::cMemoFile)
			::_SetError(-3,"Memo file ["+::cMemoFile+"] NOT FOUND.")
			::Close()
			Return .F. 
		Endif

		If ::nMemoType == 1
			::oMemoFile  := ZDBTFILE():New(self,::cMemoFile)
		ElseIF ::nMemoType == 2
			::oMemoFile  := ZFPTFILE():New(self,::cMemoFile)
		Endif

		If !::oMemoFile:Open(::lExclusive,::lCanWrite)
			::_SetError(-4,"Open Error - File ["+::cMemoFile+"] - FERROR "+cValToChar(Ferror()))
			::Close()
			Return .F. 
		Endif
		
	Endif

	If !::_ReadStruct()

		// Em caso de falha na leitura da estrutura 

		FClose(::nHData)
		::nHData := -1
		
		IF ::oMemoFile != NIL 
			::oMemoFile:Close()
			FreeObj(::oMemoFile)
		Endif

		Return .F.
		
	Endif

	// Cria o array de campos do registro atual 
	::aGetRecord := Array(::nFldCount)
	::aPutRecord := Array(::nFldCount)

	// Seta que o arquivo está aberto 
	::lOpened := .T. 

	// Vai para o topo do arquivo 
	// e Lê o primeiro registro físico 
	::GoTop()

Return .T. 


// ----------------------------------------------------------
// Fecha a tabela aberta 
// Limpa as variaveis de controle. 
// A tabela pode ser aberta novamente pela mesma instancia 

METHOD CLOSE() CLASS ZDBFFILE 

	// Fecha o arquivo aberto 
	If ::nHData <> -1
		fClose(::nHData)
	Endif

	// Se tem memo, fecha 
	IF ::oMemoFile != NIL 
		::oMemoFile:Close()
		FreeObj(::oMemoFile)
	Endif

	// Fecha todos os indices abertos 
	::ClearIndex()

	// Limpa as propriedades
	::_InitVars()

Return 


// ----------------------------------------------------------\
// Verifica se a tabela existe no disco 
METHOD EXISTS() CLASS ZDBFFILE 

	IF File(::cDataFile)
		Return .T. 
	Endif

Return .F. 

// ----------------------------------------------------------\
// Cria a tabela no disco 
// O nome já foi recebido no construtor 
// Recebe a estrutura e a partir dela cria a tabela 
// Se o objeto já está atrelado a uma definição, usa a estrutura da definição 

METHOD CREATE( aStru ) CLASS ZDBFFILE 

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cNewHeader := ''		AS Character
	Local cMemoFile				AS Character
	Local cFldName				AS Character
	Local lHasMemo	:= .F.		AS Logical
	Local lOk					AS Logical
	Local nFields	:= 0		AS Numeric
	Local nRecSize	:= 1		AS Numeric
	Local nI, nH				AS Numeric
	Local oMemoFile				AS Object

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aStru		AS Array		Optional Default NIL
	
	If ::EXISTS()
		::_SetError(-7,"CREATE ERROR - File Already Exists")
		Return .F.
	Endif

	If ::lOpened
		::_SetError(-8,"CREATE ERROR - File Already Opened")
		Return .F.
	Endif

	If aStru = NIL .AND. ::oFileDef != NIL 
		// Se a erstrutura nao foi informada 
		// Mas a tabela tem a definição , 
		// pega a estrutura da definicao 
		aStru := ::oFileDef:GetStruct()
	Endif

	//Reavalio a variável aStru
	ParamType 0		VAR aStru		AS Array

	nFields := len(aStru)

	For nI := 1 to nFields
		If aStru[nI][nST_TIPO] == 'M'
			lHasMemo := .T. 
		Endif
		If !aStru[nI][nST_TIPO]$"CNDLM"
			UserException("CREATE ERROR - INVALID FIELD TYPE "+aStru[nI][nST_TIPO]+ " ("+aStru[nI][nST_CAMPO]+")" )
			Return .F.
		Endif
		// Ajusta nome do campo 
		aStru[nI][nST_CAMPO] := Upper(padr(aStru[nI][nST_CAMPO],10))
		nRecSize += aStru[nI][nST_TAMANHO]
	Next

	// Inicio do Header
	// 1o Byte - Formato do aRquivo 
	// Campo memo será criado como FPT 
	If lHasMemo
		::nMemoType := 2
		cNewHeader += Chr(245) // FoxPro 2.x (or earlier) with memo ( FPT ) 
	Else
		cNewHeader += Chr(003) // FoxBASE+/Dbase III plus, no memo
	Endif

	// 3 Byte(s) = Last Update Date = TODAY [01-03]
	cNewHeader +=  	Chr( Year(date())-2000 ) + ;
					Chr( Month(date()) ) + ;
					Chr( Day(date()) ) 

	// 4 byte(S) - Last Record [04-07]
	cNewHeader +=  L2Bin(0) 

	// 2 byte(s) -- Begin Data Offset [08-09]
	cNewHeader +=  I2Bin( ( (nFields+1) * 32) + 2 ) 

	// 2 byte(s) -- Record Size [10-11]
	cNewHeader +=  I2Bin(nRecSize) 

	// 2 byte(s) -- Filler [12-13]
	cNewHeader +=  replicate( chr(0) , 2 )

	// 1 byte(s) -- Flag indicating incomplete transaction [14]
	cNewHeader +=  replicate( chr(0) , 1 )

	// 1 byte(s) -- Encryption flag [15]
	cNewHeader +=  replicate( chr(0) , 1 )

	// 12 byte(s) -- Reserved for dBASE for DOS in a multi-user environment [16-27]
	cNewHeader +=  replicate( chr(0) , 12 )

	// 1 byte(s) -- Production .mdx file flag; 1 if there is a production .mdx file, 0 if not [28]
	cNewHeader +=  replicate( chr(0) , 1 )

	// 1 byte(s) -- Language driver ID [29]
	cNewHeader +=  Chr(03)	//OEM 850

	// Filler ( 32 Bytes  ) [30-31]
	cNewHeader +=  replicate( chr(0) , 2 )

	// Acrescenta no Header a estrutura
	For nI := 1 to nFields

		cFldName := alltrim(aStru[nI][nST_CAMPO])
		while len(cFldName) < 10
			cFldName += chr(0)
		Enddo

		cNewHeader +=  cFldName + chr(0) // Nome
		cNewHeader +=  aStru[nI][nST_TIPO]  // Tipo 
		cNewHeader +=  replicate( chr(0) , 4 ) // Filler - Reserved
		If aStru[nI][nST_TIPO] == 'C'
			//Tratamento para campos texto grandes!
			cNewHeader +=  I2Bin(aStru[nI][nST_TAMANHO]) // Tamanho
		Else
			cNewHeader +=  chr(aStru[nI][nST_TAMANHO]) // Size
			cNewHeader +=  chr(aStru[nI][nST_DECIMAL]) // Decimal
		EndIf
		cNewHeader +=  replicate( chr(0) , 14 ) // Filler - Reserved
	Next

	// Final do Header apos estrutura 

	cNewHeader +=  chr(13)  // 0x0D = Fim da estrutura 
	cNewHeader +=  chr(0)   // 0c00 = Filler
	cNewHeader +=  chr(26)  // 0x1A = End Of File

	// Cria a tabela no disco 
	nH := fCreate(::cDataFile)

	If nH == -1
		::_SetError(-9,"CREATE ERROR - Data File ["+::cDataFile+"] - FERROR ("+cValToChar(Ferror())+")")
		Return .F. 
	Endif

	fWrite(nH,cNewHeader)
	fCLose(nH)

	If lHasMemo
		cMemoFile := substr(::cDataFile,1,rat(".",::cDataFile)-1)
		cMemoFile += '.fpt'
		oMemoFile := ZFPTFILE():New(self,cMemoFile)
		lOk := oMemoFile:Create()
		FreeObj(oMemoFile)
		If !lOk
			::_SetError(-9,"CREATE ERROR - Data File ["+::cDataFile+"] - FERROR ("+cValToChar(Ferror())+")")
			Return .F. 
		Endif
	Endif

Return .T. 


// ----------------------------------------------------------\
// Apaga a tabela do disco 

METHOD DROP() CLASS ZDBFFILE 

	nErr := 0

	If ::lOpened
		::_SetError(-8,"DROP ERROR - File Already Opened")
		Return .F.
	Endif

	If !empty(cDataFile)
		Ferase(cDataFile)
	Endif

	If !empty(cMemoFile)
		Ferase(cMemoFile)
	Endif

Return .T. 

// ----------------------------------------------------------
// Permite ligar filtro de navegação de registros deletados
// Defaul = desligado

METHOD SetDeleted( lSet ) CLASS ZDBFFILE 
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local lOldSet := ::lSetDeleted		AS Logical

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR lSet		AS Logical
	
	If pCount() > 0 
		::lSetDeleted := lSet
	Endif

Return lOldSet


// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Inicializa / Limpa as propriedades padrao do Objeto 

METHOD _InitVars() CLASS ZDBFFILE 

	// Inicialização das propriedades da classe pai
	_Super:_InitVars()

	::nHData      := -1
	::lOpened     := .F. 
	::nDataPos    := 0 
	::lHasMemo    := .F. 
	::lExclusive  := .F. 
	::lCanWrite   := .T. 
	::dLastUpd    := ctod("")
	::aPutRecord  := {}
	::lUpdPend    := .F. 
	::lNewRecord  := .F.
	::lDeleted    := .F. 
	::lSetDeleted := .F. 
	::nRecno      := 0
	::cMemoExt    := ''
	::nMemoType   := 0 

Return

// ----------------------------------------------------------
// Retorna o identificador hexadecimal do tipo do DBF

METHOD GetDBType() CLASS ZDBFFILE 
Return ::cDBFType

// ----------------------------------------------------------
// Tipo do MEMO usado, 1 = DBT , 2 = FPT

METHOD GetMemoType()  CLASS ZDBFFILE 
Return ::nMemoType

// ======================================================================================================
// Array com os tipos de DBF reconhecidos 
// O 3o elemento quando .T. indoca se o formato é suportado 

STATIC _aDbTypes := { { '0x02','FoxBASE'                                              , .F. } , ;
                      { '0x03','FoxBASE+/Dbase III plus, no memo'                     , .T. } , ;  // ####  (No Memo)
                      { '0x04','dBASE IV or IV w/o memo file'                         , .F. } , ;
                      { '0x05','dBASE V w/o memo file'                                , .F. } , ;
                      { '0x30','Visual FoxPro'                                        , .F. } , ;
                      { '0x31','Visual FoxPro, autoincrement enabled'                 , .F. } , ;
                      { '0x32','Visual FoxPro, Varchar, Varbinary, or Blob-enabled'   , .F. } , ;
                      { '0x43','dBASE IV SQL table files, no memo'                    , .F. } , ;
                      { '0x63','dBASE IV SQL system files, no memo'                   , .F. } , ;
                      { '0x7B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x83','FoxBASE+/dBASE III PLUS, with memo'                   , .T. } , ;  // ####  DBT
                      { '0x8B','dBASE IV with memo'                                   , .F. } , ;
                      { '0x8E','dBASE IV w. SQL table'                                , .F. } , ;
                      { '0xCB','dBASE IV SQL table files, with memo'                  , .F. } , ;
                      { '0xF5','FoxPro 2.x (or earlier) with memo'                    , .T. } , ;  // ####  FPT
                      { '0xE5','HiPer-Six format with SMT memo file'                  , .F. } , ;
                      { '0xFB','FoxBASE'                                              , .F. } } 

// ======================================================================================================


// ----------------------------------------------------------
// Retorna a descrição do tipo de arquivo DBF 

METHOD GetDBTypeStr() CLASS ZDBFFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cRet := '(Unknow DBF Type)'							AS Character
	Local nPos := ascan(_aDbTypes,{|x| x[1] == ::cDBFType })	AS Numeric

	If nPos > 0
		cRet := _aDbTypes[nPos][2]
	Endif

Return cRet

// ----------------------------------------------------------
// Retorna a data do ultimo update feito no arquivo 

METHOD LUPDATE() CLASS ZDBFFILE 
Return ::dLastUpd

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Realiza a leitura do Header do arquivo DBF 

METHOD _ReadHeader() CLASS ZDBFFILE 

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer := space(32)		AS Character
	Local cTemp := ''				AS Character
	Local nYear, nMonth, nDay		AS Numeric
	Local nTemp := 0				AS Numeric

	If ::nHData == -1 
		UserException("_ReadHeader() ERROR - DBF File Not Opened")
		Return .F.
	Endif

	// Reposiciono o arquivo no Offset 0
	// Le os primeiros 32 bytes do Header
	FSeek(::nHData,0)
	FRead(::nHData,@cBuffer,32)

	// ----------------------------------------
	// Database File Type

	cTemp := DBF_OFFSET(cBuffer,0,1)       
	nTemp := ASC(cTemp)

	::cDBFType := '0x'+padl( upper(DEC2HEX(nTemp)) , 2 , '0')
									
	If ::cDBFType == '0x83'   
		// FoxBASE+/dBASE III PLUS, with memo
		::lHasMemo := .T. 
		::cMemoExt := ".dbt"
		::nMemoType := 1
	ElseIf ::cDBFType == '0xF5'
		// FoxPro 2.x (or earlier) with memo
		::lHasMemo := .T. 
		::cMemoExt := ".fpt"
		::nMemoType := 2
	Endif

	If Ascan(_aDbTypes,{|x| x[1] == ::cDBFType }) == 0 
		::_SetError(-5,"DBF FORMAT ("+::cDBFType+") NOT RECOGNIZED")
		Return .F. 
	Endif

	// ----------------------------------------
	// Last Update ( YMD => 3 Bytes, binary )

	cTemp := DBF_OFFSET(cBuffer,1,3) 

	nYear  := ASC( substr(cTemp,1,1))
	nMonth := ASC( substr(cTemp,2,1))
	nDay   := ASC( substr(cTemp,3,1))

	If nYear < 50 
		nYear += 2000
	Else
		nYear += 1900
	Endif

	::dLastUpd := ctod(strzero(nDay,2)+"/"+strzero(nMonth,2)+"/"+strzero(nYear,4))

	// ----------------------------------------
	// 4 bytes (32 bits), Record Count (  LastRec ) 

	cTemp := DBF_OFFSET(cBuffer,4,4) 
	::nLastRec := Bin2L(cTemp)

	// ----------------------------------------
	// First Data Record Position  ( Offset ) 

	cTemp := DBF_OFFSET(cBuffer,8,2) 
	::nDataPos := Bin2I(cTemp)

	// ----------------------------------------
	// Length of one data record, including delete flag

	cTemp := DBF_OFFSET(cBuffer,10,2) 
	::nRecLength := Bin2I(cTemp)

	// Limpeza de variáveis 
	cTemp := NIL
	cBuffer := NIL

Return .T. 


/*
FIELD DESCRIPTOR ARRAY TABLE
BYTES DESCRIPTION
0-10 Field Name ASCII padded with 0x00
11 Field Type Identifier (see table)
12-15 Displacement of field in record
16 Field length in bytes
17 Field decimal places
18-19 Reserved
20 dBaseIV work area ID
21-30 Reserved
31 Field is part of production index - 0x01 else 0x00
*/

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Lê a estrutura de campos da tabela 

METHOD _ReadStruct() CLASS ZDBFFILE 

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cFldBuff := space(32)			AS Character
	Local cFldName, cFldType  			AS Character
	Local nFldLen , nFldDec 			AS Numeric
	Local nPos							AS Numeric

	If ::nHData == -1 
		UserException("_ReadStruct() ERROR - DBF File Not Opened")
		Return .F.
	Endif

	// Reposicionao o arquivo no Offset 32
	FSeek(::nHData,32)

	While .T.

		If FRead(::nHData,@cFldBuff,32) == 0	//EOF
			UserException("_ReadStruct() ERROR - Struct Corrupted")
			Return .F.
		EndIf
		
		If substr(cFldBuff,1,1) == chr(13) 	// 0x0D = Fim da estrutura 
			// 0x0D => Indica final da estrutura
			EXIT
		Endif
		
		cFldName := DBF_OFFSET(cFldBuff,0,11)
		cFldName := left( cFldName,AT(chr(0),cFldName )-1 )
		cFldName := padr(cFldName,10)
		
		cFldType := DBF_OFFSET(cFldBuff,11,1)

		If cFldType == 'C'
			nFldLen	:= Bin2I(DBF_OFFSET(cFldBuff,16,2))
			nFldDec	:= 0
		Else
			nFldLen	:= ASC(DBF_OFFSET(cFldBuff,16,1))
			nFldDec	:= ASC(DBF_OFFSET(cFldBuff,17,1))
		EndIf		
		aadd(::aStruct , Array(nST_TAMARR) )
		nPos	:= Len(::aStruct)
		::aStruct[nPos][nST_CAMPO]	:= cFldName
		::aStruct[nPos][nST_TIPO]	:= cFldType
		::aStruct[nPos][nST_TAMANHO]:= nFldLen
		::aStruct[nPos][nST_DECIMAL]:= nFldDec

		//Otimização!
		cFldName	:= AllTrim(Upper(cFldName))
		::jStruct[cFldName]	:= JsonObject():New()
		::jStruct[cFldName]['POS']	:= nPos
		::jStruct[cFldName]['NAME']	:= cFldName
		::jStruct[cFldName]['TYPE']	:= cFldType
		::jStruct[cFldName]['TAM']	:= nFldLen
		::jStruct[cFldName]['DEC']	:= nFldDec
	Enddo

	::nFldCount := len(::aStruct)

Return .T. 

// ----------------------------------------------------------
// Recupera o conteúdo de um campo da tabela 
// a partir da posiçao do campo na estrutura

METHOD FieldGet(uPos) CLASS ZDBFFILE 

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR uPos		AS Character,Numeric
	
	If valtype(uPos) = 'C'
		uPos := ::FieldPos(uPos)
	Endif

	If uPos > 0 .and. uPos <= ::nFldCount 

		IF ::aStruct[uPos][nST_TIPO] == 'M'
			// Campo MEMO, faz a leitura baseado 
			// no Bloco gravado na tabela 
			Return ::_ReadMemo( ::aGetRecord[uPos] )
		Else
			Return ::aGetRecord[uPos]
		Endif
	
	Endif

Return NIL


// ----------------------------------------------------------
// Atualiza um valor na coluna informada do registro atual 
// Por hora nao critica nada, apenas coloca o valor no array 

METHOD FieldPut(uPos,xValue) CLASS ZDBFFILE 

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR uPos		AS Character,Numeric
	ParamType 0		VAR xValue		AS Character,Numeric,Date,Logical

	If ( !::lCanWrite )
		UserException("Invalid FieldPut() -- File NOT OPEN for WRITING")
	Endif

	If ( ::lEOF )
		UserException("Invalid FieldPut() -- File is in EOF")
	Endif

	If valtype(uPos) = 'C'
		uPos := ::FieldPos(uPos)
	Endif

	If uPos > 0 .and. uPos <= ::nFldCount 
		::aPutRecord[uPos] := xValue
		::lUpdPend := .T. 
	Endif

Return NIL

// ------------------------------------------------------------------------------------------------
// Alimenta o array de campos diretamente, para acelerar a gravação

METHOD FieldBulk(aValores) CLASS ZDBFFILE 

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aValores		AS Array

	//---------------------------------------------------------------------------------------------
	If ( !::lCanWrite )
		UserException("Invalid FieldPut() -- File NOT OPEN for WRITING")
	Endif

	If ( ::lEOF )
		UserException("Invalid FieldPut() -- File is in EOF")
	Endif

	::aPutRecord	:= aClone(aValores)
	::lUpdPend		:= .T. 

Return

// ----------------------------------------------------------
// Recupera o nome do arquivo no disco 
METHOD FileName() CLASS ZDBFFILE
Return ::cDataFile

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| dbDelete                        | Autor | Cirilo Rocha       | Data | 09/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Marca o registro corrente como deletado                                           |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD dbDelete() CLASS ZDBFFILE 
	
	::lDeleted	:= .T.
	::Update()

Return 

// ----------------------------------------
// Retorna .T. caso o registro atual esteja deletado 
METHOD DELETED() CLASS ZDBFFILE 
Return ::lDeleted

// ----------------------------------------
// Retorna o tamanho do HEader
// -- O tamanho do Header é justamente a posicao do offser de dados 
// da tabela, após o final do Header. 

METHOD HEADER() CLASS ZDBFFILE 
Return ::nDataPos

// ----------------------------------------
// Retorna o tamanho ocupado pelo arquivo em bytes 
METHOD FileSize() CLASS ZDBFFILE 

	//Declaracao de variaveis----------------------------------------------------------------------
	Local nFileSize := 0		AS Numeric

	If ::lOpened
		nFileSize := fSeek(::nHData,0,FS_RELATIVE)
	Endif

Return nFileSize

// ----------------------------------------
// Retorna o tamanho de um registro da tabela no arquivo 
// Cada campo MEMO ocupa 10 bytes 

METHOD RECSIZE() CLASS ZDBFFILE 
Return ::nRecLength

// ----------------------------------------
// Retorna o numero do registro atualmente posicionado

METHOD RECNO() CLASS ZDBFFILE 

	If ::lEOF
		Return ::nLastRec+1
	Endif

Return ::nRecno 

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lê o registro posicionado no offset de dados atual 

METHOD _ReadRecord() CLASS ZDBFFILE 
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local cRecord := ''			AS Character
	Local cTipo					AS Character
	Local cValue				AS Character
	Local nTam					AS Numeric
	Local nBuffPos := 2			AS Numeric
	Local nI					AS Numeric
	Local nOffset				AS Numeric

	// ----------------------------------------
	// Calcula o offset do registro atual baseado no RECNO

	nOffset := ::nDataPos 
	nOffset += (::nRecno * ::nRecLength)
	nOffset -= ::nRecLength

	// Posiciona o arquivo de dados no offset do registro 
	FSeek(::nHData , nOffset )

	// Lê o registro do offset atual 
	FRead(::nHData , @cRecord , ::nRecLength )

	// Primeiro byte = Flag de deletato
	// Pode ser " " (espaço)    registro ativo 
	//          "*" (asterisco) registro deletado 
	
	::lDeleted := ( left(cRecord,1) = '*' )

	// Agora lê os demais campos e coloca no ::aGetRecord

	For nI := 1 to ::nFldCount

		cTipo	:= ::aStruct[nI][nST_TIPO]
		nTam	:= ::aStruct[nI][nST_TAMANHO]
		cValue	:= substr(cRecord,nBuffPos,nTam)

		If cTipo == 'C'
			::aGetRecord[nI] := cValue
			nBuffPos += nTam
		ElseIf cTipo == 'N'
			::aGetRecord[nI] := val(cValue)
			nBuffPos += nTam
		ElseIf cTipo == 'D'
			::aGetRecord[nI] := STOD(cValue)
			nBuffPos += nTam
		ElseIf cTipo == 'L'
			::aGetRecord[nI] := ( cValue=='T' )
			nBuffPos += nTam
		ElseIf cTipo == 'M'
			// Recupera o Offset do campo no DBT/FPT
			// aGetRecord sempre vai conter o OFFSET
			::aGetRecord[nI] := val(cValue)
			nBuffPos += nTam
		Endif
	
	Next

	// Reseta flags de BOF e EOF 
	::lBOF := .F. 
	::lEOF := .F. 

Return .T. 


// ----------------------------------------
// Insere um registro em branco no final da tabela
// Apos a inserção, voce pode fazer fieldput 
// e confirmar tudo com UPDATE 
// O array aValores se informado já preenche todos os dados necessários para a gração do registro
//  dispensando os FieldPuts e Update posterior.
METHOD Insert(	aValores	,;	//01 aValores
				lDeleted	);	//02 lDeleted
					CLASS ZDBFFILE

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR aValores		AS Array		Optional Default NIL
	ParamType 1		VAR lDeleted		AS Logical		Optional Default .F.
	
	//---------------------------------------------------------------------------------------------
	If ::lUpdPend
		// Antes de mais nada, se tem um update pendente, faz primeiro o update 
		::Update()
	Endif

	// Limpa o conteudo do registro 
	::_ClearRecord()

	//Gravação já com os valores definidos (OTIMIZAÇÃO!)
	If ValType(aValores) == 'A'
		::aPutRecord	:= aClone(aValores)
	EndIf
	::lDeleted	:= lDeleted

	// Nao estou em BOF ou EOF, 
	// Estou em modo de inserção de registro
	::lBOF := .F. 
	::lEOF := .F. 
				
	// Incrementa uma unidade no contador de registros
	::nLastRec++

	// Recno atual = registro novo 
	::nRecno := ::nLastRec

	// Cria uma pendencia de update 
	// O update vai fazer a inserção no final do arquivo 
	::lNewRecord := .T.

	// Faz o update inserir o registro em branco 
	IF ::Update()

		// Escreve o novo trailer de arquivo 
		::_WriteTrailer()

		// Atualiza o numero do ultimo registro no Header
		::_WriteLastRec()
		
		Return .T. 
	Endif

Return .F. 

// ----------------------------------------
// Grava as alterações do registro atual na tabela 

METHOD Update() CLASS ZDBFFILE
	
	//Declaracao de variaveis----------------------------------------------------------------------
	Local cTipo						AS Character
	Local cSaveRec := '' 			AS Character
	Local nTam						AS Numeric
	Local nI						AS Numeric
	Local nOffset					AS Numeric
	Local nMemoBlock, nNewBlock		AS Numeric
	Local xValue					AS Variant

	If ( ::lEOF )
		UserException("ZDBFFILE::Update() ERROR -- File is in EOF")
		Return
	Endif

	If !::lUpdPend .and. !::lNewRecord
		// Nao tem insert e nao tem update pendente, nao faz nada
		Return
	Endif

	// ----------------------------------------
	// Calcula o offset do registro atual baseado no RECNO

	nOffset := ::nDataPos 
	nOffset += (::nRecno * ::nRecLength)
	nOffset -= ::nRecLength

	// Primeiro byte do registro
	// Flag de deletado 
	cSaveRec := IIF(::lDeleted ,'*',' ') 

	// Agora concatena os demais campos 
	// Se nao houve alteração, monta o buffer com o valor lido

	For nI := 1 to ::nFldCount

		cTipo := ::aStruct[nI][nST_TIPO]
		nTam  := ::aStruct[nI][nST_TAMANHO]
		nDec  := ::aStruct[nI][nST_DECIMAL]

		If cTipo == 'C'

			If ::aPutRecord[nI] != NIL 
				// Ajusta tamanho de string com espaços a direita
				xValue	:= PADR( ::aPutRecord[nI] ,nTam)
				cSaveRec+= xValue
				::aPutRecord[nI] := NIL
				::aGetRecord[nI] := xValue
			Else
				cSaveRec += ::aGetRecord[nI]
			Endif	

		ElseIf cTipo == 'N'

			If ::aPutRecord[nI] != NIL 
				xValue := ::aPutRecord[nI]
				cSaveRec += STR( xValue , nTam, nDec)
				::aPutRecord[nI] := NIL
				::aGetRecord[nI] := xValue
			Else
				cSaveRec += STR( ::aGetRecord[nI], nTam, nDec)
			Endif

		ElseIf cTipo == 'D'

			If ::aPutRecord[nI] != NIL 
				xValue := ::aPutRecord[nI]
				cSaveRec += DTOS( xValue )
				::aPutRecord[nI] := NIL
				::aGetRecord[nI] := xValue
			Else
				cSaveRec += DTOS( ::aGetRecord[nI] )
			Endif

		ElseIf cTipo == 'L'

			If ::aPutRecord[nI] != NIL 
				xValue := ::aPutRecord[nI]
				cSaveRec += IIF( xValue , 'T' , 'F')
				::aPutRecord[nI] := NIL
				::aGetRecord[nI] := xValue
			Else
				cSaveRec += IIF( ::aGetRecord[nI] , 'T' , 'F')
			Endif


		ElseIf cTipo == 'M'

			// Update de campo memo
			// Se realmente foi feito uma troca de valor, vamos ver o que fazer 
			// O bloco usado ( caso tivesse um ) está no ::aGetRecord[nI]

			If ::aPutRecord[nI] != NIL 

				// Pega o valor a atualizar no campo memo 
				xValue := ::aPutRecord[nI]
				
				// Verifica o numero do bloco usado 
				// 0 = sem bloco , sem conteudo 
				nMemoBlock := ::aGetRecord[nI]
				
				// Faz update deste memo. Se nao usava bloco, pode passar
				// a usar. Se já usava, se o memo nao for maior do que o já existente
				// ou nao atingir o limite do block, pode usar o mesmo espaço
				nNewBlock := ::oMemoFile:WRITEMEMO( nMemoBlock , xValue ) 
				
				If nNewBlock <> nMemoBlock
					// Trocou de bloco 
					cSaveRec += str( nNewBlock , 10 )
					// Atualiza a variavel de memoria 
					::aGetRecord[nI] := nNewBlock
				Else
					// Manteve o bloco 
					cSaveRec += str( nMemoBlock , 10 )
				Endif
			
			Else

				// Memo nao foi atualizado. 
				// Mantem valor atual 
				cSaveRec += STR( ::aGetRecord[nI] , 10 )

			Endif
			
		Endif

	Next

	IF len(cSaveRec) > ::nRecLength
		// Jamais, nunca. 
		// Se meu buffer em memoria passou o tamanho do registro 
		// do arquivo, algo deu muito errado ... 
		UserException("ZDBFFILE::Update() ERROR - FIELD BUFFER OVERFLOW")
	Endif

	// Posiciona o arquivo de dados no offset do registro 
	FSeek(::nHData , nOffset )

	// Agora grava o buffer do registro inteiro 
	fWrite(::nHData , cSaveRec , ::nRecLength )

	// Desliga flag de update pendente 
	::lUpdPend := .F. 

	// Atualiza o header do DBF com a data do ultimo update 
	// caso necessario \

	If Date() > ::dLastUpd 
		// Atualiza a data em memoria 
		::dLastUpd  := Date()
		// Regrava a nova data no header 
		::_SetLUpdate()
	Endif

	// Agora que o registro está atualizado, atualiza os indices 
	if (::lNewRecord)
		// Inserção de registro, desliga o flag de inserção 
		::lNewRecord := .F. 
		// Insere a nova chave em todos os indices abertos
		aEval(::aIndexes , {|oIndex| oIndex:InsertKey() })
	Else
		// Atualiza a chave de todos os indices abertos
		aEval(::aIndexes , {|oIndex| oIndex:UpdateKey() })
	Endif

Return .T. 

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Atualiza o header da tabela com a data atualizada
// do ultimo update realizado na tabela 
// Metodo chamado apenas quando a data do header 
// da tabela estiver desatualizada 

METHOD _SetLUpdate() CLASS ZDBFFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cBuffer		AS Character

	// Vai para o offset 1 -- 3 bytes com a data do ultimo update 
	FSeek(::nHData,1)

	// Monta a nova data em 3 bytes 
	cBuffer := 	Chr( Year(::dLastUpd)-2000 ) + ;
				Chr( Month(::dLastUpd) ) + ;
				Chr( Day(::dLastUpd) ) 

	// Grava a nova data no header 
	fWrite(::nHData , cBuffer)

Return

// ----------------------------------------------------------
// *** METODO DE USO INTERNO ***
// Limpa os campos do registro atual 
// ( Inicializa todos com os valores DEFAULT ) 

METHOD _ClearRecord()  CLASS ZDBFFILE

	// Inicializa com o valor default os campos da estrutura 
	_Super:_ClearRecord()

	// Limpa flag de registro deletado 
	::lDeleted := .F. 

Return

// ----------------------------------------
// *** METODO DE USO INTERNO ***
// Lë um campo MEMO de um arquivo DBT 
// baseado no numero do bloco rececido como parametro 

METHOD _ReadMemo(nBlock) CLASS ZDBFFILE

	//Declaracao de variaveis----------------------------------------------------------------------
	Local cMemo := '' 	AS Character

	//Parametros da rotina-------------------------------------------------------------------------
	ParamType 0		VAR nBlock		AS Numeric

	If nBlock > 0

		// Le o conteúdo do campo MEMO 
		cMemo := ::oMemoFile:ReadMemo(nBlock)

	Endif

Return cMemo

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| Destroy                         | Autor | Cirilo Rocha       | Data | 09/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Método para liberar a memória do objeto                                           |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD Destroy()  CLASS ZDBFFILE 

	::Close()

	FreeObj(::oMemoFile)
	FreeObj(::oLogger)
	FreeObj(::oFileDef)

	_Super:Destroy()

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| _WriteTrailer                   | Autor | Cirilo Rocha       | Data | 09/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Grava o campo de final de arquivo 0x1A                                            |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD _WriteTrailer() CLASS ZDBFFILE 

	// Escreve o novo trailer de arquivo 
	FSeek(::nHData,0,FS_RELATIVE)
	fWrite(::nHData , Chr(26) ) // !a = End Of File -> 0x1A

Return

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| _WriteLastRec                   | Autor | Cirilo Rocha       | Data | 09/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Atualiza o número do último registro no Header do arquivo                         |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
METHOD _WriteLastRec() CLASS ZDBFFILE 

	// Atualiza o numero do ultimo registro no Header
	FSeek(::nHData,4)
	fWrite(::nHData , L2Bin(::nLastRec) )

Return
