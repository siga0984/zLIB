#include  'protheus.ch'

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| LeUmDbf                         | Autor | Júlio Wittwer      | Data | 04/01/2019  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Exemplo de como utilizar a classe zDBFFile para leitura de arquivos DBF           |##
//##|        | Este fonte é uma pequena revisão do fonte original do próprio autor da classe     |##
//##|        | https://siga0984.wordpress.com/2019/01/04/lendo-dbf-em-advpl-sem-driver-ou-rdd/   |##
//##|        | https://github.com/siga0984/zLIB                                                  |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|10/03/23| Cirilo R.| Pequena revisão no fonte original (https://github.com/cirilorocha/zLIB)|##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
User Function LeUmDBF()

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aStru								AS Array
	Local cFile := '\backup\sx3990.dbf'		AS Character
	Local nCampos							AS Numeric
	Local nI								AS Numeric
	Local oDBF								AS Object

	// Cria o objeto para leitura do arquivo 
	oDBF := zDBFFile():New(cFile)

	// Faz a abertura. Em caso de falha, 
	// aborta a aplicação obtendo os detalhes do erro
	// usando o método GetErrorStr()
	If !oDBF:Open()
		UserException( oDBF:GetErrorStr() )
	Endif

	// Mostra no log de console alguns dados sobre o arquivo aberto 
	conout(replicate('=' ,79))
	conout("Database File Name : " + cFile )
	conout("Database File Type : " + oDBF:GetDBType()+ " => "+ oDBF:GetDBTypeStr())
	conout("Last Update .......: " + dtoc(oDBF:LUpdate()) )
	conout("Record Count ......: " + cValToChar(oDBF:RecCount()))
	conout("Header Size .......: " + cValToChar(oDBF:Header()))
	conout("Record Length......: " + cValToChar(oDBF:RecSize()))

	// Recupera a estrutura da tabela 
	aStru := oDBF:DbStruct()

	// Verifica quantos campos tem a tabela 
	nCampos := len(aStru)

	// Mostra a estrutura de campos no console
	conout("")
	conout("--- Table Structure --- ")

	For nI := 1 to nCampos
		
		conout("Field ["+aStru[nI][1]+"]"+;          // Nome
			" Type ["+aStru[nI][2]+ "]"+;          // Tipo (CNDLM)
			" Size ["+Str(aStru[nI][3],3)+"]"+;    // Tamanho
			" Dec ["+Str(aStru[nI][4],2)+"]")     // Decimais
		
	Next

	// Mostra o primeiro registro da tabela, e detalhes do registro
	// Caso a tabela esteja vazia, BOF() e EOF() retornam .T. 

	conout(replicate('-' ,79))
	conout("RECNO() ...... "+cValToChar(oDBF:Recno()) )
	conout("BOF() ........ "+cValToChar(oDBF:Bof()) )
	conout("EOF() ........ "+cValToChar(oDBF:Eof()) )
	conout("DELETED() .... "+cValToChar(oDBF:Deleted()) )
	conout("")
	For nI := 1 to len(aStru)
		conout(oDBF:FieldName(nI)+" => " +;
				"["+cValToChar(oDBF:Fieldget(nI))+"]" )
	Next
	conout("")

	// Fecha a tabela 
	oDBF:Close()
	oDBF:Destroy()

	// Limpa / Libera o Objeto 
	FreeObj(oDBF)

Return
