#INCLUDE 'RWMake.ch'
#INCLUDE 'Totvs.ch'
#INCLUDE 'ParmType.ch'

//##################################################################################################
//##+========+=================================+=======+====================+======+=============+##
//##|Programa| GravarDBFTab                    | Autor | Cirilo Rocha       | Data | 10/03/2023  |##
//##+========+=================================+=======+====================+======+=============+##
//##|Desc.   | Pequeno fonte exemplo de usar a classe zDBFFile para efetuar a gravação de arqui- |##
//##|        |  vos em DBF                                                                       |##
//##|        | Baseado nos fontes de Júlio Wittwer                                               |##
//##|        | https://siga0984.wordpress.com/2019/01/04/lendo-dbf-em-advpl-sem-driver-ou-rdd/   |##
//##|        | https://github.com/siga0984/zLIB                                                  |##
//##|        |                                                                                   |##
//##|        | Este exemplo está Otimizado ao máximo para operação de cópia de uma tabela do ban-|##
//##|        |  co para um arquivo DBF, existe outro exemplo com a gravação campo a campo também |##
//##+========+==========+========================================================================+##
//##|  DATA  | ANALISTA | MANUTENÇÃO EFETUADA                                                    |##
//##+========+==========+========================================================================+##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##|        |          |                                                                        |##
//##+========+==========+========================================================================+##
//##################################################################################################
//Posições do array aStruct, apenas para melhorar a leitura do fonte!
Static nST_CAMPO	:= 1	AS Integer
//Static nST_TIPO		:= 2	AS Integer
//Static nST_TAMANHO	:= 3	AS Integer
//Static nST_DECIMAL	:= 4	AS Integer
//-------------------------------------------------------------------------------------------------
User Function GravarDBFTab()	AS Logical

	//Declaracao de variaveis----------------------------------------------------------------------
	Local aDados			AS Array
	Local aPosSrc			AS Array
	Local aStruSrc			AS Array
	Local aStruTgt			AS Array
	Local cTabSrc			AS Character
	Local oDBF				AS Object
	Local lOK	:= .T.		AS Logical
	Local nX				AS Numeric
	Local nPos				AS Numeric

	//Inicializa Variaveis-------------------------------------------------------------------------
	cTabSrc		:= 'SF4'
	(cTabSrc)->(dbSetOrder(1))	//Abre a tabela origem!
	aStruSrc	:= (cTabSrc)->(dbStruct())	//Obtenho a estrutura para criar o destino e para otimização da gravação!
	
	//Instancia objeto que será utilizado
	oDBF := zDBFFile():New('\system\bkp_'+cTabSrc+'.dbf')
	
	//Se o arquivo destino já existir apaga!-------------------------------------------------------
	If oDBF:Exists()
		lOK	:= MsErase(oDBF:FileName(),/*cIndice*/,'DBFCDXADS')
	EndIf

	If lOK
		If 	.Not. oDBF:Create(aStruSrc) .Or. ;	//Cria o arquivo destino
			.Not. oDBF:Open(.T.,.T.)			//Abre o arquivo para alteração em modo exclusivo

			UserException( oDBF:GetErrorStr() )
			lOK	:= .F.
		Endif
	EndIf

	If lOK
		//Preciso obter a estrutura criada exatamente como no destino, por causa de campos memo as
		// vezes ela não respeita a ordem dos campos passados na criação
		aStruTgt	:= oDBF:dbStruct()

		//Monta array aPosSrc para otimização da gravação!
		aPosSrc		:= Array(Len(aStruTgt))
		For nX := 1 to Len(aStruTgt)
			nPos	:= aScan(aStruSrc,{|x| RTrim(x[nST_CAMPO]) == RTrim(aStruTgt[nX][nST_CAMPO]) })
			If nPos <= 0 
				UserException( 	'Problema na estrutura da tabela '+cTabSrc+'.'+CRLF+;
								'Campo Destino: '+aStruTgt[nX][nST_CAMPO]+' não encontrado na origem!' )
				lOk	:= .F.
				Exit
			EndIf
			aPosSrc[nX]	:= nPos
		Next
	EndIf
	
	If lOK

		aDados	:= Array(Len(aStruTgt))
		(cTabSrc)->(dbGoTop())
		While (cTabSrc)->(!EOF())
			//Monta registro para gravação!
			For nX := 1 to Len(aStruTgt)
				//Lendo desta forma é muito rápido, por isso uso esse array aPosSrc já com as posições
				// mapeadas dos campos destino em relação a origem.
				aDados[nX]	:= (cTabSrc)->(FieldGet(aPosSrc[nX]))
			Next
			
			//Este método foi criado com a finalidade de já appendar o registro com todos os dados
			// aumentando muito a performance na gravação em relação a gravação de campo a campo
			oDBF:Insert(aDados,(cTabSrc)->(Deleted()))

			(cTabSrc)->(dbSkip())
		EndDo
	EndIf
	
	//Limpa objeto da memória
	If ValType(oDBF) == 'O'
		oDBF:Close()
		oDBF:Destroy()
			
		FreeObj(oDBF)
	EndIf

	//Fecha tabelas abertas
	If Select(cTabSrc) > 0
		(cTabSrc)->(dbCloseArea())
	EndIf

Return lOK
