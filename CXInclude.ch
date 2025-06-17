#DEFINE TOTVS_PROTHEUS
#Include "parmtypech.ch"

#Define _LINHA_		StrZero(ProcLine(),5)+' '

#define CR	         Chr(13)
#define LF	         Chr(10)
#define CRLF         Chr(13)+Chr(10)

//Aqui é só para não fazer a validação das funções CXVld quando são execuções automáticas de Mashups e Carol
#Define	EH_MVC	(	FWIsInCallStack('PUTMASHUPS') .Or. ;
					FWIsInCallStack('CALLMASHUPS') .Or. ;
					FWIsInCallStack('FWMVCEXECMASHUP') .Or. ;
					FWIsInCallStack('FOLDERACTIVATE') .Or. ;
					FWIsInCallStack('M030APICGC') .Or. ;
					FWIsInCallStack('M020APICGC') )

#Define AMBIENTE_CARREGADO	(	Type('cEmpAnt') == 'C' .And. ;
								Type('cFilAnt') == 'C' .And. ;
								Select('SX2') > 0 )

//Parâmetros usados em MVC
#Define nTP_MODEL	1
#Define nTP_VIEW	2

#Define nPE_MVC_MODEL		1
#Define nPE_MVC_IDPONTO		2
#Define nPE_MVC_IDMODEL		3
#Define nPE_MVC_LINHA		4

#Define MODEL_OPERATION_PRINT	8
#Define MODEL_OPERATION_COPY	9

//Otimiza funcoes de controle de numeracoes--------------------------------------------------------
#xTranslate GetSX8Num	=> FwGetSXENum

#xTranslate GetSXENum	=> FwGetSXENum

#xTranslate SuperGetMV	=> FwSuperGetMV

//Função padrão desativada no padrão substituindo pela nova
#xTranslate PutMV		=> 	FwPutMVPar

#xTranslate IsInCallStack	=> FWIsInCallStack

#xTranslate PrefixoCpo	=> FwPrefixoCpo

#xTranslate inTransaction	=> inTransact

//Função de uso restrito que foi alterada para FW
#xTranslate PTInternal	=> FwPtInternal

#xTranslate WriteProfString	=> FwWriteProfString(

#xTranslate ConOut		=> 	U_CXConOut

#xTranslate CriaVar		=> 	FwCriaVar

#xTranslate InitPad		=> 	FwInitPad

#xTranslate SvcSoapCall	=> U_CXSvcSoapCall

//Otimizacao destas funcoes para executar diretamenta a funcao de interface
#xTranslate Alert		=> 	FwAlertWarning

#xTranslate ApMsgAlert	=> FwAlertWarning

#xTranslate MsgAlert	=> FwAlertWarning

#xTranslate ApMsgInfo	=> FwAlertInfo

#xTranslate MsgInfo		=> 	FwAlertInfo

#xTranslate ApMsgStop	=> FwAlertError

#xTranslate MsgStop		=> 	FwAlertError

#xTranslate ApMsgYesNo	=> FwAlertYesNo

#xTranslate MsgYesNo	=> FwAlertYesNo

#xTranslate ApMsgNoYes	=> FwAlertNoYes

#xTranslate MsgNoYes	=> FwAlertNoYes

#xTranslate xFilial		=> FWxFilial

//A FUNCAO NOVA NAO FUNCIONA CORRETAMENTE
#xTranslate cGetFile		=> U_cGetFile

#xTranslate U_RPCPF01Q		=> U_CXNmUsr
#xTranslate UsrFullName		=> U_CXNmUsr

#xTranslate RetSqlName		=> FWSX2Util():GetFile

#xTranslate U_CXTpDado		=> U_CXDescTipo
#xTranslate U_CXSomaMes		=> MonthSum
#xTranslate U_CXSomaAno		=> YearSum
#xTranslate U_CXTamSX3		=> FwTamSX3
#xTranslate U_CXTabela 		=> FwTabPref
#xTranslate U_CXX3Descric	=> FWSX3Util():GetDescription
#xTranslate U_CXX3Titulo	=> FwX3Titulo
#xTranslate U_CXX2Titulo	=> FwSX2Util():GetX2Name
#xTranslate FwX2Nome		=> FwSX2Util():GetX2Name
//#xTranslate X2Nome			=> FwSX2Util():GetX2Name melhor não mexer com este por conta do possível uso em queries

#xTranslate TamSX3			=> FwTamSX3
#xTranslate NoAcento		=> FwNoAccent
#xTranslate FwNoAcento		=> FwNoAccent
#xTranslate MsDocument		=> MpDocument
	
//#xTranslate UsrRetName	FwUserName	//DA ERRO LOG SE NÃO ACHAR O CÓDIGO!
#xTranslate UsrFullName		=> U_CXNmUsr

//Troca funcao padrao
#xTranslate PutSX1			=> 	U_CXPutSX1

#xTranslate PutHelp			=> 	U_CXPutHelp

#xTranslate PutSX1Help		=> U_CXPutHelp

#xTranslate MemoWrite		=> U_CXWriteFile

//Trocar o modelo3 padrao--------------------------------------------------------------------------
#xTranslate Modelo3	=> 	U_CXMod3

//Retorna se a tabela é exclusiva
#xTranslate U_CXMdExc(<cAlias>) => ;
	(FWModeAccess(<cAlias>)<>'C')

#xTranslate MsgRun(<cText>,<cHeader>,<bAction>) => ;
	FWMsgRun(,<bAction>,<cHeader>,<cText>)

#xTranslate Decorrido(<nSeconds>) => ;
	PadL(LTrim(Str(Int((Seconds() - <nSeconds>)*1000))),10)

//Trocar a funcao X3Combo() padrao pela customizada é otimizada usando cache
#xTranslate X3Combo(<cCampo>,<cCombo>) => ;
	U_CXX3Combo(<cCampo>,,,,<cCombo>)

//Trocar a funcao Separa() padrao pela função de baixo nível StrTokArr2() se comporta examente como a função separa
#xTranslate Separa(<cTexto>) => ;
	StrTokArr2(<cTexto>,',',.F.)

#xTranslate Separa(<cTexto>,<cSeparador>[,<lPodenulo>]) => ;
	StrTokArr2(<cTexto>,<cSeparador>,<lPodenulo>)

//Trocar a funcao Help() padrao pela customizada que trata melhor com MVC
#xTranslate Help(<cCodigo>,<nLinha>,<cCampo>,[<cNome>],<cMensagem>,<nLinha1>) => ;
	U_CXHelp(<cCampo>,<cNome>,<cMensagem>,,,.F.)

#xTranslate MemoRead(<cFile>[,<lChangeCase>]) => ;
	U_CXReadFile(<cFile>,U_CXRotAuto())

#xTranslate MEMOREAD(<cFile>[,<lChangeCase>]) => ;
	U_CXReadFile(<cFile>,U_CXRotAuto())

//Tratamento para validacao do tipo classe---------------------------------------------------------
#xCommand CLASSPARAMEXCEPTION [ PARAM <param> VAR ] <varname> TEXT <text,...> [ MESSAGE <message> ] ;
	=> ;
	[ UserException(<message>) ] ;;
	[ UserException(PT_STR0001+<"param">+PT_STR0002+' '+<"varname">+" erro, classe experada "+\"<text>\") ] ;;
	UserException("argumento erro no parâmetro "+<"varname">+", classe experada "+\"<text>\")

#xCommand PARAMEXCEPTION PARAM <param> VAR <varname> TEXT <text>  ;
	=> ;
	UserException(PT_STR0001+' '+<"param">+PT_STR0002+' '+<"varname">+" erro, tipo experado "+<text>) 

#xCommand PARAMEXCEPTION <varname> TEXT <text>  ;
	=> ;
	UserException("argumento erro no parâmetro "+<"varname">+", tipo experado "+<text>)

#xCommand VALPARAMTYPE [ <param> VAR ] <varname> AS OBJECT CLASS <classname,...> ;
	=> ;
	__bErro := ErrorBlock({|| "UNDEFINED"}) ;;
	BEGIN SEQUENCE ;;
	__classname := Upper(GetClassName(<varname>)) ;;
	END SEQUENCE ;;
	ErrorBlock(__bErro)  ;;
	If !(__classname+',' $ Upper(\"<classname>\")+',') ;;
		CLASSPARAMEXCEPTION [ PARAM <param> VAR ] <varname> TEXT <classname> [ MESSAGE <message> ] ;;
	EndIf ;;

// Optional COM default 
#xCommand PARAMTYPE [ <param> VAR ] <varname> AS OBJECT CLASS <classname,...> ;
	[ MESSAGE <message> ] ;
	[<optional: OPTIONAL>];
	DEFAULT <uVar> ;
	[<optional: OPTIONAL>];
	=> ;
	<varname> := If(ValType(<varname>) == 'U',<uVar>,<varname>)	;;
	If ValType(<varname>) == "O" ;;
		VALPARAMTYPE [ <param> VAR ] <varname> AS OBJECT CLASS <classname> ;;
	ElseIf !(<.optional.> .and. ValType(<varname>) == 'U') ;;
		PARAMEXCEPTION [ PARAM <param> VAR ] <varname> TEXT "O->"+ValType(<varname>) [ MESSAGE <message> ] ;;
	EndIf

// Optional SEM default 
#xCommand PARAMTYPE [ <param> VAR ] <varname> AS OBJECT CLASS <classname,...> ;
	[ MESSAGE <message> ] ;
	[<optional: OPTIONAL>];
	=> ;
	If ValType(<varname>) == "O" ;;
		VALPARAMTYPE [ <param> VAR ] <varname> AS OBJECT CLASS <classname> ;;
	ElseIf !(<.optional.> .and. ValType(<varname>) == 'U') ;;
		PARAMEXCEPTION [ PARAM <param> VAR ] <varname> TEXT "O->"+ValType(<varname>) [ MESSAGE <message> ] ;;
	EndIf

//Parâmetros obrigatórios
#xCommand PARAMOBG [ <param> VAR ] <varname> ;
	[ MESSAGE <message> ] ;
	=> ;
	If (ValType(<varname>) == 'U' ) ;;
		UserException(PT_STR0001+<"param">+PT_STR0002+<"varname">+" erro, é obrigatório e está NULL. " [ MESSAGE <message> ]) ;;
	EndIf ;;

//Parâmetros opcionais com default reavaliado
#xCommand PARAMOPC [ <param> VAR ] <varname> AS <type: ARRAY, BLOCK, CHARACTER, DATE, NUMERIC, LOGICAL, OBJECT, JSON> ;
	[ , <typeN: ARRAY, BLOCK, CHARACTER, DATE, NUMERIC, LOGICAL, OBJECT, JSON> ] ;
	[ MESSAGE <message> ] ;
	DEFAULT <uVar> ;
	=> ;
	If (ValType(<varname>) == 'U');;
		If (ValType(<uVar>) $ Subs(<"type">,1,1) [ + Subs(<"typeN">,1,1) ]) ;;
			<varname> := <uVar>	;;
		Else;;
			UserException(PT_STR0001+<"param">+PT_STR0002+<"varname">+" erro, valor DEFAULT com tipo inválido. "+ ;
							Subs(<"type">,1,1) [ + "," + Subs(<"typeN">,1,1) ]+"->"+ValType(<uVar>) [ MESSAGE <message> ]) ;;
		EndIf;;
	EndIf ;;

// Optional com default (FEITO AQUI PARA MELHOR COMPATIBILIDADE COM TLPP)
#xCommand PARAMTYPE [ <param> VAR ] <varname> AS <type: ARRAY, BLOCK, CHARACTER, DATE, NUMERIC, LOGICAL, OBJECT, JSON> ;
	[ , <typeN: ARRAY, BLOCK, CHARACTER, DATE, NUMERIC, LOGICAL, OBJECT, JSON> ] ;
	[ MESSAGE <message> ] ;
	[<optional: OPTIONAL>];
	DEFAULT <uVar> ;
	[<optional: OPTIONAL>];
	=> ;
	If ValType(<varname>) <> 'U' .and. !(ValType(<varname>) $ Subs(<"type">,1,1) [ + Subs(<"typeN">,1,1) ]) ;;
		PARAMEXCEPTION [ PARAM <param> VAR ] <varname> TEXT Subs(<"type">,1,1) [ + "," + Subs(<"typeN">,1,1) ]+"->"+ValType(<varname>) [ MESSAGE <message> ] ;;
	EndIf ;;
	<varname> := If(ValType(<varname>) == 'U' ,<uVar>,<varname>)

//NAO CONSEGUI FAZER ISSO COM O CONTROLE DE ARREA
//#DEFINE GetArea	tCtrlAlias():GetAlias

//#xTranslate RestArea(<oArea>) => ;
//	<oArea>:RestArea()

//NÃO FUNCIONOU
//#xTranslate mBrowse([<upar1>],[<upar2>],[<upar3>],[<upar4>],<cAlias>[,<aFixe>[,<cCpo>[,<upar8>[,<cFun>[,;
//					<nDefault>[,<aColors>[,<cTopfun>[,<cBotfun>[,<upar14>[,<bParbloco>[,<lNotopfilter>[,;
//					<lSeeall>[,<lChgall>[,<cExprfiltop>[,<nInterval>[,<bTimeraction>[,<cTela>[,<cFilterdefault>[,;
//					<bBeforeactivate>[,<aObfuscfields>] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] => ;
//			FWmBrowse(	<cAlias>,<aFixe>,<cCpo>,IIF(ValType(<cFun>)=='C',<cFun>,'.F.'),;
//						<nDefault>,<aColors>,<cTopfun>,<cBotfun>,<bParbloco>,IIF(ValType(<lNotopfilter>)=='L',<lNotopfilter>,.F.),;
//						<lSeeall>,<lChgall>,<cExprfiltop>,<nInterval>,<bTimeraction>,<cTela>,<cFilterdefault>,;
//						<bBeforeactivate>,<aObfuscfields>

//		<oFWBrowse> := FWMBrowse():New();;
//		<oFWBrowse>:SetDataTable(.T.);;
//		If( ValType(<cAlias>) == "C" );;
//			<oFWBrowse>:SetAlias(<cAlias>);;
//		EndIf;;
//		If( ValType(<cTopFun>) == "C" );;
//			<oFWBrowse>:SetTopFun(<cTopFun>);;
//		EndIf;;
//		If( ValType(<cBotFun>) == "C" );;
//			<oFWBrowse>:SetBotFun(<cBotFun>);;
//		EndIf;;
//		If( ValType(<lNotopfilter>) == "L" );;
//			<oFWBrowse>:SetUseFilter(<lNotopfilter>);;
//		EndIf;;
//		If( ValType(<lSeeall>) == "L" );;
//			<oFWBrowse>:SetSeeAll(<lSeeall>);;
//		EndIf;;
//		If( ValType(<lChgall>) == "L" );;
//			<oFWBrowse>:SetChgAll(<lChgall>);;
//		EndIf;;
//		If( ValType(<cExprfiltop>) == "C" );;
//			<oFWBrowse>:SetFilterDefault(<cExprfiltop>);;
//		EndIf;;
//		If( ValType(<bTimeraction>) == "B" );;
//			<oFWBrowse>:SetTimer(<bTimeraction>,<nInterval>);;
//		EndIf;;
//		If( ValType(<cTela>) == "C" );;
//			<oFWBrowse>:SetX3Tela(<cTela>);;
//		EndIf

