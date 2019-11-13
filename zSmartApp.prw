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

====================================================== */

CLASS ZSMARTAPP FROM LONGNAMECLASS

   DATA oEnv
   DATA cAppTitle
   DATA oMainWnd
   DATA oMainMenuBar
   DATA aMenus
   
   DATA bDefFactory                 // Factory de definições diferenciado

   METHOD New() 					// Construtor 
   METHOD SetTitle(cTitle)          // Define o titulo da aplicação
   METHOD fromJSON()                // Cria a aplicação baseado em um arquivo JSON
   METHOD SetDefFactory()           // Seta factory de definicoes diferenciado
   METHOD SetZLIBEnv()              // Seta o ambiente 
   METHOD SetMenu()                 // Configura a MenuBar da aplicação 
   METHOD Call()                    // Encapsula a execução de uma função
   METHOD CallZApp()                // Chama um modelo de aplicação ZAPP 
   METHOD CallZautoApp()            // Modelo de chamada automatizado ZAUTOAPP
   METHOD Run()						// Roda a Aplicação 
   METHOD Close()                   // Fechamento da aplicação 
   METHOD Done()                    // Finaliza os objetos 
  
ENDCLASS


// ------------------------------------------------------
// Construtor

METHOD NEW() CLASS ZSMARTAPP
Return self

// ------------------------------------------------------
// Seta o Título da aplicação
METHOD SetTitle(cTitle) CLASS ZSMARTAPP
::cAppTitle := cTitle
Return

// ------------------------------------------------------
// Seta o ambiente 

METHOD SetZLIBEnv(oEnv) CLASS ZSMARTAPP
::oEnv := oEnv
Return

// ------------------------------------------------------
// Permite trocar / encapsular o factory de definicoes

METHOD SetDefFactory(bDefBlock)  CLASS ZSMARTAPP
::bDefFactory := bDefBlock
Return

// ------------------------------------------------------
// Cria a aplicação baseado em um arquivo JSON

METHOD fromJSON(cJsonFile) CLASS ZSMARTAPP           
Local aMenus := {}
Local aProgs := {}
Local aAjuda := {}
Local oJSon
Local cJsonStr
Local aJsonMenu 
Local nJ

// Lê a definição da aplicação 
cJsonStr := memoread(cJsonFile)

// Cria o objeto JSON baseado na definição 
oJson 		:= JsonObject():New()
oJson:fromJson(cJsonStr)

// Recupera o título da aplicação 
::cAppTitle := oJson:GetJsonText("DisplayName")

// Recupera o menu de Programas
aJsonMenu := oJson:GetJsonObject("Menu")

IF valtype(aJsonMenu) <> 'A'
	UserException("Menu not found or invalid on JSON App Definition")
Endif

For nJ := 1 to len(aJsonMenu)
	oMenuOpt   := aJsonMenu[nJ]
	cMenuLabel := oMenuOpt:GetJSonText('name')
	cMenuType  := oMenuOpt:GetJSonText('type')
	
	IF cMenuType == 'ZAUTOAPP'
		cDefName := oMenuOpt:GetJSonText('definition')
		aadd(aProgs,{cMenuLabel    , &("{ || self:CallZAutoApp('"+cDefName+"') }") } )
	Else
		conout("Unsupported Menu ["+cMenuLabel+"] Type ["+cMenuType+"]")
	Endif

Next

// Joga jora o objeto 
freeObj(oJson)

// complementa menu de programas
aadd(aProgs,{"-" , NIL } )
aadd(aProgs,{"Sai&r"     ,{ || self:Close() }} )
aadd(aMenus , {"&Programas" , aProgs } )

// Acrescenta o menu de Ajuda -> Sobre 
aadd(aAjuda,{"&Sobre"   ,{ || MsgInfo("<html><b>"+::cAppTitle+"</b><br>Aplicativo em Desenvolvimento","*** ZLIB ***") }} )
aadd(aMenus , {"&Ajuda" , aAjuda } )

// Define o menu da aplicação 
::SetMenu(aMenus)

Return

// ------------------------------------------------------

METHOD SetMenu(aMenus) CLASS ZSMARTAPP
::aMenus := aMenus
Return

// ------------------------------------------------------
// Executor

METHOD RUN() CLASS ZSMARTAPP
Local oFont
Local nI , nJ
Local oThisMenu  
Local cCaption
Local oThisItem 
Local oMainPanel

// Usa uma fonte Fixed Size
oFont := TFont():New('Courier new',,-14,.T.)
oFont:Bold := .T. 

// Seta que a partir daqui esta é a fonte default
SetDefFont(oFont)

// Cria a janela principal da aplicação
DEFINE WINDOW ::oMainWnd FROM 0,0 TO 800,600  PIXEL TITLE ::cAPPTitle COLOR CLR_WHITE,CLR_BLACK

// Painel Central da aplicação 
// Fundo da Main Window 
@ 00,00 MSPANEL oMainPanel OF ::oMainWnd COLOR CLR_WHITE,CLR_RED  SIZE 1,1 RAISED
oMainPanel:Align:= CONTROL_ALIGN_ALLCLIENT

// Cria a barra de menu superior dentro do painel 
::oMainMenuBar := tMenuBar():New(oMainPanel)

// Popula os menus da aplicação 
For nI := 1 to len(::aMenus)

	// Cria um Menu 
	oThisMenu := TMenu():New(0,0,0,0,.T., NIL ,::oMainWnd)

	// Acrescenta as opções no Menu 
	For nJ := 1 to len(::aMenus[nI][2])

		cCaption := ::aMenus[nI][2][nJ][1]
		If 	cCaption == '-'
			oThisItem := TMenuItem():New(::oMainWnd,cCaption	, NIL , NIL , .F. , NIL ,,,,,,,,,.T.)
			oThisMenu:Add( oThisItem )
		Else
			oThisItem := TMenuItem():New(::oMainWnd,cCaption	, NIL , NIL , .T. , ::aMenus[nI][2][nJ][2],,,,,,,,,.T.)
			oThisMenu:Add( oThisItem )
		Endif
	Next	

	// Acrescenta o Menu na Barra de Menus 
	::oMainMenuBar:AddItem( ::aMenus[nI][1],oThisMenu ,.T.)
	
Next

::oMainWnd:LESCCLOSE := .T.

ACTIVATE WINDOW ::oMainWnd  ; // MAXIMIZED ;
	VALID ( MsgYesNo("Deseja encerrar o aplicativo ?") )

Return


// ------------------------------------------------------
// Encapsula a execução de uma função

METHOD Call(cFnCall)  CLASS ZSMARTAPP
&cFnCall.()
Return

// ------------------------------------------------------
// Chama um modelo de aplicação ZAPP 

METHOD CallZApp(cFnCall)  CLASS ZSMARTAPP
Local aCoords 

aCoords := { ::oMainWnd:nTop+40,::oMainWnd:nLeft+10,::oMainWnd:nBottom-40,::oMainWnd:nRight-20 }
&cFnCall.(aCoords,,)

Return

// ------------------------------------------------------
// Chama um modelo de aplicação ZAUTOAPP 
// Aplicação automatica criada sobe a definição 
// A AutoApp permite a troca do factory de definicoes

METHOD CallZAutoApp(cDefName)  CLASS ZSMARTAPP
Local aCoords 
Local oApp := ZAUTOAPP():New()

// Pega as coordenadas da janela principal para rodar a aplicação em cima
aCoords := { ::oMainWnd:nTop+40,::oMainWnd:nLeft+10,::oMainWnd:nBottom-40,::oMainWnd:nRight-20 }

IF ::bDefFactory != NIL 
	// Seta um encapsulamento para o factory de definicoes
	oApp:SetDefFactory(::bDefFactory)
Endif

// Roda o MVC baseado na definicao informada
oApp:RunMVC(cDefName,aCoords)

// Libera o objeto 
FreeObj(oApp)

Return

// ------------------------------------------------------
// Encapsula o fechamento da aplicação 

METHOD Close()  CLASS ZSMARTAPP
::oMainWnd:End()
Return


// ------------------------------------------------------

METHOD Done() CLASS ZSMARTAPP
Return

