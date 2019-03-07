#include 'protheus.ch'
#include 'zlib.ch' 

#define CALCSIZEGET( X )  (( X * 4 ) + 8)

// Interface sem botoes de navegacao por hora 
// Parece nao fazer muito sentido, a nao ser em um browser ou lista
// #define HAS_NAVBUTTONS 

/* ======================================================

Classe de View Modelo -- Interface Smartclient

View Layer:

This is a presentation layer.
It consists of HTML, JSP, etc. into it.
It normally presents the UI of the application.
It is used to display the data which is fetched from the controller which in turn fetching data from model layer classes.
This view layer shows the data on UI of the application.

====================================================== */

CLASS ZMVCVIEW FROM LONGNAMECLASS

	DATA oControl        // Objeto de controle dos dados 
	DATA cTitle          // Titulo da Janela de Interface
	DATA aGets           // Array com objetos da interface e definicao 

#ifdef HAS_NAVBUTTONS
	DATA aBtnNav         // Botoes de Navegação 
#endif

	DATA aBtnAct         // Botões de ação padrão ( incluir, sair, etc ) 
	DATA aActions        // Acoes adicionais do componente
	DATA nAction         // Ação em execução 
	DATA oBtnOk          // Botão de conformação de ação 
	DATA oBtnCanc        // Botão de cancelamento de ação 
	DATA cError          // Ultimo erro da interface
	DATA oLogger         // Objeto de log 
	DATA nRecno          // Registro atual em foco 
	DATA aOldRecord      // Registro com valores de campos no inicio de uma operação 

    METHOD NEW()         // Construtor
    METHOD SetControl()  // Abarra objeto de controle de dados
	METHOD RUN()         // Inicia a interface
	METHOD RUNINTF()     // Monta componentes de tela
	METHOD DONE()        // Encerra a interface

    METHOD SEARCH()      // Pesquisa de registros
	METHOD INSERT()      // Inserção de novo registro
	METHOD UPDATE()      // Alteração de registro existente
	METHOD DELETE()      // Ecvlusão de registro já existente
	                    
#ifdef HAS_NAVBUTTONS
    METHOD GOFIRST()
    METHOD GONEXT()
    METHOD GOPREV() 
    METHOD GOLAST() 
#endif

	METHOD CONFIRM()     // Confirma operação em execução
	METHOD CANCEL()      // Cancela operação em execução
	
	METHOD GETERRORSTR() // Recupera ultima ocorencoa de erro 
	METHOD SETERROR()    // Seta uma ocorrencia de error 
	METHOD ClearError()  // Limpa ocorrencia de erro 
	
	METHOD RunAction(nAct)

ENDCLASS


// ----------------------------------------------------------
// Construtor

METHOD NEW(cTitle) CLASS ZMVCVIEW
::cTitle  := cTitle
::cError  := ''
::aGets   := {}
::aOldRecord := {}

#ifdef HAS_NAVBUTTONS
::aBtnNav := {}
#endif

::aBtnAct := {}
::aActions := {}
::nAction := 0
::nRecno  := 0

::oLogger := ZLOGGER():New("ZMVCVIEW")
::oLogger:Write("NEW","Interface ["+cTitle+"]")

Return self

// ----------------------------------------------------------
// Recebe o objeto de controle  

METHOD SetControl(oObj)  CLASS ZMVCVIEW
::oLogger:Write("SetControl","Set Control ["+GetClassName(oObj)+"]")
::oControl := oObj
Return

// ----------------------------------------------------------
// Monta e executa a interface (view)

METHOD RUN() CLASS ZMVCVIEW
Local oFont
Local oDlg

::oLogger:Write("Run","Begin Interface")
::ClearError()

// Antes de rodar verifica se o objeto de controle foi setado
If ::oControl = NIL
	::SetError("ZMVCVIEW:RUN() -- CONTROL OBJECT NOT SET")
	Return .F.
Endif

// Usa uma fonte Fixed Size
oFont := TFont():New('Courier new',,-14,.T.)

// Cria a janela principal como uma DIALOG
DEFINE DIALOG oDlg TITLE (::cTitle) ;
FROM 0,0 TO 480,930 ;
FONT oFont ;
COLOR CLR_BLACK, CLR_LIGHTGRAY PIXEL

// Interface montada na inicialização da janela
oDlg:bInit := {|| self:RUNINTF(oDlg) }

ACTIVATE DIALOG oDlg CENTER ;
VALID ( MsgYesNo("Deseja fechar e sair da aplicação ?") )

::oLogger:Write("Run","End Interface")

Return .T.

// ----------------------------------------------------------
// A execução da interface monta a tela
// Baseado na definição dos campos

METHOD RUNINTF(oDlg)  CLASS ZMVCVIEW
Local oPanelMenu
Local oPanelBase
Local oPanelCrud
Local oPanelNav
Local nI, aFieldDef , nFldCount
Local oFldDef
Local nRow , cPicture , nGetSize
Local _Dummy_
Local oNewSay , oNewGet
Local aRecord := {}
Local oTable
Local oBtnInsert, oBtnUpdate, oBtnDelete , oBtnExit
Local oBtnFirst, oBtnPrev, oBtnNext, oBtnLast
Local nActRow
Local oBtnAct, cPrompt

::oLogger:Write("RunIntf")

// Monta o menu de opções
@ 0,0 MSPANEL oPanelMenu OF oDlg SIZE 70,600 COLOR CLR_WHITE,CLR_GRAY
oPanelMenu:ALIGN := CONTROL_ALIGN_LEFT

#ifdef HAS_NAVBUTTONS

	// Painel com as opções de navegação
	@ 0,0 MSPANEL oPanelNav OF oDlg SIZE 70,600 COLOR CLR_WHITE,CLR_GRAY
	oPanelNav:ALIGN := CONTROL_ALIGN_RIGHT

#endif

@ 05,05  BUTTON oBtnSearch PROMPT "Pesquisar" SIZE 60,15 ;
	ACTION ( self:SEARCH() ) OF oPanelMenu PIXEL

@ 20,05  BUTTON oBtnInsert PROMPT "Incluir" SIZE 60,15 ;
	ACTION ( self:INSERT() ) OF oPanelMenu PIXEL

@ 35,05  BUTTON oBtnUpdate PROMPT "Alterar" SIZE 60,15 ;
	ACTION ( self:UPDATE() ) OF oPanelMenu PIXEL

@ 50,05  BUTTON oBtnDelete PROMPT "Excluir" SIZE 60,15 ;
	ACTION ( self:DELETE() ) OF oPanelMenu PIXEL

// Guarda os botões de ação

aadd(::aBtnAct,oBtnSearch) // 1
aadd(::aBtnAct,oBtnInsert) // 2 
aadd(::aBtnAct,oBtnUpdate) // 3 
aadd(::aBtnAct,oBtnDelete) // 4

// Pergunta ao controller se existem mais ações 
::aActions := ::oControl:GetActions()

nActRow := 65
For nI := 1 to len(::aActions)

    cPrompt := ::aActions[nI][1]
	@ nActRow,05  BUTTON oBtnAct PROMPT cPrompt SIZE 60,15 OF oPanelMenu PIXEL
	oBtnAct:BACTION := &("{|| self:RunAction("+cValToChar(nI)+") }")
	aadd(::aBtnAct,oBtnAct)
	oBtnAct := NIL 
	nActRow += 15 
	
Next

@ nActRow,05  BUTTON oBtnExit PROMPT "Sair" SIZE 60,15 ;
	ACTION oDlg:End() OF oPanelMenu PIXEL

// Por ultimo acrescenta o botão para sair do componente
aadd(::aBtnAct,oBtnExit)   

// No momento inicial desliga o update e o delete 
// Somente mostra update e delete quando tem um registro em foco
::aBtnAct[3]:SETENABLE(.F.)
::aBtnAct[4]:SETENABLE(.F.)

// Monta os botoes de navegação

#ifdef HAS_NAVBUTTONS

	@ 05,05  BUTTON oBtnFirst PROMPT "Primeiro" SIZE 60,15 ;
		ACTION ( self:GOFIRST() ) OF oPanelNav PIXEL

	@ 020,05  BUTTON oBtnPrev PROMPT "Anterior" SIZE 60,15 ;
		ACTION ( self:GOPREV() ) OF oPanelNav PIXEL

	@ 35,05  BUTTON oBtnNext PROMPT "Próximo" SIZE 60,15 ;
		ACTION ( self:GONEXT() ) OF oPanelNav PIXEL

	@ 50,05  BUTTON oBtnLast PROMPT "Último" SIZE 60,15 ;
		ACTION ( self:GOLAST() ) OF oPanelNav PIXEL

	// Guarda botoes de navegação
	aadd(::aBtnNav,oBtnFirst)
	aadd(::aBtnNav,oBtnPrev)
	aadd(::aBtnNav,oBtnNext)
	aadd(::aBtnNav,oBtnLast)
#endif

// Monta a tela do CRUD com os campos da tabela

@ 0,0 SCROLLBOX oPanelBase SIZE 100,100 OF oDlg VERTICAL     
oPanelBase:ALIGN := CONTROL_ALIGN_ALLCLIENT  

@ 0,0 MSPANEL oPanelCrud OF oPanelBase SIZE 1,1 COLOR CLR_WHITE,CLR_LIGHTGRAY
oPanelCrud:ALIGN := CONTROL_ALIGN_ALLCLIENT  

// Pega o array a definicao dos campos a partir da definicao do componente
aFieldsDef := ::oControl:GetObjectDef():GetFields()

// Monta os arrays de objetos e variaveis de interface
nFldCount := len( aFieldsDef )

// Inicializa os Gets e suas Variaveis
::aGets := Array( nFldCount )

// Limpa registro anterior
::aOldRecord := {}

// Começa na linha 5, um campo por linha
nRow := 5

For nI := 1 to nFldCount
	
	// Pega o objeto da definicao deste campo
	oFldDef := aFieldsDef[nI]
	
	IF oFldDef:IsVisible()

		// Monta o label deste campo -- incialmente em branco 
		@   nRow+3,05 SAY oNewSay PROMPT " " RIGHT SIZE 50,12 OF oPanelCrud PIXEL
		oNewSay:SetText(oFldDef:GetLabel() )

    Endif
	
    // Calcula o tamanho do campo baseado na picture 
	cPicture := oFldDef:GetPicture()
	If left(cPicture,3) == '@R '
		nGetSize := CALCSIZEGET( MAX(oFldDef:GetSize(),len(cPicture)-3) ) 
	Else
		nGetSize := CALCSIZEGET( oFldDef:GetSize() ) 
	Endif	
	
	// Monta o GET para este campo , inicialmente com uma variavel "dummy"
	@   nRow,60 GET oNewGet VAR _Dummy_ PICTURE (cPicture)   ;
		SIZE nGetSize ,12 OF oPanelCrud PIXEL

	// Guarda o objeto GET montado no array aGets
	// [1] Nome do campo
	// [2] Objeto tGet de interface
	// [3] Objeto da definicao do campo
	// [4] Valor do campo no GET
	// --- Inicializado com o valor default
	
	::aGets[nI] := { oFldDef:GetField() , oNewGet , oFldDef , oFldDef:DefaultValue()  }
	
	// Troco a variavel do GET
	// Monta codeblock para usar a quarta coluna 
	// do proprio array de Gets como variável de conteúdo do GET 
	
	cBLock := "{ |x| IIF( PCOUNT() > 0 , self:aGets["+cValToChar(nI)+"][4] := x , self:aGets["+cValToChar(nI)+"][4] ) }"
	oNewGet:bSetGet := &(cBLock)
	
	// O Get nasce desabilitado
	oNewGet:SetEnable(.F.)  
	
    // Verifica se este GET está visível 
	oNewGet:LVISIBLE := oFldDef:IsVisible()
	
	// Cada novo campo pula 15 PIXELS 
	// ( caso o campo esteja visivel ) 
	IF oFldDef:IsVisible()
		nRow += 15
	Endif
	
Next

// Pula mais uma linha
nRow += 15

@ nRow,60  BUTTON ::oBtnOk PROMPT "Confirmar" SIZE 60,15 ;
ACTION ( self:CONFIRM() ) OF oPanelCrud PIXEL

@ nRow,180  BUTTON ::oBtnCanc PROMPT "Cancelar" SIZE 60,15 ;
ACTION ( self:CANCEL() ) OF oPanelCrud PIXEL

// Seta a altura interna do painel do CRUD 
// Movendo o painel para novas coordenadas, calculando 
// a altura interna baseado na ultima linha da tela

oPanelCrud:Move(0,0,oPanelCrud:nWidth,(nRow+20)*2,,.T.)

// Esconde os botões de confirmar e cancelar
::oBtnOk:Hide()
::oBtnCanc:Hide()

#ifdef HAS_NAVBUTTONS
	// Esconde os botoes de navegação
	aEval (::aBtnNav , {|x| x:Hide() } )
#endif

Return

// ----------------------------------------------------------
// Metodo de finalizacao da interface
// Por hora nao precisa fazer nada, os objetos utilizados
// devem ser destruidos pelo componente que criou

METHOD DONE() CLASS ZMVCVIEW

::oLogger:Write("Done")

::aGets      := {}
::aOldRecord := {}
#ifdef HAS_NAVBUTTONS
::aBtnNav    := {}
#endif
::aBtnAct    := {}

Return


// ----------------------------------------------------------
// Metodo de inserção
// Inicia interface com registro em branco para inserção
// O registro é inicializado com os valores default

METHOD INSERT() CLASS ZMVCVIEW
Local nFldCount
Local nI , oFldDef, oGet
Local aRecord := {}
Local nPos

::oLogger:Write("Insert","Create New Record")

// A inserção via interface deve partir de um registro inicializado
// com os valores default, atraves do controlador

If !::oControl:NewRecord(aRecord)
	MsgStop("Não é possível iniciar uma inserção neste momento","oControl:NewRecord()")
	Return
Endif

// Registro anterior com valores default
::aOldRecord := aClone(aRecord)

// Ação em execução = Incluir Registro
::nAction := 1

// Alimenta interface com os valoes iniciais dos campos
// Inicializa os campos com os valores default

nFldCount := len( ::aGets )

For nI := 1 to nFldCount
	
	// Pega o nome do campo, o objeto do GET e a definicao do campo 
	cFldName := ::aGets[nI][1]
	oGet     := ::aGets[nI][2]
	oFldDef  := ::aGets[nI][3]
	
	// Busca o conteudo do registro 
	// Caso nao seja encontrado usa o valor default
	nPos := ascan( aRecord , {|x| x[1] == cFldName })

	If nPos > 0 
		// Atualiza gets com valor inicial do campo
		::aGets[nI][4] := aRecord[nPos][2]
	Else
		// Inicializa com valor default 
		::aGets[nI][4] := oFldDef:DefaultValue()
	Endif

	// Define campos ativos baseados na definicao
	oGet:SetEnable( oFldDef:IsEnabled() )

	// Define se o campo é somente leitura ou nao 
	oGet:LREADONLY := oFldDef:IsReadOnly()

Next

#ifdef HAS_NAVBUTTONS
	// Esconde os botões de navegação
	aEval (::aBtnNav , {|x| x:Hide() } )
#endif

// Desabilita os primeiros botoes de ação
for nI := 1 to 4 
	::aBtnAct[nI]:SetEnable(.F.)
Next

// Mostra botões de confirmar e cancelar
::oBtnOk:Show()
::oBtnCanc:Show()

Return

// ----------------------------------------------------------
// Confirmação da operação atual

METHOD CONFIRM() CLASS ZMVCVIEW
Local lOk
Local nFldCount
Local cFldName
Local nFld , nPos
Local aRecord := {}
Local aFound := {}
Local oGet

::oLogger:Write("Confirm","Action="+cValToChar(::nAction))

If ::nAction = 1
	
	// Confirmar inclusão de registro
	// Monta um array com o registro a incluir
	// e passa para o Controller
	
	nFldCount := len(::aGets)
	For nFld := 1 to nFldCount
		aadd(aRecord,{::aGets[nFld][1],::aGets[nFld][4]})
	Next
	
	// O Controller é que faz as honras
	// repassando para o modelo
	lOk := ::oControl:Write( aRecord )
	
	If !lOk
		
		MsgStop(::oControl:GetErrorStr(),"FALHA DE INCLUSAO")
		
	Else
		
		// Operação deu certo. 
		If MsgYesNo("<html>Registro incluído com sucesso.<br>Deseja incluir mais um registro ?")
			::Insert()
		Else
			::Cancel()
		Endif
		
	Endif
	
ElseIF ::nAction = 2
	
	// Confirmação de Busca (search)
	// A busca genérica pode informar um ou mais campos para serem pesquisados.
	// Será trazido o primeiro registro onde todos os campos preenchidos atenderem
	// ao criterio de busca.
	
	// Primeiro preenche o registro na memoria com os valores informados
	nFldCount := len(::aGets)
	For nFld := 1 to nFldCount
		If !empty(::aGets[nFld][4])
			aadd(aRecord,{::aGets[nFld][1],::aGets[nFld][4]})
		Endif
	Next
	
	lOk := ::oControl:Search( aRecord , aFound )
	
	If !lOk
		
		MsgStop(::oControl:GetErrorStr(),"Atenção")
		
	Else
		
		// Operação deu certo.
		// Atualiza valores de tela com o registro encontrado 
		// E coloca todos os gets como read only 
				
		For nFld := 1 to nFldCount
			cFldName := ::aGets[nFld][1]
			nPos := ascan( aFound , {|x| x[1] == cFldName })
			If nPos > 0 
				::aGets[nFld][4] := aFound[nPos][2]
			Else
				// Campo nao existe na estrutura 
				// ou nao foi retornado ?! Que medo ... 
				// Por hora desliga o GET
				::oLogger:Write("Confirm","Warning: Interface Field ["+cFldName+"] not found.")
				::aGets[nFld][2]:SetEnable(.F.)
			Endif
			::aGets[nFld][2]:LREADONLY := .T.
		Next
		               
		nPos := ascan( aFound , {|x| x[1] == 'RECNO' })
		If nPos > 0 
			::nRecno  := aFound[nPos][2]
			aDel(aFound,nPos)
			aSize(aFound,len(aFound)-1)
		Else
			::nRecno  := 0 
		Endif

		// Registro antigo com valores atuais
		::aOldRecord := aClone(aFound)

#ifdef HAS_NAVBUTTONS
		// Mostra botoes de navegacao 
		aEval (::aBtnNav , {|x| x:Show() } )
#endif

        // Esconde o botao de confirmar 		
        ::oBtnOk:Hide()
		
		// Mostra botões para alterar e escluir 
		::aBtnAct[3]:SETENABLE(.T.)
		::aBtnAct[4]:SETENABLE(.T.)

	Endif
	
ElseIf ::nAction = 3

	// Confirmar alteração de registro
	// Monta um array com os dados a alterar 
	// e passa para o Controler repassar ao Modelo 
	// Monta o registro apenas com os dados alterados 
	
	nFldCount := len(::aGets)
	For nFld := 1 to nFldCount
		cFldName := ::aGets[nFld][1]
		nPos := Ascan(::aOldRecord,{|x| x[1] == cFldName })
		xOldValue := ::aOldRecord[nPos][2]
		xNewValue := ::aGets[nFld][4]
		If xOldValue <> xNewValue
			aadd(aRecord,{ ::aGets[nFld][1], xNewValue })
		Endif
	Next
	
	If len(aRecord) < 1 
		// Nada foi alterado, volta pra tela de alteração 
		MsgStop("Nenhuma informação foi alterada.","ALTERAÇÃO")
		Return
	Endif
	
	// Acrescenta o RECNO para localizar o registro 
	aadd(aRecord,{"RECNO",::nRecno})
	
	// O Controller é que faz as honras
	// repassando para o modelo
	lOk := ::oControl:Update( aRecord )
	
	If !lOk
		
		MsgStop(::oControl:GetErrorStr(),"FALHA DE ALTERAÇÃO")
		
	Else

        MsgInfo("Alterações gravadas com sucesso.")
				
		// Operação deu certo. Volta ao estado inicial
		::Cancel()
		
	Endif

Else
	
	MsgStop("*** TODO ACTION "+cValTochar(::nAction)+" ***","Confirmar")
	
Endif

Return


// ----------------------------------------------------------
// O metodo de cancelamento atualiza novamente a a interface
// com um registro vazio -- estado inicial de entrada da rotina

METHOD CANCEL() CLASS ZMVCVIEW
Local nFldCount
Local oFldDef
Local nFld

// Registra log de cancelamento e qual a ação em andamento 
::oLogger:Write("Cancel","Action="+cValToChar(::nAction))

// Esconde botões de confirmar e cancelar
::oBtnOk:Hide()
::oBtnCanc:Hide()

/// Ação volta ao zero
::nAction = 0

// Preenche os campos com os valores default
nFldCount := len( ::aGets )

For nFld := 1 to nFldCount
	
	// Pega o objeto da definicao deste campo
	oFldDef := ::aGets[nFld][3]
	  
	// Atualiza gets com valor inicial do campo ( vazio ) 
	::aGets[nFld][4] := oFldDef:DefaultValue()

	// Desabilita todos os gets
	::aGets[nFld][2]:SetEnable(.F.)
	
Next

#ifdef HAS_NAVBUTTONS
	// Mostra novamente botoes de navegação
	aEval (::aBtnNav , {|x| x:Show() } )
#endif

// Habilita todos od botoes novo e inclusao
aEval( ::aBtnAct , {|x| x:SetEnable(.T.)  })

// Porem desabilita update e delete 
::aBtnAct[3]:SETENABLE(.F.)
::aBtnAct[4]:SETENABLE(.F.)

#ifdef HAS_NAVBUTTONS
// Esconde os botoes de navegação
aEval (::aBtnNav , {|x| x:Hide() } )
#endif

Return


// ----------------------------------------------------------
// A operação de busca abre a tela em branco com todos os campos
// Para a pesquisa ser feita partindo das informações preenchidas

METHOD SEARCH()      CLASS ZMVCVIEW
Local nFldCount
Local oGet,oFldDef

::oLogger:Write("Search","Open Search Interface")

// Ação em execução = CONSULTA
::nAction := 2

nFldCount := len( ::aGets )

// Habilita todos os GETS e preenche com valor default em branco

For nI := 1 to nFldCount

	oGet := ::aGets[nI][2]
	oFldDef := ::aGets[nI][3]

	// Atualiza valor default 
	::aGets[nI][4] := oFldDef:DefaultValue()

	// Define campos ativos baseados na definicao
	oGet:SetEnable( oFldDef:IsEnabled() )

	// Define se o campo é somente leitura ou nao 
	oGet:LREADONLY := oFldDef:IsReadOnly()

Next

#ifdef HAS_NAVBUTTONS
// Esconde os botões de navegação
aEval (::aBtnNav , {|x| x:Hide() } )
#endif

// Desabilita os botoes de ação
for nI := 1 to 4 
	::aBtnAct[nI]:SetEnable(.F.)
Next

// Mostra botões de confirmar e cancelar
::oBtnOk:Show()
::oBtnCanc:Show()

Return

#ifdef HAS_NAVBUTTONS

// ==================================================
// Botoes de navegacao 
// Somente sao mostrados quando tem um registro 
// sendo mostrado na tela 
// ==================================================

METHOD GOFIRST()      CLASS ZMVCVIEW
Return MsgStop("*** TODO ***")

METHOD GONEXT()      CLASS ZMVCVIEW
Return MsgStop("*** TODO ***")

METHOD GOPREV()      CLASS ZMVCVIEW
Return MsgStop("*** TODO ***")

METHOD GOLAST()      CLASS ZMVCVIEW
Return MsgStop("*** TODO ***")

#endif

// ==================================================
// ==================================================

METHOD UPDATE()      CLASS ZMVCVIEW
Local nFldCount
Local oGet,oFldDef

::oLogger:Write("Update","Open Update Interface")

// Ação em execução = Alteração 
::nAction := 3

nFldCount := len( ::aGets )

// Habilita todos os GETS baseado na definição

For nI := 1 to nFldCount

	oGet    := ::aGets[nI][2]
	oFldDef := ::aGets[nI][3]

	// Define campos ativos baseados na definicao
	oGet:SetEnable( oFldDef:IsEnabled() )

	// Define se o campo é somente leitura ou nao 
	oGet:LREADONLY := oFldDef:IsReadOnly()

Next

#ifdef HAS_NAVBUTTONS
	// Esconde os botões de navegação
	aEval (::aBtnNav , {|x| x:Hide() } )
#endif

// Desabilita os botoes de ação padrao 
for nI := 1 to 4 
	::aBtnAct[nI]:SetEnable(.F.)
Next

// Mostra botões de confirmar e cancelar
::oBtnOk:Show()
::oBtnCanc:Show()

Return


METHOD DELETE()      CLASS ZMVCVIEW
Return MsgStop("*** TODO ***")

// ----------------------------------------------------------
// Retorna string com ultimo erro 

METHOD GETERRORSTR() CLASS ZMVCVIEW
Return ::cError

// ----------------------------------------------------------
// Seta uma ocorrencia de error 
METHOD SETERROR(cError) CLASS ZMVCVIEW
::cError := cError
Return

// ----------------------------------------------------------
// Limpa ocorrencia de erro 

METHOD ClearError()   CLASS ZMVCVIEW
::cError := ''
Return

// ----------------------------------------------------------
// Pede ao controle para executar uma acao adicional 

METHOD RunAction(nAct) CLASS ZMVCVIEW
Local lOk

::oLogger:Write("RunAction","Act="+cValToChar(nAct)+";Prompt="+::aActions[nAct][1])

lOk := ::oControl:RunAction(nAct)

If !lOk
	MsgStop(::oControl:GetErrorStr(),::aActions[nAct][1])	
Endif

Return lOk

