#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Definição completa de tabela -- Estutura Fisica e Detalhamento de Campos 

( Usada como base para a definição de um componente ) 

====================================================== */

CLASS ZTABLEDEF FROM LONGNAMECLASS

   DATA cDefId
   DATA aStruct 
   DATA aIndex
   DATA aFieldDef
   DATA cUnqExpr
   DATA aEvents
   DATA aActions

   METHOD New()                        // Construtor 
   METHOD SetStruct()                  // Seta uma estrutura ( Array formato DBF ) 
   METHOD GetStruct()                  // Recupera a estrutura ( Array Formato DBF ) 
   METHOD NewFieldDef()                // Cria um objeto de definicao estendida de campo 
   METHOD GetFieldDef()                // Retorna o onjeto da definicao de um campo pelo nome 
   METHOD GetFields()                  // Retorna o array com a deefinição de todos os campos 
   METHOD AddField()                   // ACrescenta um campo na estrutura 
   METHOD AddFieldDef()                // Acrescenta definição de campo 
   METHOD AddIndex()                   // Acrescenta uma expressao de indice 
   METHOD SetUnique()                  // Seta expressao de chave unica 
   METHOD AddEvent()                   // Acrescenta um evento na definição 
   METHOD AddAction()                  // Acrescenta uma ação do componente
   METHOD RunEvents()                  // Executa um ou mais eventos 
   METHOD GetActions()
   METHOD RunAction()
   METHOD Done()                       // Finaliza a definição 
   
ENDCLASS 


// ------------------------------------------------------
// Construtor 
// Recebe o identificador da definição da tabela como parametro

METHOD NEW(cId) CLASS ZTABLEDEF
::cDefId := cId
::aStruct := {}
::aIndex := {}
::aFieldDef := {}
::cUnqExpr := ''
::aEvents := {}
::aActions := {}
Return self

// ------------------------------------------------------

METHOD SetStruct(aStruct) CLASS ZTABLEDEF
::aStruct := aStruct
Return

// ------------------------------------------------------

METHOD GetStruct() CLASS ZTABLEDEF
Return aClone(::aStruct)

// ------------------------------------------------------

METHOD NewFieldDef(cName) CLASS ZTABLEDEF
Local nPos 
Local oFieldDef 
Local aFld

// Busca o campo na estrutura fisica 
nPos := ascan( ::aStruct , {|x| x[1] == cName })
If nPos = 0  
	UserException("ZTABLEDEF:NewFieldDef()-- Field ["+cName+"] not found")
Endif

// Pega os dados da estrutura apenas deste campo 
aFld := ::aStruct[nPos]

// Cria a definicao extendida baseada no campo 
oFieldDef := ZFIELDDEF():NEW( aFld[1] , aFld[2] , aFld[3] , aFld[4] )

Return oFieldDef

// ------------------------------------------------------

METHOD GetFieldDef(cFldName)  CLASS ZTABLEDEF
Local nPos 

cFldName := alltrim(upper(cFldName))

nPos := ascan(::aFieldDef,{|x| x:GetField() == cFldName }) 

If nPos > 0 
	Return ::aFieldDef[nPos]
Endif

Return NIL

// ------------------------------------------------------
// Retorna o array com os objetos da definição dos campos

METHOD GetFields() CLASS ZTABLEDEF
Return ::aFieldDef

// ------------------------------------------------------
// Acrescenta campo na estrutura 
// Parametros Obrigatorios : cName, cType 
// nSize é obrigatorio para campos Caractere e Numéricos 
// nDec é opcional, DEFAULT = 0 

METHOD AddField(cName,cType,nSize,nDec) CLASS ZTABLEDEF
Local nPos

If nDec = NIL ; nDec := 0 ; Endif

cName := alltrim(Upper(cName))
cType := alltrim(Upper(cType))

Do Case
	Case cType = 'D'
		nSize := 8
	Case cType = 'L'
		nSize := 1
	Case cType = 'M'
		nSize := 10
EndCase

// Para campos caractere e numericos, o tamanho é obrigatorio 
If nSize = NIL
	UserException("ZTABLEDEF:AddField()-- Missing Field Size")
Endif

// Busca o campo na estrutura 
nPos := ascan( ::aStruct , {|x| x[1] == cName })
If nPos > 0 
	UserException("ZTABLEDEF:AddField()-- Field ["+cName+"] already exists")
Endif

// Acrescenta o campo no final da estrutura atual 
AADD( ::aStruct , {cName,cType,nSize,nDec} )

Return

// ------------------------------------------------------
// Acrescenta uma definição de campo na estrutura 
// Deve ser um objeto de definicao de campo 

METHOD AddFieldDef(oFldDef) CLASS ZTABLEDEF
AADD( ::aFieldDef  , oFldDef ) 
Return

// ------------------------------------------------------
// Acrescenta uma expressao de indice na tabela 

METHOD AddIndex(cIdxExpr) CLASS ZTABLEDEF
AADD( ::aIndex , cIdxExpr ) 
Return

// ------------------------------------------------------
// Define chave unica 

METHOD SetUnique(cExpr) CLASS ZTABLEDEF
::cUnqExpr := cExpr
Return

// ------------------------------------------------------
// Acrescenta um evento na definição 

METHOD AddEvent(nEvent,bBlock) CLASS ZTABLEDEF
aadd( ::aEvents , {nEvent,bBlock} )
Return


// ------------------------------------------------------
// Executa os eventos registrados sob um identificador 
// A execução sempre recebe o objeto do modelo como parametro 
// A execução dos eventos deve retornar .T. para a aplicação 
// continuar. O primeiro evento que retorne .F. interrompe 
// o processamento do loop de eventos -- caso exista mais de um 

METHOD RunEvents(nEvent,oModel) CLASS ZTABLEDEF
Local nI
Local lOk := .T. 

For nI := 1 to len(::aEvents)
	If ::aEvents[nI][1] == nEvent
		lOk := Eval(::aEvents[nI][2] , oModel )
		IF !lOk
			EXIT
		Endif
	Endif
Next

Return lOk 


// ------------------------------------------------------

METHOD RunAction(nAct,oModel) CLASS ZTABLEDEF
Local lOk
lOk := Eval(::aActions[nAct][2] , oModel )
Return lOk


// ------------------------------------------------------
// Finaliza / Limpa a definicao e suas propriedades

METHOD Done() CLASS ZTABLEDEF

::cDefId     := NIL
::aStruct    := NIL
::aIndex     := NIL
::aFieldDef  := NIL
::cUnqExpr   := NIL
::aEvents    := NIL
::aActions   := NIL

Return

// ------------------------------------------------------
// Acrescenta uma ação do componente

METHOD AddAction(cTitle,bAction) CLASS ZTABLEDEF
AADD( ::aActions , { cTitle,bAction } )
Return

// ------------------------------------------------------
//

METHOD GetActions() CLASS ZTABLEDEF
Return ::aActions

