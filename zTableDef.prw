#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Classe de abstração de definição de Tabela 

====================================================== */

CLASS ZTABLEDEF FROM LONGNAMECLASS

   DATA cDefId
   DATA aStruct 
   DATA aIndex
   DATA aFieldDef
   DATA cUnqExpr
   DATA aEvents

   METHOD New()                        // Construtor 
   METHOD SetStruct()                  // Seta uma estrutura ( Array formato DBF ) 
   METHOD GetStruct()                  // Recupera a estrutura ( Array Formato DBF ) 
   METHOD NewFieldDef()                // Cria um objeto de definicao extendida de campo 
   METHOD GetAFieldDef()               // Retorna o array com a deefinição de todos os campos 
   METHOD AddField()                   // ACrescenta um campo na estrutura 
   METHOD AddFieldDef()                // Acrescenta definição de campo 
   METHOD AddIndex()                   // Acrescenta uma expressao de indice 
   METHOD SetUnique()                  // Seta expressao de chave unica 
   METHOD AddEvent()                   // Acrescenta um evento na definição 
   
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

METHOD GetAFieldDef() CLASS ZTABLEDEF
Return aClone(::aFieldDef)

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


