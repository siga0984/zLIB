#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Classe de abstração de definição extendida de campos  da Tabela 

Replica as informações da estrutura, mas armazena informações
adicionais para a montagem de qualquer interface. Label, Descrição, 
Picture (mascara) de entrada de dados.

Por hora os eventos são colocados apenas na definição do arquivo 

====================================================== */

CLASS ZFIELDDEF FROM LONGNAMECLASS

   DATA cField                         // Nome do campo 
   DATA cType                          // Tipo AdvPL do campo 
   DATA nSize                          // Tamanho do campo 
   DATA nDec                           // Numero de decimais 
   DATA cLabel                         // Label ou descrição curta do campo 
   DATA cDescr                         // Descrição longa do campo 
   DATA cPicture                       // Mascara de dados do campo 
   DATA lRequired                      // Campo de preenchimento Requiredório -- nao pode estar vazio 
   DATA lEnabled                       // Indica se o campo habilitado 
   DATA lVisible                       // Indica se o campo está visivel 
   DATA lReadOnly                      // Indica se o campo somente pode ser lido -- mas nao editado \
   DATA lVirtual                       // Indica se o campo é virtual 
   DATA cLookTab                       // Tabela da chave estrangeira do campo  
   DATA cLookKey                       // Campo Chave da Chave estrangeira
   DATA cLookFld                       // Campo ou expressao relacionada a tabela estrangeira 

   METHOD New()                        // Construtor 
   METHOD SetLabel()                   // Informa label e descrição do campo 

   METHOD SetLookUp()                  // Informa tabela , chave e campo a recuperar 
   METHOD GetLookTable()               // Tabela da chave estrangeira 
   METHOD GetLookKey()                 // Chave da Chave Estrangeira 
   METHOD GetLookField()               // Campo ou Expressao da Tabela Estrangeira 

   METHOD SetPicture()                 // Coloca máscara no campo
   METHOD GetField()                   // Recupera o nome do campo 
   METHOD GetLabel()                   // Recupera o label informado 
   METHOD GetDescr()                   // Recupera a descrição longa do campo 
   METHOD GetPicture()                 // Recupera máscara no campo
   METHOD GetType()                    // Recupera o Tipo AdvPL do campo 
   METHOD GetSize()                    // Recupera o tamanho do campo 
   METHOD GetDec()                     // Recupera o numero de casas decimais do campo 
   METHOD DefaultValue()               // Recupera o valor do cammpo "Vazio" de acordo com o tipo 
   METHOD SetEnabled()                 // Seta se o campo está habilitado 
   METHOD SetVisible()                 // Seta se o campo está visivel 
   METHOD SetReadOnly()                // Seta se o campo é somente leitura ( nao editavel ) 
   METHOD SetVirtual()                 // Seta se o campo é virtual 
   METHOD SetRequired()                // Seta se o campo é de preenchimento Requiredório 
   METHOD IsEnabled()                  // Consulta se o campo está habilitado 
   METHOD IsVisible()                  // Consulta se o campo está visivel 
   METHOD IsReadOnly()                 // Consulta se o campo somente pode ser lido ( não editável ) 
   METHOD IsRequired()                 // Retorna se o campo é de preencimento Requiredorio 
   METHOD IsVirtual()                  // Retorna se o campo é virtual 
   
ENDCLASS 


// ------------------------------------------------------
// Construtor 
// Recebe o nome do campo 

METHOD NEW(cFld,cType,nSize,nDec) CLASS ZFIELDDEF
::cField     := cFld
::cType      := cType
::nSize      := nSize
::nDec       := nDec
::cLabel     := ''
::cDescr     := ''
::cPicture   := ''
::lRequired   := .F. 
::lEnabled   := .T. 
::lVisible   := .T. 
::lReadOnly  := .F. 
::lVirtual   := .F. 
::cLookTab := ""
::cLookKey := ""
::cLookFld := ""

Return self


// ------------------------------------------------------

METHOD SetLabel(cLabel,cDescr) CLASS ZFIELDDEF
::cLabel := cLabel
::cDescr := cDescr
Return

// ------------------------------------------------------
// Seta uma máscara de entrada para o campo 

METHOD SetPicture(cPict) CLASS ZFIELDDEF
::cPicture := cPict
Return

// ------------------------------------------------------
// Seta Tabela e campo da chave estrangeira 
// O campo normalmente é um UNIQUE INDEX 

METHOD SetLookUp(cFTable,cFKey,cFField) CLASS ZFIELDDEF  
::cLookTab := cFTable
::cLookKey := cFKey
::cLookFld := cFField
Return

// ------------------------------------------------------
// Recupera tabela da chave estrangeira 

METHOD GetLookTable() CLASS ZFIELDDEF  
Return ::cLookTab

// ------------------------------------------------------
// Recupera campo da chave estrangeira 

METHOD GetLookKey() CLASS ZFIELDDEF  
Return ::cLookKey

// ------------------------------------------------------
// Informacao a ser mostrada do relacionamento 

METHOD GetLookField() CLASS ZFIELDDEF  
Return ::cLookFld

// ------------------------------------------------------
// Recupera o nome do campo 

METHOD GetField() CLASS ZFIELDDEF  
Return ::cField

// ------------------------------------------------------
// Recupera o laber ou descrição curta do campo 

METHOD GetLabel() CLASS ZFIELDDEF  
Return ::cLabel

// ------------------------------------------------------
// Recupera a descrição longa do campo 

METHOD GetDescr() CLASS ZFIELDDEF
Return ::cDescr

// ------------------------------------------------------
// Recupera a mascara de entrada de dados do campo 

METHOD GetPicture(cPict) CLASS ZFIELDDEF
Return ::cPicture

// ------------------------------------------------------
// Recupera o tipo do campo em AdvPL 

METHOD GetType() CLASS ZFIELDDEF
Return ::cType 

// ------------------------------------------------------
// Recupera o tamanho do campo em AdvPL 

METHOD GetSize() CLASS ZFIELDDEF
Return ::nSize

// ------------------------------------------------------
// Recupera o numero de decimais para campo numerico 

METHOD GetDec() CLASS ZFIELDDEF
Return ::nDec

// ------------------------------------------------------
// Recupera o valor default do campo vazio
// [c] Caractere com espacos em branco 
// [d] data vazia
// [n] zero
// [l] falso 
// [m] String vazia 

METHOD DefaultValue() CLASS ZFIELDDEF

If ::cType  = 'C'
	Return Space(::nSize)
ElseIF ::cType  = 'N'
	Return 0
ElseIF ::cType  = 'D'
	Return CTOD("")
ElseIF ::cType  = 'L'
	Return .T. 
ElseIF ::cType  = 'M'
	Return ""
Endif

Return NIL 


// ------------------------------------------------------
// Seta se o campo é de preenchimento obrigatorio 
// ( nao pode ser vazio ) 

METHOD SetRequired(lSet)  CLASS ZFIELDDEF
::lRequired := lSet
Return

// ------------------------------------------------------
// Recupera o flag de Obrigatorio

METHOD IsRequired() CLASS ZFIELDDEF
Return ::lRequired


// ------------------------------------------------------

METHOD SetEnabled(lSet) CLASS ZFIELDDEF
::lEnabled   := lSet
Return


// ------------------------------------------------------

METHOD SetVisible(lSet) CLASS ZFIELDDEF
::lVisible   := lSet
Return

// ------------------------------------------------------

METHOD SetReadOnly(lSet) CLASS ZFIELDDEF
::lReadOnly   := lSet
Return

// ------------------------------------------------------

METHOD SetVirtual(lSet) CLASS ZFIELDDEF
::lVirtual   := lSet
Return


// ------------------------------------------------------

METHOD IsEnabled() CLASS ZFIELDDEF
Return ::lEnabled


// ------------------------------------------------------

METHOD IsVisible()  CLASS ZFIELDDEF
Return ::lVisible


// ------------------------------------------------------

METHOD IsReadOnly() CLASS ZFIELDDEF
Return ::lReadOnly

// ------------------------------------------------------

METHOD IsVirtual() CLASS ZFIELDDEF
Return ::lVirtual


