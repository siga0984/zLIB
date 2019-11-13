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
   DATA lUseCache
   DATA aAuxDefs

   METHOD New()                        // Construtor 
   METHOD AddAuxDef()
   METHOD GetAuxDefs()
   METHOD SetStruct()                  // Seta uma estrutura ( Array formato DBF ) 
   METHOD GetStruct()                  // Recupera a estrutura ( Array Formato DBF ) 
   METHOD GetIndex()                   // Recupera a definicao de indices 
   METHOD AddFieldDef()                // Acrescenta um campo e sua definição estendida
   METHOD GetFieldDef()                // Retorna o onjeto da definicao de um campo pelo nome 
   METHOD GetFields()                  // Retorna o array com a deefinição de todos os campos 
   METHOD AddField()                   // Acrescenta um campo na estrutura 
   METHOD AddIndex()                   // Acrescenta uma expressao de indice 
   METHOD SetUnique()                  // Seta expressao de chave unica 
   METHOD SetUseCache()                // Seta uso de cache para os dados 
   METHOD GetUseCache()                // Recupera definicao de uso de cache
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
::lUseCache := .F. 
::aAuxDefs  := {}
Return self

// ------------------------------------------------------
METHOD AddAuxDef(cDefName) CLASS ZTABLEDEF
aadd(::aAuxDefs,cDefName)
Return

// ------------------------------------------------------
METHOD GetAuxDefs() CLASS ZTABLEDEF
Return ::aAuxDefs


// ------------------------------------------------------

METHOD SetStruct(aStruct) CLASS ZTABLEDEF
::aStruct := aStruct
Return

// ------------------------------------------------------

METHOD GetStruct() CLASS ZTABLEDEF
Return aClone(::aStruct)

// ------------------------------------------------------

METHOD GetIndex() CLASS ZTABLEDEF
Return aClone(::aIndex)

// ------------------------------------------------------
// Cria e acrescenta objeto da definição estendida de campo 

METHOD AddFieldDef(cName,cType,nSize,nDec) CLASS ZTABLEDEF
Local oFieldDef 

// Acrescenta o campo na estrutura fisica
::AddField(cName,cType,nSize,nDec)

// Cria a definicao extendida baseada no campo 
oFieldDef := ZFIELDDEF():NEW( cName,cType,nSize,nDec )

// Acrescenta na definição
AADD( ::aFieldDef  , oFieldDef ) 

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
// Seta uso de cache para os dados 

METHOD SetUseCache(lSet) CLASS ZTABLEDEF
::lUseCache := lSet
Return

// ------------------------------------------------------
// Recupera definicao de uso de cache

METHOD GetUseCache() CLASS ZTABLEDEF
Return ::lUseCache

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

METHOD RunAction(cAction,oModel) CLASS ZTABLEDEF
Local lOk := .F.
Local nPos := ascan(::aActions,{|x| x[1] == cAction})
If nPos > 0 
	lOk := Eval(::aActions[nPos][3] , oModel )
Else
	oModel:SEtError("RunAction Failed - Action ["+cAction+"] not found.")
Endif
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

METHOD AddAction(cName,cTitle,bAction) CLASS ZTABLEDEF  
AADD( ::aActions , { cName,cTitle,bAction } )
Return

// ------------------------------------------------------
// Array de ações 
// [1] Nome 
// [2] Label 
// [3] CodeBlock
// Se a ação for default / reservada, ela apenas vai ter 
// o nome preenchido, e as demais colunas "NIL"

METHOD GetActions() CLASS ZTABLEDEF
Return ::aActions

