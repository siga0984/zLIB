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

Definição de Componente baseado em um JSON

===================================================== */

CLASS ZJSONTABLEDEF FROM ZTABLEDEF

  DATA cTitle
  DATA cTable
  DATA oLogger

  METHOD NEW()
  METHOD TableName()
  METHOD GetTitle()

ENDCLASS 

// ------------------------------------------------------
// Cria a definição do componente baseado em um JSON

METHOD NEW(cDefName) CLASS ZJSONTABLEDEF
Local oJson, cJsonStr, cJsonFile
Local aFields , oJField, oFld
Local nI
Local cFldName,cFldType,nFldSize,nFldDec,cLabel,cHint
Local cPict,lReadOnly,lVisible,lRequired

_Super:New(cDefName)

// Definicao em disco tem o nome original + extensao ".json"
cJsonFile := cDefName+'.json'

// Lê a definição do compomente / tabela
cJsonStr := memoread(cJsonFile)

If empty(cJsonStr)
	UserException('Definition file ['+cJsonFile+'] NOT FOUND.')
Endif

// Cria o objeto JSON baseado na definição 
oJson 		:= JsonObject():New()
oJson:fromJson(cJsonStr)

// Cria o Logger .. TODO reavaliar essa abordagem 
::oLogger := ZLOGGER():New('ZJSONTABLEDEF')
::oLogger:Write("NEW","Create Component Definition ["+cDefName+"]")

// Recupera o título da definição
::cTitle := oJson:GetJsonText("title")

// Recupera o nome da tabela 
::cTable := oJson:GetJsonText("tablename")

// Recupera as colunas e propriedades de cada coluna

aFields := oJson:GetJsonObject("fields")

For nI := 1 to len(aFields)

	oJField := aFields[nI]
	
	cFldName := oJField:GetJSonText('name')
	cFldType := oJField:GetJSonText('type')
	nFldSize := val(oJField:GetJSonText('size'))
	nFldDec := val(oJField:GetJSonText('decimal'))
	cLabel := oJField:GetJSonText('label')
	cHint :=  oJField:GetJSonText('hint')
	cPict := oJField:GetJSonText('picture')
    
	lReadOnly := .F.
	if oJField:GetJSonText('readonly') == '.T.'
		lReadOnly := .T.
	Endif
	
	lVisible := .T.
	IF oJField:GetJSonText('visible') == '.F.'
		lVisible := .F.
	Endif

	lRequired := .F.
	If oJField:GetJSonText('required') == '.T.'
		lRequired := .T.
	Endif
            
	IF cFldType == 'M'
		nFldSize := 10 
	ElseIF cFldType == 'D'
		nFldSize := 8
	ElseIF cFldType == 'L'
		nFldSize := 1
	Endif

	IF cFldType <> 'N'
		nFldDec := 0 
	Endif            
	
	IF empty(cLabel)
		cLabel := cFldName
	Endif              

	IF empty(cHint)
		cHint := '(Hint) '+cLabel
	Endif

	oFld := ::AddFieldDef(cFldName,cFldType,nFldSize,nFldDec)
	
	oFld:SetLabel(cLabel,cHint)
	If !empty(cPict)	
		oFld:SetPicture(cPict)
	Endif
	If lRequired
		oFld:SetRequired(.T.)
	Endif

	oFld:SetVisible(lVisible)

	oLookUp := oJField:GetJsonObject("lookup")
	
	IF oLookUp != NIL
		cAuxDef := oLookUp:GetJsonText('auxdef')
		cAuxTab := oLookUp:GetJsonText('table')
		cAuxKey := oLookUp:GetJsonText('key')
		cAuxExpr := oLookUp:GetJsonText('expression')

		::AddAuxDef(cAuxDef)

		oFld:SetLookUp(cAuxTab,cAuxKey,cAuxExpr)
		
	Endif

Next

// Índices da tabela 
// Array de strings com as expressoes... 
aIndex :=  oJson:GetJsonObject("indexes")

IF valtype(aIndex) == 'A'
	For nI := 1 to len(aIndex)
		::AddIndex(aIndex[nI])
	Next
Endif

// Expressao para índice único
cUnqIdx := oJson:GetJSonText('uniqueindex')
If !empty(cUnqIdx)
	::SetUnique(cUnqIdx)
Endif
 
// Açoes default - componentizadas

aActions :=  oJson:GetJsonObject("actions")

IF valtype(aActions) == 'A'
	For nI := 1 to len(aActions)
		IF aActions[nI] == "SEARCH"
			::AddAction("SEARCH","&Pesquisar")
		ElseIF aActions[nI] == "INSERT"
			::AddAction("INSERT","&Inserir")
		ElseIF aActions[nI] == "UPDATE"
			::AddAction("UPDATE","&Alterar")
		Endif
	Next
Endif

// Todo - Ações customizadas

Return self

// ----------------------------------------------------------

METHOD TableName() CLASS ZJSONTABLEDEF 
Return ::cTable

// ----------------------------------------------------------

METHOD GetTitle() CLASS ZJSONTABLEDEF 
Return ::cTitle

