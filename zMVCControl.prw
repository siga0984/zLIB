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

Classe de Controle DEFAULT

Controller Layer:

- It acts as an interface between View and Model.
- It intercepts all the requests which are coming from the view layer.
- It receives the requests from the view layer and processes the requests and 
  does the necessary validation for the request.
- This requests is further sent to model layer for data processing, and once the request is processed, 
  it sends back to the controller with required information and displayed accordingly by the view.

====================================================== */

CLASS ZMVCCONTROL FROM LONGNAMECLASS

   DATA oEnv                     // Environment em uso 
   DATA aModels                  // Modelos instanciados pelo Controler
   DATA oModel                   // Modelo atualmente em uso 
   DATA oView                    // View que está usando este controler
   DATA cError                   // Ultimo erro registrado 
   DATA oLogger                  // Objeto de log 

   METHOD NEW()                  // Cria o controller
   METHOD AddModel()             // Acrescenta um modelo 
   METHOD SetModel()             // Seta o modelo ativo a partir do nome da tabela 
   METHOD GetModel()             // Recupera a tabela do modelo ativo 
   METHOD SetZLibEnv()           // Seta o ambiente para uso da classe
   METHOD GetZLibEnv()           // Recupera o ambiente em uso 
   METHOD GetObjectDef()         // Recupera a definição do objeto do modelo em uso 
   METHOD GetErrorStr()          // Recupera string com o ultimo erro 
   METHOD SetError()             // Seta ocorrencia de erro na instancia atual 
   METHOD ClearError()           // Limpa registro de ultimo erro 

   METHOD Done()                 // Encerra o uso do Controler
   METHOD GetActions()           // Recupera as ações do modelo / definicao em uso 
   METHOD RunAction()            // Executa uma ação do modelo pelo nome 

   METHOD NewRecord()            // Cria um novo registro em branco no modelo atual 
   METHOD Write()                // Insere um novo registro no modelo em uso 
   METHOD Search()               // Realiza uma busca no modelo em uso 
   METHOD Update()               // Atualiza um registro do modelo em uso 
   METHOD GetData()              // Recupera todos os dados da tabela do modelo em uso 

ENDCLASS 


// ------------------------------------------------------
// Construtor do Controle
// Recebe a view ?!

METHOD NEW(oView) CLASS ZMVCCONTROL

::oModel  := NIL
::oView   := oView
::cError  := ''
::aModels := {}

// Informa o Controler para a View 
::oView:SetControl(self)

::oLogger := ZLOGGER():New("ZMVCCONTROL")
::oLogger:Write("NEW","Contol based on View ["+GetClassName(oView)+"]" )

Return self


// ----------------------------------------------------------
// Acrescenta um modelo de controle

METHOD AddModel( oModel ) CLASS ZMVCCONTROL
::oLogger:Write("AddModel","Add Model ["+GetClassName(oModel)+"]" )
AADD( ::aModels , oModel )
If ::oModel = NIL
	::oLogger:Write("AddModel","Main Model id ["+GetClassName(oModel)+"]" )
	::oModel := oModel
Endif
Return


// ----------------------------------------------------------
// Troca o modelo ativo 

METHOD SetModel(_cTable) CLASS ZMVCCONTROL
Local nPos 
::oLogger:Write("SetModel","Table="+_cTable )
nPos := ascan( ::aModels , {|x| x:cTable == _cTable } )
If nPos > 0 
	If ::oModel != ::aModels[nPos]
		::oModel := ::aModels[nPos]
		::oLogger:Write("SetModel","Change model to ["+_cTable+"]" )
	Endif
Else
	UserException("ZMVCCONTROL:SetModel() ERROR - Model ["+_cTable+"] not found")
Endif
Return

// ----------------------------------------------------------
// Retorna a tabela do modelo ativo 

METHOD GetModel()   CLASS ZMVCCONTROL
Local cTabModel := ::oModel:cTable
::oLogger:Write("GetModel","Current Model is ["+cTabModel+"]" )
Return cTabModel
   
// ----------------------------------------------------------
// Seta o ambiente em uso no processo atual 
METHOD SetZLibEnv(oEnv) CLASS ZMVCCONTROL
::oLogger:Write("SetZLibEnv")
::oEnv := oEnv
Return

// ----------------------------------------------------------
// Retorna o objeto do ambiente do processo atual 

METHOD GetZLIBEnv() CLASS ZMVCCONTROL
::oLogger:Write("GetZLIBEnv")
Return ::oEnv

// ----------------------------------------------------------
// Pede para o modelo a definição do componente

METHOD GetObjectDef() CLASS ZMVCCONTROL
::oLogger:Write("GetObjectDef")
Return ::oModel:GetObjectDef()

// ----------------------------------------------------------
// Finalizacao do controle

METHOD Done() CLASS ZMVCCONTROL
::oLogger:Write("Done")
Return 

// ----------------------------------------------------------
// Cria um registro vazio com valores default para inserção 
// Alimenta aRecord por referencia no formato [1] campo [2] conteudo 

METHOD NewRecord(aRecord)  CLASS ZMVCCONTROL
Local lRet

::oLogger:Write("NewRecord")

lRet := ::oModel:NewRecord(aRecord)
::cError := ::oModel:GetErrorStr() 
Return lRet

// ----------------------------------------------------------
// Inserção de novo registro 
// Endereça o modelo para fazer a escrita 
// Informa o registro a inserir em um array 
// no formato [1] campo [2] conteudo 

METHOD Write(aRecord) CLASS ZMVCCONTROL
Local lRet

::oLogger:Write("Write")

lRet := ::oModel:Write(aRecord)
::cError := ::oModel:GetErrorStr()
Return lRet

// ----------------------------------------------------------
// Atualização de registro 

METHOD Update(aRecord) CLASS ZMVCCONTROL
Local lRet

::oLogger:Write("Update")

lRet := ::oModel:Update(aRecord)
::cError := ::oModel:GetErrorStr()
Return lRet

// ----------------------------------------------------------
// Retorna os dados da tabela através do modelo 
// Por hora retorna TUDO 

METHOD GetData(aCols,aData) CLASS ZMVCCONTROL
Local lRet

::oLogger:Write("GetData")

lRet := ::oModel:GetData(aCols,aData)
::cError := ::oModel:GetErrorStr()

Return lRet

// ----------------------------------------------------------
// Recupera ultima ocorrencia de erro 

METHOD GetErrorStr()  CLASS ZMVCCONTROL
Return ::cError


// ----------------------------------------------------------
// Seta ocorrencia de erro 

METHOD SetError(cError) CLASS ZMVCCONTROL
::cError := cError
::oLogger:Write("SetError",cError)
Return

// ----------------------------------------------------------
// Limpa o registro de ocorrencia de erro 

METHOD ClearError() CLASS ZMVCCONTROL
::cError := ''
Return


// ----------------------------------------------------------
// Operação de busca de dados 
METHOD Search(aRecord,aFound,lExact) CLASS ZMVCCONTROL
Local lRet

::oLogger:Write("Search")

lRet := ::oModel:Search(aRecord,aFound,lExact)
::cError := ::oModel:GetErrorStr()
Return lRet

// ----------------------------------------------------------
// Retorna as ações do componente / Modelo

METHOD GetActions() CLASS ZMVCCONTROL
::oLogger:Write("GetActions")
Return ::oModel:GetObjectDef():GetActions()


// ----------------------------------------------------------
// Executa uma ação nomeada do componente / Modelo

METHOD RunAction(cAction) CLASS ZMVCCONTROL
Local lOk 
::oLogger:Write("RunAction","Action="+cAction)
::ClearError()
lOk := ::oModel:RunAction(cAction)
If !lOk
	::cError := ::oModel:GetErrorStr()
Endif
Return lOk

