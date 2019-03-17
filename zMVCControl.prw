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
   DATA aModels
   DATA oModel
   DATA oView
   DATA cError
   DATA oLogger                  // Objeto de log 

   METHOD NEW()
   METHOD AddModel()
   METHOD SetModel()
   METHOD GetModel()
   METHOD SetZLibEnv()
   METHOD GetZLibEnv()
   METHOD GetObjectDef()
   METHOD GetErrorStr()
   METHOD SetError()
   METHOD ClearError()

   METHOD Done()
   METHOD GetActions()
   METHOD RunAction()

   METHOD NewRecord()
   METHOD Write()
   METHOD Search() 
   METHOD Update() 
   METHOD GetData()

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

