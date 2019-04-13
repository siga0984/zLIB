#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Classe de Abstração de Aplicação ZLIB AdvPL 

Projetada inicialmente para SmartClient 

====================================================== */

CLASS ZAPP FROM LONGNAMECLASS

   DATA oEnv
   DATA oRunObject
   DATA cMainDef
   DATA aDefs
   DATA aModels
   DATA aControls
   
   METHOD New() 					// Construtor 
   METHOD Run()						// Roda a Aplicação 
   METHOD Done()                    // Finaliza os objetos 
   
   METHOD SetMainDef()
   METHOD AddAuxDef()
   METHOD BuildMVC()
  
ENDCLASS


// ------------------------------------------------------
// Construtor

METHOD NEW(_oEnv) CLASS ZAPP

::oEnv      := _oEnv
::aDefs     := {}
::aModels   := {}
::aControls := {}

Return self


// ------------------------------------------------------
// Executor da aplicação
// Recebe o componente a ser executado 

METHOD Run(oRunObject) CLASS ZAPP

::oRunObject := oRunObject

If ::oRunObject = NIL
	UserException("ZAPP:Run() -- AppInterface NOT SET ")
Endif

::oRunObject:Run()

Return


// ------------------------------------------------------
// Finalizador / Desrtrutor

METHOD DONE() CLASS ZAPP

Return

// ------------------------------------------------------

METHOD SetMainDef(cDefName)  CLASS ZAPP
::cMainDef := cDefName
Return                


METHOD AddAuxDef() CLASS ZAPP
Return

METHOD BuildMVC() CLASS ZAPP
Return
