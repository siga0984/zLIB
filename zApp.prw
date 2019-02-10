#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Classe de Abstração de Aplicação ZLIB AdvPL 

Projetada inicialmente para SmartClient 

====================================================== */

CLASS ZAPP FROM LONGNAMECLASS

   DATA oAppInterface
   DATA oAppObject
   
   METHOD New() 					// Construtor 
   METHOD SetInterface()            // Seta o objeto de interface
   METHOD SetObject()				// Seta o objeto de dados 
   METHOD Run()						// Roda a Aplicação 
   METHOD Done()                    // Finaliza os objetos 
  
ENDCLASS


// ------------------------------------------------------
// Construtor

METHOD NEW() CLASS ZAPP
Return self


// ------------------------------------------------------
// Seta o objeto de interface da aplicação 

METHOD SetInterface(oIntf) CLASS ZAPP
::oAppInterface := oIntf
Return

// ------------------------------------------------------
// Seta o objeto de controle da aplicação 

METHOD SetObject(oAppObj) CLASS ZAPP
::oAppObject := oAppObj
Return

// ------------------------------------------------------
// Executor da aplicação 
// Faz a inicializaçào do objeto de controle 
// Em caso de sucesso, amarra o objeto 
// na interface e roda a interface

METHOD Run() CLASS ZAPP

If ::oAppObject = NIL 
	UserException("ZAPP:Run() -- AppObject NOT SET ")
Endif

If ::oAppInterface = NIL 
	UserException("ZAPP:Run() -- AppInterface NOT SET ")
Endif

If ::oAppObject:Init()

	// Passou da inicialização 
	// Amarra o objeto e roda a interface 
	
	::oAppInterface:SetObject(::oAppObject)
	::oAppInterface:Run()

Else
	
	MsgStop( ::oAppObject:GetErrorStr() , "Failed to INIT AppObject" )

Endif

Return


// ------------------------------------------------------
// Finalizador / Desrtrutor 
// Limpa os objetos amarrados ao ZAPP 

METHOD DONE() CLASS ZAPP

IF ::oAppInterface != NIL 
	::oAppInterface:Done()
	FreeObj(::oAppInterface)
Endif

If ::oAppObject != NIL 
	::oAppObject:Done()
	FreeObj(::oAppObject)
Endif

Return

