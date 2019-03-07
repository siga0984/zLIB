#include 'protheus.ch'
#include 'zlib.ch' 

/* ======================================================

Classe de Abstração de Aplicação ZLIB AdvPL 

Projetada inicialmente para SmartClient 

====================================================== */

CLASS ZAPP FROM LONGNAMECLASS

   DATA oRunObject
   
   METHOD New() 					// Construtor 
   METHOD Run()						// Roda a Aplicação 
   METHOD Done()                    // Finaliza os objetos 
  
ENDCLASS


// ------------------------------------------------------
// Construtor

METHOD NEW() CLASS ZAPP
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

