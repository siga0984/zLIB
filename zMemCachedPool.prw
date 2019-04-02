#include "protheus.ch"

/* ==============================================================================
Classe      ZMEMCACHEDPOOL
Autor       Julio Wittwer
Data        01/2019
Descrição   Encapsula objeto client do MemCache, usando contador de referencias. 

Os programas que consomem o cache devem obter a instância usando :

   ZMEMCACHEDPOOL():GetCache( @oMemCache , @cError )

E após o uso, fazer release da instancia usando : 

   ZMEMCACHEDPOOL():ReleaseCache( @oMemCache )

==============================================================================*/


STATIC _oMemCache        // Objeto do Cache em  "Cache"
STATIC _nRefCount := 0   // Contador de referencias 

CLASS ZMEMCACHEDPOOL FROM LONGNAMECLASS

   METHOD GetCache()
   METHOD ReleaseCache()
   METHOD RefCount() 
   
ENDCLASS

// ----------------------------------------------------
// Obtem por referencia uma instancia do cache, e em caso de
// falha, obtem o erro também por refeência 

METHOD GetCache( oCache , cError ) CLASS ZMEMCACHEDPOOL

// Inicializa parametros passados por referência
oCache := NIL
cError := ""

IF _oMemCache != NIL
	// Já tenho um objeto de conexao 
	// Verifico se a conexão está OK
	IF _oMemCache:IsConnected()
		// Conexão OK, incremento contador de referencias
		// e retorno 
		_nRefCount++
		oCache := _oMemCache
	Else
		// A conexão não está OK
		// Limpa o objeto e passa para a proxima parte 
		FreeObj(_oMemCache)
		_oMemCache := NIL
	Endif
Endif

IF _oMemCache == NIL
	// Nao tenho o objeto de conexao
	// Crio o objeto e tento conectar 
	_oMemCache := ZMEMCACHED():New("localhost",11211)
	IF _oMemCache:Connect()
		// Conexão OK,incrementa contador e atribui o objeto 
		_nRefCount++
		oCache := _oMemCache
	Else
		// Nao conectou, recupera o erro, alimenta cError 
		// e mata este objeto
		cError := _oMemCache:GetErrorStr()
		FreeObj(_oMemCache)
		_oMemCache := NIL
	Endif
Endif

Return 


// ----------------------------------------------------
// Solta a referência do cache em uso, anula a variavel 
// recebida por referencia, e caso o contador 
// seja menor que um, limpa o objeto da memória 
METHOD ReleaseCache( oCache ) CLASS ZMEMCACHEDPOOL

IF oCache != NIL 
	oCache := NIL
	_nRefCount--
	IF _nRefCount < 1 
		_oMemCache:Disconnect()
		FreeObj(_oMemCache)
		_oMemCache := NIL
	Endif
Endif

Return 


// ----------------------------------------------------
// Retorna o contador de referencias de uso 
// do objeto do Cache 

METHOD RefCount() CLASS ZMEMCACHEDPOOL
Return _nRefCount

