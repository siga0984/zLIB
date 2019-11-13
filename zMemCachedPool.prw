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

