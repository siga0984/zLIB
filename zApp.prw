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
   METHOD RunMVC()
  
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
//

METHOD SetMainDef(cDefName)  CLASS ZAPP
::cMainDef := cDefName
Return

// ------------------------------------------------------
//


// ------------------------------------------------------
//

METHOD RunMVC(cDefName,aCoords) CLASS ZAPP
Local oMVCDef 
Local oDefFactory
Local oModelFactory
Local oViewFactory
Local oCtrlFactory   
Local aAuxDefs := {}

// Pega os factories do MVC
oDefFactory := ::oEnv:GetObject("ZDEFFACTORY")
oModelFactory := ::oEnv:GetObject("ZMODELFACTORY")
oViewFactory := ::oEnv:GetObject("ZVIEWFACTORY")
oCtrlFactory := ::oEnv:GetObject("ZCONTROLFACTORY")

// Cria a definição do componente
// Futuramente será possivel obter a definição do dicionário de dados 
oMVCDef := oDefFactory:GetNewDef(cDefName)

// Acrescenta definições auxiliares no array 
aEval( oMVCDef:GetAuxDefs() ,{|x| aadd(aAuxDefs,x ) })

// Cria o objeto de Modelo da Banco
// Passa a definição como parâmetro
oMVCModel := oModelFactory:GetNewModel(oMVCDef)

// Na inicialização precisa passar o ambiente 
If !oMVCModel:Init( ::oEnv )
	MsgStop( oMVCModel:GetErrorStr() , "Failed to Init Model" )
	Return 
Endif

// Cria a View a partir da definição
oMVCView := oViewFactory:GetNewView(oMVCDef)

IF aCoords != NIL
	// Top, left, bottom, right
	oMVCView:SetCoords(aCoords)
Endif

// Cria o controle 
// Por enquanto ele faz a ponte direta entre a View e o Modelo 
// Os eventos atomicos da view ficam na View, apenas 
// os macro eventos sao repassados  

oMVCCtrl := oCtrlFactory:GetNewControl(oMVCView)
oMVCCtrl:AddModel(oMVCModel)

While len(aAuxDefs) > 0 

   // Cria definicoes e modelos auxiliares para acrescentar ao contoler

   cDefName := aAuxDefs[1]
   aDel(aAuxDefs,1)
   aSize(aAuxDefs,len(aAuxDefs)-1)
   
   oMVCDef := oDefFactory:GetNewDef(cDefName)
   aEval( oMVCDef:GetAuxDefs() ,{|x| aadd(aAuxDefs,x ) })
   
   oMVCModel := oModelFactory:GetNewModel(oMVCDef)
   oMVCModel:Init( ::oEnv )

   oMVCCtrl:AddModel(oMVCModel)

Enddo

// Roda a View 
oMVCView:Run()

Return
