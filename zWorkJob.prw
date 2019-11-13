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



#include "Protheus.ch"
#include "ZLib.ch"

// =====================================================================
// Working threads de Trabalho 
// Podem / Devem ser configuradas no INI como Threads de ONSTART 
// Trabalham via IPC, e permitem espera de retorno ou não 
// =====================================================================

USER Function zWorkJob()
Local cFnCall , nFnParms, cIPCRet
Local p1,p2,p3,p4,p5,p6,p7,p8,p9,p10
Local nTimer
Local cIPCJobID := UPPER(getenvserver()+"_ZJOB_THREAD")
Local cMsg
Local nLastCall := seconds()
Local nIdleTime := 0

JobLog("Begin Working Job")

While !killapp()

	nIdleTime := seconds()-nLastCall
	IF 	nIdleTime < 0 
		nIdleTime += 86400 
	Endif
	
	SetMntObs("Working Job IDLE ( "+cValToChar(nIdleTime)+" s.)  ")
	
	// Cada processo fica em espera de chamada por 5 segundos 
	
	If IpcWaitEx(cIPCJobID,5000,@cFnCall,@nFnParms,@cIPCRet,@p1,@p2,@p3,@p4,@p5,@p6,@p7,@p8,@p9,@p10)

		// Atualiza timer de processo IDLE 
		nLastCall := seconds()

		// Monta mensagem de requisição em execução 				
		cMsg := "Running "+cFnCall
		If !empty(cIPCRet)
			cMsg += " (Return to "+cIPCRet+")"
		Endif
		cMsg += " - Start "+time()

		// Cria LOG no JOB e seta observações do Monitor		
		JobLog(cMsg)
		SetMntObs(cMsg)
		
		/* -----------------------------------------------------------------------------
		A sequencia de IFs abaixo é para ser possivel a chamada de funções
		que possual algum tipo de crítica de acordo com o número de parâmetros informados
		-- Embora isso nao seja efetivamente recomendado em um chamado remoto ou em job
		----------------------------------------------------------------------------- */
		
		// Marca tempo de inicio
		nTimer := seconds()
		
		if nFnParms == 0
			xRet := &cFnCall.()
		Elseif nFnParms == 1
			xRet := &cFnCall.(p1)
		Elseif nFnParms == 2
			xRet := &cFnCall.(p1,p2)
		Elseif nFnParms == 3
			xRet := &cFnCall.(p1,p2,p3)
		Elseif nFnParms == 4
			xRet := &cFnCall.(p1,p2,p3,p4)
		Elseif nFnParms == 5
			xRet := &cFnCall.(p1,p2,p3,p4,p5)
		Elseif nFnParms == 6
			xRet := &cFnCall.(p1,p2,p3,p4,p5,p6)
		Elseif nFnParms == 7
			xRet := &cFnCall.(p1,p2,p3,p4,p5,p6,p7)
		Elseif nFnParms == 8
			xRet := &cFnCall.(p1,p2,p3,p4,p5,p6,p7,p8)
		Elseif nFnParms == 9
			xRet := &cFnCall.(p1,p2,p3,p4,p5,p6,p7,p8,p9)
		Elseif nFnParms >= 10
			xRet := &cFnCall.(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
		Endif
		
		// ----------------------------------------------------
		// Contabiliza o tempo da chamada
		// Caso o tempo fique negativo, virou a meia-noite
		// ----------------------------------------------------
		
		nTimer := seconds() - nTimer
		if nTimer < 0
			nTimer += 86400
		Endif
		
		// Registra a execução no log do job 
		JobLog("Call "+cFnCall+" spent "+alltrim(str(nTimer,12,3))+' s.')
		
		If !empty(cIPCRet)

			// Existe IPC para retorno -- chamada síncrona
			// Tenta enviar duas vezes ... senao, desiste 
			
			IF !IpcGo(cIPCRet,xRet)
				Sleep(100)
				If !IpcGo(cIPCRet,xRet)
					JobLog("Send Response Failed to ["+cIPCRet+"]")
				Endif
			Endif
			
		Endif

		// --------------------------
		// Limpa / anula variaveis de retorno 
		// e parametros após a execução 
		
		xRet := NIL 
		p1 := NIL 
		p2 := NIL 
		p3 := NIL 
		p4 := NIL 
		p5 := NIL 
		p6 := NIL 
		p7 := NIL 
		p8 := NIL 
		p9 := NIL 
		p10 := NIL 
		
	Endif
	
Enddo

JobLog("End Working Job "+IIF(Killapp(),"[KILLED]",'[TERMINATED]'))

Return .T.
         
/* ==========================================================
Encapsulamento para chamada de WorkJobs síncrona e assíncrona 
========================================================== */

CLASS ZWORKJOB FROM LONGNAMECLASS

   // Timeout de recebimento de chamada síncrona ( em segundos / defaut = 5 ) 
   DATA nTimeOut

   METHOD New()
   METHOD SetTimeOut()
   METHOD GetTimeOut()
   METHOD CallSync()
   METHOD CallASync()

ENDCLASS


// ==================================================================================
METHOD NEW() CLASS ZWORKJOB
::nTimeOut := 5 
return self

// ==================================================================================
// Chamada Assíncrona : Nao espera nem recupera retorno 
// Retorna .T. caso a chamada tenha sido despachada 
// Retorna .F. caso nenhum job pegou a chamada para processar 
// ==================================================================================

METHOD CallASync(cFnCall,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZWORKJOB
Local cIPCJobID := UPPER(getenvserver()+"_ZJOB_THREAD")
Local nFnParms := pCount()-1
Local lCalled

lCalled := IpcGo(cIPCJobID,cFnCall,nFnParms,'',p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

Return lCalled

// ==================================================================================
// Chamada Síncrona : Espera até 5 segundos pelo retorno 
// Retorna array com 3 posicoes:
// [1] Se .T. , requisicao foi despachada 
// [2] Se .T. , houve retorno ... se .F., houve time-out na chamada
// [3] O que foi retornado da execução. Se algum dos elementos anterior for .F., este será sempre NIL 
// ==================================================================================

METHOD CallSync(cFnCall,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) CLASS ZWORKJOB 
Local cIPCJobID := UPPER(getenvserver()+"_ZJOB_THREAD")
Local cIPCRet := UPPER(getenvserver())+"_ZJOB_"+cValtoChar(ThreadID())
Local nFnParms := pCount()-1
Local lCalled := .F. 
Local lRet  := .F. 
Local xRet  := NIL
Local nI

lCalled := IpcGo(cIPCJobID,cFnCall,nFnParms,cIPCRet,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

If ( lCalled )
	// Chamada síncrona aguarda por retorno
	// TimeOut em Segundos. A cada 1 segundo verifica 
	// se houve um KillaPP() para finalizar a aplicação 
	For nI := 1 to ::nTimeOut
		lRet := IpcWaitEx(cIPCRet,1000,@xRet)
		If lRet
			EXIT
		Endif
		If KillApp()
			EXIT
		Endif
	Next
Endif

Return { lCalled , lRet , xRet }


/* -------------------------------------------------------
Funcao de Gravação de log dos Working Jobs 
Por hora, grava apenas no console
------------------------------------------------------- */

STATIC Function JobLog(cMsg)
Local cEcho := ''
cEcho += "["+procname(1)+"]"
cEcho += '['+dtos(date())+' '+time()+'.'+strzero( ( seconds() - int(seconds())) * 1000 , 3 )+']'
cEcho += "[Thread "+cValToChar(ThreadID())+"]"
cEcho += " | " + cValToChar(cMsg)
conout(cEcho)
Return


