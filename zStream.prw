#include  'protheus.ch'
#include  'zLib.ch'

/* ============================================================================

CLASSE         ZSTREAM
Autor          Júlio Wittwer
Data           01/2019
Descrição      O Objeto Stream permite gravar sequencias de dados e de objetos 
               em uma string binária, que pode ser armazenada e carregada 


============================================================================ */

CLASS ZSTREAM FROM LONGNAMECLASS

   DATA aStream          // Array com os elementos do Stream
   DATA nPos             // Elemento atual do Stream 
   
   METHOD NEW()          // Concstrutor 
   METHOD LOADSTR()      // Carrega um stream da binary String gerada pelo ::SaveStr()
   METHOD SAVESTR()      // Salva o Stream em uma Binary String 
   METHOD CLEAR()        // Limpa o Stream 
   METHOD READ()         // Lê a prixima informação do Stream 
   METHOD WRITE()        // Acrescenta uma variável no Stream
   METHOD WRITEOBJ()     // Acrescenta um Objeto no Stream 

ENDCLASS

// ----------------------------------------
// Construtor, apenas inicializa variaáveis 
METHOD NEW() CLASS ZSTREAM 
::aStream := {}
::nPos := 1
Return SELF

// ----------------------------------------
// Recebe cBuffer com o Binary Stream gerado pelo Save()
// Lê o Buffer e popula o Stream.
// Se o tream é valido -- foi gerado pela SaveStr, carrega 
METHOD LOADSTR(cBuffer) CLASS ZSTREAM 
If left(cBuffer,11) == "#_ZSTREAM_#"
	cBuffer := Substr(cBuffer,12)
	BinStr2Var( cBuffer, ::aStream ) 
	::nPos := 1
	Return .T. 
Endif
Return .F. 

// ----------------------------------------
// Recebe cBuffer por referencia 
// Salva o stream nele como Binary String
// Coloca um prefixo na BinaryString gerada
METHOD SAVESTR(cBuffer) CLASS ZSTREAM 
Var2BinStr( ::aStream , cBuffer )
cBuffer := Stuff(cBuffer , 1 , 0 , "#_ZSTREAM_#")
Return .T. 

// ----------------------------------------
// Limpa e reinicializa o Stream 

METHOD CLEAR() CLASS ZSTREAM 
::aStream := {}
::nPos := 1
Return

// ----------------------------------------
// Leitura por referência do stream , na ordem de gravação 

METHOD READ(xValue) CLASS ZSTREAM 
Local cType := ::aStream[::nPos][1] 
Local aObjData 
Local nI
Local cMacro, bBlock

If cType ==  'O'      

	// É um objeto, pego o array com as propriedades
	BinStr2Var( ::aStream[::nPos][2] , aObjData )

	// Agora popula as propriedades no Objeto 
	For nI := 1 to len(aObjData)

		// Monto um codebloc dinamico para atualizar a 
		// propriedade . Recebo o objeto e o valor a ser atualizado 
		cMacro := "{|o,x| o:"+aObjData[nI][1]+" := x }"
		bBlock := &(cMacro)
		
		// Rodo a atualização do valor 
		Eval(bBlock,xValue,aObjData[nI][2])

		// Quebro as referencias 
		bBlock := NIL
		 	
	Next
	
	// Quebro as referencias 
	aObjData := NIL 
	
Else

	// Nao é objeto, pego o que tem e recupero 
	BinStr2Var( ::aStream[nPos][2] , xValue  )

Endif

// Próxima posição para leitura 
::nPos++

Return

// ----------------------------------------
// Escreve um valor no Stream. 
// Pode até ser um objeto 

METHOD WRITE(xValue) CLASS ZSTREAM 
Local cBuffer := ''
Local cType := Valtype(xValue)
If cType $ 'CNDLMAU'
	// Acrescenta o valor no Array para Stream 
	// Coloca o tipo junto 
	Var2BinStr(xValue,cBuffer)
	aadd(::aStream,{cType,cBuffer})
	cBuffer := ''
ElseIF cType == 'O'
	// Se for um objeto, acrescenta com todas as propriedades 
	// Caso seja necessário salvar apenas algumas propriedades, 
	// deve ser usado o método WriteObj()
	::WRITEOBJ(xValue)
Else
	UserException("ZSTREAM:Write() -- Unsupported Type "+cType)
Endif
Return 

// ----------------------------------------
// Método para salvar especificamente um objeto 
// Por default salva todas as propriedades suportadas 

METHOD WRITEOBJ(oObj,lParent,cPropList) CLASS ZSTREAM 
Local cBuffer := ''
Local aObjData, aSelData := {}
Local aPropList
Local nI, nJ 

// Parametros opcionais - default 
If lParent = NIL ; lParent := .F. ; Endif
If cPropList = NIL ; cPropList := '' ; Endif

// Pega todas as propriedades da classe
aObjData := ClassDataArr(oObj,lParent)

If !empty(cPropList)
	
	// Pega lista de propriedades separadas por virgula
	aPropList := StrTokarr(cPropList,",")
	
	// Varre as propriedades da classe para salvar 
	// apenas as propriedades que batem com a lista 
	// pode ser usado "*" e "?"
	For nI := 1 to len(aObjData)
		lMatch := .F.
		For nJ := 1 to len(aPropList)
			IF match( lower(aObjData[nI][1]) , lower(aPropList[nJ]) )
				lMatch := .T.
				EXIT
			Endif
		Next
		If lMatch .and. ValType(aObjData[nI][2]) $ 'CNDLMAU'
			// Acrescenta referencia no que deve ser guardado 
			AADD(aSelData,aObjData[nI])
		Endif
	Next		
		
	// Guarda no stream as propriedades relacionadas 
	Var2BinStr( aSelData , cBuffer ) 
	
Else

	// Nao tem listam guarda TUDO 
	Var2BinStr( aObjData , cBuffer )
	
Endif

// Guarda este valor no Stream Array 
// Coloca o tipo junto 
aadd(::aStream, { 'O' , cBuffer } )

// Quebra as referências
aObjData := NIL
aSelData := NIL
cBuffer := ''

Return 

