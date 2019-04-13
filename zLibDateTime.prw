#include 'Protheus.ch'

// ----------------------------------------
// Converte data no formato AAAAMMDD para Data do AdvPL 
STATIC Function STOD(cValue)
Local cOldSet := Set(_SET_DATEFORMAT, 'yyyy:mm:dd')
Local dRet := CTOD(Substr(cValue,1,4)+":"+Substr(cValue,5,2)+":"+Substr(cValue,7,2))
Set(_SET_DATEFORMAT, cOldSet)
Return dRet

    
// ----------------------------------------
// Converte Data Juliana em Data AdvPL 
STATIC Function Date2DJ(dDate)
Return (dDate - ctod("01/01/1980")) + 2444240 

// ----------------------------------------
// Converte Data Juliana em Data AdvPL 
STATIC Function DJ2Date(nDJ)
Return ctod("01/01/1980") + ( nDJ - 2444240 )
