
#ifndef zLibMisc_CH

  #define zLibMisc_CH

  /* Setar observações para o processo atual no Monitor do Protheus */

  #xtranslate SetMntObs( <cObs> ) => PtInternal( 1 , <cObs> )

  /* constante PI com 8 casas decimais */

  #DEFINE PI 3.14159265 // ACos(-1)

  /* Pseudo-função para troca de conteúdo entre variáveis */ 

  #TRANSLATE ZSWAP( <X> , <Y> , <S> ) =>  ( <S> := <X> , <X> := <Y> , <Y> := <S> )

#endif

