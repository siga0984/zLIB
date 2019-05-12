
#ifndef zLibNBin_CH

  #define zLibNBin_CH

  #xtranslate Bin4toN( <cBin4> ) => STATICCALL( ZLIBNBIN , Bin4toN , <cBin4> ) 
  #xtranslate NtoBin4( <nNum> )  => STATICCALL( ZLIBNBIN , NtoBin4 , <nNum>  ) 

  #xtranslate Bin2toN( <cBin2> ) => STATICCALL( ZLIBNBIN , Bin2toN , <cBin2> ) 
  #xtranslate NtoBin2( <nNum> )  => STATICCALL( ZLIBNBIN , NtoBin2 , <nNum>  ) 

  #xtranslate NTOBIT8( <nNum> )  => STATICCALL( ZLIBNBIN , NTOBIT8 , <nNum>  ) 
  #xtranslate NTOBIT16( <nNum> )  => STATICCALL( ZLIBNBIN , NTOBIT16 , <nNum>  ) 

  #xtranslate NTOBITS( <nNum> )  => STATICCALL( ZLIBNBIN , NTOBITS , <nNum>  ) 
  #xtranslate BITSTON( <nNum> )  => STATICCALL( ZLIBNBIN , BITSTON , <nNum>  ) 

  #xtranslate Bit4ToHex( <cBit4> ) => STATICCALL( ZLIBNBIN , Bit4ToHex , <cBit4> ) 

  #xtranslate BIT8TON( <cBit8> , <nNum> ) => STATICCALL( ZLIBNBIN , BIT8TON , <cBit8> , @<nNum> ) 

#endif
