
#ifndef zLibDateTime_CH

  #define zLibDateTime_CH

  #xtranslate STOD( <cAnsiDate> ) => STATICCALL( ZLIBDATETIME , STOD , <cAnsiDate> ) 
  #xtranslate DATE2DJ( <dDate> ) => STATICCALL( ZLIBDATETIME , DATE2DJ , <dDate> ) 
  #xtranslate DJ2DATE( <nDJ> ) => STATICCALL( ZLIBDATETIME , DJ2DATE , <nDJ> ) 

#endif
