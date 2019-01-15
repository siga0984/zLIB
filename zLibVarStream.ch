
#ifndef zLibVarStream_CH

  #define zLibVarStream_CH

  #xtranslate Var2BinStr( <xVar>, <cBinStr> )  => <cBinStr> := __PV2STR( <xVar> )
  #xtranslate BinStr2Var( <cBinStr> , <xVar> ) => __GV2STR( <cBinStr>, @<xVar> )

#endif

