#include "stringmagic.h"




std::vector<std::string> trim_ws(std::vector<std::string> x){

  int n = x.size();
  std::vector<std::string> res(x);

  std::string tmp, xi;
  for(int i=0 ; i<n ; ++i){
    xi = x[i];
    if(!xi.empty() && (is_blank(xi[0]) || is_blank(xi[xi.size() - 1]))){
      tmp = "";
      int n_xi = xi.size();

      while(n_xi >= 1 && is_blank(xi[n_xi - 1])) --n_xi;

      int  j = 0;
      while(j < n_xi && is_blank(xi[j])) ++j;

      while(j < n_xi) tmp += xi[j++];

      res[i] = tmp;
    }
  }

  return res;
}

SEXP std_string_to_r_string(std::string x){
    
  SEXP res = PROTECT(Rf_ScalarString(Rf_mkCharCE(x.c_str(), CE_UTF8)));
  UNPROTECT(1);
  
  return res;
}

SEXP std_string_to_r_string(std::vector<std::string> x){
  
  int n = x.size();
  
  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));
  
  for(int i=0 ; i<n ; ++i){
    SET_STRING_ELT(res, i, Rf_mkCharCE(x[i].c_str(), CE_UTF8));
  }
  
  UNPROTECT(1);
  
  return res;
}

