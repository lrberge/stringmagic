/**
 * @file stringmagick.h
 * @author lrberge
 * @brief a handful of simple utility functions
**/

#pragma once

#include <Rcpp.h>
#include <string>
#include <cstring>
#include <vector>

inline bool is_blank(const char *str, int i){
  return str[i] == ' ' || str[i] == '\t' || str[i] == '\n';
}

inline bool is_blank(char c){
  return c == ' ' || c == '\n' || c == '\t';
}

std::vector<std::string> trim_ws(std::vector<std::string> x);

SEXP std_string_to_r_string(std::string x);

SEXP std_string_to_r_string(std::vector<std::string> x);

inline bool is_non_escaped_symbol(const char symbol, const char * str, int &i, int n, bool skip){
  // symbol is a simple char, like '|' or '/'

  bool ok_escape = false;
  if(skip && str[i] == '\\'){
    // what do we do here? we increment i if we have an escaped symbol
    // since i does not point at the symbol, it always returns FALSE
    // only thing that matters is the i increment
    if(i + 1 < n && str[i + 1] == symbol){
      ok_escape = true;
    }
    
    if(ok_escape){
      int j = i - 1;
      while(j > 0 && str[j--] == '\\'){
        ok_escape = !ok_escape;
      }
      
      if(ok_escape){
        ++i;
      }
    }
    
    return false;
  }
  
  // we don't skip but we still check for escaping
  bool ok_symbol = str[i] == symbol;
  
  if(ok_symbol){
    // we check escaping
    if(i >= 1 && str[i - 1] == '\\'){
      ok_escape = true;
      int j = i - 2;
      while(j > 0 && str[j--] == '\\'){
        ok_escape = !ok_escape;
      }
      
      return !ok_escape;
    }
  }
  
  return ok_symbol;
}





