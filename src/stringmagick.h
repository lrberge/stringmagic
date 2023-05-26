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





