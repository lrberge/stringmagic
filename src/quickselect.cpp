/*********************************************
 * @file quickselect.cpp                     *                                                          
 * @author lrberge                           *                                                    
 * @brief quickselect main parser and tools  *                                                                             
 * @version 1.0                              *                                                 
 * @date 2023-05-09                          *                                                     
 *********************************************/

#include "stringmagick.h"
#include <Rcpp.h>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
using namespace Rcpp;



inline bool is_select_separator(const char * str, int i){
  return str[i] == ',' || str[i] == '/' || 
       (str[i] == '=' && i + 1 < static_cast<int>(std::strlen(str)) && str[i + 1] != '>');
}

inline bool is_cond_operator(const char * str, int i){
  return str[i] == '&' || str[i] == '|';
}

// [[Rcpp::export]]
List cpp_parse_charselect(SEXP Rstr){
  // parses single character vector: 
  // in: "species, sep_{*:'^.. => _'} = ^sepal & .num / (@width), ^petal, / petal.length, petal.width"
  // out:
  // -      names: c(       "", "sep_{*:'^.. => _'}",       "",                          "")
  // -   patterns: c("species",      "^sepal & .num", "^petal",                          "")
  // -      order: c(       "",             "@width",       "", "petal.length, petal.width")
  // -    is_cond: c(    false,                 true,    false,                       false)
  //
  // NOTA: we don't take care of malformed statements: they will be dealt with in R

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  std::vector<std::string> names, patterns, order;
  std::vector<bool> is_cond;
  std::string tmp, name_tmp, patterns_tmp, order_tmp;
  bool cond = false;

  int i = 0;
  while(i < n){
    name_tmp = "";
    patterns_tmp = "";
    order_tmp = "";
    cond = false;

    // first we get the variable + find out if there is a condition
    while(i < n && !is_select_separator(str, i)){
      if(str[i] == '{' && i + 2 < n && str[i + 1] == '*'){
        // special case of variable renaming: {*:bon => jour}
        // we allow the use of curly brackets in regular expressions, that's why we ount them
        int n_open = 1;
        std::string name_operation = "{*";
        int i_start = i;
        i += 2;

        while(i < n && n_open > 0){
          if(str[i] == '{' && str[i - 1] != '\\'){
            ++n_open;
          } else if(str[i] == '}' && str[i - 1] != '\\'){
            --n_open;
          }
          name_operation += str[i++];
        }

        if(i == n){
          // we have a problem
          i = i_start;
        } else {
          // we're OK (we remove the last item not to add conditions at the last line of the loop)
          name_operation.pop_back();
          --i;
          patterns_tmp += name_operation;
        }

      } else if(is_cond_operator(str, i)){
        cond = true;
      }
      
      patterns_tmp += str[i++];
    }

    if(i < n && str[i] == '='){
      cond = false;
      name_tmp = patterns_tmp;

      ++i;
      patterns_tmp = "";
      while(i < n && !is_select_separator(str, i)){
        if(is_cond_operator(str, i)){
          cond = true;
        }
        patterns_tmp += str[i++];
      }
    }

    if(i == n || (i < n && str[i] == ',')){
      // nothing, we move on
    } else if(i < n && str[i] == '/'){
      // we're in a sorting statement
      ++i;
      
      // stripping the WS
      while(i < n && is_blank(str[i])) ++i;

      if(str[i] == '('){
        ++i;
        // only the closing paren can stop it
        while(i < n && str[i] != ')') order_tmp += str[i++];
        // we strip the possible comma after it
        ++i;
        while(i < n && (is_blank(str[i]) || str[i] == ',')) ++i;
      } else {
        // up to the end of the string
        while(i < n) order_tmp += str[i++];
      }
      
    }

    // saving the data
    names.push_back(name_tmp);     
    patterns.push_back(patterns_tmp);
    order.push_back(order_tmp);
    is_cond.push_back(cond);

    ++i;

  }

  // we save + trim the WS
  List res;
  res["names"] = trim_ws(names);
  res["patterns"]  = trim_ws(patterns);
  res["order"] = trim_ws(order);
  res["is_cond"] = is_cond;
  
  return res;
}

// [[Rcpp::export]]
List cpp_parse_conditions_in_pattern(SEXP Rstr){
  // parses single character vector: 
  // in: "^sepal & .num | petal.length"
  // out:
  // -   patterns: c("^sepal", ".num", "petal.length")
  // - operations: c(      "",  "and",           "or")

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  std::vector<std::string> all_patterns, all_operations;
  all_operations.push_back("");
  std::string pattern_tmp, operator_tmp;

  int i = 0;
  while(i < n){
    pattern_tmp = "";

    while(i < n && !is_cond_operator(str, i)) pattern_tmp += str[i++];

    all_patterns.push_back(pattern_tmp);

    if(i < n){
      operator_tmp = str[i] == '&' ? "and" : "or";
      all_operations.push_back(operator_tmp);
      ++i;
    }
  }

  List res;
  res["patterns"] = trim_ws(all_patterns);
  res["operations"] = all_operations;

  return res;
}


inline bool is_same_value_ignore_case(const char * &x_str, int nx, SEXP &y_Rstr, int index){

  const char *y_str = CHAR(STRING_ELT(y_Rstr, index));
  int ny = std::strlen(y_str);

  if(nx != ny) return false;

  for(int i=0 ; i<nx ; ++i){
    if(x_str[i] != y_str[i]){
      if(x_str[i] >= 'A' && x_str[i] <= 'Z'){
        if(x_str[i] + 32 != y_str[i]){
          return false;
        }
      } else if(x_str[i] >= 'a' && x_str[i] <= 'z'){
        if(x_str[i] - 32 != y_str[i]){
          return false;
        }
      } else {
        return false;
      }
    }
  }

  return true;
}



// [[Rcpp::export]]
LogicalVector cpp_equal_ignore_case(SEXP x_Rstr, SEXP y_Rstr, bool ignore_case = true){
  // returns a TRUE/FALSE vector giving true if the element in y is equal to x
  // it can ignore the case of x and y

  int n = Rf_length(y_Rstr);
  LogicalVector res(n);

  // 1) we check exact equality (we just need to compare the pointers)
  intptr_t * px = (intptr_t *) STRING_PTR(x_Rstr);
  intptr_t x_val = px[0];
  intptr_t * py = (intptr_t *) STRING_PTR(y_Rstr);

  for(int i=0 ; i<n ; ++i){
    if(x_val == py[i]){
      res[i] = true;
      return res;
    }
  }

  if(!ignore_case) return res;

  // 2) with ignore case

  const char *x_str = CHAR(STRING_ELT(x_Rstr, 0));
  int nx = std::strlen(x_str);

  for(int i=0 ; i<n ; ++i){
    if(is_same_value_ignore_case(x_str, nx, y_Rstr, i)){
      res[i] = true;
      return res;
    }
  }

  return res;
}


// [[Rcpp::export]]
bool cpp_is_int_in_char(SEXP Rstr){
  // we accept only regular integers: no non round numbers

  if(Rf_length(Rstr) > 1) stop("Internal error in cpp_is_num_in_char: the vector must be of length 1.");

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);
   
  if(n < 1) return false;

  int i = 0;

  while(i < n && is_blank(str[i])) ++i;

  if(i == n) return false;

  bool is_negative = str[i] == '-';
  if( !(is_negative || (str[i] >= '0' && str[i] <= '9')) ){
    return false;
  }

  ++i;

  while(i < n && is_blank(str[i])) ++i;

  if(i == n && is_negative){
    return false;
  }

  while(i < n && str[i] >= '0' && str[i] <= '9') ++i;

  while(i < n && is_blank(str[i])) ++i;

  return i == n;
}


// [[Rcpp::export]]
bool cpp_is_trailing_dots(SEXP Rstr){
  // simple function that finds out if a string ends with ..
  // like in "sepal.."
  // 3 times (only...) faster than grepl("\\.\\.$")

  if(Rf_length(Rstr) > 1) stop("Internal error in cpp_is_trailing_dots: the vector must be of length 1.");

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  if(n < 3) return false;
  
  // special case: "!a.."
  if(str[0] == '!' && n < 4 ) return false;

  if(str[n - 1] != '.') return false;

  if(str[n - 2] != '.') return false;

  return true;
}


inline bool is_valid_char_var_start(char c){
  return c == '.' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

inline bool is_valid_char_var(char c){
  return c == '.' || c == '|' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

// [[Rcpp::export]]
bool cpp_is_variable_name(SEXP Rstr){
  // simple function that finds out if a string looks like a variable name
  // - "sepal." => true
  // - "^sepal" => false
  // 1us instead of 50us using grepl regex

  if(Rf_length(Rstr) > 1) stop("Internal error in cpp_is_variable_name: the vector must be of length 1.");

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  if(n < 1) return false;

  if(!is_valid_char_var_start(str[0])) return false;

  for(int i=1 ; i<n ; ++i){
    if(!is_valid_char_var(str[i])){
      return false;
    }
  }

  return true;
}


// [[Rcpp::export]]
List cpp_parse_name_stars(SEXP Rstr){
  // "any_{nb_Pub: 'nb_', lower}"
  // list:
  //      1: list("any")
  //      2: list(c("'nb_'", "lower"), "nb_Pub")
  // writting this function in R would have been a hot mess (way bigger than here!)

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  std::vector<bool> is_star;
  List all_info;
  std::vector<std::string> all_operations, empty_vector;
  std::string pattern_tmp, operations_tmp;

  int i_bak = 0;
  int i = 0;
  while(i < n){
    pattern_tmp = "";
    while(i < n && str[i] != '{' && str[i] != '*') pattern_tmp += str[i++];

    if(!pattern_tmp.empty()){
      is_star.push_back(false);
      all_info.push_back(pattern_tmp);
    }

    if(i == n) break;

    if(str[i] == '*'){
      // star without operations
      is_star.push_back(true);
      all_info.push_back(empty_vector);
      ++i;
      
    } else {
      // we have operations     
      i_bak = i;
      ++i;

      while(i < n && (is_blank(str[i]) || str[i] == '*')) ++i;

      if(i == n || str[i] != ':'){
        // parsing problem
        for(int j=i_bak ; j < n ; ++j) pattern_tmp += str[j];
        is_star.push_back(false);
        all_info.push_back(pattern_tmp);
        break;
      }

      ++i;
      all_operations.clear();
      while(i < n && str[i] != '}'){
        operations_tmp = "";
        while(i < n && str[i] != ',' && str[i] != '}'){
          operations_tmp += str[i++];
        }
        
        all_operations.push_back(operations_tmp);
        if(str[i] == ',') ++i;
      }

      if(i == n){
        // parsing failure yet again
        for(int j=i_bak ; j < n ; ++j) pattern_tmp += str[j];
        is_star.push_back(false);
        all_info.push_back(pattern_tmp);
        break;
      }

      ++i;

      is_star.push_back(true);
      all_info.push_back(trim_ws(all_operations));
    }
  }

  List res;
  res["is_star"] = is_star;
  res["info"] = all_info;

  return res;
}
