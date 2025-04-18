#include "stringmagic.h"
#include <Rcpp.h>
#include <string>
#include <cstring>
#include <cmath>
#include <vector>
#include <algorithm>
using namespace Rcpp;

inline void append_r_str(std::string &x, SEXP &Rstr_vec, int i = 0){
  const char* str = Rf_translateCharUTF8(STRING_ELT(Rstr_vec, i));
  x.append(str);  
}

// [[Rcpp::export]]
SEXP cpp_paste_conditional(SEXP x, IntegerVector id,
                           std::string sep = "", std::string sep_last = ""){

  int n_id = max(id);
  std::vector<std::string> res(n_id);
  int n_x = Rf_length(x);

  if(n_x == 0){
    return std_string_to_r_string(res);
  }

  const bool is_sep = !sep.empty();
  const bool is_last = !sep_last.empty() && sep.compare(sep_last) != 0;

  std::string tmp = "";
  int id_current = id[0];

  for(int i=0 ; i<n_x ; ++i){
    if(id[i] == id_current){

      if(is_sep && i > 0 && id[i - 1] == id_current){
        if(is_last && (i + 1 > n_x || id[i + 1] != id_current)){
          tmp += sep_last;
        } else {
          tmp += sep;
        }
      }

      append_r_str(tmp, x, i);

    } else {
      res[id_current - 1] = tmp;
      tmp = "";
      append_r_str(tmp, x, i);
      id_current = id[i];
    }
  }

  // don't forget the last item
  res[id[n_x - 1] - 1] = tmp;

  return std_string_to_r_string(res);
}


enum {
  PUNCT, DIGIT, PUNCT_DIGIT, BLANK
};

inline bool is_invalid_char(const int invalid_type, const char *str, int i){
  // http://www.kerryr.net/pioneers/ascii3.htm
  // character numbers
  //
  // white spaces are ALWAYS invalid

  if(str[i] == ' ' || str[i] == '\t' || str[i] == '\n'){
    return true;
  }

  if(invalid_type == PUNCT || invalid_type == PUNCT_DIGIT){
    if((str[i] >= '!' && str[i] <= '/') ||
       (str[i] >= ':' && str[i] <= '@') ||
       (str[i] >= '[' && str[i] <= '`') ||
       (str[i] >= '{' && str[i] <= '~')){
      return true;
    }
  }

  if(invalid_type == DIGIT || invalid_type == PUNCT_DIGIT){
    return str[i] >= '0' && str[i] <= '9';
  }

  return false;
}

// [[Rcpp::export]]
SEXP cpp_normalize_ws(SEXP Rstr){

  int n_vec = Rf_length(Rstr);

  std::vector<std::string> res(n_vec);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, i_vec));
    int n = std::strlen(str);

    std::string x;
    int i = 0;
    while(i < n){
      while(i < n && is_blank(str, i)) ++i;
      while(i < n && !is_blank(str, i)) x += str[i++];
      if(i++ < n) x += ' ';
    }

    if(!x.empty() && x.back() == ' '){
      x.pop_back();
    }

    res[i_vec] = x;
  }

  return std_string_to_r_string(res);
}

// [[Rcpp::export]]
SEXP cpp_normalize_string(SEXP Rstr, bool clean_punct, bool clean_digit,
                                  bool clean_isolated){

  int n_vec = Rf_length(Rstr);
  std::vector<std::string> res(n_vec);

  const int invalid_type = clean_punct ? (clean_digit ? PUNCT_DIGIT : PUNCT) : (clean_digit ? DIGIT : BLANK);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, i_vec));
    int n = std::strlen(str);

    std::string x;
    int i = 0;

    while(i < n){
      while(i < n && is_invalid_char(invalid_type, str, i)) ++i;

      if(clean_isolated){
        if(i >= n - 1 || is_invalid_char(invalid_type, str, i + 1)){
          // means the current character is isolated
          ++i;
          continue;
        }
      }

      while(i < n && !is_invalid_char(invalid_type, str, i)) x += str[i++];
      if(i++ < n) x += ' ';
    }

    if(!x.empty() && x.back() == ' '){
      x.pop_back();
    }

    res[i_vec] = x;
  }

  return std_string_to_r_string(res);
}

// [[Rcpp::export]]
SEXP cpp_trimws_in_place(SEXP x){
  // this is 'fast' because the changes are in place
  // beware!

  int n_vec = Rf_length(x);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char* str = Rf_translateCharUTF8(STRING_ELT(x, i_vec));
    int n = std::strlen(str);

    if(is_blank(str[0]) || is_blank(str[n - 1])){
      int i = 0;
      while(i < n && is_blank(str, i)) ++i;
      while(n > 0 && is_blank(str, n - 1)) --n;

      std::string tmp;
      for(; i < n ; ++i){
        tmp += str[i];
      }

      SET_STRING_ELT(x, i_vec, Rf_mkCharCE(tmp.c_str(), CE_UTF8));
    }
  }

  return x;
}

// [[Rcpp::export]]
std::vector<int> cpp_which_empty(SEXP Rstr){
  // this is abt 10x faster than grep("[^ \t\n]", m, value = TRUE)

  int n_vec = Rf_length(Rstr);
  std::vector<int> res;

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char *str = CHAR(STRING_ELT(Rstr, i_vec));
    int n = std::strlen(str);
    int i = 0;

    while(i < n && is_blank(str, i)) ++i;
    if(i == n){
      res.push_back(i_vec + 1);
    }
  }

  return res;
}

// [[Rcpp::export]]
std::vector<int> cpp_find_first_index(IntegerVector index, int nb, bool is_last){

  int n = index.length();
  /// index is never equal to 0
  int current = 0;
  int n_done = 0;

  std::vector<int> res;

  int i = is_last ? n - 1 : 0;

  while(is_last ? i >= 0 : i < n){

    if(current == index[i]){
      if(n_done < nb){
        ++n_done;
        res.push_back(i + 1);
      }
    } else {
      current = index[i];
      n_done = 1;
      res.push_back(i + 1);
    }

    is_last ? --i : ++i;
  }

  if(is_last){
    std::reverse(res.begin(), res.end());
  }

  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_group_rev_index(IntegerVector index){

  int n = index.length();
  IntegerVector res(n);

  if(n == 0){
    return res;
  }

  std::vector<int> table(1);

  int current = index[0];
  int ng = 0;

  // 1) we create the table
  for(int i=0 ; i<n ; ++i){
    if(current == index[i]){
      ++table[ng];
    } else{
      ng++;
      current = index[i];
      table.push_back(1);
    }
  }

  // total number of groups (before if was an index, so we add 1 to get a total)
  ++ng;

  // start table is the cum sum of table, starting at 0
  std::vector<int> start_table(ng);
  for(int g=1 ; g<ng ; ++g){
    start_table[g] = start_table[g - 1] + table[g - 1];
  }

  // 2) we create the reverted index
  int i = 0;
  for(int g=0 ; g<ng ; ++g){
    int start = start_table[g];
    int n_table = table[g];
    for(int j=0 ; j<n_table ; ++j){
      res[i++] = start + n_table - j;
    }
  }

  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_recreate_index(IntegerVector id){

  int n = id.length();
  IntegerVector res(n);

  int g = 1;
  int id_current = id[0];

  for(int i=0 ; i<n ; ++i){
    if(id[i] != id_current){
      ++g;
      id_current = id[i];
    }
    res[i] = g;
  }

  return res;
}

inline bool is_inside(std::vector<int> v, int key){
  return std::find(v.begin(), v.end(), key) != v.end();
}

inline bool is_escaped_regex_logical(const char *str, int i, int n){
  //    in: "hey \\&"
  // pos i:        ^
  // we look before i
  // THIS IS AN EXTREMELY SPECIALIZED FUNCTION
  // true iff " \\& "
  
  if(i < 2 || i + 1 >= n || str[i - 1] != '\\' || str[i + 1] != ' ') return false;
  return str[i - 2] == ' ';  
}

inline bool is_regex_logical(const char *str, int i, int n){
  // true: " & "
  //    i:   ^
  
  return i >= 1 && i + 1 < n && str[i - 1] == ' ' && str[i + 1] == ' ';
}

// [[Rcpp::export]]
List cpp_parse_regex_pattern(SEXP Rstr, bool parse_flags, bool parse_logical){
  // Limitation: multibyte strings are not handled properly
  //  in: "fixed, word/test"
  // out: -      flags: c("fixed", "word")
  //      -   patterns: "test"
  //      -      is_or: FALSE
  //      -     is_not: FALSE
  //
  //  in: "test & !wordle"
  // out: -      flags: ""
  //      -   patterns: c("test", "wordle")
  //      -      is_or: c(FALSE, FALSE)
  //      -     is_not: c(FALSE, TRUE)
  //
  //  in: "fiw/test | wordle"
  // out: -      flags: "fiw"
  //      -   patterns: c("test", "wordle")
  //      -      is_or: c(FALSE, TRUE)
  //      -     is_not: c(FALSE, TRUE)
  //
  
  // General behavior:
  // - whenever there is a slash in the regx: we throw an error if:
  //   * the flags are invalid or **empty**
  //   * there is a 'not' before valid flags (this is not allowed either)

  if(Rf_length(Rstr) != 1) stop("Internal error in cpp_parse_regex_pattern: the vector must be of length 1.");

  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  List res;
  std::vector<std::string> flags;
  std::string flag_tmp;

  int i = 0;

  //
  // flags
  //
  
  bool is_not = false;
  bool any_slash_escaped = false;
  std::vector<int> i_skip;
  bool is_slash = false;
  
  
  if(parse_flags){
    // parsing the flags
    // since flag parsing can be dangerous, I thoroughly
    // send an error if a pattern contains a '/'
    
    // first: we check if the pattern contains '/'
    // if '/' is escaped, we consider that the user knows what she's doing
    // so we don't even check the other slashes
    // we still get all the escaped '/'
    for(int j=0 ; j<n ; ++j){
      if(str[j] == '/'){
        if(is_non_escaped_symbol('/', str, j, n, false)){
          if(!any_slash_escaped){
            is_slash = true;
          }
        } else {
          i_skip.push_back(j - 1);
          any_slash_escaped = true;
        }
      }
    }
    
    if(is_slash){
      // at the end of this loop, i points at the beginning of the pattern
      
      if(!parse_logical && str[i] == '!'){
        res["error"] = "flag starts with !";
        return res;
      }
      
      if(is_blank(str[i])){
        res["error"] = "flag starting with space";
        return res;
      }
      
      while(i < n){
        
        if(i >= 100){
          res["error"] = "flags list is too long";
          return res;
        }
        
        while(i < n && str[i] == ' ') ++i;
        while(i < n && str[i] >= 'a' && str[i] <= 'z'){
          flag_tmp += str[i++];
        }
        
        if(i < n && str[i] != ',' && str[i] != '/'){
          res["error"] = "non valid character at position";
          res["error_extra"] = i + 1;
          return res;
        }

        if(flag_tmp.empty()){
          res["error"] = "flag empty";
          res["error_extra"] = flags.size() + 1;
          return res;
        }
        
        flags.push_back(flag_tmp);
        flag_tmp = "";

        if(str[i] == '/'){
          ++i;
          break;
        } else if(str[i] == ','){
          ++i;
          continue;
        }
      }
      
      if(flags.empty()){
        stop("Internal errors: flags cannot be empty in the presence of a slash in a pattern.");
      }
      
    }
  }  

  // saving the flags
  res["flags"] = std_string_to_r_string(flags);
  
  //
  // logical operators
  //

  // now we go for the " & ", the " | " and the "!"

  std::vector<std::string> patterns;
  std::string pat_tmp;
  std::vector<bool> is_or_all;
  std::vector<bool> is_not_all;
  bool is_or_tmp = false;

  while(i < n){
    // beginning of the pattern => is there a not?
    is_not = false;
    if(parse_logical && is_non_escaped_symbol('!', str, i, n, true)){
      is_not = true;
      ++i;
    }
    
    while(i < n){
      if(any_slash_escaped && is_inside(i_skip, i)){
        ++i;
        continue;
      }
      
      if(parse_logical && (str[i] == '&' || str[i] == '|')){
        if(is_escaped_regex_logical(str, i, n)){
          // we remove the escape
          pat_tmp.pop_back();
        } else if(is_regex_logical(str, i, n)){
          // we flush
          pat_tmp.pop_back();
          patterns.push_back(pat_tmp);
          is_or_all.push_back(is_or_tmp);
          is_not_all.push_back(is_not);
          
          pat_tmp = "";
          is_or_tmp = str[i] == '|';
          i += 2;
          
          break;
        }        
      }
      
      pat_tmp += str[i++];
    }
    
    // the last item
    if(i == n){
      patterns.push_back(pat_tmp);
      is_or_all.push_back(is_or_tmp);
      is_not_all.push_back(is_not);
    }
    
  }
  
  if(patterns.empty()){
    patterns.push_back("");
    is_or_all.push_back(false);
    is_not_all.push_back(false);
  }

  res["patterns"] = std_string_to_r_string(patterns);
  res["is_or"] = is_or_all;
  res["is_not"] = is_not_all;

  return res;
}


// [[Rcpp::export]]
IntegerVector cpp_create_pos(IntegerVector index){
  // index: index vector of consecutive values
  // output: 1 2 3 1 1 2 for input 1 1 1 2 3 4 4

  int n = index.size();
  IntegerVector res(n, 1);
  int current = index[0];
  int val = 1;

  for(int i=1 ; i<n ; ++i){
    if(index[i] == current){
      ++val;
    } else {
      current = index[i];
      val = 1;
    }

    res[i] = val;
  }

  return res;
}

// [[Rcpp::export]]
IntegerVector cpp_table(IntegerVector index, int n_items){
  // index: index vector
  // contains only integer values from 1 to n_items
  
  int n = index.length();
  IntegerVector res(n_items);
  
  for(int i=0 ; i<n ; ++i){
    ++res[index[i] - 1];
  }
  
  return res;
}



inline void add_R_string(std::string &x, SEXP &R_value, std::string def){
  if(Rf_isNull(R_value)){
    if(def.size() > 0){
      x += def;
    }
  } else {
    const char *value = Rf_translateCharUTF8(STRING_ELT(R_value, 0));
    for(size_t i=0 ; i<std::strlen(value) ; ++i){
      x += value[i];
    }
  }
}

inline void add_R_string_before(std::string &x, SEXP &R_value, std::string def){
  if(Rf_isNull(R_value)){
    if(def.size() > 0){
      x = def + x;
    }
  } else {
    const char *value = Rf_translateCharUTF8(STRING_ELT(R_value, 0));
    std::string val;
    for(size_t i=0 ; i<std::strlen(value) ; ++i){
      val += value[i];
    }
    x = val + x;
  }
}


std::string format_number_single(double x, int digits, int signif, bool int_as_double, 
                                 SEXP &minus_sign, SEXP &decimal,
                                 SEXP &big_mark, SEXP &small_mark,
                                 SEXP &prefix, SEXP &suffix){
  // number formatting
  
  std::string res;
  
  bool is_neg = x < 0;
  double x_abs = fabs(x);
  
  // special cases
  if(x == 0){
    
    if(int_as_double){
      res = digits == 0 ? "0" : "0.";
      for(int i=0 ; i<digits ; ++i){
        if(i > 0 && i % 3 == 0){
          add_R_string(res, small_mark, "");
        }
        res += "0";
      }
    } else {
      res = "0";
    }
    
    add_R_string_before(res, prefix, "");
    add_R_string(res, suffix, "");
    
    return res;
  } else if(std::isnan(x)){
    return "NA";
  } else if(std::isinf(x)){
    return is_neg ? "Inf" : "-Inf";
  }
  
  
  double x_round = 0, x_trunc = 0, rest = 0;
  if(signif == 0){
    // we round at the right number of digits
    x_round = round(x_abs * pow(10, digits)) / pow(10, digits);
    x_trunc = trunc(x_round);
    rest = x_round - x_trunc;
  } else {
    // signif: we don't round
    x_trunc = trunc(x_abs);
    rest = x_abs - x_trunc;
  }

  std::string x_str = std::to_string(static_cast<int64_t>(x_trunc));
  if(x_trunc > 1e18){
    x_str = std::to_string(x_trunc);
    std::size_t dot_loc = x_str.find('.');
    if(dot_loc != std::string::npos){
      x_str.erase(x_str.begin() + dot_loc, x_str.end());
    }
  }
  
  // sign
  if(is_neg){
    add_R_string(res, minus_sign, "-");
  }
  
  // whole part
  int n_signif;
  if(x_trunc < 1000){
    res += x_str;
    n_signif = x_trunc == 0 ? 0 : x_str.size();
  } else {
    int n = x_str.size();
    n_signif = n;
    int e = n; // e: exponent
    
    // for very large numbers too much precision is meaningless
    if(x_trunc > 5e15){
      while(e > 0){
        res += e > n - 15 ? x_str[n - e] : '0';
        --e;
        if(e > 1 && e % 3 == 0) {
          add_R_string(res, big_mark, ",");
        }
      }
      
    } else {
      while(e > 0){
        res += x_str[n - e];
        --e;
        if(e > 1 && e % 3 == 0) {
          add_R_string(res, big_mark, ",");
        }
      }
    }
  }
  
  // decimal part
  bool do_digits = int_as_double || fabs(x - round(x)) > 1e-013;
  if(do_digits){
    
    if(digits > 0 || n_signif < signif){
      add_R_string(res, decimal, ".");

      std::string rest_str = std::to_string(rest);
      int n_rest = rest_str.size();
      
      int k = 0;
      while(2 + k < n_rest && (k < digits || n_signif < signif)){
        char c = rest_str[2 + k];
        
        if(n_signif > 0 || c != '0'){
          ++n_signif;
        }
        
        if(k > 0 && k % 3 == 0){
          add_R_string(res, small_mark, "");
        }
        
        res += c;
        
        ++k;
      }
      
    }
  }
  
  add_R_string_before(res, prefix, "");
  add_R_string(res, suffix, "");

  return res;
}

inline int get_R_int(SEXP x, int def){
  if(Rf_isNull(x)){
    return def;
  }
  
  if(TYPEOF(x) == REALSXP){
    return REAL(x)[0];
  }
  
  return INTEGER(x)[0];
}

inline bool get_R_bool(SEXP x, bool def){
  if(Rf_isNull(x)){
    return def;
  }
  
  return LOGICAL(x)[0];
}


// [[Rcpp::export(rng = false)]]
SEXP cpp_format_numeric(SEXP R_x, SEXP R_digits, SEXP R_signif, 
                        SEXP R_int_as_double, SEXP minus_sign, SEXP decimal, 
                        SEXP big_mark,SEXP small_mark, 
                        SEXP prefix, SEXP suffix){
  
  // about 20x faster than base::format
  
  // This function was originally developped in listable
  
  int digits = get_R_int(R_digits, 1);
  int signif = get_R_int(R_signif, 2);
  int int_as_double = get_R_bool(R_int_as_double, false);
  
  const bool is_double = TYPEOF(R_x) == REALSXP;
  
  int n = Rf_length(R_x);
  std::vector<std::string> res(n);
  for(int i=0 ; i<n ; ++i){
    double x = is_double ? REAL(R_x)[i] : INTEGER(R_x)[i];
    res[i] = format_number_single(x, digits, signif, int_as_double, minus_sign, decimal,
                                  big_mark, small_mark, prefix, suffix);
  }
  
  SEXP R_res = std_string_to_r_string(res);
  
  return R_res;
}

// [[Rcpp::export]]
SEXP cpp_split_newlines(SEXP x){
  
  std::vector<std::string> res;
  
  int n = Rf_length(x);
  
  std::string v;
  
  for(int obs = 0 ; obs<n ; obs++){
    const char *str = Rf_translateCharUTF8(STRING_ELT(x, obs));
    for(size_t i=0 ; i<std::strlen(str) ; ++i){
      if(str[i] == '\n'){
        res.push_back(v);
        v = "";
      } else {
        v += str[i];
      }
    }
  }
  
  if(n > 0){
    res.push_back(v);
  }
  
  return std_string_to_r_string(res);
}




