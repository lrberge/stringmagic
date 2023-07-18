/*******************************************************
 * @file stringmagic_parser.cpp                       *
 * @author lrberge                                     *
 * @brief main parser of the stringmagic framework,   *
 *        see description below                        *
 *        + secondary parsers                          *
 * @version 1.0                                        *         
 * @date 2023-05-09                                    *
 *******************************************************/

#include "stringmagic.h"
#include <Rcpp.h>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
using namespace Rcpp;


// DEFINITIONS
// - box = operator used to interpolate the variables
//   ex: {var}, here {} is the box. .[var], here .[] is the box
// - operator: code name of the operation to be applied to the interpolated variable
//   ex: {S, C!1, 2, 3}, here S and C are the operators
// - string_ops/sop: sting operations
//   general name of the process of parsing the full string into box, operators and the rest
// - separator: either ? or !
//   whether to evaluate from the environment or use verbatim

//
// delimiter class -------------------------------------------------------------
//


class delim{
  const char *open;
  const char *close;
  
  int size_open;
  int size_close;
 
  delim() = delete;
 
public:
  // SEXP: vector of length 2
  delim(SEXP);
  
  bool is_open(const char *, int &, int, bool);
  bool is_close(const char *, int &, int, bool);
  
  int get_size_open(){return size_open;};
  int get_size_close(){return size_close;};
 
};

delim::delim(SEXP Rstr){
  if(Rf_length(Rstr) != 2){
    stop("Internal error: Delimiter must be of length 2");
  }
  if(TYPEOF(Rstr) != STRSXP){
    stop("Internal error: Delimiter must be of type character");
  }
  
  open = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  close = Rf_translateCharUTF8(STRING_ELT(Rstr, 1));
  
  size_open = std::strlen(open);
  size_close = std::strlen(close);
}

bool check_symbol(const char *symbol, int n_symbol, const char *str, int &i, int n, bool skip){
  
  bool ok_escape = false;
  if(skip && str[i] == '\\'){
    // what do we do here? we increment i if we have an escaped symbol
    // since i does not point at the symbol, it always returns FALSE
    // only thing that matters is the i increment
    if(i + n_symbol < n){
      for(int k=0 ; k<n_symbol ; ++k){
        if(str[i + 1 + k] != symbol[k]){
          return false;
        }
      }
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
  for(int k=0 ; k<n_symbol ; ++k){
    if(str[i + k] != symbol[k]){
      return false;
    }
  }
  
  // we check escaping
  if(i >= 1 && str[i - 1] == '\\'){
    ok_escape = true;
    int j = i - 2;
    while(j >= 0 && str[j--] == '\\'){
      ok_escape = !ok_escape;
    }
    
    return !ok_escape;
  }
  
  return true;
}

bool delim::is_open(const char *str, int &i, int n, bool skip){
  return check_symbol(open, size_open, str, i, n, skip);
}

bool delim::is_close(const char *str, int &i, int n, bool skip){
  return check_symbol(close, size_close, str, i, n, skip);
}

//
// END OF: delimiter class ----------------------------------------------------- 
//

inline bool is_escaped(const char *str, int i){
  bool ok_escape = i >= 1 && str[i - 1] == '\\';
  
  if(ok_escape){
    int j = i - 2;
    while(j >= 0 && str[j--] == '\\'){
      ok_escape = !ok_escape;
    }  
  }
  
  return ok_escape;
}

inline bool is_digit(const char * str, int i){
  return str[i] >= '0' && str[i] <= '9';
}

inline bool is_special_char(const char * str, int i){
  return str[i] == '\'' || str[i] == '"' || str[i] == '`' || str[i] == ':' ||
    str[i] == ';' || str[i] == '/';
}

inline bool is_quote(const char * str, int i){
  return str[i] == '\'' || str[i] == '"' || str[i] == '`';
}

inline bool is_basic_quote(const char * str, int i){
  return str[i] == '\'' || str[i] == '"';
}

inline bool is_separator(const char * str, int i){
  return str[i] == '!' || str[i] == '?';
}


// checks inclusion of a character value in a string array
inline bool is_in_string(const char * str, int i, std::string std_str){
  // only supports endings of length <= 3
  for(size_t k=0 ; k<std_str.size() ; ++k){
    if(str[i] == std_str[k]){
      return true;
    }
  }
  return false;
}

// adds quotes only when operator_tmp is not quoted
inline void enquote(std::string &operator_tmp){

  if(operator_tmp.empty()){
    operator_tmp = "''";
    return;
  }

  int n = operator_tmp.length();

  if(n > 1){
    if((operator_tmp[0] == '\'' && operator_tmp[n - 1] == '\'') || 
          (operator_tmp[0] == '"' && operator_tmp[n - 1] == '"')){
      return;
    }
  }

  if(operator_tmp[0] == '\''){
    operator_tmp = '"' + operator_tmp + '"';
  } else {
    operator_tmp = '\'' + operator_tmp + '\'';
  }

}

void extract_quote(const char * str, int &i, int n,
                   std::string &operator_tmp, const bool inQuote_only = false){
  // extracts a quoted string and adds it to operator_tmp, also updates i
  // escaped quotes are ignored
  //
  //  assume str: "Mary: 'hello \\'John Snow\\'! hello dear!' And then he went"
  // values of i:        i (in, at the quote)                i (out, after the quote)
  //
  // operator_tmp is augmented with: "'hello \\'John Snow\\'! hello dear!'"
  // -> note that the quotes are INCLUDED
  //
  // clean: whether we get only the value in the quote
  // we also clean it

  char quote = str[i++];
  if(!inQuote_only) operator_tmp += quote;

  while(i < n){
    if(str[i] == quote){
      if(str[i - 1] == '\\'){
        // we escape the quote and remove the '\\' (if only inquote)
        if(inQuote_only) operator_tmp.pop_back();
      } else {
        break;
      }
    }

    operator_tmp += str[i++];
  }

  if(i < n){
    if(!inQuote_only) operator_tmp += quote;
    ++i;
  }
}


void extract_r_expression(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                          std::string &operator_tmp, std::string ending_str, 
                          bool check_closing, bool include_closing);

void extract_single_simple_operation(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending_str){

  // regular, simple operation
  // format: /'arg'op/ or /arg op/ or /'arg' op/

  // step 1: argument extraction (if provided)
  bool is_arg = false;
  if(is_quote(str, i)){
    is_arg = true;
    extract_quote(str, i, n, operator_tmp);
  } else {
    // we check for possible argument space separated
    while(i < n && str[i] != ',' && str[i] != ' ' && !is_in_string(str, i, ending_str) && 
                !delims.is_close(str, i, n, false) && !delims.is_open(str, i, n, false)){
      operator_tmp += str[i++]; 
    }

    if(i < n && str[i] == ' '){
      operator_tmp += str[i++];
      is_arg = true;
    }
  }
  
  if(delims.is_open(str, i, n, false)){
    is_pblm = true;
    return;
  }

  // step 2: operator extraction (if not done)
  if(is_arg){
    while(i < n && str[i] != ',' && !is_in_string(str, i, ending_str) && 
                !delims.is_close(str, i, n, false)){
      operator_tmp += str[i++];
    }
    
    if(delims.is_open(str, i, n, false)){
      is_pblm = true;
      return;
    }
  }
  
  // stripping ending WS
  while(operator_tmp.length() > 0 && operator_tmp[operator_tmp.length() - 1] == ' '){
    operator_tmp.pop_back();
  }

  if(i == n){
    is_pblm = true;
  }

}

inline bool is_paren_operator(const char * str, int i, int n){
  // the special simple operators
  // they're the only ones to allow parentheses

  if(n <= i + 3) return false;

  return (str[i] == '~' && str[i + 1] == '(') || 
            (str[i] == 'i' && str[i + 1] == 'f' && str[i + 2] == '(') ||
            (str[i] == 'v' && str[i + 1] == 'i' && str[i + 2] == 'f' && str[i + 3] == '(');

}


void extract_verbatim(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, std::string ending_str, 
                      bool check_closing, bool include_closing,
                      bool skip_box_open, bool skip_second_space = false);

void extract_simple_ops_verbatim(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending_str);

void extract_paren_operator(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp){
  // ~(simple_ops)
  // if(cond ; ops ; ops)
  // vif(cond ; verbatim ; verbatim)
  // the index i ends after the paren

  char op = str[i];

  while(str[i] != '(') operator_tmp += str[i++];
  operator_tmp += str[i++];

  if(op == '~'){
    extract_simple_ops_verbatim(delims, is_pblm, str, i, n, operator_tmp, ")");
    if(is_pblm) return;

    operator_tmp += str[i++];
  } else {
    // if or vif

    extract_r_expression(delims, is_pblm, str, i, n, operator_tmp, ";", false, false);
    if(is_pblm) return;
    
    bool is_decorative_space = str[i - 1] == ' ' && i + 1 < n && str[i + 1] == ' ';
    if(is_decorative_space){
      // we skip the decorative space
      ++i;
    }

    ++i;
    operator_tmp += "_;;;_";
    
    // if  => simple operators
    // vif => verbatim
    
    if(op == 'i'){
      extract_simple_ops_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";)");
    } else {
      extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";)", false, false, false);
    }
    
    if(is_pblm) return;
    ++i;
    
    if(str[i - 1] == ';'){
      
      if(is_decorative_space && str[i] == ' ' && str[i - 2] == ' '){
        if(op == 'v'){
          // there is no trailing WS for regular operations 
          operator_tmp.pop_back();
        }
        ++i;
      }
      
      operator_tmp += "_;;;_";
      // last round
      if(op == 'i'){
        extract_simple_ops_verbatim(delims, is_pblm, str, i, n, operator_tmp, ")");  
      } else {
        extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ")", false, false, false);
      }
      
      if(is_pblm) return;
      operator_tmp += str[i++];
    } else {
      operator_tmp += ")";
    }
    
  }

}

void extract_simple_ops_verbatim(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending_str){
  // extracts simple operations
  // the basic operations can be of the form:
  // 'arg'op, arg op
  // they are comma separated. After the comma, tabs and newlines are allowed
  // There are special operations which can include parentheses:
  // - ~(simple_ops)
  // - if(condition ; simple_ops ; simple_ops)
  //
  // THE INDEX i OUT IS AT THE ENDING!

  while(i < n){
    // each loop corresponds to a single command

    // we go to the first non blank
    while(is_blank(str, i)){
      operator_tmp += str[i++];
    }

    if(i == n) break;

    if(is_in_string(str, i, ending_str)){
      // we're good
      break;
    } else if(delims.is_close(str, i, n, false)){
      // we're a the bound of the box => can be a single variable and not an operation
      is_pblm = true;
      return;
    } else if(is_paren_operator(str, i, n)){
      // NOTE: we have a guarantee n > i+3

      extract_paren_operator(delims, is_pblm, str, i, n, operator_tmp);
      if(is_pblm) return;
      
      if(i < n && str[i] == ','){
        operator_tmp += str[i++];
      }
    } else {
      // regular, simple operation
      // format: /'arg'op/ or /arg op/

      extract_single_simple_operation(delims, is_pblm, str, i, n, operator_tmp, ending_str);

      if(str[i] == ','){
        operator_tmp += str[i++];
      } else {
        // means ENDING or box close
        break;
      }
    }
  }

  if(i == n){
    is_pblm = true;
    return;
  }

}

void extract_r_expression(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                          std::string &operator_tmp, std::string ending_str, 
                          bool check_closing, bool include_closing){
  // extracts a value after a '?' It must always be an R expression
  // the string must contain a closing box to be valid (hence cannot end at n)
  // the index i is located just after the variable
  // this is a light and mini R parser (note that it does not check the syntax)
  //
  // "bonjour {enum ? persons}"
  //                 i (in)   i (out)
  // the index is at the bound
  //
  
  bool check_ending = ending_str.size() > 0;

  // square brackets and curly brackets
  int n_cb_open = 0;
  int n_sb_open = 0;
  int n_par_open = 0;

  while(i < n){

    while(is_blank(str, i)){
      // we go to the first non blank
      operator_tmp += str[i++];
    }

    if(i == 0) break;

    if(str[i] == '#'){
      // we take the full line w/t checking anything
      // limitation of current algo: "# tricky line: \\\n"
      // but who would write that in a comment.... 
     while(str[i] != '\n' && str[i - 1] != '\\'){
        operator_tmp += str[i++];
      }

      if(i < n) operator_tmp += str[i++];

    } else if(is_quote(str, i)){
      // we get the quote
      extract_quote(str, i, n, operator_tmp);
      if(i == n) break;

    } else if(n_cb_open == 0 && n_sb_open == 0 && n_par_open == 0 && 
                 ((check_ending && is_in_string(str, i, ending_str)) || 
                  (check_closing && delims.is_close(str, i, n, false)))){
      // if here: we're done
      if(check_closing && delims.is_close(str, i, n, false)){
        // we're good! => we need to point at the extremity of the delimiter
        
        for(int k=1 ; k<delims.get_size_close() ; ++k){
          if(include_closing){
            operator_tmp += str[i++];
          } else {
            ++i;
          }
        }
      }
      
      break;
    } else {
      if(str[i] == '{'){
        ++n_cb_open;
      } else if(str[i] == '}'){
        --n_cb_open;
      } else if(str[i] == '['){
        ++n_sb_open;
      } else if(str[i] == ']'){
        --n_sb_open;
      } else if(str[i] == '('){
        ++n_par_open;
      } else if(str[i] == ')'){
        --n_par_open;
      }

      operator_tmp += str[i++];
    }
  }

  if(i == n){
    is_pblm = true;
  }

}

void extract_box_verbatim(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                                std::string &operator_tmp){
  // we extract what is inside a box verbatim
  // the resulting index i is AFTER the closing of the box
  // boxes can be escaped as per usual

  bool check_separator = false;

  if(str[i] == '/'){
    
    // we extract the verbatim until the closing box
    extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, "", true, true, false);

  } else if(str[i] == '$' || str[i] == '#'){
    // pluralization
    operator_tmp += str[i++];
    
    while(i < n && !is_separator(str, i) && !delims.is_close(str, i, n, false)){
      if(str[i] != '('){
        operator_tmp += str[i++];
      } else {
        // we handle the (zero;singular;plural)
        // => only operator to allow parens
        operator_tmp += str[i++];

        // we extract the first item
        extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";", false, false, false);
        
        // NOTA: if i == n this means there is a pblm, so we're out
        if(is_pblm) return;

        operator_tmp += str[i++];

        // we extract the second or last item
        extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";)", false, false, false);
        if(is_pblm) return;

        if(i < n && str[i] == ';'){
          // we go for another round
          operator_tmp += str[i++];
          extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ")", false, false, false);
          if(is_pblm) return;
        }

        operator_tmp += str[i++];
      }
    }

  } else if(str[i] == '&'){
    // if-else
    // NOTA: the code differs from above via the different ending that is looked after
    operator_tmp += str[i++];

    extract_r_expression(delims, is_pblm, str, i, n, operator_tmp, ";", false, false);
    if(is_pblm) return;
    
    operator_tmp += str[i++];

    // we extract the second or last item
    extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";", true, true, false);
    if(is_pblm) return;

    if(i < n && str[i] == ';'){
      // we go for another round
      operator_tmp += str[i++];
      extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, "", true, true, false);
      if(is_pblm) return;
    }

  } else {
    // regular operators
    // "hi {enum ? x}" => enum
    // operators containing parentheses:
    // - ~(simple_ops)
    // - if(condition ; simple_ops ; simple_ops)
    // - vif(condition ; verbatim ; verbatim)

    // the fun either exits at ?/!, either at a closing box
    extract_simple_ops_verbatim(delims, is_pblm, str, i, n, operator_tmp, "?!");
    if(is_pblm) return;

    if(is_separator(str, i)){
      check_separator = true;
    }

  }

  // the index i is at the bound of the box
  // otherwise => parsing problem

  if(i == n){
    is_pblm = true;
    return;
  }

  if(check_separator && is_separator(str, i)){
    if(str[i] == '?'){
      extract_r_expression(delims, is_pblm, str, i, n, operator_tmp, "", true, true);
    } else if(str[i] == '!'){
      extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, "", true, true, false, true);
    }

    if(is_pblm) return;
  }

  // we add the last character of the closing box element
  operator_tmp += str[i++];
}

void extract_verbatim(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, std::string ending_str, 
                      bool check_closing, bool include_closing,
                      bool skip_box_open, bool skip_second_space){
  //
  // the final index i stops at the ending value (and not after it)
  // the default for skip_second_space is given in its first declaration
  
  if(skip_second_space){
    // case verbatim expression and space before
    // ex: {S, " et "c ! bonjour}
    //                  ^ we start here
    operator_tmp += str[i++];
    // sikpping the space
    if(str[i] == ' '){
      ++i;  
    }    
  }

  while(i < n){
    
    if(is_in_string(str, i, ending_str)){
      if(is_escaped(str, i)){
        // means the value has been escaped
        operator_tmp.pop_back();
        operator_tmp += str[i++];
      } else {
        // we're done
        break;
      }
    } else if(check_closing && delims.is_close(str, i, n, true)){
      // the skipping of the escape is included 
      // (I've checked it works! you move i by one, then end up in the last 'else', and all is well)
      // we need to point at the extremity of the delimiter
      for(int k=1 ; k<delims.get_size_close() ; ++k){
        if(include_closing){
          operator_tmp += str[i++];
        } else {
          ++i;
        }
      }
      
      break;
    } else if(delims.is_open(str, i, n, skip_box_open)){
      operator_tmp += str[i++];
      for(int k=1 ; k<delims.get_size_open() ; ++k){
        operator_tmp += str[i++];
      }
      extract_box_verbatim(delims, is_pblm, str, i, n, operator_tmp);
    } else {
      operator_tmp += str[i++];
    }
  }
  
  if(i == n){
    is_pblm = true;
  }  

}


void parse_box(delim &delims, bool &is_pblm, const char * str, int &i, int n,
                      std::vector<std::string> &operator_vec, std::string &expr_value,
                      bool &any_plural, std::string &error_msg){
  // modifies the operator and gives the updated i
  // the i should start where to evaluate post separator (if present)

  // Here is the exhaustive list of operations allowed:
  // - "/1, 2, 3" => the split string operator. Cannot contain anything else
  // - ".['arg1'op1, 'arg2'op2 ? x]": argument + operator
  // - ".[op1, op2, ? x]": operator without argument
  // - if(cond;ops_true;ops_false), vif(cond;verbatim_true;verbatim_false): if special operator. Can be chained.
  // - ~(simple_ops): conditional operations
  // - .[& i %% 3 == 0;':'] ifelse operator
  //   .[& condition ; verbatim1 : verbatim2]
  // - .[$op1, op2, (singular:plural)]: pluralisation

  // operators using '(':
  // - if, vif, ~

  // we first get the operator, if there is an operator
  std::string operator_tmp = "";
  
  // whether to extract the ending ? or !
  bool extract_separator = false;

  if(str[i] == '/'){
    //
    // split string
    //

    operator_tmp = "/";
    operator_vec.push_back(operator_tmp);
    ++i;
    
    // this is a simple verbatim extraction without any reparsing
    extract_verbatim(delims, is_pblm, str, i, n, expr_value, "", true, false, true);
    
    if(is_pblm || i == n){
      error_msg = "slash operator not ending correctly";
      is_pblm = true;
      return;
    }

  } else if(str[i] == '&'){
    //
    // if else
    //

    operator_tmp = "&";
    ++i;
    if(i < n && str[i] == '&'){
      operator_tmp += str[i++];
    }

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";
    
    // step 1: extraction of the condition
    extract_r_expression(delims, is_pblm, str, i, n, operator_tmp, ";", false, false);
    if(is_pblm){
      error_msg = "if-else: error extracting condition";
      return;
    }

    ++i;

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    // step 2: extraction of the first verbatim
    
    if(i < n && str[i] == ' ' && str[i - 2] == ' '){
      // we strip one WS for readability
      ++i;
    }
    
    extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";", true, false, false);
    if(is_pblm){
      error_msg = "if-else: error extracting the first part";
      return;
    }

    bool two_verbatims = str[i] == ';';

    if(two_verbatims){
      if(str[i - 1] == ' ' && (i + 1 < n && str[i + 1] == ' ')){
        // we strip 1 WS on both sides for readability
        operator_tmp.pop_back();
        i += 2;
      } else {
        ++i;
      }
    } 

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    if(two_verbatims){
      extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, "", true, false, false);
      if(is_pblm){
        error_msg = "if-else: error extracting the second part";
        return;
      } 

      operator_vec.push_back(operator_tmp);
    }

  } else if(str[i] == '$' || str[i] == '#'){
    //
    // pluralisation
    //
    
    extract_separator = true;

    operator_tmp = str[i];
    ++i;

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";
    
    while(i < n && is_blank(str[i])) ++i;

    bool in_operator = false;
    while(i < n && !is_separator(str, i)){
      
      if(delims.is_close(str, i, n, false)){
        // we point at the end of the closing box
        for(int k=1 ; k<delims.get_size_close() ; ++k){
          ++i;
        }
        break;
      }

      // pluralization: op1, op2, (sing;plural)
      
      if(str[i] == ' '){
        // let's skip WS
        ++i;
      } else if(is_quote(str, i)){
        // we extract the quoted argument
        // only if the quote is the first element, ie not yet inside an operator
        
        if(!in_operator){
          extract_quote(str, i, n, operator_tmp);
        } else {
          operator_tmp += str[i++];
        }       
        
      } else if(str[i] == '('){

        if(in_operator){
          // there was a parsing error. This MUST be without anything before
          error_msg = "pluralization: operators cannot contain parentheses";
          is_pblm = true;
          return;
        }

        in_operator = false;

        // we parse it into: 'stuff'zero 'stuff'singular, 'stuff'plural
        ++i;

        std::string op_first = "", op_second = "", op_third = "";
        bool is_third = false, is_special = false;

        //
        // step 1: extraction of the first verbatim element
        //

        extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";", false, false, false);
        if(is_pblm){
          error_msg = "pluralization: (v1;v2) error extracting the first verbatim";
          return;
        }

        op_first = operator_tmp;
        operator_tmp = "";
        ++i;

        if(i + 1 >= n){
          error_msg = "pluralization: (v1;v2) error extracting the first verbatim";
          is_pblm = true;
          return;
        }

        //
        // step 2: extraction of the second verbatim element        
        //

        // stripping WS
        if(str[i] == ' ' && str[i - 2] == ' '){
          op_first.pop_back();
          ++i;
        } else if(str[i] == ';'){
          // special case (zero;;stuff)
          is_special = true;
          is_third = true;
          ++i;
        }

        extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ";)", false, false, false);
        if(is_pblm){
          error_msg = "pluralization: (v1;v2) error extracting the second verbatim";
          return;
        }

        op_second = operator_tmp;
        operator_tmp = "";
        ++i;

        if(i == n){
          error_msg = "pluralization: (v1;v2) error extracting the second verbatim";
          is_pblm = true;
          return;
        }

        //
        // step 3: the third element
        //

        if(str[i - 1] == ';'){
          is_third = true;

          // stripping WS
          if(str[i] == ' ' && str[i - 2] == ' '){
            op_second.pop_back();
            ++i;
          }
          
          extract_verbatim(delims, is_pblm, str, i, n, operator_tmp, ")", false, false, false);
          if(is_pblm){
            error_msg = "pluralization: (v1;v2;v3) error extracting the third verbatim";
            return;
          }

          op_third = operator_tmp;
          operator_tmp = "";
          ++i;
        }

        // taikng care of the different cases

        enquote(op_first);
        enquote(op_second);

        if(is_third){
          enquote(op_third);
          
          op_first += "zero";
          if(is_special){
            op_third = op_second;
          }
          op_second += "singular";
          op_third += "plural";
        } else {
          op_first += "singular";
          op_second += "plural";
        }

        operator_vec.push_back(op_first);
        operator_vec.push_back(op_second);
        if(is_third){
          operator_vec.push_back(op_third);
        }

      } else if(str[i] == ','){
        // comma: separator of operators

        if(!operator_tmp.empty()){
          operator_vec.push_back(operator_tmp);
          operator_tmp = "";
        }

        in_operator = false;
        ++i;

        while(i < n && is_blank(str[i])) ++i;

      } else {
        // regular operators
        if(!in_operator) in_operator = true;

        operator_tmp += str[i++];
      }
    }

    if(i == n){
      error_msg = "pluralization: the interpolation ends incorrectly";
      is_pblm = true;
      return;
    }

    any_plural = true;

    // we push the last operator
    if(!operator_tmp.empty()){
      operator_vec.push_back(operator_tmp);
      operator_tmp = "";
    }

  } else {
    //
    // normal case
    //
    
    extract_separator = true;

    // we tolerate only one whitespace per operator:
    // - "{80 width ? x}": valid
    // - "{80  width ? x}": not valid
    
    int n_ops = 0;

    // normal operations must end with a separator
    while(i < n && !is_separator(str, i)){

      // stripping the WS
      while(i < n && is_blank(str[i])) ++i;

      if(delims.is_close(str, i, n, false)){
        // normal operations cannot end with a closing box
        // UNLESS we're in a simple argument evaluation box: like '{x}'
        
        for(int k=1 ; k<delims.get_size_close() ; ++k) ++i;
        
        if(n_ops == 1){
          expr_value = operator_vec[0];
          operator_vec.clear();
        } else {
          is_pblm = true;  
        }
        
        return;

      } else {
        ++n_ops;

        if(is_paren_operator(str, i, n)){
          // paren-like operations: if(cond;op;op), ~(ops), vif(cond;veb;verb)
          extract_paren_operator(delims, is_pblm, str, i, n, operator_tmp);
        } else {
          // regular, simple operation
          extract_single_simple_operation(delims, is_pblm, str, i, n, operator_tmp, "?!");
        }

        if(is_pblm) return;
        
        if(!operator_tmp.empty()){
          operator_vec.push_back(operator_tmp);  
          operator_tmp = "";
        }

        if(i < n && str[i] == ','){
          ++i;
          while(i < n && is_blank(str[i])) ++i;
        }

      }
    }

    if(i == n){
      is_pblm = true;
      return;
    }

  }

  if(extract_separator && is_separator(str, i)){
    // we end with a separator: normal case or plural case
    bool is_eval = str[i] == '?';
    operator_tmp = str[i];

    // ".[op1 ! bonjour]" => we want to strip the first space in the verbatim side
    if(!is_eval && i >= 1 && str[i - 1] == ' ' && i + 1 < n && str[i + 1] == ' '){
      ++i;
    }
    
    // we add the separator (? or !) as an operator
    operator_vec.push_back(operator_tmp);
    ++i;

    if(is_eval){
      extract_r_expression(delims, is_pblm, str, i, n, expr_value, "", true, false);
    } else {
      extract_verbatim(delims, is_pblm, str, i, n, expr_value, "", true, false, true);
    }
  }

}

// [[Rcpp::export]]
List cpp_string_magic_parser(SEXP Rstr, SEXP Rdelimiters, bool only_last_parsed_section = false){
  // Rstr: string from R of length 1

  List res;
  // const char *str = CHAR(STRING_ELT(Rstr, 0));
  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));

  delim delims(Rdelimiters);

  // pluralization flag
  bool any_plural = false;

  std::string sop_value = "";
  std::string error_msg = "";

  int n = std::strlen(str);

  int i = 0;  
  while(i < n){

    // if not currently open => we check until open
    std::string string_value = "";
    while(i < n && !delims.is_open(str, i, n, true)){
      string_value += str[i++];
    }
    
    if(!string_value.empty()){
      res.push_back(std_string_to_r_string(string_value));
    }

    if(i < n){
      // there was one open
      i += delims.get_size_open();
      int i_start = i;
    
      bool is_pblm = false;

      List sop_element;
      std::vector<std::string> operator_vec;
      std::string expr_value;

      // modifies i, expr_value and operator_vec "in place"
      parse_box(delims, is_pblm, str, i, n, operator_vec, expr_value, any_plural, error_msg);
      
      if(is_pblm){
        // the parsing could not be achieved.... 
        // => we take the value inside the box as an r expression
        // => Note it is often not really a parsing error but a valid behavior, 
        //    when the operations are empty, like in {bonjour}
        
        if(error_msg.empty()){
          // we try to extract an R expression and watch out for a possible extra error
          std::vector<std::string> empty_vec;
          sop_element.push_back(empty_vec);
          
          is_pblm = false;
          i = i_start;
          extract_r_expression(delims, is_pblm, str, i, n, expr_value, "", true, false);
          
          if(is_pblm){
            error_msg = "no closing bracket";
          }          
        }
        
        if(error_msg.empty()){
          // we move on
          sop_element.push_back(std_string_to_r_string(expr_value));   
        } else {
          // there is an error and we need to report it now
          res = List();
          
          std::vector<std::string> info_error;
          info_error.push_back(error_msg);
          
          std::string parsed_section = "";
          for(int k=(i_start - delims.get_size_open()) ; k<=i && k<n ; ++k){
            parsed_section += str[k];
          }
          
          info_error.push_back(parsed_section);
          
          res.push_back(std_string_to_r_string(info_error));
          res.attr("error") = true;
          return res;          
        }        
              
      } else {
        sop_element.push_back(std_string_to_r_string(operator_vec));
        sop_element.push_back(std_string_to_r_string(expr_value));
      }
      
      if(only_last_parsed_section){
        std::string parsed_section = "";
        for(int k=(i_start - delims.get_size_open()) ; k<=i && k<n ; ++k){
          parsed_section += str[k];
        }
        res.push_back(std_string_to_r_string(parsed_section));
      } else {
        res.push_back(sop_element);  
      }
      
      ++i;      
    }
    
  }

  res.attr("plural") = any_plural;

  return res;
}

//
// END PARSER
//


/*
In the section below are utility functions to extend the parsing to set pieces
*/


// [[Rcpp::export]]
SEXP cpp_extract_quote_from_op(SEXP Rstr){

  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  std::string res;

  if(!is_quote(str, 0)){
    for(int i=0 ; i<n ; ++i) res += str[i];

  } else {
    int i = 0;
    extract_quote(str, i, n, res, true);
  }

  return std_string_to_r_string(res);
}

// detects 'if(' or 'vif('
inline bool is_if_operator(const char * str, int i, int n){
  if(n - i < 4) return false;
  
  bool offset = str[i] == 'v';
  
  return str[i + offset] == 'i' && str[i + offset + 1] == 'f' && str[i + offset + 2] == '(';
}

// [[Rcpp::export]]
List cpp_parse_operator(SEXP Rstr){
  // returns a vectors of the values of the operator
  // it always return a list of length 4:
  // - operator: str
  // - options: str vector
  // - argument: str
  // - eval: logical
  // 
  // ex: 
  // "80 width.#" 
  // => list(operator = "width", options = "#", argument = "80", eval = FALSE)
  // 
  // "enum.5.i.or"
  // => list(operator = "enum", options = c("5", "i", "or"), argument = "", eval = FALSE)
  //
  // We also handle SPECIAL CASES:
  // - "%.3f" => operator = "%", options = "", argument = ".3f"
  // - ~(5 first, enum.i.or) => operator "~(5 first, enum.i.or)", options = "", argument = ""
  //   => same for @, & and <
  // - "5ko" => operator = "ko", options = "", argument = "5"
  // - backticks can be used to be free to use ' and " in quotes
  //   the syntax is `!my'stuff" is ok`, ie ! right after the backtick


  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);
  
  std::string argument;
  int i = 0;
  bool is_eval = false;

  std::vector<std::string> options;
  
  //
  // quote extraction (if available)
  //

  if(is_quote(str, i)){
    is_eval = str[i] == '`';
    extract_quote(str, i, n, argument, true);
  }
  
  // special case: `!stuff`
  if(!argument.empty() && is_eval && argument[0] == '!'){
    std::string new_arg;
    for(size_t j=1 ; j<argument.length() ; ++j) new_arg += argument[j];
    is_eval = false;
    argument = new_arg;
  }

  
  //
  // operator extraction (beware the case "80 width")
  //
  
  // we skip a possible leading space (can't be more than 1)
  if(str[i] == ' ') ++i;

  std::string op, op_raw;
    
  if(str[i] == '~' || is_if_operator(str, i, n)){
    // special case
    while(i < n && str[i] != '('){
      op += str[i++];
    }
    
    ++i;
    argument = "";
    while(i < n - 1){
      argument += str[i++];
    }

  } else if(str[i] == '%' && argument.empty()){
    // special case: "%.3f", here .3f is an argument and NOT an option!
    // we send everything after '%' in the options

    op = "%";
    ++i;

    while(i < n){
      argument += str[i++];
    }

  } else {
    // we extract up to the ' ' if there is one
    // then we extract up to the '.' that defines the options
    // why? "0.8 width" => we don't want to have '.' as an option here

    int i_start = i;
    while(i < n && str[i] != ' '){
      op_raw += str[i++];
    }

    if(i < n && str[i] == ' '){
      // case "80 width"
      argument = op_raw;
      ++i;
      op = "";

      while(i < n && str[i] != '.'){
        op += str[i++];
      }
    } else {
      // we restart the parsing
      i = i_start;
      
      // special case: integer stuck to operator ("5ko")
      if(is_digit(str, i) && argument.empty()){
        argument += str[i++];
        while(i < n && is_digit(str, i)){
          argument += str[i++];
        }
      }
      
      while(i < n && str[i] != '.'){
        op += str[i++];
      }
      
    }
  }
  
  //
  // options
  //
  
  if(i < n && str[i] == '.'){
    ++i;
    
    while(i < n){
      std::string opt_tmp;

      while(i < n && str[i] != '.'){
        opt_tmp += str[i++];
      }

      options.push_back(opt_tmp);
      opt_tmp = "";
      ++i;
    }
  }

  //
  // save
  //

  List res;
  res["operator"] = std_string_to_r_string(op);
  res["options"] = std_string_to_r_string(options);
  res["argument"] = std_string_to_r_string(argument);
  res["eval"] = is_eval;

  return res;
}

// [[Rcpp::export]]
List cpp_extract_pipe(SEXP Rstr, bool check_double = false){
  // we extract up to the first non escaped pipe, pipes can occur afterwards no problem
  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);
  
  List res;
  
  std::string value;
  std::string extra;
  bool is_pipe = false;
  bool is_double = false;
  
  int i = 0;
  while(i < n){
    if(is_non_escaped_symbol('|', str, i, n, true)){
      is_pipe = true;
      ++i;
      if(check_double && i < n && str[i] == '|'){
        ++i;
        is_double = true;
      }
      break;
    } else {
      value += str[i++];
    }
  }
  
  while(i < n){
    extra += str[i++];
  }
  
  res["value"] = std_string_to_r_string(value);
  res["extra"] = std_string_to_r_string(extra);
  res["is_pipe"] = is_pipe; 
  res["is_double"] = is_double; 
  
  return res;
}

// [[Rcpp::export]]
SEXP cpp_parse_simple_operations(SEXP Rstr, SEXP Rdelimiters){
  // "'-'S, title, C" => c("'-'S", "title", "C")
  
  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);
  
  delim delims(Rdelimiters);
  
  std::vector<std::string> operator_vec;
  std::string operator_tmp;
  bool is_pblm = false;
  
  int i = 0;
  while(i < n && !is_separator(str, i)){
    
    // stripping the WS
    while(i < n && is_blank(str[i])) ++i;

    if(delims.is_close(str, i, n, false)){
      // normal operations cannot end with a closing box
      // we'll throw an error
      break;

    } else {

      if(is_paren_operator(str, i, n)){
        // paren-like operations: if(cond;op;op), ~(ops), vif(cond;veb;verb)
        extract_paren_operator(delims, is_pblm, str, i, n, operator_tmp);
        if(i == n && str[n - 1] != ')'){
          i = 0;
          operator_vec.push_back(operator_tmp);  
          break;
        }
      } else {
        // regular, simple operation
        extract_single_simple_operation(delims, is_pblm, str, i, n, operator_tmp, "?!");
      }

      if(!operator_tmp.empty()){
        operator_vec.push_back(operator_tmp);  
        operator_tmp = "";
      }

      if(is_pblm && i < n){
        // we'll throw an error only if not gone until the end
        break;
      }
      
      if(i < n && str[i] == ','){
        ++i;
        while(i < n && is_blank(str[i])) ++i;
      }

    }
  }
  
  if(i < n){
    // we must go until the end otherwise this means there is an error
    if(is_separator(str, i)){
      std::string op;
      op += str[i];
      operator_vec.push_back(op);
    }
    operator_vec.insert(operator_vec.begin(), "_ERROR_");
  }

  return std_string_to_r_string(operator_vec);
}

// [[Rcpp::export]]
SEXP cpp_magic_split(SEXP Rstr, SEXP Rsymbol, SEXP Rdelimiters){

  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str); 
  
  const char *symbol_full = Rf_translateCharUTF8(STRING_ELT(Rsymbol, 0));
  int n_symbol = std::strlen(symbol_full);
  
  if(n_symbol != 1){
    stop("Internal error: the symbol for splitting the string must be of length 1.");
  }
  
  char symbol = symbol_full[0];

  delim delims(Rdelimiters);
  
  std::vector<std::string> res;
  std::string string_tmp;
  
  int i = 0;
  while(i < n){
    
    while(i < n && !is_non_escaped_symbol(symbol, str, i, n, true) && !delims.is_open(str, i, n, true)){
      string_tmp += str[i++];
    }
    
    if(i == n || str[i] == symbol){
      // we're done
      res.push_back(string_tmp);
      string_tmp = "";
      ++i;
      
      // we strip WS
      while(i < n && is_blank(str, i)) ++i;
      
      continue;
    }
    
    // If here: we're in a box!
    bool is_pblm = false;
    string_tmp += str[i++];
    for(int k=1 ; k<delims.get_size_open() ; ++k){
      string_tmp += str[i++];
    }
    
    // we extract the box, the index is after the closing box
    extract_box_verbatim(delims, is_pblm, str, i, n, string_tmp);
  }
  
  // we save the last one if needed
  if(!string_tmp.empty()){
    res.push_back(string_tmp);
  }
  
  return std_string_to_r_string(res);
}
  
// [[Rcpp::export]]
SEXP cpp_find_closing_problem(SEXP Rstr, SEXP Rdelimiters){
  // the string we get in there is the FULL box parsed as R code and we know it contains an error
  
  const char *str = Rf_translateCharUTF8(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);  
  
  delim delims(Rdelimiters);
  int i = 0;
  if(delims.is_open(str, i, n, false)){
    i = delims.get_size_open();
  }  
  
  std::string res = "";
  std::string tmp;
  
  // square brackets and curly brackets
  int n_cb_open = 0;
  int n_sb_open = 0;
  int n_par_open = 0;

  while(i < n){

    while(is_blank(str, i)){
      // we go to the first non blank
      ++i;
    }

    if(is_quote(str, i)){
      // we get the quote
      res = str[i];
      extract_quote(str, i, n, tmp);
      if(i == n) break;
      res = "";

    } else if(n_cb_open == 0 && n_sb_open == 0 && n_par_open == 0 && delims.is_close(str, i, n, false)){
      // we're good!
      res = "";
      break;

    } else {
      if(str[i] == '{'){
        ++n_cb_open;
      } else if(str[i] == '}'){
        --n_cb_open;
      } else if(str[i] == '['){
        ++n_sb_open;
      } else if(str[i] == ']'){
        --n_sb_open;
      } else if(str[i] == '('){
        ++n_par_open;
      } else if(str[i] == ')'){
        --n_par_open;
      }

      ++i;
    }
  }
  
  if(res.empty()){
    if(n_cb_open > 0){
      res = "{";
    } else if(n_sb_open > 0){
      res = "[";
    } else if(n_par_open > 0){
      res = "(";
    } else if(!delims.is_close(str, i, n, false)){
      res = "delim";
    }
  }

  return std_string_to_r_string(res);
}
