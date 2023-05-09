#include <Rcpp.h>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
using namespace Rcpp;


// we need cpp11 for string.pop_back()
// [[Rcpp::plugins("cpp11")]]

// DEFINITIONS
// - box = operator used to interpolate the variables
//   ex: {var}, here {} is the box. .[var], here .[] is the box
// - operator: code name of the operation to be applied to the interpolated variable
//   ex: {S, C!1, 2, 3}, here S and C are the operators
// - string_ops/sop: sting operations
//   general name of the process of parsing the full string into box, operators and the rest
// - separator: either ? or !
//   whether to evaluate from the environment or use verbatim

// here DSB is equal to 2 => we will use it!!!
enum {
CUB = 1, DSB = 2
};

inline bool is_box_open(const int box_type, const char * str, int &i, int n, bool skip = false){

  if(skip && str[i] == '\\'){
    // here we need to 'avoid' the backslash since it is only used to
    // escape the opening box (note that in regex this is not checked)
    if(box_type == DSB && i + 2 < n && str[i + 1] == '.' && str[i + 2] == '['){
      ++i;
    } else if(box_type == CUB && i + 1 < n && str[i + 1] == '{'){
      ++i;
    }
    return false;
  }

  if(box_type == DSB){
    if(i + 2 < n){
      return str[i] == '.' && str[i + 1] == '[';
    } else {
      return false;
    }
  } else {
    // curly brackets
    if(i + 1 < n){
      return str[i] == '{';
    } else {
      return false;
    }
  }
}

inline bool is_box_bracket_open(const int box_type, const char * str, int i){
  return box_type == DSB ? str[i] == '[' : str[i] == '{';
}

inline bool is_box_bound(const int box_type, const char * str, int i, int n){

  if(box_type == DSB){
    if(str[i] == ']'){
      return true;
    } else if(i + 2 < n){
      return str[i] == '.' && str[i + 1] == '[';
    } else {
      return false;
    }
  } else {
    // curly brackets
    if(str[i] == '}'){
      return true;
    } else if(i + 1 < n){
      return str[i] == '{';
    } else {
      return false;
    }
  }
}

inline char get_closing_box_char(const int box_type){
  return box_type == DSB ? ']' : '}';
}

inline std::string get_closing_box_string(const int box_type){
  return box_type == DSB ? "]" : "}";
}

inline bool is_box_close(const int box_type, const char * str, int &i, bool skip = false){
  // sikp: we skip the backslash which can be used for escaping

  if(skip && str[i] == '\\'){
    int n = std::strlen(str);
    if( i + 1 < n && ((box_type == DSB && str[i + 1] == ']') || (box_type == CUB && str[i + 1] == '}')) ){
      ++i;
    }
    return false;
  }

  return box_type == DSB ? str[i] == ']' : str[i] == '}';
}


inline bool is_box_close_verbatim(const int box_type, const char * str, int &i){
  // sikp: we skip the backslash which can be used for escaping

  bool is_close = box_type == DSB ? str[i] == ']' : str[i] == '}';

  return is_close && !(i > 0 && str[i - 1] == '\\');
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

inline bool is_valid_operator_paren(std::string &x){
  // valid ones:
  // - @<=3(), #==4(), <regex>(), ~(), @(), #()
  // x: operator_tmp
  //    the (so far) extracted operator, ex @<=4
  //    by construction, it must start by the operator associated to the paren

  int n = x.size();

  if(n == 0) return false;

  if(!(x[0] == '@' || x[0] == '&' || x[0] == '<' || x[0] == '~')){
    return false;
  }

  if(n == 1){
    // <> always require 2+
    return x[0] != '<';
  }

  if(x[0] == '~'){
    // always unitary
    return false;
  }

  if(x[0] == '<'){
    // end with a non escaped >
    if(x[n - 1] != '>' || x[n - 2] == '\\'){
      return false;
    }

    // we trim 'readability' WS
    if(n >= 5 && x[1] == ' ' && x[n - 2] == ' '){
      std::string x_cpy{'<'};
      for(int i=2 ; i < n - 2 ; ++i){
        x_cpy += x[i];
      }
      x_cpy += '>';

      x = x_cpy;
    }

    return true;
  }

  // here x[0] is equal to @ or #
  if(n < 3) return false;

  int i = 0;
  if(x[2] == '='){
    if(!(x[1] == '<' || x[1] == '>' || x[1] == '=') || n < 4){
      return false;
    }
    i = 3;
  } else {
    // case: @<5
    if(!(x[1] == '<' || x[1] == '>')){
      return false;
    }
    i = 2;
  }

  // we check this is composed only of digits
  for(; i < n ; ++i){
    if(!(x[i] >= '0' && x[i] <= '9')){
      return false;
    }
  }

  return true;

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

inline bool ending_ifelse_part(int part, const int box_type, const char * str, int &i){

  if(part == 1){
    return str[i] == ';' || is_box_close(box_type, str, i);
  } else {
    return is_box_close(box_type, str, i);
  }

}

void extract_ifelse_section(const int box_type, const char * str, int &i, int n,
                            std::string &operator_tmp, int part){
  // extracts the content of a 'box' in the ifelse statement
  // there are limitations in the parsing of this function:
  // .[=test ; a.[']'R ? x]] will not be parsed properly bc the ';' or ']' in operators don't work
  // In this case, we need to escape them explicitly

  int n_open = 0;

  while(i < n){
    if(ending_ifelse_part(part, box_type, str, i)){
        if(str[i - 1] == '\\'){
          // means the value has been escaped
          operator_tmp.pop_back();
          operator_tmp += str[i++];
        } else {
          // we're done
          break;
        }
    } else if(is_box_open(box_type, str, i, n)){

      if(str[i - 1] == '\\'){
        // means the value has been escaped
        operator_tmp.pop_back();
        operator_tmp += str[i++];
      } else {
        // valid opening box:
        // we go all our way until we find a closing statement
        n_open = 1;
        operator_tmp += str[i++];
        if(box_type == DSB){
          operator_tmp += str[i++];
        }

        while(n_open > 0 && i < n){

          // we bookkeep the brackets
          if(is_box_bracket_open(box_type, str, i)){
            if(str[i - 1] == '\\'){
              // escaped
              operator_tmp.pop_back();
            } else {
              ++n_open;
            }
          } else if(is_box_close(box_type, str, i)){
            if(str[i - 1] == '\\'){
              // escaped
              operator_tmp.pop_back();
            } else {
              --n_open;
            }
          }

          operator_tmp += str[i++];

          if(n_open == 0){
            break;
          }
        }
      }
    } else {
      // regular case
      operator_tmp += str[i++];
    }
  }

}

void extract_operator(const int box_type, const char * str, int &i, int n,
                      std::vector<std::string> &operator_vec,
                      bool &is_eval, bool &any_plural, bool no_whitespace = false){
  // modifies the operator and gives the updated i
  // the i should start where to evaluate post separator (if present)

  // no_whitespace: used when we consider the full string for parsing.
  // if it starts with a ' ' this means that it is a regular string and there are no operators
  // ex: " hej! How are you?" => not and operator
  //     "hej! How are you?"  => could have been, but will lead to parsing failure

  // Here is the exhaustive list of operations allowed:
  // - "/1, 2, 3" => the split string operator. Cannot contain anything else
  // - ".['arg1'op1, 'arg2'op2 ? x]": argument + operator
  // - ".[op1, op2, ? x]": operator without argument
  // - @<=5(true:false) (also works with #): if special operator. Can be chained.
  //   ex: ".[@<=3(q) ? x]"
  // - @(true:false): special if without comparator
  // - <regex>(true:false): if special operator. Can be chained
  //    ex: ".[<bon.+r>(Q:D) ? x]" 
  //    => I'm still hesitating on the form of the operator.
  // - .[& i %% 3 == 0;':'] ifelse operator
  //   .[& condition ; verbatim1 : verbatim2]
  // - .[~(chain_operators)]: the conditional operator
  //   all operations are applied conditionally
  // - .[$op1, op2, (singular:plural)]: pluralisation

  // operators using '(':
  // - @, #, <>, ~

  // we first get the operator, if there is an operator
  std::string operator_tmp = "";

  int i_start = i;
  bool any_operator = true;

  if(str[i] == '/'){
    //
    // split string
    //

    operator_tmp = "/";
    operator_vec.push_back(operator_tmp);
    ++i;

  } else if(str[i] == '='){
    //
    // if else
    //

    operator_tmp = "=";
    ++i;
    if(i < n && str[i] == '='){
      operator_tmp += '=';
      ++i;
    }

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    bool loop_once = true;
    while(loop_once){
      // I just use a while to make short circuits with break
      loop_once = false;

      while(i < n && str[i] != ';') operator_tmp += str[i++];

      if(i >= n - 1 || i < 3){
        // why n-1? because we require stg after the ';'
        // why i < 3, because we require a non empty condition! '=a;'
        any_operator = false;
        break;
      }

      operator_vec.push_back(operator_tmp);
      ++i;
      operator_tmp = "";

      if(str[i - 2] == ' ' && str[i] == ' '){
        // we skip the WS
        ++i;
      }

      //
      // the second part
      //

      extract_ifelse_section(box_type, str, i, n, operator_tmp, 1);

      if(i == n || (i == n - 1 && !is_box_close(box_type, str, i))){
        any_operator = false;
        break;
      }

      if(is_box_close(box_type, str, i)){
        ++i;
        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
        break;
      }

      ++i;

      //
      // the third part, here str[i - 1] == ';'
      //

      // we trim 1 WS on each side of ' ; '
      if(str[i - 2] == ' ' && str[i] == ' '){
        if(!operator_tmp.empty()){
          operator_tmp.pop_back();
          ++i;
        }
      }

      operator_vec.push_back(operator_tmp);
      operator_tmp = "";

      extract_ifelse_section(box_type, str, i, n, operator_tmp, 2);

      if(i == n){
        any_operator = false;
        break;
      }

      // we're good
      operator_vec.push_back(operator_tmp);
      ++i;
    }

    if(any_operator){
      // we go one back, so that in the next algorithm,
      // we're at the bound of the box

      --i;
    }


  } else if(str[i] == '$' || str[i] == '#'){
    //
    // pluralisation
    //

    operator_tmp = str[i];
    ++i;

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    bool in_operator = false;
    while(i < n && !is_separator(str, i) && !is_box_close(box_type, str, i)){

      // pluralization: op1, op2, (sing;plural)

      if(str[i] == ' '){
        // we strip white spaces
        ++i;
      } else if(str[i] == '('){

        if(in_operator){
          // there was a parsing error. This MUST be without anything before
          any_operator = false;
          break;
        }

        in_operator = false;

        // we parse it into: 'stuff'zero 'stuff'singular, 'stuff'plural
        ++i;

        std::string op_first = "", op_second = "", op_third = "";
        bool is_third = false, is_special = false;

        if(is_quote(str, i)){
          extract_quote(str, i, n, operator_tmp);

          // we move along, stripping the white spaces
          while(i < n && str[i] == ' ') ++i;

        } else {
          operator_tmp = '"';
          while(i < n && str[i] != ';' && str[i] != ')'){
            operator_tmp += str[i++];
          }
          operator_tmp += '"';
        }

        // // debug
        // Rcout << "first op:" << operator_tmp << "\n";

        if(i == n || str[i] != ';'){
          // parsing error: there must be something after a ';', even if empty
          any_operator = false;
          break;
        }

        op_first = operator_tmp;
        // operator_tmp += "singular";

        // operator_vec.push_back(operator_tmp);
        operator_tmp = "";
        ++i;

        // OK, the first part is valid

        // // debug
        // Rcout << "str[i] = '" << str[i] << "'\n";

        // the second part
        if(str[i - 1] == ';'){

          if(str[i] == ';'){
            // this is the special: op_second and op_third are identical
            ++i;
            is_special = true;
            is_third = true;
          }

          if(str[i] == ' ' && str[i - 2] == ' '){
            // we strip the WS: ".[$(he ; they)]" => ".[$(he;they)]"
            ++i;
          }

          // now the second part

          if(is_quote(str, i)){
            extract_quote(str, i, n, operator_tmp);
            // we move along, stripping the white spaces
            while(i < n && str[i] == ' ') ++i;

          } else {
            operator_tmp = '"';
            while(i < n && str[i] != ';' && str[i] != ')'){
              operator_tmp += str[i++];
            }
            operator_tmp += '"';
          }
          
          // // debug
          // Rcout << "second op: " << operator_tmp << "\n";
          // Rcout << "str[i] = '" << str[i] << "'\n";

          if(i == n || !(str[i] == ';' || str[i] == ')') || (is_special && str[i] != ')')){
            // parsing error
            any_operator = false;
            break;
          }

          op_second = operator_tmp;

          // operator_tmp += "plural";
          // operator_vec.push_back(operator_tmp);
          operator_tmp = "";
          ++i;
        }

        // OK, the second part is valid

        // Rcout << "str[i] = '" << str[i] << "'\n";

        // is there a third part?
        if(str[i - 1] == ';'){
          is_third = true;

          if(str[i] == ' ' && str[i - 2] == ' '){
            // we strip the WS: ".[$(no;the ; the)]" => ".[$(no;the;the)]"
            // beware: ".[no ; ;the]"
            ++i;
          }

          // now the third part

          if(is_quote(str, i)){
            extract_quote(str, i, n, operator_tmp);
            // we move along, stripping the white spaces
            while(i < n && str[i] == ' ') ++i;

          } else {
            operator_tmp = '"';
            while(i < n && str[i] != ')'){
              operator_tmp += str[i++];
            }
            operator_tmp += '"';
          }

          // // debug
          // Rcout << "third operator: " << operator_tmp << "\n";
          // Rcout << "str[i] = " << str[i] << "\n";

          if(i == n || str[i] != ')'){
            // parsing error
            any_operator = false;
            break;
          }

          op_third = operator_tmp;

          // operator_tmp += "plural";

          // operator_vec.push_back(operator_tmp);
          operator_tmp = "";
          ++i;
        }

        // taikng care of the different cases

        if(is_third){
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

      } else {
        // regular operators
        if(!in_operator) in_operator = true;

        operator_tmp += str[i++];
      }
    }

    if(i == n){
      // parsing error
      any_operator = false;
    }

    if(any_operator){

      any_plural = true;

      // we push the last operator
      if(!operator_tmp.empty()){
        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
      }

      if(is_separator(str, i)){
        operator_tmp = str[i];
        operator_vec.push_back(operator_tmp);
        ++i;
      }

    }

  } else {
    //
    // normal case
    //

    bool is_comma = false;
    // any_ws: flag for whitespaces:
    // we tolerate only one whitespace per operator:
    // - "{80 swidth ? x}": valid
    // - "{80  swidth ? x}": not valid
    bool any_ws = false;

    if(str[i] == ' '){
      if(no_whitespace){
        // not an operator.
        // i = n to avoid the loop
        i = n;
        any_operator = false;
      } else {
        // skip the first whitespaces
        while(i < n && str[i] == ' ') ++i;
      }
    }

    // a sequence of normal operators must end with a separator
    while(i < n && !is_separator(str, i)){

      if(str[i] == '(' || (operator_tmp.empty() && (str[i] == '<' || str[i] == '@' 
                           || str[i] == '&' || str[i] == '~'))){
        // this if deals with:
        // ~(ops), @<=3(t;f), <regex>(t;f), &(t;f)

        // we deal with quotes
        // Used (so far) only in if statements

        // Note that if str[i] == '<', we parse directly here because WE WANT TO KEEP THE WHITESPACES!!!
        // We also WANT TO KEEP THE PARENTHESES!!!

        if(str[i] == '('){
          // error: paren MUST follow only @, &, <, ~
          any_operator = false;
          break;

        } else if(str[i] == '<'){
          operator_tmp += str[i++];

          while(i < n - 1 && !(str[i] == '(' && str[i - 1] == '>')){
            operator_tmp += str[i++];
          }

          // here str[i] == '('

        } else {
          // we get the operator up to the paren
          operator_tmp += str[i++];

          while(i < n - 1 && str[i] != '('){
            if(str[i] == ' '){
              // we skip the spaces
              ++i;
            } else {
              operator_tmp += str[i++];
            }
          }
        }

        if(i == n - 1){
          // error
          any_operator = false;
          break;
        }

        if(is_valid_operator_paren(operator_tmp)){
          // There is no such thing as nested (),
          // so if we find another '(' it's a problem

          operator_tmp += '(';
          ++i;

          while(i < n && str[i] != ')' && !is_separator(str, i)){
            // we don't really care about parsing here
            // we'll do that later

            if(is_quote(str, i)){
              extract_quote(str, i, n, operator_tmp);
            } else {
              // any other stuff gets in
              operator_tmp += str[i++];
            }
          }

          if(i == n || (i < n && str[i] != ')')){
            any_operator = false;
            break;
          }

          operator_tmp += str[i++];

          operator_vec.push_back(operator_tmp);
          any_ws = false;
          operator_tmp = "";

        } else {
          // Otherwise => problem
          any_operator = false;
          break;
        }

      } else if(is_quote(str, i)){
        is_comma = false;
        if(operator_tmp.length() > 0){
          // we save the existing command if needed
          operator_vec.push_back(operator_tmp);
          any_ws = false;
          operator_tmp = "";
        }

        // we get the full quoted value
        extract_quote(str, i, n, operator_tmp);

      } else if(is_box_bound(box_type, str, i, n)){
        // there should be no box bound in the operator
        // if so, this is an error. We always reach a separator first

        any_operator = false;
        break;

      } else {
        // Note: we need to be very strict on the use of commas.
        // otherwise, when string_as_box = TRUE, we will think that many things are operators when it's not.

        // comma: separation between operations
        if(str[i] == ','){
          if(operator_tmp.length() > 0){
            operator_vec.push_back(operator_tmp);
            any_ws = false;
            operator_tmp = "";
          }
          is_comma = true;
        } else if(str[i] == ' '){
          // spaces are only allowed:
          // - after a comma
          // - as trailing before the operand (? or !)
          // - a single space separating the operator from its argument
          //   ex: "{5 first}" is OK while "{5   first}" is not

          if(is_comma){
            // nothing, we move along
          } else {
            if(!any_ws && i < n + 1 && !(str[i + 1] == ' ' || str[i + 1] == ',' || is_separator(str, i + 1))){
              // tolerance for single WS in operator
              any_ws = true;
              operator_tmp += ' ';
            } else {
              // if the spaces are only trailing, OK
              while(i < n && str[i] == ' ') ++i;
              if(is_separator(str, i)){
                // OK
                break;
              } else {
                // non trailing WS, not after comma, not a single WS in operator
                // => error
                any_operator = false;
                break;
              }
            }
          }

        } else {
          is_comma = false;
          operator_tmp += str[i];
        }

        ++i;
      }
    }

    if(i == n){
      any_operator = false;
    }

    if(any_operator){
      if(operator_tmp.length() > 0){
        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
      }

      // we end with a separator
      is_eval = str[i] == '?';
      operator_tmp = str[i];

      // ".[op1 ! bonjour]" => we want to strip the first space in the verbatim side
      if(!is_eval && i >= 1 && str[i - 1] == ' ' && i + 1 < n && str[i + 1] == ' '){
        ++i;
      }

      operator_vec.push_back(operator_tmp);
      ++i;
    }
  }

  // if we didn't find a valid operator or there was a parsing error => we reset
  if(!any_operator){
    std::vector<std::string> empty_vec;
    operator_vec = empty_vec;
    i = i_start;
  }

}

// [[Rcpp::export]]
List cpp_string_ops(SEXP Rstr, bool is_dsb){
  // Rstr: string from R of length 1

  List res;
  const char *str = CHAR(STRING_ELT(Rstr, 0));

  const int box_type = is_dsb ? DSB : CUB;

  // pluralization flag
  bool any_plural = false;

  // box open flag
  int n_open = 0;

  std::string string_value = "";
  std::string sop_value = "";

  int n = std::strlen(str);

  int i = 0;
  while(i < n){

    // if not currently open => we check until open
    if(n_open == 0){

      while(i < n && !is_box_open(box_type, str, i, n, true)){
        string_value += str[i++];
      }

      res.push_back(string_value);

      if(i < n){
        // there was one open
        // box type: 1 is curly and 2 is dsb
        i += box_type;
        ++n_open;
        string_value = "";
      }

    } else {

      List sop_element;
      std::vector<std::string> operator_vec;

      // modifies i and operator_vec "in place"
      bool is_eval = true;
      extract_operator(box_type, str, i, n, operator_vec, is_eval, any_plural);

      sop_element.push_back(operator_vec);

      // init
      sop_value = "";

      // we now get the value to be evaluated or treated as verbatim

      if(is_eval || operator_vec.empty()){
        // operator_vec is empty: two reasons.
        // A) .[var], so if we have .[ 'stuff'], we want to extract the quote in full
        // B) .[op1, (op2), , , op3 ? x] => parsing error. So what we do with it does not matter

        // we strip all the white spaces
        while(i < n && str[i] == ' ') ++i;

        if(is_basic_quote(str, i)){
          // we take verbatim the full quote
          // this means it can contain .[] without issue

          char quote = str[i++];
          sop_value += quote;

          while(i < n && str[i] != quote) sop_value += str[i++];

          if(i < n){
            sop_value += quote;
            ++i;
            // we then let it go, if there is a parsing problem, we'll
            // spot that directly in R, no point to
            // doing thorough error handling here
          }
        }
      }

      while(i < n && n_open > 0){

        // we bookeep the brackets
        if(is_box_bracket_open(box_type, str, i)){
          ++n_open;
        } else if(is_box_close(box_type, str, i, true)){
          --n_open;
        }

        if(n_open == 0){
          ++i;
          break;
        }

        sop_value += str[i++];
      }

      sop_element.push_back(sop_value);

      res.push_back(sop_element);
    }
  }

  res.attr("plural") = any_plural;

  return res;
}

// [[Rcpp::export]]
List cpp_string_ops_full_string(SEXP Rstr, bool is_dsb){
  // When we consider the full string a verbatim within dsb
  // dsb("' 's, C ! 1, 2, 4")

  const char *str = CHAR(STRING_ELT(Rstr, 0));

  int n = std::strlen(str);

  const int box_type = is_dsb ? DSB : CUB;

  List dsb_element;
  std::vector<std::string> operator_vec;

  // is eval is not used here but is required in extract_operator
  bool is_eval = false;
  bool any_plural = false;
  bool no_whitespace = true;

  // modifies i and operator_vec "in place"
  int i = 0;
  extract_operator(box_type, str, i, n, operator_vec, is_eval, any_plural, no_whitespace);

  dsb_element.push_back(operator_vec);

  // init
  std::string dsb_value = "";

  // Remember that the full string is verbatim
  for(; i < n ; ++i){
    dsb_value += str[i];
  }

  dsb_element.push_back(dsb_value);

  return dsb_element;
}

inline bool is_if_separator(const char * str, int i, int n, bool case_true = false){
  if(case_true){
    return i >= n || str[i] == ';';
  } else {
    return i >= n;
  }
}

// [[Rcpp::export]]
List cpp_string_op_if_extract_old(SEXP Rstr){

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  List if_elements;
  std::vector<std::string> operator_vec;
  std::vector<std::string> empty_vec;
  std::string operator_tmp = "";

  // the code is close to extract_operator, but not identical....
  // it's a bit code duplication and I really don't like that
  // but otherwise, the code would get more ugly in extract_operator...

  bool is_comma = false;
  bool any_problem = false;

  // true
  int i = 0;
  int n_loop = 0;

  while(n_loop++ < 2){

    // 1st loop: colon is separator
    bool colon = n_loop == 1;
    // 2nd loop: EOL is separator

    while(i < n && !is_if_separator(str, i, n, colon)){

      if(is_quote(str, i)){
        is_comma = false;
        if(operator_tmp.length() > 0){
          // we save the existing command if needed
          operator_vec.push_back(operator_tmp);
          operator_tmp = "";
        }

        // we get the full quoted value
        extract_quote(str, i, n, operator_tmp);

      } else {

        // comma: separation between operations
        if(str[i] == ','){
          if(operator_tmp.length() > 0){
            operator_vec.push_back(operator_tmp);
            operator_tmp = "";
          }
          is_comma = true;
        } else if(str[i] == ' '){
          // nothing, but if NOT after a comma => error
          if(!is_comma){
            // if the spaces are only trailing, OK
            while(i < n && str[i] == ' ') ++i;
            if(is_if_separator(str, i, n, colon)){
              // OK
              break;
            } else if(i < n){
              any_problem = true;
              break;
            }
          }

        } else {
          is_comma = false;
          operator_tmp += str[i];
        }

        ++i;
      }
    }

    if(any_problem){
      List error;
      error.push_back(false);
      return(error);
    }

    while(i < n && (str[i] == ' ' || str[i] == ':')) ++i;

    if(operator_tmp.length() > 0){
      operator_vec.push_back(operator_tmp);
      operator_tmp = "";
    }

    if_elements.push_back(operator_vec);
    operator_vec = empty_vec;
  }


  return if_elements;
}



// [[Rcpp::export]]
List cpp_string_op_if_extract(SEXP Rstr){
  // in: @<=3(true ; false)
  // out: [[1]] @<=3
  //      [[2]] true
  //      [[3]] false

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  // we decrease n by one since we don't care of the closing paren
  --n;

  List if_elements;
  std::vector<std::string> operator_vec;
  std::vector<std::string> empty_vec;
  std::string operator_tmp = "";

  //
  // operator
  //

  int i = 0;
  if(str[0] == '<'){
    // special case regex
    operator_tmp += str[i++];
    while(i < n && !(str[i] == '(' && str[i - 1] == '>')) operator_tmp += str[i++];

  } else {
    // regular, easy case
    while(i < n && str[i] != '(') operator_tmp += str[i++];
  }

  operator_vec.push_back(operator_tmp);
  operator_tmp = "";
  if_elements.push_back(operator_vec);
  operator_vec = empty_vec;

  // we pass the paren
  ++i;


  //
  // true first, then false
  //

  bool any_problem = false;

  // since the code is identical for true and false, I create a loop
  for(int T_or_F = 0 ; T_or_F < 2 ; ++T_or_F){

    bool case_true = T_or_F == 0;
    bool is_comma = false;         //=> to flag the syntax errors (spaces only alled after commas)

    // we clean the leading WS
    while(i < n && str[i] == ' ') ++i;

    while(!is_if_separator(str, i, n, case_true)){

      if(is_quote(str, i)){
        is_comma = false;
        if(operator_tmp.length() > 0){
          // we save the existing command if needed
          operator_vec.push_back(operator_tmp);
          operator_tmp = "";
        }

        // we get the full quoted value
        extract_quote(str, i, n, operator_tmp);

      } else {
        // comma: separation between operations
        if(str[i] == ','){
          if(operator_tmp.length() > 0){
            operator_vec.push_back(operator_tmp);
            operator_tmp = "";
          }
          is_comma = true;
        } else if(str[i] == ' '){
          // nothing, but if NOT after a comma => error
          if(!is_comma){
            // if the spaces are only trailing, OK
            while(i < n && str[i] == ' ') ++i;
            if(is_if_separator(str, i, n, case_true)){
              // OK
              break;
            } else {
              any_problem = true;
              break;
            }
          }

        } else {
          is_comma = false;
          operator_tmp += str[i];
        }

        ++i;
      }
    }

    if(any_problem){
      List error;
      error.push_back(false);
      return(error);
    }

    // we push the last operator
    if(!operator_tmp.empty()){
      operator_vec.push_back(operator_tmp);
    }
    operator_tmp = "";
    if_elements.push_back(operator_vec);
    operator_vec = empty_vec;

    if(case_true && (i == n - 1 && str[i] == ';')){
      // we don't go through the 'false'  case
      if_elements.push_back(empty_vec);
      break;
    }

    // we pass the separator (;)
    ++i;
  }

  return if_elements;
}


// [[Rcpp::export]]
StringVector cpp_paste_conditional(StringVector x, IntegerVector id,
                                   std::string sep = "", std::string sep_last = ""){

  int n = max(id);
  StringVector res(n);
  int n_x = x.length();

  if(n_x == 0){
    return res;
  }

  const bool is_sep = !sep.empty();
  const bool is_last = !sep_last.empty() && sep.compare(sep_last) != 0;

  std::string tmp = "";
  int id_current = id[0];

  for(int i=0 ; i<n_x ; ++i){
    if(id[i] == id_current){

      if(is_sep && i > 0 && id[i - 1] == id_current){
        if(is_last && (i + 1 > n || id[i + 1] != id_current)){
          tmp += sep_last;
        } else {
          tmp += sep;
        }
      }

      tmp += x[i];

    } else {
      res[id_current - 1] = tmp;
      tmp = x[i];
      id_current = id[i];
    }
  }

  // don't forget the last item
  res[id[n_x - 1] - 1] = tmp;

  return res;
}



// [[Rcpp::export]]
std::string cpp_extract_quote_from_op(SEXP Rstr){

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  std::string res;

  if(!is_quote(str, 0)){
    for(int i=0 ; i<n ; ++i) res += str[i];

  } else {
    int i = 0;
    extract_quote(str, i, n, res, true);
  }

  return res;
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
  // "80 swidth.#" 
  // => list(operator = "swidth", options = "#", argument = "80", eval = FALSE)
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


  const char *str = CHAR(STRING_ELT(Rstr, 0));
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
  if(!argument.empty() && argument[0] == '!'){
    std::string new_arg;
    for(size_t j=1 ; j<argument.length() ; ++j) new_arg += argument[j];
    is_eval = false;
    argument = new_arg;
  }

  
  //
  // operator extraction (beware the case "80 swidth")
  //
  
  // we skip a possible leading space (can't be more than 1)
  if(str[i] == ' ') ++i;

  std::string op, op_raw;
    
  if(str[i] == '~' || str[i] == '@' || str[i] == '&' || str[i] == '<'){
    // special case => everything in the operator
    while(i < n){
      op += str[i++];
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
    // why? "0.8 swidth" => we don't want to have '.' as an option here

    int i_start = i;
    while(i < n && str[i] != ' '){
      op_raw += str[i++];
    }

    if(i < n && str[i] == ' '){
      // case "80 swidth"
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
  res["operator"] = op;
  res["options"] = options;
  res["argument"] = argument;
  res["eval"] = is_eval;

  return res;
}

inline bool is_blank(const char *str, int i){
  return str[i] == ' ' || str[i] == '\t' || str[i] == '\n';
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
StringVector cpp_normalize_ws(SEXP Rstr){

  int n_vec = Rf_length(Rstr);

  StringVector res(n_vec);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char *str = CHAR(STRING_ELT(Rstr, i_vec));
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

  return res;
}


// [[Rcpp::export]]
StringVector cpp_normalize_string(SEXP Rstr, bool clean_punct, bool clean_digit,
                                  bool clean_isolated){

  int n_vec = Rf_length(Rstr);
  StringVector res(n_vec);

  const int invalid_type = clean_punct ? (clean_digit ? PUNCT_DIGIT : PUNCT) : (clean_digit ? DIGIT : BLANK);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    const char *str = CHAR(STRING_ELT(Rstr, i_vec));
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

  return res;
}

// [[Rcpp::export]]
StringVector cpp_trimws(StringVector x){
  // this is 'fast' because the changes are in place:
  // 'res' is an alias here
  // 
  // can I skip the cstring conversion? => to investigate how to speed that up
  /// bc the computation seems unnecessary

  int n_vec = x.length();
  StringVector res(x);

  for(int i_vec=0 ; i_vec<n_vec ; ++i_vec){
    String xi = x[i_vec];
    const char* str(xi.get_cstring());
    int n = std::strlen(str);

    if(str[0] == ' ' || str[n - 1] == ' '){
      int i = 0;
      while(i < n && is_blank(str, i)) ++i;
      while(n > 0 && is_blank(str, n - 1)) --n;

      std::string tmp;
      for(; i < n ; ++i){
        tmp += str[i];
      }

      res[i_vec] = tmp;
    }
  }

  return res;
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

inline bool is_blank(char c){
  return c == ' ' || c == '\n' || c == '\t';
}

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

  if(str[n - 1 ] != '.') return false;

  if(str[n - 2 ] != '.') return false;

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

inline bool is_valid_flag_pattern(char c){
  return (c >= 'a' && c <= 'z') || c == ',' || c == ' ';
}

// [[Rcpp::export]]
List cpp_parse_str_is_pattern(SEXP Rstr, bool parse_logical){
  // Limitation: multibyte strings are not handled properly
  //  in: "fixed, word/test"
  // out: -    flags: c("fixed", "word")
  //      - patterns: "test"
  //      -    is_or: FALSE
  //
  //  in: "test & wordle"
  // out: -    flags: ""
  //      - patterns: c("test", "wordle")
  //      -    is_or: c(FALSE, FALSE)
  //
  //  in: "fiw / test | wordle"
  // out: -    flags: "fiw"
  //      - patterns: c("test", "wordle")
  //      -    is_or: c(FALSE, TRUE)

   if(Rf_length(Rstr) != 1) stop("Internal error in cpp_parse_str_is_pattern: the vector must be of length 1.");

  const char *str = CHAR(STRING_ELT(Rstr, 0));
  int n = std::strlen(str);

  List res;
  std::vector<std::string> flags;
  std::string flag_tmp;

  int i = 0;

  //
  // flags
  //

  bool is_flag = true;
  while(i < n){
    while(i < n && is_blank(str[i])) ++i;
    while(i < n && str[i] >= 'a' && str[i] <= 'z'){
      flag_tmp += str[i++];
    }
    while(i < n && is_blank(str[i])) ++i;

    if(!flag_tmp.empty()){
      flags.push_back(flag_tmp);
      flag_tmp = "";
    }

    if(i == n || (str[i] != '/' && str[i] != ',')){
      is_flag = false;
      flags.clear();
      break;
    } else if(str[i] == '/'){
      break;
    } else if(str[i] == ','){
      ++i;
      continue;
    }
  }

  // saving the flags
  res["flags"] = flags;

  if(!is_flag){
    // we start over again
    i = 0;
  } else {
    if(i > 1 && i + 1 < n && str[i - 1] == ' ' && str[i + 1] == ' '){
      // "fixed / test" => "fixed/test"
      ++i;
    }
    ++i;
  }

  //
  // logical operators
  //

  // now we go for the " & " and the " | "

  std::vector<std::string> patterns;
  std::string pat_tmp;
  std::vector<bool> is_or_all;
  bool is_or_tmp = false;

  while(i < n){
    while(i < n && (!parse_logical || (str[i] != '&' && str[i] != '|'))){
      pat_tmp += str[i++];
    }

    if(i == n){
      patterns.push_back(pat_tmp);
      is_or_all.push_back(is_or_tmp);
    } else {
      if(i > 1 && str[i - 1] == ' ' && i + 2 < n && str[i + 1] == ' '){
        //                                 \-> not a typo (we expect stuff after the operator)
        // this is a logical operator
        
        // we flush
        pat_tmp.pop_back();
        patterns.push_back(pat_tmp);
        is_or_all.push_back(is_or_tmp);

        pat_tmp = "";
        is_or_tmp = str[i] == '|';
        i += 2;
      } else {
        pat_tmp += str[i++];
      }
    }
  }

  res["patterns"] = patterns;
  res["is_or"] = is_or_all;

  return res;
}

inline bool is_ending(const char * str, int i, std::string ending){
  // only supports endings of length <= 3
  return (str[i] == ending[0] || (ending.length() > 1 && str[i] == ending[1]));
}

//
// a few forward declarations
//

inline void enquote(std::string &operator_tmp){
  // we add quotes only when operator_tmp is not quoted

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

void extract_r_expression(bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, char ending);

void extract_single_simple_operation(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending){

  // regular, simple operation
  // format: /'arg'op/ or /arg op/ or /'arg' op/

  // step 1: argument extraction (if provided)
  bool is_arg = false;
  if(is_quote(str, i)){
    is_arg = true;
    extract_quote(str, i, n, operator_tmp);
  } else {
    // we check for possible argument space separated
    while(i < n && str[i] != ',' && str[i] != ' ' && !is_ending(str, i, ending) && 
                !is_box_close(box_type, str, i, false)){
      operator_tmp += str[i++];
    }

    if(i < n && str[i] == ' '){
      operator_tmp += str[i++];
      is_arg = true;
    }
  }

  // step 2: operator extraction (if not done)
  if(is_arg){
    while(i < n && str[i] != ',' && !is_ending(str, i, ending) && 
                !is_box_close(box_type, str, i, false)){
      operator_tmp += str[i++];
    }
  }
  
  // stripping ending WS
  while(operator_tmp.length() > 0 && operator_tmp[operator_tmp.length() - 1] == ' '){
    operator_tmp.pop_back();
  }
  
  Rcout << "single_simple_op: " << operator_tmp << "\n";

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


void extract_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, std::string ending);

void extract_simple_ops_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending);

void extract_paren_operator(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp){
  // ~(simple_ops)
  // if(cond ; ops ; ops)
  // vif(cond ; verbatim ; verbatim)
  // the index i ends after the paren

  char op = str[i];

  while(str[i] != '(') operator_tmp += str[i++];
  operator_tmp += str[i++];

  if(op == '~'){
    extract_simple_ops_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ")");
    if(is_pblm) return;

    operator_tmp += str[i++];
  } else {
    // if or vif

    extract_r_expression(is_pblm, str, i, n, operator_tmp, ';');
    if(is_pblm) return;

    operator_tmp += str[i++];

    if(op == 'i'){
      // if => simple operators

      extract_simple_ops_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";)");
      if(is_pblm) return;
      operator_tmp += str[i++];

      if(str[i] == ';'){
        // last round
        extract_simple_ops_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ")");
        if(is_pblm) return;
        operator_tmp += str[i++];
      }

    } else if(op == 'v'){
      // vif => verbatim

      extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";)");
      if(is_pblm) return;
      operator_tmp += str[i++];

      if(str[i] == ';'){
        // last round
        extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ")");
        if(is_pblm) return;
        operator_tmp += str[i++];
      }
    }
  }

}

void extract_simple_ops_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                                 std::string &operator_tmp, std::string ending){
  // extracts simple operations
  // the basic operations can be of the form:
  // 'arg'op, arg op
  // they are comma separated. After the comma, tabs and newlines are allowed
  // There are special operations which can include parentheses:
  // - ~(simple_ops)
  // - if(condition ; simple_ops ; simple_ops)
  // - vif(condition ; verbatim ; verbatim)
  //
  // THE INDEX i OUT IS AT THE ENDING!

  while(i < n){
    // each loop corresponds to a single command

    // we go to the first non blank
    while(is_blank(str, i)){
      operator_tmp += str[i++];
    }

    if(i == n) break;

    if(is_ending(str, i, ending)){
      // we're good
      break;
    } else if(is_box_close(box_type, str, i, false)){
      // we're a the bound of the box => can be a single variable and not an operation
      break;
    } else if(is_paren_operator(str, i, n)){
      // NOTE: we have a guarantee n > i+3

      extract_paren_operator(box_type, is_pblm, str, i, n, operator_tmp);
      if(is_pblm) return;
      
      if(i < n && str[i] == ','){
        operator_tmp += str[i++];
      }
    } else {
      // regular, simple operation
      // format: /'arg'op/ or /arg op/

      extract_single_simple_operation(box_type, is_pblm, str, i, n, operator_tmp, ending);

      if(str[i] == ','){
        operator_tmp += str[i++];
      } else {
        // means ENDING or box close
        break;
      }
    }
  }
  
  Rcout << "simple_ops_verbatim: " << operator_tmp << "\n";

  if(i == n){
    is_pblm = true;
    return;
  }

}

void extract_r_expression(bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, char ending){
  // extracts a value after a '?' It must always be an R expression
  // the string must contain a closing box to be valid (hence cannot end at n)
  // the index i is located just after the variable
  // this is a mini R parser (note that it does not check the syntax)
  //
  // "bonjour {enum ? persons}"
  //                 i (in)   i (out)
  // the index is at the bound
  //

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

    } else if(n_cb_open == 0 && n_sb_open == 0 && n_par_open == 0 && str[i] == ending){
      // we're good!
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

// forward declaration to allow use within extract_operator_verbatim
void extract_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, std::string ending);

void extract_box_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                                std::string &operator_tmp){
  // we extract what is inside a box verbatim
  // the resulting index i is AFTER the closing of the box
  // boxes can be escaped as per usual

  bool check_separator = false;

  if(str[i] == '/'){
    while(i < n && is_box_close_verbatim(box_type, str, i)){
      operator_tmp += str[i++];
    }

  } else if(str[i] == '$' || str[i] == '#'){
    // pluralization
    operator_tmp += str[i++];
    
    while(i < n && !is_separator(str, i) && !is_box_close(box_type, str, i, false)){
      if(str[i] != '('){
        operator_tmp += str[i++];
      } else {
        // we handle the (zero;singular;plural)
        // => only operator to allow parens
        operator_tmp += str[i++];

        // we extract the first item
        extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";");
        
        // NOTA: if i == n this means there is a pblm, so we're out
        if(is_pblm) return;

        operator_tmp += str[i++];

        // we extract the second or last item
        extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";)");
        if(is_pblm) return;

        if(i < n && str[i] == ';'){
          // we go for another round
          operator_tmp += str[i++];
          extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ")");
          if(is_pblm) return;
        }

        operator_tmp += str[i++];
      }
    }

  } else if(str[i] == '&'){
    // if-else
    // NOTA: the code differs from above via the different ending that is looked after
    operator_tmp += str[i++];

    extract_r_expression(is_pblm, str, i, n, operator_tmp, ';');
    if(is_pblm) return;
    
    operator_tmp += str[i++];

    // we extract the second or last item
    std::string ending = ";" + get_closing_box_char(box_type);
    extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ending);
    if(is_pblm) return;

    if(i < n && str[i] == ';'){
      // we go for another round
      operator_tmp += str[i++];
      ending = get_closing_box_string(box_type);
      extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ending);
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
    extract_simple_ops_verbatim(box_type, is_pblm, str, i, n, operator_tmp, "?!");
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
    std::string ending = get_closing_box_string(box_type);
    if(str[i] == '?'){
      extract_r_expression(is_pblm, str, i, n, operator_tmp, ending[0]);
    } else if(str[i] == '!'){
      extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ending);
    }

    if(is_pblm) return;
  }

  // we add the closing box element
  operator_tmp += str[i++];
  
  Rcout << "box_verbatim: " << operator_tmp << "\n";
}

void extract_verbatim(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                      std::string &operator_tmp, std::string ending){
  // ending: we stop at the ending, can be at most 2 char length
  // the final index i stops at the ending value (and not after it)

  while(i < n){
    Rcout << "i = " << i << " ; verb[i] = " << str[i] << "\n";
    
    if(is_ending(str, i, ending)){
      Rcout << "verbatim: ending = true\n";
      if(str[i - 1] == '\\'){
        // means the value has been escaped
        operator_tmp.pop_back();
        operator_tmp += str[i++];
      } else {
        // we're done
        break;
      }
    } else if(is_box_open(box_type, str, i, n, true)){
      operator_tmp += str[i++];
      extract_box_verbatim(box_type, is_pblm, str, i, n, operator_tmp);
    } else {
      operator_tmp += str[i++];
    }
  }
  
  if(i == n){
    is_pblm = true;
  }  

}


void parse_box(const int box_type, bool &is_pblm, const char * str, int &i, int n,
                      std::vector<std::string> &operator_vec, std::string &expr_value,
                      bool &any_plural){
  // modifies the operator and gives the updated i
  // the i should start where to evaluate post separator (if present)

  // Here is the exhaustive list of operations allowed:
  // - "/1, 2, 3" => the split string operator. Cannot contain anything else
  // - ".['arg1'op1, 'arg2'op2 ? x]": argument + operator
  // - ".[op1, op2, ? x]": operator without argument
  // - if(cond;ops_true;ops_false), vif(cond;verbatim_true;verbatim_false): if special operator. Can be chained.
  // - ~(simple_ops): conditionnal operations
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
    while(i < n && !is_box_close(box_type, str, i, true)){
      expr_value += str[i++];
    }
    
    if(i == n){
      is_pblm = true;
      return;
    }

  } else if(str[i] == '&'){
    //
    // if else
    //

    operator_tmp = "&";
    ++i;

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";
    
    // step 1: extraction of the condition
    extract_r_expression(is_pblm, str, i, n, operator_tmp, ';');
    if(is_pblm) return;

    ++i;

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    // step 2: extraction of the first verbatim
    std::string ending = ";" + get_closing_box_char(box_type);
    extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ending);
    if(is_pblm) return;
    
    Rcout << "if-else: end first verbatim" << "\n";

    bool two_verbatims = str[i] == ';';

    if(two_verbatims && str[i - 1] == ' ' && (i + 1 < n && str[i + 1] == ' ')){
      // we strip 1 WS on both sides for readability
      operator_tmp.pop_back();
      i += 2;
    } else {
      ++i;
    }

    operator_vec.push_back(operator_tmp);
    operator_tmp = "";

    if(two_verbatims){
      extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, get_closing_box_string(box_type));
      if(is_pblm) return;

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
    while(i < n && !is_separator(str, i) && !is_box_close(box_type, str, i)){

      // pluralization: op1, op2, (sing;plural)

      if(str[i] == ' '){
        // we strip white spaces
        ++i;
      } else if(str[i] == '('){

        if(in_operator){
          // there was a parsing error. This MUST be without anything before
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

        extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";");
        if(is_pblm) return;

        op_first = operator_tmp;
        operator_tmp = "";
        ++i;

        if(i + 1 >= n){
          is_pblm = true;
          return;
        }

        //
        // step 2: extraction of the second verbatim element        
        //
        
        Rcout << "second element" << "\n";

        // stripping WS
        if(str[i] == ' ' && str[i - 2] == ' '){
          op_first.pop_back();
          ++i;
        } else if(str[i] == ';'){
          // special case (zero;;stuff)
          is_special = true;
          is_third = true;
        }

        extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ";)");
        if(is_pblm) return;

        op_second = operator_tmp;
        operator_tmp = "";
        ++i;

        if(i == n){
          is_pblm = true;
          return;
        }

        //
        // step 3: the third element
        //
        
        Rcout << "third element" << "\n";

        if(str[i - 1] == ';'){
          is_third = true;

          // stripping WS
          if(str[i] == ' ' && str[i - 2] == ' '){
            op_second.pop_back();
            ++i;
          }
          
          extract_verbatim(box_type, is_pblm, str, i, n, operator_tmp, ")");
          if(is_pblm) return;

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
    // - "{80 swidth ? x}": valid
    // - "{80  swidth ? x}": not valid

    // normal operations must end with a separator
    while(i < n && !is_separator(str, i)){

      // stripping the WS
      while(i < n && is_blank(str[i])) ++i;

      if(is_box_close(box_type, str, i)){
        // normal operations cannot end with a closing box => parsing problem
        is_pblm = true;
        return;

      } else {

        if(is_paren_operator(str, i, n)){
          // paren-like operations: if(cond;op;op), ~(ops), vif(cond;veb;verb)
          extract_paren_operator(box_type, is_pblm, str, i, n, operator_tmp);
        } else {
          // regular, simple operation
          extract_single_simple_operation(box_type, is_pblm, str, i, n, operator_tmp, "?!");
        }

        if(is_pblm) return;
        
        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
        ++i;

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
    
    std::string closing_box = get_closing_box_string(box_type);
    if(is_eval){
      extract_r_expression(is_pblm, str, i, n, expr_value, closing_box[0]);
    } else {
      extract_verbatim(box_type, is_pblm, str, i, n, expr_value, closing_box);
    }
  }

}

// [[Rcpp::export]]
List cpp_string_ops_new(SEXP Rstr, bool is_dsb){
  // Rstr: string from R of length 1

  List res;
  const char *str = CHAR(STRING_ELT(Rstr, 0));

  const int box_type = is_dsb ? DSB : CUB;

  // pluralization flag
  bool any_plural = false;

  std::string sop_value = "";

  int n = std::strlen(str);

  int i = 0;
  while(i < n){

    // if not currently open => we check until open
    std::string string_value = "";
    while(i < n && !is_box_open(box_type, str, i, n, true)){
      string_value += str[i++];
    }

    res.push_back(string_value);

    if(i < n){
      // there was one open
      // box type: 1 is curly and 2 is dsb
      i += box_type;
      int i_start = i;
    
      bool is_pblm = false;      

      List sop_element;
      std::vector<std::string> operator_vec;
      std::string expr_value;

      // modifies i, expr_value and operator_vec "in place"
      parse_box(box_type, is_pblm, str, i, n, operator_vec, expr_value, any_plural);
      
      if(is_pblm){
        // the parsing could not be achieved.... 
        // => we take the value inside the box as an r expression
        // => Note it is often not really a parsing error but a valid behavior, 
        //    when the operations are empty, like in {bonjour}
        
        std::vector<std::string> empty_vec;
        sop_element.push_back(empty_vec);
        
        is_pblm = false;
        i = i_start;
        extract_r_expression(is_pblm, str, i, n, expr_value, get_closing_box_char(box_type));
        
        Rcout << "main: is_pblm. R expr: " << expr_value << "\n";
        
        // if we end up with is_pblm again, OK, there will be an error in the R side
        sop_element.push_back(expr_value);   
              
      } else {
        sop_element.push_back(operator_vec);
        sop_element.push_back(expr_value);
      }

      res.push_back(sop_element);
      ++i;      
    }
    
  }

  res.attr("plural") = any_plural;

  return res;
}












