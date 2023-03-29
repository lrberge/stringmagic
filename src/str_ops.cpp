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

inline bool is_box_open(const int box_type, const char * str, int i, int n){

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

inline bool is_box_close(const int box_type, const char * str, int i){
  return box_type == DSB ? str[i] == ']' : str[i] == '}';
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
  // values of i:        i (in, at the quote)               i (out, after the quote)
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
  // .[&test ; a.[']'R ? x]] will not be parsed properly bc the ';' or ']' in operators don't work
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
        // why i < 3, because we require a non empty condition! '&a;'
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

      ++i;

      if(i == n){
        // means is_box_close -> true: we're done
        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
        break;
      }

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

    operator_tmp = "$";
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

        if(i == n || !(str[i] == ';' || str[i] == ')')){
          // parsing error
          any_operator = false;
          break;
        }

        operator_tmp += "singular";

        operator_vec.push_back(operator_tmp);
        operator_tmp = "";
        ++i;

        // OK, the first part is valid

        // is there a second part?
        if(str[i - 1] == ';'){

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
            while(i < n && str[i] != ')'){
              operator_tmp += str[i++];
            }
            operator_tmp += '"';
          }

          if(i == n || (str[i] != ')' || str[i] != ';')){
            // parsing error
            any_operator = false;
            break;
          }

          operator_tmp += "plural";

          operator_vec.push_back(operator_tmp);
          operator_tmp = "";
          ++i;
        }

        // OK, the second part is valid

        // is there a third part?
        if(str[i - 1] == ';'){

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

          if(i == n || (str[i] != ')' || str[i] != ';')){
            // parsing error
            any_operator = false;
            break;
          }

          operator_tmp += "plural";

          operator_vec.push_back(operator_tmp);
          operator_tmp = "";
          ++i;
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
          } else if(!any_ws){
            // tolerance for single WS in operator
            any_ws = true;
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
      while(i < n && !is_box_open(box_type, str, i, n)){
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
        } else if(is_box_close(box_type, str, i)){
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































