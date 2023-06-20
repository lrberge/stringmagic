#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 09:40:24 2022
# ~: string ops core functions
#----------------------------------------------#


####
#### User-level ####
####

#' Simple print function for objects of class `smagick`
#' 
#' Print `smagick` character vectors in a nice way.
#' 
#' @method print smagick
#' 
#' @param x A `smagick` object, obtained from the function `smagick`.
#' @param ... Not currently used.
#' 
#' @author 
#' Laurent Berge
#' 
#' @inherit str_clean seealso 
#' 
#' @examples 
#' 
#' cars = row.names(mtcars)
#' 
#' print(cars)
#' 
#' smagick("/The first cars are:, {6 first ? cars}")
#' 
#' 
print.smagick = function(x, ...){
  if(length(x) == 0){
    print(character(0))
  } else {
    cat(x, sep = "\n")
  }
}

#' Register custom operations to apply them in smagick
#' 
#' Extends the capabilities of [smagick()] by adding any custom operation
#' 
#' @param fun A function which must have at least the arguments 'x' and '...'. 
#' Additionnaly, it can have the arguments: 'argument', 'options', 'group', 'group_flag'.
#' This function must return a vector.
#' This function will be internally called by `smagick` in the form 
#' `fun(x, argument, options, group, group_flag)`.`x`: the value to which the 
#' operation applies. `argument`: the quoted `smagick` argument (always character). 
#' `options`: a character vector of `smagick` options. The two last arguments are of use
#' only in group-wise operations if `fun` changes the lengths of vectors. `group`: an index of
#' the group to which belongs each observation (integer). `group_flag`: value between 0
#' and 2; 0: no grouping operation requested; 1: keep track of groups; 2: apply grouping.
#' @param alias Character scalar, the name of the operation.
#' @param valid_options A character vector or NULL (default). Represents a list of 
#' valid options for the operation. This is used: a) to enable auto-completion,
#' b) for error-handling purposes.
#' 
#' 
#' @author 
#' Laurent R. Berge
#' 
#' @seealso 
#' `smagick()`
#' 
#' @examples
#' 
#' # let's create an operation that adds markdown emphasis
#' # to elements of a vector
#' 
#' # A) define the function
#' fun_emph = function(x, ...) paste0("*", x, "*")
#' 
#' # B) register it
#' smagick_register(fun_emph, "emph")
#' 
#' # C) use it
#' x = smagick("/right, now")
#' smagick("Take heed, {emph, c? x}.")
#' 
#' #
#' # now let's add the option "strong"
#' fun_emph = function(x, options, ...) {
#'   if("strong" %in% options){
#'     paste0("***", x, "***")
#'   } else {
#'     paste0("*", x, "*")
#'   }
#' }
#' 
#' smagick_register(fun_emph, "emph", "strong")
#' 
#' x = smagick("/right, now")
#' smagick("Take heed, {emph.strong, c? x}.")
#' 
#' #
#' # now let's add an argument
#' fun_emph = function(x, argument, options, ...){
#'   arg = argument
#'   if(nchar(arg) == 0) arg = "*"
#'   
#'   if("strong" %in% options){
#'     arg = paste0(rep(arg, 3), collapse = "")
#'   }
#'   
#'   paste0(arg, x, arg)
#' }
#' 
#' smagick_register(fun_emph, "emph", "strong")
#' 
#' x = smagick("/right, now")
#' smagick("Take heed, {'_'emph.s, c? x}.")
#' 
#' 
#' 
#' 
smagick_register = function(fun, alias, valid_options = NULL){
  # fun: must be a function with x and ... as arguments
  # the argument names must be in:
  # x, argument, options, group, conditonnal_flag

  if(missing(fun)){
    stop("The argument `fun` must be provided. PROBLEM: it is missing.")
  }

  if(!is.function(fun)){
    stop_hook("The argument `fun` must be a function. ",
              "PROBLEM: it is not a function, instead it is of class {enum.bq?class(fun)}.")
  }

  check_character(alias, scalar = TRUE, mbt = TRUE)
  check_character(valid_options, no_na = TRUE, null = TRUE)

  fun_args = names(formals(fun))
  
  arg_must = c("...", "x")
  arg_missing = setdiff(arg_must, fun_args)
  if(length(arg_missing) > 0){
    stop_hook("The argument `fun` must be a function with {enum.bq?arg_missing} in its arguments.",
              "\nPROBLEM: it has no argument {enum.bq.or?arg_missing}.")
  }

  valid_args = dsb("/x, argument, options, group, group_flag")
  arg_pblm = setdiff(setdiff(fun_args, "..."), valid_args)
  if(length(arg_pblm) > 0){
    stop_hook("The argument `fun` must have specific argument names. Valid arguments are {enum.bq.or?valid_args}.",
              "\nPROBLEM: the argument{$s, enum.bq, are?arg_pblm} invalid.")
  }

  OPERATORS = getOption("smagick_operations_origin")

  if(alias %in% OPERATORS){
    stop_hook("The argument `alias` must not be equal to an existing internal argument.",
              "\nPROBLEM: the operation {bq?alias} is already an internal operation.")
  }

  user_operators = getOption("smagick_user_ops")
  if(is.null(user_operators)){
    user_operators = list()
  }

  if(!is.null(valid_options)){
    attr(fun, "valid_options") = valid_options
  }

  user_operators[[alias]] = fun

  options("smagick_user_ops" = user_operators)
  options("smagick_operations" = c(OPERATORS, names(user_operators)))

}

####
#### ... dsb ####
####

#' @describeIn smagick Like `smagick` but interpolation is with the dot-square-bracket ".[]"
dsb = function(..., envir = parent.frame(), sep = "", vectorize = FALSE,
               slash = TRUE, collapse = NULL, help = NULL, use_DT = TRUE){


  if(!missing(vectorize)) check_logical(vectorize, scalar = TRUE)
  if(!missing(slash)) check_logical(slash, scalar = TRUE)
  if(!missing(collapse)) check_character(collapse, null = TRUE, scalar = TRUE)
  if(!missing(sep)) check_character(sep, scalar = TRUE)
  if(!missing(envir)) check_envir(envir)

  if(...length() == 0){
    if(missnull(help)){
      return("")
    }
  } else if(identical(..1, "--help")){
    sc = sys.call()
    if(identical(sc[[2]], "--help")){
      help = "_COMPACT_" # means full help
    }
  }

  set_pblm_hook()

  res = smagick_internal(..., is_dsb = TRUE, envir = envir, sep = sep,
                            vectorize = vectorize, slash = slash,
                            collapse = collapse, help = help, is_root = use_DT,
                            check = TRUE, fun_name = "dsb")

  if(inherits(res, "help")){
    return(invisible(NULL))
  }

  class(res) = c("smagick", "character")
  res
}


#' @describeIn smagick Like `dsb` but without error handling (leads to slightly faster run times).
.dsb = function(..., envir = parent.frame(), sep = "", vectorize = FALSE,
                check = FALSE, slash = FALSE, use_DT = FALSE){

  set_pblm_hook()
  smagick_internal(..., is_dsb = TRUE, envir = envir,
                      slash = slash, sep = sep,
                      vectorize = vectorize, is_root = use_DT,
                      check = check, fun_name = ".dsb")
}



####
#### ... smagick ####
####

#' String interpolation with operation chaining
#'
#' This is firstly a string interpolation tool. On top of this it can apply, and chain, over 50
#' basic string operations to the interpolated variables. Advanced support for pluralization.
#'
#' @param ... Character scalars that will be collapsed with the argument `sep`. To interpolate, you can 
#' use `"{x}"` within each character string to insert the value of `x` in the string. 
#' You can add string operations in each `"{}"` instance with the syntax `"'arg'op ? x"` 
#' (resp. `"'arg'op ! x"`) to apply the operation `'op'` with the argument `'arg'` to `x`
#'  (resp. the verbatim of `x`). Otherwise, what to say? Ah, nesting is enabled, and since 
#' there's over 50 operators, it's a bit complicated to sort you out in this small space. 
#' But type `smagick("--help")` to prompt a compact help, or use the argument `help = "keyword"`
#' (or `help = TRUE`) to obtain a selective help from the main documentation.
#' @param envir An environment used to evaluate the variables in `"{}"`. By default the variables are
#' evaluated using the environment from where the function is called.
#' @param sep Character scalar, default is the empty string `""`. It is used to collapse all
#'  the elements in `...` before applying any operation.
#' @param vectorize Logical scalar, default is `FALSE`. If `TRUE`, Further, elements in `...` are 
#' NOT collapsed together, but instead vectorised.
#' @param slash Logical, default is `TRUE`. If `TRUE`, then starting the string with a slash,
#' like in `smagick("/one, two, three")` will split the character string after the slash breaking at
#' each comma followed by spaces or newlines. The previous example leads to the string vector
#' `c("one", "two", "three")`. 
#' @param help Character scalar or `TRUE`, default is `NULL`. This argument
#' is used to generate a dynamic help on the console. If `TRUE`, the user can select which
#' topic to read from the main documentation, with the possibility to search for keywords and
#' navigate the help pages. If a character scalar, then a regex search is perfomed on the main
#' documentation and any section containining a match is displayed. The user can easily
#' navigate across matches.
#' @param use_DT Logical, default is `TRUE`. If you use `smagick` in a `data.table` call
#' and interpolate variables within the `data.table`, you want this argument to be `TRUE`.
#' It only incurs a small overhead.
#' @param collapse Character scalar, default is `NULL`. If provided, the character vector
#' that should be returned is collapsed with the value of this argument. This leads
#' to return a string of length 1.
#' @param check Logical scalar, default is TRUE. Whether to enable error-handling. 
#' Without errorhandling you can save something of the order of 40ms. Useful only
#' in long loops.
#'
#' @details 
#' There are over 50 basic string operations, it supports pluralization, string operations can be 
#' nested, operations can be applied group-wise or conditionally and
#' operators have sensible defaults. 
#'  (it may be the most powerful feature)
#' You can also declare your own operations with [smagick_register()]. They will be 
#' seamlessly integrated to `smagick`.
#'
#' Access a compact help on the console with `smagick("--help")` or use the argument `help` to which
#' you can pass keywords or regular expressions and fecth select pieces from the main documentation.
#' 
#' This function is compatible with calls within a `data.table`. Note that it 
#' shouldn't work (try [glue](https://glue.tidyverse.org/index.html) for instance)!
#' To make it work, we made use of how `data.table` works internally and, since we used stuff 
#' not exposed in its API, it is by construction unstable. `smagick` has been tested, and works, on
#' `data.table` version 1.14.2 (2021-09-27) and version 1.14.8 (2023-02-17).
#' 
#' 
#' @section Interpolation and string operations: Principle:
#' 
#' To interpolate a variable, say `x`, simply use `{x}`. For example `x = "world"; smagick("hello {x}")` leads 
#' to "hello world".
#' 
#' To any interpolation you can add operations. Taking the previous example, say we want to display
#'  "hello W O R L D". This means upper casing all letters of the interpolated variable and adding a space between 
#' each of them. Do you think we can do that? Of course yes: smagick("hello {upper, ''s, c ? x}"). And that's it.
#' 
#' Now let's explain what happened. Within the `{}` *box*, we first write a set of 
#' operations, here "upper, ''s, c", then add "?" and finally write 
#' the variable to interpolate, "x".  The operations (explained in more details 
#' below) are `upper`, upper-casing all letters, ''s: splitting
#' with the empty string, 'c': concatenating with spaces the vector of string that was just split.
#' The question mark means that the expression coming after it is to be evaluated 
#' (this is opposed to the exclamation mark presented next).
#' 
#' The syntax is always the same: {operations ? expression}, where the operations section
#' is a *comma separated* list of operations.
#' These operations are of the form `'arg'op`, with `arg` the argument to the operator 
#' code `op`. These operations are performed sequantially from left to right.
#' 
#' Some operations, like `upper`, accept options. You attach options to an operation 
#' with a dot followed by the option name. Formally: `op.option1.option2`, etc.
#' Example: `x = "hi there. what's up? fine." ; smagick("He said: {upper.sentence, Q ? x}")`.
#' Leads to: `He said: "Hi there. What's up? Fine."`.
#' 
#' Both operators and options are partially matched. So `smagick("He said: {up.s, Q ? x}")` would 
#' also work.
#' 
#' @section  Verbatim interpolation and nesting: Principle:
#' 
#' Instead of interpolating a variable, say `x`, with `{x}`, you can use an exclamation 
#' mark to trigger varbatim evaluation.
#' For example `smagick("hello {!x}")` would lead to "hello x". It's a
#'  bit disappointing, right? What's the point of doing that? Wait until the next two paragraphs.
#' 
#' Verbatim evaluation is a powerful way to apply operations to plain text. For example:
#'  `smagick("hello {upper, ''s, c ! world}")` leads to "hello W O R L D".
#' 
#' (A note in passing. The spaces surrounding the exclamation mark are non necessary,
#'  but when one space is present on both sides of the `!`, then the verbatim
#' expression only begins after it. Ex: "{upper! hi}" leads to " HI" while `"{upper ! hi}"` 
#' leads to "HI" and `"{upper !  hi}"` leads to " HI".)
#' 
#' The second advantage of verbatim evaluations is *nesting*. Anything in a verbatim 
#' expression is evaluated with the function `smagick`.
#' This means that any *box* will be evaluated as previously described. Let's
#'  give an example. You want to write the expression of a polynomial of order n: a + bx + cx^2 + etc.
#' You can do that with nesting. Assume we have `n = 2`.
#' 
#' Then `smagick("poly({n}): {' + 'c ! {letters[1 + 0:n]}x^{0:n}}")` leads to 
#' "poly(2): ax^0 + bx^1 + cx^2".
#' 
#' How does it work? The verbatim expression (the one following the exclamation mark),
#'  here `"{letters[1 + 0:n]}x^{0:n}"`, is evaluated with `smagick`.
#' `smagick("{letters[1 + 0:n]}x^{0:n}")` leads to the vector c("ax^0", "bx^1", "cx^2").
#' 
#' The operation `' + 'c` then concatenates (or collapses) that vector with ' + '.
#'  This value is then appended to the previous string.
#' 
#' We could refine by adding a cleaning operation in which we replace "x^0" and "^1" 
#' by the empty string. Let's do it:
#' 
#' `smagick("poly({n}): {' + 'c, 'x\\^0|\\^1'r ! {letters[1 + 0:n]}x^{0:n}}")` leads to 
#' "poly(2): a + bx + cx^2", what we wanted.
#' 
#' You can try to write a function to express the polynomial as before: although it is 
#' a simple task, my guess is that it will require more typing.
#' 
#' @section Operations: General syntax:
#' 
#' As seen in the previous sections, within a *box* (i.e. `"{}"`), multiple operations
#'  can be performed.
#' We can do so by stacking the operations codes and in a comma separated enumeration.
#' Operations can have arguments, and operations can also have options. The general 
#' syntax, with argument and options, is:
#' 
#' `{'arg1'op1.optionA.optionB, arg2 op2.optionC, `arg3`op3, 51op4 ? x}`
#' 
#' The argument can appear in four forms: a) inside single or double quotes just 
#' before the operation name (`arg1` above), 
#' b) verbatim, separated with a space, just before the operation name (`arg2` above), 
#' c) inside bactick quotes the argument is evaluated from the environment (`arg3` above),
#' or d) when the argument is an integer it can be juxtaposed to the opeation name (like in `op4` above).
#' 
#' The options are always dot separated and attached to the operation name, they are 
#' specific to each operation.
#' 
#' Both the operation name and the option names are partially matched.
#' 
#' @section Basic string operations:
#' 
#' This section describes some of the most common string operations: extracting, replacing, collapsing, splitting, etc.
#' These functions accept generic flags ("ignore", "fixed", "word") in their patterns (syntax: "flags/pattern"). 
#' Please see the dedicated section for more information on flags.
#' 
#' + s, S: splits the string according to a pattern. The two operations have different defaults: `' '` 
#' for `s` and `',[ \t\n}*'` for `S` (i.e. comma separation). 
#' Ex.1: `smagick("{S ! romeo, juliet}")` leads to the vector c("romeo", "juliet"). 
#' Ex.2: `smagick("{'f/+'s, '-'c ! 5 + 2} = 3")` leads to "5 - 2 = 3" (note the flag "fixed" in `s`'s pattern).
#' + c, C: to concatenate multiple strings into a single one. The two operations are 
#' identical, only their default change. c: default is `' '`, C: default is `', | and '`.
#'   The syntax of the argument is 's1' or 's1|s2'. s1 is the string used to concatenate 
#' (think `paste(x, collapse = s1)`). In arguments of the form `'s1|s2'`, `s2` will be used to concatenate the last two elements. 
#' Ex.1: `x = 1:4; smagick("Et {' et 'c ? x}!")` leads to "Et 1 et 2 et 3 et 4!".
#' Ex.2: `smagick("Choose: {', | or 'c ? 2:4}?")` leads to "Choose: 2, 3 or 4?".
#' + x, X: extracts patterns from a string. Both have the same default: `'[[:alnum:]]+'`. 
#' `x` extracts the first match while `X` extracts **all** the matches.
#'   Ex.1: `x = c("6 feet under", "mahogany") ; smagick("{'\\w{3}'x ? x}")` leads to the vector c("fee", "mah").
#'   Ex2.: `x = c("6 feet under", "mahogany") ; smagick("{'\\w{3}'X ? x}")` leads to the
#'  vector c("fee", "und", "mah", "oga").
#' + extract: extracts multiple patterns from a string, this is an alias to the operation `X` described above.
#' Use the option "first" to extract only the first match for each string (behavior becomes like `x`).
#' Ex: `x = c("margo: 32, 1m75", "luke doe: 27, 1m71") ; smagick("{'^\\w+'extract ? x} is {'\\d+'extract.first ? x}")`
#' leads to c("margo is 32", "luke is 27").
#' + r, R: replacement within a string. The two operations are identical and have no default.
#'  The syntax is `'old'` or `'old => new'` with `'old'` the pattern to find and `new` the replacement. If `new` is missing, it is 
#' considered the empty string. This operation also accepts the flag "total" which instruct to 
#' replace the fulll string in case the pattern is found.
#' Ex.1: `smagick("{'e'r ! Where is the letter e?}")` leads to "Whr is th lttr ?".
#' Ex.2: `smagick("{'(?<!\\b)e => a'R ! Where is the letter e?}")` leads to "Whara is tha lattar e?".
#' Ex.3: `smagick("{'t/e => here'r ! Where is the letter e?}")` leads to "here".
#' + clean: replacement with a string. Similar to the operation `r`, except that here the comma is
#' a pattern separator, see detailed explanations in [str_clean()]. Ex: `smagick("{'f/[, ]'clean ! x[a]}")` 
#' leads to "xa".
#' + get: restricts the string only to values respecting a pattern. This operation has no default.
#' Accepts the options "equal" and "in".
#' By default it uses the same syntax as [str_get()] so that you can use regex flags and 
#' include logical operations with `' & '` and `' | '` to detect patterns.
#' If the option "equal" is used, a simple string equality with the argument is tested (hence
#' no flags are accepted). If the option "in" is used, the argument is first split with respect to commas
#' and then set inclusion is tested. 
#' Example: `x = row.names(mtcars) ; smagick("Mercedes models: {'Merc & [[:alpha:]]$'get, '^.+ 'r, C ? x}")`
#' leads to "Mercedes models: 240D, 280C, 450SE, 450SL and 450SLC".
#' + is: detects if a pattern is present in a string, returns a logical vector. This operation has no default.
#' Accepts the options "equal" and "in".
#' By default it uses the same syntax as [str_is()] so that you can use regex flags and 
#' include logical operations with `' & '` and `' | '` to detect patterns.
#' If the option "equal" is used, a simple string equality with the argument is tested (hence
#' no flags are accepted). If the option "in" is used, the argument is first split with respect to commas
#' and then set inclusion is tested. 
#' Mostly useful as the final operation in a [str_ops()] call.
#' Example: `x = c("Mark", "Lucas") ; smagick("Mark? {'i/mark'is, C ? x}")` leads to "Mark? TRUE and FALSE".
#' + which: returns the index of string containing a specified pattern. With no default, can be applied
#' to a logical vector directly. 
#' By default it uses the same syntax as str_which() so that you can use regex flags and 
#' include logical operations with `' & '` and `' | '` to detect patterns.
#' If the option "equal" is used, a simple string equality with the argument is tested (hence
#' no flags are accepted). If the option "in" is used, the argument is first split with respect to commas
#' and then set inclusion is tested. 
#' Mostly useful as the final operation in a [str_ops()] call.
#' Ex.1: `x = c("Mark", "Lucas") ; smagick("Mark is number {'i/mark'which ? x}.")` leads to 
#' "Mark is number 1.".
#' 
#' @section Operations changing the length or the order:
#' 
#' + first: keeps only the first `n` elements. Example: `smagick("First 3 numbers: {3 first, C ? mtcars$mpg}.")`
#' leads to "First 3 numbers: 21, 21 and 22.8.". Negative numbers as argument remove the 
#' first `n` values. You can add a second argument in the form `'n1|n2'first` in which case the first `n1` and last
#' `n2` values are kept; `n1` and `n2` must be positive numbers.
#' + K, Ko, KO: keeps only the first `n` elements; has more options than `first`. The syntax is `'n'K`, 
#' `'n|s'K`, `'n||s'K`. The values Ko and KO only accept the two first syntax (with `n` only).
#' `n` provides the number of elements to keep. If `s` is provided and the number of 
#' elements are greater than `n`, then in 'n|s' the string `s` is added at the end, and
#' if 'n||s' the string s replaces the nth element.
#'   The string `s` accepts specials values:
#'   + `:n:` or `:N:` which gives the total number of items in digits or letters (N)
#'   + `:rest:` or `:REST:` which gives the number of elements that have been truncated in digits or letters (REST)
#'   Ex: `smagick("{'3|:rest: others'K ? 1:200}")` leads to the vector `c("1", "2", "3", "197 others")`.
#'   + The operator 'n'Ko is like `'n||:rest: others'K` and 'n'KO is like `'n||:REST: others'K`.
#' + last: keeps only the last `n` elements. Example: `smagick("Last 3 numbers: {3 last, C ? mtcars$mpg}.")`
#' leads to "Last 3 numbers: 19.7, 15 and 21.4.". Negative numbers as argument remove the 
#' last `n` values.
#' + sort: sorts the vector in increasing order. Accepts optional arguments and the option "num". 
#' Example: `x = c("sort", "me") ; smagick("{sort, c ? x}")` leads to "me sort". 
#' If an argument is provided, it must be a regex pattern that will be applied to
#' the vector using [str_clean()]. The sorting will be applied to the modified version of the vector
#' and the original vector will be ordered according to this sorting. 
#' Ex: `x = c("Jon Snow", "Khal Drogo")`; `smagick("{'.+ 'sort, C?x}")` leads to 
#' "Khal Drogo and Jon Snow". The option "num" sorts over a numeric version 
#' (with silent conversion) of the vector and reorders the original vector accordingly. 
#' Values which could not be converted are last.
#' **Important note**: the sorting operation is applied before any character conversion.
#' If previous operations were applied, it is likely that numeric data were transformed to character.
#' Note the difference: `x = c(20, 100, 10); smagick("{sort, ' + 'c ? x}")` leads to "10 + 20 + 100"
#' while `smagick("{n, sort, ' + 'c ? x}")` leads to "10 + 100 + 20" because the operation "n"
#' first transformed the numeric vector into character.
#' + dsort: sorts the vector in decreasing order. It accepts an optional argument and 
#' the option "num". Example: `smagick("5 = {dsort, ' + 'c ? 2:3}")` 
#' leads to "5 = 3 + 2". See the operation "sort" for a description of the argument and the option.
#' + rev: reverses the vector. Example: `smagick("{rev, ''c ? 1:3}")` leads to "321".
#' + unik: makes the string vector unique. Example: `smagick("Iris species: {unik, C ? iris$Species}.")`
#' leads to "Iris species: setosa, versicolor and virginica.".
#' + each: repeats each element of the vector `n` times. Option "c" then collapses the full vector 
#' with the empty string as a separator. Ex.1: `smagick("{/x, y}{2 each ? 1:2}")` leads to the 
#' vector `c("x1", "y1", "x2", "y2")`. Ex.2: `smagick("Large number: 1{5 each.c ! 0}")` leads to 
#' "Large number: 100000".
#' + times: repeats the vector sequence `n` times. Option "c" then collapses the full vector 
#' with the empty string as a separator. Example: `smagick("What{6 times.c ! ?}")` leads to "What??????".
#' + rm: removes elements from the vector. Options: "empty", "blank", "noalpha", "noalnum", "all".
#' The *optional* argument represents the pattern used to detect strings to be deleted. 
#' Ex.1: `x = c("Luke", "Charles")`; `smagick("{'i/lu'rm ? x}")` leads to "charles". By default it removes
#' empty strings. Option "blank" removes strings containing only blank characters (spaces, tab, newline).
#' Option "noalpha" removes strings not containing letters. Option "noalnum" removes strings not 
#' containing alpha numeric characters. Option "all" removes all strings (useful in conditions, see 
#' the dedicated section). If an argument is provided, only the options "empty" and "blank" are available.
#' Ex.2: `x = c("I want to enter.", "Age?", "21")`; `smagick("Nightclub conversation: {rm.noalpha, c ! - {x}}")` 
#' leads to "Nightclub conversation: - I want to enter. - Age?"
#' + nuke: removes all elements, equivalent to `rm.all` but possibly more explicit (not sure). 
#' Useful in conditions, see the dedicated section.
#' Example: `x = c(5, 7, 453, 647); smagick("Small numbers only: {if(.>20 ; nuke), C ? x}")` leads 
#' to "Small numbers only: 5 and 7";
#' + insert: inserts a new element to the vector. Options: "right" and "both". Option "right" adds
#' the new element to the right. Option "both" inserts the new element on the two sides of the vector.
#' Example: `smagick("{'3'insert.right, ' + 'c ? 1:2}")` leads to "1 + 2 + 3".
#' 
#' 
#' @section Formatting operations:
#'  
#' + lower: lower cases the full string.
#' + upper: upper cases the full string. Options: "first" and "sentence".
#' Option "first" upper cases only the first character. Option "sentence"
#' upper cases the first letter after punctuation. 
#' Ex: `x = "hi. how are you? fine." ; smagick("{upper.sentence ? x}")` leads
#' to "Hi. How are you? Fine.".
#' + title: applies a title case to the string. Options: "force" and "ignore".
#' Option "force" first puts everything to lowercase before applying the title case. 
#' Option "ignore" ignores a few small prepositions ("a", "the", "of", etc).
#' Ex: `x = "bryan is in the KITCHEN" ; smagick("{title.force.ignore ? x}")` leads to "Bryan Is in the Kitchen".
#' + ws: normalizes whitespaces (WS). It trims the whitespaces on the edges and transforms any succession 
#' of whitespaces into a single one. Can also be used to further clean the string with its options. 
#' Options: "punct", "digit", "isolated". Option "punct" cleans the punctuation. Option "digit" cleans digits.
#' Option "isolated" cleans isolated letters. WS normalization always come after any of these options.
#' **Important note:** punctuation (or digits) are replaced with WS and **not** 
#' the empty string. This means that `smagick("ws.punct ! Meg's car")` will become "Meg s car".
#' + trimws: trims the white spaces on both ends of the strings.
#' + q, Q, bq: to add quotes to the strings. q: single quotes, Q: double quotes, bq: 
#' back quotes. `x = c("Mark", "Pam"); smagick("Hello {q, C ? x}!")` leads to "Hello 'Mark' and 'Pam'!".
#' + format, Format: applies the base R's function [base::format()] to the string. 
#' By default, the values are left aligned, *even numbers* (differently from [base::format()]'s behavior).
#' The upper case command (`Format`) applies right alignment. Options: "0", "zero", "right", "center".
#' Options "0" or "zero" fills the blanks with 0s: useful to format numbers. Option "right" right aligns,
#' and "center" centers the strings.
#' Ex: `x = c(1, 12345); smagick("left: {format.0, q, C ? x}, right: {Format, q, C ? x}")` 
#' leads to "left: '000001' and '12,345', right: '     1' and '12,345'".
#' + %: applies [base::sprintf()] formatting. The syntax is 'arg'% with arg an sprintf formatting,
#' or directly the sprint formatting, e.g. `% 5s`. Example: `smagick("pi = {%.3f ? pi}")` leads
#' to "pi = 3.142".
#' + stopwords: removes basic English stopwords (the snowball list is used). 
#' The stopwords are replaced with an empty space but the left and right WS are 
#' untouched. So WS normalization may be needed (see operation `ws`).
#'   `x = c("He is tall", "He isn't young"); smagick("Is he {stop, ws, C ? x}?")` leads to "Is he tall and young?".
#' + ascii: turns all letters into ASCII with transliteration. Failed translations
#'  are transformed into question marks. Options: "silent", "utf8". By default, if some conversion fails
#' a warning is prompted. Option "silent" disables the warning in case of failed conversion. The conversion 
#' is done with [base::iconv()], option "utf8" indicates that the source endocing is UTF-8, can be useful 
#' in some cases.
#' + n: formats integers by adding a comma to separate thousands. Options: "letter", "upper", "0", "zero".
#' The option "letter" writes the number in letters (large numbers keep their numeric format). The option
#' "upper" is like the option "letter" but uppercases the first letter. Options "0" or "zero" left pads
#' numeric vectors with 0s. Ex.1: `x = 5; smagick("He's {N ? x} years old.")` leads to "He's five years old.".
#' Ex.2: `x = c(5, 12, 52123); smagick("She owes {n.0, '$'paste, C ? x}.")` leads to 
#' "She owes $5, $12 and $52,123.".
#' + N: same as `n` but automatically adds the option "letter".
#' + nth: when applied to a number, these operators write them as a rank. Options: "letter", 
#' "upper", "compact".
#' Ex.1: `n = c(3, 7); smagick("They finished {nth, enum ? n}!")` leads to "They finished 3rd and 7th!". 
#' Option "letter" tries to write the numbers in letters, but note that it stops at 20. Option "upper"
#' is the same as "letter" but uppercases the first letter. Option "compact" aggregates
#' consecutive sequences in the form "start_n_th to end_n_th". 
#' Ex.2: `smagick("They arrived {nth.compact ? 5:20}.")` leads to "They arrived 5th to 20th.".
#' Nth: same as `nth`, but automatically adds the option "letter". Example:
#' `n = c(3, 7); smagick("They finished {Nth, enum ? n}!")` leads to "They finished third and seventh!".
#' + ntimes: write numbers in the form `n` times. Options: "letter", "upper". Option 
#' "letter" writes the number in letters (up to 100). Option "upper" does the same as "letter" 
#' and uppercases the first letter. Example: `smagick("They lost {C ! {ntimes ? c(1, 12)} against {/Real, Barcelona}}.")`
#' leads to "They lost once against Real and 12 times against Barcelona.".
#' + Ntimes: same as `ntimes` but automatically adds the option "letter".
#' Example: `x = 5; smagick("This paper was rejected {Ntimes ? x}...")` leads to
#' "This paper was rejected five times...".
#' + cfirst, clast: to select the first/last characters of each element. 
#'   Ex: `smagick("{19 cfirst, 9 clast ! This is a very long sentence}")` leads to "very long".
#' Negative numbers remove the first/last characters.
#' + k: to keep only the first n characters (like `cfirst` but with more options). The
#'  argument can be of the form `'n'k`, `'n|s'k` or `'n||s'k` with `n` a number and `s` a string.
#'   `n` provides the number of characters to keep. Optionnaly, only for strings whose
#'  length is greater than `n`, after truncation, the string `s` can be appended at the end.
#'   The difference between 'n|s' and 'n||s' is that in the second case the strings
#'  will always be of maximum size `n`, while in the first case they can be of length `n + nchar(s)`.
#'   Ex: `smagick("{4k ! long sentence}")` leads to "long",  `smagick("{'4|..'k ! long sentence}") `
#' leads to "long..", `smagick("{'4||..'k ! long sentence}")` leads to "lo..".
#' + fill: fills the character strings up to a size. Options: "right", "center".
#' Accepts arguments of the form `'n'` or `'n|s'`, with `n` a number and `s` a symbol. 
#' Default is left-alignment of the strings. 
#' Option "right" right aligns and "center" centers the strings. When using `'n|s'`, the symbol `s`
#' is used for the filling. By default if no argument is provided, the
#' maximum size of the character string is used. See help for [str_fill()] for more information.
#' Ex.1: `smagick("Numbers: {'5|0'fill.right, C ? c(1, 55)}")` leads to "Numbers: 00001 and 00055".
#' + paste: pastes some character to all elements of the string. This operation has no default.
#' Options: "both", "right", "front", "back", "delete". By default, a string is pasted on the left.
#' Option "right" pastes on the right and "both" pastes on both sides. Option "front" only 
#' pastes on the first element while option "back" only pastes on the last element. Option "delete"
#' first replaces all elements with the empty string.
#' Example: `smagick("6 = {'|'paste.both, ' + 'c ? -3:-1}")` leads to "6 = |-3| + |-2| + |-1|".
#' 
#' 
#' @section Other operations:
#' 
#' 
#' + num: converts to numeric. Options: "warn", "soft", "rm", "clear". By default, the conversion
#' is performed silently and elements that failed to convert are turned into NA. 
#' Option "warns" displays a warning if the conversion to numeric fails. 
#' Option "soft" does not convert if the conversion of at least one element fails. 
#' Option "rm" converts and removes the elements that could not be converted. 
#' Option "clear" turns failed conversions into the empty string, and hence lead to a character vector.
#' Example: `x = c(5, "six"); smagick("Compare {num, C, q ? x} with {num.rm, C, q ? x}.")` leads to 
#' "Compare '5 and NA' with '5'.", and `smagick("Compare {num.soft, C, q ? x} with {clear, C, q ? x}.")`
#' leads to "Compare '5 and six' with '5 and '.".
#' + enum: enumerates the elements. It creates a single string containing the comma 
#' separated list of elements.
#'   If there are more than 7 elements, only the first 6 are shown and the number of
#'  items left is written.
#'   For example `smagick("enum ? 1:5")` leads to "1, 2, 3, 4, and 5".
#'   You can add the following options by appending the letter to enum after a dot:
#'   + q, Q, or bq: to quote the elements
#'   + or, nor: to finish with an 'or' (or 'nor') instead of an 'and'
#'   + i, I, a, A, 1: to enumerate with this prefix, like in: i) one, and ii) two
#'   + a number: to tell the number of items to display
#'   Ex.1: `x = c("Marv", "Nancy"); smagick("The main characters are {enum ? x}.")` leads to 
#' "The main characters are Marv and Nancy.".
#'   Ex.2: `x = c("orange", "milk", "rice"); smagick("Shopping list: {enum.i.q ? x}.")` leads to
#'  "Shopping list: i) 'orange', ii) 'milk', and iii) 'rice'."
#' + len: gives the length of the vector. Options "letter", "upper", "format".
#' Option "letter" writes the length in words (up to 100). Option "upper" is the same 
#' as letter but uppercases the first letter. Option "format" add comma separation for thousands.
#' Example: `smagick("Size = {len.format ? 1:5000}")` leads to "Size = 5,000".
#' + width: formats the string to fit a given width by cutting at word boundaries. 
#' Accepts arguments of the form `'n'` or `'n|s'`, with `n` a number and `s` a string. 
#' An argument of the form `'n|s'` will add `s` at the beginning of each line. Further,
#' by default a trailing white space is added to `s`; to remove this 
#' behavior, add an underscore at the end of it. 
#' The argument `n` is either 
#' an integer giving the target character width (minimum is 15), or it can be a fraction expressing the 
#' target size as a fraction of the current screen. Finally it can be an expression that 
#' uses the variable `.sw` which will capture the value of the current screen width.
#' Ex.1: `smagick("{15 width ! this is a long sentence}")` leads to "this is a long\\nsentence".
#' Ex.2: `smagick("{15 width.#> ! this is a long sentence}")` leads to "#> this is a long\\n#> sentence".
#' + dtime: displays a formatted time difference. Option "silent" does not report a warning if the
#' operation fails. It accepts either objects of class `POSIXt` or `difftime`.
#' Example: `x = Sys.time() ; Sys.sleep(0.5) ; smagick("Time: {dtime ? x}")` leads to something 
#' like "Time: 514ms".
#' 
#' @section Group-wise operations:
#' 
#' In `smagick`, the splitting operation `s` (or `S`) keeps a memory of the strings 
#' that were split. Use the tilde operator, of the form `~(op1, op2)`, to apply operations
#' group-wise, to each of the split strings.
#' 
#' Better with an example. `x = c("Oreste, Hermione", "Hermione, Pyrrhus", "Pyrrhus, Andromaque") ;`
#' `smagick("Troubles ahead: {S, ~(' loves 'c), C ? x}.")` leads to 
#' "Troubles ahead: Oreste loves Hermione, Hermione loves Pyrrhus and Pyrrhus loves Andromaque.".
#' 
#' Almost all operations can be applied group-wise (although only operations changing the order or 
#' the length of the strings really matter).
#' 
#' @section Conditional operations:
#' 
#' There are two operators to apply operations conditionally: `if` and `vif`, the latter
#' standing for *verbatim if*. 
#' 
#' The syntax of `if` is `if(cond ; ops_true ; ops_false)` with `cond` a
#' condition (i.e. logical operation) on the value being interpolated, `ops_true` a comma-separated
#' sequence of operations if the condition is `TRUE` and `ops_false` an *optional* a sequence of
#' operations if the condition is `FALSE`.
#' 
#' Ex.1: Let's take a sentence, delete words of less than 4 characters, and trim 
#' words of 7+ characters. 
#' x = "Songe Cephise a cette nuit cruelle qui fut pour tout un peuple une nuit eternelle"
#' `smagick("{' 's, if(.nchar<=4 ; nuke ; '7|..'k), c ? x}")`.
#' Let's break it down. First the sentence is split w.r.t. spaces, leading to a vector
#' of words. Then we use the special variable `.nchar` in `if`'s condition to refer 
#' to the number of characters of the current vector (the words). The words with 
#' less than 4 characters are nuked (i.e. removed), and the other words are
#' trimmed at 7 characters. Finally the modified vector of words is collapsed with 
#' the function `c`, leading to the result.
#' 
#' The condition `cond` accepts the following special values: `.` (the dot), `.nchar`, `.C`, `.len`, `.N`.
#' The dot, `.`, refers to the current vector. `.nchar` represent the number of characters 
#' of the current vector (equivalent to `nchar(.)`). `.C` is an alias to `.nchar`.
#' `.len` represent the length of the current vector (equivalent to `length(.)`). 
#' `.N` is an alias to `.len`.
#' 
#' If a condition leads to a result of length 1, then the operations are applied to 
#' the full string vector and not element-wise (as was the case in Ex.1). Contrary to element-wise conditions
#' for which operations modifying the length of the vectors are forbidden (apart from nuking),
#' such operations are fine in full-string conditions.
#' 
#' Ex.2: `x = smagick("x{1:10}")`; `smagick("y = {if(.N>4 ; 3 first, '...'insert.right), ' + 'c ? x}")`
#' leads to "y = x1 + x2 + x3 + ...". the same opration applied to `x = smagick("x{1:4}")`
#' leads to "y = x1 + x2 + x3 + x4".
#' 
#' For `vif`, the syntax is `vif(cond ; verb_true ; verb_false)` with `verb_true`
#' a verbatim value with which the vector will be replaced if the condition is `TRUE`. 
#' This is similar for `verb_false`. The condition works as in `if`.
#' 
#' Ex.3: `x = c(1, 25, 12, 6) ; smagick("Values: {vif(.<10 ; <10), C ? x}")` leads to 
#' "Values: <10, 25, 12 and <10". As we can see values lower than 10 are replaced
#' with "<10" while other values are not modified.
#' 
#' Ex.4: `x = smagick("x{1:10}")`; `smagick("y = {vif(.N>4 ; {/{x[1]}, ..., {last?x}}), ' + 'c ? x}")`
#' leads to "y = x1 + ... + x10".
#' Let's break it down. If the length of the vector is greater than 4 (here it's 10), then
#' the full string is replaced with `"{/{x[1]}, ..., {last?x}}"`. Interpolation applies to
#' such string. Hence the slash operation (see the dedicated section) breaks the string w.r.t.
#' the commas, leading to the vector `c("{x[1]}", "...", "{last?x}")`. Since the 
#' string contain curly brackets, interpolation is applied again. This leads to 
#' the vector `c("x1", "...", "x10")`. Finally, this vector is collapsed with ' + ' leading
#' to the final string.
#' Note that there are many ways to get to the same result. Here is another example:
#' `smagick("y = {vif(.N>4 ; {x[1]} + ... + {last?x} ; {' + 'c ? x}) ? x}")`.
#' 
#' The `vif` condition allows the use of '.' to refer to the current value in 
#' `verb_true` and `verb_false`, as illustrated by the last example:
#' 
#' Ex.5: `smagick("{4 last, vif(. %% 2 ; x{.} ; y{rev?.}), C ? 1:11}")`
#' leads to "y10, x9, y8 and x11".
#' 
#' 
#' @section Special interpolation: The slash:
#' 
#' Interpolations starting with a slash are different from regular interpolations. 
#' Ex.1: `x = 3:4; smagick("{/one, two, {x}}")` leads to the vector `c("one", "two", "3", "4")`.
#' 
#' When a "/" is the first character of an interpolation:
#' - all characters until the closing bracket are taken as verbatim
#' - the verbatim string is split according to comma separation (formally they are split with `,[ \t\n]+`),
#' resulting into a vector
#' - if the vector contains any *box*, extra interpolations are resolved and appended to the vector
#' 
#' In Ex.1 the string "one, two, {x}" is taken as verbatim and split w.r.t. commas, leading to c("one", "two", "{x}"). 
#' Since the last element contained an opening box, it is interpolated and inserted into the vector, leading
#' to the result.
#' 
#' By default, thanks to the argument `slash = TRUE`, you can apply the slash operator without
#' the need of an interpolation box (provided the slash appears as the first character), see the example below. 
#' 
#' Ex.2: `x = 3:4; smagick("/one, two, {x}")` also leads to the vector `c("one", "two", "3", "4")`.
#' 
#' @section Special interpolation: if-else:
#' 
#' Using an ampersand ("&") as the first character of an interpolation leads to an *if-else* operation.
#' Using two ampersands ("&&") leads to a slightly different operation described at the end of this section.
#' 
#' The syntax is as follows: `{&cond ; verb_true ; verb_false}` with `cond` a
#' condition (i.e. logical operation) on the value being interpolated, `verb_true`
#' a verbatim value with which the vector will be replaced if the condition is `TRUE` and 
#' `verb_false` an *optional* verbatim value with which the vector will be replaced if the condition is `FALSE`. 
#' If not provided, `verb_false` is considered to be the empty string unless the operator is 
#' the double ampersand described at the end of this section.
#' 
#' Note that in `cond`, you can use the function `len`, an alias to `length`.
#' 
#' Ex.1: `x = 1:5`; \code{smagick("x is {&len(x)<10 ; short ; {`log10(.N)-1`times, ''c ! very }long}")}
#' leads to "x is short". With `x = 1:50`, it leads to "x is long", and to "x is very very long"
#' if `x = 1:5000`.
#' 
#' If a condition leads to a result of length 1, the full string is replaced by the verbatim 
#' expression. Further, this expression will be interpolated if requested. This was the case
#' in Ex.1 where `verb_false` was interpolated.
#' 
#' If the condition's length is greater than 1, then each logical values equal to `TRUE` is replaced
#' by `verb_true`, and `FALSE` or `NA` values are replaced with `verb_false`. Note,
#' importantly, that **no interpolation is perfomed in that case**.
#' 
#' Ex.2: `x = 1:3 ; smagick("x is {&x == 2 ; two ; not two}")` leads to the vector 
#' `c("x is not two", "x is two", "x is not two")`.
#' 
#' In that example, when x is odd, it is replaced with "odd", and when even it is
#' replaced with the elements of y.
#' 
#' Using the two ampersand operator ("&&") is like the simple ampersand version but the 
#' default for `verb_false` is the variable used in the condition itself. So the syntax is
#' `{&&cond ; verb_true}` and *it does not accept* `verb_false`.
#' 
#' Ex.3: `i = 3 ; smagick("i = {&&i == 3 ; three}")` leads to "i = three", and to "i = 5" if `i = 5`. 
#' 
#' 
#' @section Special interpolation: Pluralization:
#' 
#' There is advanced support for pluralization which greatly facilitates the writing of messages 
#' in natural language.
#' 
#' There are two ways to pluralize: over length or over value. To trigger a "pluralization" interpolation
#' use as first character:
#' - `$` to pluralize over the length of a variable (see Ex.2)
#' - `#` to pluralize over the value of a variable (see Ex.1)
#' 
#' Ex.1: `x = 5; smagick("I bought {N?x} book{#s}.")` leads to "I bought five books.". 
#' If `x = 1`, this leads to "I bought one book.".
#' 
#' The syntax is `{#plural_ops ? variable}` or `{#plural_ops}` where `plural_ops` are
#' specific pluralization operations which will be described below. 
#' The pluralization is perfomed *always* with respect to the value of a variable. 
#' You can either add the variable explicitly (`{#plural_ops ? variable}`) or refer
#' to it implicitly (`{#plural_ops}`). If implicit, then the algorithm will look at the 
#' previous variable that was interpolated and pluralize over it. This is exaclty what happens in
#' Ex.1 where `x` was interpolated in `{N?x}` and plural operation `s` in `{#s}` then applied to 
#' `x`. It was equivalent to have `{#s ? x}`. If a variable wasn't interpolated before, then
#' the next interpolated variable will be used (see Ex.2). If no variable is interpolated
#' at all, an error is thrown.
#' 
#' Ex.2: `x = c("J.", "M."); smagick("My BFF{$s, are} {C?x}!")` leads to "My BFFs are J. and M.!".
#' If "x = "S.", this leads to "My BFF is S.!".
#' 
#' Pluralizing accepts the following operations:
#' - s, es: adds an "s" (or "es") if it is plural (> 1), nothing otherwise. Accepts the option `0` or `zero` which 
#' treats a 0-length or a 0-value as plural.
#' - y or ies: adds an 'y' if singular and 'ies' if plural (>1). Accepts the option `0` or `zero` which 
#' treats a 0-length or a 0-value as plural.
#' - enum: enumerates the elements (see help for the regular operation `enum`)
#' - n, N, len, Len: add the number of elements ("len") or the value ("n") of the variable as a formatted number or 
#' in letters (upper case versions). Accepts the options `letter` (to write in letter) 
#' and `upper` (to uppercase the first letter).
#' - nth, ntimes: writes the value of the variable as an order (nth) or a frequence (ntimes). Accepts the option `letter`
#' to write the numbers in letters (uppercase version of the operator does the same).
#' - is, or any verb: conjugates the verb appropriately
#' 
#' You can chain operations, in that case a whitespace is automatically added between them.
#' 
#'  Ex.3: `x = c(7, 3, 18); smagick("The winning number{$s, is, enum ? sort(x)}.")`
#' leads to "The winning numbers are 3, 7 and 18.". With `x = 7` this leads to
#' "The winning number is 7.".
#' 
#' On top of the previous operations, there is a special operation allowing to add verbatim text depending on 
#' the situation. The syntax is as follows:
#' - `(s1;s2)`: adds verbatim 's1' if singular and 's2' if plural (>1)
#' - `(s1;s2;s3)`: adds verbatim 's1' if zero, 's2' if singular (=1) and 's3' if plural
#' - `(s1;;s3)`: adds verbatim 's1' if zero, 's3' if singular or plural (i.e. >=1)
#' 
#' These case-dependent verbatim values **are interpolated** (if appropriate). In these interpolations
#' you need not refer explicitly to the variable for pluralization interpolations.
#' 
#' Ex.4: `x = 3; smagick("{#(Sorry, nothing found.;;{#N.upper} match{#es, were} found.)?x}")` leads to 
#' "Three matches were found.". If "x = 1", this leads to "One match was found." and if "x = 0" this leads
#' to "Sorry, nothing found.".
#' 
#' @section Escaping and special cases:
#' 
#' The opening and closing brakets, `{}`, are special characters and cannot be used as regular text. 
#' To bypass their special meaning, you need to escape them with a double backslash.
#' 
#' Ex.1: `smagick("open = \\\\{, close = }")` leads to `"open = {, close = }"`.
#' Ex.2: `smagick("many {5 times.c ! \\\\}}")` leads to `many }}}}}`.
#' 
#' You only need to escape the special delimiters which the algorithm is currently looking for.
#' As you can see, you don't need to escape the closing bracket in Ex.1 since no box
#' was open. On the other hand, you need to escape it in Ex.2.
#' 
#' Alternatively, you can use the function `dsb()` to interpolate with `.[]` instead of `{}`.
#' You can also use the following hack:
#' 
#' Ex.3: smagick("I {'can {write} {{what}} I want'}") leads to `"I can {write} {{what}} I want"`.
#' 
#' Since `{expr}` evaluates `expr`, the stuff inside the *box*, you can pass a 
#' character string and it will stay untouched.
#' 
#' In the few operations expecting a semi-colon (if-else and pluralization), it can also be
#' escaped with a double backslash.
#' 
#' In interpolations, the exclamation mark (`!`) signals a verbatim expression. But what
#' if you use it to mean the logical operation *not* in an operation-free interpolation? 
#' In that case, you need a hack: use a question mark (`?`) first to indicate to the
#' algorithm that you want to evaluate the expression. 
#' 
#' Ex.4: `smagick("{!TRUE} is {?!TRUE}")` leads to "TRUE is FALSE". The first expression is
#' taken verbatim while the second is evaluated.
#' 
#' @inheritSection str_is Generic pattern flags
#'
#' @return
#' It returns a character vector whose length depends on the elements and operations in the interpolations.
#' 
#' @seealso 
#' If you want to apply a chain of operations on a single vector, see [str_ops()] which 
#' may be more appropriate.
#'
#' @examples
#'
#' #
#' # BASIC USAGE ####
#' #
#'
#' x = c("Romeo", "Juliet")
#'
#' # {x} inserts x
#' smagick("Hello {x}!")
#'
#' # elements in ... are collapsed with "" (default)
#' smagick("Hello {x[1]}, ",
#'     "how is {x[2]} doing?")
#'
#' # Splitting a comma separated string
#' # The mechanism is explained later
#' smagick("/J. Mills, David, Agnes, Dr Strong")
#'
#' # Nota: this is equivalent to (explained later)
#' smagick("{', *'S ! J. Mills, David, Agnes, Dr Strong}")
#'
#' #
#' # Applying low level operations to strings
#' #
#'
#' # Two main syntax:
#'
#' # A) expression evaluation
#' # {operation ? x}
#' #            | |
#' #            |  \-> the expression to be evaluated
#' #             \-> ? means that the expression will be evaluated
#'
#' # B) verbatim
#' # {operation ! x}
#' #            | |
#' #            |  \-> the expression taken as verbatim (here 'x')
#' #             \-> ! means that the expression is taken as verbatim
#'
#' # operation: usually 'arg'op with op an operation code.
#'
#' # Example: splitting
#' x = "hello dear"
#' smagick("{' 's ? x}")
#' # x is split by ' '
#'
#' smagick("{' 's ! hello dear}")
#' # 'hello dear' is split by ' '
#' # had we used ?, there would have been an error
#'
#'
#' # There are 50+ string operators
#' # Operators usually have a default value
#' # Operations can have options
#' # Operations can be chained by separating them with a comma
#'
#' # Example: default of 's' is ' ' + chaining with collapse
#' smagick("{s, ' my 'c ! hello dear}")
#'
#' #
#' # Nesting
#' #
#'
#' # {operations ! s1{expr}s2}
#' #             |   |
#' #             |    \-> expr will be interpolated then added to the string
#' #              \-> nesting requires verbatim evaluation: '!'
#'
#' smagick("The variables are: {C ! x{1:4}}.")
#'
#' # This one is ugly but it shows triple nesting
#' smagick("The variables are: {ws, C ! {2 times ! x{1:4}}{','s, 4 each !  ,_sq}}.")
#'
#' #
#' # Splitting
#' #
#'
#' # s: split with fixed pattern, default is ' '
#' smagick("{s ! a b c}")
#' smagick("{' b 's !a b c}")
#'
#' # S: same as 's' but default is ',[ \t\n]*'
#' smagick("{S !a, b, c}")
#' smagick("{'[[:punct:] ]+'S ! a! b; c}")
#' 
#' # add regex flags: e.g. fixed search
#' smagick("{'f/.'s ! hi.there}")
#' 
#'
#' #
#' # Collapsing
#' #
#'
#' # c and C do the same, their default is different
#' # syntax: 's1|s2' with
#' # - s1 the string used for collapsing
#' # - s2 (optional) the string used for the last collapse
#'
#' # c: default is ' '
#' smagick("{c ? 1:3}")
#'
#' # C: default is ', | and '
#' smagick("{C ? 1:3}")
#'
#' smagick("{', | or 'c ? 1:4}")
#'
#' #
#' # Extraction
#' #
#'
#' # extract: to extract patterns (option first)
#' # x: alias to extract.first
#' # X: alias to extract
#' # syntax: 'pattern'x
#' # Default is '[[:alnum:]]+'
#'
#' x = "This years is... 2020"
#' smagick("{x ? x}") # similar to smagick("{extract.first ? x}")
#' smagick("{X ? x}") # similar to smagick("{extract ? x}")
#'
#' smagick("{'\\d+'x ? x}")
#'
#' #
#' # STRING FORMATTING ####
#' #
#'
#' #
#' # upper, lower, title
#'
#' # upper case the first letter
#' smagick("{upper.first ! julia mills}")
#'
#' # title case
#' smagick("{title ! julia mills}")
#'
#' # upper all letters
#' smagick("{upper ! julia mills}")
#'
#' # lower case
#' smagick("{lower ! JULIA MILLS}")
#'
#' #
#' # q, Q, bq: single, double, back quote
#'
#' smagick("{S, q, C ! Julia, David, Wilkins}")
#' smagick("{S, Q, C ! Julia, David, Wilkins}")
#' smagick("{S, bq, C ! Julia, David, Wilkins}")
#'
#' #
#' # format, Format: formats the string to fit the same length
#'
#' # format: the right side is filled with blanks
#' # Format: the left side is filled with blanks, the string is right aligned
#'
#' score = c(-10, 2050)
#' nm = c("Wilkins", "David")
#' smagick("Monopoly scores:\n{'\n'c ! - {format ? nm}: {Format ? score} US$}")
#'
#' # OK that example may have been a bit too complex,
#' # let's make it simple:
#'
#' smagick("Scores: {format ? score}")
#' smagick("Names: {Format ? nm}")
#'
#' #
#' # ws: white space normalization
#'
#' # ws: suppresses trimming white spaces + normalizes successive white spaces
#' # Add the following options in any order to:
#' # - punct: remove punctuation
#' # - digit: remove digits
#' # - isolated: remove isolated characters
#'
#' smagick("{ws ! The   white  spaces are now clean.  }")
#'
#' smagick("{ws.punct ! I, really -- truly; love punctuation!!!}")
#'
#' smagick("{ws.digit ! 1, 2, 12, a microphone check!}")
#'
#' smagick("{ws.i ! 1, 2, 12, a microphone check!}")
#'
#' smagick("{ws.d.i ! 1, 2, 12, a microphone check!}")
#'
#' smagick("{ws.p.d.i ! 1, 2, 12, a microphone check!}")
#'
#' #
#' # %: applies sprintf formatting
#'
#'  # add the formatting as a regular argument
#' smagick("pi = {'.2f'% ? pi}")
#' # or right after the %
#' smagick("pi = {%.2f ? pi}")
#'
#' #
#' # paste: appends text on each element
#' # Accepts the options: right, both, front and back
#' # It accepts the special values :1:, :i:, :I:, :a:, :A: to create enumerations
#'
#' # adding '|' on both sides
#' smagick("{'|'paste.both, ' + 'c ! x{1:4}}")
#' 
#'
#' # Enumerations
#' acad = smagick("/you like admin, you enjoy working on weekends, you really love emails")
#' smagick("Main reasons to pursue an academic career:\n {':i:) 'paste, C ? acad}.")
#' 
#' # You can also use the enum command
#' smagick("Main reasons to pursue an academic career:\n {enum.i ? acad}.")
#'
#' #
#' # stopwords: removes basic English stopwords
#' # the list is from the Snowball project:
#' #  http://snowball.tartarus.org/algorithms/english/stop.txt
#'
#' smagick("{stop, ws ! It is a tale told by an idiot, full of sound and fury, signifying nothing.}")
#'
#' #
#' # k: keeps the first n characters
#' # syntax: nk: keeps the first n characters
#' #         'n|s'k: same + adds 's' at the end of shortened strings
#' #         'n||s'k: same but 's' counts in the n characters kept
#'
#' words = smagick("/short, constitutional")
#' smagick("{5k ? words}")
#'
#' smagick("{'5|..'k ? words}")
#'
#' smagick("{'5||..'k ? words}")
#'
#' #
#' # K: keeps the first n elements
#' # syntax: nK: keeps the first n elements
#' #         'n|s'K: same + adds the element 's' at the end
#' #         'n||s'K: same but 's' counts in the n elements kept
#' #
#' # Special values :rest: and :REST:, give the number of items dropped
#'
#' bx = smagick("/Pessac Leognan, Saint Emilion, Marguaux, Saint Julien, Pauillac")
#' smagick("Bordeaux wines I like: {3K, ', 'C ? bx}.")
#'
#' smagick("Bordeaux wines I like: {'3|etc..'K, ', 'C ? bx}.")
#'
#' smagick("Bordeaux wines I like: {'3||etc..'K, ', 'C ? bx}.")
#'
#' smagick("Bordeaux wines I like: {'3|and at least :REST: others'K, ', 'C ? bx}.")
#'
#' #
#' # Ko, KO: special operator which keeps the first n elements and adds "others"
#' # syntax: nKo
#' # KO gives the rest in letters
#'
#' smagick("Bordeaux wines I like: {4KO, C ? bx}.")
#'
#' #
#' # r, R: string replacement 
#' # syntax: 's'R: deletes the content in 's' (replaces with the empty string)
#' #         's1 => s2'R replaces s1 into s2
#'
#' smagick("{'e'r, ws ! The letter e is deleted}")
#'
#' # adding a perl look-behind
#' smagick("{'(?<! )e'r !The letter e is deleted}")
#'
#' smagick("{'e => a'r !The letter e becomes a}")
#'
#' smagick("{'([[:alpha:]]{3})[[:alpha:]]+ => \\1.'r ! Trimming the words}")
#'
#' # Alternative way with simple operations: split, shorten, collapse
#' smagick("{s, '3|.'k, c ! Trimming the words}")
#'
#' #
#' # times, each
#' # They accept the option c to collapse with the empty string
#'
#' smagick("N{10 times.c ! o}!")
#'
#' smagick("{3 times.c ? 1:3}")
#' smagick("{3 each.c ? 1:3}")
#'
#' #
#' # erase: replaces the items by the empty string
#' # -> useful in conditions
#'
#' smagick("{erase ! I am going to be annihilated}")
#'
#' #
#' # ELEMENT MANIPULATION ####
#' #
#'
#' #
#' # rm: removes the elements
#' # Its (optional) argument is a regular expression giving which element to remove
#' # Many options: "empty", "blank", "noalpha", "noalnum", "all" 
#'
#' x = c("Destroy", "All")
#' smagick("{'A'rm ? x}")
#' 
#' smagick("{rm.all ? x}")
#'
#' x = smagick("/1, 12, 123, 1234, 123456, 1234567")
#' # we delete elements whose number of characters is lower or equal to 3
#' # => see later section CONDITIONS
#' smagick("{if(.nchar > 3 ; nuke) ? x}")
#'
#' #
#' # PLURALIZATION ####
#' #
#'
#' # Two ways to enable pluralization:
#' # {$ command}: means the plural is decuced from the length of the variable
#' # {# command}: means the plural is decuced from the value of the variable
#'
#' # Explanatory example
#' x = c("Eschyle", "Sophocle", "Euripide")
#' n = 37
#' smagick("The author{$s, enum, have ? x} written {#N ? n} play{#s}.")
#'
#' x = "Laurent Berge"
#' n = 0
#' smagick("The author{$s, enum, have ? x} written {#N ? n} play{#s}.")
#'
#' # How does it work?
#' # First is {$s, enum, have ? x}.
#' # The commands `s`, `enum` and `have` are applied to `x` which must come after a `?`
#' #    => there the plural (whether an s is added and how to conjugate the verb have) depends
#' #       on the **length** of the vector `x`
#' #
#' # Second comes {#N ? n}.
#' # The double dollar sign means that the command `N` will be applied to the **value** n.
#' # The value must come after the `?`
#' #
#' # Third is {#s}.
#' # The object to which `s` should be applied is missing (there is no `? n`).
#' # The default is to apply the command to the previous object. In this case,
#' #  this is `n`.
#'
#' # Another similar example illustrating that we need not express the object several times:
#' x = c("Eschyle", "Sophocle", "Euripide")
#' smagick("The {Len ? x} classic author{$s, are, enum}.")
#'
#'
#'
#' #
#' # ARGUMENTS FROM THE ENVIRONMENT ####
#' #
#'
#' # Arguments can be evaluated from the calling environment.
#' # Simply use backticks instead of quotes.
#'
#' dollar = 6
#' reason = "glory"
#' smagick("Why do you develop packages? For {`dollar`times.c ! $}?",
#'     "For money? No... for {upper,''s, c ? reason}!", sep = "\n")
#' 
#' 
#' #
#' # COMPATIBILITY WITH data.table ####
#' # 
#' 
#' # smagick is compatible with data.table (tested on version 1.14.2)
#' 
#' if(requireNamespace("data.table")){
#'   library(data.table)
#'   dt_iris = as.data.table(iris)
#'   dt_small = dt_iris[, .(species_PL = sma("{first, 10 fill.c ? Species}: {%.1f ? mean(Petal.Length)}")), 
#'                      by = Species]
#'   print(dt_small)
#' }
#'
#'
#'    
#'
smagick = function(..., envir = parent.frame(), sep = "", vectorize = FALSE, check = TRUE,
               slash = TRUE, collapse = NULL, help = NULL, use_DT = TRUE){


  if(!missing(vectorize)) check_logical(vectorize, scalar = TRUE)
  if(!missing(slash)) check_logical(slash, scalar = TRUE)
  if(!missing(collapse)) check_character(collapse, null = TRUE, scalar = TRUE)
  if(!missing(sep)) check_character(sep, scalar = TRUE)
  if(!missing(envir)) check_envir(envir)

  if(...length() == 0){
    if(missnull(help)){
      return("")
    }
  } else if(identical(..1, "--help")){
    sc = sys.call()
    if(identical(sc[[2]], "--help")){
      help = "_COMPACT_" # means full help
    }
  }

  set_pblm_hook()

  # is_root is only used to enable DT evaluation

  res = smagick_internal(..., is_dsb = FALSE, envir = envir, sep = sep,
                            vectorize = vectorize, slash = slash, help = help,
                            collapse = collapse, is_root = use_DT,
                            check = TRUE, fun_name = "smagick")

  if(inherits(res, "help")){
      return(invisible(NULL))
  }

  class(res) = c("smagick", "character")
  res
}

#' @describeIn smagick Alias to `smagick`.
sma = smagick

#' @describeIn smagick Like `smagick` but without any error handling to save a few ms
.smagick = function(..., envir = parent.frame(), sep = "", vectorize = FALSE,
                check = FALSE, slash = FALSE, use_DT = FALSE){

  set_pblm_hook()

  smagick_internal(..., is_dsb = FALSE, envir = envir,
                      slash = slash, sep = sep,
                      vectorize = vectorize, is_root = use_DT,
                      check = check, fun_name = ".smagick")
}

#' @describeIn smagick Alias to `.smagick`.
.sma = .smagick

####
#### Internal ####
####

smagick_internal = function(..., is_dsb = TRUE, envir = parent.frame(),  data = list(),
                               sep = "", vectorize = FALSE,
                               slash = TRUE, collapse = NULL,
                               help = NULL, is_root = FALSE,
                               check = FALSE, fun_name = "dsb", plural_value = NULL){
  
  # flag useful to easily identify this environment (used in error messages)
  is_smagick_internal = TRUE

  if(!is.null(help)){
    if(identical(help, "_COMPACT_")){
      msg = getOption("smagick_help_compact")
      
      msg = paste(msg, collapse = "\n")
      stop_up("Help requested.", msg = msg)
    } else {
      
      on.exit(smagick_dynamic_help(help))
      
      stop_up("smagick: Help requested.")
    }    
  }

  if(is_root){
    # we check for data table calls
    is_dt = FALSE
    sc = sys.calls()
    n_sc = length(sc)
    if(n_sc >= 4){

      for(i in 2:(n_sc - 2)){
        ls_i = ls(envir = parent.frame(i), all.names = TRUE)
        is_dt = all(c(".SD", ".I", ".N") %in% ls_i)
        if(is_dt){
          break
        }
      }

      if(is_dt){
          is_by = ".BY" %in% ls_i && ".Random.seed" %in% ls(envir = parent.frame(i + 1), all.names = TRUE)

          if(is_by){
            # that's not great but it works somehow
            dt_call = sc[[n_sc - i - 1]]
            dt_name = dt_call[[2]]
            dt_data = unclass(eval(dt_name, parent.frame(i + 1)))
            for(v in c(".I", ".N", ".GRP", ".BY")){
              dt_data[[v]] = get(v, parent.frame(i))
            }
          } else {
            dt_data = unclass(get("x", parent.frame(i + 2)))
            for(v in c(".N", ".GRP")){
              dt_data[[v]] = get(v, parent.frame(i))
            }
          }

          attr(envir, "dt_data") = dt_data
      }
    }
  }

  if(...length() == 0){
    return("")

  } else if(...length() == 1){
    x = as.character(..1)

    if(length(x) > 1){
      stop_hook("`", fun_name, "` can only be applied to character scalars. ",
                "\nPROBLEM: the argument is of length ",
           length(x), "")
    }

  } else {

    if(check){
      dots = check_expr(list(...), "In `", fun_name, "`, one element of ... could not be evaluated.")
    } else {
      dots = list(...)
    }

    if(any(lengths(dots) > 1)){
      qui = which(lengths(dots) > 1)[1]
      stop_hook("`", fun_name, "` can only be applied to character scalars.",
                "\nPROBLEM: The ", n_th(qui),
                " elment in ... is of length ", length(dots[[qui]]), ".")
    }

    if(!vectorize){
      # Note: using paste(..1, ..2, sep = sep) explicitly only saves 2us vav do.call
      # not worth it.

      dots$sep = sep
      x = do.call(paste, dots)
    } else {
      # vectorize
      n = length(dots)
      res = vector("list", n)
      for(i in 1:n){
        res[[i]] = smagick_internal(dots[[i]], is_dsb = is_dsb, slash = slash, 
                                       envir = envir, check = check, fun_name = fun_name)
      }

      return(unlist(res))
    }
  }

  if(is.na(x) || length(x) == 0){
    return(x)
  }

  BOX_OPEN = if(is_dsb) ".[" else "{"

  if(slash && substr(x, 1, 1) == "/"){
    # we apply the "slash" operation
    x = sma_operators(substr(x, 2, nchar(x)), "/", NULL, "", check = check, envir = envir, 
                      group_flag = 0, is_dsb = is_dsb, fun_name = fun_name)
    return(x)
  } 
  
  if(!grepl(BOX_OPEN, x, fixed = TRUE)){
    return(x)

  } else {
    x_parsed = cpp_smagick_parser(x, is_dsb)
    if(length(x_parsed) == 1 && isTRUE(attr(x_parsed, "error"))){
      report_smagick_parsing_error(x, x_parsed, is_dsb)
    }
  }

  # NOTE on the parsing:
  # - parsing returns a list.
  #   If, eg: x = "str1.[stuff1]str2.[stuff2]"
  #   we will obtain a list of 4 elements
  #   note that the empty string between two boxes is an element
  # - an element can be either of length 1 or of length 2
  #   * l=1: means this is a regular string
  #   * l=2: this is a list of two elements:
  #     + the first element is a vector of operators, with ? or ! always at the end (when available)
  #     + the second element is the string to be interpolated (when available, some operators don't
  #       require interpolation, like pluralization)
  #   ex: "hello: .[u, C ! .[/luc, domi]]"
  #     [[1]]: "hello: "
  #     [[2]]:
  #           [[1]]: vector c("u", "C")
  #           [[2]]: scalar ".[/luc, domi]"
  # - note that nestedness is dealt with in here and NOT in the parser.
  #

  ANY_PLURAL = isTRUE(attr(x_parsed, "plural"))
  if(ANY_PLURAL){
    # we need to book keep the evaluations to 'know' to which the pluralization applies
    x_values_all = vector("list", length(x_parsed))
  }

  n_x_all = lengths(x_parsed)

  n = length(x_parsed)
  res = character(0)
  i_done = FALSE
  for(i in 1:n){

    if(i_done){
      i_done = FALSE
      next
    }

    xi = x_parsed[[i]]

    if(length(xi) == 1){
      # This is a regular string (no operations involved)

      if(i == 1){
        res = xi
      } else if(nchar(xi) > 0){
        res = paste0(res, xi)
      }

    } else {
      #
      # operations
      #

      operators = xi[[1]]
      xi = xi[[2]]

      if(length(operators) == 0){

        # we need to evaluate xi
        xi_call = check_set_smagick_parsing(xi, check)

        if(is.character(xi_call)){
          xi = xi_call
        } else {
          xi = check_set_smagick_eval(xi_call, data, envir, check)
        }

        if(ANY_PLURAL){
          # we save
          x_values_all[[i]] = xi
        }

      } else {

        n_op = length(operators)
        verbatim = operators[n_op] %in% c("!", "/")
        
        if(operators[1] %in% c("&", "&&")){
          data[["len"]] = function(x) length(x)
        }

        is_ifelse = FALSE
        concat_nested = FALSE
        is_xi_done = FALSE
        is_plural = ANY_PLURAL && operators[1] %in% c("$", "#")
        i_candidate = i # => used when evaluating later xi by anticipation
        if(ANY_PLURAL){

          if(!is_plural){
            # if the xi has already been evaluated by anticipation, we catch it
            if(lengths(x_values_all)[i] > 0){
              xi = x_values_all[[i]]
              is_xi_done = TRUE
            }
          }

          if(is_plural){
            #
            # Finding out candidates for pluralization
            #

            if(identical(xi, "")){

              if(!is.null(plural_value)){
                # we are in a nested pluralization call:
                # ex: "{$(;; Maybe you meant: {enum.bq.4}?) ? sugg)}"
                # here 'sugg' would be in `plural_value`
                # we would be in the evaluation of 
                # " Maybe you meant: {enum.bq.4}?"
                # if we did not pass it along, there would have been an error

                xi = plural_value
                is_xi_done = TRUE

              } else {
                if(length(operators) == 1){
                  example = 'Example: x = c("Mark", "Sarah"); dsb(".[$enum, is ? x] away.")'
                  example = bespoke_msg(example, fun_name)
                  .stop_hook("In `", fun_name, "`, the pluralization operator (`", operators[1], 
                             "`) must contain operations. ", '\n', example)
                }

                # We need to find it out!!!!!!
                xl = lengths(x_values_all)
                if(all(xl == 0)){
                  # We need to evaluate the next ones
                  ok = FALSE
                  if(i < n){
                    for(j in (i + 1):n){
                      if(length(x_parsed[[j]]) == 2 && !identical(x_parsed[[j]][[2]], "")){
                        ok = TRUE
                        break
                      }
                    }
                  }

                  if(!ok){
                    pblm = paste0(operators[1], paste0(operators[-1], collapse = ", "))
                    example = 'Example: x = c("Mark", "Sarah"); dsb(".[$enum, is ? x] away.")'
                    example = bespoke_msg(example, fun_name)

                    .stop_hook("In `", fun_name, "`, the pluralization (`", pblm, "`) did not find any value to pluralize on. ",
                            "Please provide one by adding it after a question mark.\n", example)
                  }

                  i_candidate = j
                  xi = x_parsed[[j]][[2]]

                } else {
                  # Means that stg has been evaluated before
                  qui = tail(which(xl > 0), 1)
                  xi = x_values_all[[qui]]
                  is_xi_done = TRUE
                }
              }

            } else {
              # xi != ""

              # last item is ? or !, we trim it
              operators = operators[-n_op]
            }
          }
        }

        if(!is_plural) {
          # regular operators

          if(operators[1] %in% c("&", "&&")){
            # if else operator
            is_ifelse = TRUE
            xi_raw = xi = operators[2]
            verbatim = FALSE
          } else if(operators[n_op] == "/"){
            operators = "/"
          } else {
            operators = operators[-n_op]
            # The two separators ? and ! have no default operation
          }

          # If split operator, we need to concatenate
          concat_nested = length(operators) > 0 && grepl("(?:^|[^[:alpha:]])(?:s|S)$", operators[[1]])
        }


        #
        # evaluation of xi
        #


        if(!is_xi_done){
          if(verbatim){
            # for the slash operation, we delay the interpolation
            if(operators[1] != "/" && grepl(BOX_OPEN, xi, fixed = TRUE)){
              xi = smagick_internal(xi, is_dsb = is_dsb, envir = envir, slash = FALSE,
                                      vectorize = concat_nested, check = check, fun_name = fun_name)
            }
          } else if(!verbatim){
            # evaluation
            xi_call = check_set_smagick_parsing(xi, check)
            xi = check_set_smagick_eval(xi_call, data, envir, check)
          }
        }

        if(ANY_PLURAL){
          # we save the results
          x_values_all[[i]] = xi
          if(i != i_candidate){
            x_values_all[[i_candidate]] = xi
          }
        }


        #
        # Applying the operators
        #

        if(is_plural){
          xi = sma_pluralize(operators, xi, fun_name, is_dsb, envir, check)

        } else if(is_ifelse){

          xi_val = NULL
          vars = all.vars(str2lang(xi_raw))

          if(length(vars) == 0){
            if(operators[1] == "&&"){
              # ERROR
              form = "{&& cond ; true}"
              example = '\nEXAMPLE: x = c(5, 700); smagick("Value: {&&x > 20 ; > 20}")'

              .stop_hook("The if-else operator `&&`, of the form ", form,
                      ", requires at least one variable to be evaluated in the condition.",
                      "\nPROBLEM: no variable could be found.", example)
            }
            
          } else {
            do_eval = TRUE
            if(operators[1] == "&"){
              do_eval = grepl(BOX_OPEN, operators[3], fixed = TRUE)
              if(!do_eval && length(operators) == 4){
                do_eval = grepl(BOX_OPEN, operators[4], fixed = TRUE)
              }
            }
            
            if(do_eval){
              xi_call = str2lang(vars[1])
              xi_val = check_set_smagick_eval(xi_call, data, envir, check)
            }
            
            if(operators[1] == "&&" && length(xi) != length(xi_val)){
              form = "{&& cond ; true}"
              example = 'EXAMPLE: x = c(5, 700); smagick("Value: {&&x > 20 ; > 20}")'

              stop_hook("The if-else operator `&&`, of the form {form} ",
                      "requires that the first variable used in the condition ",
                      "(here {bq?vars[1]}) is of the same length as the condition.",
                      "\nPROBLEM: the condition is of length {len.f?xi} while the ",
                      "variable is of length {len.f?xi_val}.",
                      "\n{example}")
            }
          }
          
          if(ANY_PLURAL){
            # we save the variable over which the ifelse is done
            x_values_all[[i]] = xi_val
            if(i != i_candidate){
              x_values_all[[i_candidate]] = xi_val
            }
          }

          xi = sma_ifelse(operators, xi, xi_val, fun_name, envir = envir,
                          check = check, is_dsb = is_dsb)
        } else {
          #
          # REGULAR OPERATORS
          #

          group_flag = any(str_x(operators, 1) == "~")
          
          for(j in seq_along(operators)){
            opi = operators[[j]]
            op_parsed = sma_char2operator(opi, fun_name)

            if(op_parsed$eval){
              argument_call = check_set_oparg_parse(op_parsed$argument, opi, check)
              argument = check_set_oparg_eval(argument_call, data, envir, opi, check)
            } else {
              argument = op_parsed$argument
            }

            if(check){
              xi = check_expr(sma_operators(xi, op_parsed$operator, op_parsed$options, argument,
                                              check = check, envir = envir, group_flag = group_flag,
                                              is_dsb = is_dsb, fun_name = fun_name),
                                get_smagick_context(), " See error below:",
                                verbatim = TRUE, up = 1)
            } else {
              xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, check = check,
                                 envir = envir, group_flag = group_flag,
                                 is_dsb = is_dsb, fun_name = fun_name)
            }
          }
        }

        extra = attr(xi, "extra")
        if(length(extra) > 0){

          if(length(extra$element_add_first) > 0){
            if(length(extra$element_add_last) > 0){
              xi = c(extra$element_add_first, xi, extra$element_add_last)
            } else {
              xi = c(extra$element_add_first, xi)
            }
          } else if(length(extra$element_add_last) > 0){
            xi = c(xi, extra$element_add_last)
          }

          if(length(extra$str_add_first) > 0){
            if(length(xi) > 0){
              xi[1] = paste0(extra$str_add_first, xi[1])
            } else {
              xi = extra$str_add_first
            }
          }

          if(length(extra$str_add_last) > 0){
            if(length(xi) > 0){
              xi[length(xi)] = paste0(xi[length(xi)], extra$str_add_last)
            } else {
              xi = extra$str_add_last
            }
          }

          attr(xi, "extra") = NULL
        }
      }
      
      if(i == 1){
        res = xi
      } else{
        if(i < n && n_x_all[i + 1] == 1){
          if(vectorize){
            res = c(res, xi, x_parsed[[i + 1]])
          } else {
            res = paste0(res, xi, x_parsed[[i + 1]])
          }

          i_done = TRUE
        } else {
          if(vectorize){
            res = c(res, xi)
          } else {
            res = paste0(res, xi)
          }
        }
      }
    }
  }

  if(!is.null(collapse) && length(res) > 1){
    res = paste0(res, collapse = collapse)
  }

  return(res)
}


sma_char2operator = function(x, fun_name){

  op_parsed = cpp_parse_operator(x)

  OPERATORS = getOption("smagick_operations")
  
  ok = FALSE
  op = op_parsed$operator

  if(nchar(op) == 0){
    .stop_hook("In ", fun_name, ", if a quoted value is present, the operators must ",
              "be of the form 'value'op, ",
              "with 'op' an operator. ",
              "\nPROBLEM: In `", x, "` the operator is missing.")
  }
  
  argument = op_parsed$argument
  do_eval = op_parsed$eval
  
  # we partially match operators if needed
  if(!op %in% OPERATORS){
    op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
    op_parsed$operator = op
  }

  if(nchar(argument) == 0 && !substr(x, 1, 1) %in% c("'", "\"", "`")){
    # no argument provided: we need to:
    # - set default values if available
    # - send error if operator requires argument
    
    # default values
    argument = switch(op,
                      s = " ", S = ",[ \t\n]*",
                      x = "[[:alnum:]]+", X = "[[:alnum:]]+", extract = "[[:alnum:]]+",
                      c = " ", C = ", | and ",
                      times = 1, each = 1,
                      first = 1, last = 1,
                      cfirst = 1, clast = 1,
                      trim = 1, 
                      "")
    
    op_parsed$argument = argument

    if(op %in% c("R", "r", "%", "k", "K", "paste", "get", "is")){
      ex = c("R" = 'x = "She loves me."; dsb(".[\'s\\b => d\'R ? x]")',
            "r" = 'x = "Amour"; dsb(".[\'ou => e\'r ? x]...")',
            "%" = 'dsb("pi is: .[%.03f ? pi]")',
            "k" = 'dsb("The first 8 letters of the longuest word are: .[8k, q ! the longuest word].")',
            "K" = 'x = 5:9; dsb("The first 2 elements of `x` are: .[2K, C ? x].")',
            "get" = 'x = row.names(mtcars) ; dsb("My fav. cars are .[\'toy\'get.ignore, \'the \'app, enum ? x].")',
            "is" = 'x = c("Bob", "Pam") ; dsb(".[\'am\'is ? x]")',
            "paste" = 'x = "those, words"; dsb("Let\'s emphasize .[S, \'**\'paste.both, c ? x].")')

      ex = bespoke_msg(ex[op])
      .stop_hook("The operator `", op,
              "` has no default value, you must provide values explicitly.\n Example: ", 
              ex)
    }
  }

  if(op %in% c("Ko", "KO")){
    # special case
    text = if(op == "Ko") "||:rest: others" else "||:REST: others"
    argument = paste0(argument, text)
    op_parsed$operator = op = "K"
    op_parsed$argument = argument
  }
    
  if(!ok && !op %in% OPERATORS){
    
    op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
    op_parsed$operator = op
    
    if(!op %in% OPERATORS){
      
      context = get_smagick_context()

      sugg_txt = suggest_item(op, OPERATORS, newline = FALSE, info = "operator")
      
      op = gsub("\n", "\\n", op)
      op = gsub("\t", "\\t", op)

      msg = .dsb(".[context]",
              "\nPROBLEM: .[bq?op] is not a valid operator. ", sugg_txt,
              "\n\nINFO: Type smagick('--help') for more help or smagick(help = regex) or smagick(help = TRUE).",
              "\nEx. of valid stuff: smagick(\"{10 first, `6/2`last, ''c, 'i => e'r, upper.first ? letters}!\") ")

      .stop_hook(msg)
    }

  }

  op_parsed
}


sma_operators = function(x, op, options, argument, check = FALSE, envir = NULL, 
                         group_flag = 0, is_dsb = FALSE, fun_name = "smagick"){

  # group_flag:  0 nothing
  #                    1 keep track of conditional things
  #                    2 apply conditional

  extra = attr(x, "extra")
  group_index = attr(x, "group_index")

  if(op == "/"){
    # slash ####
    
    # What happens:
    # - string is split wrt commas
    # - any interpolation is vectorized
    
    res = cpp_parse_slash(x, is_dsb)
    
    box = if(is_dsb) ".[" else "{"
    
    is_open = grepl(box, res, fixed = TRUE)
    n_open = sum(is_open)
    if(n_open > 0){
      n_res = length(res)
      all_elements = vector("list", n_res)
      
      for(i in 1:n_res){
        if(is_open[i]){
          all_elements[[i]] = smagick_internal(res[i], is_dsb = is_dsb, envir = envir,
                                                  check = check, fun_name = fun_name)  
        } else {
          all_elements[[i]] = res[i]
        }        
      }
      
      res = do.call(base::c, all_elements)      
    }    
    
    
  } else if(op %in% c("c", "C")){
    # C, collapse ####
    # collapse

    # argument of the form: 'main_sep|last_sep'
    sep_last = ""
    is_last = FALSE
    if(length(x) > 1 && grepl("|", argument, fixed = TRUE)){
      is_last = TRUE
      arg_split = strsplit(argument, "(?<!\\\\)\\|", perl = TRUE)[[1]]
      if(grepl("\\", argument, fixed = TRUE)){
        arg_split = gsub("\\|", "|", arg_split, fixed = TRUE)
      }

      argument = arg_split[[1]]
      if(length(arg_split) == 1){
        # case with escape \\|
        is_last = grepl("(?<!\\\\)\\|$", argument, perl = TRUE)
        if(is_last){
          sep_last = ""
        }
      } else if(length(arg_split) == 2){
        # regular case
        sep_last = arg_split[[2]]
      } else {
        arg_split = as.list(arg_split)
        args_paste = arg_split[-1]
        args_paste[["sep"]] = "|"
        sep_last = do.call(paste, args_paste)
      }
    }
    sep = argument

    if(!is.null(group_index) && group_flag == 2){
      if(!is.character(x)){
        x = as.character(x)
      }
      res = cpp_paste_conditional(x, group_index, sep, sep_last)
      group_index = seq_along(res)
      
    } else {
      n_x = length(x)
      if(is_last){
        # This is the "last" operator
        if(n_x == 2){
          res = paste(x, collapse = sep_last)
        } else {
          res = paste(x[-n_x], collapse = sep)
          res = paste0(res, sep_last, x[n_x])
        }
      } else {
        res = paste(x, collapse = sep)
      }

      if(group_flag == 1){
        group_index = NULL
      }

    }
    
  } else if(op %in% c("s", "S", "r", "R", "clean", "get", "is", "which", "x", "X", "extract")){
    # split, replace, clean, extract, get, is, which ####
    
    valid_options = c("word", "ignore", "fixed")
    if(op == "extract"){
      valid_options = c(valid_options, "first")
    } else if(op %in% c("r", "R", "clean")){
      valid_options = c(valid_options, "total")
    } else if(op %in% c("get", "is", "which")){
      valid_options = c(valid_options, "equal", "in")
    }

    options = check_set_options(options, valid_options)
    is_word = "word" %in% options
    is_ignore = "ignore" %in% options
    is_fixed = "fixed" %in% options

    if(op == "extract"){
      if("first" %in% options){
        op = "x"
      } else {
        op = "X"
      }
    }

    if(op %in% c("s", "S", "X", "x", "extract")){
      # otherwise => dealt with in str_is or str_clean
      # we don't repeat the processing (otherwise => bugs)
      
      pat_parsed = format_simple_regex_flags(argument, fixed = is_fixed, word = is_word, 
                                             ignore = is_ignore)
      argument = pat_parsed$pattern
      is_fixed = pat_parsed$fixed
    } 

    #
    # now the operations
    #
    
    if(!is.character(x) && op != "which"){
      x = as.character(x)
    }
    
    if(op %in% c("s", "S")){
      # strsplit applied to "" returns character(0)
      # if occurring: need to fix
      
      x_split = strsplit(x, argument, fixed = is_fixed, perl = !is_fixed)
      
      fix_empty = any(x == "")

      if(group_flag != 0){
        # we keep track of the group index
        x_len_all = lengths(x_split)
        
        if(fix_empty){
          x_len_all[x_len_all == 0] = 1
        }
        
        group_index = rep(seq_along(x_len_all), x_len_all)
      }
      
      # Note that unlist removes empty strings
      if(fix_empty){
        x_split[x == ""] = ""
      }

      res = unlist(x_split)
      
    } else if(op %in% c("r", "R", "clean")){
      is_total = "total" %in% options

      pipe = "=>"
      if(grepl(" => ", argument, fixed = TRUE)){
        pipe = " => "
      }
      
      sep = if(op == "clean") ",[ \t\n]+" else ""

      res = str_clean(x, argument, pipe = pipe, sep = sep, 
                      ignore.case = is_ignore, fixed = is_fixed, word = is_word, 
                      total = is_total)

    } else if(op == 'x'){
      x_pat = regexpr(argument, x, fixed = is_fixed, perl = !is_fixed)

      res = substr(x, x_pat, x_pat - 1 + attr(x_pat, "match.length"))
    } else if(op == "X"){
      # extract all patterns

      x_list = regmatches(x, gregexpr(argument, x, fixed = is_fixed, perl = !is_fixed))

      if(group_flag != 0){
        # we keep track of the group index
        x_len_all = lengths(x_list)
        # I could avoid the last line... later
        group_index = rep(seq_along(x_len_all), x_len_all)
        group_index = cpp_recreate_index(group_index)
      }

      res = unlist(x_list)
    } else if(length(argument) == 1 && argument == "" && op == "which"){
      
      if(!is.logical(x)){
        stop_hook("The operation `which` must apply only to logical values.",
                 "\nPROBLEM: the current value to which it is applied is not logical ",
                 "(instead is is of class {enum.bq?class(x)}).")
      }

      res = which(x)

      if(group_flag == 1){
        group_index = group_index[res]
        group_index = cpp_recreate_index(group_index)
      } else if(group_flag == 2){
        stop_hook("The operation `which` cannot be applied in ",
                  "conditional statements (inside `~()`).",
                  "\nFIX: simply put this operation after the conditional statement.")
      }
      
    } else {
      # default is str_is
      if("equal" %in% options || "in" %in% options){
        if("equal" %in% options){
          res = x == argument
        } else {
          arg_split = strsplit(argument, ",[ \t\n]+")
          res = x %in% unlist(arg_split)
        }
        
      } else {
        res = str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)
      }

      if(op %in% c("get", "which")){

        if(group_flag == 1){
          group_index = group_index[res]
          group_index = cpp_recreate_index(group_index)
        } else if(group_flag == 2){
          stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                    "conditional statements (inside `~()`).",
                    "\nFIX: simply put this operation after or before the conditional statement.")
        }

        if(op == "get"){
          res = x[res]
        } else if(op == "which"){
          res = which(res)
        }

      }
    } 
    

  } else if(op %in% c("each", "times")){
    # times/each ####

    if(!is_numeric_in_char(argument)){
      msg = paste0("In `dsb`: the operator `", op, "` must have numeric arguments, `",
                   argument, "` is not numeric.")
      .stop_hook(bespoke_msg(msg))
    }
    
    if(length(argument) != 1){
      msg = paste0("In `dsb`: the operator `", op, "` must have an argument of length 1.",
                   "\nPROBLEM: the argument is of length ", length(argument), ".")
                   
      msg = bespoke_msg(msg)
      .stop_hook(msg)
    }
    
    if(op == "each"){
      res = rep(x, each = as.numeric(argument))
    } else {
      res = rep(x, as.numeric(argument))
    }

    if("c" %in% options){
      res = paste(res, collapse = "")
    }

    group_index = NULL

  } else if(op == "upper"){
    # upper, lower, title, Q, ####
    
    options = check_set_options(options, c("first", "sentence"))
        
    if("first" %in% options){
      # First letter only, if relevant
      res = as.character(x)
      substr(res, 1, 1) = toupper(substr(x, 1, 1))  
    } else if("sentence" %in% options){
      # we add upper case like in sentences
      # does not work for spanish for instance (with inverted !? coming first)
      res = gsub("(^|[.?!])(\\s*)(\\p{Ll})", "\\1\\2\\U\\3", x, perl = TRUE)
    } else {
      res = toupper(x)  
    }
    
  } else if(op == "lower"){
    res = tolower(x)

  } else if(op == "title"){
    # title case

    options = check_set_options(options, c("force", "ignore"))

    if("force" %in% options){
      # puts in lowercase first
      x = tolower(x)
    }

    is_ignore = "ignore" %in% options
    if(is_ignore){
      # we add __IGNORE in front of the items so that upper casing won't proc
      IGNORE = c("a", "the", "in", "on", "at", "of", 
                 "for", "to")
      pattern = paste0("(?<=[^:!?.]) (", paste0(IGNORE, collapse = "|"), ")(?=(\\s|$))")
      x = gsub(pattern, " __IGNORE\\1", x, perl = TRUE)
    }
    
    # should I add unicode awareness with (*UCP)? Option?
    # I would say yes by default bc you only apply this function
    # to small strings

    res = gsub("(*UCP)(^|[^\\p{L}])(\\p{Ll})", "\\1\\U\\2", x, perl = TRUE)
    
    if(is_ignore){
      res = gsub("__IGNORE", "", res, fixed = TRUE)
    }

  } else if(op == "q"){
    res = paste0("'", x, "'")

  } else if(op == "Q"){
    res = paste0("\"", x, "\"")

  } else if(op == "bq"){
    res = paste0("`", x, "`")

  } else if(op %in% c("format", "Format")){
    # Format, % ####

    options = check_set_options(options, c("0", "zero", "right", "center"))

    is_zero = any(options %in% c("0", "zero"))
    is_right = "right" %in% options || op == "Format" || is_zero
    is_center = any(options == "center")
    is_left = !is_right && !is_center

    if(is_right){
      res = format(x, justify = "right", big.mark = ",")
    } else {
      res = format(x, big.mark = ",")
    }   

    if(is.numeric(x)){
      if(is_zero){
        res = gsub(" ", "0", res, fixed = TRUE)
      } else if(!is_right){
        # we have to do that.... not sure the use cases need to be optimized
        pos = if(is_center) "centre" else "left"
        res = format(cpp_normalize_ws(res), justify = pos)
      }
    } else if(is_zero && is_numeric_in_char(x)){
      res = gsub(" ", "0", res, fixed = TRUE)
    }

  } else if(op == "%"){
    res = sprintf(paste0("%", argument), x)

  } else if(op == "ws"){
    # White spaces, tws, trim ####

    # w: only whitespaces
    # p: punct
    # d: digits
    # i: isolated
    
    options = check_set_options(options, c("punct", "digit", "isolated"))

    clean_punct = "punct" %in% options
    clean_digit = "digit" %in% options
    clean_isolated = "isolated" %in% options

    res = cpp_normalize_string(x, clean_punct, clean_digit, clean_isolated)
    
  } else if(op == "tws"){
    # trimming WS

    # note that the changes are 'in place' => it does not matter here
    # but I should not use cpp_trimws in other functions
    res = cpp_trimws_in_place(x)

  } else if(op == "trim"){
    # we trim

    nb = argument
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      .stop_hook(bespoke_msg(msg))
    }
    nb = as.numeric(nb)

    is_both = opt_equal(options, "both")
    is_right = any(c("r", "right") %in% options) || nb < 0
    
    if(!is.character(x)){
      x = as.character(x)
    }

    nx = nchar(x)
    if(is_both){
      # on both sides
      nb = abs(nb)

      res = substr(x, 1, nx - nb)
      res = substr(res, 1 + nb, nchar(res))

    } else if(is_right){
      # we revert: start from the end
      nb = abs(nb)
      res = substr(x, 1, nx - nb)
    } else {
      res = substr(x, 1 + nb, nx)
    }

  } else if(op %in% c("k", "K")){
    # keep: either the nber of characters (k) or the number of elements (K)

    #
    # Keep, enum ####
    #
    
    info = extract_pipe(argument, op, double = TRUE, numeric = TRUE, mbt = TRUE)
    nb = info$value
    add = info$extra
    is_included = info$is_double

    if(op == "k"){
      if(!is.character(x)){
        x = as.character(x)
      }
      qui = nchar(x) > nb
      res = substr(x, 1, nb)

      if(is_included){
        res[qui] = substr(res[qui], 1, nb - nchar(add))
      }

      if(nchar(add) > 0){
        res[qui] = paste0(res[qui], add)
      }

    } else if(op == "K"){

      if(nb == 0){
        res = character(0)
      } else {
        res = if(nb < length(x)) x[1:nb] else x
        
        if(group_flag == 2){
          stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                    "conditional statements (inside `~()`).",
                    "\nFIX: simply put this operation after or before the conditional statement.")
        }

        if(any(grepl(":(?:n|N|rest|REST):", add))){
          n = length(x)
          N = ""
          if(grepl(":N:", add, fixed = TRUE)){
            N = n_letter(n)
            add = gsub(":N:", N, add, fixed = TRUE)
          }

          add = gsub(":n:", n, add, fixed = TRUE)
          n_rest = n - nb + is_included
          if(n_rest > 0){
            add = gsub(":rest:", n_rest, add, fixed = TRUE)
            add = gsub(":REST:", n_letter(n_rest), add, fixed = TRUE)
          }
        }

        if(length(x) > nb){
          if(is_included){
            res = res[-nb]
          }

          if(nchar(add) > 0){
            res = c(res, add)
          }
        }
      }
    }
  } else if(op == "enum"){
    # NOTE:
    # for conditional operations, the code is slow.
    # but I don't see a use case for large vectors....
    # we use enum for smallish things so it should be OK.

    if(!is.null(group_index) && group_flag == 2){

      res = unname(tapply(x, group_index, enum_main, options = options))
      group_index = seq_along(group_index)

    } else {
      res = enum_main(x, options = options)

      if(group_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("first", "last", "cfirst", "clast")){
    # first, last, cfirst, clast ####

    nb = argument
    
    nb_extra = NULL
    if(op == "first" && grepl("|", argument, fixed = TRUE)){
      arg_split = strsplit(argument, "|", fixed = TRUE)[[1]]
      nb = arg_split[1]
      nb_extra = arg_split[2]
      
      if(!is.null(nb_extra) && !is_numeric_in_char(nb_extra)){
        msg = paste0("In `dsb`: in the operator `", op, "` the argument can be of the form ",
                     "'n1|n2' with `n1` and `n2` numbers. \nPROBLEM: the second element `",
                    nb_extra, "` is not numeric.")
        .stop_hook(bespoke_msg(msg))
      }
      
      nb_extra = as.numeric(nb_extra)
    }
    
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`: the operator `", op, "` must have a numeric argument. \nPROBLEM: `",
                   argument, "` is not numeric.")
      .stop_hook(bespoke_msg(msg))
    }    

    # the numeric argument can also be passed in options
    qui_num = which(grepl("^\\d+$", options))
    if(length(qui_num)){
      nb = options[qui_num[1]]
    }

    nb = as.numeric(nb)

    if(str_x(op, 1) == "c"){
      # we select the first/last characters
      
      if(!is.character(x)){
        x = as.character(x)
      }
      
      if(nb < 0){
        nb = abs(nb)
        nx = nchar(x)
        if(op == "cfirst"){
          res = substr(x, nb + 1, nx)
        } else {
          res = substr(x, 1, nx - nb)
        }
      } else {
        if(op == "cfirst"){
          res = substr(x, 1, nb)
        } else {
          nx = nchar(x)
          res = substr(x, nx - nb + 1, nx)
        }
      }
      
    } else {
      # we select the first/last elements

      if(!is.null(group_index) && group_flag == 2){
        is_last = op == "last"
        qui = cpp_find_first_index(group_index, nb, is_last)
        
        res = x[qui]
        group_index = group_index[qui]
        
        if(!is.null(nb_extra)){
          qui = cpp_find_first_index(group_index, nb, TRUE)
          res = c(res, x[qui])
          group_index = c(group_index, group_index[qui])
          
          my_order = order(group_index)
          res = res[my_order]
          group_index = group_index[my_order]
        }
        
      } else {
        if(op == "first"){
          
          if(nb < 0){
            
            if(!is.null(nb_extra)){
              msg = paste0("In `dsb`: in the operator `", op, "` the argument can be of the form ",
                          "'n1|n2' with `n1` and `n2` positive numbers. \nPROBLEM: the first element `",
                          nb, "` is negative.")
              .stop_hook(bespoke_msg(msg))
            }
            
            nb = abs(nb)
            if(nb > length(x)){
              res = character(0)
            } else {
              res = tail(x, length(x) - nb)
            }            
          } else {
            res = head(x, nb)
            
            if(!is.null(nb_extra)){
              if(nb_extra < 0){
                msg = paste0("In `dsb`: in the operator `", op, "` the argument can be of the form ",
                            "'n1|n2' with `n1` and `n2` positive numbers. \nPROBLEM: the second element `",
                            nb_extra, "` is negative.")
                .stop_hook(bespoke_msg(msg))
              }
              
              res = c(res, tail(x, nb_extra))
            }
            
          }    
        } else {
          if(nb < 0){
            nb = abs(nb)
            if(nb > length(x)){
              res = character(0)
            } else {
              res = head(x, length(x) - nb)
            }            
          } else {
            res = tail(x, nb)
          }
        }

        if(group_flag == 1){
          # this operation destroys the groups
          group_index = NULL
        }
      }
    }

  } else if(op == "rev"){
    # rev, sort, dsort ####
    
    if(!is.null(group_index) && group_flag == 2){
      qui = cpp_group_rev_index(group_index)
      res = x[qui]
    } else {
      res = rev(x)
    }

  } else if(op %in% c("sort", "dsort")){
    
    is_decreasing = op == "dsort"
    x2sort = x
    is_modified = FALSE
    if(nchar(argument) > 0){
      is_modified = TRUE
      x2sort = str_clean(x, argument)
    }
    
    options = check_set_options(options, "num")
    if("num" %in% options){
      is_modified = TRUE
      x2sort = suppressWarnings(as.numeric(x2sort))
    }
    
    if(group_flag == 1){
      # it makes no sense to keep track of the indexes when sorting the full string
      # ex: a1 b2 => split => a 1 b 2 => sort => a b 1 2
      # after that, the conditional operations (on a1 and b2) don't make much sense
      group_index = NULL
      group_flag = 0
    }

    if(!is.null(group_index) && group_flag == 2){
      
      if(is_decreasing){
        new_order = order(-group_index, x2sort, decreasing = TRUE)
      } else {
        new_order = order(group_index, x2sort)
      }

      res = x[new_order]
      group_index = cpp_recreate_index(group_index[new_order])
    } else {
      if(is_modified){
        new_order = order(x2sort, decreasing = is_decreasing)
        res = x[new_order]
      } else {
        res = sort(x, decreasing = is_decreasing)
      }
      
    }

  } else if(op %in% c("paste", "insert")){
    # paste, insert ####
    # paste: at the beginning/end of all strings
    # insert: element at the beginning/end of the vector 
    
    valid_options = c("both", "right")
    if(op == "paste"){
      valid_options = c(valid_options, "front", "back", "delete")
    }
    
    options = check_set_options(options, valid_options)

    if("delete" %in% options){
      x = character(length(x))
    }

    left = right = ""
    if("both" %in% options){
      left = right = argument
    } else if("right" %in% options){
      right = argument
    } else {
      left = argument
    }

    if(nchar(argument) == 0){
      res = x

    } else if(op == "paste"){

      if(length(x) == 0){
        res = x

      } else if(any(options %in% c("front", "back"))) {
        res = x
        if("front" %in% options){
          res[[1]] = paste0(left, x[1], right)
        }
        
        if("back" %in% options){
          n = length(x)
          res[[n]] = paste0(left, x[n], right)
        }        
        
      } else {

        # We replace the special values
        # :1:, :i:, :a:
        # we allow only one special value
        n_x = length(x)

        pat = c(":1:", ":01:", ":i:", ":a:", ":I:", ":A:")

        for(i in 1:2){
          tmp = if(i == 1) left else right
          any_done = FALSE

          for(p in pat){
            if(grepl(p, tmp, fixed = TRUE)){
              any_done = TRUE
              tmp_split = strsplit(tmp, p, perl = TRUE)[[1]]
              if(length(tmp_split) > 1){
                txt = switch(p,
                             ":1:" = 1:n_x,
                             ":01:" = str_fill(1:n_x, right = TRUE, symbol = "0"),
                             ":i:" = tolower(as.roman(1:n_x)),
                             ":I:" = as.character(as.roman(1:n_x)),
                             ":a:" = enum_letter(n_x),
                             ":A:" = toupper(enum_letter(n_x)))

                if(length(tmp_split) == 1){
                  tmp = paste0(tmp_split[[1]], txt)
                } else  {
                  tmp_new = paste0(tmp_split[[1]], txt, tmp_split[[2]])
                  k = 2
                  while(k + 1 <= length(tmp_split)){
                    k = k + 1
                    tmp_new = paste0(tmp_new, txt, tmp_split[[k]])
                  }
                  tmp = tmp_new
                }
              }

              break
            }
          }

          if(any_done){
            if(i == 1){
              left = tmp
            } else {
              right = tmp
            }
          }
        }

        if(any(nchar(left) > 0)){
          if(any(nchar(right) > 0)){
            res = paste0(left, x, right)
          } else {
            res = paste0(left, x)
          }
        } else {
          res = paste0(x, right)
        }
      }
    } else if(op == "insert"){
      # inserts an ELEMENT at the beginning/end
      
      if(group_flag == 2){
        stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                  "conditional statements (inside `~()`).",
                  "\nFIX: simply put this operation after or before the conditional statement.")
      }

      if(nchar(left) > 0){
        if(nchar(right) > 0){
          res = c(left, x, right)
        } else {
          res = c(left, x)
        }
      } else {
        res = c(x, right)
      }

    } else if(op %in% "Append"){
      # THIS IS DEPRECATED:
      # I give it just in case
      # appends **implicitly** at the beginning of the first/last string

      res = x

      if(any(grepl(":(?:n|N):", c(left, right)))){
        n = length(x)
        n_letter = ""
        if(any(grepl(":N:", c(left, right), fixed = TRUE))){
          n_letter = n_letter(n)
          left = gsub(":N:", n_letter, left)
          right = gsub(":N:", n_letter, right)
        }

        left = gsub(":n:", n, left)
        right = gsub(":n:", n, right)
      }

      if(is.null(extra)){
        extra = list()
      }

      if(nchar(left) > 0){
        extra$str_add_first = paste0(extra$str_add_first, left)
      }

      if(nchar(right) > 0){
        extra$str_add_last = paste0(extra$str_add_last, right)
      }

    }

    # END: paste/insert

  } else if(op == "fill"){
    # fill ####
    
    valid_options = c("right", "center")
    options = check_set_options(options, valid_options, op = op)
    
    info = extract_pipe(argument, op, double = FALSE, numeric = TRUE, mbt = FALSE)
    argument = info$value
    symbol = info$extra
    
    center = "center" %in% options
    right = "right" %in% options
    
    if(symbol == ""){
      symbol = " "
    }
    
    if(nchar(symbol) != 1){
      stop_hook("In the operator `fill`, the symbol used to fill must be of length 1.",
                "\nPROBLEM: the symbol, equal to {bq?symbol}, is of length {n?nchar(symbol)}.",
                "\nEXAMPLE: to fill with 0s: `fill.0`; of length 10 with underscores and right-aligned text: `'10|_'fill.right`.")
    }
    
    if(argument == ""){
      argument = NULL
    }
    
    if(!is.null(argument)){
      if(is_numeric_in_char(argument)){
        argument = as.numeric(argument)
      } else {
        stop_hook("In the operator `fill`, the argument (giving the length of the fill) must be numeric.",
                "\nPROBLEM: the argument, equal to {bq?argument}, is not numeric ",
                "(instead it is of class {enum ? class(argument)}.",
                "\nEXAMPLE: a fill of size 10: `10 fill`; of size 10 (using quotes) on the right: `'10'fill.right`.")
      }
    }
    
    res = str_fill(x, argument, symbol = symbol, right = right, center = center)
    
  } else if(op == "unik"){
    # unik ####

    if(!is.null(group_index) && group_flag == 2){

      # not really fast 
      # => to be improved
      df = data.frame(g = group_index, x = x, stringsAsFactors = FALSE)
      df_unik = unique(df)

      group_index = df_unik$g
      res = df_unik$x

    } else {
      
      res = unique(x)

      if(group_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("nth", "Nth", "ntimes", "Ntimes", "n", "N", "len", "Len")){
    #
    # nth, ntimes, n, len ####
    #
    
    # parsing the default values of the options and if we display in letters
    is_letter = substr(op, 1, 1) %in% c("N", "L")
    if(is_letter){
      op = tolower(op)
    }
    
    if(op == "nth"){
      opt_default = c("letter", "upper", "compact")
    } else if(op == "n"){
      opt_default = c("letter", "upper", "0", "zero")
    } else if(op == "len"){
      opt_default = c("letter", "upper", "format")
    } else {
      opt_default = c("letter", "upper")
    }

    options = check_set_options(options, opt_default)
    
    if(!is_letter){
      is_letter = any(c("letter", "upper") %in% options)
    }

    # operations
    if(op == "nth"){
      res = n_th(x, letters = is_letter, compact = "compact" %in% options)

    } else if(op == "ntimes"){
      res = n_times(x, letters = is_letter)

    } else if(op == "n"){
      is_zero = any(options %in% c("0", "zero"))
      # we force the conversion to numeric
      mark = if(is_zero) "" else ","
      if(is.numeric(x)){
        if(is_letter){
          res = n_letter(x)
        } else {
          res = format(x, big.mark = mark)
          
          if(is_zero){
            res = gsub(" ", "0", res, fixed = TRUE)
          } else {            
            res = cpp_trimws_in_place(res)
          }
        }
      } else {
        x_num = suppressWarnings(as.numeric(x))
        is_x_num = which(!is.na(x_num))
        num_val = x_num[is_x_num]

        if(is_letter){
          res_num = n_letter(num_val)
        } else {
          res_num = format(num_val, big.mark = mark)
          if(is_zero){
            res_num = gsub(" ", "0", res_num, fixed = TRUE)
          } else {            
            res_num = cpp_trimws_in_place(res_num)
          }
        }

        res = x
        res[is_x_num] = res_num
      }
      
    } else if(op == "len"){

      # if conditional: lengths of all the groups
      if(!is.null(group_index) && group_flag == 2){
        res = tabulate(group_index)
        group_index = seq_along(res)
      } else {
        if(group_flag == 1){
          group_index = 1
        }

        res = length(x)
      }

      if(is_letter){
        res = n_letter(res)
      } else if(opt_equal(options, "format")){
        res = format(res, big.mark = ",")
      }
    }

    if("upper" %in% options){
      res = gsub("^(.)", "\\U\\1", res, perl = TRUE)
    }
  } else if(op == "width"){
    # width, dtime ####
    
    sw = getOption("width")
    data = list(.sw = sw)

    info = extract_pipe(argument, op, double = FALSE, numeric = TRUE, data = data, mbt = FALSE)
    nb = info$value
    comment = info$extra
    
    if(nb == ""){
      nb = NULL
    }

    if(str_x(comment, -1) == "_"){
      # the underscore means that we don't add a space
      comment = str_trim(comment, -1)
    } else if(comment != ""){
      comment = paste0(comment, " ")
    }

    # code is slow but I don't see why would one want it to be super fast
    # (only for user-content, not performance/data-analysis content)

    res = character(length(x))
    for(i in seq_along(x)){
      res[i] = fit_screen(x[i], width = nb, leader = comment)
    }
    
  } else if(op == "dtime"){
    res = format_difftime(x, options)

  } else if(op == "erase"){
    # erase, rm, nuke, num ####
    #

    options = check_set_options(options, c("fixed", "ignore", "word"))

    if(argument == ""){
      res = character(length(x))
    } else {
      is_fixed = "fixed" %in% options
      is_word = "word" %in% options
      is_ignore = "ignore" %in% options

      qui = str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)

      res[qui] = ""
    }

  } else if(op %in% c("rm", "nuke")){
    options = check_set_options(options, c("empty", "blank", "noalpha", "noalnum", "all", 
                                           "fixed", "ignore", "word"))
                                           
    if(op == "nuke"){
      res = character(0)
      return(res)
    }
    
    res = NULL
    if(argument != ""){
      is_fixed = "fixed" %in% options
      is_word = "word" %in% options
      is_ignore = "ignore" %in% options

      qui = !str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)
      
      options = setdiff(options, c("fixed", "ignore", "word"))
      if("blank" %in% options){
        # 10 times faster than grepl("\\S", x, perl = TRUE)
        qui_blank = cpp_which_empty(x)
        if(length(qui_blank) > 0){
          qui[qui_blank] = FALSE
        } 
        
      } else if("empty" %in% options){
        qui = qui & x != ""
      }

    } else {
      options = setdiff(options, c("fixed", "ignore", "word"))
      if(length(options) == 0){
        opt = "empty"
      } else {
        opt = options[1]
      }

      # beware I define here the ones we should keep, **not* the ones we should drop

      if(opt == "empty"){
        qui = x != ""
      } else if(opt == "blank"){
        # 10 times faster than grepl("\\S", x, perl = TRUE)
        qui = cpp_which_empty(x)
        if(length(qui) == 0){
          qui = seq_along(x)
        } else {
          qui = -qui
        }
      } else if(opt == "noalpha"){
        qui = grepl("[\\p{L}]", x, perl = TRUE)
      } else if(opt == "noalnum"){
        qui = grepl("[\\p{L}[:digit:]]", x, perl = TRUE)
      } else if(opt == "all"){
        qui = integer(0) 
      }
    }

    if(is.logical(qui)){
      qui = which(qui)
    }

    res = x[qui]
    if(!is.null(group_index) && group_flag != 0){
      group_index = group_index[qui]
      group_index = cpp_recreate_index(group_index)
    }

  } else if(op == "num"){
    options = check_set_options(options, c("warn", "soft", "rm", "clear"))

    # warn: warning if failed conversions
    is_warn = "warn" %in% options
    # soft: if some numbers couldn't be parsed to numeric: no conversion is done
    is_soft = "soft" %in% options
    # rm: the on numeric are removed
    is_rm = "rm" %in% options
    # clear: the non numeric are turned into empty strings
    is_clear = "clear" %in% options

    is_valid_num = TRUE
    if(is_warn || is_soft){
      is_valid_num = is_numeric_in_char(x)
      if(!is_valid_num && is_warn){
        msg = if(!is_soft) "" else " No conversion is performed."
        warning("In operation `num`: when trying to convert to numeric, NAs were created.", 
                 msg, call. = FALSE)
      }
    }

    if(is_valid_num || !is_soft){
      res = suppressWarnings(as.numeric(x))
      if(is_rm || is_clear){
        if(anyNA(res)){
          qui_na = which(is.na(res))
          if(is_rm){
            res = res[-qui_na]
            if(!is.null(group_index) && group_flag != 0){
              group_index = group_index[-qui_na]
              group_index = cpp_recreate_index(group_index)
            }
          } else { 
            # is_clear
            res[qui_na] = ""
          }
        }
      }
    } else {
      res = x
    }

  } else if(op == "ascii"){
    # ascii, stop ####
    res = str_to_ascii(x, options)

  } else if(op == "stopwords"){

    # current limitation: does not work for quoted words
    #                     but quoted words are not stopwords usually
    # + the algo is quite inefficient

    # Snowball stopwords
    # These come from http://snowballstem.org/algorithms/english/stop.txt
    stopwords = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves",
                  "you", "your", "yours", "yourself", "yourselves", "he", "him",
                  "his", "himself", "she", "her", "hers", "herself", "it", "its",
                  "itself", "they", "them", "their", "theirs", "themselves", "what",
                  "which", "who", "whom", "this", "that", "these", "those", "am", "is",
                  "are", "was", "were", "be", "been", "being", "have", "has", "had",
                  "having", "do", "does", "did", "doing", "would", "should",
                  "could", "ought", "i'm", "you're", "he's", "she's", "it's",
                  "we're", "they're", "i've", "you've", "we've", "they've",
                  "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll",
                  "you'll", "he'll", "she'll", "we'll", "they'll", "isn't",
                  "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
                  "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't",
                  "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's",
                  "that's", "who's", "what's", "here's", "there's", "when's", "where's",
                  "why's", "how's", "a", "an", "the", "and", "but", "if", "or",
                  "because", "as", "until", "while", "of", "at", "by", "for",
                  "with", "about", "against", "between", "into", "through",
                  "during", "before", "after", "above", "below", "to", "from",
                  "up", "down", "in", "out", "on", "off", "over", "under",
                  "again", "further", "then", "once", "here", "there", "when",
                  "where", "why", "how", "all", "any", "both", "each", "few",
                  "more", "most", "other", "some", "such", "no", "nor", "not",
                  "only", "own", "same", "so", "than", "too", "very")

    n = length(x)
    x_split = strsplit(x, "(?<=[[:alnum:]])(?=[^[:alnum:]'])|(?<=[^[:alnum:]'])(?=[[:alnum:]])", perl = TRUE)
    x_len = lengths(x_split)
    x_vec = unlist(x_split)

    id = rep(1:n, x_len)

    # Lowering is costly, checking costs a bit less
    if(any(grepl("[[:upper:]]", x))){
      qui_drop = which(tolower(x_vec) %in% stopwords)
    } else {
      qui_drop = which(x_vec %in% stopwords)
    }

    x_vec = x_vec[-qui_drop]
    id = id[-qui_drop]
    
    res = cpp_paste_conditional(x_vec, id)

  } else if(op %in% c("if", "vif")){
    #
    # Conditions: if, vif ####
    #
    
    info = strsplit(argument, "_;;;_", fixed = TRUE)[[1]]
    
    cond_raw = info[[1]]
    true_section = info[[2]]
    is_false_section = length(info) == 3
    if(is_false_section){
      false_section = info[[3]]  
    } else {
      false_section = NULL
    }

    # computing the condition
    # a) parsing
    if(check){
      cond_parsed = check_expr(str2lang(cond_raw), 
            .sma("The string operation {bq?op} should be of the form ",
                 "{op}(condition ; true ; false). ",
                 "\nPROBLEM: the condition {bq?cond_raw} could not be parsed, see error below:"), 
            verbatim = TRUE)
    } else {
      cond_parsed = str2lang(cond_raw)
    }
    
    # b) evaluating
    if(is.character(cond_parsed)){
      # REGEX
      
      if(check){
        cond = check_expr(str_is(x, cond_parsed), 
                  .sma("The operation is of the form {bq?op}(cond ; true ; false). ",
                  "When `cond` is a pure character string, the function `str_is()` ",
                  "is applied with `cond` being the searched pattern.",
                  "\nPROBLEM: in {op}({'_;;;_ => ;'R ? argument}), the condition ",
                  "{bq?cond_raw} led to an error, see below:"),
                  verbatim = TRUE)
      } else {
        cond = str_is(x, cond_parsed)
      }      
      
    } else {
      vars = all.vars(cond_parsed)
      my_data = list()
      for(v in vars){
        if(v %in% c(".N", ".len")){
          my_data[[v]] = length(x)
        } else if(v %in% c(".nchar", ".C")){
          if(!is.character(x)){
            x = as.character(x)
          }
          my_data[[v]] = nchar(x)
        } else if(v == "."){
          my_data[[v]] = x
        }
      }
      
      if(check){
        cond = check_expr(eval(cond_parsed, my_data, envir), 
                  .sma("The string operation {bq?op} should be of the form ",
                       "{op}(condition ; true ; false). ",
                       "\nPROBLEM: the condition {bq?cond_raw} could not be ",
                       "evaluated, see error below:"), 
                   verbatim = TRUE)  
      } else {
        cond = eval(cond_parsed, my_data, envir)
      }
    }
    
    # further error checking
    if(!length(cond) %in% c(1, length(x))){
      stop_hook("In {bq?op} operations, the condition (here {bq?cond_raw}) must ",
                "be either of length 1 (applying to the full string), ",
                "either of the length of the interpolated value.", 
                "\nPROBLEM: the condition is of length {len.f?cond} while the ",
                "interpolated value is of length {len?x}.")
    }
    
    if(is.numeric(cond)){
      # numeric values are converted to logical
      cond = as.logical(cond)
    }
    
    if(!is.logical(cond)){
      stop_hook("In {bq?op} operations, the condition (here {bq?cond_raw}) must be logical.",
                "\nPROBLEM: the condition is not logical, instead it is of class ",
                "{enum.bq ? class(cond)}.")
    }
    
    # NA = FALSE
    if(anyNA(cond)){
      cond[is.na(cond)] = FALSE
    }
    
    #
    # Computing the operations
    #    
    
    is_elementwise = length(cond) > 1
    
    if(is_elementwise){
      x_true = x[cond]
      x_false = x[!cond]
    } else {
      if(cond){
        x_true = x
        x_false = character(0)
      } else {
        x_true = character(0)
        x_false = x
      }
    }
    
    # All operations that change the length of the element should be forbidden
    # we check that on the fly

    for(state in c("true", "false")){

      if(state == "true"){
        xi = x_true
      } else {
        xi = x_false
      }
      
      if(length(xi) == 0){
        next
      }
      
      if(state == "false" && !is_false_section){
        break
      }
      
      instruction = if(state == "true") true_section else false_section
      
      if(op == "if"){
        n_old = length(xi)
        cond_flag = +grepl("~(", instruction, fixed = TRUE)
        xi = apply_simple_operations(xi, op, instruction, check, envir, 
                                     group_flag = cond_flag, is_dsb, fun_name)
        
        if(is_elementwise && !length(xi) %in% c(0, n_old)){
          stop_hook("In {bq?op} operations, when conditions are of length > 1, the ",
                   "operations either a) can remove the string completely ",
                   "(e.g. with `nuke`), or b) must not change the length of the element.",
                   "\nPROBLEM: the original vector is of length {n?n_old} while after ",
                   "the operations it becomes of length {len.f ? xi}")
        }
      } else {
        # verbatim if
        if(grepl(".", instruction, fixed = TRUE)){
          data = list("." = xi)
        } else {
          data = list()
        }
        xi = smagick_internal(instruction, is_dsb = is_dsb, data = data, envir = envir, check = check, 
                                fun_name = fun_name)
      }
      
      if(state == "true"){
        x_true = xi
      } else {
        x_false = xi
      }
    }
    
    if(is_elementwise){
      
      if(length(x_true) == 0 && length(x_false) == 0){
        res = character(0)
        if(group_flag != 0){
          group_index = NULL
        }
      } else if(length(x_true) == 0){
        if(op == "vif" && length(x_false) == 1){
          res = x
          res[!cond] = x_false
        } else {
          res = x_false  
        }
        
        if(group_flag != 0){
          group_index = group_index[!cond]
        }
      } else if(length(x_false) == 0){
        if(op == "vif" && length(x_true) == 1){
          res = x
          res[cond] = x_true
        } else {
          res = x_true
        }
        
        if(group_flag != 0){
          group_index = group_index[cond]
        }
      } else {
        res = x
        res[cond] = x_true
        res[!cond] = x_false
      }
    } else {
      
      res = if(cond) x_true else x_false

      # we can add extra stuff, if needed

      extra_if = attr(res, "extra")
      if(length(extra_if) > 0){
        if(is.null(extra)){
          extra = list()
        }

        if(length(extra_if$element_add_first) > 0){
          extra$element_add_first = c(extra$element_add_first, extra_if$element_add_first)
        }

        if(length(extra_if$element_add_last) > 0){
          extra$element_add_last = c(extra$element_add_last, extra_if$element_add_last)
        }

        if(length(extra_if$str_add_first) > 0){
          extra$str_add_first = paste0(extra$str_add_first, extra_if$str_add_first)
        }

        if(length(extra_if$str_add_last) > 0){
          extra$str_add_last = paste0(extra$str_add_last, extra_if$str_add_last)
        }
      }
    }

  } else if(op == "~"){
    # Conditional: ~ ####
    res = apply_simple_operations(x, op, argument, check, envir, 
                                  group_flag = 2, is_dsb, fun_name)
                                  
    group_index = attr(res, "group_index")

  } else {
    # user-defined operations ####
    
    user_ops = getOption("smagick_user_ops")
    if(op %in% names(user_ops)){
      fun = user_ops[[op]]
      valid_options = attr(fun, "valid_options")

      if(!is.null(valid_options)){
        options = check_set_options(options, valid_options, free = any(valid_options == ""))
      }

      tmp = fun(x = x, argument = argument, options = options, group = group_index, 
                group_flag = group_flag)

      if(is.list(tmp)){
        if(!all(c("x", "group") %in% names(tmp))){
          pblm  = setdiff(c("x", "group") %in% names(tmp))
          stop_hook("The user-defined operation {bq?alias} is not well formed. The function should",
                    " return either a vector, either a list of exactly two elements 'x' and 'group'.",
                    "\nPROBLEM: it returns a list but the item{$s, are ? pblm} missing.")
        }
        res = tmp$x
        group_index = tmp$group
      } else {
        res = tmp
      }

    } else {
      msg = paste0("In `dsb`: the operator `", op, "` is not recognized. ",
                  "Internal error: this problem should have been spotted beforehand.")
      .stop_hook(bespoke_msg(msg))
    }
  }

  if(length(extra) > 0){
    attr(res, "extra") = extra
  }

  if(length(group_index) > 0){
    attr(res, "group_index") = group_index
  }

  return(res)
}



sma_pluralize = function(operators, xi, fun_name, is_dsb, envir, check){

  plural_len = operators[1] == "$"

  if(!plural_len){
    # xi should be a number
    is_pblm = length(xi) != 1 || !is.atomic(xi) || any(is.na(xi))

    if(!is_pblm && !is.numeric(xi)){
      xi = suppressWarnings(as.numeric(xi))
      is_pblm = any(is.na(xi))
    }

    if(!is_pblm){
      is_pblm = xi < 0
    }

    if(is_pblm){
      example = 'Example: x = 5; dsb("there .[#is, N ? x] cat.[#s] in the room.")'
      example = bespoke_msg(example, fun_name)

      extra = ""
      reason = NULL
      if(length(xi) != 1){
        extra = " single"
        reason = "PROBLEM: it is not of length 1."
      } else if(!is.atomic(xi)){
        reason = paste0("PROBLEM: it is not atomic (it is of class", class(xi)[1], ".")
      } else if(is.na(xi)){
        reason = "PROBLEM: It is NA."
      } else if(xi < 0){
        extra = " positive"
        reason = "PROBLEM: It is negative."
      }

      .stop_hook("The pluralization operator `#` applies to a", extra, " number. ", reason, ".\n",
              example)
    }
  }

  n = if(plural_len) length(xi) else xi
  IS_ZERO = n == 0
  IS_PLURAL = n > 1

  operators = operators[-1]
  n_op = length(operators)
  res = rep(NA_character_, n_op)
  for(i in 1:n_op){
    op_parsed = cpp_parse_operator(operators[i])

    op = op_parsed$operator
    options = op_parsed$options
    argument = op_parsed$argument

    if(op %in% c("es", "s", "y", "ies")){
      zero_case = opt_equal(options, c("zero", "0"))

      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      if(op %in% c("s", "es")){
        if(is_really_plural){
          res[i] = op
        } else {
          res[i] = ""
        }
      } else {
        if(is_really_plural){
          res[i] = "ies"
        } else {
          res[i] = "y"
        }
      }

    } else if(op %in% c("n", "N", "len", "Len")){

      is_letter = opt_equal(options, c("letter", "upper")) || substr(op, 1, 1) %in% c("N", "L")
      is_upper = opt_equal(options, "upper")

      if(opt_equal(options, "no") && IS_ZERO){
        if(is_upper){
          res[i] = "No"
        } else {
          res[i] = "no"
        }
      } else if(is_letter){
        val = n_letter(n)

        if(is_upper){
          val = gsub("^(.)", "\\U\\1", val, perl = TRUE)
        }

        res[i] = val
      } else {
        res[i] = format(n, big.mark = ",")
      }
      
    } else if(op %in% c("nth", "Nth")){
      is_compact = opt_equal(options, "compact") 
      is_letter = opt_equal(options, c("letter", "upper")) || substr(op, 1, 1) == "N"
      val = n_th(n, letters = is_letter, compact = is_compact)
      if(opt_equal(options, "upper")){
        val = gsub("^(.)", "\\U\\1", val, perl = TRUE)
      }
      res[i] = val

    } else if(op %in% c("ntimes", "Ntimes")){
      is_letter = opt_equal(options, "letter") || substr(op, 1, 1) == "N"
      res[i] = n_times(n, letters = is_letter)

    } else if(op %in% c("zero", "singular", "plural")){
      if((op == "zero" && IS_ZERO) || 
         (op == "singular" && !IS_PLURAL && !(any(grepl("zero$", operators)) && IS_ZERO)) || 
         (op == "plural" && IS_PLURAL)){
          
          value = smagick_internal(argument, is_dsb = is_dsb, envir = envir,
                                      slash = FALSE, check = check, fun_name = fun_name,
                                      plural_value = xi)
        if(value != ""){
          res[i] = value
        }        
      }
    } else if(op == "enum"){

      res[i] = enum_main(xi, options = options)

    } else {
      # The verb is always last

      if(nchar(op) < 2){
        example = 'Example: x = c("Charles", "Alice"); smagick("{$Is, enum ? x} crazy? Yes {$(he:they), are}.")'
        .stop_hook("In pluralization, `", op, "` is expected to be a verb ",
                   "and verbs must always be composed of at least two letters.\n", example)
      }

      zero_case = opt_equal(options, c("zero", "0"))
      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      res[i] = conjugate(op, is_really_plural)
    }

  }

  res = res[!is.na(res)]
  paste(res, collapse = " ")
}

sma_ifelse = function(operators, xi, xi_val, fun_name, envir, is_dsb, check){

  if(is.numeric(xi)){
    xi = xi != 0
  }
  
  amp = operators[1]
  is_double_amp = amp == "&&"
  if(!is.logical(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); smagick("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

    .stop_hook("The if-else operator `", amp, "`, of the form ", form,
            ", accepts only logical values in the condition. ",
            "PROBLEM: the value is not logical, but of class `",
            class(xi)[1], "`.", example)
  }

  if(anyNA(xi)){
    form = "{&cond ; true ; false}"
    if(is_double_amp) form = "{&&cond ; true}"
    example = '\nEXAMPLE: x = Sys.time(); smagick("Hello {&format(x, \'%H\') < 20 ; Sun ; Moon}!")'

    .stop_hook("The if-else operator `", amp, "`, of the form ", form,
              ", accepts only non-NA logical values.\n",
              "PROBLEM: the condition contains NA values.", example)
  }

  data = NULL
  BOX_OPEN = if(is_dsb) ".[" else "{"
  true = operators[3]
  false = operators[4]
  
  if(is_double_amp && !is.na(false)){
    .stop_hook("The if-else operator `&&`, of the form ", 
               "{&&cond ; true} does not accept a 'false' statement (since it will ",
               "be filled with the value of the variable in the condition). ",
              "\nFIX: remove the false statement or use the if-else operator `&` (single ampersand).",
              "\nEXAMPLE: x = 1:5",
              "           compare smagick(\"{x} = {&x %% 2;odd;even}\")",
              "\n              to smagick(\"{x} = {&x %% 2;odd}\")",
              "\n              to smagick(\"{x} = {&&x %% 2;odd}\")")
  }
  
  if(is.na(false)) false = ""
  
  true = cpp_extract_quote_from_op(true)
  false = cpp_extract_quote_from_op(false)
  
  n_x = length(xi)
  is_vector = n_x > 1
  
  if(is_vector){
    # if vector: true/false can be vectors
    for(i in 1:2){
      log_op = if(i == 1) true else false
      if(grepl(BOX_OPEN, log_op, fixed = TRUE)){
        if(is.null(data)){
          data = list("." = xi_val, ".N" = length(xi_val), ".len" = length(xi_val))
        }
        
        log_op_eval = smagick_internal(log_op, is_dsb = is_dsb, envir = envir, data = data,
                              slash = FALSE, check = check, fun_name = fun_name, 
                              plural_value = xi_val)
        
        if(!length(log_op) %in% c(1, n_x)){
          form = "{&cond ; true ; false}"
          if(is_double_amp) form = "{&&cond ; true}"
          .stop_hook("The if-else operator of the form {form} accepts values in ",
                      "{&i==1;true;false} which can be interpolated, as is the case here ",
                      "for {bq?log_op}.",
                      "\nThe length of the interpolated value must be equal to 1 or the length ",
                      "of the condition.",
                      "PROBLEM: the length of the condition is {n?n_x} while the length of ",
                      "`{&i==1;true;false}` is {len.format?log_op_eval}.")
        }
        
        if(i == 1){
          true = log_op_eval
        } else {
          false = log_op_eval
        }        
      }
    }
  }

  if(operators[1] == "&"){
    # we replace TRUE and FALSE with the strings, even empty
    
    true_long = length(true) > 1
    false_long = length(false) > 1
    
    if(true_long || false_long){
       if(true_long){
        res = true
        if(false_long){
          res[xi] = false[xi]
        } else {
          res[xi] = false
        }
       } else {
        res = false
        res[xi] = true
       }
       
       if(anyNA(xi)){
        res[is.na(xi)] = NA
       }
    } else {
      res = c(false, true)[xi + 1]
    }
    
  } else {
    # we change only the ones that need to be changed
    res = xi_val
    
    if(length(true) > 1){
      res[xi] = true[xi]
    } else {
      res[xi] = true
    }
  }

  # we allow nestedness only for single values
  if(length(res) == 1 && grepl(BOX_OPEN, res, fixed = TRUE)){
    if(is.null(data)){
      data = list("." = xi_val, ".N" = length(xi_val), ".len" = length(xi_val))
    }
    res = smagick_internal(res, is_dsb = is_dsb, envir = envir, data = data,
                           slash = FALSE, check = check, fun_name = fun_name, 
                           plural_value = xi_val)
  }

  res
}


apply_simple_operations = function(x, op, operations_string, check = FALSE, envir = NULL, 
                                 group_flag = 0, is_dsb = FALSE, fun_name = "smagick"){
                                  
  op_all = cpp_parse_simple_operations(operations_string, is_dsb)
    
  if(length(op_all) == 0){
    return(x)
  }
  
  if(identical(op_all[1], "_ERROR_")){
    op_msg = if(op =='~') "~(op1, op2)" else "if(cond ; op1, op2 ; op3, op4)"
    last = gsub("_;;;_", ";", tail(op_all, 1))
    extra = ""
    if(last %in% c("!", "?")){
      extra = smagick("The character {bq?last} is forbidden in operations.")
    } else if(last != "_ERROR_"){
      extra = smagick("Operations must be of the form `'arg'operator.option` but the value {bq?last} is ill formed.")
    } else {
      extra = smagick("The value {bq?operations} could not be parsed.")
    }
      
    msg = smagick("The operator {bq?op} expects a suite of valid operations (format: {bq?op_msg}). ",
               "\nPROBLEM: the operations were not formatted correctly. ", extra)
    
    .stop_hook(msg)
  }
  
  #
  # applying the operations
  #
  
  xi = x

  for(i in seq_along(op_all)){
    opi = op_all[i]

    op_parsed = sma_char2operator(opi, fun_name)

    if(op_parsed$eval){
      if(check){
        argument_call = check_expr(str2lang(op_parsed$argument), 
                                   .sma("In the operation `{op}()`, the ",
                                        "{&length(op_all) == 1 ; operation ; chain of operations} ",
                                        " {bq?operations_string} led to a problem.", 
                                        "In operation {bq?opi}, the argument in backticks ", 
                                        "(equal to {bq?op_parsed$argument}) is evaluated from the calling environment.",
                                        "\nPROBLEM: the argument could not be parsed."),
                                    verbatim = TRUE)

        argument = check_expr(eval(argument_call, envir),
                              "In the operation `{op}()`, the ",
                              "{&length(op_all) == 1 ; operation ; chain of operations} ",
                              " {bq?operations_string} led to a problem.", 
                              "In operation {bq?opi}, the argument in backticks ", 
                              "(equal to {bq?op_parsed$argument}) is evaluated from the calling environment.",
                              "\nPROBLEM: the argument could not be evaluated.")
      } else {
        argument = eval(str2lang(op_parsed$argument), envir)
      }

    } else {
      argument = op_parsed$argument
    }

    if(check){
      xi = check_expr(sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                                      group_flag = group_flag, is_dsb = is_dsb, fun_name = fun_name),
                                     "In the operation `{op}()`, the ",
                                     "{&length(op_all) == 1 ; operation ; chain of operations} ",
                                     " {bq?operations_string} led to a problem.", 
                                     "\nPROBLEM: the operation {opi} failed. Look up the doc?")
    } else {
      xi = sma_operators(xi, op_parsed$operator, op_parsed$options, argument, 
                         group_flag = group_flag, is_dsb = is_dsb, fun_name = fun_name)
    }
  }

  res = xi

}

setup_operations = function(){
  OPERATORS = c("/", "s", "S", "x", "X", "extract", "c", "C", "r", "R", "clean",
                "times", "each", "fill",
                "~", "if", "vif",
                "upper", "lower", "q", "Q", "bq", 
                "format", "Format", "%",
                "erase", "rm", "nuke", "paste", "insert", 
                "k", "K", "last", "first",
                "cfirst", "clast", "unik", "num", "enum",
                "rev", "sort", "dsort", "ascii", "title",
                "ws", "tws", "trim", "get", "is", "which",
                "n", "N", "len", "Len", "width", "dtime",
                "stopwords", "nth", "Nth", "ntimes", "Ntimes")
  options("smagick_operations" = sort(OPERATORS))
  options("smagick_operations_origin" = sort(OPERATORS))
}



