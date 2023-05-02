#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 09:40:24 2022
# ~: string ops core functions
#----------------------------------------------#


####
#### User-level ####
####

print.string_ops = function(x, ...){
  if(length(x) == 0){
    print(character(0))
  } else {
    cat(x, sep = "\n")
  }
}

####
#### ... str_op ####
####

#' Chains basic operations to character vectors
#'
#' Simple tool to perform multiple operations to character vectors.
#'
#' @param x A character vector. If not a character vector but atomistic (i.e. not a list), 
#' it will be converted to a character vector.
#' @param op Character **scalar**. Character scalar containing the comma separated values 
#' of operations to perform to the vector. For help type `dsb("--help")`.
#' @param pre_unik Logical scalar, default is `NULL`. Whether to first unique the vector 
#' before applying the possibly costly string operations, and merging back the result. 
#' For very large vectors with repeated values the time gained can be substantial. By 
#' default, this is `TRUE` for vector of length 1M or more.
#'
#' @return
#' In general it returns a character vector. It may ba of a length different from the original
#'  one, depending on the operations performed. 
#'
#' @author
#' Laurent Berge
#'
#' @examples
#' 
str_op = function(x, op, pre_unik = NULL){

  if(missing(x)){
    stop("Argument `x` must be provided. PROBLEM: it is currently missing.")
  }

  if(!is.atomic(x)){
    stop("Argument `x` must be atomic. Currently it is of class `", class(x)[1], "`")
  }

  if(!is.character(x)){
    x = as.character(x)
  }

  check_character(op, mbt = TRUE, scalar = TRUE)
  check_logical(pre_unik, null = TRUE, scalar = TRUE)

  # For very large vectors, we unique
  n = length(x)
  if(is.null(pre_unik)) pre_unik = n > 1e6

  if(pre_unik){
    x_int = to_integer(x)
    x_small = x[!duplicated(x_int)]

    res_small = str_op(x_small, op, pre_unik = FALSE)
    res = res_small[x_int]
  } else {
    operation = paste0(op, " ? x")
    res = .dsb(operation, string_as_box = TRUE)
  }

  if("group_index" %in% names(attributes(res))){
    attr(res, "group_index") = NULL
  }

  res
}


####
#### ... dsb ####
####

#' Simple and powerful string manipulation with the dot square bracket operator
#'
#' Compactly performs many low level string operations. Advanced support for pluralization.
#'
#'
#'
#' @param ... Character scalars that will be collapsed with the argument `sep`. You can 
#' use `".[x]"` within each character string to insert the value of `x` in the string. 
#' You can add string operations in each `".[]"` instance with the syntax `"'arg'op ? x"` 
#' (resp. `"'arg'op ! x"`) to apply the operation `'op'` with the argument `'arg'` to `x`
#'  (resp. the verbatim of `x`). Otherwise, what to say? Ah, nesting is enabled, and since 
#' there's over 30 operators, it's a bit complicated to sort you out in this small space. 
#' But type `dsb("--help")` to prompt an (almost) extensive help, or use the argument help.
#' @param frame An environment used to evaluate the variables in `".[]"`.
#' @param sep Character scalar, default is `""`. It is used to collapse all the elements in `...`.
#' @param vectorize Logical, default is `FALSE`. If `TRUE`, Further, elements in `...` are 
#' NOT collapsed together, but instead vectorised.
#' @param string_as_box Logical, default is `TRUE`. Whether the original character strings 
#' should be nested into a `".[]"`. If `TRUE`, then things like `dsb("S!one, two")` 
#' are equivalent to `dsb(".[S!one, two]")` and hence create the vector `c("one", "two")`.
#' @param collapse Character scalar or `NULL` (default). If provided, the resulting 
#' character vector will be collapsed into a character scalar using this value as a separator.
#'
#'
#' There are over 30 basic string operations, it supports pluralization, string operations can be 
#' nested (it may be the most powerful feature), operators have sensible defaults.
#'
#' See detailed help on the console with `dsb("--help")` or use the argument `help`. The real
#'  help is in fact in the "Examples" section.
#'
#'
#' @return
#' It returns a character vector whose length depends on the elements and operations in `".[]"`.
#'
#' @examples
#'
#' #
#' # BASIC USAGE ####
#' #
#'
#' x = c("Romeo", "Juliet")
#'
#' # .[x] inserts x
#' dsb("Hello .[x]!")
#'
#' # elements in ... are collapsed with "" (default)
#' dsb("Hello .[x[1]], ",
#'     "how is .[x[2]] doing?")
#'
#' # Splitting a comma separated string
#' # The mechanism is explained later
#' dsb("/J. Mills, David, Agnes, Dr Strong")
#'
#' # Nota: this is equivalent to (explained later)
#' dsb("', *'S !J. Mills, David, Agnes, Dr Strong")
#'
#' #
#' # Applying low level operations to strings
#' #
#'
#' # Two main syntax:
#'
#' # A) expression evaluation
#' # .[operation ? x]
#' #             | |
#' #             |  \-> the expression to be evaluated
#' #              \-> ? means that the expression will be evaluated
#'
#' # B) verbatim
#' # .[operation ! x]
#' #             | |
#' #             |  \-> the expression taken as verbatim (here 'x')
#' #              \-> ! means that the expression is taken as verbatim
#'
#' # operation: usually 'arg'op with op an operation code.
#'
#' # Example: splitting
#' x = "hello dear"
#' dsb(".[' 's ? x]")
#' # x is split by ' '
#'
#' dsb(".[' 's ! hello dear]")
#' # 'hello dear' is split by ' '
#' # had we used ?, there would have been an error
#'
#' # By default, the full string is nested in .[], so in that case no need to use .[]:
#' dsb("' 's ? x")
#' dsb("' 's ! hello dear")
#'
#' # There are 35+ string operators
#' # Operators usually have a default value
#' # Operations can be chained by separating them with a comma
#'
#' # Example: default of 's' is ' ' + chaining with collapse
#' dsb("s, ' my 'c ! hello dear")
#'
#' #
#' # Nesting
#' #
#'
#' # .[operations ! s1.[expr]s2]
#' #              |    |
#' #              |     \-> expr will be evaluated then added to the string
#' #               \-> nesting requires verbatim evaluation: '!'
#'
#' dsb("The variables are: .[C ! x.[1:4]].")
#'
#' # This one is ugly but it shows triple nesting
#' dsb("The variables are: .[w, C ! .[2* ! x.[1:4]].[','s, 4** !  ,_sq]].")
#'
#' #
#' # Splitting
#' #
#'
#' # s: split with fixed pattern, default is ' '
#' dsb("s ! a b c")
#' dsb("' b 's !a b c")
#'
#' # S: split with regex pattern, default is ',[ \t\n]*'
#' dsb("S !a, b, c")
#' dsb("'[[:punct:] ]+'S ! a! b; c")
#'
#' #
#' # Collapsing
#' #
#'
#' # c and C do the same, their default is different
#' # syntax: 's1||s2' with
#' # - s1 the string used for collapsing
#' # - s2 (optional) the string used for the last collapse
#'
#' # c: default is ' '
#' dsb("c ? 1:3")
#'
#' # C: default is ', || and '
#' dsb("C ? 1:3")
#'
#' dsb("', || or 'c ? 1:4")
#'
#' #
#' # Extraction
#' #
#'
#' # x: extracts the first pattern
#' # X: extracts all patterns
#' # syntax: 'pattern'x
#' # Default is '[[:alnum:]]+'
#'
#' x = "This years is... 2020"
#' dsb("x ? x")
#' dsb("X ? x")
#'
#' dsb("'\\d+'x ? x")
#'
#' #
#' # STRING FORMATTING ####
#' #
#'
#' #
#' # u, U, title: uppercase first/all letters
#'
#' # first letter
#' dsb("u ! julia mills")
#'
#' # title case: split -> upper first letter -> collapse
#' dsb("title ! julia mills")
#'
#' # upper all letters
#' dsb("U ! julia mills")
#'
#' #
#' # L: lowercase
#'
#' dsb("L ! JULIA MILLS")
#'
#' #
#' # q, Q, bq: single, double, back quote
#'
#' dsb("S, q, C ! Julia, David, Wilkins")
#' dsb("S, Q, C ! Julia, David, Wilkins")
#' dsb("S, bq, C ! Julia, David, Wilkins")
#'
#' #
#' # f, F: formats the string to fit the same length
#'
#' # f: the right side is filled with blanks
#' # F: the left side is filled with blanks
#'
#' score = c(-10, 2050)
#' nm = c("Wilkins", "David")
#' dsb("Monopoly scores:\n.['\n'c ! - .[f ? nm]: .[F ? score] US$]")
#'
#' # OK that example may have been a bit too complex,
#' # let's make it simple:
#'
#' dsb("Scores: .[f ? score]")
#' dsb("Names: .[F ? nm]")
#'
#' #
#' # w: white space normalization
#'
#' # w: suppresses trimming white spaces + normalizes successive white spaces
#' # Add the following letters in any order to:
#' # - p: remove punctuation
#' # - d: remove digits
#' # - i: remove isolated characters
#' #
#' # W is a shorthand to wp.
#'
#' dsb("w ! The   white  spaces are now clean.  ")
#'
#' dsb("W ! I, really -- truly; love punctuation!!!")
#'
#' # same: dsb("wp ! I, really -- truly; love punctuation!!!")
#'
#' dsb("wd ! 1, 2, 12, a microphone check!")
#'
#' dsb("wi ! 1, 2, 12, a microphone check!")
#'
#' dsb("wdi ! 1, 2, 12, a microphone check!")
#'
#' dsb("wpdi ! 1, 2, 12, a microphone check!")
#'
#' #
#' # %: applies sprintf formatting
#'
#' dsb("pi = .['.2f'% ? pi]")
#'
#' #
#' # a: appends text on each item
#' # ar: appends text on the right of each item
#' # syntax: 's1|s2'a, adds s1 at the beginning and s2 at the end of the string
#' # It accepts the special values :1:, :i:, :I:, :a:, :A:
#' # These values create enumerations (only one such value is accepted)
#'
#' # appending square brackets
#' dsb("'[|]'a, ' + 'c ! x.[1:4]")
#'
#' # Enumerations
#' acad = dsb("/you like admin, you enjoy working on weekends, you really love emails")
#' dsb("Main reasons to pursue an academic career:\n .[':i:) 'a, C ? acad].")
#'
#' #
#' # A: same as 'a' but adds at the begining/end of the full string (not on the elements)
#' # special values: :n:, :N:, give the number of elements
#'
#' characters = dsb("/David, Wilkins, Dora, Agnes")
#' dsb("There are .[':N: characters: 'A, C ? characters].")
#'
#' # Alternative with pluralization
#' dsb("There .[$is, N ? characters] character.[$s]: .[$enum].")
#'
#'
#' #
#' # stop: removes basic English stopwords
#' # the list is from the Snowball project:
#' #  http://snowball.tartarus.org/algorithms/english/stop.txt
#'
#' dsb("stop, w ! It is a tale told by an idiot, full of sound and fury, signifying nothing.")
#'
#' #
#' # k: keeps the first n characters
#' # syntax: nk: keeps the first n characters
#' #         'n|s'k: same + adds 's' at the end of shortened strings
#' #         'n||s'k: same but 's' counts in the n characters kept
#'
#' words = dsb("/short, constitutional")
#' dsb("5k ? words")
#'
#' dsb("'5|..'k ? words")
#'
#' dsb("'5||..'k ? words")
#'
#' #
#' # K: keeps the first n elements
#' # syntax: nK: keeps the first n elements
#' #         'n|s'K: same + adds the element 's' at the end
#' #         'n||s'K: same but 's' counts in the n elements kept
#' #
#' # Special values :rest: and :REST:, give the number of items dropped
#'
#' bx = dsb("/Pessac Leognan, Saint Emilion, Marguaux, Saint Julien, Pauillac")
#' dsb("Bordeaux wines I like: .[3K, ', 'C ? bx].")
#'
#' dsb("Bordeaux wines I like: .['3|etc..'K, ', 'C ? bx].")
#'
#' dsb("Bordeaux wines I like: .['3||etc..'K, ', 'C ? bx].")
#'
#' dsb("Bordeaux wines I like: .['3|and at least :REST: others'K, ', 'C ? bx].")
#'
#' #
#' # Ko, KO: special operator which keeps the first n elements and adds "others"
#' # syntax: nKo
#' # KO gives the rest in letters
#'
#' dsb("Bordeaux wines I like: .[4KO, C ? bx].")
#'
#' #
#' # r: string replacement with fixed search
#' # R: string replacement, perl regular expressions
#' # syntax: 's'R: deletes the content in 's' (replaces with the empty string)
#' #         's1 => s2'R replaces s1 into s2
#'
#' dsb("'e'r ! The letter e is deleted")
#'
#' # adding a perl look-behind
#' dsb("'(?<! )e'R !The letter e is deleted")
#'
#' dsb("'e => a'r !The letter e becomes a")
#'
#' dsb("'([[:alpha:]]{3})[[:alpha:]]+ => \\1.'R ! Trimming the words")
#'
#' # Alternative way with simple operations: split, shorten, collapse
#' dsb("s, '3|.'k, c ! Trimming the words")
#'
#' #
#' # *, *c, **, **c: replication, replication + collapse
#' # syntax: n* or n*c
#' # ** is the same as * but uses "each" in the replication
#'
#' dsb("N.[10*c ! o]!")
#'
#' dsb("3*c ? 1:3")
#' dsb("3**c ? 1:3")
#'
#' #
#' # d: replaces the items by the empty string
#' # -> useful in conditions
#'
#' dsb("d!I am going to be annihilated")
#'
#' #
#' # ELEMENT MANIPULATION ####
#' #
#'
#' #
#' # D: removes the element completely
#' # -> useful in conditions
#'
#' x = c("Destroy", "All")
#' dsb("D ? x")
#'
#' x = dsb("/1, 12, 123, 1234, 123456, 1234567")
#' # we delete elements whose number of characters is lower or equal to 3
#' # => see later section CONDITIONS
#' dsb("@<=3(D) ? x")
#'
#' #
#' # i, I: inserts an item
#' # syntax: 's1|s2'i: inserts s1 first and s2 last
#'
#' characters = dsb("/David, Wilkins, Dora, Agnes, Trotwood")
#' dsb("'Heep|Spenlow'i, C ? characters")
#'
#' #
#' # PLURALIZATION ####
#' #
#'
#' # Two ways to enable pluralization:
#' # .[$ command]: means the plural refers to the length of the object
#' # .[# commands]: means the plural refers to a number
#' # You must start with a dollar sign: this signals pluralization
#'
#' # Explanatory example
#' x = c("Eschyle", "Sophocle", "Euripide")
#' n = 37
#' dsb("The author.[$s, enum, have ? x] written .[#N ? n] play.[#s].")
#'
#' x = "Laurent Berge"
#' n = 0
#' dsb("The author.[$s, enum, have ? x] written .[#N ? n] play.[#s].")
#'
#' # How does it work?
#' # First is .[$s, enum, have ? x].
#' # The commands `s`, `enum` and `have` are applied to `x` which must come after a `?`
#' #    => there the plural (whether an s is added and how to conjugate the verb have) depends
#' #       on the **length** of the vector `x`
#' #
#' # Second comes .[#N ? n].
#' # The double dollar sign means that the command `N` will be applied to the **value** n.
#' # The value must come after the `?`
#' #
#' # Third is .[#s].
#' # The object to which `s` should be applied is missing (there is no `? n`).
#' # The default is to apply the command to the previous object. In this case,
#' #  this is `n`.
#'
#' # Another similar example illustrating that we need not express the object several times:
#' x = c("Eschyle", "Sophocle", "Euripide")
#' dsb("The .[$n ? x] classic author.[$s, are, enum].")
#'
#'
#' #
#' # CONDITIONS ####
#' #
#'
#' # There are three types of conditions:
#' # - on the number of elements
#' # - on the number of characters
#' # - on patterns
#'
#' # Conditions on the number of elements:
#' # - the syntax is: #<=3(true:false)
#' # - the operations listed in `true` will be applied to the full vector if it
#' #   contains 3 elements or less
#' # - the operations listed in `false` will be applied to the full vector if it
#' #   contains strictly more than 3 elements
#' # - the operation #(true:false) is a shorthand for #>1(true:false)
#'
#' dsb("#>=5('+'c : ' + 'c) ? 1:8")
#' dsb("#>=5('+'c : ' + 'c) ? 8:10")
#'
#' # Conditions on the number of characters
#' # - the syntax is: @<=3(true:false)
#' # - the operations listed in `true` will be applied to the elements with 3 characters or less
#' # - the operations listed in `false` will be applied to other elements
#' # - the operation @(true:false) is a shorthand for @>0(true:false)
#'
#' x = c("short", "long sentence")
#' dsb("@<=5('*|*'a:x) ? x")
#'
#' # Conditions on patterns:
#' # - the syntax is: <regex>(true:false)
#' # - regex should be a valid perl regular expression
#' # - the operations listed in `true` will be applied to the elements matching the regex
#' # - the operations listed in `false` will be applied to the other elements
#' # - use # first in the regex to trigger fixed search instead of regex-search
#' # - <#x^2>(true:false) will search for the fixed pattern 'x^2'
#'
#' # Keeping only elements containing a digit
#' x = c("hello", "age = 25", "young", "762")
#' dsb("<\\d>(D), C ? x")
#'
#' dsb("Elements with digits: .[<\\d>(:D), Q, C ? x].")
#'
#'
#' #
#' # ARGUMENTS FROM THE FRAME ####
#' #
#'
#' # Arguments can be evaluated from the calling frame.
#' # Simply use backticks instead of quotes.
#'
#' dollar = 6
#' reason = "glory"
#' dsb("Why do you develop packages? For .[`dollar`*c!$]?",
#'     "For money? No... for .[U,''s, c?reason]!", sep = "\n")
#'
#'
#'
#'
#'
dsb = function(..., frame = parent.frame(), sep = "", vectorize = FALSE,
               string_as_box = TRUE, collapse = NULL, help = NULL, use_DT = TRUE){


  if(!missing(vectorize)) check_logical(vectorize, scalar = TRUE)
  if(!missing(string_as_box)) check_logical(string_as_box, scalar = TRUE)
  if(!missing(collapse)) check_character(collapse, null = TRUE, scalar = TRUE)
  if(!missing(sep)) check_character(sep, scalar = TRUE)
  if(!missing(frame)) check_envir(frame)

  if(...length() == 0){
    if(missnull(help)){
      return("")
    }
  } else if(identical(..1, "--help")){
    sc = sys.call()
    if(identical(sc[[2]], "--help")){
      help = TRUE # means full help
    }
  }

  set_pblm_hook()

  res = string_ops_internal(..., is_dsb = TRUE, frame = frame, sep = sep,
                            vectorize = vectorize, string_as_box = string_as_box,
                            collapse = collapse, help = help, is_root = use_DT,
                            check = TRUE, fun_name = "dsb")

  if(inherits(res, "help")){
    return(invisible(NULL))
  }

  class(res) = c("string_ops", "character")
  res
}


#' @describeIn dsb Like `dsb` but without nice error messages (leads to slightly faster run times).
.dsb = function(..., frame = parent.frame(), sep = "", vectorize = FALSE,
                check = FALSE, string_as_box = FALSE, use_DT = FALSE){

  set_pblm_hook()
  string_ops_internal(..., is_dsb = TRUE, frame = frame,
                      string_as_box = string_as_box, sep = sep,
                      vectorize = vectorize, is_root = use_DT,
                      check = check, fun_name = ".dsb")
}



#' Title
#'
#' @param ...
#' @param sep
#' @param frame
#' @param string_as_box
#'
#' @return
#' @export
#'
#' @examples
dsb_cat = function(..., sep = "", frame = parent.frame(), string_as_box = FALSE){
  set_pblm_hook()

  res = string_ops_internal(..., is_dsb = TRUE, frame = frame,
                            string_as_box = string_as_box,
                            sep = sep, vectorize = FALSE, check = TRUE,
                            collapse = "", fun_name = "dsb_cat")

  cat(res)
}


#' @describeIn dsb_cat
dsb_msg = function(..., sep = "", frame = parent.frame(), string_as_box = FALSE,
                   appendLF = TRUE){
  set_pblm_hook()

  res = string_ops_internal(..., is_dsb = TRUE, frame = frame,
                            string_as_box = string_as_box,
                            sep = sep, vectorize = FALSE, check = TRUE,
                            collapse = "", fun_name = "dsb_msg")

  message(res, appendLF = appendLF)

}


#' Title
#'
#' @param ...
#' @param frame
#' @param vectorize
#' @param string_as_box
#'
#' @return
#' @export
#'
#' @examples
dsb_c = function(..., frame = parent.frame(), vectorize = FALSE,
                 string_as_box = TRUE){
  set_pblm_hook()

  dots = error_sender(list(...), "Some elements in `...` could not be evaluated.")

  n_dots = length(dots)

  if(n_dots == 0){
    return(NULL)
  }

  res = vector("list", n_dots)
  for(i in 1:n_dots){
    res[[i]] = string_ops_internal(dots[[i]], is_dsb = TRUE, frame = frame,
                                   string_as_box = string_as_box,
                                   sep = sep, vectorize = TRUE, check = TRUE,
                                   collapse = NULL, fun_name = "dsb_c")
  }

  unlist(res)
}

####
#### ... cub ####
####

#' @section Interpolation and string operations: principle:
#' 
#' To interpolate a variable, say `x`, simply use `{x}`. For example `x = "world"; cub("hello {x}")` leads 
#' to "hello world".
#' 
#' To any interpolation you can add operations. Taking the previous example, say we want to display
#'  "hello W O R L D"; that
#' is upper casing all letters of the interpolated variable and adding a space between 
#' each of them. Do you think we can do that? Of course yes: cub("hello {U, ''s, c ? x}"). And that's it.
#' 
#' Now let's explain what happened. Within the `{}` *box*, we first write a set of 
#' operations, here "U, ''s, c", then add "?" and finally write 
#' the variable to interpolate, "x".  The operations (explained in more details 
#' below) are U: upper casing all letters, ''s: splitting
#' with the empty string, 'c': concatenating with spaces the vector of string that was just split.
#' The question mark means that the expression coming after it is to be evaluated 
#' (this is opposed to the exclamation mark presented next).
#' 
#' The syntax is always the same: {operations ? expression}, where the operations 
#' is a *comma separated* list of operations.
#' These operations are of the form `'arg'op`, with `arg` the argument to the operator 
#' code `op`. These operations are performed sequantially from left to right.
#' 
#' @section  Verbatim interpolation and nesting: principle:
#' 
#' Instead of interpolating a variable, say `x`, with `{x}`, you can use an exclamation 
#' mark to trigger varbatim evaluation.
#' For example `cub("hello {!x}")` would lead to "hello x". Tadaa... Well that's a
#'  bit disappointing, right? What's the point of doing that? Wait until the next two paragraphs.
#' 
#' Verbatim evaluation is a powerful way to apply operations to plain text. For example:
#'  `cub("hello {U, ''s, c ! world}")` leads to "hello W O R L D".
#' 
#' A note in passing. The spaces surrounding the exclamation mark are non necessary,
#'  but when one space is present on both sides of the !, then the verbatim
#' expression only begins after it. Ex: "{U! hi}" leads to " HI" while "{U ! hi}" 
#' leads to "HI" and "{U !  hi}" leads to " HI".
#' 
#' The second advantage of verbatim evaluations is *nesting*. Anything in a verbatim 
#' expression is evaluated with the function `cub`.
#' This means that any *box* will be evaluated as previously described. Let's
#'  give an example. You want to write the expression of a polynomial of order n: a + bx + cx^2 + etc.
#' You can do that very easily with nesting. Assume we have `n = 2`.
#' 
#' Then `cub("poly({n}): {' + 'c ! {letters[1 + 0:n]}x^{0:n}}")` leads to 
#' "poly(2): ax^0 + bx^1 + cx^2".
#' 
#' How does it work? The verbatim expression (the one following the exclamation mark),
#'  here "{letters[1 + 0:n]}x^{0:n}", is evaluated with `cub`.
#' `cub("{letters[1 + 0:n]}x^{0:n}")` leads to the vector c("ax^0", "bx^1", "cx^2").
#' 
#' The operation `' + 'c` then concatenates (or collapses) that vector with ' + '.
#'  This value is then appended to the previous string.
#' 
#' We could refine by adding a cleaning operation in which we replace "x^0" and "^1" 
#' by the empty string. Let's do it:
#' 
#' `cub("poly({n}): {' + 'c, 'x\\^0|\\^1'r ! {letters[1 + 0:n]}x^{0:n}}")` leads to 
#' "poly(2): a + bx + cx^2", what we wanted.
#' 
#' You can try to write a function to express the polynomial as before: although it is 
#' a simple task, my guess is that it will require more typing.
#' 
#' @section Main string as a box:
#' 
#' As just seen, within a box you can add a set of operations to be performed. The thing is,
#'  by default the main string is considered to be, when appropriate, within a box.
#' This means that you can perform operations right from the start: `cub("U, ''s, c ! yes!")` leads to "Y E S !".
#' 
#' This is in fact equivalent to `cub("{U, ''s, c ! yes!}")`. Note that this behavior 
#' is triggered only when appropriate.
#' However in some instances it may interfere with what the user really wants. You can 
#' disable this feature with `string_as_box = FALSE`.
#' 
#' @section Operations without arguments:
#' 
#' As seen in the previous sections, within a *box* (i.e. `"{}"`), multiple operations
#'  can be performed.
#' We can do so by stacking the operations codes and in a comma separated enumeration.
#' There are two types of operations, with and without arguments. Here we cover the later.
#' 
#' + l, u, U, title: to modify the case of the string. l: all letters to lowercase, u: 
#' first letter to uppercase, U: all letters to uppercase, title: title case.
#' + w: to normalize the whitespaces (WS). It trims the whitespaces and transform any succession 
#' of whitespaces into a single one.
#'   + you can append d, i, p to this operation code in any order to: d: clean alll digits,
#'  i: clean isolated letters, p: clean punctuation.
#'     This means that wp cleans all punctuation and normalizes WS. And wpi cleans 
#' all punctuation, removes all isolated letters and normalizes WS.
#'     **Important note:** punctuation (or digits) are replaced with WS and **not** 
#' the empty string. This means that `cub("wp ! Meg's car")` will become "Meg s car".
#' + q, Q, bq: to add quotes to the strings. q: single quotes, Q: double quotes, bq: 
#' back quotes. `x = c("Mark", "Pam"); cub("Hello {q,C?x}!")` leads to "Hello 'Mark' and 'Pam'!".
#' + f, F: applies the base R's function format to the string. f: format with left
#'  alignment, F: format with right alignment.
#'   Ex: `x = c(1, 12345); cub("left: {f,q,C?x}, right: {F,q,C?x}")` 
#' leads to "left: '1     ' and '12,345', right: '     1' and '12,345'".
#' + d: replaces the content with the empty string (useful in conditions, see section below).
#' + D: deletes all content (useful in conditions, see section below). Ex: we want 
#' to display only elements with digits. x = c("Flora", "62", "James", "32"); 
#' cub("Ages: {<\\d>(:D),C?x}.") leads to "Ages: 62 and 32.".
#' + e: removes empty strings. Ex: x = c("", "hello", "!"); cub("Non-empty: {e,c?x}") 
#' leads to "Non-empty: hello !".
#' + E: removes strings containing only whitespaces or punctuation. x = c("", "hello", "!"); 
#' cub("Non-Empty: {E,c?x}") leads to "Non-Empty: hello".
#' + stop: removes basic English stopwords (the snowball list is used). 
#' The stopwords are replaced with an empty space but the left and right WS are 
#' untouched. So WS normalization may be needed (see operation w).
#'   `x = c("He is tall", "He isn't young"); cub("Is he {stop,w,C?x}?")` leads to "Is he tall and young?".
#' + ascii: turns all letters into ascii with transliteration. Non ascii elements
#'  are transformed into question marks.
#' + sort, dsort: sorts the elements. sort: sorts increasingly, dsort: sorts by decreasing order.
#' + rev: reverses the elements.
#' + num: converts to numeric silently (without warning).
#' + enum: enumerates the elements. It creates a single string containing the comma 
#' separated list of elements.
#'   If there are more than 7 elements, only the first 6 are shown and the number of
#'  items left is written.
#'   For example cub("enum ? 1:5") leads to "1, 2, 3, 4, and 5".
#'   You can add the following options by appending the letter to enum after a dot:
#'   + q, Q, or bq: to quote the elements
#'   + or: to finish with an 'or' instead of an 'and'
#'   + i, I, a, A, 1: to enumerate with this prefix, like in: i) one, and ii) two
#'   + a number: to tell the number of items to display
#'   Ex1: x = c("Marv", "Nancy"); cub("The main characters are {enum ? x}.") leads to 
#' "The main characters are Marv and Nancy.".
#'   Ex2: x = c("orange", "milk", "rice"); cub("Shopping list: {enum.i.q ? x}.") leads to
#'  "Shopping list: i) 'orange', ii) 'milk', and iii) 'rice'."
#' + nth, Nth: when applied to a number, these operators write them as a rank.
#'   Example: n = c(3, 7); cub("They finished {nth, enum ? n}!") leads to
#'   "They finished 3rd and 7th!". The upper case Nth operator tries to write
#'   the numbers in letters, but note that it stops at 20 (then numbers are used, as in nth).
#'   In the previous example, Nth would lead to "They finished third and seventh!".
#' 
#' @section Operations with arguments:
#' For operations with arguments, the syntax is (in general) of the form 'arg'op with arg the
#'  value of the argument and op the operation code.
#' Here is the list of operations with arguments:
#' + s, S: to split a string. s: default is ' ' and uses fixed pattern, S: default is 
#' ',[ \t\n}*' and uses regular expression patterns.
#' + c, C: to concatenate multiple strings into a single one. The two operations are 
#' identical, only their default change. c: default is ' ', C: default is ', || and '.
#'   The syntax of the argument is 's1' or 's1||s2'. s1 is the string used to concatenate 
#' (think paste(x, collapse = s1)). If an argument of the form 's1||s2' is used,
#'   then s2 will be used to concatenate the last two elements. Ex1: x = 1:4; 
#' cub("Et {' et 'c?x}!") leads to "Et 1 et 2 et 3 et 4!".
#'   Ex2: cub("Choose: {', || or '?2:4}?") leads to "Choose: 2, 3 or 4?".
#' + x, X: extracts patterns from a string. The pattern must be a regular expression. 
#' Both have the same default: '[[:alnum:]]+'. x: extracts the first match, X: extracts **all** the matches.
#'   Ex1: x = c("6 feet under", "mahogany") ; cub("'\\w{3}'x ? x") leads to the vector c("fee", "mah").
#'   Ex2: x = c("6 feet under", "mahogany") ; cub("'\\w{3}'X ? x") leads to the
#'  vector c("fee", "und", "mah", "oga").
#' + r, R: replacement within a string. r: uses fixed search, R: uses regular expressions.
#'  The syntax is 'old', 'old => new', 'old_=>_new', or 'old=>new'
#'   with old the pattern to find and new the replacement. If new is missing, it is 
#' considered the empty string. Ex1: cub("'e'r ! the letter e is gone") leads to "th lttr  is gon".
#'   Ex2: cub("'(?<!\\b)e => a'R ! the letter e is gone") leads to "tha lattar e is gona".
#' + `*`, `*c`, `**`, `**c`: replicate the elements. These operations replicate the elements 
#' a certain number of times given as argument.
#'   The c means that after the replication the elments are collapsed with the empty string.
#'  If n is the argument: two stars means that each element
#'   is repeated n times while one star means that the entire vector is repeated n times.
#'  For this operation you can omit the single quotes: `"'5'*"` is similar to `"5*"`
#'   Ex1: cub("yes{10 times.c ! !}") leads to yes!!!!!!!!!!. Ex2: n = 2; cub("', 'c ! {2* ? 
#' letters[1:n]}{`n`** ? 1:2}") leads to "a1, b1, a2, b2".
#' + first, last: to select the firs/last elements. The syntax is 'n'first or firstn or 
#' first.n with n a number (same for last).
#'   Ex: cub("'15'first, last3 ? letters") leads to the vector "m", "n", "o".
#' + cfirst, clast: to select the first/last characters of each element. The syntax is
#'  'n'cfirst, cfirstn, cfirst.n with n a number (same for clast).
#'   Ex: cub("cfirst.20, '9'clast! This is a very long sentence") leads to "very long".
#' + %: applies sprintf formatting. The syntax is 'arg'% with arg an sprintf formatting,
#'  e.g. '.3f' (float with 3 digits), or '5s' (string of width 5).
#'   Ex: cub("pi = {'.3f'% ? pi}")
#' + k: to keep only the first n characters (like cfirst but with more options). The
#'  syntax is nk, 'n'k, 'n|s'k or 'n||s'k with n a number and s a string.
#'   n provides the number of characters to keep. Optionnaly, only for strings whose
#'  length is greater than n, after truncation, the string s can be appended at the end.
#'   The differenc e between 'n|s' and 'n||s' is that in the second case the strings
#'  will always be of maximum size n, while in the first case they can be of length n + nchar(s).
#'   Ex: cub("4k ! long sentence") leads to "long",  cub("'4|..'k ! long sentence") 
#' leads to "long..", cub("4k||.. ! long sentence") leads to "lo..".
#' + K, Ko, KO: to keep only the first n elements (like first but with more options). 
#' The syntax is nK, 'n'K, 'n|s'K, 'n||s'K. The values Ko and KO only accept the two first syntax (with n only).
#'   n provides the number of elements to keep. If s is provided and the number of 
#' elements are greater than n, then in 'n|s' the string s is added at the end, and
#'  if 'n||s' the string s replaces the nth element.
#'   The string s accepts specials values:
#'   + :n: or :N: which give the total number of items in digits or letters (N)
#'   + :rest: or :REST: which give the number of elements that have been truncated in digits or letters (REST)
#'   Ex: cub("'3|:rest: others'K ? 1:200") leads to the vector "1", "2", "3", "197 others".
#'   + The operator 'n'Ko is like 'n||:rest: others'K and 'n'KO is like 'n||:REST: others'K.
#' +
#' 
#' @section Group-wise operations:
#' 
#' 
#' 
#' @section Pluralization:
#' 
#' 
#' 
#' 
#' ** conditions
#' 
cub = function(..., frame = parent.frame(), sep = "", vectorize = FALSE,
               string_as_box = TRUE, collapse = NULL, use_DT = TRUE){


  if(!missing(vectorize)) check_logical(vectorize, scalar = TRUE)
  if(!missing(string_as_box)) check_logical(string_as_box, scalar = TRUE)
  if(!missing(collapse)) check_character(collapse, null = TRUE, scalar = TRUE)
  if(!missing(sep)) check_character(sep, scalar = TRUE)
  if(!missing(frame)) check_envir(frame)

  if(...length() == 0){
    if(missnull(help)){
      return("")
    }
  } else if(identical(..1, "--help")){
    sc = sys.call()
    if(identical(sc[[2]], "--help")){
      help = TRUE # means full help
    }
  }

  set_pblm_hook()

  # is_root is only used to enable DT evaluation

  res = string_ops_internal(..., is_dsb = FALSE, frame = frame, sep = sep,
                            vectorize = vectorize, string_as_box = string_as_box,
                            collapse = collapse, is_root = use_DT,
                            check = TRUE, fun_name = "cub")

  if(inherits(res, "help")){
      return(invisible(NULL))
  }

  class(res) = c("string_ops", "character")
  res
}


.cub = function(..., frame = parent.frame(), sep = "", vectorize = FALSE,
                check = FALSE, string_as_box = FALSE, use_DT = FALSE){

  set_pblm_hook()

  string_ops_internal(..., is_dsb = FALSE, frame = frame,
                      string_as_box = string_as_box, sep = sep,
                      vectorize = vectorize, is_root = use_DT,
                      check = check, fun_name = ".cub")
}


cub_cat = function(..., sep = "", frame = parent.frame(), string_as_box = FALSE){
  set_pblm_hook()

  res = string_ops_internal(..., is_dsb = FALSE, frame = frame,
                            string_as_box = string_as_box,
                            sep = sep, vectorize = FALSE, check = TRUE,
                            collapse = "", fun_name = "cub_cat")

  cat(res)
}


cub_msg = function(..., sep = "", frame = parent.frame(), string_as_box = FALSE,
                   appendLF = TRUE){
  set_pblm_hook()

  res = string_ops_internal(..., is_dsb = FALSE, frame = frame,
                            string_as_box = string_as_box,
                            sep = sep, vectorize = FALSE, check = TRUE,
                            collapse = "", fun_name = "cub_msg")

  message(res, appendLF = appendLF)

}


cub_c = function(..., frame = parent.frame(), vectorize = FALSE,
                 string_as_box = TRUE){
  set_pblm_hook()

  dots = error_sender(list(...), "Some elements in `...` could not be evaluated.")

  n_dots = length(dots)

  if(n_dots == 0){
    return(NULL)
  }

  res = vector("list", n_dots)
  for(i in 1:n_dots){
    res[[i]] = string_ops_internal(dots[[i]], is_dsb = FALSE, frame = frame,
                                   string_as_box = string_as_box,
                                   sep = sep, vectorize = TRUE, check = TRUE,
                                   collapse = NULL, fun_name = "cub_c")
  }

  unlist(res)
}

####
#### Help ####
####

setup_help = function(){


  msg = c(
    "# Welcome to dsb help",
    "Usage: dsb(s) with 's' a character string",
    " ",
    "# BASIC usage ------------|",
    "    dsb evaluates anything in '.[]' and inserts it in 's'.",
    '    Ex: if x = "John", then dsb("Hi .[x]!") -> "Hi John!"',
    " ",
    "# STRING OPERATIONS ------|",
    "    Each .[] instance supports one or more string operations.",
    "    The syntax is .['arg'op?x] or .['arg'op!x], with:",
    "      - 'arg' a quoted string used as argument,",
    "      - op an operator code,",
    "      - ? or !:",
    "        + ?: evaluates the expression x",
    "        + !: takes x as verbatim",
    "      - x an expression to be evaluated or some verbatim text (no quote needed).",
    '    Ex: dsb(".[\' + \'c?1:3] = 6") -> "1 + 2 + 3 = 6". 1:3 is collapsed (c) with \' + \'.',
    "",
    "    Using ! instead of ? applies the operation to the *verbatim* of the expression.",
    '    Ex: dsb(".[\': => 2\'r!1:3] = 6") -> "123 = 6".',
    "        In the string '1:3', ':' is replaced (r) with '2'.",
    "",
    "    Operations can be chained using comma-separation. The syntax is: .['s1'op1, 's2'op2?x]",
    "    Evaluations are from left to right.",
    '    Ex: dsb(".[\': => 2\'r, \'\'s, \' + \'c!1:3] = 6") -> "1 + 2 + 3 = 6',
    "        1) '1:3'            -> ':' is replaced (r) with '2'  -> '123',",
    "        2) '123'            -> is split (s) with ''          -> c('1', '2', '3')",
    "        3) c('1', '2', '3') -> is collapsed (c) with ' + '   -> '1 + 2 + 3'",
    "",
    "    Nesting works, but only in verbatim components.",
    "    Ex: x = c(\"Doe\", \"Smith\")",
    "        dsb(\"Hi .[' and 'c!John .[x]]\") -> \"Hi John Doe and John Smith\"",
    "",
    "    If string_as_box = TRUE (default), the initial string is implicitly embedded in .[] ",
    "  and considered as verbatim. This means that operations can be applied from the start.",
    "    Ex: dsb(\"', 's!a1, b3, d8\") -> c(\"a1\", \"b3\", \"d8\")",
    "",
    "    Operators have default values, so the quoted argument is optional.",
    '    Ex: dsb("c?1:3") -> "1 2 3". 1:3 is collapsed (c) with \' \', its default.',
    "",
    "# OPERATORS --------------|",
    "    Below is the list of operators and their default argument when relevant.",
    "    OPERATOR _TAB_ DEFAULT _TAB_ VALUE ",
    "",
    "  Splitting:",
    "    - splits a character vector with 's' or 'S' (regex enabled)",
    "    - uses strsplit(x, split = 's')",
    "    s _TAB_ ' ' _TAB_ split, fixed = TRUE",
    "    S _TAB_ ',[ \\t\\n]*' _TAB_ split, perl = TRUE",
    "    ex: dsb(\"'[[:punct:] ]'S ! x^2 = 4\")",
    "",
    "  Extraction:",
    "    - extracts one ('x') or several ('X') patterns from a character string",
    "    x _TAB_ '[[:alnum:]]+' _TAB_ extracts the first pattern, perl = TRUE",
    "    X _TAB_ '[[:alnum:]]+' _TAB_ extracts all patterns, perl = TRUE",
    "    ex: x = c(\"6 feet under\", \"mahogany\") ; dsb(\"'\\\\w{3}'x ? x\")",
    "",
    "  Collapse:",
    "    - collapses a vector with paste(x, collapse = 's'),",
    "    - use a double pipe to apply a special collapse to the last values,",
    "    - the syntax is 's1||s2', s2 will be applied to the last 2 values,",
    "    - .[', || and 'c!1:3] -> \"1, 2 and 3\".",
    "    c _TAB_ '' _TAB_ collapse",
    "    C _TAB_ ', || and ' _TAB_ collapse",
    "    ex: dsb(\"C ? 1:5\")",
    "",
    "  Replication:",
    "    - use '5'times to replicate 5 times,",
    "    - you can avoid using quotes, as in: `5 times`",
    "    times _TAB_ NO DEFAULT _TAB_ replicates n times",
    "    times.c _TAB_ NO DEFAULT _TAB_ replicates n times, then collapses with ''",
    "    each _TAB_ NO DEFAULT _TAB_ replicates n times, each",
    "    each.c _TAB_ NO DEFAULT _TAB_ replicates n times, each, then collapses with ''",
    "    ex: dsb(\"yes.[10*c ! !]\")",
    "",
    "  Replacement: ",
    "    - the syntax is 'old => new', 'old=>new' or 'old_=>_new',",
    "    - if new is missing, it is considered as the empty string,",
    "    - they have no default",
    "    r _TAB_ NO DEFAULT _TAB_ replacement, fixed = TRUE ",
    "    R _TAB_ NO DEFAULT _TAB_ replacement, perl = TRUE ",
    "",
    "",
    "  Operators without arguments:",
    "    u _TAB_  _TAB_ puts the first letter of the string to uppercase",
    "    U _TAB_  _TAB_ puts the complete string to uppercase",
    "    l, L _TAB_  _TAB_ puts the complete string to lowercase",
    "    title _TAB_  _TAB_ puts the string in title case",
    "    q _TAB_  _TAB_ adds single quotes",
    "    Q _TAB_  _TAB_ adds double quotes",
    "    bq _TAB_  _TAB_ adds back quotes",
    "    f _TAB_  _TAB_ applies format(x)",
    "    F _TAB_  _TAB_ applies format(x, justify = 'right')",
    "    d _TAB_  _TAB_ replaces the content with the empty string",
    "    D _TAB_  _TAB_ removes all elements (useful in conditions)",
    "    e _TAB_  _TAB_ removes empty strings",
    "    E _TAB_  _TAB_ removes strings containing only white spaces or punctuation",
    "    stop _TAB_  _TAB_ removes basic English stop words",
    "    ascii _TAB_  _TAB_ transforms all letters into ASCII with transliteration",
    "    sort _TAB_  _TAB_ sorts all elements",
    "    dsort _TAB_  _TAB_ sorts all elements decreasingly",
    "    rev _TAB_  _TAB_ reverts the order of the elements",
    "    num _TAB_  _TAB_ converts the string to numeric (silently)",
    "",
    "  Normalizing white spaces and cleaning:",
    "    - the `w` commands normalizes the white spaces (trims left and right and suppresses duplicate WS)",
    "    - you can add additionnal parameters to further clean:",
    "      + p: punctuation",
    "      + d: digits",
    "      + i: isolated characters",
    "    - you can add any of them in any order after the w. ex: `wi`, `wd`, `wpd`, all work.",
    "    - uppercase `w` (ie `W`) is equivalent to `wp`",
    "    - note that the algorithm is quite fast but you may lose the encoding if the data is non-ascii",
    "    ex: x = \"  bad,,  text! 77 format? \" ; dsb(\"w ? x\") ; dsb(\"wpd ? x\")",
    "",
    "  Enumerations:",
    "    - the `enum` command creates an enumeration of the elements",
    "    - by default, it creates a comma separated list of elements finishing with 'and'",
    "    - in case there are more than 7 elements, only the first 6 are shown and the number of items left is written",
    "    - after this keyword, you can add the following options:",
    "      + q, Q, bq: to quote the elements",
    "      + or: to finish with an 'or' instead of an 'and'",
    "      + i, I, a, A, 1: to enumerate with this prefix, like in: i) one, and ii) two",
    "      + a number: to tell the number of items to display",
    "    ex: x = c(\"Marv\", \"Nancy\"); dsb(\"The main characters are .[enum ? x].\") ",
    "        x = c(\"orange\", \"milk\", \"rice\"); dsb(\"Shopping list: .[enum.i.q ? x].\") ",
    "",
    "  sprintf formatting:",
    "    - applies a formatting via sprintf,",
    "    - .['.3f'%?x] is equivalent to sprintf('%.3f', x),",
    "    - you can also use `%.3f` directly: .[%.3f ? pi]",
    "    % _TAB_ NO DEFAULT _TAB_ applies sprintf formatting",
    "    ex: dsb(\"pi = .[%.2f ? pi]\")",
    "",
    "  Selecting the first/last elements/characters",
    "    - syntax is 'n'first or firstn",
    "    first _TAB_ NO DEFAULT _TAB_ selects the first n",
    "    last _TAB_ NO DEFAULT _TAB_ selects the last n",
    "    cfirst _TAB_ NO DEFAULT _TAB_ selects the first n characters",
    "    clast _TAB_ NO DEFAULT _TAB_ selects the last n characters",
    "    ex: dsb(\"'15'first, last3 ? letters\")",
    "",
    "  Keeping a selected number of characters or elements: ",
    "    - the syntax is n K, 'n'K, 'n|s'K, or 'n||s'K",
    "    - with 'n' a number and 's' a string,",
    "    - K keeps the first n elements and drops the rest,",
    "    - k keeps the first n characters of each element,",
    "    - optionaly a string 's' can be appended at the end,",
    "    - but only when the length is greater than n.",
    "    - 'n|s' simply appends 's' while 'n||s' includes the length of 's' in the calculation of the final length.",
    "    - the special markup :rest: and :REST: can be used in 's'.",
    "    - ex: .['5||..'k!c('hello', 'bonjour')] -> c('hello', 'bon..')",
    "    -     .['5|..'k!c('hello', 'bonjour')]  -> c('hello', 'bonjo..')",
    "    k _TAB_ NO DEFAULT _TAB_ keeps the first n characters",
    "    K _TAB_ NO DEFAULT _TAB_ keeps the first n elements",
    "    Ko _TAB_ NO DEFAULT _TAB_ keeps the first n elements and adds 'others'",
    "    ex: dsb(\"4Ko, C ? 1:200\")",
    "",
    "  Insertion and appending operators:",
    "    - the syntax is 's1|s2' to insert, or append, the string s1 first and the string 's2' last.",
    "    - the syntax is 's' for `ar` and `da`",
    "    - note that '_|_' can also be used to separate first/last.",
    "    - the uppercase versions insert/append invisibly: ie without changing the vector.",
    "    a _TAB_ NO DEFAULT _TAB_ equiv. to paste0(s1, x, s2)",
    "    ar _TAB_ NO DEFAULT _TAB_ appends right; equiv. to paste0(x, s)",
    "    da _TAB_ NO DEFAULT _TAB_ deletes the content and appends s",
    "    A _TAB_ NO DEFAULT _TAB_ appends s1/s2 to the beginning/end of the full string",
    "    i _TAB_ NO DEFAULT _TAB_ inserts s1/s2 at the beginning/end of the vector",
    "    I _TAB_ NO DEFAULT _TAB_ inserts s1/s2 at the beginning/end of the vector",
    "",
    "# CONDITIONS--------------|",
    "  There are three types of conditions:",
    "    - on the number of characters, with @",
    "    - on the number of elements, with #",
    "    - on the patterns, with <regex>",
    "",
    "  Condition on the number of characters",
    "    - the syntax is: @<=3(true:false)",
    "    - the operations listed in `true` will be applied to the elements with 3 characters or less",
    "    - the operations listed in `false` will be applied to other elements",
    "    - the operation @(true:false) is a shorthand for @>0(true:false)",
    "    ex: x = c(\"short\", \"long sentence\") ; dsb(\"@<=5('*|*'a:x) ? x\")",
    "",
    "  Condition on the number of elements:",
    "    - the syntax is: &<=3(true:false)",
    "    - the operations listed in `true` will be applied to the full vector if it contains 3 elements or less",
    "    - the operations listed in `false` will be applied to the full vector if it contains strictly more than 3 elements",
    "    - the operation #(true:false) is a shorthand for #>1(true:false)",
    "    ex: dsb(\"#>=5('+'c : ' + 'c) ? 1:8\") ; dsb(\"#>=5('+'c : ' + 'c) ? 8:10\")",
    "",
    "  Condition on the patterns:",
    "    - the syntax is: <regex>(true:false)",
    "    - regex should be a valid perl regular expression",
    "    - the operations listed in `true` will be applied to the elements matching the regex",
    "    - the operations listed in `false` will be applied to the other elements",
    "    - use # first in the regex to trigger fixed search instead of regex-search",
    "    - <#x^2>(true:false) will search for the fixed pattern 'x^2'",
    "    ex: x = c(\"hello\", \"age = 25\", \"young\") ; dsb(\"<\\\\d>(D) ? x\")",
    "",
    "# CONDITIONAL OPERATIONS--------------|",
    "  Some operations turn a single element into several elements (e.g.: `s` or `X`)",
    "  You can use the conditional operator `~(ops)` to perfom operations within each element.",
    "    - operations listed in `~(ops)` will be applied separately for each element that has been split.",
    "    - conditional operations only have sense if they come after a splitting operation.",
    "    ex: x = c(\"Name: Frederick, Age: 32, Height: 1m68\", \"Francesca, 24, 1m85\")",
    "        dsb(\"' 's, <\\\\d>(:D), W, ~(' -- 'c) ? x\")",
    "",
    "# PLURALIZATION ----|",
    "  To trigger pluralization use a dollar sign right at the beginning of the operators.",
    "  There are two pluralization tags: `$` and `#`.",
    "  The tag `$` pluralizes on the *length* of the element.",
    "  The tag `#` pluralizes on the *value* of the element.",
    "  ex, length: x = c(\"Mark\", \"Francis\"); dsb(\".[$enum, is?x] here.\")",
    "  ex, value: n = 1; dsb(\".[n] file.[#s, were] found.\")",
    "",
    "  When pluralizing you can perform the following operations:",
    "    - s: adds an 's' if it is plural",
    "    - y or ies: adds an 'y' if singular and 'ies' if plural",
    "    - enum: enumerates the elements (see help for the regular enum)",
    "    - (s1;s2): adds verbatim 's1' if singular and 's2' if plural",
    "    - (s1;s2;s3): adds verbatim 's1' if zero, 's2' if singular and 's3' if plural",
    "    - (s1;;s3): adds verbatim 's1' if zero, 's3' if singular or plural",
    "    - is, or any verb: conjugates the verb appropriately",
    "    - n, N: add the number of elements as a number (n) or in letters (N)",
    "  You can chain operations, in that case a whitespace is automatically added between them.",
    "  ex: x = sample(20, 5); dsb(\"The winning number.[$s, is, enum ? sort(x)].\")",
    "",
    "  You need not provide the value over which to pluralize if it has been used previously or will be used afterwards:",
    "  ex: x = \"Mara\"; dsb(\"I like .[C ? x], .[$(she:they), is] my best friend.[$s].\")",
    "",
    "# SPECIALS ---------------|",
    "    Use '/' first to split the character with commas:",
    '    Ex: dsb("/x1, x2")             -> c("x1", "x2")',
    '        dsb("Hi .[/David, Dora]!") -> c("Hi David!", "Hi Dora!")',
    "",
    "    In quoted arguments, use backticks to evaluate them from the frame.",
    '    Ex: n = 3 ; dsb("`n`times.c!$") -> "$$$". The \'$\' is replicated n times, then collapsed.'
  )


  options("stringmagick_help" = msg)


}


####
#### Internal ####
####

string_ops_internal = function(..., is_dsb = TRUE, frame = parent.frame(),
                               sep = "", vectorize = FALSE,
                               string_as_box = TRUE, collapse = NULL,
                               help = NULL, is_root = FALSE,
                               check = FALSE, fun_name = "dsb", plural_value = NULL){


  if(!is.null(help)){
    msg = getOption("stringmagick_help")
    if(!is_dsb) {
      msg = dsb2curb(msg)
    }
    msg = format_help(msg, help)

    message(msg)

    obj = list()
    class(obj) = "help"

    return(obj)
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

          attr(frame, "dt_data") = dt_data
      }
    }
  } else {
    is_dt = "dt_data" %in% names(attributes(frame))
  }

  if(...length() == 0){
    return("")

  } else if(...length() == 1){
    x = as.character(..1)

    if(length(x) > 1){
      stop_hook("`", fun_name, "` can only be applied to character scalars. ",
                "Problem: the argument is of length ",
           length(qui), "")
    }

  } else {

    if(check){
      dots = error_sender(list(...), "In `", fun_name, "`, one element of ... could not be evaluated.")
    } else {
      dots = list(...)
    }

    if(any(lengths(dots) > 1)){
      qui = which(lengths(dots) > 1)[1]
      stop_hook("`", fun_name, "` can only be applied to character scalars. Problem: The ", n_th(qui),
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
        res[[i]] = string_ops_internal(dots[[i]], is_dsb = is_dsb, string_as_box = string_as_box, frame = frame, check = check)
      }

      return(unlist(res))
    }
  }

  if(is.na(x) || length(x) == 0){
    return(x)
  }

  BOX_OPEN = if(is_dsb) ".[" else "{"

  if(string_as_box){
    x_parsed = list(cpp_string_ops_full_string(x, is_dsb))

    if(identical(x_parsed[[1]][[2]], "")){
      x_parsed = cpp_string_ops(x, is_dsb)
    }

  } else if(!grepl(BOX_OPEN, x, fixed = TRUE)){
    return(x)

  } else {
    x_parsed = cpp_string_ops(x, is_dsb)

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
      } else {
        res = paste0(res, xi)
      }

    } else {
      #
      # operations
      #

      operators = xi[[1]]
      xi = xi[[2]]

      if(length(operators) == 0){

        if(string_as_box){
          # means verbatim => no evaluation
          if(grepl(BOX_OPEN, xi, fixed = TRUE)){
            xi = string_ops_internal(xi, is_dsb = is_dsb, frame = frame, string_as_box = FALSE, check = check)
          }

        } else {
          # we need to evaluate xi
          if(check){
            xi_call = error_sender(str2lang(xi), "The value `", xi,
                                   "` could not be parsed.")
          } else {
            xi_call = str2lang(xi)
          }

          if(is.character(xi_call)){
            # if a string literal => it's nesting
            if(grepl(BOX_OPEN, xi_call, fixed = TRUE)){
              xi = string_ops_internal(xi_call, is_dsb = is_dsb, frame = frame, string_as_box = FALSE, check = check)
            }
          } else {
            if(is_dt){
              if(check){
                xi = error_sender(eval_dt(xi_call, frame), "The value `", xi,
                                  "` could not be evaluated.")
              } else {
                xi = eval_dt(xi_call, frame)
              }
            } else if(check){
              xi = error_sender(eval(xi_call, frame), "The value `", xi,
                                "` could not be evaluated.")
            } else {
              xi = eval(xi_call, frame)
            }
          }
        }

        if(ANY_PLURAL){
          # we save
          x_values_all[[i]] = xi
        }

      } else {

        n_op = length(operators)
        verbatim = operators[n_op] %in% c("!", "/")

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
                  stop_hook("In `", fun_name, "`, the pluralization operator (`", operators[1], "`) must contain operations. ",
                          '\n', example)
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

                    stop_hook("In `", fun_name, "`, the pluralization (`", pblm, "`) did not find any value to pluralize on. ",
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

          if(operators[1] %in% c("=", "==")){
            # if else operator
            is_ifelse = TRUE
            xi_raw = xi = operators[2]
            verbatim = FALSE
          } else if(operators[n_op] == "/"){
            operators = "',[ \t\n\r]*'S"
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
          if(verbatim && grepl(BOX_OPEN, xi, fixed = TRUE)){
            xi = string_ops_internal(xi, is_dsb = is_dsb, frame = frame, string_as_box = FALSE,
                                     vectorize = concat_nested, check = check)

          } else if(!verbatim){
            # evaluation
            if(is_dt){
              if(check){
                xi = error_sender(eval_dt(xi_call, frame), "The value `", xi,
                                  "` could not be evaluated.")
              } else {
                xi = eval_dt(xi_call, frame)
              }
            } else if(check){
              xi_call = error_sender(str2lang(xi), "The value `", xi,
                                     "` could not be parsed.")
              xi = error_sender(eval(xi_call, frame), "The value `", xi,
                                "` could not be evaluated.")
            } else {
              xi = eval(str2lang(xi), frame)
            }

            if(is.function(xi)){
              stop_hook("`", fun_name, "` cannot coerce functions into strings. Problem: `",
                      trimws(x_parsed[[i]][[2]]), "` is a function.")
            }
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
          xi = sop_pluralize(operators, xi, fun_name, is_dsb, frame, check)

        } else if(is_ifelse) {

          xi_val = NULL
          if(operators[1] == "=="){
            vars = all.vars(str2lang(xi_raw))

            if(length(vars) == 0){
              # ERROR
              form = ".[== cond ; true ; false]"
              example = 'Example: x = c(5, 700); dsb("Value: .[==x > 20 ; > 20]")'

              form = bespoke_msg(form, fun_name)
              example = bespoke_msg(example, fun_name)

              stop_hook("The if-else operator `==`, of the form ", form,
                      ", requires at least one variable to be evaluated in the condition.",
                      " PROBLEM: no variable could be found.\n", example)
            }
            
            xi_call = str2lang(vars[1])
            if(is_dt){
              if(check){
                xi_val = error_sender(eval_dt(xi_call, frame), "The value `", xi,
                                  "` could not be evaluated.")
              } else {
                xi_val = eval_dt(xi_call, frame)
              }
            } else {
              xi_val = eval(xi_call, frame)
            }

          }

          xi = sop_ifelse(operators, xi, xi_val, fun_name, frame = frame,
                          check = check, is_dsb = is_dsb)
        } else {
          #
          # REGULAR OPERATORS
          #

          conditional_flag = any(str_x(operators, 1) == "~")
          
          for(j in seq_along(operators)){
            opi = operators[[j]]
            op_parsed = sop_char2operator(opi, fun_name)

            if(op_parsed$eval){
              if(check){
                argument_call = error_sender(str2lang(op_parsed$argument),
                                           "In operation `", opi, "`, the value `",
                                           op_parsed$argument, "` could not be parsed.")

                argument = error_sender(eval(argument_call, frame),
                                      "In operation `", opi, "`, the value `",
                                      op_parsed$argument, "` could not be evaluated.")
              } else {
                argument = eval(str2lang(op_parsed$argument), frame)
              }

            } else {
              argument = op_parsed$argument
            }

            if(check){
              xi = error_sender(sop_operators(xi, op_parsed$operator, op_parsed$options, argument,
                                              check = check,
                                              frame = frame, conditional_flag = conditional_flag),
                                "The operation `", opi, "` failed. Please revise your call.")
            } else {
              xi = sop_operators(xi, op_parsed$operator, op_parsed$options, argument, check = check,
                                 frame = frame, conditional_flag = conditional_flag)
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


sop_char2operator = function(x, fun_name){

  op_parsed = cpp_parse_operator(x)

  OPERATORS = getOption("smagick_operations")
  
  ok = FALSE
  op = op_parsed$operator

  if(nchar(op) == 0){
    stop_hook("In ", fun_name, ", if a quoted value is present, the operators must ",
              "be of the form 'value'op, ",
              "with 'op' an operator. Problem: In `", x, "` the operator is missing.")
  }
  
  argument = op_parsed$argument
  do_eval = op_parsed$eval
  
  if(substr(op, 1, 1) %in% c("@", "&", "<", "~")){
    ok = TRUE

  } else {

    # we partially match operators if needed
    if(!op %in% OPERATORS){
      op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
      op_parsed$operator = op
    }

    if(nchar(argument) == 0){
      # no argument provided: we need to:
      # - set default values if available
      # - send error if operator requires argument
      
      # default values
      argument = switch(op,
                        s = " ", S = ",[ \t\n]*",
                        x = "[[:alnum:]]+", X = "[[:alnum:]]+", extract = "[[:alnum:]]+",
                        c = " ", C = ", || and ",
                        times = 1, each = 1,
                        first = 1, last = 1,
                        cfirst = 1, clast = 1,
                        trim = 1, 
                        "")
      
      op_parsed$argument = argument

      if(op %in% c("R", "r", "%", "k", "K", "append", "get", "is", "which")){
        ex = c("R" = 'x = "She loves me."; dsb("\'s\\b => d\'R ? x")',
              "r" = 'x = "Amour"; dsb(".[\'ou => e\'R ? x]...")',
              "%" = 'dsb("pi is: .[%.03f ? pi]")',
              "k" = 'dsb("The first 8 letters of the longuest word are: .[8k, q ! the longuest word].")',
              "K" = 'x = 5:9; dsb("The first 2 elements of `x` are: .[2K, C ? x].")',
              "get" = 'x = row.names(mtcars) ; dsb("My fav. cars are .[\'toy\'get.ignore, \'the \'app, enum ? x].")',
              "is" = 'x = c("Bob", "Pam") ; dsb("\'am\'is ? x")',
              "which" = 'x = c("Bob", "Pam") ; dsb("\'am\'which ? x")',
              "append" = 'x = "those, words"; dsb("Let\'s emphasize .[S, \'**\'append.both, c ? x].")')

        ex = bespoke_msg(ex[op])
        stop_hook("The operator `", op,
                "` has no default value, you must provide values explicitly.\n Example: ", ex)
      }
    }
  }

  if(op %in% c("Ko", "KO")){
    # special case
    text = if(op == "Ko") "||:rest: others" else "||:REST: others"
    argument = paste0(argument, text)
    op_parsed$operator = "K"
    op_parsed$argument = argument
  }
    
  if(!ok && !op %in% OPERATORS){
    
    op = check_set_options(op, OPERATORS, case = TRUE, free = TRUE)
    op_parsed$operator = op

    if(!op %in% OPERATORS){
      example = 'x = c("king", "kong"); dsb("OMG it\'s .[\'i => o\'r, \'-\'c ? x]!")'
      example = bespoke_msg(example)

      sugg_txt = suggest_item(op, OPERATORS)

      msg = c("The operation `", x, "` is not valid. It must be something quoted followed by a valid operator.",
              "\n `", op, "` is not a valid operator.", sugg_txt,
              "\n  Valid operators (limited list, see help): ",
              "\n                   to split: s, S / to replace: r, R  / to collapse: c, C / to extract: x, X",
              "\n                   to replicate: times, each / to replicate and collapse with the empty string: times.c",
              "\n                   to upper/lower case: u, U, L / to single, double, back quote: q, Q, bq",
              "\n                   to format f, F / to apply sprintf format: %",
              "\n                   to format whitespaces: ws, tws / to append: append, Append / to insert: insert",
              "\n                   to keep: k (#characters), K (#items) / to delete: rm",
              "\n                   to remove stopwords: stop ",
              "\n------------------------------",
              "\n  type dsb('--help') for more help or dsb(help = 'word').",
              "\n  Example: .[', *'S, 'a => b'r ? var] first splits the variable var by commas then replaces every 'a' with a 'b'.")

      msg = bespoke_msg(msg)

      .stop_hook("In ", fun_name, ", the operation is not valid, see below. ", msg = msg)
    }

  }

  op_parsed
}


sop_operators = function(x, op, options, argument, check = FALSE, frame = NULL, conditional_flag = 0){

  # conditional_flag:  0 nothing
  #                    1 keep track of conditional things
  #                    2 apply conditional

  extra = attr(x, "extra")
  group_index = attr(x, "group_index")

  if(op %in% c("s", "S")){
    # S, C ####
    # Split is always applied on verbatim stuff => length 1

    is_ignore = opt_equal(options, "ignore")
    is_fixed = opt_equal(options, "fixed")

    # fixed = op == "s"
    if(is_ignore){
      # ignore case
      is_fixed = FALSE
      argument = paste0("(?i)", argument)
    }

    x_split = strsplit(x, argument, fixed = is_fixed, perl = !is_fixed)

    if(conditional_flag != 0){
      # we keep track of the group index
      x_len_all = lengths(x_split)
      # I could avoid the last line... later
      group_index = rep(seq_along(x_len_all), x_len_all)
      group_index = cpp_recreate_index(group_index)
    }

    # NOTA:
    # strsplit returns stg of length 0 for empty strings, my algo then removes these elements
    # should I drop them or keep them????
    # ex: strsplit("", "m")

    res = unlist(x_split)

    res = res[nchar(res) > 0]

  } else if(op %in% c("c", "C")){
    # collapse

    if(!is.null(group_index) && conditional_flag == 2){

      if(grepl("||", argument, fixed = TRUE)){
        argument_split = strsplit(argument, "||", fixed = TRUE)[[1]]
        sep = argument_split[[1]]
        sep_last = argument_split[[2]]
      } else {
        sep = sep_last = argument
      }

      res = cpp_paste_conditional(x, group_index, sep, sep_last)
      group_index = seq_along(group_index)

    } else {
      n_x = length(x)
      if(n_x > 1 && grepl("||", argument, fixed = TRUE)){
        # This is the "last" operator
        argument_split = strsplit(argument, "||", fixed = TRUE)[[1]]
        if(n_x == 2){
          res = paste(x, collapse = argument_split[[2]])
        } else {
          res = paste(x[-n_x], collapse = argument_split[[1]])
          res = paste0(res, argument_split[[2]], x[n_x])
        }
      } else {
        res = paste(x, collapse = argument)
      }

      if(conditional_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("r", "R", "get", "is", "which", "x", "X", "extract")){
    # R, X, get, is, which ####

    valid_options = c("word", "ignore", "fixed")
    if(op == "extract"){
      valid_options = c(valid_options, "first")
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

    if(is_word){
      items = strsplit(argument, ",[ \t\n]+")[[1]]
      if(is_fixed){
        items = paste0("\\Q", items, "\\E")
        is_fixed = FALSE
      }
      argument = paste0("\\b(", paste0(items, collapse = "|"), ")\\b")
    }

    if(is_ignore){
      # ignore case
      is_fixed = FALSE
      argument = paste0("(?i)", argument)
    }

    if(op %in% c("r", "R")){
      new = ""
      if(grepl("=>", argument, fixed = TRUE)){
        pat = "=>"

        if(grepl("_=>_", argument, fixed = TRUE)){
          pat = "_=>_"
        } else if(grepl(" => ", argument, fixed = TRUE)){
          pat = " => "
        }
        
        argument_split = strsplit(argument, pat, fixed = TRUE)[[1]]
        argument = argument_split[[1]]
        new = argument_split[[2]]
      }

      res = gsub(argument, new, x, fixed = is_fixed, perl = !is_fixed)
    } else if(op == 'x'){
      x_pat = regexpr(argument, x, fixed = is_fixed, perl = !is_fixed)

      res = substr(x, x_pat, x_pat - 1 + attr(x_pat, "match.length"))
    } else if(op == "X"){
      # extract all patterns

      x_list = regmatches(x, gregexpr(argument, x, fixed = is_fixed, perl = !is_fixed))

      if(conditional_flag != 0){
        # we keep track of the group index
        x_len_all = lengths(x_list)
        # I could avoid the last line... later
        group_index = rep(seq_along(x_len_all), x_len_all)
        group_index = cpp_recreate_index(group_index)
      }

      res = unlist(x_list)
    } else if(argument == "" && op == "which"){

      if(!is.logical(x)){
        stop_hook("The operation `which` must apply only to logical values.",
                 "\nPROBLEM: the current value to which it is applied is not logical ",
                 "(instead is is of class {enum.bq?class(x)}).")
      }

      res = which(x)

      if(conditional_flag == 1){
        group_index = group_index[res]
        group_index = cpp_recreate_index(group_index)
      } else if(conditional_flag == 2){
        stop_hook("The operation `which` cannot be applied in ",
                  "conditionnal statements (inside `~()`).",
                  "FIX: simply put this operation after the conditionnal statement.")
      }
      
    } else {
      # default is str_is
      # the user cannot use " | " nor " & "
      res = str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)

      if(op %in% c("get", "which")){

        if(conditional_flag == 1){
          group_index = group_index[res]
          group_index = cpp_recreate_index(group_index)
        } else if(conditional_flag == 2){
          stop_hook("The operation `{q?argument}{op}` cannot be applied in ",
                    "conditionnal statements (inside `~()`).",
                    "FIX: simply put this operation after the conditionnal statement.")
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
      stop_hook(bespoke_msg(msg))
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

  } else if(op == "U"){
    # U, L, titles, Q, F, % ####

    res = toupper(x)

  } else if(op == "u"){
    # First letter only, if relevant
    res = x
    substr(res, 1, 1) = toupper(substr(x, 1, 1))

  } else if(op %in% c("l", "L")){
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
      IGNORE = c("a", "the", "in", "on", "in", "at", "of", 
                 "for", "to")
      pattern = paste0("([^:!?.]) (", paste0(IGNORE, collapse = "|"), ")( |$)")
      x = gsub(pattern, "\\1 __IGNORE\\2\\3", x, perl = TRUE)
    }

    res = gsub("(^|[^\\p{L}])([\\p{L}])", "\\1\\U\\2", x, perl = TRUE)
    
    if(is_ignore){
      res = gsub("__IGNORE", "", res, fixed = TRUE)
    }

  } else if(op == "q"){
    res = paste0("'", x, "'")

  } else if(op == "Q"){
    res = paste0("\"", x, "\"")

  } else if(op == "bq"){
    res = paste0("`", x, "`")

  } else if(op %in% c("f", "F")){

    options = check_set_options(options, c("0", "zero", "right"))

    is_zero = any(options %in% c("0", "zero"))
    is_right = "right" %in% options || op == "F" || is_zero
    is_left = !is_right

    if(is_right){
      res = format(x, justify = "right", big.mark = ",")
    } else {
      res = format(x, big.mark = ",")
    }   

    if(is.numeric(x)){
      if(is_zero){
        res = gsub(" ", "0", res, fixed = TRUE)
      } else if(is_left){
        # we have to do that.... not sure the use cases need to be optimized
        res = format(cpp_normalize_ws(res))
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
    res = cpp_trimws(x)

  } else if(op == "trim"){
    # we trim

    nb = argument
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      stop_hook(bespoke_msg(msg))
    }
    nb = as.numeric(nb)

    is_both = opt_equal(options, "both")
    is_right = any(c("r", "right") %in% options) || nb < 0

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

    argument_split = argument
    pat = c("_||_", "_|_", "||", "|")
    for(p in pat){
      if(grepl(p, argument, fixed = TRUE)){
        argument_split = strsplit(argument, p, fixed = TRUE)[[1]]
        break
      }
    }
    is_included = grepl("||", p, fixed = TRUE)

    if(length(argument_split) == 1){
      nb = argument
      add = ""
      is_included = FALSE
    } else {
      nb = argument_split[[1]]
      add = argument_split[[2]]
    }

    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`, the operator `", op, "` must first contain a numeric argument, `",
                   argument, "` does not contain a numeric first.")
      stop_hook(bespoke_msg(msg))
    }

    nb = as.numeric(nb)

    if(op == "k"){
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

    if(!is.null(group_index) && conditional_flag == 2){

      res = unname(tapply(x, group_index, enum_main, options = options))
      group_index = seq_along(group_index)

    } else {
      res = enum_main(x, options = options)

      if(conditional_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("first", "last", "cfirst", "clast")){
    # first, last, cfirst, clast, rev, sort ####

    nb = argument
    
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      stop_hook(bespoke_msg(msg))
    }

    # the numeric argument can also be passed in options
    qui_num = which(grepl("^\\d+$", options))
    if(length(qui_num)){
      nb = options[qui_num[1]]
    }

    nb = as.numeric(nb)

    if(str_x(op, 1) == "c"){
      # we select the first/last characters

      if(op == "cfirst"){
        res = substr(x, 1, nb)
      } else {
        nx = nchar(x)
        res = substr(x, nx - nb + 1, nx)
      }

    } else {
      # we select the first/last elements

      if(!is.null(group_index) && conditional_flag == 2){
        qui = cpp_find_first_index(group_index, nb, op == "last")
        res = x[qui]
        group_index = group_index[qui]
      } else {
        if(op == "first"){
          res = head(x, nb)
        } else {
          res = tail(x, nb)
        }

        if(conditional_flag == 1){
          # this operation destroys the groups
          group_index = NULL
        }
      }
    }

  } else if(op == "rev"){

    if(!is.null(group_index) && conditional_flag == 2){
      qui = cpp_group_rev_index(group_index)
      res = x[qui]
    } else {
      res = rev(x)
    }

  } else if(op %in% c("sort", "dsort")){

    if(!is.null(group_index) && conditional_flag == 2){

      if(op == "dsort"){
        qui = order(-group_index, x, decreasing = TRUE)
      } else {
        qui = order(group_index, x)
      }

      res = x[qui]
    } else {
      res = sort(x, decreasing = op == "dsort")
    }

  } else if(op %in% c("append", "Append", "insert")){
    # Appends at the beginning/end of all strings

    #
    # Insert/Append ####
    #

    if("d" %in% options){
      x = character(length(x))
    }

    left = right = ""
    if("b" %in% options){
      # 'both'
      left = right = argument
    } else if("r" %in% options){
      right = argument
    } else {
      left = argument
    }

    if(nchar(argument) == 0){
      res = x

    } else if(op == "append"){

      if(length(x) == 0){
        res = x

      } else {

        # We replace the special values
        # :1:, :i:, :a:
        # we allow only one special value
        n_x = length(x)

        pat = c(":1:", ":i:", ":a:", ":I:", ":A:")

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

    # END: insert/append

  } else if(op == "unik"){
    # unik ####

    if(!is.null(group_index) && conditional_flag == 2){

      # not really fast 
      # => to be improved
      df = data.frame(g = group_index, x = x, stringsAsFactors = FALSE)
      df_unik = unique(df)

      group_index = df_unik$g
      res = df_unik$x

    } else {
      
      res = uique(x)

      if(conditional_flag == 1){
        group_index = NULL
      }

    }

  } else if(op %in% c("nth", "Nth", "ntimes", "Ntimes", "n", "N", "len", "Len")){
    #
    # nth, ntimes, nst, n, len ####
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
      if(is.numeric(x)){
        if(is_letter){
          res = n_letter(x)
        } else {
          res = format(x, big.mark = ",")
          res = cpp_trimws(res)
        }
      } else {
        x_num = suppressWarnings(as.numeric(x))
        is_x_num = which(!is.na(x_num))
        num_val = x_num[is_x_num]

        if(is_letter){
          res_num = n_letter(num_val)
        } else {
          res_num = format(num_val, big.mark = ",")
          if(is_zero){
            res_num = gsub(" ", "0", res_num, fixed = TRUE)
          } else {            
            res_num = cpp_trimws(res_num)
          }
        }

        res = x
        res[is_x_num] = res_num
      }
      
    } else if(op == "len"){

      # if conditional: lengths of all the groups
      if(!is.null(group_index) && conditional_flag == 2){
        res = tabulate(group_index)
        group_index = seq_along(res)
      } else {
        if(conditional_flag == 1){
          group_index = 1
        }

        res = length(x)
      }

      if(is_letter){
        res = n_letter(res)
      }
    }

    if("upper" %in% options){
      val = gsub("^(.)", "\\U\\1", val, perl = TRUE)
    }
  } else if(op == "swidth"){
    #
    # swidth, dtime ####
    #

    comment = ""
    if(length(options) > 0) comment = options[1]

    if(str_x(comment, -1) == "!"){
      # the bang means that we don't add a space
      comment = str_trim(comment, -1)
    } else {
      comment = paste0(comment, " ")
    }

    nb = argument
    if(!is_numeric_in_char(nb)){
      msg = paste0("In `dsb`: the operator `", op, "` must first contain a numeric argument. PROBLEM: `",
                   argument, "` is not numeric.")
      stop_hook(bespoke_msg(msg))
    }
    nb = as.numeric(nb)

    # code is slow but I don't see why would one want it to be super fast
    # (only for user-content, not performance/data-analysis concent)

    res = character(length(x))
    for(i in seq_along(x)){
      res[i] = fit_screen(x[i], width = nb, leader = comment)
    }
    
  } else if(op == "dtime"){
    res = format_difftime(x)

  } else if(op == "erase"){

    #
    # erase, rm, num ####
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

  } else if(op == "rm"){
    options = check_set_options(options, c("empty", "blank", "noalpha", "noalnum", "all", 
                                           "fixed", "ignore", "word"))
    
    res = NULL
    if(argument != ""){
      is_fixed = "fixed" %in% options
      is_word = "word" %in% options
      is_ignore = "ignore" %in% options

      qui = !str_is(x, pattern = argument, fixed = is_fixed, ignore.case = is_ignore, word = is_word)

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
    if(!is.null(group_index) && conditional_flag != 0){
      group_index = group_index[qui]
      group_index = cpp_recreate_index(group_index)
    }

  } else if(op == "num"){
    options = check_set_options(options, c("warn", "soft", "rm", "clean"))

    is_warn = "warn" %in% options
    is_soft = "soft" %in% options
    is_rm = "rm" %in% options
    is_clean = "clean" %in% options

    is_valid_num = TRUE
    if(is_warn || is_soft){
      is_valid_num = is_numeric_in_char(x)
      if(!is_valid_num && is_warn){
        msg = if(!is_soft) "" else " No conversion is performed"
        warning("In operation `num`: when trying to convert to numeric, NAs were created.", msg)
      }
    }

    if(is_valid_num || !is_soft){
      res = suppressWarnings(as.numeric(x))
      if(is_rm || is_clean){
        if(anyNA(res)){
          qui_na = which(is.na(res))
          if(is_rm){
            res = res[-qui_na]
            if(!is.null(group_index) && conditional_flag != 0){
              group_index = group_index[-qui_na]
              group_index = cpp_recreate_index(group_index)
            }
          } else { 
            # is_clean
            res[qui_na] = ""
          }
        }
      }
    } else {
      res = x
    }

  } else if(substr(op, 1, 1) %in% c("@", "&", "<")){
    #
    # Conditions: @, &, < ####
    #

    # These aren't really useful and are really complicated 
    # => maybe I should drop them????

    code = substr(op, 1, 1)
    operators_TF = cpp_string_op_if_extract(op)

    if(isFALSE(operators_TF[[1]])){
      if(code == ">"){
        cond = gsub(">\\(.+", ">", op)
      } else {
        cond = gsub("\\(.+", "", op)
      }

      example = c("@" = 'x = c("Mark", "Ben", "Laetitia"); dsb("And the long names are .[@<=3(D), Q, C ? x]!")',
                  "&" = 'x = c("mother", "in", "law"); dsb("She stings like my .[&(\'-\'c) ? x].")',
                  "<" = 'x = c("5", "five"); dsb(".[u ? x] is a .[< ^\\d+$ >(d, \'number\'a ; d, \'word\'a) ? x].")')
                  # {=is.numeric(x) ; number ; word}

      example = bespoke_msg(example[code])

      # means error
      stop_hook("Parsing error in the operators of the `", op, "` statement. ",
           "It must be of the form: ", cond, "(true ; false) with `true` and `false` chains of valid operations (although you cannot use other 'if's).",
           "\nExample:", example)
    }

    cond = operators_TF[[1]]
    true = operators_TF[[2]]
    false = operators_TF[[3]]

    # We compute the condition
    is_len = code == "#"
    len_true = FALSE
    if(code == "<"){
      regex = str_trim(cond, 1, 1)

      qui = str_is(x, pattern = regex)

      qui[is.na(qui)] = FALSE

      if(do_negate){
        qui = !qui
      }

      x_true = x[qui]
      x_false = x[!qui]

    } else if(code == "@"){

      if(cond == "@"){
        cond = "@>0"
      }

      nc = nchar(x)
      cond = gsub("@", "nc", cond, fixed = TRUE)
      qui = eval(str2lang(cond))

      qui[is.na(qui)] = FALSE

      x_true = x[qui]
      x_false = x[!qui]

    } else {

      if(cond == "&"){
        cond = "&>1"
      }

      lx = length(x)
      cond = gsub("&", "lx", cond, fixed = TRUE)
      len_true = eval(str2lang(cond))

      if(len_true){
        x_true = x
        x_false = character(0)
      } else {
        x_true = character(0)
        x_false = x
      }
    }


    # All operators that change the length of the element:
    # FORBIDDEN = c("c", "C", "times", "each", "s", "S", "i", "I", "A", "v", "V")
    FORBIDDEN = c()

    for(state in c("true", "false")){

      if(state == "true"){
        if(is_len && !len_true) next
        xi = x_true
      } else {
        if(is_len && len_true) next
        xi = x_false
      }

      index = 2 + (state == "false")
      op_all = operators_TF[[index]]

      for(i in seq_along(op_all)){
        opi = op_all[i]

        op_parsed = sop_char2operator(opi, fun_name)

        if(!is_len && op_parsed$operator %in% FORBIDDEN){
          stop_hook("In the `", op, "` statement, you cannot use operators that would change the length of the vector, hence `",
                  op_parsed$operator, "` is forbidden.")
        }

        if(op_parsed$eval){
          if(check){
            argument_call = error_sender(up = 1, str2lang(op_parsed$argument),
                                       "In operation `", opi, "`, the value `",
                                       op_parsed$argument, "` could not be parsed.")

            argument = error_sender(up = 1, eval(argument_call, frame),
                                  "In operation `", opi, "`, the value `",
                                  op_parsed$argument, "` could not be evaluated.")
          } else {
            argument = eval(str2lang(op_parsed$argument), frame)
          }

        } else {
          argument = op_parsed$argument
        }

        if(check){
          xi = error_sender(up = 1, sop_operators(xi, op_parsed$operator, op_parsed$options, argument),
                            "The operation `", opi, "` failed. Please revise your call.")
        } else {
          xi = sop_operators(xi, op_parsed$operator, op_parsed$options, argument)
        }
      }

      if(state == "true"){
        x_true = xi
      } else {
        x_false = xi
      }
    }

    if(is_len){
      res = if(len_true) x_true else x_false

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

    } else {
      # extra is not touched

      res = x

      if(length(x_true) == 0 && length(x_false) == 0){
        res = character(0)
        if(conditional_flag != 0){
          group_index = NULL
        }
      } else if(length(x_true) == 0){
        res[!qui] = x_false
        res = res[!qui]
        if(conditional_flag != 0){
          group_index = group_index[!qui]
        }
      } else if(length(x_false) == 0){
        res[qui] = x_true
        res = res[qui]
        if(conditional_flag != 0){
          group_index = group_index[qui]
        }
      } else {
        res[qui] = x_true
        res[!qui] = x_false
      }
    }

  } else if(substr(op, 1, 1) == "~"){
    # Conditional: ~ ####

    operators_cond = cpp_string_op_if_extract(op)

    if(length(operators_cond[[3]]) != 0 || isFALSE(operators_cond[[1]])){
      example = 'x = c("dear", "that\'s uncle Pegotty"); dsb("Hello .[\' \'s, ~(last1), c, u ? x].")'
      example = bespoke_msg(example)

      stop_hook("The conditional operator `~` must be of the form `~(operations)`. PROBLEM: `", op,
                "` does not lead to a valid chain of operations.\nExample: ", example)
    }

    op_all = operators_cond[[2]]

    # All operators that change the length of the element:
    FORBIDDEN = c("K", "A", "Ar", "I", "Ir", "i", "ir")

    xi = x

    for(i in seq_along(op_all)){
      opi = op_all[i]

      op_parsed = sop_char2operator(opi, fun_name)

      if(op_parsed$operator %in% FORBIDDEN){
        stop_hook("In the conditional statement `", op, "` some oprators are forbiden. ",
                  "PROBLEM: this is the case for `", op_parsed$operator, "`.")
      }

      if(op_parsed$eval){
        if(check){
          argument_call = error_sender(str2lang(op_parsed$argument),
                                     "In operation `", opi, "`, the value `",
                                     op_parsed$argument, "` could not be parsed.")

          argument = error_sender(eval(argument_call, frame),
                                "In operation `", opi, "`, the value `",
                                op_parsed$argument, "` could not be evaluated.")
        } else {
          argument = eval(str2lang(op_parsed$argument), frame)
        }

      } else {
        argument = op_parsed$argument
      }

      if(check){
        xi = error_sender(sop_operators(xi, op_parsed$operator, op_parsed$options, argument, conditional_flag = 2),
                          "The operation `", opi, "` failed. Please revise your call.")
      } else {
        xi = sop_operators(xi, op_parsed$operator, op_parsed$options, argument, conditional_flag = 2)
      }
    }

    res = xi

  } else if(op == "ascii"){
    # ascii, stop ####
    res = str_to_ascii(x, options)

  } else if(op == "stop"){

    # current limitation: does not work for quoted words
    #                     but quoted words are not stopwords usually

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


  } else {
    user_ops = getOption("smagick_user_ops")
    if(op %in% names(user_ops)){
      fun = user_ops[[op]]
      valid_options = attr(fun, "valid_options")

      if(!is.null(valid_options)){
        options = check_set_options(options, valid_options, free = any(valid_options == ""))
      }

      tmp = fun(x = x, argument = argument, options = options, group = group_index, 
                conditional_flag = conditional_flag)
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
      stop_hook(bespoke_msg(msg))
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



sop_pluralize = function(operators, xi, fun_name, is_dsb, frame, check){

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

      stop_hook("The pluralization operator `#` applies to a", extra, " number. ", reason, ".\n",
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

    if(op %in% c("s", "y", "ies")){
      zero_case = opt_equal(options, c("zero", "0"))

      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      if(op == "s"){
        if(is_really_plural){
          res[i] = "s"
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
      val = n_th(n, letters = is_letter, is_compact = is_compact)
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
          value = string_ops_internal(argument, is_dsb = is_dsb, frame = frame,
                                      string_as_box = FALSE, check = check, 
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
        example = 'Example: x = c("Charles", "Alice"); dsb(".[$Is, enum ? x] crazy? Yes .[$(he:they), are].")'
        example = bespoke_msg(example, fun_name)

        stop_hook("In pluralization, `", op, "` is expected to be a verb and verbs must always be composed of at least two letters.\n", example)
      }

      zero_case = opt_equal(options, c("zero", "0"))
      is_really_plural = IS_PLURAL || (zero_case && IS_ZERO)

      res[i] = conjugate(op, is_really_plural)
    }

  }

  res = res[!is.na(res)]
  paste(res, collapse = " ")
}

sop_ifelse = function(operators, xi, xi_val, fun_name, frame, is_dsb, check){

  if(length(xi) == 1 && is.numeric(xi) && !is.na(xi)){
    xi = xi != 0 
  }

  if(!is.logical(xi)){
    form = paste0(".[", operator[1], " cond ; true ; false]")

    example = 'Example: x = Sys.time(); dsb("Hello .[&format(x, \'%H\') < 20 ; Sun ; Moon]!")'
    example = bespoke_msg(example, fun_name)

    stop_hook("The if-else operator `", operator[1], "`, of the form ", form,
            ", accepts only logical values in the condition. ",
            "PROBLEM: the value is not logical, but of class `",
            class(xi)[1], "`.\n", example)
  }

  if(anyNA(xi)){
    form = paste0(".[", operator[1], " cond ; true ; false]")

    example = 'Example: x = Sys.time(); dsb("Hello .[=format(x, \'%H\') < 20 ; Sun ; Moon]!")'
    example = bespoke_msg(example, fun_name)

    stop_hook("The if-else operator `", operator[1], "`, of the form ", form,
              ", accepts only non-NA logical values.\n",
              "PROBLEM: the condition contains NA values.\n", example)
  }

  true = operators[3]
  false = operators[4]
  if(is.na(false)) false = ""

  if(operators[1] == "="){
    # we replace TRUE and FALSE with the strings, even empty

    true = cpp_extract_quote_from_op(true)
    false = cpp_extract_quote_from_op(false)

    res = c(false, true)[xi + 1]
  } else {
    # we don't touch if there is an empty string
    res = xi_val

    if(nchar(true) > 0){
      true = cpp_extract_quote_from_op(true)
      res[xi] = true
    }

    if(nchar(false) > 0){
      false = cpp_extract_quote_from_op(false)
      res[!xi] = false
    }

  }

  # we allow nestedness only for single values
  if(length(res) == 1){
    res = string_ops_internal(res, is_dsb = is_dsb, frame = frame,
                                string_as_box = FALSE, check = check)
  }

  res
}



setup_operations = function(){
  OPERATORS = c("s", "S", "x", "X", "extract", "c", "C", "r", "R",
                "times", "each",
                "u", "U", "l", "L", "q", "Q", "bq", "f", "F", "%",
                "erase", "rm", "append", "Append", 
                "k", "K", "last", "first",
                "cfirst", "clast", "unik", "num", "enum",
                "rev", "sort", "dsort", "ascii", "title",
                "ws", "tws", "trim", "get", "is", "which",
                "n", "N", "len", "Len", "swidth", "dtime",
                "stop", "insert", "nth", "Nth", "ntimes", "Ntimes")
  options("smagick_operations" = sort(OPERATORS))
}



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

  valid_args = dsb("/x, argument, options, group, conditionnal_flag")
  arg_pblm = setdiff(setdiff(fun_args, "..."), valid_args)
  if(length(arg_pblm) > 0){
    stop_hook("The argument `fun` must have specific argument names. Valid arguments are {enum.bq.or?valid_args}.",
              "\nPROBLEM: the argument{$s, enum.bq, are?arg_pblm} invalid.")
  }

  OPERATORS = getOption("smagick_operations")

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
