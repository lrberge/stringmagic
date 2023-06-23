


#' String interpolation with operation chaining
#'
#' This is firstly a string interpolation tool. On top of this it can apply, and chain, over 50
#' basic string operations to the interpolated variables. Advanced support for pluralization.
#'
#' @param ... Character scalars that will be collapsed with the argument `sep`. Note that 
#' named arguments are used for substitution.
#' 
#' To interpolate, you can 
#' use `"{x}"` within each character string to insert the value of `x` in the string. 
#' You can add string operations in each `"{}"` instance with the syntax `"'arg'op ? x"` 
#' (resp. `"'arg'op ! x"`) to apply the operation `'op'` with the argument `'arg'` to `x`
#'  (resp. the verbatim of `x`). Otherwise, what to say? Ah, nesting is enabled, and since 
#' there's over 50 operators, it's a bit complicated to sort you out in this small space. 
#' 
#' But type `smagick("--help")` to prompt a compact help, or use the argument `.help = "keyword"`
#' (or `.help = TRUE`) to obtain a selective help from the main documentation.
#' @param .envir An environment used to evaluate the variables in `"{}"`. By default the variables are
#' evaluated using the environment from where the function is called or using the named 
#' arguments passed to the function.
#' @param .sep Character scalar, default is the empty string `""`. It is used to collapse all
#'  the elements in `...` before applying any operation.
#' @param .vectorize Logical scalar, default is `FALSE`. If `TRUE`, Further, elements in `...` are 
#' NOT collapsed together, but instead vectorised.
#' @param .help Character scalar or `TRUE`, default is `NULL`. This argument
#' is used to generate a dynamic help on the console. If `TRUE`, the user can select which
#' topic to read from the main documentation, with the possibility to search for keywords and
#' navigate the help pages. If a character scalar, then a regex search is perfomed on the main
#' documentation and any section containining a match is displayed. The user can easily
#' navigate across matches.
#' @param .data.table Logical, default is `TRUE`. If you use `smagick` in a `data.table` call
#' and interpolate variables within the `data.table`, you want this argument to be `TRUE`.
#' It only incurs a small overhead.
#' @param .collapse Character scalar, default is `NULL`. If provided, the character vector
#' that should be returned is collapsed with the value of this argument. This leads
#' to return a string of length 1.
#' @param .check Logical scalar, default is `TRUE`. Whether to enable error-handling. 
#' Without errorhandling you can save something of the order of 40us. Useful only
#' in long loops.
#' @param .delim Character vector of length 1 or 2. Default is `c("{", "}")`. Defines 
#' the opening and the closing delimiters for interpolation. 
#' 
#' If of length 1, it must be of the form: 1) the opening delimiter, 
#' 2) a single space, 3) the closing delimiter. Ex: `".[ ]"` is equivalent to `c(".[", "]")`.
#' The default value is equivalent to `"{ }"`.
#' @param .default Logical scalar, default is `TRUE`. Whether to use the global defaults 
#' set with the function [setSmagick()]. If `FALSE`, then the default value of the arguments
#' is guaranteed to be ones of the function definition.
#'
#' @details 
#' There are over 50 basic string operations, it supports pluralization, string operations can be 
#' nested, operations can be applied group-wise or conditionally and
#' operators have sensible defaults. 
#'  (it may be the most powerful feature)
#' You can also declare your own operations with [smagick_register()]. They will be 
#' seamlessly integrated to `smagick`.
#'
#' Access a compact help on the console with `smagick("--help")` or use the argument `.help` to which
#' you can pass keywords or regular expressions and fecth select pieces from the main documentation.
#' 
#' This function is compatible with calls within a `data.table` without named arguments. 
#' Note that it shouldn't work (try [glue](https://glue.tidyverse.org/index.html) for instance)!
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
#' and uppercases the first letter. Example: `smagick("They lost {C ! {ntimes ? c(1, 12)} against {S!Real, Barcelona}}.")`
#' leads to "They lost once against Real and 12 times against Barcelona.".
#' + Ntimes: same as `ntimes` but automatically adds the option "letter".
#' Example: `x = 5; smagick("This paper was rejected {Ntimes ? x}...")` leads to
#' "This paper was rejected five times...".
#' + firstchar, lastchar: to select the first/last characters of each element. 
#'   Ex: `smagick("{19 firstchar, 9 lastchar ! This is a very long sentence}")` leads to "very long".
#' Negative numbers remove the first/last characters.
#' + k: to keep only the first n characters (like `firstchar` but with more options). The
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
#' Ex.4: `x = smagick("x{1:10}")`; `smagick("y = {vif(.N>4 ; {S!{x[1]}, ..., {last?x}}), ' + 'c ? x}")`
#' leads to "y = x1 + ... + x10".
#' Let's break it down. If the length of the vector is greater than 4 (here it's 10), then
#' the full string is replaced with `"{S!{x[1]}, ..., {last?x}}"`. Interpolation applies to
#' such string. Hence the split operation `S` breaks the string w.r.t.
#' the commas (default behavior), leading to the vector `c("{x[1]}", "...", "{last?x}")`. Since the 
#' string contains curly brackets, interpolation is applied again. This leads to 
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
#' Alternatively, use the argument `.delim` to set custom delimiters.
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
#' @inheritSection str_is Generic regular expression flags
#'
#' @return
#' It returns a character vector whose length depends on the elements and operations in the interpolations.
#' 
#' @seealso 
#' 
#' To set new operators, see the function [smagick_register()].
#' 
#' To modify the default values of smagick, use the function [setSmagick()].
#' 
#' If you want to apply a chain of operations on a single vector, see [str_ops()] which 
#' may be more appropriate.
#' 
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
#' str_vec("J. Mills, David, Agnes, Dr Strong")
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
#' acad = str_vec("you like admin, you enjoy working on weekends, you really love emails")
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
#' words = str_vec("short, constitutional")
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
#' bx = str_vec("Pessac Leognan, Saint Emilion, Marguaux, Saint Julien, Pauillac")
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
#' x = str_vec("1, 12, 123, 1234, 123456, 1234567")
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
#'     "For money? No... for {upper,''s, c ? reason}!", .sep = "\n")
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
"smagick"
