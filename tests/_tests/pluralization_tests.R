#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-05-17
# ~: pluralization tests
#------------------------------------------------------------------------------#


chunk("plural")

dsb = function(...) smagick(..., .delim = ".[ ]")

#
# numbers ####
#

x = 5
txt = dsb("There .[#is, N ? x] cat.[#s] in the room.")
test(txt, "There are five cats in the room.")

x = 1
txt = dsb("There .[#is, N] cat.[#s ? x] in the room.")
test(txt, "There is one cat in the room.")

x = 7953
txt = dsb(".[#n ? x] observation.[#s, are] missing.")
test(txt, "7,953 observations are missing.")

x = 1
txt = dsb(".[#n ? x] observation.[#s, are] missing.")
test(txt, "1 observation is missing.")

#
# With 0 in () ####
#

x = 0
txt = dsb("There .[#(are no;;.[#is, N]) ? x] director.[#(ies;y;ies)].")
test(txt, "There are no directories.")

x = 1
txt = dsb("There .[#(are no;;.[#is, N]) ? x] director.[#(ies;y;ies)].")
test(txt, "There is one directory.")

x = 5
txt = dsb("There .[#(are no;;.[#is, N]) ? x] director.[#(ies;y;ies)].")
test(txt, "There are five directories.")

#
# plural (s, ies) ####
#

# without 0 option
n = 0
txt = smagick("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There is 0 file and zero directory.")

n = 1
txt = smagick("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There is 1 file and one directory.")

n = 5
txt = smagick("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There are 5 files and five directories.")

# with 0 option
n = 0
txt = smagick("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There are no files and zero directories.")

n = 1
txt = smagick("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There is 1 file and one directory.")

n = 5
txt = smagick("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There are 5 files and five directories.")

#
# plural: nested evaluation ####
#

x = character(0)
txt = smagick("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist.")

x = "append"
txt = smagick("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist. Maybe you meant: `append`?")

x = c("append", "array")
txt = smagick("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist. Maybe you meant: `append` or `array`?")


# With length
x = c("Charles", "Alice")
txt = dsb(".[$Is, enum ? x] crazy? Hmm... no .[$(he;they), aren't].")
test(txt, "Are Charles and Alice crazy? Hmm... no they aren't.")

txt = dsb(".[$Is, enum] crazy? Hmm... no .[$(he;they), aren't ? x[1]].") 
test(txt, "Is Charles crazy? Hmm... no he isn't.")

#
# enum, full force ####
# 

txt = dsb("I like the letter.[$s, enum.1.q ! .[S!u, v, w]].")
test(txt, "I like the letters 1) 'u', 2) 'v', and 3) 'w'.")

txt = dsb("I like the letter.[$s, enum.1.Q ! u].")
test(txt, 'I like the letter "u".')

txt = dsb("Choose one: .[$enum.bq.or ? 1:3]")
test(txt, "Choose one: `1`, `2` or `3`")

#
# multiple interpolations
#

a = 577
b = c("x", "y")
txt = dsb(".[#n ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "577 observations are missing. It concerns the variables `x` and `y`.")

a = 1
txt = dsb(".[#n ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "1 observation is missing. It concerns the variables `x` and `y`.")

txt = dsb(".[#n.u ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b[1]].")
test(txt, "One observation is missing. It concerns the variable `x`.")

# combining the two
x = 1:5
txt = smagick("{$n.u ? x} observation{$s}. He arrived {#nth.letter ? 3} and scored {#ntimes.le ? 1}.")
test(txt, "Five observations. He arrived third and scored once.")

#
# la conjugaison ####
#

pple = c("Francis", "Henry")
txt = smagick("{$enum, is, (a;) ? pple} tall guy{$s}.",
          "\n{$(He;They), like} to eat donuts.",
          "\nWhen happy, at the pub {$(he;they), goes}!",
          "\n{$Don't, (he;they)} have wit, {$(he;they)} who {$try}?")
test(txt, "Francis and Henry are tall guys.\nThey like to eat donuts.\nWhen happy, at the pub they go!\nDon't they have wit, they who try?")

pple = "Francis"
txt = smagick("{$enum, is, (a;) ? pple} tall guy{$s}.",
          "\n{$(He;They), like} to eat donuts.",
          "\nWhen happy, at the pub {$(he;they), goes}!",
          "\n{$Don't, (he;they)} have wit, {$(he;they)} who {$try}?")
test(txt, "Francis is a tall guy.\nHe likes to eat donuts.\nWhen happy, at the pub he goes!\nDoesn't he have wit, he who tries?")





