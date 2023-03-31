#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Aug  3 09:40:02 2022
# ~: string.ops tests
#----------------------------------------------#



####
#### dsb ####
####


####
#### ... regular operators ####
####

#
# w & co

x = " &Hej! Welcome!! A\t\n 6-feet wide bed awaits!\t."

txt = dsb("w ? x")
test(txt, "&Hej! Welcome!! A 6-feet wide bed awaits! .")

txt = dsb("wp ? x")
test(txt, "Hej Welcome A 6 feet wide bed awaits")

txt = dsb("wd ? x")
test(txt, "&Hej! Welcome!! A -feet wide bed awaits! .")

txt = dsb("wi ? x")
test(txt, "&Hej! Welcome!! 6-feet wide bed awaits!")

txt = dsb("wpd ? x")
test(txt, "Hej Welcome A feet wide bed awaits")

txt = dsb("wpi ? x")
test(txt, "Hej Welcome feet wide bed awaits")

txt = dsb("wdi ? x")
test(txt, "&Hej! Welcome!! -feet wide bed awaits!")

txt = dsb("wpdi ? x")
test(txt, "Hej Welcome feet wide bed awaits")

#
# tws, trim

x = c("  bonjour les gens", "ahem
   ", NA)

txt = dsb("tws ? x")
test(txt, c("bonjour les gens", "ahem", NA))

x = c("bonjour", "ahem")
txt = dsb("4 trim ? x")
test(txt, c("our", ""))

txt = dsb("-2 trim ? x")
test(txt, c("bonjo", "ah"))

txt = dsb("5 trim.r ? x")
test(txt, c("bo", ""))

# 
# ascii

test(dsb("laurent .[ascii ! bergé]"), "laurent berge")

#
# Case

x = "where is bryan? Bryan is in the KITCHEN."

txt = dsb("l ? x")
test(txt, "where is bryan? bryan is in the kitchen.")

txt = dsb("u ? x")
test(txt, "Where is bryan? Bryan is in the KITCHEN.")

txt = dsb("U ? x")
test(txt, "WHERE IS BRYAN? BRYAN IS IN THE KITCHEN.")

txt = dsb("title ? x")
test(txt, "Where Is Bryan? Bryan Is In The KITCHEN.")

x = "results from a new estimator: a new hope"
txt = dsb("title.i ? x")
test(txt, "Results from a New Estimator: A New Hope")

#
# r, R

x = c("[]", "Moon", "Sun")

txt = dsb("Stare at the .['[]'r ? x]")
test(txt, c("Stare at the ", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['[] => ...'r ? x]")
test(txt, c("Stare at the ...", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['[[:upper:]]'R ? x]")
test(txt, c("Stare at the []", "Stare at the oon", "Stare at the un"))

txt = dsb("Stare at the .['[[:upper:]] => P'R ? x]")
test(txt, c("Stare at the []", "Stare at the Poon", "Stare at the Pun"))

#
#


#
# e, E

x = c("55", "bonjour", "", " \t\n")

txt = dsb("Those are the texts: .[e ? x]")
test(txt, c("Those are the texts: 55", "Those are the texts: bonjour",
            "Those are the texts:  \t\n"))

txt = dsb("Those are the texts: .[E ? x]")
test(txt, c("Those are the texts: 55", "Those are the texts: bonjour"))


# app
x = c("Sir Eustace", "Anne", "Suzanne")
txt = dsb("Names emph: .['*'app.b, C ? x].")
test(txt, "Names emph: *Sir Eustace*, *Anne* and *Suzanne*.")

txt = dsb("Names emph: .[* app.b, C ? x].")
test(txt, "Names emph: *Sir Eustace*, *Anne* and *Suzanne*.")

txt = dsb("Here comes .[<#Eustace>('man'app.d ; 'woman'app.d), 'a 'a, C ? x].")
test(txt, "Here comes a man, a woman and a woman.")


# App

x = c("Saint Estephe", "Saint Julien")

txt = dsb("My fav. wine.[&('s are 'A ; ' is 'A), C ? x].")
test(txt, "My fav. wines are Saint Estephe and Saint Julien.")


####
#### ... pluralization ####
####

# With numbers
x = 5
txt = dsb("There .[#is, N ? x] cat.[#s] in the room.")
test(txt, "There are five cats in the room.")

x = 1
txt = dsb("There .[#is, N] cat.[#s ? x] in the room.")                  # + afterwards
test(txt, "There is one cat in the room.")

x = 7953
txt = dsb(".[#n ? x] observation.[#s, are] missing.")
test(txt, "7,953 observations are missing.")

x = 1
txt = dsb(".[#n ? x] observation.[#s, are] missing.")
test(txt, "1 observation is missing.")

# With 0 in ()

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
# plural (s, ies)

# without 0 option
n = 0
txt = cub("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There is 0 file and zero directory.")

n = 1
txt = cub("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There is 1 file and one directory.")

n = 5
txt = cub("There {#is, n ? n} file{#s} and {#N} director{#y}.")
test(txt, "There are 5 files and five directories.")

# with 0 option
n = 0
txt = cub("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There are no files and zero directories.")

n = 1
txt = cub("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There is 1 file and one directory.")

n = 5
txt = cub("There {#is.0, n.letters.no ? n} file{#s.ze} and {#N} director{#y.zero}.")
test(txt, "There are 5 files and five directories.")

#
# plural: nested evaluation

x = character(0)
txt = cub("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist.")

x = "append"
txt = cub("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist. Maybe you meant: `append`?")

x = c("append", "array")
txt = cub("The value `a` does not exist.{$(;; Maybe you meant: {$enum.or.bq}?) ? x}")
test(txt, "The value `a` does not exist. Maybe you meant: `append` or `array`?")


# With length
x = c("Charles", "Alice")
txt = dsb(".[$Is, enum ? x] crazy? Hmm... no .[$(he;they), aren't].")
test(txt, "Are Charles and Alice crazy? Hmm... no they aren't.")

txt = dsb(".[$Is, enum] crazy? Hmm... no .[$(he;they), aren't ? x[1]].")  # + afterwards
test(txt, "Is Charles crazy? Hmm... no he isn't.")

# enum, full force
txt = dsb("I like the letter.[$s, enum.1.q ! .[/u, v, w]].")
test(txt, "I like the letters 1) 'u', 2) 'v', and 3) 'w'.")

txt = dsb("I like the letter.[$s, enum.1.Q ! u].")
test(txt, 'I like the letter "u".')

txt = dsb("Choose one: .[$enum.bq.or ? 1:3]")
test(txt, "Choose one: `1`, `2` or `3`")

# multiple values
a = 577
b = c("x", "y")
txt = dsb(".[#n ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "577 observations are missing. It concerns the variables `x` and `y`.")

a = 1
txt = dsb(".[#n ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "1 observation is missing. It concerns the variables `x` and `y`.")

txt = dsb(".[#n ? a] observation.[#s, are] missing. It concerns the variable.[$s, enum.bq ? b[1]].")
test(txt, "1 observation is missing. It concerns the variable `x`.")

# combining the two

x = 1:5
txt = cub("{$n.u ? x} observation{$s}. He arrived {#nth.letter ? 3} and scored {#ntimes.le ? 1}.")
text(txt, "5 observations. He arrived third.")

####
#### n, len ####
####

x = c(45546, "bonjour")
txt = cub("A = {n ? x} ; B = {n.l ? 55}")
test(txt, c("A = 45,546 ; B = fifty-five", "A = bonjour ; B = fifty-five"))

txt = cub("x is of length {len ? x}, or {Len ? x}.")
test(txt, "x is of length 2, or two.")

# conditionnaly
x = c("bonjour les gens", "la pluie", "est drue, je rentre")
txt = cub("Number of words: {' 'S, ~(len), C ? x}.")
test(txt, "Number of words: 3, 2 and 4.")

num = str_op(x, "' 'S, ~(len), num")
test(num, c(3, 2, 4))

#
# swidth
#

x = "Rome, l'unique objet de mon ressentiment, Rome a qui vient ton bras d'immoler mon amant"
txt = cub("Voici le texte a apprendre:\n{40 swidth.> ? x}.")
test(txt, c("Voici le texte a apprendre:\n> Rome, l'unique objet de mon\n> ressentiment, Rome a qui vient ton\n> bras d'immoler mon amant."))

#
# difftime
#

x = 3654
txt = cub("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 1 hour 00 min.")

x = structure(1680294984.14505, class = c("POSIXct", "POSIXt")) - structure(1680292481.19258, class = c("POSIXct", "POSIXt"))
txt = cub("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 41 min 42 sec.")

####
#### ... if-else ####
####

x = c(15, 550)
txt = dsb("The value is .[& x > 50 ; > 50 ; <= 50]")
test(txt, c("The value is <= 50", "The value is > 50"))

x = c(15, 550)
txt = dsb("The value is .[&& x > 50 ; > 50]")
test(txt, c("The value is 15", "The value is > 50"))


####
#### ... conditions ####
####

#
# @: number of characters

x = dsb("/le, bal, vin, ides, amens")

txt = dsb("@(u : D) ! .['[vin]'R ? x]")
test(txt, c("Le", "Bal", "Des", "Ames"))

txt = dsb("@ >= 4(u:1k) ? x")
test(txt, c("l", "b", "v", "Ides", "Amens"))

txt = dsb("@ < 4(:U) ? x")
test(txt, c("le", "bal", "vin", "IDES", "AMENS"))

txt = dsb("@ == 3(U) ? x")
test(txt, c("le", "BAL", "VIN", "ides", "amens"))


#
# #: number of elements

x = dsb("/julien, rebecca, sarah")

txt = dsb("Here .[#('are 'A:'is 'A), C ? x]")
test(txt, "Here are julien, rebecca and sarah")

txt = dsb("Here .[#('are 'A:'is 'A), C ? x[1]]")
test(txt, "Here is julien")


#
# <: regex




####
#### str_is ####
####


x = dsb("/one, two, one... two, microphone, check")

val = str_is(x, "^...$")
test(val, c(TRUE, TRUE, FALSE, FALSE, FALSE))

val = str_is(x, "#...")
test(val, c(FALSE, FALSE, TRUE, FALSE, FALSE))

val = str_is(x, "!#...")
test(val, c(TRUE, TRUE, FALSE, TRUE, TRUE))

val = str_is(x, c("#one", "#c"))
test(val, c(FALSE, FALSE, FALSE, TRUE, FALSE))

val = str_is(x, c("#one", "|#c"))
test(val, c(TRUE, FALSE, TRUE, TRUE, TRUE))


# escaping

x = dsb("/hey!, #twitter, !##!")

val = str_is(x, "\\#")
test(val, c(FALSE, TRUE, TRUE))

val = str_is(x, "!\\#")
test(val, c(TRUE, FALSE, FALSE))

val = str_is(x, "\\!")
test(val, c(TRUE, FALSE, TRUE))

val = str_is(x, "\\!#")
test(val, c(FALSE, FALSE, TRUE))


####
#### str_get ####
####


x = rownames(mtcars)

val = str_get(x, "Mazda")
test(val, c("Mazda RX4", "Mazda RX4 Wag"))

val = str_get(x, c("!\\d", "u"))
test(val, c("Hornet Sportabout", "Lotus Europa"))

val = str_get(x, c("Mazda", "Volvo"), seq = TRUE)
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Volvo 142E"))

val = str_get(x, c("Volvo", "Mazda"), seq = TRUE)
test(val, c("Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))


####
#### selvars ####
####


x = rownames(mtcars)

val = selvars(x, "^Maz")
test(val, c("Mazda RX4", "Mazda RX4 Wag")

val = selvars(x, "^Mer, !#450")
test(val, c("Merc 240D", "Merc 230", "Merc 280", "Merc 280C")

val = selvars(x, "^Maz, &!@\\d")
test(val, c("Mazda RX4", "Mazda RX4 Wag", "Hornet Sportabout", "Valiant",
            "Cadillac Fleetwood", "Lincoln Continental", "Chrysler Imperial",
            "Honda Civic", "Toyota Corolla", "Toyota Corona", "Dodge Challenger",
            "AMC Javelin", "Pontiac Firebird", "Lotus Europa", "Ford Pantera L",
            "Ferrari Dino", "Maserati Bora"))

val = selvars(x, "@\\d, !^Merc || ^Fiat, !^Maz")
test(val, c("Fiat 128", "Fiat X1-9", "Datsun 710", "Hornet 4 Drive", "Duster 360",
            "Camaro Z28", "Porsche 914-2", "Volvo 142E", "Mazda RX4", "Mazda RX4 Wag"))

####
#### str_split2df ####
####


x = c("Nor rain, wind", "When my information")
id = c("ws", "jmk")

val = str_split2df(x, "[[:punct:] ]+")
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))

val = str_split2df(x, "[[:punct:] ]+", id = id)
test(val$x, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = str_split2df(text ~ my_id, base, "[[:punct:] ]+")
test(val$text, c("Nor", "rain", "wind", "When", "my", "information"))
test(val$obs, c(1L, 1L, 1L, 2L, 2L, 2L))
test(val$my_id, c("ws", "ws", "ws", "jmk", "jmk", "jmk"))

base = data.frame(text = x, my_id = id)
val = str_split2df(text ~ my_id, base, "[[:punct:] ]+", add.pos = TRUE)
test(val$pos, c(1L, 2L, 3L, 1L, 2L, 3L))

####
#### str_clean ####
####

x = c("hello world  ", "it's 5 am....")

val = str_clean(x, c("o", "#."))
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "o|\\.")
test(val, c("hell wrld  ", "it's 5 am"))

val = str_clean(x, "@w")
test(val, c("hello world", "it's 5 am...."))

val = str_clean(x, c("o(?= )", "@wpi"))
test(val, c("hell world", "it am"))



