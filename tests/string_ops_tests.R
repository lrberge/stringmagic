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


# a, ar, al, da
x = c("Sir Eustace", "Anne", "Suzanne")
txt = dsb("Names emph: .['*|*'a, C ? x].")
test(txt, "Names emph: *Sir Eustace*, *Anne* and *Suzanne*.")

txt = dsb("Names strong: .['**'al, '**'ar, C ? x].")
test(txt, "Names strong: **Sir Eustace**, **Anne** and **Suzanne**.")

txt = dsb("Here comes .[<#Eustace>('man'da : 'woman'da), 'a 'al, C ? x].")
test(txt, "Here comes a man, a woman and a woman.")


# A, Ar, Al

x = c("Saint Estephe", "Saint Julien")

txt = dsb("My fav. wine.[#('s are 'A : ' is 'Al), C ? x].")
test(txt, "My fav. wines are Saint Estephe and Saint Julien.")


####
#### ... pluralization ####
####

# With numbers
x = 5
txt = dsb("There .[$$is, N ? x] cat.[$$s] in the room.")
test(txt, "There are five cats in the room.")

x = 1
txt = dsb("There .[$$is, N] cat.[$$s ? x] in the room.")                  # + afterwards
test(txt, "There is one cat in the room.")

x = 7953
txt = dsb(".[$$n ? x] observation.[$$s, are] missing.")
test(txt, "7,953 observations are missing.")

x = 1
txt = dsb(".[$$n ? x] observation.[$$s, are] missing.")
test(txt, "1 observation is missing.")

# With length
x = c("Charles", "Alice")
txt = dsb(".[$Is, enum ? x] crazy? Hmm... no .[$(he:they), aren't].")
test(txt, "Are Charles and Alice crazy? Hmm... no they aren't.")

txt = dsb(".[$Is, enum] crazy? Hmm... no .[$(he:they), aren't ? x[1]].")  # + afterwards
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
txt = dsb(".[$$n ? a] observation.[$$s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "577 observations are missing. It concerns the variables `x` and `y`.")

a = 1
txt = dsb(".[$$n ? a] observation.[$$s, are] missing. It concerns the variable.[$s, enum.bq ? b].")
test(txt, "1 observation is missing. It concerns the variables `x` and `y`.")

txt = dsb(".[$$n ? a] observation.[$$s, are] missing. It concerns the variable.[$s, enum.bq ? b[1]].")
test(txt, "1 observation is missing. It concerns the variable `x`.")



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



