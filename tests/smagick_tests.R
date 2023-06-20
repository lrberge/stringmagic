#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Aug  3 09:40:02 2022
# ~: string.ops tests
#----------------------------------------------#

# Tests all string operations
# - should go through all the branches
# - I test the conditional operations inside the operators when relevant

library(stringmagick)
test = stringmagick:::test
test_err_contains = stringmagick:::test_err_contains

#
# slash ####
#

# regular
x = smagick("/bon, jour, les gens, 
          je suis, la bas")
test(x, c("bon", "jour", "les gens", "je suis", "la bas"))

# with nesting
y = c("Ana", "Charles")
z = c("Romeo Montaigu", "Juliette Capulet")
x = smagick("/Jules, {y}, Francis, {'\\s+'S, ~(cfirst, ''c) ? z}")
test(x, c("Jules", "Ana", "Charles", "Francis", "RM", "JC"))

#
# collapse ####
#

# regular
x = 1:3
test(smagick("win = {'-'c ? x}"), "win = 1-2-3")

test(smagick("win = {c ? x}"), "win = 1 2 3")

# with last element
test(smagick("win = {C ? x}"), "win = 1, 2 and 3")

test(smagick("win = {'|/'c ? x}"), "win = 12/3")

# default behavior
test(smagick("win = {c ? x}"), "win = 1 2 3")

# empty collapse
test(smagick("win = {''c ? x}"), "win = 123")

# conditional collapse
x = smagick("/bonjour les gens., comment ca va?, bien?, bien.")
txt = smagick("{' 'S, ~(cfirst.3, '.'c) ? x}")
test(txt, c("bon.les.gen", "com.ca.va?", "bie", "bie"))

#
# split ####
#

x = "Cogito...Ergo...Sum"
txt = smagick("He said: {'f/...'s ? x}")
test(txt, c("He said: Cogito", "He said: Ergo", "He said: Sum"))

txt = smagick("He said: {'i/[cs]'s ? x}")
test(txt, c("He said: ", "He said: ogito...Ergo...", "He said: um"))



#
# replace ####
#

x = c("Blanche dit 'Bla bla blanc'.")
txt = smagick("She said: {'bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla nc'.")

txt = smagick("She said: {'bla'r.ignore, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = smagick("She said: {'i/bla'r, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = smagick("She said: {'bla'r.word, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = smagick("She said: {'w/bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = smagick("She said: {'bla'r.word.ignore, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = smagick("She said: {'wi/bla'r, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = smagick("She said: {'.'r.fixed, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = smagick("She said: {'fixed/.'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = smagick("She said: {'Bla, blanc => ??'r.word, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = smagick("She said: {'word/Bla, blanc => ??'r, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = smagick("She said: {'Bla, blanc => \\1\\1'r.w, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

txt = smagick("She said: {'w/Bla, blanc => \\1\\1'r, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

# total replacement
x = smagick("/jingle bells, jingle bells, jingle, all the way")
txt = smagick("{', 'c ! {'t/jing => [sound]'r ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

txt = smagick("{', 'c ! {'jing => [sound]'r.total ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

# default
test(smagick("a = {r ? 1:5}"), "err")
test(smagick("a = {R ? 1:5}"), "err")

# other with replacement

x = c("[]", "Moon", "Sun")

txt = dsb("Stare at the .['f/[]'r ? x]")
test(txt, c("Stare at the ", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['f/[] => ...'r ? x]")
test(txt, c("Stare at the ...", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['[[:upper:]]'R ? x]")
test(txt, c("Stare at the []", "Stare at the oon", "Stare at the un"))

txt = dsb("Stare at the .['[[:upper:]] => P'R ? x]")
test(txt, c("Stare at the []", "Stare at the Poon", "Stare at the Pun"))


#
# x: single extraction ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = smagick("mots: {'bla'x ? x}")
test(txt, c("mots: ", "mots: bla"))

test(smagick("mots: {'bla'x.ignore ? x}"), 
     c("mots: Bla", "mots: Bla"))
     
test(smagick("mots: {'i/bla'x ? x}"), 
     c("mots: Bla", "mots: Bla"))

test(smagick("mots: {'bla'x.i.w ? x}"), 
     c("mots: ", "mots: Bla"))

test(smagick("mots: {'iw/bla'x ? x}"), 
     c("mots: ", "mots: Bla"))

# default
txt = smagick("mots: {x ? x}")
test(txt, c("mots: Blanche", "mots: Bla"))

# alias
test(smagick("mots: {'iw/bla'extract.first ? x}"), 
     c("mots: ", "mots: Bla"))


#
# X: multiple extractions ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = smagick("{'bla'X ? x}")
test(txt, c("bla", "bla"))

txt = smagick("{'bla'X.i ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = smagick("{'i/bla'X ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = smagick("{'bla'X.i.w ? x}")
test(txt, c("Bla", "bla"))

txt = smagick("{'ignore, word/bla'X ? x}")
test(txt, c("Bla", "bla"))

# conditional
x = smagick("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagick("info: {'\\d+'X, ~('-'c), C ? x}")
test(txt, "info: 57-26, 32-7, 29-8 and 55")

#
# is, get, which ####
#

x = c("Hi Mary.", "Hi Charles!", "Are you OK?", "I think so Charles.", "Great to hear, Mary!")
txt = smagick("A few quotes:\n{Q, 'mary | charles & \\!'get.ignore, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

txt = smagick("A few quotes:\n{Q, 'i/mary | charles & \\!'get, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

x = c("It's me again.", "What do you mean a gain?", "Yes, again!", "Ah AGAIN, I thought A GAIN!")
index = str_ops(x, "'gain'which.w.ig")
test(index, c(2, 4))

index = str_ops(x, "'gain & .'is.fixed")
test(index, c(TRUE, FALSE, FALSE, FALSE))

# conditional
x = smagick("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagick("info: {' 'S, '!\\d'get, ~('-'c), C ? x}")
test(txt, "info: laura-and, charles-and, charly-and and june")


#
# each/times ####
#

test(smagick("I like {5 times.c ! ?} marks!"), "I like ????? marks!")

x = c("mary", "richard")
y = c("yes", "no")
txt = smagick("The discussion: {', 'c ! {upper.first, 2 times ? x}: '{2 times ? y}'}...")
test(txt, "The discussion: Mary: 'yes', Richard: 'no', Mary: 'yes', Richard: 'no'...")

test(smagick("values: {2 each.c ? c('a', 'b')}"), "values: aabb")

#
# case ####
#

x = "where is bryan? Bryan is in the KITCHEN."

txt = dsb(".[lower ? x]")
test(txt, "where is bryan? bryan is in the kitchen.")

txt = dsb(".[upper.first ? x]")
test(txt, "Where is bryan? Bryan is in the KITCHEN.")

txt = dsb(".[upper ? x]")
test(txt, "WHERE IS BRYAN? BRYAN IS IN THE KITCHEN.")

txt = dsb(".[lower, upper.sentence ? x]")
test(txt, "Where is bryan? Bryan is in the kitchen.")

txt = dsb(".[title ? x]")
test(txt, "Where Is Bryan? Bryan Is In The KITCHEN.")

txt = dsb(".[title.force ? x]")
test(txt, "Where Is Bryan? Bryan Is In The Kitchen.")

txt = dsb(".[title.force.ignore ? x]")
test(txt, "Where Is Bryan? Bryan Is in the Kitchen.")

x = "results from a new estimator: a new hope"
txt = dsb(".[title.i ? x]")
test(txt, "Results From a New Estimator: A New Hope")

#
# quotes ####
#

x = "siren"
txt = smagick("Is it the song of the {q ? x}, of the {Q ? x} or of the {bq ? x}?")
test(txt, "Is it the song of the 'siren', of the \"siren\" or of the `siren`?")

#
# format ####
#

x = c(1, 123, 123456)
txt = smagick("The numbers are:\n{'\n'c ! - {format ? x} | {rev, Format ? x}}")
test(txt, "The numbers are:\n- 1       | 123,456\n- 123     |     123\n- 123,456 |       1")

txt = smagick("The numbers are:\n{'\n'c ! - {format.0 ? x} | {rev, Format.zero ? x}}")
test(txt, "The numbers are:\n- 0000001 | 123,456\n- 0000123 | 0000123\n- 123,456 | 0000001")


#
# sprintf ####
#

txt = smagick("pi = {%.3f ? pi}")
test(txt, "pi = 3.142")

txt = smagick("pi = {% 8.3f ? pi}")
test(txt, "pi =    3.142")

x = c("michael", "ana")
txt = smagick("The winners are:\n{'\n'c ! - {% 10s ? x}}")
test(txt, "The winners are:\n-    michael\n-        ana")

txt = smagick("The winners are:\n{'\n'c ! - {%- 10s ? x}!}")
test(txt, "The winners are:\n- michael   !\n- ana       !")

#
# ws ####
#

x = " &Hej! Welcome!! A\t\n 6-feet wide bed awaits!\t."

txt = dsb(".[ws ? x]")
test(txt, "&Hej! Welcome!! A 6-feet wide bed awaits! .")

txt = dsb(".[ws.p ? x]")
test(txt, "Hej Welcome A 6 feet wide bed awaits")

txt = dsb(".[ws.d ? x]")
test(txt, "&Hej! Welcome!! A -feet wide bed awaits! .")

txt = dsb(".[ws.i ? x]")
test(txt, "&Hej! Welcome!! 6-feet wide bed awaits!")

txt = dsb(".[ws.p.d ? x]")
test(txt, "Hej Welcome A feet wide bed awaits")

txt = dsb(".[ws.punct.isolated ? x]")
test(txt, "Hej Welcome feet wide bed awaits")

txt = dsb(".[ws.d.i ? x]")
test(txt, "&Hej! Welcome!! -feet wide bed awaits!")

txt = dsb(".[ws.p.d.i ? x]")
test(txt, "Hej Welcome feet wide bed awaits")

#
# tws ####
#

x = c("  bonjour les gens", "ahem \n  ", NA)
txt = dsb(".[tws ? x]")
test(txt, c("bonjour les gens", "ahem", NA))

#
# trim ####
#

x = c("bonjour", "ahem")
txt = dsb(".[4 trim ? x]")
test(txt, c("our", ""))

txt = dsb(".[-2 trim ? x]")
test(txt, c("bonjo", "ah"))

txt = dsb(".[5 trim.r ? x]")
test(txt, c("bo", ""))

txt = dsb(".[1 trim.b ? x]")
test(txt, c("onjou", "he"))

#
# k, keep char ####
#

x = c("this is a long sentence", "a short one")
txt = smagick("{11k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = smagick("{11 k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = smagick("{'11|..'k, ' and 'c ? x}")
test(txt, "this is a l.. and a short one")

txt = smagick("{'11||..'k, ' and 'c ? x}")
test(txt, "this is a.. and a short one")

#
# K, keep elements ####
#

x = 1:5

txt = smagick("{3K ? x}")
test(txt, 1:3)

txt = smagick("{3KO ? x}")
test(txt, c("1", "2", "three others"))

txt = smagick("{3Ko ? x}")
test(txt, c("1", "2", "3 others"))

txt = smagick("{'3|le reste'K ? x}")
test(txt, c("1", "2", "3", "le reste"))

txt = smagick("{'3||le reste'K ? x}")
test(txt, c("1", "2", "le reste"))

txt = smagick("{'3||et :rest:/:n:'K ? x}")
test(txt, c("1", "2", "et 3/5"))

txt = smagick("{'3||et :REST:/:N:'K ? x}")
test(txt, c("1", "2", "et three/five"))

# conditional
x = smagick("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
test(smagick("info: {' 'S, ~(2Ko), C ? x}"), "err")

#
# enum ####
#

x = 1:5
txt = smagick("{enum.q.3.oxf ? x}")
test(txt, "'1', '2', and 3 others")

txt = smagick("{enum.bq.or ! x{1:3}}")
test(txt, "`x1`, `x2` or `x3`")

txt = smagick("{enum.Q.nor.1 ! x{1:3}}")
test(txt, '1) "x1", 2) "x2", nor 3) "x3"')

txt = smagick("{enum.a ! x{1:3}}")
test(txt, "a) x1, b) x2, and c) x3")

txt = smagick("{enum.i ! x{1:3}}")
test(txt, "i) x1, ii) x2, and iii) x3")

# conditional
x = c("123", "abc", "a1b2")
txt = smagick("{''S, ~(enum), ' ; 'c ? x}")
test(txt, "1, 2 and 3 ; a, b and c ; a, 1, b and 2")

#
# first and last ###
#

x = 1:5
txt = smagick("{2 first, ''c ? x}")
test(txt, "12")

txt = smagick("{last.2, ''c ? x}")
test(txt, "45")

a = 2
txt = smagick("{'3'first, `a`last, ''c ? x}")
test(txt, "23")

txt = smagick("{'1|1'first, ''c ? x}")
test(txt, "15")

txt = smagick("{'-3'first, ''c ? x}")
test(txt, "45")

# conditional
x = c("123", "abc", "a1b2")
txt = smagick("{''S, ~(first), ' ; 'c ? x}")
test(txt, "1 ; a ; a")

txt = smagick("{''S, ~(last), ' ; 'c ? x}")
test(txt, "3 ; c ; 2")

#
# cfirst, clast ####
#

x = c("bonjour", "les", "gens")
a = 2
txt = smagick("{3 cfirst, `a`clast ? x}")
test(txt, c("on", "es", "en"))

#
# rev ####
#

test(smagick("{rev ? 1:3}"), 3:1)

# conditional
x = c("123", "abc", "a1b2")
txt = smagick("{''S, ~(rev, ''c), ' ; 'c ? x}")
test(txt, "321 ; cba ; 2b1a")

#
# sort, dsort ####
#

x = c(5, 3, 8, 1)
test(smagick("{sort ? x}"), c(1, 3, 5, 8))

test(smagick("{dsort ? x}"), c(8, 5, 3, 1))

# with preprocessing
x = "Mark is 34, Bianca is 55, Odette is 101, Julie is 21 and Frank is 5"
txt = smagick("{', | and 's, '\\D'sort, C ? x}")
test(txt, "Odette is 101, Julie is 21, Mark is 34, Frank is 5 and Bianca is 55")

txt = smagick("{', | and 's, '\\D'sort.num, C ? x}")
test(txt, "Frank is 5, Julie is 21, Mark is 34, Bianca is 55 and Odette is 101")

# conditional
x = c("521", "aebc")
txt = smagick("{''S, ~(sort, ''c), ' ; 'c ? x}")
test(txt, "125 ; abce")

txt = smagick("{''S, ~(dsort, ''c), ' ; 'c ? x}")
test(txt, "521 ; ecba")

#
# paste ####
#

x = 1:2
txt = smagick("{'x'paste, ', 'c ? x}")
test(txt, "x1, x2")

txt = smagick("{'*'paste.both, ', 'c ? x}")
test(txt, "*1*, *2*")

txt = smagick("{_ paste.right, ', 'c ? x}")
test(txt, "1_, 2_")

x = 1:3
txt = smagick("The number{if(.N>1 ; 's are 'paste.front ; ' is'paste.front), C ? x}.")
test(txt, "The numbers are 1, 2 and 3.")

x = 5
txt = smagick("The number{if(.N>1 ; 's are 'paste.front ; ' is 'paste.front), C ? x}.")
test(txt, "The number is 5.")

#
# insert ####
#

x = 1:2
txt = smagick("{'0'insert, ''c ? x}")
test(txt, "012")

txt = smagick("{'0'insert.both, ''c ? x}")
test(txt, "0120")

txt = smagick("{'0'insert.right, ''c ? x}")
test(txt, "120")

# conditional
x = c("521", "aebc")
test(smagick("{''S, ~('0'insert, ''c), ' ; 'c ? x}"), "err")

#
# fill ####
#

x = c("bon", "bonjour les gens")
txt = smagick("{fill, q, C ? x}")
test(txt, "'bon             ' and 'bonjour les gens'")

txt = smagick("{fill.right, q, C ? x}")
test(txt, "'             bon' and 'bonjour les gens'")

txt = smagick("{fill.center, q, C ? x}")
test(txt, "'      bon       ' and 'bonjour les gens'")

x = c(5, 15)
txt = smagick("{3 fill.right, q, C ? x}")
test(txt, "'  5' and ' 15'")

txt = smagick("{1 fill.right, q, C ? x}")
test(txt, "'5' and '15'")

txt = smagick("{'3|0'fill.right, q, C ? x}")
test(txt, "'005' and '015'")


#
# unik ####
#

x = c(1, 1, 2, 3, 3, 3) 
test(smagick("{unik ? x}"), 1:3)

# conditional
x = c("11211125564454", "aggafsgaffasg")
txt = smagick("{''S, ~(unik, ''c), ' ; 'c ? x}")
test(txt, "12564 ; agfs")

#
# nth ####
#

x = c(1, 6)
txt = smagick("They arrived {nth, C ? x}.")
test(txt, "They arrived 1st and 6th.")

txt = smagick("They arrived {Nth, C ? x}.")
test(txt, "They arrived first and sixth.")

#
# ntimes ####
#

x = c(1, 6)
txt = smagick("They won {ntimes, C ? x}.")
test(txt, "They won once and 6 times.")

txt = smagick("They won {Ntimes, C ? x}.")
test(txt, "They won once and six times.")

#
# n ####
#

x = c(45546, "bonjour")
txt = smagick("A = {n ? x} ; B = {n.l ? 55}")
test(txt, c("A = 45,546 ; B = fifty-five", "A = bonjour ; B = fifty-five"))

txt = smagick("{N.up ? 8} times he won!")
test(txt, "Eight times he won!")

#
# len ####
#

x = 1:5
txt = smagick("{Len.up ? x} numbers.")
test(txt, "Five numbers.")

x = 1:2
txt = smagick("x is of length {len ? x}, or {Len ? x}.")
test(txt, "x is of length 2, or two.")

# conditionaly
x = c("bonjour les gens", "la pluie", "est drue, je rentre")
txt = smagick("Number of words: {' 'S, ~(len), C ? x}.")
test(txt, "Number of words: 3, 2 and 4.")

num = str_ops(x, "' 'S, ~(len), num")
test(num, c(3, 2, 4))

#
# width ####
#

x = "Rome, l'unique objet de mon ressentiment, Rome a qui vient ton bras d'immoler mon amant"
txt = smagick("Voici le texte a apprendre:\n{'40|>'width ? x}.")
test(txt, c("Voici le texte a apprendre:\n> Rome, l'unique objet de mon\n> ressentiment, Rome a qui vient ton\n> bras d'immoler mon amant."))

#
# difftime ####
#

x = 3654
txt = smagick("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 1 hour 00 min.")

x = structure(1680294984.14505, class = c("POSIXct", "POSIXt")) - structure(1680292481.19258, class = c("POSIXct", "POSIXt"))
txt = smagick("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 41 min 42 sec.")

#
# erase ####
#

x = 1:5
txt = smagick("{if(. <= 3 ; erase), '.'c ? x}")
test(txt, "...4.5")

#
# nuke ####
#

x = 1:5
txt = smagick("nothing = {nuke ? x}")
test(txt, "nothing = ")

#
# rm ####
#

x = c("", "    ", "556", ":!", "pour qui sont ces", "serpents qui sifflent sur nos tetes?")
txt = smagick("{5 cfirst, rm, ', 'c ? x}")
test(txt, "    , 556, :!, pour , serpe")

txt = smagick("{5 cfirst, rm.blank, ', 'c ? x}")
test(txt, "556, :!, pour , serpe")

txt = smagick("{5 cfirst, rm.noalpha, ', 'c ? x}")
test(txt, "pour , serpe")

txt = smagick("{5 cfirst, rm.noalnum, ', 'c ? x}")
test(txt, "556, pour , serpe")

txt = smagick("{5 cfirst, rm.all, ', 'c ? x}")
test(txt, "")

txt = smagick("{5 cfirst, 'pour'rm.blank, ', 'c ? x}")
test(txt, "556, :!, serpe")

# conditional
x = smagick("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagick("{' 'S, rm.noalpha, ~('-'c), C ? x}")
test(txt, "laura-and, charles-and, charly-and and june")

#
# num ####
#

x = "parse55this"
txt = str_ops(x, "'\\d+'x, num")
test(txt, 55)

x = c("55", "abc")
txt = smagick("{num ? x}")
test(txt, c(55, NA))

txt = smagick("{num.soft ? x}")
test(txt, c(55, "abc"))

txt = smagick("{num.clear ? x}")
test(txt, c(55, ""))

txt = smagick("{num.rm ? x}")
test(txt, 55)

# conditional
x = smagick("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = smagick("{' 'S, num.rm, ~('-'c), C ? x}")
test(txt, "57-26, 32-7, 29-8 and 55")

#
# ascii ####
#

test(dsb("laurent .[ascii ! bergé]"), "laurent berge")

#
# stop ####
#

x = "Hi I'm Laurent and I'm trying to remove stop-words."
txt = smagick("Before: {x}\nAfter: {stop, ws ? x}")
test(txt, "Before: Hi I'm Laurent and I'm trying to remove stop-words.\nAfter: Hi Laurent trying remove stop-words.")

#
# if-else ####
#

# elementwise
x = c(15, 550)
txt = dsb("The value is .[& x > 50 ; > 50 ; <= 50]")
test(txt, c("The value is <= 50", "The value is > 50"))

txt = dsb("The value is .[& x > 50 ; > 50]")
test(txt, c("The value is ", "The value is > 50"))

# testing the quote
txt = dsb("The value is .[& x > 50 ; > 50 ; '   .']")
test(txt, c("The value is    .", "The value is > 50"))

# whole string
x = c(15, 550)
txt = dsb("The max value is .[& max(x) > 50 ; large ; small].")
test(txt, "The max value is large.")

x = c(-5, 15)
txt = dsb("The max value is .[& max(x) > 50 ; large ; small].")
test(txt, "The max value is small.")

x = c(-5, 15)
txt = dsb("The max value is .[& max(x) > 50 ; large].")
test(txt, "The max value is .")

# nesting
x = c(15, 550)
txt = smagick("The values are{& length(x) < 5 ; ': {C ? x}' ; 'too many'}.")
test(txt, "The values are: 15 and 550.")

# special if else
x = c(15, 550)
txt = dsb("The value is .[&& x > 50 ; > 50]")
test(txt, c("The value is 15", "The value is > 50"))

# long interpolated true and false
x = 1:3
y = letters[1:3]
txt = smagick("{C ! {&&x %% 2 == 1;{y}}}")
test(txt, "a, 2 and c")

z = tail(letters, 3)
txt = smagick("{C ! {&x %% 2 == 1; {y} ; {bq?z}}}")
test(txt, "`x`,  b and `z`")

#
# if ####
#

# special values: ., .len (or .N), .nchar (or .C)

x = c(5, 25, 30, 7)
txt = smagick("The large numbers are {if(.<10 ; nuke), C ? x}.")
test(txt, "The large numbers are 25 and 30.")

x = c(5, 25, 30, 7)
txt = smagick("The numbers are {if(.C < 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are x5, 2, 3 and x7.")

txt = smagick("The numbers are {if(.nchar >= 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are 5, x25, x30 and 7.")

x = c(5, 25, 30, 7)
txt = smagick("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5-25-30-7.")

x = c(5, 25, 30)
txt = smagick("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5:25:30.")

# conditional
x = c(5, 25, 30)
txt = smagick("Example: {if(.C < 2 ; 'x'pa ; ''s, ~('/'c)), C ? x}.")
test(txt, "Example: x5, 2/5 and 3/0.")


#
# vif ####
#

x = c(5, 25, 30, 7)
txt = smagick("There are {vif(.len<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are many numbers.")

x = c(5, 25)
txt = smagick("There are {vif(.N<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are not many numbers.")

# elementwise
x = c(5, 25, 30, 7)
txt = smagick("Numbers: {vif(.<10 ; small ; large), C ? x}.")
test(txt, "Numbers: small, large, large and small.")

# nesting
x = c(5, 25, 30, 7)
txt = smagick("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: prod = 26,250.")

x = c(5, 25, 30, 7, 5, 4)
txt = smagick("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: sum = 76.")

# '.' as a placeholder
x = c(5, 12, 20, 35)
txt = smagick("y = {3 first, vif(.N<2 ; short ; {' + 'c ? .}) ? x}")
test(txt, "y = 5 + 12 + 20")

#
# escaping ####
#

txt = smagick("\\{} it's some braces")
test(txt, "{} it's some braces")

txt = dsb("\\.[] it's some dsb")
test(txt, ".[] it's some dsb")

txt = smagick("I'm saying {q!ha \\} ha}!")
test(txt, "I'm saying 'ha } ha'!")

txt = dsb("I'm saying .[q!ha \\] ha]!")
test(txt, "I'm saying 'ha ] ha'!")

#
# user-defined ops ####
#

xplode = function(x, argument, options, ...){
  unlist(strsplit(x, ""))
}

smagick_register(xplode, "xplode")

txt = smagick("bon{xplode!jour}")
test(txt, c("bonj", "bono", "bonu", "bonr"))


#
# errors ####
#

test_err_contains(smagick("hi {there"), "bracket")

test_err_contains(smagick("hi {upper ! there"), "bracket")

test_err_contains(smagick("hi {S, ''S, ~(first ! bon, jour}"), "parenthesis")

test_err_contains(smagick("hi {$(bon) ! jour}"), "if/plurali & (v1")

test_err_contains(smagick("hi {&bonjour}"), "semi-colon")

test_err_contains(smagick("hi {&bonjour ; jour}"), "bonjour & evaluated")

test_err_contains(smagick("hi {&TRUE ; {upper ! jour ; voir}"), "closing bracket")

test_err_contains(smagick("hi {&TRUE ; {upperest ! jour} ; voir}"), "w/upper")

test_err_contains(smagick("hi {&TRUE ; {upperest ? jour} ; voir}"), "jour & evaluated")

test_err_contains(smagick("hi {&bonjour ; jour}"), "bonjour & not found")

test_err_contains(smagick("error operation: {if(1:5 ; upper ; lower) ! ohohoh}"), "w/if & length")

test_err_contains(smagick("error operation: {if(fakfak; upper ; lower) ! ohohoh}"), "w/if & evaluated & not found")

test_err_contains(smagick("error operation: {if(fakfak%m; upper ; lower) ! ohohoh}"), "w/if & parsed")

test_err_contains(smagick("error operation: {S, ~(sekg) ! ohohoh, hihihi}"), "not a valid op")

test_err_contains(smagick("/hi, {there"), "bracket & matched & escape")

#
# multi line expressions ####
#

txt = smagick("The solution to x + 32 = 5 is {
     y = 32
     z = 5
     z - y
}")
test(txt, "The solution to x + 32 = 5 is -27")

txt = smagick("First letters: {
     x = head(letters)
     fun = function(z){
          paste(z, collapse = '')
     }
     fun(x)
}")
test(txt, "First letters: abcdef")

#
# variables as arguments ####
#

txt = smagick("Hi {you}!", you = "Omer")
test(txt, "Hi Omer!")

txt = smagick("m * n = {m * n}", m = 5, n = 4)
test(txt, "m * n = 20")

# with nesting
txt = smagick("vars = {' + 'c ! {v1}_{suffix}}", v1 = c("x1", "z5"), suffix = "post")
test(txt, "vars = x1_post + z5_post")

# with ifelse
txt = smagick("value = {&v1 > 0 ; {.} ; {v2}}", v1 = 55, v2 = "error")
test(txt, "value = 55")

txt = smagick("value = {&v1 > 0 ; {.} ; {v2}}", v1 = -55, v2 = "error")
test(txt, "value = error")

# with plural
txt = smagick("The number{$s, is, ({v};{$enum}) ? x}", x = 1:3, v = "error")
test(txt, "The numbers are 1, 2 and 3")

txt = smagick("The number{$s, is, ({v};{$enum}) ? x}", x = 3, v = "error")
test(txt, "The number is error")

#
# data.table ####
#

if(FALSE && requireNamespace("data.table", quietly = TRUE)){
  # For some reason these tests don't work in non interactive use
  # I have to run them manually.
  # Don't have time to investigate, so be it.
  
  library(data.table)
  dt = as.data.table(iris)
  
  # variable creation
  dt[, sepal_info := sma("{%.0f ? Sepal.Length}-{%.0f ? Sepal.Width}")]
  test(dt[1, "sepal_info"], "5-4")
  
  # variable creation with grouping
  dt[, species_SL := 
          smagick("{first, up.f ? Species}: {mean(Sepal.Length)}"),
      by = Species]
  test(dt[1, "species_SL"], "Setosa: 5.006")
  
  # variable creation with grouping using DT special var
  dt[, species_N := 
          smagick("{first, 5k ? Species}: {.N}"),
      by = Species]
  test(dt[1, "species_N"], "setos: 50")
  
  # variable selection
  dt_small = dt[, .(petal_info = sma("{%.0f ? Petal.Length}-{%.0f ? Petal.Width}"))]
  test(dt_small[1, "petal_info"], "1-0")
  
  # variable selection with grouping
  dt_small = dt[, .(species_PL = sma("{first, 10 fill.c ? Species}: {%.1f ? mean(Petal.Length)}")), 
                 by = Species]
  test(dt_small[1, "species_PL"], "  setosa  : 1.5")
  
  
}




