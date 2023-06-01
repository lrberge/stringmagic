#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Aug  3 09:40:02 2022
# ~: string.ops tests
#----------------------------------------------#

# Tests all string operations
# - should go through all the branches
# - I test the conditional operations inside the operators when relevant


#
# slash ####
#

# regular
x = cub("/bon, jour, les gens, 
          je suis, la bas")
test(x, c("bon", "jour", "les gens", "je suis", "la bas"))

# with nesting
y = c("Ana", "Charles")
z = c("Romeo Montaigu", "Juliette Capulet")
x = cub("/Jules, {y}, Francis, {'\\s+'S, ~(cfirst, ''c) ? z}")
test(x, c("Jules", "Ana", "Charles", "Francis", "RM", "JC"))

#
# collapse ####
#

# regular
x = 1:3
test(cub("win = {'-'c ? x}"), "win = 1-2-3")

test(cub("win = {c ? x}"), "win = 1 2 3")

# with last element
test(cub("win = {C ? x}"), "win = 1, 2 and 3")

test(cub("win = {'|/'c ? x}"), "win = 12/3")

# default behavior
test(cub("win = {c ? x}"), "win = 1 2 3")

# empty collapse
test(cub("win = {''c ? x}"), "win = 123")

# conditional collapse
x = cub("/bonjour les gens., comment ca va?, bien?, bien.")
txt = cub("{' 'S, ~(cfirst.3, '.'c) ? x}")
test(txt, c("bon.les.gen", "com.ca.va?", "bie", "bie"))

#
# split ####
#

x = "Cogito...Ergo...Sum"
txt = cub("He said: {'f/...'s ? x}")
test(txt, c("He said: Cogito", "He said: Ergo", "He said: Sum"))

txt = cub("He said: {'i / [cs]'s ? x}")
test(txt, c("He said: ogito...Ergo...", "He said: um"))



#
# replace ####
#

x = c("Blanche dit 'Bla bla blanc'.")
txt = cub("She said: {'bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla nc'.")

txt = cub("She said: {'bla'r.ignore, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = cub("She said: {'i / bla'r, ws ? x}")
test(txt, "She said: nche dit ' nc'.")

txt = cub("She said: {'bla'r.word, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = cub("She said: {'w / bla'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla blanc'.")

txt = cub("She said: {'bla'r.word.ignore, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = cub("She said: {'wi / bla'r, ws ? x}")
test(txt, "She said: Blanche dit ' blanc'.")

txt = cub("She said: {'.'r.fixed, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = cub("She said: {'fixed / .'r, ws ? x}")
test(txt, "She said: Blanche dit 'Bla bla blanc'")

txt = cub("She said: {'Bla, blanc => ??'r.word, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = cub("She said: {'word / Bla, blanc => ??'r, ws ? x}")
test(txt, "She said: Blanche dit '?? bla ??'.")

txt = cub("She said: {'Bla, blanc => \\1\\1'r.w, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

txt = cub("She said: {'w / Bla, blanc => \\1\\1'r, ws ? x}")
test(txt, "She said: Blanche dit 'BlaBla bla blancblanc'.")

# total replacement
x = cub("/jingle bells, jingle bells, jingle, all the way")
txt = cub("{', 'c ! {'t / jing => [sound]'r ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

txt = cub("{', 'c ! {'jing => [sound]'r.total ? x}}")
test(txt, "[sound], [sound], [sound], all the way")

# default
test(cub("a = {r ? 1:5}"), "err")
test(cub("a = {R ? 1:5}"), "err")

# other with replacement

x = c("[]", "Moon", "Sun")

txt = dsb("Stare at the .['f / []'r ? x]")
test(txt, c("Stare at the ", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['f / [] => ...'r ? x]")
test(txt, c("Stare at the ...", "Stare at the Moon", "Stare at the Sun"))

txt = dsb("Stare at the .['[[:upper:]]'R ? x]")
test(txt, c("Stare at the []", "Stare at the oon", "Stare at the un"))

txt = dsb("Stare at the .['[[:upper:]] => P'R ? x]")
test(txt, c("Stare at the []", "Stare at the Poon", "Stare at the Pun"))


#
# x: single extraction ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = cub("mots: {'bla'x ? x}")
test(txt, c("mots: ", "mots: bla"))

test(cub("mots: {'bla'x.ignore ? x}"), 
     c("mots: Bla", "mots: Bla"))
     
test(cub("mots: {'i/bla'x ? x}"), 
     c("mots: Bla", "mots: Bla"))

test(cub("mots: {'bla'x.i.w ? x}"), 
     c("mots: ", "mots: Bla"))

test(cub("mots: {'iw/bla'x ? x}"), 
     c("mots: ", "mots: Bla"))

# default
txt = cub("mots: {x ? x}")
test(txt, c("mots: Blanche", "mots: Bla"))

# alias
test(cub("mots: {'iw/bla'extract.first ? x}"), 
     c("mots: ", "mots: Bla"))


#
# X: multiple extractions ####
#

x = c("Blanche dit", " 'Bla bla blanc'.")
txt = cub("{'bla'X ? x}")
test(txt, c("bla", "bla"))

txt = cub("{'bla'X.i ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = cub("{'i / bla'X ? x}")
test(txt, c("Bla", "Bla", "bla", "bla"))

txt = cub("{'bla'X.i.w ? x}")
test(txt, c("Bla", "bla"))

txt = cub("{'ignore, word / bla'X ? x}")
test(txt, c("Bla", "bla"))

# conditional
x = cub("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = cub("info: {'\\d+'X, ~('-'c), C ? x}")
test(txt, "info: 57-26, 32-7, 29-8 and 55")

#
# is, get, which ####
#

x = c("Hi Mary.", "Hi Charles!", "Are you OK?", "I think so Charles.", "Great to hear, Mary!")
txt = cub("A few quotes:\n{Q, 'mary | charles & !'get.i, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

txt = cub("A few quotes:\n{Q, 'i/mary | charles & !'get, '  'paste, '\n'c ? x}")
test(txt, "A few quotes:\n  \"Hi Charles!\"\n  \"Great to hear, Mary!\"")

x = c("It's me again.", "What do you mean a gain?", "Yes, again!", "Ah AGAIN, I thought A GAIN!")
index = str_op(x, "'gain'which.w.i")
test(index, c(2, 4))

index = str_op(x, "'gain & .'is.fixed")
test(index, c(TRUE, FALSE, FALSE, FALSE))

# conditional
x = cub("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = cub("info: {' 'S, '!\\d'get, ~('-'c), C ? x}")
test(txt, "info: laura-and, charles-and, charly-and and june")


#
# each/times ####
#

test(cub("I like {5 times.c ! ?} marks!"), "I like ????? marks!")

x = c("mary", "richard")
y = c("yes", "no")
txt = cub("The discussion: {', 'c ! {upper.first, 2 times ? x}: '{2 times ? y}'}...")
test(txt, "The discussion: Mary: 'yes', Richard: 'no', Mary: 'yes', Richard: 'no'...")

test(cub("values: {2 each.c ? c('a', 'b')}"), "values: aabb")

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
test(txt, "Where Is Bryan? Bryan Is In The Kitchen.")

x = "results from a new estimator: a new hope"
txt = dsb(".[title.i ? x]")
test(txt, "Results From a New Estimator: A New Hope")

#
# quotes ####
#

x = "siren"
txt = cub("Is it the song of the {q ? x}, of the {Q ? x} or of the {bq ? x}?")
test(txt, "Is it the song of the 'siren', of the \"siren\" or of the `siren`?")

#
# format ####
#

x = c(1, 123, 123456)
txt = cub("The numbers are:\n{'\n'c ! - {format ? x} | {rev, Format ? x}}")
test(txt, "The numbers are:\n- 1       | 123,456\n- 123     |     123\n- 123,456 |       1")

txt = cub("The numbers are:\n{'\n'c ! - {format.0 ? x} | {rev, Format.zero ? x}}")
test(txt, "The numbers are:\n- 0000001 | 123,456\n- 0000123 | 0000123\n- 123,456 | 0000001")


#
# sprintf ####
#

txt = cub("pi = {%.3f ? pi}")
test(txt, "pi = 3.142")

txt = cub("pi = {% 8.3f ? pi}")
test(txt, "pi =    3.142")

x = c("michael", "ana")
txt = cub("The winners are:\n{'\n'c ! - {% 10s ? x}}")
test(txt, "The winners are:\n-    michael\n-        ana")

txt = cub("The winners are:\n{'\n'c ! - {%- 10s ? x}!}")
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
txt = cub("{11k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = cub("{11 k, ' and 'c ? x}")
test(txt, "this is a l and a short one")

txt = cub("{'11|..'k, ' and 'c ? x}")
test(txt, "this is a l.. and a short one")

txt = cub("{'11||..'k, ' and 'c ? x}")
test(txt, "this is a.. and a short one")

#
# K, keep elements ####
#

x = 1:5

txt = cub("{3K ? x}")
test(txt, 1:3)

txt = cub("{3KO ? x}")
test(txt, c("1", "2", "three others"))

txt = cub("{3Ko ? x}")
test(txt, c("1", "2", "3 others"))

txt = cub("{'3|le reste'K ? x}")
test(txt, c("1", "2", "3", "le reste"))

txt = cub("{'3||le reste'K ? x}")
test(txt, c("1", "2", "le reste"))

txt = cub("{'3||et :rest:/:n:'K ? x}")
test(txt, c("1", "2", "et 3/5"))

txt = cub("{'3||et :REST:/:N:'K ? x}")
test(txt, c("1", "2", "et three/five"))

# conditional
x = cub("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
test(cub("info: {' 'S, ~(2Ko), C ? x}"), "err")

#
# enum ####
#

x = 1:5
txt = cub("{enum.q.3.oxf ? x}")
test(txt, "'1', '2', and 3 others")

txt = cub("{enum.bq.or ! x{1:3}}")
test(txt, "`x1`, `x2` or `x3`")

txt = cub("{enum.Q.nor.1 ! x{1:3}}")
test(txt, '1) "x1", 2) "x2", nor 3) "x3"')

txt = cub("{enum.a ! x{1:3}}")
test(txt, "a) x1, b) x2, and c) x3")

txt = cub("{enum.i ! x{1:3}}")
test(txt, "i) x1, ii) x2, and iii) x3")

# conditional
x = c("123", "abc", "a1b2")
txt = cub("{''S, ~(enum), ' ; 'c ? x}")
test(txt, "1, 2 and 3 ; a, b and c ; a, 1, b and 2")

#
# first and last ###
#

x = 1:5
txt = cub("{2 first, ''c ? x}")
test(txt, "12")

txt = cub("{last.2, ''c ? x}")
test(txt, "45")

a = 2
txt = cub("{'3'first, `a`last, ''c ? x}")
test(txt, "23")

txt = cub("{'1|1'first, ''c ? x}")
test(txt, "15")

txt = cub("{'-3'first, ''c ? x}")
test(txt, "45")

# conditional
x = c("123", "abc", "a1b2")
txt = cub("{''S, ~(first), ' ; 'c ? x}")
test(txt, "1 ; a ; a")

txt = cub("{''S, ~(last), ' ; 'c ? x}")
test(txt, "3 ; c ; 2")

#
# cfirst, clast ####
#

x = c("bonjour", "les", "gens")
a = 2
txt = cub("{3 cfirst, `a`clast ? x}")
test(txt, c("on", "es", "en"))

#
# rev ####
#

test(cub("{rev ? 1:3}"), 3:1)

# conditional
x = c("123", "abc", "a1b2")
txt = cub("{''S, ~(rev, ''c), ' ; 'c ? x}")
test(txt, "321 ; cba ; 2b1a")

#
# sort, dsort ####
#

x = c(5, 3, 8, 1)
test(cub("{sort ? x}"), c(1, 3, 5, 8))

test(cub("{dsort ? x}"), c(8, 5, 3, 1))

# conditional
x = c("521", "aebc")
txt = cub("{''S, ~(sort, ''c), ' ; 'c ? x}")
test(txt, "125 ; abce")

txt = cub("{''S, ~(dsort, ''c), ' ; 'c ? x}")
test(txt, "521 ; ecba")

#
# paste ####
#

x = 1:2
txt = cub("{'x'paste, ', 'c ? x}")
test(txt, "x1, x2")

txt = cub("{'*'paste.both, ', 'c ? x}")
test(txt, "*1*, *2*")

txt = cub("{_ paste.right, ', 'c ? x}")
test(txt, "1_, 2_")

x = 1:3
txt = cub("The number{if(.N>1 ; 's are 'paste.front ; ' is'paste.front), C ? x}.")
test(txt, "The numbers are 1, 2 and 3.")

x = 5
txt = cub("The number{if(.N>1 ; 's are 'paste.front ; ' is 'paste.front), C ? x}.")
test(txt, "The number is 5.")

#
# append ####
#

x = 1:2
txt = cub("{'0'append, ''c ? x}")
test(txt, "012")

txt = cub("{'0'append.both, ''c ? x}")
test(txt, "0120")

txt = cub("{'0'append.right, ''c ? x}")
test(txt, "120")

# conditional
x = c("521", "aebc")
test(cub("{''S, ~('0'append, ''c), ' ; 'c ? x}"), "err")

#
# fill ####
#

x = c("bon", "bonjour les gens")
txt = cub("{fill, q, C ? x}")
test(txt, "'bon             ' and 'bonjour les gens'")

txt = cub("{fill.right, q, C ? x}")
test(txt, "'             bon' and 'bonjour les gens'")

txt = cub("{fill.center, q, C ? x}")
test(txt, "'       bon      ' and 'bonjour les gens'")

x = c(5, 15)
txt = cub("{3 fill.right, q, C ? x}")
test(txt, "'  5' and ' 15'")

txt = cub("{1 fill.right, q, C ? x}")
test(txt, "'5' and '15'")

txt = cub("{3 fill.right.0, q, C ? x}")
test(txt, "'005' and '015'")


#
# unik ####
#

x = c(1, 1, 2, 3, 3, 3) 
test(cub("{unik ? x}"), 1:3)

# conditional
x = c("11211125564454", "aggafsgaffasg")
txt = cub("{''S, ~(unik, ''c), ' ; 'c ? x}")
test(txt, "12564 ; agfs")

#
# nth ####
#

x = c(1, 6)
txt = cub("They arrived {nth, C ? x}.")
test(txt, "They arrived 1st and 6th.")

txt = cub("They arrived {Nth, C ? x}.")
test(txt, "They arrived first and sixth.")

#
# ntimes ####
#

x = c(1, 6)
txt = cub("They won {ntimes, C ? x}.")
test(txt, "They won 1 time and 6 times.")

txt = cub("They won {Ntimes, C ? x}.")
test(txt, "They won once and six times.")

#
# n ####
#

x = c(45546, "bonjour")
txt = cub("A = {n ? x} ; B = {n.l ? 55}")
test(txt, c("A = 45,546 ; B = fifty-five", "A = bonjour ; B = fifty-five"))

txt = cub("{N.up ? 8} times he won!")
test(txt, "Eight times he won!")

#
# len ####
#

x = 1:5
txt = cub("{Len.up ? x} numbers.")
test(txt, "Five numbers.")

x = 1:2
txt = cub("x is of length {len ? x}, or {Len ? x}.")
test(txt, "x is of length 2, or two.")

# conditionaly
x = c("bonjour les gens", "la pluie", "est drue, je rentre")
txt = cub("Number of words: {' 'S, ~(len), C ? x}.")
test(txt, "Number of words: 3, 2 and 4.")

num = str_op(x, "' 'S, ~(len), num")
test(num, c(3, 2, 4))

#
# swidth ####
#

x = "Rome, l'unique objet de mon ressentiment, Rome a qui vient ton bras d'immoler mon amant"
txt = cub("Voici le texte a apprendre:\n{40 swidth.> ? x}.")
test(txt, c("Voici le texte a apprendre:\n> Rome, l'unique objet de mon\n> ressentiment, Rome a qui vient ton\n> bras d'immoler mon amant."))

#
# difftime ####
#

x = 3654
txt = cub("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 1 hour 00 min.")

x = structure(1680294984.14505, class = c("POSIXct", "POSIXt")) - structure(1680292481.19258, class = c("POSIXct", "POSIXt"))
txt = cub("Time since last check: {dtime ? x}.")
test(txt, "Time since last check: 41 min 42 sec.")

#
# erase ####
#

x = 1:5
txt = cub("{if(. <= 3 ; erase), '.'c ? x}")
test(txt, "...4.5")

#
# nuke ####
#

x = 1:5
txt = cub("nothing = {nuke ? x}")
test(txt, "nothing = ")

#
# rm ####
#

x = c("", "    ", "556", ":!", "pour qui sont ces", "serpents qui sifflent sur nos tetes?")
txt = cub("{5 cfirst, rm, ', 'c ? x}")
test(txt, "    , 556, :!, pour , serpe")

txt = cub("{5 cfirst, rm.blank, ', 'c ? x}")
test(txt, "556, :!, pour , serpe")

txt = cub("{5 cfirst, rm.noalpha, ', 'c ? x}")
test(txt, "pour , serpe")

txt = cub("{5 cfirst, rm.noalnum, ', 'c ? x}")
test(txt, "556, pour , serpe")

txt = cub("{5 cfirst, rm.all, ', 'c ? x}")
test(txt, "")

txt = cub("{5 cfirst, 'pour'rm.blank, ', 'c ? x}")
test(txt, "556, :!, serpe")

# conditional
x = cub("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = cub("{' 'S, rm.noalpha, ~('-'c), C ? x}")
test(txt, "laura-and, charles-and, charly-and and june")

#
# num ####
#

x = "parse55this"
txt = str_op(x, "'\\d+'x, num")
test(txt, 55)

x = c("55", "abc")
txt = cub("{num ? x}")
test(txt, c(55, NA))

txt = cub("{num.soft ? x}")
test(txt, c(55, "abc"))

txt = cub("{num.clean ? x}")
test(txt, c(55, ""))

txt = cub("{num.rm ? x}")
test(txt, 55)

# conditional
x = cub("/laura 57 and 26, charles 32 and 7, charly 29 and 8, june 55")
txt = cub("{' 'S, num.rm, ~('-'c), C ? x}")
test(txt, "57-26, 32-7, 29-8 and 55")

#
# ascii ####
#

test(dsb("laurent .[ascii ! bergé]"), "laurent berge")

#
# stop ####
#

x = "Hi I'm Laurent and I'm trying to remove stop-words."
txt = cub("Before: {x}\nAfter: {stop, ws ? x}")
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
txt = cub("The values are{& length(x) < 5 ; ': {C ? x}' ; 'too many'}.")
test(txt, "The values are: 15 and 550.")

# special if else
x = c(15, 550)
txt = dsb("The value is .[&& x > 50 ; > 50]")
test(txt, c("The value is 15", "The value is > 50"))

#
# if ####
#

# special values: ., .len (or .N), .nchar (or .C)

x = c(5, 25, 30, 7)
txt = cub("The large numbers are {if(.<10 ; nuke), C ? x}.")
test(txt, "The large numbers are 25 and 30.")

x = c(5, 25, 30, 7)
txt = cub("The numbers are {if(.C < 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are x5, 2, 3 and x7.")

txt = cub("The numbers are {if(.nchar >= 2 ; 'x'pa ; 1k), C ? x}.")
test(txt, "The numbers are 5, x25, x30 and 7.")

x = c(5, 25, 30, 7)
txt = cub("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5-25-30-7.")

x = c(5, 25, 30)
txt = cub("The hash is {if(.N < 4 ; ':'c ; '-'c), C ? x}.")
test(txt, "The hash is 5:25:30.")

# conditional
x = c(5, 25, 30)
txt = cub("Example: {if(.C < 2 ; 'x'pa ; ''s, ~('/'c)), C ? x}.")
test(txt, "Example: x5, 2/5 and 3/0.")


#
# vif ####
#

x = c(5, 25, 30, 7)
txt = cub("There are {vif(.len<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are many numbers.")

x = c(5, 25)
txt = cub("There are {vif(.N<3 ; not many ; many), C ? x} numbers.")
test(txt, "There are not many numbers.")

# elementwise
x = c(5, 25, 30, 7)
txt = cub("Numbers: {vif(.<10 ; small ; large), C ? x}.")
test(txt, "Numbers: small, large, large and small.")

# nesting
x = c(5, 25, 30, 7)
txt = cub("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: prod = 26,250.")

x = c(5, 25, 30, 7, 5, 4)
txt = cub("Numbers: {vif(.N > 5 ; sum = {n?sum(x)} ; prod = {n?prod(x)}), C ? x}.")
test(txt, "Numbers: sum = 76.")

# '.' as a placeholder
x = c(5, 12, 20, 35)
txt = cub("y = {3 first, vif(.N<2 ; short ; {' + 'c ? .}) ? x}")
test(txt, "y = 5 + 12 + 20")

#
# escaping ####
#

txt = cub("\\{} it's some braces")
test(txt, "{} it's some braces")

txt = dsb("\\.[] it's some dsb")
test(txt, ".[] it's some dsb")

txt = cub("I'm saying {q!ha \\} ha}!")
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

txt = cub("bon{xplode!jour}")
test(txt, c("bonj", "bono", "bonu", "bonr"))



