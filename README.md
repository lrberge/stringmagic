
# stringmagic

The purpose of `stringmagic` is to facilitate the manipulation of character strings.

It introduces various functions to facilitate pattern detections via
[regular expression (regex) logic](LINK), or to efficienty [clean character vectors](LINK) in a 
readable way. Consistently across the package, regular expressions gain [optional flags](LINK) 
to monitor how the patterns should behave (fixed search? ignore case? add word boundaries? etc).
For more information, see the vignette on [string tools](LINK).

The main contribution of this package, and flagship function, is `smagic` which introduces 
a new language tailored to create complex character strings. 

### Getting started with `smagic`

`smagic` behaves in a similar way to the well known function [glue](https://glue.tidyverse.org/).
Use curly brackets to interpolate variables: i.e to insert their value directly in the string:
```r
library(stringmagic)
x = "John" ; y = "Mary"
smagic("Hi {x}! How's {y} doing?")
#> [1] "Hi John! How's Mary doing?"
```

Almost anything `glue` can do, `smagic` can do, and if you're thinking about speed, they're about
as fast (see the section [Performance](#Performance)).

The difference, and originality, of `smagic` is that you can apply any arbitrary operation to 
the interpolated variables. You want to create an enumeration? Sure. Add quotation marks? Check. 
Change the case? No problem. Sort on a substring? Of course! There are over 50 built-in operations and the
magic is that applying these operations is about as simple as saying them.

**Operations.** The syntax to add operations is as follows:

![operation](vignettes/images/operation-template.png)

The `operations` are a comma separated sequence of keywords, each keyword being bound to a specific function. Here is an example in which we apply two operations:
```r
lovers = c("romeo", "juliet")
smagic("Famous lovers: {title, enum ? lovers}.")
#> [1] "Famous lovers: Romeo and Juliet."
```

![example-lovers](vignettes/images/example-simple_operation.png)

**Arguments.** Some operations, like `split`, require arguments. Arguments are passed using quoted text
just before the operation. The syntax is:

![argument](vignettes/images/argument.png)

Let's take the example of splitting an email address and keeping the text before the domain:
```r
email = "John@Doe.com"
smagic("This message comes from {'@'split, first ? email}.")
#> [1] "This message comes from John."
```

![example-argument](vignettes/images/example-argument.png)

**Options.** Many arguments acccept options. These options are keywords working like flags (i.e. things
that can be turned on) and change the behavior of the current operation. 
Add options using a dot separated sequence of keywords attached to the operation:

![options](vignettes/images/options.png)


We have seen the `enum` operation in an earlier example, let's add a couple of options to it.
```r
fields = c("maths", "physics")
smagic("This position requires a PhD in either: {enum.i.or ? fields}.")
#> [1] "This position requires a PhD in either: i) maths, or ii) physics."
```

![example-options](vignettes/images/example-options.png)

**Anything else?** So far we have just scratched the surface of `smagic` possiblities.
Other features are: advanced support for pluralization, nesting, conditional operations,
grouped operations, compact if-else statements, and unlimited customization.
Here is a list of resources:

+ [an introduction to `smagic`](LINK)
+ [`smagic`'s reference](LINK)
+ [operations reference](LINK)

### Friendly errors

`smagic` tries to be friendly to the user by providing useful error messages:
```r
x = c("Zeus", "Hades", "Poseidon")
smagic("The {len?x} brothers: {anum?x}.")
#> Error: in smagic("The {len?x} brothers: {anum?x}."): 
#> CONTEXT: Problem found in "The {len?x} brothers: {anum?x}.",
#>          when dealing with the interpolation `{anum?x}`.
#> PROBLEM: `anum` is not a valid operator. Maybe you meant `enum`?
#> 
#> INFO: Type smagic('--help') for more help or smagic(.help = "regex") or smagic(.help = TRUE).
#> Ex. of valid stuff: smagic("Letters: {10 first, `6/2`last, ''c, 'i => e'r, upper.first ? letters}!")

smagic("The iris species are: {unik, sort ? iris[['Species']}.")
#> Error: in smagic("The iris species are: {unik, sort ? iris[...: 
#> CONTEXT: Problem found in "The iris species are: {unik, sort ? iris[['Species']}.",
#>          when dealing with the interpolation `{unik, sort ? iris[['Species']}.`.   
#> PROBLEM: in the expression ` iris[['Species']`, a bracket (`[`) open is not closed.
```

The cost of this feature in terms of computing time is of the order of magnitude of 50 micro seconds (of course it depends on context). 
If you're not interested in informative error messages, `.smagic` (note the `"."` prefix) is identical to `smagic` but avoids error handling and is then slightly faster.

### Performance

**Basic interpolation.** For regular string interpolations, the performance of `smagic` is similar to the performance of `glue`. That is to say, the price to pay for user experience is in the ballpark of 100 micro seconds (on my -- slow -- computer). Let's have a simple benchmark:

```r
library(microbenchmark)
library(glue)

x = "Romeo" ; y = "Juliet"
microbenchmark(base    =   paste0(x, " seems to love ", y, "."),
               glue    =     glue("{x} seems to love {y}."),
               smagic  =   smagic("{x} seems to love {y}."),
               .smagic =  .smagic("{x} seems to love {y}."))
#> Unit: microseconds
#>     expr  min    lq    mean median     uq   max neval
#>     base  1.2  1.40   1.739   1.55   1.70   5.2   100
#>     glue 76.8 83.80 105.493  87.80  96.50 295.4   100
#>   smagic 88.6 92.60 106.779  98.85 105.75 334.0   100
#>  .smagic 43.8 47.25  56.626  50.30  54.25 191.2   100
```

The difference with the base function `base::paste0` looks impressive (it looks 50 times faster), but is in fact not really important. Both `glue` and `smagic` processing time is due to overheads: a fixed cost that does not depend on the size of the vectors in input. Hence for large vectors or operations that run in one millisecond or more, this difference is negligible.

As you can notice, `.smagic`, `smagic` without error handling, is about twice faster than `glue`. But I'm not sure that sacrificing user expericence for a 50us overhead is really worth it!

**Complex operations** The function `smagic` shines when performing complex string manipulation. The question we ask here is: how much does it cost in terms of perfomance? Let's look at the following operation:

```r
x = c("Zeus", "Hades", "Poseidon")
smagic("The {len?x} brothers: {enum?x}.")
#> [1] "The 3 brothers: Zeus, Hades and Poseidon."
```

Although the interface is different, let's compare `smagic` to `glue` and `paste0`:

```r
x = c("Zeus", "Hades", "Poseidon")
microbenchmark(base = paste0("The ", length(x), " brothers: ", 
                        paste0(paste0(x[1:2], collapse = ", "), " and ", x[3]), "."),
               glue = glue("The {length(x)} brothers: {x_enum}.", 
                        x_enum = paste0(paste0(x[1:2], collapse = ", "), " and ", x[3])),
               smagic = smagic("The {len?x} brothers: {enum?x}."),
               smagic_bis = smagic("The {length(x)} brothers: {x_enum}.", 
                              x_enum = paste0(paste0(x[1:2], collapse = ", "), " and ", x[3])))
#> Unit: microseconds
#>        expr   min    lq    mean median     uq   max neval
#>        base   4.7   5.7   6.555   6.20   7.15  20.8   100
#>        glue  94.9 111.8 122.886 123.85 130.75 172.2   100
#>      smagic 406.1 422.7 444.330 435.00 451.25 643.7   100
#>  smagic_bis 116.2 136.3 144.848 144.05 150.60 285.8   100
```

As we can see, the processing overhead of `smagic` specific syntax is a few hundred microseconds. 
Remember that since regular interpolation can be performed, you can always fall back to `glue`-like processing (and benefit from the same performance), as is illustrated by the last command of the benchmark.
