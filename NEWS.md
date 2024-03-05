
# stringmagix 1.1.0

## New functions

- add `string_extract` to extract patterns

- add `string_split` to split character strings

## Improvements

- `string_ops` now uses `...` to pass operations. This is backward compatible.

- `string_clean`: now the magic flag also expands the replacements:
```R
x = "Hi Mary, how's John doing?"
from = "John"
to = "Kate"
string_clean(x, "m/{from} => {to}")
#> [1] "Hi Mary, how's Kate doing?"
```

- `string_magic`: add the `comma` flag to the `enum` operation. In that case, the enumeration ends with ", " instead of ", and ".

## Aliases

- new battery of short aliases: `sma` for `string_magic`, `catma` for catmagic, `mema` for `message_magic`, etc.. (`st_ops`, `st_is`, `st_any`, `st_all`, `stextract`, `stwhich`, `stget`, `stclean`, `stvec`, `streplace`, `stsplit` -- short names with vowels after `st` have an underscore.)

# stringmagic 1.0.0

First public release. The syntax should be stable.  

This package is a spinoff from [fixest's formula syntax interpolation](https://lrberge.github.io/fixest/reference/xpd.html).

Many thanks to Achim Zeileis, Vincent Arel-Bundock and Kyle Butts who provided insightful comments
during the development.
