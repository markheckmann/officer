# generates unique identifiers

generates unique identifiers based on
[`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html).

## Usage

``` r
uuid_generate(n = 1, ...)
```

## Arguments

- n:

  integer, number of unique identifiers to generate.

- ...:

  arguments sent to
  [`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)

## Examples

``` r
uuid_generate(n = 5)
#> [1] "b219e575-e39a-4356-9136-f86062a8989d"
#> [2] "cfd82824-c024-42ea-b044-1ea7191223dd"
#> [3] "50b36ac2-7623-4273-8502-95b9ae0abaa2"
#> [4] "b20b3cfe-d960-492c-b24d-e1e4110bf914"
#> [5] "4b4c9b99-27f1-4cf4-bac2-d7694b99f662"
```
