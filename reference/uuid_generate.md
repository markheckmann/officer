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
#> [1] "ce01d883-82a8-4f3f-9136-618248e42571"
#> [2] "4e86bc2a-861f-4b28-b2c5-b0fc966d15e8"
#> [3] "c2b2120c-e801-41b7-9b34-76b6a73d2ec7"
#> [4] "51a86bbd-b214-450d-9722-7c175dc5abb4"
#> [5] "41bd7f83-74f9-4f79-80b9-f4966094d866"
```
