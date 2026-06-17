# Github version of the package

Get the version from the DESCRIPTION file of the master branch in the
github repo

## Usage

``` r
package_github_version(x, file_lines = NULL)
```

## Arguments

- x:

  a single repo/package to check Ex:
  package_github_version("tidyverse/dplyr")

- file_lines:

  (default = NULL, internal)

## Examples

``` r
if (FALSE) {
  package_github_version("jrosell/jrrosell")
}
```
