
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coauthornetwork

The goal of coauthornetwork is to plot a network of authors-coauthors
from Google Scholar.

## Installation

You can install it from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/coauthornetwork")
```

## Example

The package only has two functions: `grab_network` to extract the
network of coauthors and `plot_coauthors` to plot the network as a
`ggraph`. The only thing you need is the string in the Google Scholar
profile after the root of the website. For example, a Google Scholar
profile URL is typically structured like this
`https://scholar.google.es/citations?user=amYIKXQAAAAJ&hl=en`.
`grab_network` will accept the end of the URL:
`citations?user=amYIKXQAAAAJ&hl=en` and search for the network of
coauthors.

A basic example:

``` r
library(coauthornetwork)

final_network <- grab_network('citations?user=amYIKXQAAAAJ&hl=en', n_coauthors = 5)
plot_coauthors(final_network, size_labels = 3)
```

<img src="man/figures/README-example-1.png" width="100%" />

`grab_network` has an additional argument called `n_deep` which controls
the degree of depth in which to go down the coauthorship list. An
`n_deep` of 1 (default) will grab all of the coauthors of the Google
Scholar profile and also their coauthors. An `n_deep` of 2 will expand
to this:

Google Scholar Profile – \> Coauthors –\> Coauthors –\> Coauthors

I urge the user to use an `n_deep` of 2 at most because the network can
grow exponentially with an `n_deep` of 2 or above. For
example..

``` r
final_network <- grab_network('citations?user=amYIKXQAAAAJ&hl=en', n_coauthors = 10, n_deep = 2)
plot_coauthors(final_network, size_labels = 3)
```

<img src="man/figures/README-pressure-1.png" width="100%" />
