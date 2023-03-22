
# via: Virtual Arrays <img src="man/figures/logo.png" align="right" />

[![](https://img.shields.io/badge/devel%20version-0.1.0-green.svg)](https://github.com/adamkocsis/via)
[![](https://www.r-pkg.org/badges/version/via?color=blue)](https://cran.r-project.org/package=via)
[![](http://cranlogs.r-pkg.org/badges/grand-total/via?color=yellow)](https://cran.r-project.org/package=via)
[![CRAN
checks](https://badges.cranchecks.info/summary/via.svg)](https://cran.r-project.org/web/checks/check_results_via.html)

The curse of dimensionality is a blessing in disguise. Although
multi-dimensional arrays in R are restricted to the six basic types,
many programming languages vectors and arrays as a template, which means
that you can make such compound objects from any type of object that you
like.

This becomes especially handy when it comes to organisation of
high-dimensional, spatial data.

## The package

The base class VirtualArray is defined, which acts as a wrapper around
lists allowing users to fold arbitrary sequential data into
n-dimensional, R-style virtual arrays. The derived XArray class is
defined to be used for homogeneous lists that contain a single class of
objects. The RasterArray and SfArray classes enable the use of stacked
spatial data instead of lists.

<div class="alert alert-danger" data-role="alert">

The package is still under intense development\!

</div>

-----

## Notes

#### History

The functions here were originally developed and published as part of
the [chronosphere](https://cran.r-project.org/package=chronosphere) R
package. For better compliance with [UNIX
philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) and more
efficient distribution/development, the original chronosphere has been
broken up to three R packages:

  - [`rgplates`](https://adamkocsis.github.io/rgplates/): functions
    related to tectonic reconstructions.
  - [`via`](https://adamkocsis.github.io/via/): Virtual Arrays for
    efficient organisation of high-dimenional data.
  - `chronosphere`: version-controlled data distribution.

This is a pre-alpha version, and like R, comes with absolutely no
warranty.
