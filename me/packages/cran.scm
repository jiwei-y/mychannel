(define-module (me packages cran)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages cran))

(define-public r-lfe
  (package
    (name "r-lfe")
    (version "2.8-8")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lfe" version))
              (sha256
               (base32
                "0wdbhnyzzvpkjyjsxidkcwpssij3k476lhlfdkk2xwhnz8l2khhg"))))
    (properties `((upstream-name . "lfe")))
    (build-system r-build-system)
    (propagated-inputs (list r-formula r-matrix r-sandwich r-xtable))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/MatthieuStigler/lfe")
    (synopsis "Linear Group Fixed Effects")
    (description
     "Transforms away factors with many levels prior to doing an OLS. Useful for
estimating linear models with multiple group fixed effects, and for estimating
linear models which uses factors with many levels as pure control variables.
See Gaure (2013) <doi:10.1016/j.csda.2013.03.024> Includes support for
instrumental variables, conditional F statistics for weak instruments, robust
and multi-way clustered standard errors, as well as limited mobility bias
correction (Gaure 2014 <doi:10.1002/sta4.68>).  WARNING: This package is NOT
under active development anymore, no further improvements are to be expected,
and the package is at risk of being removed from CRAN.")
    (license artistic2.0)))

(define-public r-collapse
  (package
    (name "r-collapse")
    (version "1.8.8")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "collapse" version))
              (sha256
               (base32
                "1598jrp7zdviz0y2gzlc6nqmd3jig25fwzypl2sbd0ry6n8pizzk"))))
    (properties `((upstream-name . "collapse")))
    (build-system r-build-system)
    (propagated-inputs (list r-rcpp))
    (native-inputs (list r-knitr))
    (home-page "https://sebkrantz.github.io/collapse/")
    (synopsis "Advanced and Fast Data Transformation")
    (description
     "This package provides a C/C++ based package for advanced data transformation and
statistical computing in R that is extremely fast, class-agnostic, and
programmer friendly through a flexible and parsimonious syntax.  It is well
integrated with base R, 'dplyr' / (grouped) 'tibble', 'data.table', 'sf', 'plm'
(panel-series and data frames), and non-destructively handles other matrix or
data frame based classes (like 'ts', 'xts' / 'zoo', 'tsibble', ...) --- Key
Features: --- (1) Advanced statistical programming: A full set of fast
statistical functions supporting grouped and weighted computations on vectors,
matrices and data frames.  Fast and programmable grouping, ordering, unique
values/rows, factor generation and interactions.  Fast and flexible functions
for data manipulation, data object conversions, and memory efficient R
programming. (2) Advanced aggregation: Fast and easy multi-data-type,
multi-function, weighted and parallelized data aggregation. (3) Advanced
transformations: Fast row/column arithmetic, (grouped) replacing and sweeping
out of statistics (by reference), (grouped, weighted) scaling/standardizing,
(higher-dimensional) between (averaging) and (quasi-)within (demeaning)
transformations, linear prediction, model fitting and testing exclusion
restrictions. (4) Advanced time-computations: Fast and flexible indexed time
series and panel data classes.  Fast (sequences of) lags/leads, and
(lagged/leaded, iterated, quasi-, log-) differences and (compounded) growth
rates on (irregular) time series and panels.  Multivariate auto-, partial- and
cross-correlation functions for panel data.  Panel data to (ts-)array
conversions. (5) List processing: Recursive list search, splitting,
extraction/subsetting, apply, and generalized row-binding / unlisting to data
frame. (6) Advanced data exploration: Fast (grouped, weighted, panel-decomposed)
summary statistics and descriptive tools.")
    (license (list gpl2+
                   (fsdg-compatible "file://LICENSE")))))
(define-public r-plm
  (package
    (name "r-plm")
    (version "2.6-2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plm" version))
              (sha256
               (base32
                "0r0yhr00hfjh5nksp4pbkifjyf6jbxcq8hwjqlbdmvkz51fi4ayz"))))
    (properties `((upstream-name . "plm")))
    (build-system r-build-system)
    (propagated-inputs (list r-bdsmatrix
                             r-collapse
                             r-formula
                             r-lattice
                             r-lmtest
                             r-mass
                             r-maxlik
                             r-nlme
                             r-rdpack
                             r-sandwich
                             r-zoo))
    (native-inputs (list r-knitr))
    (home-page "https://cran.r-project.org/package=plm")
    (synopsis "Linear Models for Panel Data")
    (description
     "This package provides a set of estimators for models and (robust) covariance
matrices, and tests for panel data econometrics, including within/fixed effects,
random effects, between, first-difference, nested random effects as well as
instrumental-variable (IV) and Hausman-Taylor-style models, panel generalized
method of moments (GMM) and general FGLS models, mean groups (MG), demeaned MG,
and common correlated effects (CCEMG) and pooled (CCEP) estimators with common
factors, variable coefficients and limited dependent variables models.  Test
functions include model specification, serial correlation, cross-sectional
dependence, panel unit root and panel Granger (non-)causality.  Typical
references are general econometrics text books such as Baltagi (2021),
Econometric Analysis of Panel Data (<doi:10.1007/978-3-030-53953-5>), Hsiao
(2014), Analysis of Panel Data (<doi:10.1017/CBO9781139839327>), and Croissant
and Millo (2018), Panel Data Econometrics with R (<doi:10.1002/9781119504641>).")
    (license gpl2+)))

(define-public r-matlib
  (package
    (name "r-matlib")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "matlib" version))
              (sha256
               (base32
                "0m03zygd43r0z7cldiwsc91p1xyni6pdvwm8pz4jcg5bmqasfg0w"))))
    (properties `((upstream-name . "matlib")))
    (build-system r-build-system)
    (propagated-inputs (list r-car r-mass r-rgl r-xtable))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/friendly/matlib")
    (synopsis
     "Matrix Functions for Teaching and Learning Linear Algebra and Multivariate Statistics")
    (description
     "This package provides a collection of matrix functions for teaching and learning
matrix linear algebra as used in multivariate statistical methods.  These
functions are mainly for tutorial purposes in learning matrix algebra ideas
using R.  In some cases, functions are provided for concepts available elsewhere
in R, but where the function call or name is not obvious.  In other cases,
functions are provided to show or demonstrate an algorithm.  In addition, a
collection of functions are provided for drawing vector diagrams in 2D and 3D.")
    (license gpl2+)))

(define-public r-tsibbledata
  (package
    (name "r-tsibbledata")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "tsibbledata" version))
              (sha256
               (base32
                "0f9gi8h49xc4xd38fg0s26p4xrkpxr4qfdfapk7p1vfszb31ihjd"))))
    (properties `((upstream-name . "tsibbledata")))
    (build-system r-build-system)
    (propagated-inputs (list r-rappdirs r-tsibble))
    (home-page "https://tsibbledata.tidyverts.org/")
    (synopsis "Diverse Datasets for 'tsibble'")
    (description
     "This package provides diverse datasets in the 'tsibble' data structure.  These
datasets are useful for learning and demonstrating how tidy temporal data can
tidied, visualised, and forecasted.")
    (license gpl3)))

(define-public r-feasts
  (package
    (name "r-feasts")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "feasts" version))
              (sha256
               (base32
                "1ffmm0fsm366fiiai3r2s3md96f6h5lyzc0qxdfv0b7sqavh2m09"))))
    (properties `((upstream-name . "feasts")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr
                             r-fabletools
                             r-ggplot2
                             r-lifecycle
                             r-lubridate
                             r-rlang
                             r-scales
                             r-slider
                             r-tibble
                             r-tidyr
                             r-tsibble
                             r-vctrs))
    (native-inputs (list r-knitr))
    (home-page "http://feasts.tidyverts.org/")
    (synopsis "Feature Extraction and Statistics for Time Series")
    (description
     "This package provides a collection of features, decomposition methods,
statistical summaries and graphics functions for the analysing tidy time series
data.  The package name 'feasts' is an acronym comprising of its key features:
Feature Extraction And Statistics for Time Series.")
    (license gpl3)))

(define-public r-tsibble
  (package
    (name "r-tsibble")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "tsibble" version))
              (sha256
               (base32
                "172xb358q3fcfs9rdapllqwmw6yj6qx93bc9br6kfd437sk86rx6"))))
    (properties `((upstream-name . "tsibble")))
    (build-system r-build-system)
    (propagated-inputs (list r-anytime
                             r-dplyr
                             r-ellipsis
                             r-generics
                             r-lifecycle
                             r-lubridate
                             r-rlang
                             r-tibble
                             r-tidyselect
                             r-vctrs))
    (native-inputs (list r-knitr))
    (home-page "https://tsibble.tidyverts.org")
    (synopsis "Tidy Temporal Data Frames and Tools")
    (description
     "This package provides a 'tbl_ts' class (the 'tsibble') for temporal data in an
data- and model-oriented format.  The 'tsibble' provides tools to easily
manipulate and analyse temporal data, such as filling in time gaps and
aggregating over calendar periods.")
    (license gpl3)))

(define-public r-fabletools
  (package
    (name "r-fabletools")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "fabletools" version))
              (sha256
               (base32
                "1appg3pzw6b08nxh0p1ldbvv1j091v0kcygm808nf04v9axp5f95"))))
    (properties `((upstream-name . "fabletools")))
    (build-system r-build-system)
    (propagated-inputs (list r-distributional
                             r-dplyr
                             r-generics
                             r-ggplot2
                             r-lifecycle
                             r-progressr
                             r-r6
                             r-rlang
                             r-tibble
                             r-tidyr
                             r-tidyselect
                             r-tsibble
                             r-vctrs))
    (native-inputs (list r-knitr))
    (home-page "https://fabletools.tidyverts.org/")
    (synopsis "Core Tools for Packages in the 'fable' Framework")
    (description
     "This package provides tools, helpers and data structures for developing models
and time series functions for 'fable' and extension packages.  These tools
support a consistent and tidy interface for time series modelling and analysis.")
    (license gpl3)))

(define-public r-fable
  (package
    (name "r-fable")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "fable" version))
              (sha256
               (base32
                "185l3gd83nys11k389ha2ssdm9rkw9aackmz2cd2mvlym72rsnq7"))))
    (properties `((upstream-name . "fable")))
    (build-system r-build-system)
    (propagated-inputs (list r-distributional
                             r-dplyr
                             r-fabletools
                             r-rcpp
                             r-rlang
                             r-tibble
                             r-tidyr
                             r-tsibble))
    (native-inputs (list r-knitr))
    (home-page "https://fable.tidyverts.org")
    (synopsis "Forecasting Models for Tidy Time Series")
    (description
     "This package provides a collection of commonly used univariate and multivariate
time series forecasting models including automatically selected exponential
smoothing (ETS) and autoregressive integrated moving average (ARIMA) models.
These models work within the 'fable' framework provided by the 'fabletools'
package, which provides the tools to evaluate, visualise, and combine models in
a workflow consistent with the tidyverse.")
    (license gpl3)))

(define-public r-fpp3
  (package
    (name "r-fpp3")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "fpp3" version))
              (sha256
               (base32
                "1920hlckc1bvdhqrp6mdl900c34k62v87k5l22zwwycx5gcicrxh"))))
    (properties `((upstream-name . "fpp3")))
    (build-system r-build-system)
    (propagated-inputs (list r-cli
                             r-crayon
                             r-dplyr
                             r-fable
                             r-fabletools
                             r-feasts
                             r-ggplot2
                             r-lubridate
                             r-magrittr
                             r-purrr
                             r-rstudioapi
                             r-tibble
                             r-tidyr
                             r-tsibble
                             r-tsibbledata
                             r-urca))
    (home-page "https://github.com/robjhyndman/fpp3-package")
    (synopsis
              "Data for \"Forecasting: Principles and Practice\" (3rd Edition)")
    (description
     " All data sets required for the examples and exercises in the book \"Forecasting:
principles and practice\" by Rob J Hyndman and George Athanasopoulos
<https://OTexts.com/fpp3/>.  All packages required to run the examples are also
loaded.")
    (license gpl3)))

(define-public r-atsa
  (package
    (name "r-atsa")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "aTSA" version))
              (sha256
               (base32
                "1p3spas0sxj08hkb8p6k2fy64w86prlw1hbnrqnrklr0hnkg2g54"))))
    (properties `((upstream-name . "aTSA")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/package=aTSA")
    (synopsis "Alternative Time Series Analysis")
    (description
     "Contains some tools for testing, analyzing time series data and fitting popular
  time series models such as ARIMA, Moving Average and Holt Winters, etc.  Most
  functions also provide nice and clear outputs like SAS does, such as identify,
  estimate and forecast, which are the same statements in PROC ARIMA in SAS.")
    (license #f)))

(define-public r-ardl
  (package
    (name "r-ardl")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ARDL" version))
              (sha256
               (base32
                "0vmx21dvbz5l67drdma77z9x30j6sw650lhkrkcbifjn915x6dks"))))
    (properties `((upstream-name . "ARDL")))
    (build-system r-build-system)
    (propagated-inputs (list r-aod
                             r-dplyr
                             r-dynlm
                             r-lmtest
                             r-msm
                             r-stringr
                             r-zoo))
    (home-page "https://github.com/Natsiopoulos/ARDL")
    (synopsis "ARDL, ECM and Bounds-Test for Cointegration")
    (description
     "Creates complex autoregressive distributed lag (ARDL) models providing just the
  order and automatically constructs the underlying unrestricted and restricted
  error correction model (ECM).  It also performs the bounds-test for
  cointegration as described in Pesaran et al. (2001) <doi:10.1002/jae.616> and
  provides the multipliers and the cointegrating equation.")
    (license gpl3)))

(define-public r-dynlm
  (package
    (name "r-dynlm")
    (version "0.3-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "dynlm" version))
              (sha256
               (base32
                "1g8qsb5m69yn35sm0shd97vbnbpqfkjkd7lbkwch1gpfxkld53zq"))))
    (properties `((upstream-name . "dynlm")))
    (build-system r-build-system)
    (propagated-inputs (list r-car r-lmtest r-zoo))
    (home-page "https://cran.r-project.org/package=dynlm")
    (synopsis "Dynamic Linear Regression")
    (description "Dynamic linear models and time series regression.")
    (license #f)))

(define-public r-vars
  (package
    (name "r-vars")
    (version "1.5-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "vars" version))
              (sha256
               (base32
                "1ggkmds5ibww61mkc50bzh75py4n2g9bd6wqb3nvfw9zn58k7znx"))))
    (properties `((upstream-name . "vars")))
    (build-system r-build-system)
    (propagated-inputs (list r-lmtest r-mass r-sandwich r-strucchange r-urca))
    (home-page "https://www.pfaffikus.de")
    (synopsis "VAR Modelling")
    (description
     "Estimation, lag selection, diagnostic testing, forecasting, causality analysis,
forecast error variance decomposition and impulse response functions of VAR
models and estimation of SVAR and SVEC models.")
    (license gpl2+)))

(define-public r-tserieschaos
  (package
    (name "r-tserieschaos")
    (version "0.1-13.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "tseriesChaos" version))
              (sha256
               (base32
                "0qfrrzd0h8n9zp7wj5fl88wkiv22fs5zy8x509g316j0avm5zjr3"))))
    (properties `((upstream-name . "tseriesChaos")))
    (build-system r-build-system)
    (propagated-inputs (list r-desolve))
    (home-page "https://cran.r-project.org/package=tseriesChaos")
    (synopsis "Analysis of Nonlinear Time Series")
    (description
     "Routines for the analysis of nonlinear time series.  This work is largely
inspired by the TISEAN project, by Rainer Hegger, Holger Kantz and Thomas
Schreiber: <http://www.mpipks-dresden.mpg.de/~tisean/>.")
    (license gpl2)))

(define-public r-tsdyn
  (package
    (name "r-tsdyn")
    (version "11.0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "tsDyn" version))
              (sha256
               (base32
                "0dplrl4vknha2aa0h3vg0sivn2dm0h23my6y2gdghhplvzy0qaa4"))))
    (properties `((upstream-name . "tsDyn")))
    (build-system r-build-system)
    (propagated-inputs (list r-foreach
                             r-forecast
                             r-mass
                             r-matrix
                             r-mgcv
                             r-mnormt
                             r-nnet
                             r-tseries
                             r-tserieschaos
                             r-urca
                             r-vars))
    (home-page "https://github.com/MatthieuStigler/tsDyn/wiki")
    (synopsis "Nonlinear Time Series Models with Regime Switching")
    (description
     "Implements nonlinear autoregressive (AR) time series models.  For univariate
series, a non-parametric approach is available through additive nonlinear AR.
Parametric modeling and testing for regime switching dynamics is available when
the transition is either direct (TAR: threshold AR) or smooth (STAR: smooth
transition AR, LSTAR).  For multivariate series, one can estimate a range of
TVAR or threshold cointegration TVECM models with two or three regimes.  Tests
can be conducted for TVAR as well as for TVECM (Hansen and Seo 2002 and Seo
2006).")
    (license gpl2+)))

;; business-science, maybe non-necessary?
(define-public r-sweep
  (package
    (name "r-sweep")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "sweep" version))
              (sha256
               (base32
                "1705mcp9p5h50ifqjjwx61z5wl5izv889nxcgdkx1i0dlcr61l2a"))))
    (properties `((upstream-name . "sweep")))
    (build-system r-build-system)
    (propagated-inputs (list r-broom
                             r-dplyr
                             r-forecast
                             r-lubridate
                             r-rlang
                             r-tibble
                             r-tidyr
                             r-timetk))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/business-science/sweep")
    (synopsis "Tidy Tools for Forecasting")
    (description
     " Tidies up the forecasting modeling and prediction work flow, extends the
  'broom' package with 'sw_tidy', 'sw_glance', 'sw_augment', and 'sw_tidy_decomp'
  functions for various forecasting models, and enables converting 'forecast'
  objects to \"tidy\" data frames with 'sw_sweep'.")
    (license gpl3+)))

(define-public r-timetk
  (package
    (name "r-timetk")
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "timetk" version))
              (sha256
               (base32
                "0jwa3qllninjsq5xx7rhls72w1955a3ysscaq80vby7fjij80pwb"))))
    (properties `((upstream-name . "timetk")))
    (build-system r-build-system)
    (propagated-inputs (list r-anytime
                             r-assertthat
                             r-dplyr
                             r-forcats
                             r-forecast
                             r-generics
                             r-ggplot2
                             r-hms
                             r-lubridate
                             r-padr
                             r-plotly
                             r-purrr
                             r-readr
                             r-recipes
                             r-rlang
                             r-rsample
                             r-slider
                             r-stringi
                             r-stringr
                             r-tibble
                             r-tidyr
                             r-tidyselect
                             r-timedate
                             r-tsfeatures
                             r-xts
                             r-zoo))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/business-science/timetk")
    (synopsis "A Tool Kit for Working with Time Series in R")
    (description
     " Easy visualization, wrangling, and feature engineering of time series data for
  forecasting and machine learning prediction.  Consolidates and extends time
  series functionality from packages including 'dplyr', 'stats', 'xts',
  'forecast', 'slider', 'padr', 'recipes', and 'rsample'.")
    (license gpl3+)))

(define-public r-anytime
  (package
    (name "r-anytime")
    (version "0.3.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "anytime" version))
              (sha256
               (base32
                "0jjpqynai5nd7mfy1smb44356f3d5bmpxhs1i9x9jw5c959c35hh"))))
    (properties `((upstream-name . "anytime")))
    (build-system r-build-system)
    (propagated-inputs (list r-bh r-rcpp))
    (home-page "http://dirk.eddelbuettel.com/code/anytime.html")
    (synopsis "Anything to 'POSIXct' or 'Date' Converter")
    (description
     "Convert input in any one of character, integer, numeric, factor, or ordered type
  into 'POSIXct' (or 'Date') objects, using one of a number of predefined formats,
  and relying on Boost facilities for date and time parsing.")
    (license gpl2+)))

(define-public r-padr
  (package
    (name "r-padr")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "padr" version))
              (sha256
               (base32
                "1l8i40vqpy033j8nc8fqp9ymn378bhyy4hkj8xhk94npv7jibqkk"))))
    (properties `((upstream-name . "padr")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr r-lubridate r-rcpp r-rlang))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/EdwinTh/padr")
    (synopsis "Quickly Get Datetime Data Ready for Analysis")
    (description
     "Transforms datetime data into a format ready for analysis.  It offers two core
  functionalities; aggregating data to a higher level interval (thicken) and
  imputing records where observations were absent (pad).")
    (license expat)))

(define-public r-tsfeatures
  (package
    (name "r-tsfeatures")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "tsfeatures" version))
              (sha256
               (base32
                "1zi7p7gdd9bg6rpdin88rq9qi43cpk663xmvffa7d5p1w45831kd"))))
    (properties `((upstream-name . "tsfeatures")))
    (build-system r-build-system)
    (propagated-inputs (list r-forecast
                             r-fracdiff
                             r-furrr
                             r-future
                             r-purrr
                             r-rcpproll
                             r-tibble
                             r-tseries
                             r-urca))
    (native-inputs (list r-knitr))
    (home-page "https://pkg.robjhyndman.com/tsfeatures/")
    (synopsis "Time Series Feature Extraction")
    (description
     "Methods for extracting various features from time series data.  The features
  provided are those from Hyndman, Wang and Laptev (2013)
  <doi:10.1109/ICDMW.2015.104>, Kang, Hyndman and Smith-Miles (2017)
  <doi:10.1016/j.ijforecast.2016.09.004> and from Fulcher, Little and Jones (2013)
  <doi:10.1098/rsif.2013.0048>.  Features include spectral entropy,
  autocorrelations, measures of the strength of seasonality and trend, and so on.
  Users can also define their own feature functions.")
    (license gpl3)))

(define-public r-nycflights13
  (package
    (name "r-nycflights13")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "nycflights13" version))
              (sha256
               (base32
                "07aa76c77pm1wpzkwjmzv8n6ir5i6fxawx8wx586gwc5wajcb1qf"))))
    (properties `((upstream-name . "nycflights13")))
    (build-system r-build-system)
    (propagated-inputs (list r-tibble))
    (home-page "https://github.com/hadley/nycflights13")
    (synopsis "Flights that Departed NYC in 2013")
    (description
     "Airline on-time data for all flights departing NYC in 2013.  Also includes useful 'metadata' on airlines, airports, weather, and planes.")
    (license #f)))

(define-public r-gapminder
  (package
    (name "r-gapminder")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gapminder" version))
              (sha256
               (base32
                "067cra1ca4ngwjx8d1y9pyzwcpsfi1wcal0glzyy6ghd1k6jflpv"))))
    (properties `((upstream-name . "gapminder")))
    (build-system r-build-system)
    (propagated-inputs (list r-tibble))
    (home-page "https://github.com/jennybc/gapminder")
    (synopsis "Data from Gapminder")
    (description
     "An excerpt of the data available at Gapminder.org.  For each of 142 countries, the package provides values for life expectancy, GDP per capita, and population, every five years, from 1952 to 2007.")
    (license #f)))

(define-public r-lahman
  (package
    (name "r-lahman")
    (version "9.0-0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "Lahman" version))
              (sha256
               (base32
                "0q6whyqlxdkm17i6hichnd89y3pnjc0xc5aabdanf1i4lpv5vynr"))))
    (properties `((upstream-name . "Lahman")))
    (build-system r-build-system)
    (propagated-inputs (list r-dplyr))
    (native-inputs (list r-knitr))
    (home-page "https://CRAN.R-project.org/package=Lahman")
    (synopsis "Sean 'Lahman' Baseball Database")
    (description
     "This package provides the tables from the 'Sean Lahman Baseball Database' as a set of R data.frames.  It uses the data on pitching, hitting and fielding performance and other tables from 1871 through 2019, as recorded in the 2020 version of the database.  Documentation examples show how many baseball questions can be investigated.")
    (license (list gpl2+ gpl3+))))
