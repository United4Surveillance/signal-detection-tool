# Wrapper around the algo.cusum_with_reset Copied from the code of the surveillance package and adapted for algo.cusum_with_reset

Wrapper around the algo.cusum_with_reset Copied from the code of the
surveillance package and adapted for algo.cusum_with_reset

## Usage

``` r
cusum_with_reset(
  sts,
  control = list(range = range, k = 1.04, h = 2.26, m = NULL, trans = "standard", alpha =
    NULL),
  ...
)
```

## Arguments

- sts:

  surveillance::sts() format of aggregated timeseries of case counts

- control:

  control object:

  `range`

  :   determines the desired time points which should be evaluated

  `k`

  :   is the reference value

  `h`

  :   the decision boundary

  `m`

  :   how to determine the expected number of cases – the following
      arguments are possible

      `numeric`

      :   a vector of values having the same length as `range`. If a
          single numeric value is specified then this value is
          replicated `length(range)` times.

      `NULL`

      :   A single value is estimated by taking the mean of all
          observations previous to the first `range` value.

      `"glm"`

      :   A GLM of the form \$\$\log(m_t) = \alpha + \beta t +
          \sum\_{s=1}^S (\gamma_s \sin(\omega_s t) + \delta_s
          \cos(\omega_s t)),\$\$ where \\\omega_s = \frac{2\pi}{52}s\\
          are the Fourier frequencies is fitted. Then this model is used
          to predict the `range` values.

  `trans`

  :   one of the following transformations (warning: Anscombe and NegBin
      transformations are experimental)

      `rossi`

      :   standardized variables z3 as proposed by Rossi

      `standard`

      :   standardized variables z1 (based on asymptotic normality) -
          This is the default.

      `anscombe`

      :   anscombe residuals – experimental

      `anscombe2nd`

      :   anscombe residuals as in Pierce and Schafer (1986) based on
          2nd order approximation of E(X) – experimental

      `pearsonNegBin`

      :   compute Pearson residuals for NegBin – experimental

      `anscombeNegBin`

      :   anscombe residuals for NegBin – experimental

      `none`

      :   no transformation

  `alpha`

  :   parameter of the negative binomial distribution, s.t. the variance
      is \\m+\alpha \*m^2\\

  `reset`

  :   logical: Should the CUSUM statistic be reset to 0 immediately
      after an alarm? This is the traditional form of the chart as used
      in industrial process control, but not the default choice in
      outbreak detection when continuous periods of abnormal disease
      activity should be flagged.

- ...:

  any additional parameters which should be passed onto
  [wrap.algo](https://rdrr.io/pkg/surveillance/man/wrap.algo.html)
