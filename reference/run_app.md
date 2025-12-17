# Run the Shiny Application

Run the Shiny Application

## Usage

``` r
run_app(
  path_to_yaml = NULL,
  config_set = "default",
  onStart = function() {
     cat("Warnings are turned off\n")
     options(warn = -1)
 
       get_shp_config_or_internal()
     onStop(function() {
         options(warn = 0)
 
           cat("Warnings turned on\n")
     })
 },
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
```

## Arguments

- path_to_yaml:

  A character string specifying the file path to the .yml configuration
  file on the user's computer. If no external .yml file is used, set
  this parameter to NULL.

- config_set:

  A character string specifying the name of the configuration to read
  from the .yml file.

- onStart:

  A function that will be called before the app is actually run. This is
  only needed for `shinyAppObj`, since in the `shinyAppDir` case, a
  `global.R` file can be used for this purpose.

- options:

  Named options that should be passed to the `runApp` call (these can be
  any of the following: "port", "launch.browser", "host", "quiet",
  "display.mode" and "test.mode"). You can also specify `width` and
  `height` parameters which provide a hint to the embedding environment
  about the ideal height/width for the app.

- enableBookmarking:

  Can be one of `"url"`, `"server"`, or `"disable"`. The default value,
  `NULL`, will respect the setting from any previous calls to
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html).
  See
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html)
  for more information on bookmarking your app.

- uiPattern:

  A regular expression that will be applied to each `GET` request to
  determine whether the `ui` should be used to handle the request. Note
  that the entire request path must match the regular expression in
  order for the match to be considered successful.

- ...:

  arguments to pass to golem_opts. See \`?golem::get_golem_options\` for
  more details.
