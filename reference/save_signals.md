# Save signals

Save the processed results to a CSV file and return codes and messages
for application use.

## Usage

``` r
save_signals(signals, original_input_data, filepath = "")
```

## Arguments

- signals:

  The processed results data frame.

- original_input_data:

  The original input data used for analysis.

- filepath:

  The optional filepath to save the results. If not provided, a filename
  is generated.

## Value

A list containing success status (TRUE or FALSE) and a message (NULL for
success, a warning, or an error).

## Examples

``` r
# Save signals with default or custom filepath
if (FALSE) { # \dontrun{
data <- preprocess_data(SignalDetectionTool::input_example)
save_signals(SignalDetectionTool::get_signals(data), data)
} # }
```
