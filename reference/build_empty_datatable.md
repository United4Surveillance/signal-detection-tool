# Create an Empty DataTable with a Custom Message

This function generates a minimal \`DT::datatable\` displaying a custom
message. It is useful for placeholder content when there is no data to
display.

## Usage

``` r
build_empty_datatable(message)
```

## Arguments

- message:

  A character string specifying the message to display in the table.

## Value

A \`DT::datatable\` object containing a single-row, single-column table
with the custom message.

## Details

The resulting DataTable will have the following features: - No column
headers. - No search box, pagination, or sorting functionality. - A
single-row table displaying the provided message.

## Examples

``` r
# Create an empty DataTable with a custom message
build_empty_datatable("No data available")

{"x":{"filter":"none","vertical":false,"data":[["No data available"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","paging":false,"ordering":false,"columnDefs":[{"orderable":false,"targets":0},{"name":"Message","targets":0}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}
```
