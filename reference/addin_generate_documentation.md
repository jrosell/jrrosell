# Adding to generate documentation

It changes the selected code for the the generated documentation using
the configured ollama model.

## Usage

``` r
addin_generate_documentation(
  model = "qwen2.5-coder:3b",
  context = rstudioapi::getActiveDocumentContext()
)
```

## Arguments

- model:

  A single string with the ollama model to use.

- context:

  the IDE context. Defaults to rstudioapi::getActiveDocumentContext

## Value

Nothing.
