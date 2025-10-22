# ------------------------------------------------------------------------------
# page_controls.R
#
# This file defines the 'page controls' UI component.
#
# ------------------------------------------------------------------------------

# Threshold input UI component
threshold_input <- tags$div(
  id = "threshold_container",
  style = "",
  tags$label("Threshold", `for` = "threshold", style = "margin-right: 0.5em; margin-bottom: 0;"),
  numericInput(
    inputId = "threshold",
    label = NULL,
    value = 0.5,
    min = 0.1,
    max = 1,
    step = 0.001,
    width = "7em"
  ),
  tags$small(
    "Range: 0.01 to 1.00",
    style = "display: block; margin-top: 0.5em; margin-left: 0.5em; color: #6c757d;" # Hint styling
  )
)

# Add horizontal bar above the main panel
page_controls <- tags$div(
  class = "card shadow rounded p-3 mb-4", # Apply same styling as main panel
  threshold_input
)
