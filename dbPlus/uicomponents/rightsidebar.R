# This R Script contains the composition of the right side bar. The right side bar contains three different tabs.

rsb <- rightSidebar(
  background = "dark",
  rightSidebarTabContent(
    id = 1,
    title = "Tab 1",
    icon = "desktop",
    active = TRUE,
    sliderInput(
      "obs",
      "Number of observations:",
      min = 0, max = 1000, value = 500
    )
  ),
  rightSidebarTabContent(
    id = 2,
    title = "Tab 2",
    icon = "star",
    textInput("caption", "Caption", "Data Summary")
  ),
  rightSidebarTabContent(
    id = 3,
    icon = "paint-brush",
    title = "Tab 3",
    numericInput("obs", "Observations:", 10, min = 1, max = 100)
  )
)

