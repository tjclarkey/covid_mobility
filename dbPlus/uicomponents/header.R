hdr <- dashboardHeaderPlus(
  enable_rightsidebar = TRUE,
  fixed = FALSE,
  rightSidebarIcon = "gears",
  left_menu = tagList(
    dropdownBlock(
      id = "dropdown1",
      title = "Dropdown 1",
      icon = icon("sliders"),
      sliderInput(
        inputId = "in_dropdown1",
        label = "Number of Observations",
        min = 10, max = 100, value = 65
      ),
      prettyToggle(
        inputId = "in_toggle",
        label_on = "Include Region Average",
        label_off = "Exclude Region Average",
        icon_on = icon("check"),
        icon_off = icon("remove")
      )
    )
  )
)
