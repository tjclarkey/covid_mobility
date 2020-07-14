body <- dashboardBody(
  fluidRow(
    gradientBox(
      title = "Graphic 1",
      icon = "fa fa-th",
      gradientColor = "teal",
      boxToolSize = "xs", 
      footer = selectInput(
        "country_g1", "Select A Country:", countries, "United Kingdom", 
        selectize = TRUE
      ),
      plotOutput("graphic1")
    )
  ),
  fluidRow(
    boxPlus(
      title = "Closable Box with dropdown", 
      closable = TRUE, 
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_dropdown = TRUE,
      dropdown_icon = "wrench",
      dropdown_menu = dropdownItemList(
        dropdownItem(url = "https://www.google.com", name = "Link to google"),
        dropdownItem(url = "#", name = "item 2"),
        dropdownDivider(),
        dropdownItem(url = "#", name = "item 3")
      ),
      plotOutput("histU")
    ),
    gradientBox(
      title = "My gradient Box",
      icon = "fa fa-th",
      gradientColor = "teal",
      width = 6,
      boxToolSize = "xs", 
      footer = sliderInput(
        "obs", 
        "Number of observations:",
        min = 0, max = 1000, value = 500, width = "100%"
      ),
      "This is a gradient box",
      plotOutput("histL")
    )
  )
)
  

