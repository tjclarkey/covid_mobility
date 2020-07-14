body1 <- dashboardBody(
  fluidRow(
    box(
      width = 8,
      fluidRow(
        column(
          width = 7,
          selectInput(
            "country_g1", "Select A Country:", countries, "United Kingdom", 
            selectize = TRUE
          )),
        column(
          width = 5,
          selectInput(
            "type_g1", "Select Mobility Type:", 
            c("Residential" = "Residential",
              "Retail and Recreation" = "Recreation_and_Retail",
              "Parks" = "Parks",
              "Groceries and Pharmacy" = "Groceries_and_Pharmacies",
              "Transit Stations" = "Transit_Stations",
              "Workplaces" = "Workplaces"),
            "Residential"
          )
        )),
        fluidRow(
          plotOutput("graphic1")
        )
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