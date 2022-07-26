sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro", icon = icon("info")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"))
  )
)