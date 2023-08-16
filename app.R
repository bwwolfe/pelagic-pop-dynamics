### app hosted as of 2023-08-17 at https://recfish.pelagic-movement.cloud.edu.au/shiny/ksm723/

library(shiny)
if( !require(deSolve)) install.packages("deSolve")
if( !require(ggpmisc)) install.packages("ggpmisc")

library(deSolve)
library(ggplot2)
library(tidyr)
library(ggpmisc)
annotate_npc_abs <- function(label, x, y, ...) 
{
  grid::grid.draw(grid::textGrob(
    label, x = unit(x, "npc"), y = unit(y, "npc"), ...))
}

# Define UI
ui <- fluidPage(
  titlePanel("Marine predator population dynamics"),
  sidebarLayout(
    sidebarPanel(
      "Tunas (prey)",
      #sliderInput("R", "Starting population of Tunas", value = 100, min = 0, max = 5000),
      sliderInput("K", "Carrying Capacity of Tunas", value = 10000, min = 100, max = 10000),
      sliderInput("r", "Growth Rate of Tunas (r)", value = 0.1, min = 0, max = 1),
      hr(),
      "Seals (tuna predators)",
      sliderInput("N", "Starting population of Seals", value = 50, min = 0, max = 500),
      sliderInput("a", "Attack Rate of Seals on Tunas (a)", value = 0.008, min = 0, max = 0.02),
      #sliderInput("q", "Mortality Rate of Seals (q)", value = 0.05, min = 0, max = 1),
      hr(),
      "White Sharks (seal predators)",
      sliderInput("P", "Starting population of White Sharks", value = 20, min = 0, max = 100),
      sliderInput("e", "Attack Rate of White Sharks on Seals (e)", value = 0.012, min = 0, max = 0.05),
      #sliderInput("b", "Mortality Rate of White Sharks (b)", value = 0.2, min = 0, max = 1),
      sliderInput("time", "Simulation time (years)", min = 10, value = 100, max = 1000),
      width = 3
    ),
    mainPanel(
      plotOutput("plot"),#tableOutput("dat"), #used this for trblshting,
      # h3("Background"),
      # div(span("In this marine ecosystem, there are three trophic levels of predators:"),br(),
      #     strong("Tunas"), span("are at the bottom, and they in turn rely on a finite supply of food available,
      #   so the ecosystem can support a fixed number of them in the absence of predation."),
      #   br(),
      #   strong("Seals"), span("eat tunas."), br(),
      #   strong("White sharks"), span("eat seals."),
      #   br(), p("There are 1000 tunas at the start of the simulation. If any of the populations drop below 1 individual,
      #         the population goes extinct.")),
      width = 9)
  )
)

# Define server
server <- function(input, output) {
  f <- 0.1
  z <- 0.1
  q <- 0.05
  b <- 0.05
  R <- 2500
  # R = 100
  # N = 50
  # P = 20
  # r = .3
  # a = 0.008
  # q = 0.05
  # e = 0.01
  # b = 0.2
  # time = 1000
  # # 
  # input <- reactiveValues("R" = R,"N" = N,"P" = P,"r" = r,"a"=a,"q"=q,"e"=e,"b"=b,"time"=time,"f" = f)
  #parms <- c(r,a,f,q,e,b,z)
  
  ## Model function
  lv3 <- function(t, start, parms) {
    with(as.list(c(parms, start)), {
      dR <-  r * R * (1 - R/K) - a * N * R
      dN <- f * a * N * R - q * N - e * N * P
      dP <-  z * e * N * P - b * P
      list(c(dR, dN, dP))
    })
  }
  
  eventFun <- function(t, y, p) {
    if (all(y <= 1)) {
      y <- rep(0, length(y)) 
    }
    if(any(y < 1)) y[y < 1] <-0
    return(y)
  }
  
  
  ## Root function
  
  rootFun <- function(t, y, p) {
    roots <- y < 1  
    y[roots] <- -1  
    return(y)
  }
  
  
  ## Simulation function
  lv3_sim <- function(start, parms, time) {
    sim <- as.data.frame(lsoda(start, time, lv3, parms, maxsteps = 50000,
                               events = list(func = eventFun, root = TRUE),
                               rootfunc = rootFun))
    colnames(sim) <- c("time", "Tuna", "Seal", "White Shark")
    return(sim)
  }
  output$dat <-
    renderTable({
      parms <- c(r = input$r, a = input$a, f = f, K = input$K,#input$f,  
                 q = input$q, e = input$e, z = z,#input$z,
                 b = input$b)
      start <- c(R = input$R, N = input$N, P = input$P)
      time <- seq(0, input$time, by = 1)
      
      lv3_sim(start, parms, time)
    })
  
  output$plot <- renderPlot({
    parms <- c(r = input$r, a = input$a, f = f, K = input$K,#input$f,  
               q = input$q, e = input$e, z = z,#input$z,
               b = input$b)
    start <- c(R = R,#input$R,
               N = input$N, P = input$P)
    time <- seq(0, input$time, by = 1)
    
    sim_data <- lv3_sim(start, parms, time)
    
    sim_data_long <-
      pivot_longer(sim_data, 
                   cols = c("Tuna", "Seal", "White Shark"),
                   values_to = "N",
                   names_to = "TrophicLevel") |>
      transform(TrophicLevel =
                  factor(TrophicLevel,
                         levels = c("Tuna", "Seal", "White Shark"),
                         ordered = TRUE)
      )
    egg::ggarrange(plots = mapply(\(spp, col, ymax){
      
      plodat<- subset(sim_data_long, TrophicLevel == spp)
      
      if(any(plodat$N==0)) {
        stink <- geom_point(data = plodat[which(plodat$N==0)[1], ],
                            aes(time,N), size = 5, shape = 4, stroke = 2, colour = "red4")
        plodat <- plodat[1:(which(plodat$N==0)[1]),]
      }else stink <- geom_blank()
      
      ggplot(plodat) +
        geom_line(aes(x = time, y = N), colour = col,
                  linewidth = 1) +
        #scale_y_continuous(trans = "log10") +
        #scale_colour_manual(values = c("#210124", "#e6e6ea", "#d95d39", "#750d37", "#477890")[c(1,3,5)]) +
        labs(x = ifelse(plodat$TrophicLevel[1] == "White Shark", "Time", ""), y = "Population size") +
        theme_bw(16) +
        theme(plot.margin= margin(0,3,0,1,"lines")) + 
        #scale_y_continuous(limits = c(0, ymax), expand = c(0.02,0, 0.2,0)) +
        coord_cartesian(ylim = c(0,ymax), expand = TRUE, clip = "off") + 
        scale_x_continuous(limits = c(0,input$time), expand = c(0.01,0)) + 
        facet_wrap(vars(TrophicLevel), ncol = 1, scales = "free_y") + stink +
        annotate("text_npc", label = paste0("N=",floor(plodat$N[nrow(plodat)])), npcx = 1.05, npcy = 0.5 )
    },
    spp = c("Tuna", "Seal", "White Shark"),
    col = c("#d95d39", "#310062", "#477890"),
    ymax = c(10000,250,150),
    SIMPLIFY = FALSE),
    ncol = 1,
    #labels = c("Tuna", "Seal", "White Shark")
    )
  }, height = 650, width = 1000 )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
