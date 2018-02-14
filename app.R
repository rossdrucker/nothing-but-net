library(shiny)
library(ggplot2)
library(png)
library(grid)
library(raster)
library(magick)

colors = read.csv("data/School Colors.csv", stringsAsFactors = FALSE)

create_circle = function(center = c(0, 0), npoints = 500, diameter = 1, start = 0, end = 2){
  pts = seq(start * pi, end * pi, length.out = npoints)
  data.frame(x = center[1] + ((diameter/2) * cos(pts)),
             y = center[2] + ((diameter/2) * sin(pts)))
}

# Function needed to get opposite side of court
# (court is 94 feet long)
reverse_y = function(y){
  94 - y
}

# Put coordinates into a data frame for easy use in
# ggplot functions
new_coords = function(x, y, group, description){
  new_coords_df = data.frame(x = x, y = y)
  new_coords_df$group = group
  new_coords_df$side = 1
  group = group + 1
  
  new_coords_df2 = data.frame(x = x, y = reverse_y(y))
  new_coords_df2$group = group
  new_coords_df2$side = 2
  group <<- group + 1
  
  new_coords_df = rbind(new_coords_df, new_coords_df2)
  new_coords_df$description = description
  
  return(new_coords_df)
}

# Rotate court
court_rotator = function(court){
  rotated_court = court
  rotated_court$x = pi * (court$x/180)
  rotated_court$y = pi * (court$y/180)
  rotation_matrix = matrix(
    c(cos(pi/2), sin(pi/2),
      -sin(pi/2), cos(pi/2)
    ),
    ncol = 2
  )
  rotated_coords = apply(rotated_court[, c("x", "y")], 1, function(x) x %*% rotation_matrix)
  rotated_court$x = rotated_coords[1, ]
  rotated_court$y = rotated_coords[2, ]
  rotated_court$x = 180 * (rotated_court$x/pi)
  rotated_court$y = 180 * (rotated_court$y/pi)
  return(rotated_court)
}

# Restricted Area
circle_ra_in = create_circle(center = c(25, 5 + (3/12)), diameter = 8)
circle_ra_out = create_circle(center = c(25, 5 + (3/12)), diameter = (4 + (2/12)) * 2)

# Three Point Arc
circle_3pt_in = create_circle(center = c(25, 5 + (3/12)), diameter = (20 + (7/12)) * 2)
circle_3pt_out = create_circle(center = c(25, 5 + (3/12)), diameter = (20 + (9/12)) * 2)

# Center Circle
circle_mid_in = create_circle(center = c(25, 47), diameter = (6 - (2/12)) * 2)
circle_mid_out = create_circle(center = c(25, 47), diameter = 6 * 2)

# Free Throw Circle
circle_ft_in = create_circle(center = c(25, 19), diameter = (6 - (2/12)) * 2)
circle_ft_out = create_circle(center = c(25, 19), diameter = 6 * 2)

# Hoop
circle_hoop = create_circle(center = c(25, 5 + (3/12)), diameter = 1.5)

group = 1

# Create baseline
portrait_court = new_coords(
  x = c(-2/12, -2/12, 50 + (2/12), 50 + (2/12)),
  y = c(-2/12, 0, 0, -2/12),
  group = group,
  description = "Baseline"
)

# Create and add left line
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(-2/12, -2/12, 0, 0),
                         y = c(0, 47 - (1/12), 47 - (1/12), 0),
                         group = group,
                         description = "Left Line"
                       )
)

# Create and add right line
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(50, 50, 50 + (2/12), 50 + (2/12)),
                         y = c(0, 47 - (1/12), 47 - (1/12), 0),
                         group = group,
                         description = "Right Line"
                       )
)

# Create and add coach's box
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(-3, -3, 3, 3),
                         y = c(28, 28 + (2/12), 28 + (2/12), 28),
                         group = group,
                         description = "Coach's Box"
                       )
)

# Create and add three point line -- bottom left
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(4 + (3/12), 4 + (3/12), 4 + (3/12) + (2/12), 4 + (3/12) + (2/12)),
                         y = c(0, 5 + (3/12), 5 + (3/12), 0),
                         group = group,
                         description = "Three Point -- Bottom Left"
                       )
)

# Create and add paint -- bottom right
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(45 + (9/12) - (2/12), 45 + (9/12) - (2/12), 45 + (9/12), 45 + (9/12)),
                         y = c(0, 5 + (3/12), 5 + (3/12), 0),
                         group = group,
                         description = "Paint -- Bottom Right"
                       )
)

# Create and add paint -- bottom left
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19, 19, 19 + 2/12, 19 + 2/12),
                         y = c(0, 19, 19, 0),
                         group = group,
                         description = "Free Throw Line -- Bottom Left"
                       )
)

# Create and add paint -- bottom right
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(31 - (2/12), 31 - (2/12), 31, 31),
                         y = c(0, 19, 19, 0),
                         group = group,
                         description = "Paint -- Bottom Right"
                       )
)

# Create and add free throw line
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19, 19, 31, 31),
                         y = c(19 - (2/12), 19, 19, 19 - (2/12)),
                         group = group,
                         description = "Free Throw Line"
                       )
)

# Create and add backboard
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(22, 22, 28, 28),
                         y = c(4 - (2/12), 4, 4, 4 - (2/12)),
                         group = group,
                         description = "Backboard"
                       )
)

# Create and add three point arch
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(circle_3pt_out$x[1:250], rev(circle_3pt_in$x[1:250])),
                         y = c(circle_3pt_out$y[1:250], rev(circle_3pt_in$y[1:250])),
                         group = group,
                         description = "Three Point Arch"
                       )
)

# Create and add restricted area
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(circle_ra_out$x[1:250], rev(circle_ra_in$x[1:250])),
                         y = c(circle_ra_out$y[1:250], rev(circle_ra_in$y[1:250])),
                         group = group,
                         description = "Restricted Area"
                       )
)

# Create and add top of free throw circle
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(circle_ft_out$x[1:250], rev(circle_ft_in$x[1:250])),
                         y = c(circle_ft_out$y[1:250], rev(circle_ft_in$y[1:250])),
                         group = group,
                         description = "Top of Free Throw Circle"
                       )
)

# Create and add lowest left block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19 - (8/12), 19 - (8/12), 19, 19),
                         y = c(7, 8, 8, 7),
                         group = group,
                         description = "Lowest Left Block"
                       )
)

# Create and add second left block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19 - (8/12), 19 - (8/12), 19, 19),
                         y = c(11, 11 + (2/12), 11 + (2/12), 11),
                         group = group,
                         description = "Second Left Block"
                       )
)

# Create and add third left block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19 - (8/12), 19 - (8/12), 19, 19),
                         y = c(14 + (2/12), 14 + (4/12), 14 + (4/12), 14 + (2/12)),
                         group = group,
                         description = "Third Left Block"
                       )
)

# Create and add fourth left block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(19 - (8/12), 19 - (8/12), 19, 19),
                         y = c(17 + (4/12), 17 + (6/12), 17 + (6/12), 17 + (4/12)),
                         group = group,
                         description = "Fourth Left Block"
                       )
)

# Create and add lowest right block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(31 + (8/12), 31 + (8/12), 31, 31),
                         y = c(7, 8, 8, 7),
                         group = group,
                         description = "Lowest right Block"
                       )
)

# Create and add second right block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(31 + (8/12), 31 + (8/12), 31, 31),
                         y = c(11, 11 + (2/12), 11 + (2/12), 11),
                         group = group,
                         description = "Second Right Block"
                       )
)

# Create and add third right block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(31 + (8/12), 31 + (8/12), 31, 31),
                         y = c(14 + (2/12), 14 + (4/12), 14 + (4/12), 14 + (2/12)),
                         group = group,
                         description = "Third Right Block"
                       )
)

# Create and add fourth right block
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(31 + (8/12), 31 + (8/12), 31, 31),
                         y = c(17 + (4/12), 17 + (6/12), 17 + (6/12), 17 + (4/12)),
                         group = group,
                         description = "Fourth Right Block"
                       )
)

# Create and add time line
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(-(2/12), -(2/12), 50 + (2/12), 50 + (2/12)),
                         y = c(47 - (1/12), 47, 47, 47 - (1/12)),
                         group = group,
                         description = "Time Line"
                       )
)

# Create and add center circle
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = c(circle_mid_out$x[250:500], rev(circle_mid_in$x[250:500])),
                         y = c(circle_mid_out$y[250:500], rev(circle_mid_in$y[250:500])),
                         group = group,
                         description = "Center Circle"
                       )
)

# Create and add hoop
portrait_court = rbind(portrait_court,
                       new_coords(
                         x = circle_hoop$x,
                         y = circle_hoop$y,
                         group = group,
                         description = "Hoop"
                       )
)

landscape_court = court_rotator(court = portrait_court)

portrait_court_nc = portrait_court[portrait_court$description != "Center Circle", ]

landscape_court_nc = landscape_court[landscape_court$description != "Center Circle", ]

ui = pageWithSidebar(
  headerPanel("Pass PlottR"),
  
  sidebarPanel(
    radioButtons("orientation", "Court Orientation:", c("Landscape", "Portrait", "Half")),
    
    selectInput(
      "home_team",
      "Home Team:",
      c("", "Alabama", "Arizona", "Arizona State", "Arkansas", "Auburn", "Baylor",
        "Boston College", "California", "Clemson", "Colorado", "Duke", "Florida",
        "Florida State", "Georgia", "Georgia Tech", "Illinois", "Indiana", "Iowa",
        "Iowa State", "Kansas", "Kansas State", "Kentucky", "Louisville", "LSU",
        "Maryland", "Miami", "Michigan", "Michigan State", "Minnesota", "Mississippi State",
        "Mizzou", "NC State", "Nebraska", "North Carolina", "Northwestern", "Notre Dame",
        "Ohio State", "Oklahoma", "Oklahoma State", "Ole Miss", "Oregon", "Oregon State",
        "Penn State", "Pittsburgh", "Purdue", "Rutgers", "South Carolina", "Stanford",
        "Syracuse", "TCU", "Tennessee", "Texas", "Texas A&M", "Texas Tech", "UCLA", "USC",
        "Utah", "Vanderbilt", "Virginia", "Virginia Tech", "Wake Forest", "Washington",
        "Washington State", "West Virginia", "Wisconsin"
      )
    ),
    
    selectInput(
      "away_team",
      "Away Team:",
      c("", "Alabama", "Arizona", "Arizona State", "Arkansas", "Auburn", "Baylor",
        "Boston College", "California", "Clemson", "Colorado", "Duke", "Florida",
        "Florida State", "Georgia", "Georgia Tech", "Illinois", "Indiana", "Iowa",
        "Iowa State", "Kansas", "Kansas State", "Kentucky", "Louisville", "LSU",
        "Maryland", "Miami", "Michigan", "Michigan State", "Minnesota", "Mississippi State",
        "Mizzou", "NC State", "Nebraska", "North Carolina", "Northwestern", "Notre Dame",
        "Ohio State", "Oklahoma", "Oklahoma State", "Ole Miss", "Oregon", "Oregon State",
        "Penn State", "Pittsburgh", "Purdue", "Rutgers", "South Carolina", "Stanford",
        "Syracuse", "TCU", "Tennessee", "Texas", "Texas A&M", "Texas Tech", "UCLA", "USC",
        "Utah", "Vanderbilt", "Virginia", "Virginia Tech", "Wake Forest", "Washington",
        "Washington State", "West Virginia", "Wisconsin"
      )
    ),
    
    radioButtons("playType", "Type of Play", c("Pass", "Shot")),
    
    radioButtons("eventTeam", "Which team?", c("Home", "Away"))
    #verbatimTextOutput("info")
  ),
  
  mainPanel(
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info"),
    actionButton("update", "Add Event")
  )
)

server = function(input, output){
  
  output$plot1 = renderPlot({
    if(input$orientation == "Landscape"){
      if(input$home_team == ""){
        ggplot(data = landscape_court, aes(x = x, y = y + 50, group = group)) +
          geom_rect(xmin = 0, xmax = 94, ymin = 0, ymax = 50, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", "000000", sep = ""), 
            fill = paste("#", "ffffff", sep = "")
          ) + 
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          coord_equal() +
          xlab("") +
          ylab("")
      }
      else{
        logo = readPNG(paste("Logos/", input$home_team, ".png", sep = ""))
        logo = rasterGrob(logo, interpolate = TRUE)
        
        ggplot(data = landscape_court_nc, aes(x = x, y = y + 50, group = group)) +
          geom_rect(xmin = 0, xmax = 94, ymin = 0, ymax = 50, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", colors$primary[colors$schools == input$home_team], sep = ""),
            fill = paste("#", colors$secondary[colors$schools == input$home_team], sep = "")
          ) +
          coord_equal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          xlab("") +
          ylab("") +
          annotation_custom(
            logo,
            xmin = 39,
            xmax = 55,
            ymin = 17,
            ymax = 33
          )
      }
    }
    else if(input$orientation == "Portrait"){
      if(input$home_team == ""){
        ggplot(data = court, aes(x = x, y = y, group = group)) +
          geom_rect(xmin = 0, xmax = 50, ymin = 0, ymax = 94, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", "000000", sep = ""), 
            fill = paste("#", "ffffff", sep = "")
          ) + 
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          coord_equal() +
          xlab("") +
          ylab("")
      }
      else{
        logo = image_read(paste("Logos/", input$home_team, ".png", sep = ""))
        logo = image_rotate(logo,-90)
        logo = rasterGrob(logo, interpolate = TRUE)
        
        ggplot(data = portrait_court_nc, aes(x = x, y = y, group = group)) +
          geom_rect(xmin = 0, xmax = 50, ymin = 0, ymax = 94, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", colors$primary[colors$schools == input$home_team], sep = ""),
            fill = paste("#", colors$secondary[colors$schools == input$home_team], sep = "")
          ) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          coord_equal() +
          xlab("") +
          ylab("") +
          annotation_custom(
            logo,
            xmin = 17,
            xmax = 33,
            ymin = 39,
            ymax = 55
          )
      }
    }
    else if(input$orientation == "Half"){
      if(input$home_team == ""){
        ggplot(data = portrait_court[portrait_court$side == 1, ], aes(x = x, y = y, group = group)) + 
          geom_rect(xmin = 0, xmax = 50, ymin = 0, ymax = 47, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", "000000", sep = ""),
            fill = paste("#", "ffffff", sep = "")
          ) +
          coord_equal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          xlab("") + 
          ylab("")
      }
      else{
        ggplot(data = portrait_court[portrait_court$side == 1, ], aes(x = x, y = y, group = group)) + 
          geom_rect(xmin = 0, xmax = 50, ymin = 0, ymax = 47, fill = "#d2ab6f") +
          geom_polygon(
            col = paste("#", colors$primary[colors$schools == input$home_team], sep = ""),
            fill = paste("#", colors$secondary[colors$schools == input$home_team], sep = "")
          ) +
          coord_equal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
          ) +
          xlab("") + 
          ylab("")
      }
    }
  })
  
  output$info = renderText({
    paste0("x = ", input$plot_click$x, "\ny = ", input$plot_click$y)
  })
}

shinyApp(ui, server)
