#####################################################################################
#####################################################################################
##                                                                                 ##
##  This script produces a ggplot version of a regulation college basketball       ##
##  court. Each unit in x and y is equivalent to one foot (12 in) and all parts of ##
##  the model are drawn to scale. This will be used to create an interface for     ##
##  point-and-click pass tracking for data collection                              ##
##                                                                                 ##
#####################################################################################
#####################################################################################

library(ggplot2)

# Function to draw circle in ggplot
create_circle = function(center = c(0, 0),
                         npoints = 500,
                         diameter = 1,
                         start = 0,
                         end = 2) {
  pts = seq(start * pi, end * pi, length.out = npoints)
  data.frame(x = center[1] + ((diameter/2) * cos(pts)),
             y = center[2] + ((diameter/2) * sin(pts)))
}

# Function needed to get opposite side of court
# (court is 94 feet long)
reverse_y = function(y) {
  94 - y
}

# Put coordinates into a data frame for easy use in
# ggplot functions
new_coords = function(x, y, group, description) {
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
court = new_coords(
  x = c(-2/12, -2/12, 50 + (2/12), 50 + (2/12)),
  y = c(-2/12, 0, 0, -2/12),
  group = group,
  description = "Baseline"
)

# Create and add left line
court = rbind(court,
              new_coords(
                x = c(-2/12, -2/12, 0, 0),
                y = c(0, 47 - (1/12), 47 - (1/12), 0),
                group = group,
                description = "Left Line"
              )
)

# Create and add right line
court = rbind(court,
              new_coords(
                x = c(50, 50, 50 + (2/12), 50 + (2/12)),
                y = c(0, 47 - (1/12), 47 - (1/12), 0),
                group = group,
                description = "Right Line"
              )
)

# Create and add coach's box
court = rbind(court,
              new_coords(
                x = c(-3, -3, 3, 3),
                y = c(28, 28 + (2/12), 28 + (2/12), 28),
                group = group,
                description = "Coach's Box"
              )
)

# Create and add three point line -- bottom left
court = rbind(court,
              new_coords(
                x = c(4 + (3/12), 4 + (3/12), 4 + (3/12) + (2/12), 4 + (3/12) + (2/12)),
                y = c(0, 5 + (3/12), 5 + (3/12), 0),
                group = group,
                description = "Three Point -- Bottom Left"
              )
)

# Create and add paint -- bottom right
court = rbind(court,
              new_coords(
                x = c(45 + (9/12) - (2/12), 45 + (9/12) - (2/12), 45 + (9/12), 45 + (9/12)),
                y = c(0, 5 + (3/12), 5 + (3/12), 0),
                group = group,
                description = "Paint -- Bottom Right"
              )
)

# Create and add paint -- bottom left
court = rbind(court,
              new_coords(
                x = c(19, 19, 19 + 2/12, 19 + 2/12),
                y = c(0, 19, 19, 0),
                group = group,
                description = "Free Throw Line -- Bottom Left"
              )
)

# Create and add paint -- bottom right
court = rbind(court,
              new_coords(
                x = c(31 - (2/12), 31 - (2/12), 31, 31),
                y = c(0, 19, 19, 0),
                group = group,
                description = "Paint -- Bottom Right"
              )
)

# Create and add free throw line
court = rbind(court,
              new_coords(
                x = c(19, 19, 31, 31),
                y = c(19 - (2/12), 19, 19, 19 - (2/12)),
                group = group,
                description = "Free Throw Line"
              )
)

# Create and add backboard
court = rbind(court,
              new_coords(
                x = c(22, 22, 28, 28),
                y = c(4 - (2/12), 4, 4, 4 - (2/12)),
                group = group,
                description = "Backboard"
              )
)

# Create and add three point arch
court = rbind(court,
              new_coords(
                x = c(circle_3pt_out$x[1:250], rev(circle_3pt_in$x[1:250])),
                y = c(circle_3pt_out$y[1:250], rev(circle_3pt_in$y[1:250])),
                group = group,
                description = "Three Point Arch"
              )
)

# Create and add restricted area
court = rbind(court,
              new_coords(
                x = c(circle_ra_out$x[1:250], rev(circle_ra_in$x[1:250])),
                y = c(circle_ra_out$y[1:250], rev(circle_ra_in$y[1:250])),
                group = group,
                description = "Restricted Area"
              )
)

# Create and add top of free throw circle
court = rbind(court,
              new_coords(
                x = c(circle_ft_out$x[1:250], rev(circle_ft_in$x[1:250])),
                y = c(circle_ft_out$y[1:250], rev(circle_ft_in$y[1:250])),
                group = group,
                description = "Top of Free Throw Circle"
              )
)

# Create and add lowest left block
court = rbind(court,
              new_coords(
                x = c(19 - (8/12), 19 - (8/12), 19, 19),
                y = c(7, 8, 8, 7),
                group = group,
                description = "Lowest Left Block"
              )
)

# Create and add second left block
court = rbind(court,
              new_coords(
                x = c(19 - (8/12), 19 - (8/12), 19, 19),
                y = c(11, 11 + (2/12), 11 + (2/12), 11),
                group = group,
                description = "Second Left Block"
              )
)

# Create and add third left block
court = rbind(court,
              new_coords(
                x = c(19 - (8/12), 19 - (8/12), 19, 19),
                y = c(14 + (2/12), 14 + (4/12), 14 + (4/12), 14 + (2/12)),
                group = group,
                description = "Third Left Block"
              )
)

# Create and add fourth left block
court = rbind(court,
              new_coords(
                x = c(19 - (8/12), 19 - (8/12), 19, 19),
                y = c(17 + (4/12), 17 + (6/12), 17 + (6/12), 17 + (4/12)),
                group = group,
                description = "Fourth Left Block"
              )
)

# Create and add lowest right block
court = rbind(court,
              new_coords(
                x = c(31 + (8/12), 31 + (8/12), 31, 31),
                y = c(7, 8, 8, 7),
                group = group,
                description = "Lowest right Block"
              )
)

# Create and add second right block
court = rbind(court,
              new_coords(
                x = c(31 + (8/12), 31 + (8/12), 31, 31),
                y = c(11, 11 + (2/12), 11 + (2/12), 11),
                group = group,
                description = "Second Right Block"
              )
)

# Create and add third right block
court = rbind(court,
              new_coords(
                x = c(31 + (8/12), 31 + (8/12), 31, 31),
                y = c(14 + (2/12), 14 + (4/12), 14 + (4/12), 14 + (2/12)),
                group = group,
                description = "Third Right Block"
              )
)

# Create and add fourth right block
court = rbind(court,
              new_coords(
                x = c(31 + (8/12), 31 + (8/12), 31, 31),
                y = c(17 + (4/12), 17 + (6/12), 17 + (6/12), 17 + (4/12)),
                group = group,
                description = "Fourth Right Block"
              )
)

# Create and add time line
court = rbind(court,
              new_coords(
                x = c(-(2/12), -(2/12), 50 + (2/12), 50 + (2/12)),
                y = c(47 - (1/12), 47, 47, 47 - (1/12)),
                group = group,
                description = "Time Line"
              )
)

# Create and add center circle
court = rbind(court,
              new_coords(
                x = c(circle_mid_out$x[250:500], rev(circle_mid_in$x[250:500])),
                y = c(circle_mid_out$y[250:500], rev(circle_mid_in$y[250:500])),
                group = group,
                description = "Center Circle"
              )
)

# Create and add hoop
court = rbind(court,
              new_coords(
                x = circle_hoop$x,
                y = circle_hoop$y,
                group = group,
                description = "Hoop"
              )
)

# Make portrait court image
get_portrait_court = function(){
  portrait_court = court
  ggplot(data = portrait_court, aes(x = x, y = y, group = group)) +
    geom_polygon(col = "#e04e39", fill = "#13294b") + 
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

# Make landscape court image
get_landscape_court = function(){
  landscape_court = court
  landscape_court$x = pi * (landscape_court$x/180)
  landscape_court$y = pi * (landscape_court$y/180)
  rotation_matrix = matrix(
    c(cos(pi/2),
      sin(pi/2), -sin(pi/2),
      cos(pi/2)
    ),
    ncol = 2
  )
  rotated_coords = apply(landscape_court[, c("x", "y")], 1, function(x) x %*% rotation_matrix)
  landscape_court$x = rotated_coords[1, ]
  landscape_court$y = rotated_coords[2, ]
  landscape_court$x = 180 * (landscape_court$x/pi)
  landscape_court$y = 180 * (landscape_court$y/pi)
  
  ggplot(data = landscape_court, aes(x = x, y = y, group = group)) +
    geom_polygon(col = "#e04e39", fill = "#13294b") +
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

# Make half court
get_half_court = function(){
  ggplot(data = court[court$side == 1, ], aes(x = x, y = y, group = group)) + 
    geom_polygon(col = "#e04e39", fill = '#13294b') +
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
