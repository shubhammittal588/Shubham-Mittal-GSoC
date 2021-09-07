# Galton box animation using animint2 package in R
# Author: Shubham Mittal

# ======================== Paramters which can be varied =====================================
no.of.levels <-
  14 # How many times a ball chooses to fall left or right
iterations <- 300 #or no of balls sent
width <- 400 #width and height of the animantion plot
height <- 350
# The location of your output folder where the animation is stored
filepath <- "/Users/shubhammittal/Desktop/Animated_interactive_ggplots_GSoC"
# ===========================================================================================


library(animint2)
library(data.table)
library(comprehenr)

# Divide the plot into rectangles with each rectangle being blank, containing a triangle or a ball(i.e.circle)
width.segments <- 2 * no.of.levels + 1
width.segment <- width / width.segments
height.segments <- no.of.levels + 1
height.segment <- height / height.segments

# Function that returns the triangle coordinates
getTriangleXYcoors <-
  function(height.level,
           width.level,
           height.segment,
           width.segment) {
    x_base <- (width.level - 1) * width.segment
    y_base <- (height.level - 1) * height.segment
    x1 <- x_base + 1 / 8 * width.segment
    x2 <- x_base + 1 / 2 * width.segment
    x3 <- x_base + 7 / 8 * width.segment
    y1 <- y_base + 1 / 8 * height.segment
    y2 <- y_base + 7 / 8 * height.segment
    y3 <- y_base + 1 / 8 * height.segment
    return (list(x1, x2, x3, x1, y1, y2, y3, y1))
  }

# This datatable will store the x,y coordinates of the trianngles (along with the color and fill )
polydata <- data.table()

# Select the rectangles in which the triangles should be there according to the pattern and add
# their coordinates to 'polydata'
for (i in 1:width.segments) {
  if (i %% 2 == 1) {
    for (j in 1:min(i, width.segments - i + 1)) {
      if (j %% 2 == 1) {
        coords <- getTriangleXYcoors(j, i, height.segment, width.segment)
        polydata <-
          rbind(
            polydata,
            data.frame(
              x = unlist(coords[1:4]),
              y = unlist(coords[5:8]),
              group = paste0(i, j),
              color = "black",
              fill = "white"
            )
          )
        
      }
    }
  }
  else if (i %% 2 == 0) {
    for (j in 2:min(i, width.segments - i + 1)) {
      if (j %% 2 == 0) {
        coords <- getTriangleXYcoors(j, i, height.segment, width.segment)
        polydata <-
          rbind(
            polydata,
            data.frame(
              x = unlist(coords[1:4]),
              y = unlist(coords[5:8]),
              group = paste0(i, j),
              color = "black",
              fill = "white"
            )
          )
      }
    }
  }
}

# Draw the triangles
viz = ggplot() + theme_animint(width = width, height = height)
viz <- viz +
  geom_polygon(
    data = polydata,
    alpha = .5,
    aes(
      x = x,
      y = y,
      group = group,
      fill = fill,
      colour = color
    )
  ) +
  scale_colour_identity() + scale_fill_identity() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# We approximate a circle by using geom_polygon, for that we find the points for given center
# Group and iteration values are for working with animint later
circleFun <- function(center = c(0, 0),
                      group,
                      iteration = 1,
                      diameter = width.segment / 1.2,
                      npoints = 16) {
  r = diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(
    x = xx,
    y = yy,
    group = group,
    iteration = iteration
  ))
}

# We number the circles from 1 to 'no.of.circles' in the order top to bottom and left to right
# The level() function returns the level of the circle, where the topmost circle has level 1
# and the bottommost circles have level 'no.of.levels'
no.of.circles = no.of.levels * (no.of.levels + 1) / 2
breaks <- to_vec(for (i in 0:no.of.levels)
  i * (i + 1) / 2)
level <- function(a) {
  cut(a, breaks, labels = FALSE, right = TRUE)
  
}

# This datset will store 1 if there is a ball in the segment of the particular circle
# The first circle is where balls are thrown from, will always have a ball except
# for the end when balls are stopped to be thrown.
iteration_circles <-
  data.table("circle1" = (rep(
    c(1, 0), c(iterations - no.of.levels, no.of.levels)
  )))

iteration_circles[, paste0("circle", 2:(no.of.circles + 1)) := rep(0, iterations)]
coin <-
  c(0, 1) # For random toss between falling left or falling right
for (i in 1:iterations) {
  for (j in no.of.circles:1) {
    level.j <- level(j)
    if (iteration_circles[i, ..j] == 1 & level.j != no.of.levels) {
      random_toss <-
        sample(
          coin,
          prob = c(0.5, 0.5),
          replace = TRUE,
          size = 1
        )
      if (random_toss == 0) {
        iteration_circles[i + 1, j + level.j] <- 1
      }
      else{
        iteration_circles[i + 1, j + level.j + 1] <- 1
      }
    }
  }
}

# According to the circle number, will return the coordinates of the circle in the plot
circleCenter <- function(circle_number) {
  level.circle <- level(circle_number)
  y_coor <- (height.segments - level.circle - 0.5) * height.segment
  mid <- (width.segments + 1) / 2
  a <- mid - level.circle
  x.base <- width.segment / 2
  x.first <- x.base + a * width.segment
  circles.before <- level.circle * (level.circle - 1) / 2
  circle_extra <- circle_number - circles.before - 1
  x_coor <- x.first + width.segment * 2 * circle_extra
  return (c(x_coor, y_coor))
}

# make the data.table which contains the datatable for showing the circles according to the iteration number
final.circles.dt <-
  data.table(
    x = numeric(),
    y = numeric(),
    group = numeric(),
    iteration = numeric()
  )
for (circle in 1:no.of.circles) {
  center = circleCenter(circle)
  for (iter in 1:iterations) {
    if (iteration_circles[iter, ..circle] == 1) {
      final.circles.dt <-
        rbind(final.circles.dt, circleFun(center, circle, iter))
    }
  }
}

# Data for the text labels and color of balls seen in the plot
text.x.coors <-
  seq(1, no.of.levels) * 2 * width.segment - 0.5 * width.segment
text.y.coors <- seq(-height / 20, -height / 20, no.of.levels)
text.label <- seq(1, no.of.levels)
text.dt <-
  data.table(x = text.x.coors, y = text.y.coors, label = text.label)
colfunc <-
  colorRampPalette(c("red", "yellow2", "springgreen", "royalblue", "red"))
colors <- colfunc(1.1 * iterations * no.of.circles)
final.circles.dt$color.circle <-
  colors[(final.circles.dt$iteration - 1) * no.of.circles + final.circles.dt$group *
           no.of.circles / 10]

# Add the circles to the plot, they are grouped by the circle number and will be shown according to selected iteration number
viz <- viz +
  geom_polygon(
    data = final.circles.dt,
    aes(
      x = x,
      y = y,
      group = group,
      color = color.circle,
      fill = color.circle
    ),
    showSelected = "iteration"
  ) + #, chunk_vars=c("iteration") - But this makes loading slow
  geom_text(data = text.dt,
            aes(x = x, y = y,
                label = label),
            size = 14)

# making the bar-plot
# We note that geom_histogram doesnt work with animint
col.nums <-
  (no.of.levels * (no.of.levels - 1) / 2 + 1):(no.of.levels * (no.of.levels +
                                                                 1) / 2)
iteration_circles.hist <- cumsum(iteration_circles[, ..col.nums])
colnames(iteration_circles.hist) <-
  as.character(unlist(1:no.of.levels))
iteration_circles.hist <-
  cbind(iteration_circles.hist, iteration = rownames(iteration_circles.hist))
molten <-
  melt(iteration_circles.hist,
       id.vars = "iteration",
       variable.name = "circleNum")
molten$iteration <- as.numeric((molten$iteration))

# Add text to bar-plot
max.count <- max(molten$value)
text_bar_coors <- seq(-0.1 * max.count, -0.1 * max.count, no.of.levels)
text.dt$text_bar_coors <- text_bar_coors

# Adding the bar-plot
viz2 <- ggplot() +
  theme_animint(width = width, height = height) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_bar(
    aes(x = circleNum, y = value,
        group = circleNum),
    data = molten,
    fill = "#1293EE",
    color = "white",
    showSelected = "iteration",
    stat = "identity",
    position = "identity"
  ) + scale_colour_identity() +
  scale_fill_identity() +
  geom_text(
    data = text.dt,
    aes(
      x = label,
      y = text_bar_coors,
      label = label,
      group = label
    ),
    size = 14
  ) +
  geom_text(
    data = molten,
    aes(
      x = circleNum,
      y = value + 0.035 * max.count,
      label = value,
      group = circleNum
    ),
    size = 14,
    showSelected = "iteration"
  )

# Final animation
(viz.final2 <- animint(
  viz,
  viz2,
  time = list(variable = "iteration", ms = 100),
  first = list(iteration = 1)
))

# Saving it to a directory - uncomment it
#if (!requireNamespace("servr")) install.packages("servr")
#servr::httd("/Users/shubhammittal/Desktop/Animated_interactive_ggplots_GSoC")
#animint2dir(viz.final2, filepath)