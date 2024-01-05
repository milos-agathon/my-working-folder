# Load the package
library(rayshader)

# Generate some random points in 3D space
set.seed(123)
n = 100
points = data.frame(x = runif(n, -10, 10),
                    y = runif(n, -10, 10),
                    z = runif(n, -10, 10))

# Create a matrix of elevation values
elmat = matrix(0, nrow = 100, ncol = 100)

# Add the points as spheres with emissive materials
for (i in 1:n) {
  elmat = elmat %>%
    add_object(sphere(x = points$x[i],
                      y = points$y[i],
                      z = points$z[i],
                      radius = 0.2,
                      material = light(color = sample(colors(), 1),
                                       intensity = 10)))
}

# Plot the map in 3D with a black background
plot_3d(elmat, zscale = 10, background = "black")
