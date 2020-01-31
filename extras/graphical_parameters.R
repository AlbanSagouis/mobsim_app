library(pals)
palette_tab <- data.frame(palette_name = c(c('alphabet','alphabet2', 'cols25', 'glasbey','kelly','okabe','polychrome', 'stepped', 'stepped2', 'stepped3', 'tol', 'watlington', 'brewer.paired'), 
                                           c('cubehelix','gnuplot','parula','tol.rainbow','cividis','brewer.spectral','brewer.brbg','ocean.thermal','ocean.curl','ocean.haline','inferno','plasma','viridis')),
                          palette_max_number = c(c(26, 26, 25, 32, 22, 8, 36, 24, 20, 20, 12, 16, 12), rep('', 13)),
                          palette_type = c(rep("discrete", 13), rep("continuous", 13)))