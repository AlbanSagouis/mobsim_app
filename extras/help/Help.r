Help <- list(
   
   # simulating communities
   S = list(title = "Species richness", content = "min = 5, max = 500 or Number of Individuals"),
   N = list(title = "Number of individuals", content = "min = 10, max = 5000"),
   select_sad_type = list(title = "Species Abundance Distribution type", content = "Choose in the list the type of distribution to use to allocate abundances between species."),
   CVslider = list(title = "Coefficient of variation of abundances", content = "CV = standard deviation of abundances divided by the mean abundance. Parameter controlling the shape of SAD: low values make more even communities."),
   spatagg = list(title = "Aggregation parameter", content = "Parameter controling how clustered the individuals are around the mother points. Either a single value and all species will be similarly clustered or as many values as species, separated by commas."),
   spatdist = list(title = "Clustering method", content = "Number of clusters: mean number of individuals per cluster. Number of mother points: Number of cores around which populations are simulated for each species."),
   spatcoef = list(title = "Clustering parameter", content = "A single value can be specified and all species will have the same number of cores OR one number can be given per species (separated by commas)"),
   method_type = list(title = "How to simulate a community", content = "random_mother_points: mother points are randomly set and individuals are simulated using a thomas process. click_for_mother_points: Click on the plot to specify mother point coordinates. click_for_species_ranges: Draw 1 rectangle per species in which mother ponts and individuals will be simulated. uploading_community_data: Upload an R data file containing an object of class Community as produced by MOBsim function sim_thomas_community()."),
   community_uploading_tool = list(title = "Community upload", content = "Upload an R data file containing an object of class Community as produced by MOBsim function sim_thomas_community()."),
   species_range_uploading_tool = list(title = "Species range upload", content = "csv data fame with 5 columns: species_id, xmin, xmax, ymin, ymax."),
   simulation_seed = list(title="Simulation seed", content = "Enter a number here to be used as seed for community simulation. Write 'Not specified' to come back to a random seed. When  simulation seed is specified, the Species Abundance Distribution, the coordinates of mother points and the coordinates of individuals are fixed."),
   
   # Sampling
   sampling_seed = list(title = "Sampling seed", content = "Enter a number here to be used as seed for quadrat placement. Write 'Not specified' to come back to a random placement. Only quadrat placement is fixed when specifying the seed."),
   area_of_quadrats = list(title = "Area of a single quadrat", content = "Unique numeric value specifying the area of the square sampling quadrats"),
   
   
   
   
   keep_step = list(title = "Compare simulations", content = "Save simulation and move to next step"),
   
   
   
   # Plotting
   color_palette = list(title="Choose a suitable color palette", content = "Some colorpalettes are suitable only for a limnited number of values, some are Colorblind friendly and others are not"),
   rarefaction_curves_plot = list(title = "Plot tools", content = "Lightly colored curves represent rarefaction curve for each quadrat: alpha scale. Dark green curve represents rarefaction curve for all quadrats together: gamma scale. The blue curve represents the 'truth': rarefaction curve based on the actual complete community. Click on a light green curve to highlight the corresponding quadrat. Draw a rectangle to zoom-in, double-click anywhere to zoom-out."),
   sampling_plot = list(title = "Community plot with quadrats", content = "Each dot is an individual, each color is a species. Click on a quadrat to highlight the corresponding rarefaction curve"),
   
   # Big table
   bigtable_output = list(title = "Summary of all simulation runs", content = "Select rows to delete simulations using the Remove selected simulations button or compare the selected communities."),
   downloadSimulationTable = list(title = "Download simulation table", content = "You can download this table as a data frame to keep track of your simulations or run statistical analyses on diversity metrics in your R session."),
   downloadSimulationList = list(title = "Download all simulated data", content = "You can download a list of all your simulations, including Species Abundance Distribution, individual coordinates, quadrats coordinates and sizes to run further analyses in your R session. Each element of the list, results from a simulation, is named using the sim_ID found in the simulation table.")
   
)
