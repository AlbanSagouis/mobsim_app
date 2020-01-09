#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# library(devtools)
# install_github('MoBiodiv/mobsim')    # downloads the latest version of the package
library(mobsim, lib.loc="./Library")
# library(ggplot2)
# library(markdown)

bigtable_names <- c('sim_ID','method','n_species','n_individuals','seed_simulation','n_quadrats','quadrat_area','seed_sampling',
	 'gamma_richness','gamma_shannon','gamma_simpson',
	 'alpha_mean_richness','alpha_mean_shannon','alpha_mean_simpson'
)
empty_bigtable <- function() matrix(NA, nrow=0, ncol=length(bigtable_names), dimnames=list(c(), bigtable_names))

# Define server logic for slider examples
shinyServer(function(input, output, session) {

	values <- reactiveValues()

	# Big table tab
	
	session$userData$sim_ID <- 1
	values$bigtable <- empty_bigtable()
	
	## Adding rows to the simulation table
	### From Simulation and sampling tabs
	#### Restart button
	observeEvent(
		input$Restart, {

		isolate({
			session$userData$sim_ID <- session$userData$sim_ID + 1
			values$bigtable <- rbind(values$bigtable, c(
				sim_ID = session$userData$sim_ID,
				method = input$method_type,
				n_species = input$S,
				n_individuals = input$N,
				seed_simulation = seed_simulation(),
				n_quadrats = NA,
				quadrat_area =NA,
				seed_sampling = NA,
				gamma_richness = NA,
				gamma_shannon = NA,
				gamma_simpson = NA,
				alpha_mean_richness = NA,
				alpha_mean_shannon = NA,
				alpha_mean_simpson = NA
			))
		})
	})
	#### New sampling button
	observeEvent(
		input$new_sampling_button, {
		
		isolate({
			session$userData$sim_ID <- session$userData$sim_ID + 1
			values$bigtable <- rbind(values$bigtable, c(
				sim_ID = session$userData$sim_ID,
				method = input$method_type,
				n_species = input$S,
				n_individuals = input$N,
				seed_simulation = seed_simulation(),
				n_quadrats = input$number_of_quadrats,
				quadrat_area = input$area_of_quadrats,
				seed_sampling = seed_sampling(),
				gamma_richness = sampling_gamma_table()$n_species,
				gamma_shannon = sampling_gamma_table()$shannon,
				gamma_simpson = sampling_gamma_table()$simpson,
				alpha_mean_richness = sampling_alpha_summary_table()["n_species","mean"],
				alpha_mean_shannon = sampling_alpha_summary_table()["shannon","mean"],
				alpha_mean_simpson = sampling_alpha_summary_table()["simpson","mean"]
			))
		})
	})
	
	### From step-by-step tab
	#### Restart button
	observeEvent(
		input$sbsRestart, {

		isolate({
			session$userData$sim_ID <- session$userData$sim_ID + 1
			values$bigtable <- rbind(values$bigtable, c(
				sim_ID = session$userData$sim_ID,
				method = "random_mother_points",
				n_species = input$sbsS,
				n_individuals = input$sbsN,
				seed_simulation = seed_simulation(),
				n_quadrats = input$sbsnumber_of_quadrats,
				quadrat_area = input$sbsarea_of_quadrats,
				seed_sampling = seed_sampling(),
				gamma_richness = sbsgamma_table()$n_species,
				gamma_shannon = sbsgamma_table()$shannon,
				gamma_simpson = sbsgamma_table()$simpson,
				alpha_mean_richness = sbsalpha_summary_table()["n_species","mean"],
				alpha_mean_shannon = sbsalpha_summary_table()["shannon","mean"],
				alpha_mean_simpson = sbsalpha_summary_table()["simpson","mean"]
			))
		})
	})
	#### New sampling button
	observeEvent(
		input$sbsnew_sampling_button, {
				
		isolate({
			session$userData$sim_ID <- session$userData$sim_ID + 1
			values$bigtable <- rbind(values$bigtable, c(
				sim_ID = session$userData$sim_ID,
				method = "random_mother_points",
				n_species = input$sbsS,
				n_individuals = input$sbsN,
				seed_simulation = seed_simulation(),
				n_quadrats = input$sbsnumber_of_quadrats,
				quadrat_area = input$sbsarea_of_quadrats,
				seed_sampling = seed_sampling(),
				gamma_richness = sbsgamma_table()$n_species,
				gamma_shannon = sbsgamma_table()$shannon,
				gamma_simpson = sbsgamma_table()$simpson,
				alpha_mean_richness = sbsalpha_summary_table()["n_species","mean"],
				alpha_mean_shannon = sbsalpha_summary_table()["shannon","mean"],
				alpha_mean_simpson = sbsalpha_summary_table()["simpson","mean"]
			))
		})
	})
	
	## Remove all simulations
	observeEvent(input$rem_all_simulations, {
		values$bigtable <- empty_bigtable()
	})
	
	## Remove selected simulations
	observeEvent(input$rem_selected_simulations, {
		values$bigtable <- values$bigtable[-as.numeric(input$bigtable_output_rows_selected), ]
	})
	
	## Download simulation table
	output$downloadSimulationTable <- downloadHandler(
		filename = function() {paste("Simulation_table.csv", sep="")},
		content  = function(fname) {
			write.csv(values$bigtable, file=fname)
		}
	)
	
	## render bigtable
	output$bigtable_output <- renderDataTable(
		DT::datatable(values$bigtable, options = list(searching=FALSE, pageLength=20))
	)
	output$bigtable_selected_simulations <- renderPrint(paste(values$bigtable[as.numeric(input$bigtable_output_rows_selected), "sim_ID"], collapse=", "))
	
	

	
	
	#########################################################################################################
	# SIMULATION TAB
  
  # update range for species richness, an observed species has minimum one individual
	observe({
		updateSliderInput(session, "S", min=5, max=input$N, value=5, step=5)
	})
  # update the number of species in the drop down species list.
	observe({
		updateSelectInput(session, "species_ID", "Pick species ID", paste("species", 1:input$S, sep="_"))
	})


	output$select_sad_type <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points"))	{
			return()
		} else {
			selectizeInput("sad_type", "SAD Type", choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls"))
		}					
	})
						
	  
	output$CVslider <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points","click_for_species_ranges") | is.null(input$sad_type))	
			return()
		switch(input$sad_type,
			"lnorm"=sliderInput("coef", label="CV(abundance), i.e. standard deviation of abundances divided by the mean abundance",value=1, min=0, max=5, step=0.1, ticks=F),
			"geom"=sliderInput("coef",label="Probability of success in each trial. 0 < prob <= 1",value=0.5,min=0,max=1,step=0.1, ticks=F),
			"ls"=textInput("coef",label="Fisher's alpha parameter",value=1)
		)
	})

	output$text_spat_agg <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			textInput(inputId="spatagg", label="Spatial Aggregation (mean distance from mother points)", value = 0.1)
		}			
	})
	
	## seed simulation
	seed_simulation <- reactive({
		input$Restart
		input$sbsRestart
		
		isolate({
			if(input$sbssimulation_seed == "Not specified")	sample(1:2^15, 1) else as.numeric(input$sbssimulation_seed)
		})
	})
	
	seed_sampling <- reactive({
		input$Restart
		input$new_sampling_button
		input$sbsRestart
		input$sbsnew_sampling_button
		
		isolate({
			if(input$sbssampling_seed == "Not specified")	sample(1:2^15, 1) else as.numeric(input$sbssampling_seed)
		})
	})

	# debugging
	output$debugging_seed <- renderText(paste0("input = ", input$sbssimulation_seed, ", reactive = ", seed_simulation()))

	##		random_mother_points

	output$spatdist <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			selectizeInput(inputId="spatdist", "Cluster parameter", choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster"))
		}					
	})

	output$spatcoef <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			textInput(inputId="spatcoef",label="Integer values separated by commas", value="0")
		}					
	})



	##		click_for_mother_points


	output$species_ID_input <- renderUI({
		if (!input$method_type %in% c("click_for_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			selectInput("species_ID", "Pick species ID", paste("species", 1:input$S, sep="_"))
		}
	})

	values$DT <- data.frame(x = numeric(),
								 y = numeric(),
								 species_ID = factor())

	output$on_plot_selection <- renderPlot({
	  if (is.null(input$method_type)) {
			return()
		} else {
			if(input$method_type %in% c("click_for_mother_points","click_for_species_ranges")) {
					color_vector <- rainbow(input$S)
					par(mex=0.6, mar=c(3,3,0,0), cex.axis=0.8)
					plot(x=values$DT$x, y=values$DT$y, col=color_vector[values$DT$species_ID], xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", las=1, asp=1, pch=20)
					abline(h=c(0,1), v=c(0,1), lty=2)
			}
		}
	})


	output$rem_point_button <- renderUI({
		if (!input$method_type %in% c("click_for_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			actionButton("rem_point", "Remove Last Point")
		}
	})

	output$rem_all_points_button <- renderUI({
		if (!input$method_type %in% c("click_for_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			actionButton("rem_all_points", "Remove All Points")
		}
	})

	 
	## point coordinates
	### storing coordinates
	observeEvent(input$plot_click, {
		add_row = data.frame(species_ID = factor(input$species_ID, levels = paste("species", 1:input$S, sep="_")),
									x = input$plot_click$x,
									y = input$plot_click$y
								 )
		values$DT = rbind(values$DT, add_row)
	})

	observeEvent(input$rem_all_points, {
		if (input$method_type == "click_for_mother_points")	{
			rem_row = values$DT[-(1:nrow(values$DT)), ]
			values$DT = rem_row
		}
		if (input$method_type == "click_for_species_ranges")	{
			rem_row = values$DT_species_ranges[-(1:nrow(values$DT_species_ranges)), ]
			values$DT_species_ranges = rem_row
		}
	})
		 
	observeEvent(input$rem_point, {
		if (input$method_type == "click_for_mother_points")	{
			rem_row = values$DT[-nrow(values$DT), ]
			values$DT = rem_row
		}
		if (input$method_type == "click_for_species_ranges")	{
			values$DT_species_ranges <- values$DT_species_ranges[-as.numeric(input$datatable_species_ranges_rows_selected), ]
		}
	})

	### showing coordinates in a text box
	output$info <- renderText({
	if (input$method_type != "click_for_mother_points")	{
			return()
		} else {
			xy_str <- function(e) {
				if(is.null(e)) return("")
				paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
			}
			xy_range_str <- function(e) {
				if(is.null(e)) return("")
				paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
						 " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
			}
			
			paste0(
				"click: ", xy_str(input$plot_click),
				"brush: ", xy_range_str(input$plot_brush)
			)
		}
	})

	### showing coordinates in a table
	output$datatable <- renderTable({
	if (!input$method_type %in% c("click_for_mother_points"))	{
			return()
		} else {
			values$DT
		}
	})

	### showing community summary
	observe({
		output$simcomsummary <- renderPrint({
			input$Restart
			summary(session$userData$sim.com, digits=2)
		})
	})


	##		click_for_species_ranges

	### storing coordinates
	values$DT_species_ranges <- data.frame(xmin = numeric(), xmax = numeric(),
							 ymin = numeric(),  ymax = numeric(),
							 species_ID = factor())

								
	# brush <- NULL
	# makeReactiveBinding("brush")
	# observeEvent(input$plot_brush, {
		# brush <<- input$plot_brush
	# })

	# observeEvent(input$species_ID, {
		# session$resetBrush("plot_brush")
	# })

	# observeEvent(input$resetPlot, {
		# session$resetBrush("plot_brush")
		# brush <<- NULL
	# })
	### changing species_ID after each brushing event
	observeEvent(input$plot_brush, {
		updateSelectInput(session,
			inputId = "species_ID",
			selected= paste("species", as.numeric(gsub(x=input$species_ID, pattern="species_", replacement="\\2"))+1, sep="_")
		)
	})
	
	observeEvent(input$plot_brush, {
		add_row = data.frame(species_ID = factor(input$species_ID, levels = paste("species", 1:input$S, sep="_")),
									xmin = input$plot_brush$xmin,
									xmax = input$plot_brush$xmax,
									ymin = input$plot_brush$ymin,
									ymax = input$plot_brush$ymax
								 )
		values$DT_species_ranges = rbind(values$DT_species_ranges, add_row)
	})
	

	### showing coordinates in a table
	output$datatable_species_ranges <- renderDataTable({
	if (!input$method_type %in% c("click_for_species_ranges"))	{
			return()
		} else {
			DT_species_ranges_rounded <- values$DT_species_ranges
			DT_species_ranges_rounded[,colnames(DT_species_ranges_rounded) != "species_ID"] <- round(values$DT_species_ranges[, colnames(values$DT_species_ranges) != "species_ID"], 2)
			
			DT::datatable(DT_species_ranges_rounded, options = list(searching=FALSE, pageLength=15))
		}
	})







	##		uploading_community_data
	output$community_uploading_tool <- renderUI({
		if (input$method_type != "uploading_community_data")	{
			return()
		} else {
			fileInput(inputId="loaded_file", label="Choose rData community File", multiple = FALSE,
						accept = "", width = NULL,
						buttonLabel = "Browse...", placeholder = "No file selected")
		}
	})



	## plot theme
	### plot function
 	plot_layout <- function(sim.com) {
		layout(matrix(c(1,2,3,
							 4,5,6), byrow = T, nrow = 1, ncol = 6),
				 heights = c(1,1), widths=c(1,1,1))
		
		sad1 <- community_to_sad(sim.com)
		sac1 <- spec_sample_curve(sim.com)
		divar1 <- divar(sim.com, exclude_zeros=F)
		dist1 <- dist_decay(sim.com)
		
		plot(sad1, method = "octave")
		plot(sad1, method = "rank")
		
		plot(sim.com, main = "Community distribution")
		
		plot(sac1)
		plot(divar1)
		plot(dist1)
	}

	
	
	
	
	
  output$InteractivePlot <- renderPlot({
    input$Restart
    
    isolate({
		
		if(input$method_type != "uploading_community_data") {
      
		# set.seed(229377)	# 229376
		
		
			spatagg_num <- as.numeric(unlist(strsplit(trimws(input$spatagg), ",")))
			spatcoef_num <- as.numeric(unlist(strsplit(trimws(input$spatcoef), ",")))
		 
			if(input$spatdist=="n.mother") n.mother <- spatcoef_num else n.mother <- NA
			if(input$spatdist=="n.cluster") n.cluster <- spatcoef_num else n.cluster <- NA
			
			simulation_parameters <- switch(input$method_type,
									"random_mother_points" = list(mother_points = n.mother,
																		cluster_points = n.cluster,
																		xmother = NA,
																		ymother = NA,
																		xrange = c(0,1),
																		yrange = c(0,1)),
									"click_for_mother_points" = list(mother_points = NA,
																		cluster_points = NA,
																		xmother = tapply(values$DT$x, values$DT$species_ID, list),
																		ymother = tapply(values$DT$y, values$DT$species_ID, list),
																		xrange = c(0,1),
																		yrange = c(0,1)),
									"click_for_species_ranges" = list(mother_points = n.mother,
																		cluster_points = n.cluster,
																		xmother = NA,
																		ymother = NA,
																		xrange = data.frame(values$DT_species_ranges$xmin, values$DT_species_ranges$xmax),
																		yrange = data.frame(values$DT_species_ranges$ymin, values$DT_species_ranges$ymax))
									)
									


			# sim.com might rather be set as a reactive ({ }) that is then called sim.com(). is this more efficient ?
			session$userData$sim.com <- switch(input$sad_type,
							"lnorm"=sim_thomas_community(s_pool = input$S, n_sim = input$N, 
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sad_type, sad_coef=list(cv_abund=input$coef),
								fix_s_sim = T, seed = seed_simulation(),
								xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange),
							"geom"=sim_thomas_community(s_pool = input$S, n_sim = input$N,
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sad_type, sad_coef=list(prob=input$coef),
								fix_s_sim = T, seed = seed_simulation(),
								xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange),
							"ls"=sim_thomas_community(s_pool = input$S, n_sim = input$N,
								sad_type = input$sad_type, sad_coef=list(N=input$N,alpha=as.numeric(input$coef)),
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								fix_s_sim = T, seed = seed_simulation(),
								xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange)
						)
		} else {
			session$userData$sim.com <- get(load(input$loaded_file$datapath))
		}

		plot_layout(session$userData$sim.com)
		session$userData$previous.sim.com <- session$userData$sim.com
		
	})
	})
		
	

	
	output$PreviousInteractivePlot <- renderPlot({
		if(!input$keepInteractivePlot){
			return()
		} else {
			plot_layout(session$userData$previous.sim.com)
		}
	})
	
	# observeEvent(input$keepInteractivePlot, {	# adds as many previous plots as clicks. works in conjunction with renderPlot of outpu$PreviousInteractivePlot and in replacement of ui plotOutput(outpu$PreviousInteractivePlot).
		# insertUI(
			# selector = "#InteractivePlot",
			# where = "beforeBegin",
			# ui = plotOutput("PreviousInteractivePlot", height="300px",width="1500px")
		# )
	# })
	
	
	# Downloading data and plots
	## downloading data
	output$downloadData <- downloadHandler(
		filename = function() {paste("community", sep="")},
		content  = function(fname) {
			sim.com <- session$userData$sim.com
			save("sim.com", file=fname)
		}
	)
	
	## downloading plot
	output$downloadMobPlot <- downloadHandler(
	
		filename = function() {paste("plotMOB", input$plot_saving_format, sep=".")},
		content  = switch(input$plot_saving_format,
			# "pdf" = function(fname) {
				# pdf(filename=fname, width=input$plot_saving_width, height=input$plot_saving_height)
				# print(plot_layout(session$userData$sim.com))
				# dev.off()
			# },
			# "svg" = function(fname) {
				# svg(filename=fname, width=15, height=3)
				# print(plot_layout(session$userData$sim.com))
				# dev.off()
			# },
			"png" = function(fname) {
				png(filename=fname, width=15, height=3, units="in", res=300)
				print(plot_layout(session$userData$sim.com))
				dev.off()
			},
			"tiff" = function(fname) {
				tiff(filename=fname, width=15, height=3, units="in", res=300)	# input$plot_saving_resolution
				print(plot_layout(session$userData$sim.com))
				dev.off()
			},
		)
		# content=function(fname) {
			# png(filename=fname, width=1500, height=300)
			# print(plot_layout(session$userData$sim.com))
			# dev.off()
		# }
	)

	
	
	
	
	######################################################################################################################################################
	# Sampling tab
	# Sampling
	## Community summary
	output$community_summary_table <- renderTable({
		input$Restart
		data.frame(Community = "",
						n_species = length(levels(session$userData$sim.com$census$species)),
						n_individuals = nrow(session$userData$sim.com$census))
	})
	## Sampling parameters
	
	## Plot the community and sampling squares
	sampling_quadrats <- reactive({
		input$new_sampling_button
		
		sample_quadrats(comm=session$userData$sim.com, n_quadrats=input$number_of_quadrats, quadrat_area=input$area_of_quadrats, method = input$sampling_method, avoid_overlap=T, plot=F, seed=seed_sampling())
	})
	
	# previous_sampling_quadrats <- reactive({
		# input$keepRarefactionCurvesPlot
		
		# isolate({
			# sample_quadrats(comm=session$userData$sim.com, n_quadrats=input$number_of_quadrats, quadrat_area=input$area_of_quadrats, method = input$sampling_method, avoid_overlap=T, plot=F)
		# })
	# })
	
	# eventReactive("new_sampling_button", {
		# session$userData$sampled_quadrats <- sampling_quadrats()
	# })

	
	output$sampling_plot <- renderPlot({
		input$Restart
		input$new_sampling_button
		
		isolate({
			# session$userData$sampled_quadrats <- sampling_quadrats()
			quadrats_coordinates <- sampling_quadrats()$xy_dat
			plot(session$userData$sim.com, main = "Community distribution")
			graphics::rect(quadrats_coordinates$x,
								quadrats_coordinates$y,
								quadrats_coordinates$x + sqrt(input$area_of_quadrats),
								quadrats_coordinates$y + sqrt(input$area_of_quadrats),
								lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
			# points(input$sampling_plot_click$x, input$sampling_plot_click$y, pch="x", col="red")	# debugging help
			# text(sampling_quadrats()$xy_dat$x, sampling_quadrats()$xy_dat$y, pch=as.character(1:nrow(sampling_quadrats()$xy_dat)))	# debugging help
		})
		
		if(!is.null(input$rarefaction_curves_plot_click)) {	# highlight
			graphics::rect(xleft = quadrats_coordinates[rarefaction_curves_click_info(), "x"],
					ybottom = quadrats_coordinates[rarefaction_curves_click_info(), "y"],
					xright = quadrats_coordinates[rarefaction_curves_click_info(), "x"] + sqrt(input$area_of_quadrats),
					ytop = quadrats_coordinates[rarefaction_curves_click_info(), "y"] + sqrt(input$area_of_quadrats),
					lwd=2, col = grDevices::adjustcolor("forestgreen", alpha.f = 0.5))
					
		}
	})
	
	output$previous_sampling_plot <- renderPlot({
		input$Restart
		input$keepRarefactionCurvesPlot
		
		isolate({
			session$userData$previous_sampled_quadrats <- sampling_quadrats()
			plot(session$userData$sim.com, main = "Community distribution")
			quadrats_coordinates <- session$userData$previous_sampled_quadrats$xy_dat
			graphics::rect(quadrats_coordinates$x,
								quadrats_coordinates$y,
								quadrats_coordinates$x + sqrt(input$area_of_quadrats),
								quadrats_coordinates$y + sqrt(input$area_of_quadrats),
								lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
			# points(input$sampling_plot_click$x, input$sampling_plot_click$y, pch="x", col="red")	# debugging help
			# text(session$userData$previous_sampled_quadrats$xy_dat$x, session$userData$previous_sampled_quadrats$xy_dat$y, pch=as.character(1:nrow(session$userData$previous_sampled_quadrats$xy_dat)))	# debugging help
		})
		
		# if(!is.null(input$rarefaction_curves_plot_hover)) {	# highlight
			# graphics::rect(xleft = quadrats_coordinates[rarefaction_curves_hover_info(), "x"],
					# ybottom = quadrats_coordinates[rarefaction_curves_hover_info(), "y"],
					# xright = quadrats_coordinates[rarefaction_curves_hover_info(), "x"] + sqrt(input$area_of_quadrats),
					# ytop = quadrats_coordinates[rarefaction_curves_hover_info(), "y"] + sqrt(input$area_of_quadrats),
					# lwd=2, col = grDevices::adjustcolor("forestgreen", alpha.f = 0.5))				
		# }
	})
	
	
	## Sampling summary
	### gamma scale
	sampling_gamma_table <- reactive({
		input$Restart
		input$new_sampling_button
		
		isolate({
			abund <- apply(sampling_quadrats()$spec_dat, 2, sum)
			abund <- abund[abund > 0]
			relabund <- abund/sum(abund)
			shannon <- - sum(relabund * log(relabund))
			simpson <- 1- sum(relabund^2)
			data.frame(
							Gamma_scale = "",
							n_species= length(abund),
							shannon = round(shannon, 3),
							# ens_shannon = round(exp(shannon), 3),
							simpson = round(simpson, 3)
							# ens_simpson = round(1/(1 - simpson), 3)
			)
		})
	})
	output$sampling_gamma_table <- renderTable(sampling_gamma_table())
	
	previous_sampling_gamma_table <- reactive({
		input$Restart
		input$keepRarefactionCurvesPlot
		isolate({
			abund <- apply(session$userData$previous_sampled_quadrats$spec_dat, 2, sum)
			abund <- abund[abund > 0]
			relabund <- abund/sum(abund)
			shannon <- - sum(relabund * log(relabund))
			simpson <- 1- sum(relabund^2)
			data.frame(
							Gamma_scale = "",
							n_species= length(abund),
							shannon = round(shannon, 3),
							# ens_shannon = round(exp(shannon), 3),
							simpson = round(simpson, 3)
							# ens_simpson = round(1/(1 - simpson), 3)
			)
		})
	})
	output$previous_sampling_gamma_table <- renderTable(previous_sampling_gamma_table())

	### alpha scale
	output$sampling_alpha_table <- renderDataTable({
		input$Restart
		input$new_sampling_button
		isolate({
			quadrats_coordinates <- sampling_quadrats()$xy_dat
			DT::datatable(
				t(round(sapply(1:nrow(quadrats_coordinates), function(i) div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=session$userData$sim.com)), 3))[,c('n_species','n_endemics','shannon','simpson')],
				options=list(searching = FALSE, info = TRUE, sort = TRUE))
		})
	})
	
	sampling_alpha_summary_table <- reactive({
		input$Restart
		input$new_sampling_button
		isolate({
			quadrats_coordinates <- sampling_quadrats()$xy_dat
			temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
				div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=session$userData$sim.com)
			}), 3)))[,c('n_species','n_endemics','shannon','simpson')]
			funs <- list(min=min, max=max, mean=mean, sd=sd)
			data.frame(Alpha_scale=colnames(temp), round(sapply(funs, mapply, temp),3))
		})
	})
	output$sampling_alpha_summary_table <- renderTable(sampling_alpha_summary_table())
	
	output$previous_sampling_alpha_summary_table <- renderTable({
		input$Restart
		input$keepRarefactionCurvesPlot
		isolate({
			quadrats_coordinates <- session$userData$previous_sampled_quadrats$xy_dat
			temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
				div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=session$userData$sim.com)
			}), 3)))[,c('n_species','n_endemics','shannon','simpson')]
			funs <- list(min=min, max=max, mean=mean, sd=sd)
			data.frame(Alpha_scale=colnames(temp), round(sapply(funs, mapply, temp),3))
		})
	})
	
	## Sampling efficiency plot
	### computing the rarefaction curves
	rarefaction_curves_list <- reactive({
		apply(sampling_quadrats()$spec_dat, 1, function(site) {
			rare_curve(site)
		})
	})
	

		
	ranges <- reactiveValues(x = NULL, y = NULL)		# used to zoom in the plot
	
	### Plotting
	output$rarefaction_curves_plot <- renderPlot({
		input$Restart
		input$new_sampling_button
		input$rarefaction_curves_plot_dblclick	# zooming in and out
		# input$rarefaction_curves_plot_click	# highlighting individual site
		input$sampling_plot_click_info			# highlighting individual site
		
		isolate({
			plot(spec_sample_curve(session$userData$sim.com, method="rarefaction"), xlim=ranges$x, ylim=ranges$y)
			lines(rare_curve(apply(sampling_quadrats()$spec_dat, 2, function(species) sum(species>0))), lwd=3, col="limegreen")	# Drawing gamma scale curve
			lapply(rarefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
			# for (site in names(rarefaction_curves_list())) {		# verification aid
				# temp=rarefaction_curves_list()[[site]]
				# text(gsub(site, pattern="site", replacement=""), x=10, y=temp[10])
			# }			
		})
		
		if(!is.null(input$sampling_plot_click)) {	# highlight
			lines(rarefaction_curves_list()[[sampling_plot_click_info()]], lwd=4, col="forestgreen")
		}
		
		# if(!is.null(input$rarefaction_curves_plot_click)) {	# highlight
			# lines(rarefaction_curves_list()[[rarefaction_curves_hover_info()]], lwd=4, col="forestgreen")
		# }
	})

	output$previous_rarefaction_curves_plot <- renderPlot({
		input$Restart
		input$keepRarefactionCurvesPlot
		input$rarefaction_curves_plot_dblclick	# zooming in and out
		# input$rarefaction_curves_plot_hover	# highlighting individual site
		# input$sampling_plot_click_info			# highlighting individual site
		
		isolate({
			plot(spec_sample_curve(session$userData$sim.com, method="rarefaction"), xlim=ranges$x, ylim=ranges$y)
			lines(rare_curve(apply(session$userData$previous_sampled_quadrats$spec_dat, 2, function(species) sum(species>0))), lwd=3, col="limegreen")	# Drawing gamma scale curve
			lapply(previous_rarefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
			# for (site in names(previous_rarefaction_curves_list())) {		# verification aid
				# temp=previous_rarefaction_curves_list()[[site]]
				# text(gsub(site, pattern="site", replacement=""), x=10, y=temp[10])
			# }			
		})
		
		# if(!is.null(input$sampling_plot_click)) {	# highlight
			# lines(previous_rarefaction_curves_list()[[sampling_plot_click_info()]], lwd=4, col="forestgreen")
		# }
		
		# if(!is.null(input$rarefaction_curves_plot_hover)) {	# highlight
			# lines(previous_rarefaction_curves_list()[[rarefaction_curves_hover_info()]], lwd=4, col="forestgreen")
		# }
	})

	### Zooming inside the rarefaction curve plot
		# brush on the desired area and double-click
		# When a double-click happens, check if there's a brush on the plot.
		# If so, zoom to the brush bounds; if not, reset the zoom.
		## Desired behaviour: brush to zoom in, double click to zoom out.
	observeEvent(input$rarefaction_curves_plot_dblclick, {
		brush <- input$rarefaction_curves_plot_brush
		if (!is.null(brush)) {
			ranges$x <- c(brush$xmin, brush$xmax)
			ranges$y <- c(brush$ymin, brush$ymax)
		} else {
			ranges$x <- NULL
			ranges$y <- NULL
		}
	})
	
	### Highlighting sampling sites rarefaction curve and table line and quadrat
	#### sampling plot
		# what if there is overlap?
		# what if the quadrats are so small, they are unclickable?

	sampling_plot_click_info <- reactive({	# gives the selected site name
		if(!is.null(input$sampling_plot_click)) {
			x0 <- sampling_quadrats()$xy_dat$x
			y0 <- sampling_quadrats()$xy_dat$y
			size <- sqrt(input$area_of_quadrats)
			
			click <- input$sampling_plot_click
			
			which_site <- logical(input$number_of_quadrats)
			for(site in 1:input$number_of_quadrats) {
				which_site[site] <- (click$x >= x0[site] & click$x < (x0[site] + size) & click$y >= y0[site] & click$y < (y0[site] + size))
			}
			rownames(sampling_quadrats()$xy_dat)[which_site]
		}
	})
	
	
	
	#### rarefaction curve plot
	# rarefaction_curves_hover_info <- reactive({	# gives the selected site name as.character (a site number would be more efficient)
		# if(!is.null(input$rarefaction_curves_plot_hover)) {
			# vectemp <- unlist(rarefaction_curves_list())
			# tabtemp <- data.frame(site = sapply(strsplit(names(vectemp), split="\\."), head, 1),
							# x = as.numeric(sapply(strsplit(names(vectemp), split="\\."), tail, 1)),
							# y = vectemp)

			# hover <- input$rarefaction_curves_plot_hover	# xy coordinates of the cursor
			# distance <- sqrt((hover$x-tabtemp$x)^2+(hover$y-tabtemp$y)^2)
			#if(min(distance) < 3)
				# as.character(tabtemp$site[which.min(distance)])
		# }
	# })
	rarefaction_curves_click_info <- reactive({	# gives the selected site name as.character (a site number would be more efficient)
		if(!is.null(input$rarefaction_curves_plot_click)) {
			vectemp <- unlist(rarefaction_curves_list())
			tabtemp <- data.frame(site = sapply(strsplit(names(vectemp), split="\\."), head, 1),
							x = as.numeric(sapply(strsplit(names(vectemp), split="\\."), tail, 1)),
							y = vectemp)

			click <- input$rarefaction_curves_plot_click	# xy coordinates of the cursor
			distance <- sqrt((click$x-tabtemp$x)^2+(click$y-tabtemp$y)^2)
			##if(min(distance) < 3)
				as.character(tabtemp$site[which.min(distance)])
		}
	})
	
	
	observeEvent(input$keepRarefactionCurvesPlot, autoDestroy = FALSE, {	# adds as many previous plots as clicks. works in conjunction with renderPlot of outpu$PreviousInteractivePlot and in replacement of ui plotOutput(outpu$PreviousInteractivePlot).
		# output$quicktesttext <- renderTable(data.frame(length=length(session$userData$previous_sampled_quadrats),
				# class=class(session$userData$previous_sampled_quadrats),
				# isnull=is.null(session$userData$previous_sampled_quadrats)))
		output$quicktesttext <- renderTable(data.frame(length=length(sampling_quadrats()$spec_dat),
				class=class(sampling_quadrats()$spec_dat),
				isnull=is.null(sampling_quadrats()$spec_dat)))
				
		previous_rarefaction_curves_list <- reactive({
			isolate({
				apply(session$userData$previous_sampled_quadrats$spec_dat, 1, function(site) {
					rare_curve(site)
				})
			})
		})
				
		output$previous_rarefaction_curves_plot <- renderPlot({
			input$rarefaction_curves_plot_dblclick	# zooming in and out
			# input$rarefaction_curves_plot_hover	# highlighting individual site
			# input$sampling_plot_click_info			# highlighting individual site
			
			isolate({
				plot(spec_sample_curve(session$userData$sim.com, method="rarefaction"), xlim=ranges$x, ylim=ranges$y)
				lines(rare_curve(apply(session$userData$previous_sampled_quadrats$spec_dat, 2, function(species) sum(species>0))), lwd=3, col="limegreen")	# Drawing gamma scale curve
				lapply(previous_rarefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
				# for (site in names(previous_rarefaction_curves_list())) {		# verification aid
					# temp=previous_rarefaction_curves_list()[[site]]
					# text(gsub(site, pattern="site", replacement=""), x=10, y=temp[10])
				# }
			})
		})

		
		insertUI(
			selector = "#sampling_alpha_summary_table",
			where = "afterEnd",
			ui = tableOutput("previous_sampling_gamma_table")
			# ui = tableOutput("quicktesttext")
		)
		
		insertUI(
			selector = "#previous_sampling_gamma_table",
			where = "afterEnd",
			ui = tableOutput("previous_sampling_alpha_summary_table")
			# ui = tableOutput("quicktesttext")
		)

		insertUI(
			selector = "#rarefaction_curves_plot",
			where = "afterEnd",
			ui = plotOutput("previous_rarefaction_curves_plot")
		)
		
		insertUI(
			selector = "#sampling_plot",
			where = "afterEnd",
			ui = plotOutput("previous_sampling_plot")
		)
	})
	
	
	
	## Distance decay plots
	output$sampling_distance_decay_plot <- renderPlot({
		input$Restart
		input$new_sampling_button
		
		isolate({
			plot(dist_decay_quadrats(sampling_quadrats(), method = "bray", binary = F), ylim=c(0,1))
		})
	})
	
	
	
	
	
	
	
	
			
	### % of species found
	# output$samplingsimulationsummary <- renderPrint({
		# input$sampling_simulation_button
		# isolate({
			# set.seed(33)
			# withProgress(message = 'Simulating', value = 0, {	# style="old"
				# session$userData$sap_test <- lapply(1:input$nrep_for_sampling_simulation, function(i) {
					# quadrats <- sample_quadrats(session$userData$sim.com, avoid_overlap=T, quadrat_area=input$area_of_quadrats, n_quadrats=input$number_of_quadrats, plot=F)
					# incProgress(1/input$nrep_for_sampling_simulation, detail = paste("Doing repetition", i))
					# return(list(
						# richness = sum(apply(quadrats$spec_dat, 2, sum)>0),
						# standardised_difference = as.numeric(apply(quadrats$spec_dat,2,sum)/sum(quadrats$spec_dat) - table(session$userData$sim.com$census$species)/sum(table(session$userData$sim.com$census$species)))
					# ))
					# }
				# )
			# })
			# session$userData$richness <- unlist(sapply(session$userData$sap_test, function(x) x["richness"]))
			# summary(session$userData$richness)
		# })
	# })
	
	
			
	
	
	### abundance assessment accuracy
	# output$sampling_hist <- renderPlot({
		# input$sampling_simulation_button
		# session$userData$standardised_difference <- sapply(session$userData$sap_test, function(x) x["standardised_difference"])
		# plot(density(unlist(session$userData$standardised_difference)), main="Abundance assessment\naccuracy", las=1)
	# })
	
	
	
	
	
	
	
	
	
	
	
	
	###############################################################################################
	# STEP BY STEP TAB
	sbsvalues <- reactiveValues()
	
	## Parameters
	# update range for species richness, an observed species has minimum one individual
	observe({
		updateSliderInput(session, "sbsS", min=5, max=input$sbsN, value=50, step=5)
	})
	output$sbsCVslider <- renderUI({
		switch(input$sbssad_type,
			"lnorm"=sliderInput("sbscoef", label="CV(abundance), i.e. standard deviation of abundances divided by the mean abundance",value=1, min=0, max=5, step=0.1, ticks=F),
			"geom"=sliderInput("sbscoef", label="Probability of success in each trial. 0 < prob <= 1", value=0.5, min=0, max=1, step=0.1, ticks=F),
			"ls"=textInput("sbscoef", label="Fisher's alpha parameter", value=1)
		)
	})
	
	
	
	## community simulation
	sbssim.com <- reactive({
		input$sbsRestart
    
		isolate({
			# set.seed(229377)	# 229376
		
			spatagg_num <- as.numeric(unlist(strsplit(trimws(input$sbsspatagg), ",")))
			spatcoef_num <- as.numeric(unlist(strsplit(trimws(input$sbsspatcoef), ",")))
		 
			if(input$sbsspatdist=="n.mother") n.mother <- spatcoef_num else n.mother <- NA
			if(input$sbsspatdist=="n.cluster") n.cluster <- spatcoef_num else n.cluster <- NA
			
			simulation_parameters <- list(mother_points=n.mother,
																		cluster_points=n.cluster,
																		xmother=NA,
																		ymother=NA)
									

			switch(input$sbssad_type,
							"lnorm"=sim_thomas_community(s_pool = input$sbsS, n_sim = input$sbsN, 
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sbssad_type, sad_coef=list(cv_abund=input$sbscoef),
								fix_s_sim = T, seed = seed_simulation()),
							"geom"=sim_thomas_community(s_pool = input$sbsS, n_sim = input$sbsN,
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sbssad_type, sad_coef=list(prob=input$sbscoef),
								fix_s_sim = T, seed = seed_simulation()),
							"ls"=sim_thomas_community(s_pool = input$sbsS, n_sim = input$sbsN,
								sad_type = input$sbssad_type, sad_coef=list(N=input$sbsN,alpha=as.numeric(input$sbscoef)),
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								fix_s_sim = T, seed = seed_simulation())
						)

		# session$userData$sbsprevious.sim.com <- sbssim.com()
		
		})
	})

	## Community summary
	output$sbscommunity_summary_table <- renderTable({
		input$sbsRestart
		# data.frame(sbsspatagg=input$sbsspatagg,
			# sbscoef=input$sbscoef,
			# sbsspatcoef=input$sbsspatcoef,
			# sbsspatdist=input$sbsspatdist,
			# sbssad_type=input$sbssad_type,
			# sbsS=input$sbsS,
			# sbsN=input$sbsN)
		# data.frame(isnull=is.null(sbssim.com()),
			# class=class(sbssim.com()),
			# length=length(sbssim.com()))
		# sbssim.com()$census
		data.frame(Community = "",
						n_species = length(levels(sbssim.com()$census$species)),
						n_individuals = nrow(sbssim.com()$census))
	})
	
	## Sampling 
	sbssampling_quadrats <- reactive({
		input$sbsnew_sampling_button
		
		sample_quadrats(comm=sbssim.com(), n_quadrats=input$sbsnumber_of_quadrats, quadrat_area=input$sbsarea_of_quadrats, avoid_overlap=T, plot=F, seed=seed_sampling())
	})
	
	## Sampling summary
	### gamma scale
	sbsgamma_table <- reactive({
		input$sbsRestart
		input$sbsnew_sampling_button
		
		isolate({
			abund <- apply(sbssampling_quadrats()$spec_dat, 2, sum)
			abund <- abund[abund > 0]
			relabund <- abund/sum(abund)
			shannon <- - sum(relabund * log(relabund))
			simpson <- 1- sum(relabund^2)
			data.frame(
							Gamma = "",
							n_species= length(abund),
							shannon = round(shannon, 3),
							# ens_shannon = round(exp(shannon), 3),
							simpson = round(simpson, 3)
							# ens_simpson = round(1/(1 - simpson), 3)
			)
		})
	})
	output$sbsgamma_table <- renderTable(sbsgamma_table())
	

	### alpha scale
	sbsalpha_summary_table <- reactive({
		input$sbsRestart
		input$sbsnew_sampling_button
		
		isolate({
			quadrats_coordinates <- sbssampling_quadrats()$xy_dat
			temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
				div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$sbsarea_of_quadrats), ysize=sqrt(input$sbsarea_of_quadrats), comm=sbssim.com())
			}), 3)))[,c('n_species','n_endemics','shannon','simpson')]
			funs <- list(min=min, max=max, mean=mean, sd=sd)
			data.frame(Alpha=colnames(temp), round(sapply(funs, mapply, temp),3))
		})
	})
	output$sbsalpha_summary_table <- renderTable(sbsalpha_summary_table())

	
	## Plots
	### community and sampling squares
	
	output$sbssampling_plot <- renderPlot({
		input$sbsRestart
		input$sbsnew_sampling_button
		
		isolate({
			quadrats_coordinates <- sbssampling_quadrats()$xy_dat
			plot(sbssim.com(), main = "Community distribution")
			graphics::rect(quadrats_coordinates$x,
								quadrats_coordinates$y,
								quadrats_coordinates$x + sqrt(input$sbsarea_of_quadrats),
								quadrats_coordinates$y + sqrt(input$sbsarea_of_quadrats),
								lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
		})
	})
	
	### distance decay plot
	output$sbsdistance_decay_plot <- renderPlot({
		input$sbsRestart
		input$sbsnew_sampling_button
		
		isolate({
			plot(dist_decay_quadrats(sbssampling_quadrats(), method = "bray", binary = F), ylim=c(0,1))
		})
	})
	
	
	### rarefaction curves
	#### Computing
	sbsrarefaction_curves_list <- reactive({
		apply(sbssampling_quadrats()$spec_dat, 1, function(site) {
			rare_curve(site)
		})
	})
	
	
	#### Plotting
	sbsvalues$ranges <- c(x = NULL, y = NULL)		# used to zoom in the plot
	
	output$sbsrarefaction_curves_plot <- renderPlot({
		input$sbsRestart
		input$sbsnew_sampling_button
		input$sbsrarefaction_curves_plot_dblclick	# zooming in and out
		# input$rarefaction_curves_plot_click	# highlighting individual site
		# input$sampling_plot_click_info			# highlighting individual site
		
		isolate({
			plot(spec_sample_curve(sbssim.com(), method="rarefaction"), xlim=sbsvalues$ranges$x, ylim=sbsvalues$ranges$y)
			lines(rare_curve(apply(sbssampling_quadrats()$spec_dat, 2, function(species) sum(species>0))), lwd=3, col="limegreen")	# Drawing gamma scale curve
			lapply(sbsrarefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
			# for (site in names(sbsrarefaction_curves_list())) {		# verification aid
				# temp=sbsrarefaction_curves_list()[[site]]
				# text(gsub(site, pattern="site", replacement=""), x=10, y=temp[10])
			# }
		})
		
		# if(!is.null(input$sampling_plot_click)) {	# highlight
			# lines(sbsrarefaction_curves_list()[[sampling_plot_click_info()]], lwd=4, col="forestgreen")
		# }
		
		# if(!is.null(input$rarefaction_curves_plot_click)) {	# highlight
			# lines(sbsrarefaction_curves_list()[[rarefaction_curves_hover_info()]], lwd=4, col="forestgreen")
		# }
	})
	
	
	
	output$sbsfirst_step <- renderUI({
		fluidRow(align="center",
			column(width=4,
				tableOutput("sbscommunity_summary_table"),
				tableOutput("sbsgamma_table"),
				tableOutput("sbsalpha_summary_table")
				# dataTableOutput("bigtable_output")
			),
			column(width=4,
				plotOutput("sbssampling_plot")
			),
			column(width=4,
				switch(input$sbsplot_choice, 
					"distance_decay_choice"    = plotOutput("sbsdistance_decay_plot"),
					"rarefaction_curve_choice" = plotOutput("sbsrarefaction_curves_plot")
				)
			),
			hr()
		)
	})
	## end of first step
	
	

	## next steps
	observeEvent(input$sbskeep_step, {
		# ui ID
		divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
		btnID <- paste0(divID, "rmv")
		community_summary_id <- paste0(divID, "comm")
		gamma_table_id <- paste0(divID, "gamm")
		alpha_summary_table_id <- paste0(divID, "alph")
		sampling_plot_ID <- paste0(divID, "samp")
		distance_decay_plot_ID <- paste0(divID, "dist")
		rarefaction_plot_ID <- paste0(divID, "rare")
	
		# only create button if there is none
		if (is.null(sbsvalues[[divID]])) {
      
			insertUI(
				selector = "#sbsfirst_step",
				where="afterEnd",
				ui = tags$div(id = divID,
					fluidRow(
						column(width=4,
							tableOutput(community_summary_id),
							tableOutput(gamma_table_id),
							tableOutput(alpha_summary_table_id),
							actionButton(btnID, "Remove this step", class = "pull-right btn btn-danger")
						),
						column(width=4,
							plotOutput(sampling_plot_ID)
						),
						column(width=4,
							switch(input$sbsplot_choice, 
								"distance_decay_choice"    = plotOutput(distance_decay_plot_ID),
								"rarefaction_curve_choice" = plotOutput(rarefaction_plot_ID)
							)
						)
					)
				)
			)
      
      output[[community_summary_id]] <- renderTable({
			isolate({
				data.frame(Community = "",
							n_species = length(levels(sbssim.com()$census$species)),
							n_individuals = nrow(sbssim.com()$census))
			})
		})
		
      output[[gamma_table_id]] <- renderTable({
			isolate({
				abund <- apply(sbssampling_quadrats()$spec_dat, 2, sum)
				abund <- abund[abund > 0]
				relabund <- abund/sum(abund)
				shannon <- - sum(relabund * log(relabund))
				simpson <- 1- sum(relabund^2)
				data.frame(
								Gamma = "",
								n_species= sum(abund >0),
								shannon = round(shannon, 3),
								# ens_shannon = round(exp(shannon), 3),
								simpson = round(simpson, 3)
								# ens_simpson = round(1/(1 - simpson), 3)
				)
			})
		})      
		
		output[[alpha_summary_table_id]] <- renderTable({
			isolate({
				quadrats_coordinates <- sbssampling_quadrats()$xy_dat
				temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
					div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$sbsarea_of_quadrats), ysize=sqrt(input$sbsarea_of_quadrats), comm=sbssim.com())
				}), 3)))[,c('n_species','n_endemics','shannon','simpson')]
				funs <- list(min=min, max=max, mean=mean, sd=sd)
				data.frame(Alpha=colnames(temp), round(sapply(funs, mapply, temp),3))
			})
		})
		
		output[[sampling_plot_ID]] <- renderPlot({
			isolate({
				quadrats_coordinates <- sbssampling_quadrats()$xy_dat
				plot(sbssim.com(), main = "Community distribution")
				graphics::rect(quadrats_coordinates$x,
									quadrats_coordinates$y,
									quadrats_coordinates$x + sqrt(input$sbsarea_of_quadrats),
									quadrats_coordinates$y + sqrt(input$sbsarea_of_quadrats),
									lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
			})
		})
		
		output[[distance_decay_plot_ID]] <- renderPlot({
			isolate({
				plot(dist_decay_quadrats(sbssampling_quadrats(), method = "bray", binary = F), ylim=c(0,1))
			})
		})
		
		output[[rarefaction_plot_ID]] <- renderPlot({
			isolate({
				plot(spec_sample_curve(sbssim.com(), method="rarefaction"), xlim=sbsvalues$ranges$x, ylim=sbsvalues$ranges$y)
				lines(rare_curve(apply(sbssampling_quadrats()$spec_dat, 2, function(species) sum(species>0))), lwd=3, col="limegreen")	# Drawing gamma scale curve
				lapply(sbsrarefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
			})
		})


      # make a note of the ID of this section, so that it is not repeated accidentally
      sbsvalues[[divID]] <- TRUE
      
      # create a listener on the newly-created button that will remove it from the app when clicked
      observeEvent(input[[btnID]], {
        removeUI(selector = paste0("#", divID))
        
        sbsvalues[[divID]] <- NULL
        
      }, ignoreInit = TRUE, once = TRUE)
      
      # otherwise, print a message to the console
    } else {
      message("The button has already been created!")
    }
  })
  
  
  
  

######################################################################################################################

	# COMPARISON TAB
	
	simtab <- reactive({
		input$compare_selected_simulations
		
		data.frame(input$bigtable)[input$bigtable_output_rows_selected, ]
	})
	
	

#######################################################################################################################

	# SAVING ALL SIMULATIONS
	
	values$saved_simulations <- list()
	
	observeEvent(input$Restart, {	# save mother point coordinates and ranges?
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = session$userData$sim.com
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})
	
	observeEvent(input$new_sampling_button, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = session$userData$sim.com,
									sampled_quadrats = sampling_quadrats()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})	

	observeEvent(input$sbsRestart, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sbssim.com(),
									sampled_quadrats = sbssampling_quadrats()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})
	
	observeEvent(input$sbsnew_sampling_button, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sbssim.com(),
									sampled_quadrats = sbssampling_quadrats()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})	
	
	# Download simulation data list
	output$downloadSimulationList <- downloadHandler(
		filename = function() {paste("Simulation_list.rds", sep="")},
		content  = function(fname) {
			saveRDS(values$saved_simulations, file = fname)
		}
	)
	
	
	
	
	
	
	
	
	
	
	

}) # end of server()





dist_decay_quadrats <- function(samples1, method = "bray", binary = F)
{
   com_mat <- samples1$spec_dat[rowSums(samples1$spec_dat) > 0,]
   d <- stats::dist(samples1$xy_dat[rowSums(samples1$spec_dat) > 0,])

   similarity <- 1 - vegan::vegdist(com_mat, method = method,
                                    binary = binary)
   similarity[!is.finite(similarity)] <- NA

   dat_out <- data.frame(distance = as.numeric(d),
                         similarity = as.numeric(similarity))

   # order by increasing distance
   dat_out <- dat_out[order(dat_out$distance), ]

   class(dat_out) <- c("dist_decay", "data.frame")

   return(dat_out)
}