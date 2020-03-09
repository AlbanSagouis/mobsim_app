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
# library(mobsim, lib.loc="./Library")
# library(ggplot2)
library(markdown)

source("extras/code/Diversity_Area_Relationships.R", local = TRUE)
source("extras/code/rThomas_r.r", local = TRUE)
source("extras/code/Sample_quadrats.R", local = TRUE)
source("extras/code/Sim_Community.R", local = TRUE)
source("extras/code/SAC_spatial.R", local = TRUE)
source("extras/help/Help.r", local = TRUE)
source("extras/help/Labels.r", local = TRUE)
source("extras/graphical_parameters.R", local = TRUE)

comparativeTable_names <- c('sim_ID','method','n_species','n_individuals','seed_simulation','n_quadrats','quadrat_area','seed_sampling',
	 'gamma_richness','gamma_ens_shannon','gamma_ens_simpson',
	 'alpha_mean_richness','alpha_mean_ens_shannon','alpha_mean_ens_simpson'
)
empty_comparativeTable <- function() matrix(NA, nrow=0, ncol=length(comparativeTable_names), dimnames=list(c(), comparativeTable_names))




# Define server logic for slider examples
shinyServer(function(input, output, session) {
   # Global values
	values <- reactiveValues()
	
	# Plotting preferences
	spacolor_palette_individuals <- reactive({
	   do.call(color_palette(), list(n=debounced_spaS()))
	})
	color_palette_individuals <- reactive({
      do.call(color_palette(), list(n=input$S))
	})
	sbscolor_palette_individuals <- reactive({
	   do.call(color_palette(), list(n=input$sbsS))
	})

	
	#########################################################################################################
	# SAD -POPULATION SIMULATION TAB
  
  # update range for species richness, an observed species has minimum one individual
	observeEvent(input$sadN,{
		updateSliderInput(session, "sadS", max=debounced_sadN(), value=debounced_sadS())
	})

	output$sadCVslider <- renderUI({
		switch(input$sadsad_type,
			"lnorm"=sliderInput("sadcoef", label = p("CV (abundance)", tags$style(type="text/css", "#sadCVslider_icon {vertical-align: top;}"),
			                                         popify(bsButton("sadCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                                title = Help$CVsliderLnorm$title, content = Help$CVsliderLnorm$content, trigger = "focus")),
			                    value=1, min=0, max=5, step=0.1, ticks=F),
			"geom"=sliderInput("sadcoef", label = p("Probability of success in each trial. 0 < prob <= 1", tags$style(type="text/css", "#sadCVslider_icon {vertical-align: top;}"),
			                                        popify(bsButton("sadCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                               title = Help$CVsliderGeom$title, content = Help$CVsliderGeom$short_content, trigger = "focus")),
			                   value=0.5,min=0,max=1,step=0.05, ticks=F),
			"ls"=textInput("sadcoef", label = p("Fisher's alpha parameter", tags$style(type="text/css", "#sadCVslider_icon {vertical-align: top;}"),
			                                    popify(bsButton("sadCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                           title = Help$CVsliderLs$title, content = Help$CVsliderLs$content, trigger = "focus")),
			               value = 1)
		)
	})	
	
	
	reactive_sadS <- reactive(input$sadS)
	reactive_sadN <- reactive(input$sadN)
	#  Adding a delay when N and S sliders are triggered too often which can lead R to freeze
	debounced_sadS <- debounce(r = reactive_sadS, millis=1000)
	debounced_sadN <- debounce(r = reactive_sadN, millis=1000)
	
	
	
	sadsim.sad <- reactive({
	   input$sadRestart
	   req(input$sadcoef)
	   
	   switch(input$sadsad_type,
         "lnorm" = sim_sad(s_pool=debounced_sadS(), n_sim=debounced_sadN(), sad_type=input$sadsad_type, sad_coef = list(cv_abund=input$sadcoef), fix_s_sim = TRUE),
         "geom" = sim_sad(s_pool=debounced_sadS(), n_sim=debounced_sadN(), sad_type=input$sadsad_type, sad_coef = list(prob=input$sadcoef), fix_s_sim = TRUE),
         "ls" = sim_sad(s_pool=NA, n_sim=debounced_sadN(), sad_type=input$sadsad_type, sad_coef = list(N=debounced_sadN(), alpha=as.numeric(input$sadcoef)), fix_s_sim = TRUE)
      )
	})
	
	output$sadsad_plots <- renderPlot({
	   par(mfrow=c(1,2))
	   plot(sadsim.sad(), method = "octave")
		plot(sadsim.sad(), method = "rank")
	})
	

	#########################################################################################################
	# SPACE - DISTRIBUTION SIMULATION TAB
  
  # update range for species richness, an observed species has minimum one individual
	observeEvent(debounced_spaN(),{
	   updateSliderInput(session, "spaS", min=5, max=debounced_spaN(), value=debounced_spaS(), step=5)
	})
	
	output$spaCVslider <- renderUI({
	   switch(input$spasad_type,
	          "lnorm"=sliderInput("spacoef", label = p("CV (abundance)", tags$style(type="text/css", "#spaCVslider_icon {vertical-align: top;}"),
	                                                   popify(bsButton("spaCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
	                                                          title = Help$CVsliderLnorm$title, content = Help$CVsliderLnorm$content, trigger = "focus")),
	                              value=1, min=0, max=5, step=0.1, ticks=F),
	          
	          "geom"=sliderInput("spacoef", label = p("Probability of success in each trial. 0 < prob <= 1", tags$style(type="text/css", "#spaCVslider_icon {vertical-align: top;}"),
	                                                  popify(bsButton("spaCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
	                                                         title = Help$CVsliderGeom$title, content = Help$CVsliderGeom$short_content, trigger = "focus")),
	                             value=0.5,min=0,max=1,step=0.05, ticks=F),
	          
	          "ls"=textInput("spacoef", label = p("Fisher's alpha parameter", tags$style(type="text/css", "#spaCVslider_icon {vertical-align: top;}"),
	                                              popify(bsButton("spaCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
	                                                     title = Help$CVsliderLs$title, content = Help$CVsliderLs$content, trigger = "focus")),
	                         value = 10)
	   )
	})	
	
	reactive_spaS <- reactive(input$spaS)
	reactive_spaN <- reactive(input$spaN)
	#  Adding a delay when N and S sliders are triggered too often which can lead R to freeze
	debounced_spaS <- debounce(r = reactive_spaS, millis=1000)
	debounced_spaN <- debounce(r = reactive_spaN, millis=1000)
	
	
	spasim.com <- reactive({
	   input$spaRestart
	   req(input$spacoef)
	   
	   spatagg_num <- as.numeric(unlist(strsplit(trimws(input$spaspatagg), ",")))
	   spatcoef_num <- as.numeric(unlist(strsplit(trimws(input$spaspatcoef), ",")))
	   
	   if(input$spaspatdist=="n.mother") n.mother <- spatcoef_num else n.mother <- NA
	   if(input$spaspatdist=="n.cluster") n.cluster <- spatcoef_num else n.cluster <- NA
	   
	   simulation_parameters <- list(mother_points = n.mother,
                                   cluster_points = n.cluster,
                                   xmother = NA,
                                   ymother = NA,
                                   xrange = c(0,1),
                                   yrange = c(0,1))

	   switch(input$spasad_type,
	          "lnorm"=sim_thomas_community(s_pool = debounced_spaS(), n_sim = debounced_spaN(), 
	                                       sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
	                                       sad_type = input$spasad_type, sad_coef=list(cv_abund=input$spacoef),
	                                       fix_s_sim = T, seed = NULL,
	                                       xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange),
	          "geom"=sim_thomas_community(s_pool = debounced_spaS(), n_sim = debounced_spaN(),
	                                      sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
	                                      sad_type = input$spasad_type, sad_coef=list(prob=input$spacoef),
	                                      fix_s_sim = T, seed = NULL,
	                                      xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange),
	          "ls"=sim_thomas_community(s_pool = NA, n_sim = debounced_spaN(),
	                                    sad_type = input$spasad_type, sad_coef=list(N=debounced_spaN(),alpha=as.numeric(input$spacoef)),
	                                    sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
	                                    fix_s_sim = T, seed = NULL,
	                                    xrange = simulation_parameters$xrange, yrange = simulation_parameters$yrange)
	   )
	})
	

	
	output$spasad_plots <- renderPlot({
		plot(community_to_sad(spasim.com()), method = "rank")
	})
	
	output$spacom_plots <- renderPlot({
	   sac1   <- spec_sample_curve(spasim.com(), method="rarefaction")
	   divar1 <- divar(spasim.com(), exclude_zeros=F)
	   dist1  <- dist_decay(spasim.com())
	  
	   par(mfrow = c(2, 2))
	   
		plot(spasim.com(), main = "Community map", col=spacolor_palette_individuals())
		abline(h=spasim.com()$y_min_max, v=spasim.com()$x_min_max, lty = 3)
	   
		if(input$exercise_number == 3)   {
	      plot(sac1, log = rarefaction_curves_loglog())
	      plot(divar1)
	      plot(dist1)
	   }
		
	}, height = 800)
	
	
	
	
	
	#########################################################################################################
	# BASIC SAMPLING TAB
	
	output$bsacommunity_summary_table <- renderTable({
	   input$spaRestart
	   
	   data.frame(Community = "",
	              n_species = length(levels(spasim.com()$census$species)),
	              n_individuals = nrow(spasim.com()$census))
	})
	## Sampling parameters
	
	## Sampling summary
	### gamma scale
	bsasampling_gamma_table <- reactive({
      abund <- apply(bsasampling_quadrats()$spec_dat, 2, sum)
      abund <- abund[abund > 0]
      relabund <- abund/sum(abund)
      shannon <- - sum(relabund * log(relabund))
      simpson <- 1- sum(relabund^2)
      data.frame(
         Gamma_scale = "",
         n_species= length(abund),
         # shannon = round(shannon, 3),
         ens_shannon = round(exp(shannon), 3),
         # simpson = round(simpson, 3)
         ens_simpson = round(1/(1 - simpson), 3)
      )
	})
	output$bsasampling_gamma_table <- renderTable(bsasampling_gamma_table())
	
	bsasampling_alpha_summary_table <- reactive({
      quadrats_coordinates <- bsasampling_quadrats()$xy_dat
      temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
         div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$bsaarea_of_quadrats), ysize=sqrt(input$bsaarea_of_quadrats), comm=spasim.com())
      }), 3)))[,c('n_species','n_endemics','ens_shannon','ens_simpson')]
      funs <- list(min=min, max=max, mean=mean, sd=sd)
      data.frame(Alpha_scale=colnames(temp), round(sapply(funs, mapply, temp),3))
	})
	output$bsasampling_alpha_summary_table <- renderTable(bsasampling_alpha_summary_table())
	
	
	## Plot the community and sampling squares
	bsasampling_quadrats <- reactive({
	   input$bsanew_sampling_button
	   
	   sample_quadrats(comm=spasim.com(), n_quadrats=input$bsanumber_of_quadrats, quadrat_area=input$bsaarea_of_quadrats, method = "random", avoid_overlap=T, plot=F, seed = NULL)
	})
	
	bsararefaction_curves_list <- reactive({
	   apply(bsasampling_quadrats()$spec_dat, 1, function(site) {
	      rare_curve(site)
	   })
	})
	
	output$bsasampling_plot <- renderPlot({
      quadrats_coordinates <- bsasampling_quadrats()$xy_dat
      plot(spasim.com(), main = "Community distribution", col= spacolor_palette_individuals())
      graphics::rect(quadrats_coordinates$x,
                     quadrats_coordinates$y,
                     quadrats_coordinates$x + sqrt(input$bsaarea_of_quadrats),
                     quadrats_coordinates$y + sqrt(input$bsaarea_of_quadrats),
                     lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
	})
	
	output$bsararefaction_curves_plot <- renderPlot({
	   # input$Restart
	   # input$new_sampling_button
	   # input$rarefaction_curves_plot_brush	# zooming in and out
	   # input$rarefaction_curves_plot_dblclick	# zooming in and out
	   # # input$rarefaction_curves_plot_click	# highlighting individual site
	   # input$sampling_plot_click_info			# highlighting individual site
	   # input$rarefaction_curves_loglog        # update plot when log axes rule change
	   # 
	   # isolate({
	      plot(spec_sample_curve(spasim.com(), method="rarefaction"), log = rarefaction_curves_loglog())
	      lines(rare_curve(apply(bsasampling_quadrats()$spec_dat, 2, sum)), lwd=3, col="limegreen")  	# Drawing gamma scale curve
	      lapply(bsararefaction_curves_list(), lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves

	   
	   # if(!is.null(input$sampling_plot_click)) {	# highlight
	   #    lines(rarefaction_curves_list()[[sampling_plot_click_info()]], lwd=4, col="forestgreen")
	   })
	
	output$bsadivar_plot <- renderPlot({
	   divar1 <- divar(spasim.com(), exclude_zeros=F)
	   plot(divar1)
	   abline(v = input$bsaarea_of_quadrats * input$bsanumber_of_quadrats / ((spasim.com()$x_min_max[2] - spasim.com()$x_min_max[1]) * (spasim.com()$y_min_max[2] - spasim.com()$y_min_max[1])), lty = 2, lwd = 2) # proportion of the total area sampled
	})
	
	output$bsadist_decay_plot <- renderPlot({
	   dist1 <- dist_decay(spasim.com())
	   plot(dist1)
	   
	   dist_quadrats <- dist_decay_quadrats(bsasampling_quadrats(), method = "bray", binary = F)
	   points(similarity ~ distance, data = dist_quadrats, col = "limegreen")
	   dd_loess <- stats::loess(similarity ~ distance, data = dist_quadrats)
	   pred_sim <- stats::predict(dd_loess)
	   graphics::lines(dist_quadrats$distance, pred_sim, col = "limegreen", lwd = 2, add = TRUE)
	})
	   
	
	
	
	
	
	#########################################################################################################
	# SIMULATION TAB
  
  # update range for species richness, an observed species has minimum one individual
	observeEvent(input$N, {
		updateSliderInput(session, "S", max=input$N, value=input$S)
	})
  # update the number of species in the drop down species list.
	observe({
		updateSelectInput(session, "species_ID", "Pick species ID", paste("species", 1:input$S, sep="_"))
	})


	output$select_sad_type <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points"))	{
			return()
		} else {
			selectizeInput("sad_type", label = p(Labels$sad_type, tags$style(type="text/css", "#sad_type_icon {vertical-align: top;}"),
			                                     popify(bsButton("sad_type_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                            title = Help$select_sad_type$title, content = Help$select_sad_type$content, placement = "bottom", trigger = "focus")), 
			               choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls"), selected = "lnorm")
		}					
	})
						
	  
	output$CVslider <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points","click_for_species_ranges") | is.null(input$sad_type))	
			return()
		switch(input$sad_type,
			"lnorm"=sliderInput("coef", label = p("CV (abundance)", tags$style(type="text/css", "#CVsliderLnorm_icon {vertical-align: top;}"),
			                                      popify(bsButton("CVsliderLnorm_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                             title = Help$CVsliderLnorm$title, content = Help$CVsliderLnorm$content, trigger = "focus")),
			                    value=1, min=0, max=5, step=0.1, ticks=F),
			"geom"=sliderInput("coef", label = p("Probability of success in each trial. 0 < prob <= 1", tags$style(type="text/css", "#CVsliderGeom_icon {vertical-align: top;}"),
			                                     popify(bsButton("CVsliderGeom_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                            title = Help$CVsliderGeom$title, content = Help$CVsliderGeom$short_content, trigger = "focus")),
			                   value=0.5,min=0,max=1,step=0.1, ticks=F),
			"ls"=textInput("coef", label = p("Fisher's alpha parameter", tags$style(type="text/css", "#CVsliderLs_icon {vertical-align: top;}"),
			                                 popify(bsButton("CVsliderLs_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                        title = Help$CVsliderLs$title, content = Help$CVsliderLs$content, trigger = "focus")),
			               value = 1)
		)
	})

	output$text_spat_agg <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			textInput(inputId = "spatagg", label = p(Labels$spatagg, tags$style(type="text/css", "#spatagg_icon {vertical-align: top;}"),
			                                         popify(bsButton("spatagg_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                                title = Help$spatagg$title, content = Help$spatagg$content, trigger = "focus")),
			          value = 0.1)
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

	### debugging
	output$debugging_seed <- renderText(paste0("input = ", input$sbssimulation_seed, ", reactive = ", seed_simulation()))

	##		random_mother_points

	output$spatdist <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			selectizeInput(inputId="spatdist", p(Labels$spatdist, tags$style(type="text/css", "#spatdist_icon {vertical-align: top;}"),
			                                     popify(bsButton("spatdist_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                            title = Help$spatdist$title, content = Help$spatdist$content, trigger = "focus")),
			               choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster"), selected = "n.mother")
		}					
	})

	output$spatcoef <- renderUI({
		if (!input$method_type %in% c("random_mother_points","click_for_species_ranges"))	{
			return()
		} else {
			textInput(inputId="spatcoef",label=p(Labels$spatcoef, tags$style(type="text/css", "#spatcoef_icon {vertical-align: top;}"),
			                                     popify(bsButton("spatcoef_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                            title = Help$spatcoef$title, content = Help$spatcoef$content, trigger = "focus")),
			          value="1")
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
		if(input$method_type %in% c("click_for_mother_points","click_for_species_ranges")) {
				# color_vector <- rainbow(input$S)
				color_vector <- color_palette_individuals()
				par(mex=0.6, mar=c(3,3,0,0), cex.axis=0.8)
				plot(x=values$DT$x, y=values$DT$y, col=color_vector[values$DT$species_ID], xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", las=1, asp=1, pch=20)
				abline(h=c(0,1), v=c(0,1), lty=2)
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
			summary(sim.com(), digits=2)
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
	
	### Range values from plot
	observeEvent(input$plot_brush, {
		add_row = data.frame(species_ID = factor(input$species_ID, levels = paste("species", 1:input$S, sep="_")),
									xmin = input$plot_brush$xmin,
									xmax = input$plot_brush$xmax,
									ymin = input$plot_brush$ymin,
									ymax = input$plot_brush$ymax
								 )
		values$DT_species_ranges = rbind(values$DT_species_ranges, add_row)
	})
	
	### uploading range values per species
	output$species_range_uploading_tool <- renderUI({
		if (input$method_type != "click_for_species_ranges")	{
			return()
		} else {
			fileInput(inputId="loaded_ranges", label=p("Choose a data.frame of ranges", tags$style(type="text/css", "#species_range_uploading_tool_icon {vertical-align: top;}"),
			                                           popify(bsButton("species_range_uploading_tool_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                                  title = Help$species_range_uploading_tool$title,
			                                                  content = Help$species_range_uploading_tool$content,
			                                                  placement = "bottom", trigger = "focus")),
			         multiple = FALSE,
						accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"), width = NULL,
						buttonLabel = "Browse...", placeholder = "No file selected")
		}
	})
	
	observeEvent(input$loaded_ranges,{
		req(input$loaded_ranges)
		values$DT_species_ranges <- read.csv(input$loaded_ranges$datapath, h=T)
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
			fileInput(inputId="loaded_file", label=p("Choose rData community File", tags$style(type="text/css", "#community_uploading_tool_icon {vertical-align: top;}"),
			                                         popify(bsButton("community_uploading_tool_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                                title = Help$community_uploading_tool$title,
			                                                content = Help$community_uploading_tool$content,
			                                                placement = "bottom", trigger = "focus")),
			         multiple = FALSE,
						accept = "", width = NULL,
						buttonLabel = "Browse...", placeholder = "No file selected")
		}
	})



	## plot theme
	### plot function
 	plot_layout <- function(sim.com, colors_for_individuals) {
		layout(matrix(c(1,2,3,
							 4,5,6), byrow = T, nrow = 1, ncol = 6),
				 heights = c(1,1), widths=c(1,1,1))
		
		sad1 <- community_to_sad(sim.com)
		sac1 <- spec_sample_curve(sim.com, method="rarefaction")
		divar1 <- divar(sim.com, exclude_zeros=F)
		dist1 <- dist_decay(sim.com)
		
		plot(sad1, method = "octave")
		plot(sad1, method = "rank")
		
		plot(sim.com, main = "Community distribution", col = colors_for_individuals)
		
		plot(sac1, log = rarefaction_curves_loglog())
		plot(divar1)
		plot(dist1)
	}

	
	
	
	
	
   sim.com <- reactive({
      input$Restart
      req(input$coef)
    # validate(
		# need(input$spatdist, label="1"),
		# need(input$spatagg, label="2"),
		# need(input$spatcoef, label="3"),
		# need(input$method_type, label="4"),
		# need(input$sad_type, label="5"),
		# need(input$S, label="S")
	# )

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
			tempsim.com <- switch(input$sad_type,
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
			tempsim.com <- get(load(input$loaded_file$datapath))
		}

		session$userData$previous.sim.com <- tempsim.com
		tempsim.com
	})
	})
		
   output$InteractivePlot <- renderPlot({
      input$Restart
      input$rarefaction_curves_loglog
      isolate({
		   plot_layout(sim.com(), colors_for_individuals = color_palette_individuals())
      })
	})
	
	output$PreviousInteractivePlot <- renderPlot({
		if(!input$keepInteractivePlot){
			return()
		} else {
			plot_layout(session$userData$previous.sim.com, colors_for_individuals = color_palette_individuals())
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
			sim.com <- sim.com()
			save("sim.com", file=fname)
		}
	)
	
	## downloading plot
	output$downloadMobPlot <- downloadHandler(
	
		filename = function() {paste("plotMOB", input$plot_saving_format, sep=".")},
		content  = switch(input$plot_saving_format,
			# "pdf" = function(fname) {
				# pdf(filename=fname, width=input$plot_saving_width, height=input$plot_saving_height)
				# print(plot_layout(sim.com()))
				# dev.off()
			# },
			# "svg" = function(fname) {
				# svg(filename=fname, width=15, height=3)
				# print(plot_layout(sim.com()))
				# dev.off()
			# },
			"png" = function(fname) {
				png(filename=fname, width=15, height=3, units="in", res=300)
				print(plot_layout(sim.com()))
				dev.off()
			},
			"tiff" = function(fname) {
				tiff(filename=fname, width=15, height=3, units="in", res=300)	# input$plot_saving_resolution
				print(plot_layout(sim.com()))
				dev.off()
			}
		)
		# content=function(fname) {
			# png(filename=fname, width=1500, height=300)
			# print(plot_layout(sim.com()))
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
						n_species = length(levels(sim.com()$census$species)),
						n_individuals = nrow(sim.com()$census))
	})
	## Sampling parameters
	
	## Plot the community and sampling squares
	sampling_quadrats <- reactive({
		input$new_sampling_button
		
		sample_quadrats(comm=sim.com(), n_quadrats=input$number_of_quadrats, quadrat_area=input$area_of_quadrats, method = input$sampling_method, avoid_overlap=T, plot=F, seed=seed_sampling())
	})
	
	# previous_sampling_quadrats <- reactive({
		# input$keepRarefactionCurvesPlot
		
		# isolate({
			# sample_quadrats(comm=sim.com(), n_quadrats=input$number_of_quadrats, quadrat_area=input$area_of_quadrats, method = input$sampling_method, avoid_overlap=T, plot=F)
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
			plot(sim.com(), main = "Community distribution", col= color_palette_individuals())
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
			plot(sim.com(), main = "Community distribution", col= color_palette_individuals())
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
							# shannon = round(shannon, 3),
							ens_shannon = round(exp(shannon), 3),
							# simpson = round(simpson, 3)
							ens_simpson = round(1/(1 - simpson), 3)
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
							# shannon = round(shannon, 3),
							ens_shannon = round(exp(shannon), 3),
							# simpson = round(simpson, 3)
							ens_simpson = round(1/(1 - simpson), 3)
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
				t(round(sapply(1:nrow(quadrats_coordinates), function(i) div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=sim.com())), 3))[,c('n_species','n_endemics','ens_shannon','ens_simpson')],
				options=list(searching = FALSE, info = TRUE, sort = TRUE))
		})
	})
	
	sampling_alpha_summary_table <- reactive({
		input$Restart
		input$new_sampling_button
		isolate({
			quadrats_coordinates <- sampling_quadrats()$xy_dat
			temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
				div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=sim.com())
			}), 3)))[,c('n_species','n_endemics','ens_shannon','ens_simpson')]
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
				div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$area_of_quadrats), ysize=sqrt(input$area_of_quadrats), comm=sim.com())
			}), 3)))[,c('n_species','n_endemics','ens_shannon','ens_simpson')]
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
		input$rarefaction_curves_plot_brush	# zooming in and out
		input$rarefaction_curves_plot_dblclick	# zooming in and out
		# input$rarefaction_curves_plot_click	# highlighting individual site
		input$sampling_plot_click_info			# highlighting individual site
		input$rarefaction_curves_loglog        # update plot when log axes rule change
		
		isolate({
			plot(spec_sample_curve(sim.com(), method="rarefaction"), xlim=ranges$x, ylim=ranges$y, log = rarefaction_curves_loglog())
			lines(rare_curve(apply(sampling_quadrats()$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
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
		input$rarefaction_curves_plot_brush	   # zooming in and out
		input$rarefaction_curves_plot_dblclick	# zooming in and out
		# input$rarefaction_curves_plot_hover	# highlighting individual site
		# input$sampling_plot_click_info			# highlighting individual site
		input$rarefaction_curves_loglog        # update plot when log axes rule change
		
		isolate({
			plot(spec_sample_curve(sim.com(), method="rarefaction"), xlim=ranges$x, ylim=ranges$y, log = rarefaction_curves_loglog())
			lines(rare_curve(apply(session$userData$previous_sampled_quadrats$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
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
		# brush to zoom in, double click to zoom out.
	observeEvent(input$rarefaction_curves_plot_brush, {
		ranges$x <- c(input$rarefaction_curves_plot_brush$xmin, input$rarefaction_curves_plot_brush$xmax)
		ranges$y <- c(input$rarefaction_curves_plot_brush$ymin, input$rarefaction_curves_plot_brush$ymax)
	})
	
	observeEvent(input$rarefaction_curves_plot_dblclick, {
      ranges$x <- NULL   # sum(sampling_quadrats()$spec_dat)
      ranges$y <- NULL   # as.numeric(sim.com()$n_species)
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
		   input$rarefaction_curves_plot_brush	   # zooming in and out
		   input$rarefaction_curves_plot_dblclick	# zooming in and out
			# input$rarefaction_curves_plot_hover	# highlighting individual site
			# input$sampling_plot_click_info			# highlighting individual site
		   input$rarefaction_curves_loglog        # update plot when log axes rule change
		   
			isolate({
				plot(spec_sample_curve(sim.com(), method="rarefaction"), xlim=ranges$x, ylim=ranges$y, log = rarefaction_curves_loglog())
				lines(rare_curve(apply(session$userData$previous_sampled_quadrats$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
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
					# quadrats <- sample_quadrats(sim.com(), avoid_overlap=T, quadrat_area=input$area_of_quadrats, n_quadrats=input$number_of_quadrats, plot=F)
					# incProgress(1/input$nrep_for_sampling_simulation, detail = paste("Doing repetition", i))
					# return(list(
						# richness = sum(apply(quadrats$spec_dat, 2, sum)>0),
						# standardised_difference = as.numeric(apply(quadrats$spec_dat,2,sum)/sum(quadrats$spec_dat) - table(sim.com()$census$species)/sum(table(sim.com()$census$species)))
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
		updateSliderInput(session, "sbsS", max=input$sbsN, value=input$sbsS)   # does not work? app freezes when N value is set to a smaller value than S
	})
	
	output$sbsCVslider <- renderUI({
		switch(input$sbssad_type,
			"lnorm"=sliderInput("sbscoef", label=p("CV (abundance)", tags$style(type="text/css", "#sbsCVslider_icon {vertical-align: top;}"),
			                                      popify(bsButton("sbsCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                             title = Help$CVsliderLnrom$title, content = Help$CVsliderLnorm$content, trigger = "focus")),
			                    value=1, min=0, max=5, step=0.1, ticks=F),
			"geom"=sliderInput("sbscoef", label=p("Probability of success in each trial. 0 < prob <= 1", tags$style(type="text/css", "#sbsCVslider_icon {vertical-align: top;}"),
			                                     popify(bsButton("sbsCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                            title = Help$CVsliderGeom$title, content = Help$CVsliderGeom$short_content, trigger = "focus")),
			                   value=0.5, min=0, max=1, step=0.1, ticks=F),
			"ls"=textInput("sbscoef", label=p("Fisher's alpha parameter", tags$style(type="text/css", "#sbsCVslider_icon {vertical-align: top;}"),
			                                 popify(bsButton("sbsCVslider_icon", label="", icon=icon("question-circle"), size="extra-small"),
			                                        title = Help$CVsliderLs$title, content = Help$CVsliderLs$content, trigger = "focus")),
			               value=1)
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
							"ls"=sim_thomas_community(s_pool = NA, n_sim = input$sbsN,
								sad_type = input$sbssad_type, sad_coef=list(N=input$sbsN, alpha=as.numeric(input$sbscoef)),
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
							# shannon = round(shannon, 3),
							ens_shannon = round(exp(shannon), 3),
							# simpson = round(simpson, 3)
							ens_simpson = round(1/(1 - simpson), 3)
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
			}), 3)))[,c('n_species','n_endemics','ens_shannon','ens_simpson')]
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
			plot(sbssim.com(), main = "Community distribution", col= sbscolor_palette_individuals())
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
		input$rarefaction_curves_loglog        # update plot when log axes rule change
		
		isolate({
			plot(spec_sample_curve(sbssim.com(), method="rarefaction"), xlim=sbsvalues$ranges$x, ylim=sbsvalues$ranges$y, log = rarefaction_curves_loglog())
			lines(rare_curve(apply(sbssampling_quadrats()$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
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
				# dataTableOutput("comparativeTable_output")
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
								# shannon = round(shannon, 3),
								ens_shannon = round(exp(shannon), 3),
								# simpson = round(simpson, 3)
								ens_simpson = round(1/(1 - simpson), 3)
				)
			})
		})      
		
		output[[alpha_summary_table_id]] <- renderTable({
			isolate({
				quadrats_coordinates <- sbssampling_quadrats()$xy_dat
				temp <- 	as.data.frame(t(round(sapply(1:nrow(quadrats_coordinates), function(i) {
					div_rect(x0=quadrats_coordinates$x[i], y0=quadrats_coordinates$y[i], xsize=sqrt(input$sbsarea_of_quadrats), ysize=sqrt(input$sbsarea_of_quadrats), comm=sbssim.com())
				}), 3)))[,c('n_species','n_endemics','ens_shannon','ens_simpson')]
				funs <- list(min=min, max=max, mean=mean, sd=sd)
				data.frame(Alpha=colnames(temp), round(sapply(funs, mapply, temp),3))
			})
		})
		
		output[[sampling_plot_ID]] <- renderPlot({
			isolate({
				quadrats_coordinates <- sbssampling_quadrats()$xy_dat
				plot(sbssim.com(), main = "Community distribution", col= sbscolor_palette_individuals())
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
				plot(spec_sample_curve(sbssim.com(), method="rarefaction"), xlim=sbsvalues$ranges$x, ylim=sbsvalues$ranges$y, log = rarefaction_curves_loglog())
				lines(rare_curve(apply(sbssampling_quadrats()$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
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

	# COMPARISON
	
	simtab <- reactive({		
		as.data.frame(values$comparativeTable[input$comparativeTable_output_rows_selected, ], stringsAsFactors=FALSE)
	})
	
	output$simtab_output <- renderTable(simtab())
	
	output$compplot_types_selection <- renderUI({
	   if(input$compplot_style == "Split") {
	      checkboxGroupInput(inputId="compplot_types", label="Plot types", choices=c("Community map", "Distance decay", "Rarefaction curve"), selected="Rarefaction curve")
	   } else {
	      return()
	   }
   })
	
	
	
	## PLOTTING
	### Preparation
	nplot <- reactive(length(input$compplot_types))
	nsim <- reactive(nrow(simtab()))
	simlist <- reactive(values$saved_simulations[as.character(simtab()$sim_ID)])
	
	### Zooming tools
	compranges <- reactiveValues(x = NULL, y = NULL)
	
	observeEvent(input$comparativeTable_output_rows_selected, {
		compranges$x <- c(1, max(unlist(lapply(simlist(), function(sim) sum(sim$sampled_quadrats$spec_dat)))))	# why sapply does not work?
		compranges$y <- c(1, max(as.numeric(simtab()$n_species)))
	})
	
	observeEvent(input$comparison_plot_brush, {
		if (!is.null(input$comparison_plot_brush)) {
			compranges$x <- c(input$comparison_plot_brush$xmin, input$comparison_plot_brush$xmax)
			compranges$y <- c(input$comparison_plot_brush$ymin, input$comparison_plot_brush$ymax)
		} else {
			compranges$x <- c(1, max(sapply(simlist(), function(sim) sum(sim$sampled_quadrats$spec_dat))))
			compranges$y <- c(1, max(as.numeric(simtab()$n_species)))
		}
	})
	observeEvent(input$comparison_plot_dblclick, {
		compranges$x <- c(1, max(sapply(simlist(), function(sim) sum(sim$sampled_quadrats$spec_dat))))
		compranges$y <- c(1, max(as.numeric(simtab()$n_species)))
	})
	
	
	output$comp_plot <- renderPlot({
		if(is.null(input$comparativeTable_output_rows_selected)) {
			return()
		} else {
			
			if(input$compplot_style == "Split")	{
				par(mfcol = c(nplot(), nsim()), mex=.6, mar=c(2.5,3,3,3))
				lapply(simlist(), function(sim) {
					if("Community map" %in% input$compplot_types) {
						plot(sim$community, main= paste0("sim ID ", sim$sim_ID), col = color_palette_individuals())	# Plotting the community
						graphics::rect(sim$sampled_quadrats$xy_dat$x,	# Plotting quadrats
											sim$sampled_quadrats$xy_dat$y,
											sim$sampled_quadrats$xy_dat$x + sqrt(sim$quadrats_area),
											sim$sampled_quadrats$xy_dat$y + sqrt(sim$quadrats_area),
											lwd = 2, col = grDevices::adjustcolor("white", alpha.f = 0.6))
					}
					if("Rarefaction curve" %in% input$compplot_types) {
						plot(spec_sample_curve(sim$community, method="rarefaction"), log = rarefaction_curves_loglog())	# Plotting rarefaction curves
						lines(rare_curve(apply(sim$sampled_quadrats$spec_dat, 2, sum)), lwd=3, col="limegreen")	# Drawing gamma scale curve
						lapply(sim$rarefaction_curve_list, lines, lwd=2, col=adjustcolor("green", alpha=0.5))	# Drawing all alpha scale curves
					}
					if("Distance decay" %in% input$compplot_types) {
						plot(dist_decay_quadrats(sim$sampled_quadrats, method = "bray", binary = F), ylim=c(0,1))	# Plotting distance decay
					}
				})
			}
			
			if(input$compplot_style == "Stacked")	{
				# community_x_limits <- range(sapply(simlist(), function(sim) sim$community$x_min_max))
				# community_y_limits <- range(sapply(simlist(), function(sim) sim$community$y_min_max))

				colorList <- if(palette_tab[palette_tab$palette_name == color_palette(), "palette_type"] == "continuous") { # if there is no maximal number of values
				   do.call(color_palette(), list(n=nsim()))
				} else {
   				   if(nsim() > as.numeric(as.character(palette_tab[palette_tab$palette_name == color_palette(), "palette_max_number"])))   { # ca ca ne marche que si la palette est discrete
   				   viridis(nsim())
   				} else {
   			      do.call(color_palette(), list(n=nsim()))
   				}
				}
				names(colorList) <- as.character(simtab()$sim_ID)
				
				plot(x=NA, y=NA, type="n",
					# xlim=c(0, max(as.numeric(simtab()$n_individuals))),
					xlim=compranges$x,
					ylim=compranges$y,
					xlab="Number of sampled individuals", ylab="Number of species", log = rarefaction_curves_loglog())
				
				lapply(simlist(), function(sim)	{	# gamma scale
					lines(rare_curve(apply(sim$sampled_quadrats$spec_dat, 2, sum)), lwd=3, col=colorList[as.character(sim$sim_ID)])
					lapply(sim$rarefaction_curve_list, lines, lwd=2, col=adjustcolor(colorList[as.character(sim$sim_ID)], alpha=0.5))	# alpha scale curves
				})
				legend("topleft", legend=names(colorList), col=colorList, bty="n", pch=19)
			}
		}
	})#, width=ifelse(is.null(nsim()), NA, function(){300*nsim()}), height=ifelse(is.null(nplot()), NA, function(){350*nplot()}))
	
	output$debugging_simulation_table <- renderText({
		# paste("class selected: ", class(input$comparativeTable_output_rows_selected), ", length selected: ", length(input$comparativeTable_output_rows_selected), ", class comparativeTable: ", class(values$comparativeTable), ", class simtab: ", class(simtab())[1], ", nrow: ", nrow(simtab()), ", nrowdataframesimtab: ", nrow(as.data.frame(simtab())), ", length sim list: ", length(values$saved_simulations), ", radio buttons:", input$compplot_types)
		paste0("class simtab()$n_individuals: ", class(simtab()$n_individuals))
	})

#######################################################################################################################

	# SAVING ALL SIMULATIONS
	
	values$saved_simulations <- list()
	
	observeEvent(input$Restart, {	# save mother point coordinates and ranges?
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sim.com()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})
	
	observeEvent(input$new_sampling_button, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sim.com(),
									quadrats_area = input$area_of_quadrats,
									sampled_quadrats = sampling_quadrats(),
									rarefaction_curve_list = rarefaction_curves_list()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})	

	observeEvent(input$sbsRestart, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sbssim.com(),
									quadrats_area = input$area_of_quadrats,
									sampled_quadrats = sbssampling_quadrats(),
									rarefaction_curve_list = sbsrarefaction_curves_list()
								)
			names(values$saved_simulations)[names(values$saved_simulations) == "templist"] <- session$userData$sim_ID
		})
	})
	
	observeEvent(input$sbsnew_sampling_button, {
		isolate({
			values$saved_simulations$templist <- list(
									sim_ID = session$userData$sim_ID,
									community = sbssim.com(),
									quadrats_area = input$area_of_quadrats,
									sampled_quadrats = sbssampling_quadrats(),
									rarefaction_curve_list = sbsrarefaction_curves_list()
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
	
	
	
	
	
	
	
	
	# SPECIES BY SPECIES SIMULATION
	# spe prefix
	
	
	speSinitial <- 5
	speN <- 100
	spesad_type <- "lnorm"
	specoef <- 1
	
   spe <- reactiveValues(S = speSinitial,
                         sad = sim_sad(s_pool=speSinitial, n_sim=speN, sad_type=spesad_type, sad_coef = list(cv_abund=specoef), fix_s_sim = TRUE))
	# S
   observeEvent(input$speS, {
      spe$S <- input$speS
   })
   
	# reactive_speS <- reactive(spe$S)
	#  Adding a delay when N and S sliders are triggered too often which can lead R to freeze
	# debounced_speS <- debounce(r = reactive_speS, millis=500)
	
   
	
	# SAD
	spesim.sad <- reactive({
	   # input$speS
	   input$speRestart
	   # req(input$specoef)
	   # req(debounced_speS)
	   isolate({
   	   sad <- switch(spesad_type,
   	          "lnorm" = sim_sad(s_pool=spe$S, n_sim=speN, sad_type="lnorm", sad_coef = list(cv_abund=specoef), fix_s_sim = TRUE),
   	          "geom" = sim_sad(s_pool=spe$S, n_sim=speN, sad_type="geom", sad_coef = list(prob=specoef), fix_s_sim = TRUE)
   	   )
         spe$sad <- sad
         return(sad)
	   })
	})
	
   
   
	observeEvent(input$speAddSpecies, {
	   # S
	   newSpeciesNumbers <- (spe$S+1):(spe$S + input$speNewS)
	   spe$S <- spe$S + input$speNewS
	   # SAD
	   spe$sad <- c(spe$sad, rep(input$speNewN, input$speNewS))
	   names(spe$sad)[newSpeciesNumbers] <- paste0("species", newSpeciesNumbers)
	   class(spe$sad) <- c("sad", "integer")
	   # Update input$speS
	   updateNumericInput(session, inputId = 'speS', value = spe$S)
	})
	
	
	output$spesad_plots <- renderPlot({
	   req(spesim.sad())
	   par(mfrow=c(1,2))
	   plot(spe$sad, method = "octave")
	   plot(spe$sad, method = "rank")
	})
	
	
	output$speCommunity_text <- renderText(paste0(spe$S, " species, and length spesim.sad(): ", length(spe$sad), ", and class spesim.sad(): ", class(spe$sad)[1], ", last species name=", names(spe$sad)[length(spe$sad)]))
	
	
	
	
	
	
	
	
	
	
	# GRAPHICAL PARAMETERS TAB
	## Palettes
	
	observe({
	   updateSelectInput(session, "color_palette", selected = color_palette())
	})
	
	output$discrete_palettes <- renderPlot({
	   labs <- paste0(palette_tab$palette_name, ifelse( palette_tab$palette_type == "discrete", "(max ", ""),
	                  palette_tab$palette_max_number, ifelse( palette_tab$palette_type == "discrete", ")", ""))
	   par(mar=c(0,5,1,1), cex=2)
	   pal.bands(alphabet(), alphabet2(), cols25(), glasbey(), kelly(), okabe(), polychrome(), stepped(), stepped2(), stepped3(), tol(), watlington(), brewer.paired(12), 
	             cubehelix, gnuplot, parula, tol.rainbow, cividis, brewer.spectral, brewer.brbg,ocean.thermal, ocean.curl, ocean.haline, inferno, plasma, viridis,
	             labels=labs, show.names=FALSE)
	}, width=520, height=600)
	
	output$CBF_test_plot <- renderPlot({# Check colorblindness suitability
	   if(input$CBF_test){
	      par(mar=c(0,4,1,1), cex=2)
	      if(palette_tab[palette_tab$palette_name == color_palette(), "palette_max_number"] == "discrete")  {
   	      pal.safe(pal = get(input$color_palette),
   	               n = as.numeric(as.character(palette_tab[palette_tab$palette_name == input$color_palette, "palette_max_number"]))) # call the proper pal function with the string, add the max number of colors
	      } else {
	         pal.safe(pal = get(input$color_palette), main = input$color_palette) # why issues when replaced by color_palette()?
	      }
	   } else {
	      return()
	   }
	}, width=520, height=600)
	
	output$clicktext <- renderText({
	   paste0("y = ", round(as.numeric(input$discrete_palettes_click$y), 2), ", pal = ", color_palette(), ", class = ", class(color_palette()), ", loglog = ", paste(input$rarefaction_curves_loglog, collapse=""))
	})
	
	color_palette <- reactive({
	   if(is.null(input$discrete_palettes_click$y)) {
	      input$color_palette
	   } else {
	      as.character(palette_tab[nrow(palette_tab) - round(input$discrete_palettes_click$y, 1) + 1, "palette_name"])
	   }
	})

	
	rarefaction_curves_loglog <- reactive({
	   paste(input$rarefaction_curves_loglog, collapse = "")
	})
	
	
	
	# HELP ICONS

	output$simulation_seed_icon <- renderUI(icon("question-circle"))
	output$sampling_seed_icon <- renderUI(icon("question-circle"))
	output$area_of_quadrats_icon <- renderUI(icon("question-circle"))
	
	output$keep_step_icon <- renderUI(icon("question-circle"))
	
	output$color_palette_icon <- renderUI(icon("question-circle"))
	output$rarefaction_curves_plot_icon <- renderUI(icon("question-circle"))
	output$sampling_plot_icon <- renderUI(icon("question-circle"))
	
	output$comparativeTable_output_icon <- renderUI(icon("question-circle"))
	output$downloadSimulationTable_icon <- renderUI(icon("question-circle"))
	output$downloadSimulationList_icon <- renderUI(icon("question-circle"))
	
	
	
	# Big table tab
	
	session$userData$sim_ID <- 1
	values$comparativeTable <- empty_comparativeTable()
	
	## Adding rows to the simulation table
	### From Simulation and sampling tabs
	#### Restart button
	observeEvent(
	   input$Restart, {
	      
	      isolate({
	         session$userData$sim_ID <- session$userData$sim_ID + 1
	         values$comparativeTable <- rbind(values$comparativeTable, c(
	            sim_ID = session$userData$sim_ID,
	            method = input$method_type,
	            n_species = input$S,
	            n_individuals = input$N,
	            seed_simulation = seed_simulation(),
	            n_quadrats = NA,
	            quadrat_area =NA,
	            seed_sampling = NA,
	            gamma_richness = NA,
	            gamma_ens_shannon = NA,
	            gamma_ens_simpson = NA,
	            alpha_mean_richness = NA,
	            alpha_mean_ens_shannon = NA,
	            alpha_mean_ens_simpson = NA
	         ))
	      })
	   })
	#### New sampling button
	observeEvent(
	   input$new_sampling_button, {
	      
	      isolate({
	         session$userData$sim_ID <- session$userData$sim_ID + 1
	         values$comparativeTable <- rbind(values$comparativeTable, c(
	            sim_ID = session$userData$sim_ID,
	            method = input$method_type,
	            n_species = input$S,
	            n_individuals = input$N,
	            seed_simulation = seed_simulation(),
	            n_quadrats = input$number_of_quadrats,
	            quadrat_area = input$area_of_quadrats,
	            seed_sampling = seed_sampling(),
	            gamma_richness = sampling_gamma_table()$n_species,
	            gamma_ens_shannon = sampling_gamma_table()$shannon,
	            gamma_ens_simpson = sampling_gamma_table()$simpson,
	            alpha_mean_richness = sampling_alpha_summary_table()["n_species","mean"],
	            alpha_mean_ens_shannon = sampling_alpha_summary_table()["shannon","mean"],
	            alpha_mean_ens_simpson = sampling_alpha_summary_table()["simpson","mean"]
	         ))
	      })
	   })
	
	### From step-by-step tab
	#### Restart button
	observeEvent(
	   input$sbsRestart, {
	      
	      isolate({
	         session$userData$sim_ID <- session$userData$sim_ID + 1
	         values$comparativeTable <- rbind(values$comparativeTable, c(
	            sim_ID = session$userData$sim_ID,
	            method = "random_mother_points",
	            n_species = input$sbsS,
	            n_individuals = input$sbsN,
	            seed_simulation = seed_simulation(),
	            n_quadrats = input$sbsnumber_of_quadrats,
	            quadrat_area = input$sbsarea_of_quadrats,
	            seed_sampling = seed_sampling(),
	            gamma_richness = sbsgamma_table()$n_species,
	            gamma_ens_shannon = sbsgamma_table()$shannon,
	            gamma_ens_simpson = sbsgamma_table()$simpson,
	            alpha_mean_richness = sbsalpha_summary_table()["n_species","mean"],
	            alpha_mean_ens_shannon = sbsalpha_summary_table()["shannon","mean"],
	            alpha_mean_ens_simpson = sbsalpha_summary_table()["simpson","mean"]
	         ))
	      })
	   })
	#### New sampling button
	observeEvent(
	   input$sbsnew_sampling_button, {
	      
	      isolate({
	         session$userData$sim_ID <- session$userData$sim_ID + 1
	         values$comparativeTable <- rbind(values$comparativeTable, c(
	            sim_ID = session$userData$sim_ID,
	            method = "random_mother_points",
	            n_species = input$sbsS,
	            n_individuals = input$sbsN,
	            seed_simulation = seed_simulation(),
	            n_quadrats = input$sbsnumber_of_quadrats,
	            quadrat_area = input$sbsarea_of_quadrats,
	            seed_sampling = seed_sampling(),
	            gamma_richness = sbsgamma_table()$n_species,
	            gamma_ens_shannon = sbsgamma_table()$shannon,
	            gamma_ens_simpson = sbsgamma_table()$simpson,
	            alpha_mean_richness = sbsalpha_summary_table()["n_species","mean"],
	            alpha_mean_ens_shannon = sbsalpha_summary_table()["shannon","mean"],
	            alpha_mean_ens_simpson = sbsalpha_summary_table()["simpson","mean"]
	         ))
	      })
	   })
	
	## Remove all simulations
	observeEvent(input$rem_all_simulations, {
	   values$comparativeTable <- empty_comparativeTable()
	})
	
	## Remove selected simulations
	observeEvent(input$rem_selected_simulations, {
	   values$comparativeTable <- values$comparativeTable[-as.numeric(input$comparativeTable_output_rows_selected), ]
	})
	
	## Download simulation table
	output$downloadSimulationTable <- downloadHandler(
	   filename = function() {paste("Simulation_table.csv", sep="")},
	   content  = function(fname) {
	      write.csv(values$comparativeTable, file=fname)
	   }
	)
	
	## render comparativeTable
	output$comparativeTable_output <- renderDataTable(
	   DT::datatable(values$comparativeTable, options = list(searching=FALSE, pageLength=20))
	)
	output$comparativeTable_selected_simulations <- renderPrint(paste(values$comparativeTable[as.numeric(input$comparativeTable_output_rows_selected), "sim_ID"], collapse=", "))
	
	
	
	
	
	
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