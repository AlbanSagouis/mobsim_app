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
library(ggplot2)

# Define server logic for slider examples
shinyServer(function(input, output, session) {
  
  # update range for species richness, an observed species has minimum one individual
	observe({
		updateSliderInput(session, "S", min=5, max=input$N, value=5, step=5)
	})
  # update the number of species in the drop down species list.
	observe({
		updateSelectInput(session, "species_ID", "Pick species ID", paste("species", 1:input$S, sep="_"))
	})

  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                         y = numeric(),
                         species_ID = factor())

output$select_sad_type <- renderUI({
	if (!input$method_type %in% c("random_mother_points","click_for_mother_points"))	{
		return()
	} else {
		selectizeInput("sad_type", "SAD Type", choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls"))
	}					
})
					
  
output$CVslider <- renderUI({
	if (!input$method_type %in% c("random_mother_points","click_for_mother_points") | is.null(input$sad_type))	
		return()
	switch(input$sad_type,
		"lnorm"=sliderInput("coef", label="CV(abundance), i.e. standard deviation of abundances divided by the mean abundance",value=1, min=0, max=5, step=0.1, ticks=F),
		"geom"=sliderInput("coef",label="Probability of success in each trial. 0 < prob <= 1",value=0.5,min=0,max=1,step=0.1, ticks=F),
		"ls"=textInput("coef",label="Fisher's alpha parameter",value=1)
	)
})

output$text_spat_agg <- renderUI({
	if (!input$method_type %in% c("random_mother_points","click_for_mother_points"))	{
		return()
	} else {
		textInput(inputId="spatagg", label="Spatial Aggregation (mean distance from mother points)", value = 0.1)
	}			
})


##		random_mother_points

output$spatdist <- renderUI({
	if (input$method_type != "random_mother_points")	{
		return()
	} else {
		selectizeInput(inputId="spatdist", "Cluster parameter", choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster"))
	}					
})

output$spatcoef <- renderUI({
	if (input$method_type != "random_mother_points")	{
		return()
	} else {
		textInput(inputId="spatcoef",label="Integer values separated by commas", value="1")
	}					
})



##		click_for_mother_points


output$species_ID_input <- renderUI({
	if (input$method_type != "click_for_mother_points")	{
		return()
	} else {
		selectInput("species_ID", "Pick species ID", paste("species", 1:input$S, sep="_"))
	}
})

output$on_plot_selection <- renderPlot({
  if (is.null(input$method_type)) {
		return()
	} else {
		if(input$method_type=="click_for_mother_points") {
				color_vector <- rainbow(input$S)
				par(mex=0.6, mar=c(3,3,0,0), cex.axis=0.8)
				plot(x=values$DT$x, y=values$DT$y, col=color_vector[values$DT$species_ID], xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", las=1, asp=1, pch=20)
				abline(h=c(0,1), v=c(0,1), lty=2)
		}
	}
})


output$rem_point_button <- renderUI({
	if (input$method_type != "click_for_mother_points")	{
		return()
	} else {
		actionButton("rem_point", "Remove Last Point")
	}
})

output$rem_all_points_button <- renderUI({
	if (input$method_type != "click_for_mother_points")	{
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
 rem_row = values$DT[-(1:nrow(values$DT)), ]
 values$DT = rem_row
})
	 
observeEvent(input$rem_point, {
 rem_row = values$DT[-nrow(values$DT), ]
 values$DT = rem_row
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
if (input$method_type != "click_for_mother_points")	{
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
		
		set.seed(229376)
		
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
      
		set.seed(229376)
		
		
			spatagg_num <- as.numeric(unlist(strsplit(trimws(input$spatagg), ",")))
			spatcoef_num <- as.numeric(unlist(strsplit(trimws(input$spatcoef), ",")))
		 
			if(input$spatdist=="n.mother") n.mother <- spatcoef_num else n.mother <- NA
			if(input$spatdist=="n.cluster") n.cluster <- spatcoef_num else n.cluster <- NA
			
			simulation_parameters <- switch(input$method_type,
									"random_mother_points"=list(mother_points=n.mother,
																		cluster_points=n.cluster,
																		xmother=NA,
																		ymother=NA),
									"click_for_mother_points"=list(mother_points=NA,
																		cluster_points=NA,
																		xmother=tapply(values$DT$x, values$DT$species_ID, list),
																		ymother=tapply(values$DT$y, values$DT$species_ID, list))
									)
									


			# sim.com might rather be set as a reactive ({ }) that is then called sim.com(). is this more efficient ?
			session$userData$sim.com <- switch(input$sad_type,
							"lnorm"=sim_thomas_community(s_pool = input$S, n_sim = input$N, 
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sad_type, sad_coef=list(cv_abund=input$coef),
								fix_s_sim = T),
							"geom"=sim_thomas_community(s_pool = input$S, n_sim = input$N,
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								sad_type = input$sad_type, sad_coef=list(prob=input$coef),
								fix_s_sim = T),
							"ls"=sim_thomas_community(s_pool = input$S, n_sim = input$N,
								sad_type = input$sad_type, sad_coef=list(N=input$N,alpha=as.numeric(input$coef)),
								sigma=spatagg_num, mother_points=simulation_parameters$mother_points, cluster_points=simulation_parameters$cluster_points, xmother=simulation_parameters$xmother, ymother=simulation_parameters$ymother,
								fix_s_sim = T)
						)
		} else {
			session$userData$sim.com <- get(load(input$loaded_file$datapath))
		}

		plot_layout(session$userData$sim.com)
		session$userData$previous.sim.com <- session$userData$sim.com
		
	})
	})
		
	

	
	output$PreviousInteractivePlot <- renderPlot({
		if(!input$keep){
			return()
		} else {
			plot_layout(session$userData$previous.sim.com)
		}
	})
	
	# observeEvent(input$keep, {	# adds as many previous plots as clicks. works in conjunction with renderPlot of outpu$PreviousInteractivePlot and in replacement of ui plotOutput(outpu$PreviousInteractivePlot).
		# insertUI(
			# selector = "#InteractivePlot",
			# where = "beforeBegin",
			# ui = plotOutput("PreviousInteractivePlot", height="300px",width="1500px")
		# )
	# })
	
	
	# Sampling
	## Sampling parameters
	
	## Plot the community and sampling squares
	output$sampling_plot <- renderPlot({
		input$Restart
		plot(session$userData$sim.com, main = "Community distribution")
		sample_quadrats(comm=session$userData$sim.com, n_quadrats=input$number_of_quadrats, quadrat_area=input$area_of_quadrats, plot=T, method = input$sampling_method, avoid_overlap=T)
	})	
	
	## Sampling summary
	### % of species found
	output$samplingsummary <- renderPrint({
		input$sampling_simulation_button
		isolate({
			set.seed(33)
			withProgress(message = 'Simulating', value = 0, {	# style="old"
				session$userData$sap_test <- lapply(1:input$nrep_for_sampling_simulation, function(i) {
					quadrats <- sample_quadrats(session$userData$sim.com, avoid_overlap=T, quadrat_area=input$area_of_quadrats, n_quadrats=input$number_of_quadrats, plot=F)
					incProgress(1/input$nrep_for_sampling_simulation, detail = paste("Doing repetition", i))
					return(list(
						richness = sum(apply(quadrats$spec_dat, 2, sum)>0),
						standardised_difference = as.numeric(apply(quadrats$spec_dat,2,sum)/sum(quadrats$spec_dat) - table(session$userData$sim.com$census$species)/sum(table(session$userData$sim.com$census$species)))
					))
					}
				)
			})
			session$userData$richness <- unlist(sapply(session$userData$sap_test, function(x) x["richness"]))
			summary(session$userData$richness)
			# class(session$userData$sap_test)
			#length(session$userData$richness)
			#session$userData$sap_test
		})
	})
	
	### abundance assessment accuracy
	output$sampling_hist <- renderPlot({
		input$sampling_simulation_button
		session$userData$standardised_difference <- sapply(session$userData$sap_test, function(x) x["standardised_difference"])
		plot(density(unlist(session$userData$standardised_difference)), main="Abundance assessment\naccuracy", las=1)
	})
	
	# Downloading data and plots
	
	output$downloadData <- downloadHandler(
		filename =function() {paste("community", sep="")},
		content = function(fname) {
			sim.com <<- session$userData$sim.com
			save("sim.com", file=fname)
		}
	)
	
	output$downloadMobPlot <- downloadHandler(
		filename=function(fname) {"plotMOB.png"},
		content=function(fname) {
			png(filename=fname, width=1500, height=300)
			print(plot_layout(session$userData$sim.com))
			dev.off()
		}
	)
			
	
	
})