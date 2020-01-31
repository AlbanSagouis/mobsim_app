#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
# library(mobsim)
library(DT)
source("extras/help/Help.R", local = TRUE)
source("extras/help/Labels.R", local = TRUE)
source("extras/graphical_parameters.R", local = TRUE)

# Define UI for slider demo application
  
navbarPage("Visualization of biodiversity pattern", selected="Plot patameters",
	tabPanel("Introduction", includeMarkdown("introduction.md")),
	tabPanel("MOBsim", 

		fluidPage(
			fluidRow(
				column(width=3,
					# Slider inputs
					sliderInput("N", "Number of individuals",
									min=10, max=5000, value=1000, step=10, ticks=F),
					
					sliderInput("S", "Species Richness",
									min=5, max=500, value=5, step=5, ticks=F),
					
					selectizeInput("method_type", label="Method", choices=c("Random mother points"="random_mother_points", "Click for mother points"="click_for_mother_points", "Click for species ranges"="click_for_species_ranges", "User community file"="uploading_community_data"), selected="Random mother points", multiple=FALSE)
					
					# checkboxInput("sample_setting_button", "Sample setting", value=FALSE)
				),
				
				column(width=3,
					uiOutput("select_sad_type"),
					uiOutput("CVslider"),

					uiOutput("text_spat_agg"),
					icon("question-circle"),
											
					uiOutput("spatdist"),
					icon("question-circle"),
					uiOutput("spatcoef"),
					icon("question-circle"),
					
					uiOutput("community_uploading_tool")
				),
				
				column(width=3,
					uiOutput("species_ID_input"),
					plotOutput(outputId="on_plot_selection",
						click = "plot_click",
						brush = brushOpts(id="plot_brush", resetOnNew=TRUE)
					),
					uiOutput("species_range_uploading_tool")
					# fluidRow(align="center", actionButton(inputId="resetPlot", label="Clear plot"))
						# column(width=6, actionButton(inputId="resetBrush", label="Reset brush"))
					# uiOutput("info")
				),
				
				column(width=3,
					div(style = 'height:50vh; overflow-y: scroll',
						tableOutput("datatable"),
						dataTableOutput("datatable_species_ranges")
					),
					fluidRow(
						column(offset=1, width=5, uiOutput("rem_point_button")),
						column(width=6, uiOutput("rem_all_points_button"))
					),
					verbatimTextOutput("simcomsummary", placeholder=FALSE),
					tags$style(type = 'text/css', '#simcomsummary {font-size: 10px;}')	# font-family: calibri light; background-color: rgba(255,255,255,0.40); color: black; border-style: none;}')
				)
			)
		),
		
		
		fluidRow(align="center",
			# Restart action button
			actionButton(inputId="Restart",label="Restart Simulation"),
			# Download sim.com action button
			downloadButton("downloadData", "Download community object"),
			# Check box
			checkboxInput(input='keepInteractivePlot', label='Keep this simulation plot', value=FALSE)			
		),
		
		fluidRow(align="center",
			# plotOutput("PreviousInteractivePlot", height="600px",width="750px"),
			# plotOutput("InteractivePlot", height="600px",width="750px")
			plotOutput("InteractivePlot", height="300px",width="1500px"),
			plotOutput("PreviousInteractivePlot", height="300px",width="1500px")
		),
		
		fluidRow(align="center",
			# column(width=4, numericInput("plot_saving_width", label="Width in inches", value=15)),
			# column(width=4, numericInput("plot_saving_height", label="Height in inches", value=3)),
			# column(width=4, numericInput("plot_saving_reolution", label="Resolution", value=72)),
			
			column(width=6, selectInput("plot_saving_format", label="Saving format", choices=c("tiff","png"), selected="png")),
			column(width=6, downloadButton("downloadMobPlot", "Download plot"))

		),

		bsPopover(id="N", title = Help$N$title, content=Help$N$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="S", title = Help$S$title, content=Help$S$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="select_sad_type", title = Help$select_sad_type$title, content = Help$select_sad_type$content),
		bsPopover(id="CVslider", title = Help$CVslider$title, content = Help$CVslider$content),
		bsPopover(id="text_spat_agg", title = Help$spatagg$title, content = Help$spatagg$content),
		bsPopover(id="spatdist", title = Help$spatdist$title, content = Help$spatdist$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="spatcoef", title = Help$spatcoef$title, content = Help$spatcoef$content),
		bsPopover(id="method_type", title= Help$method_type$title, content = Help$method_type$content, placement="bottom"),
		bsPopover(id="community_uploading_tool", title = Help$community_uploading_tool$title, content = Help$community_uploading_tool$content),
		bsPopover(id="species_range_uploading_tool", title = Help$species_range_uploading_tool$title, content = Help$species_range_uploading_tool$content)
		
	),
	
	tabPanel("Sampling",
		tableOutput("community_summary_table"),
		column(width=4, selectInput("sampling_method", label="Sampling Method", choices=c("random","grid"), selected="random")), #,"transect"
		column(width=4, numericInput("number_of_quadrats", label="Number of quadrats", value=20, min=1, max=1000, step=1)),
		column(width=4, numericInput("area_of_quadrats", label="Area of quadrats", value=0.005, min=0.00001, max=1, step=0.005)),
		
		# column(width=6, numericInput("nrep_for_sampling_simulation", label="Number of simulation repetitions", value=10, min=5, max=200, step=5)),
		# column(width=6, actionButton("sampling_simulation_button", label="Simulation")),	#, style = "margin-top: 25px;"
		column(width=6,
			actionButton("new_sampling_button", label="New sampling"),
			actionButton("keepRarefactionCurvesPlot", label='Keep this sampling design')
		),	#, style = "margin-top: 25px;"
		
		fluidRow(
			# Simulation
				# verbatimTextOutput("samplingsimulationsummary", placeholder=FALSE),
				# plotOutput("sampling_hist", height="600px",width="500px")
			# Tables
			column(width=4,
				tableOutput("sampling_gamma_table"),
				tableOutput("sampling_alpha_summary_table")
			),
				# dataTableOutput("sampling_alpha_table")
			# Plots
			column(width=4,
				plotOutput("rarefaction_curves_plot",
					click = "rarefaction_curves_plot_click",
					dblclick = "rarefaction_curves_plot_dblclick",
					brush = brushOpts(
						id = "rarefaction_curves_plot_brush",
						resetOnNew = TRUE)
					# hover = hoverOpts(id ="rarefaction_curves_plot_hover")
				)
			),
			column(width=4,
				plotOutput("sampling_plot", click="sampling_plot_click")	#, height="600px",width="600px"
				# plotOutput("sampling_distance_decay_plot")
			)
		),
		
		bsPopover(id = "area_of_quadrats", title = Help$area_of_quadrats$title, content = Help$area_of_quadrats$content),
		bsPopover(id = "rarefaction_curves_plot", title = Help$rarefaction_curves_plot$title, content = Help$rarefaction_curves_plot$content, trigger = "hover"),
		bsPopover(id = "sampling_plot", title = Help$sampling_plot$title, content = Help$sampling_plot$content)
	),
	
	
	tabPanel("Step-by-step",
        # tags$head(tags$style(HTML("
        # . {
          # font-size: 75%;
        # }
        # "))),
		sidebarLayout(
			sidebarPanel(
				fluidRow(
					column(width=6,
						# number of species
						numericInput("sbsS", "Species Richness", min=5, max=500, value=50, step=5)
					),
					column(width=6,
						# number of individuals
						numericInput("sbsN", "Number of individuals", min=10, max=5000, value=1000, step=10)
					)
				),
				selectizeInput("sbssad_type", "SAD Type", choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls")),
				uiOutput("sbsCVslider"),

				selectizeInput(inputId="sbsspatdist", Labels$spatdist, choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster")),
				helpText("Number of mother points per species OR number of individuals per cluster."),
				textInput(inputId="sbsspatcoef",label = Labels$spatcoef, value="0"),
				textInput(inputId="sbsspatagg", label = Labels$spatagg, value = 0.1),
				textInput("sbssimulation_seed", label = "Simulation seed", value="Not specified"),
				# verbatimTextOutput("debugging_seed"),
				
				# Restart action button
				actionButton(inputId="sbsRestart",label="Restart Simulation"),
				
				# sampling parameters
				fluidRow(
					column(width=4,
						numericInput("sbsnumber_of_quadrats", label="Number of quadrats", value=20, min=1, max=1000, step=1)
					),
					column(width=4,
						numericInput("sbsarea_of_quadrats", label="Area of quadrats", value=0.005, min=0.00001, max=1, step=0.005)
					),
					column(width=4,
						# numericInput("sbssampling_seed", label="Sampling seed", value=NULL, min=1, max=2^15, step=1)
						textInput("sbssampling_seed", label="Sampling seed", value="Not specified")
					)
				),
				actionButton("sbsnew_sampling_button", label="Restart sampling"),
				# Next step
				actionButton("sbskeep_step", label="Next step"),
				selectizeInput(inputId="sbsplot_choice", "Plot type", choices = c("Distance decay"="distance_decay_choice", "Rarefaction curve"="rarefaction_curve_choice"))
			),
			mainPanel(
			# plot of the community
			# distance decay
				uiOutput("sbsfirst_step")
			)
		),
		bsPopover(id="sbsS", title = Help$S$title, content = Help$S$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="sbsN", title =  Help$N$title, content =  Help$N$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="sbsspatagg", title = Help$spatagg$title, content = Help$spatagg$content),
		bsPopover(id="sbsspatdist", title = Help$spatdist$title, content = Help$spatdist$content, placement = "bottom", trigger = "hover", options = NULL),
		bsPopover(id="sbsspatcoef", title=Help$spatcoef$title, content=Help$spatcoef$content),
		bsPopover(id="sbssimulation_seed", title = Help$simulation_seed$title, content=Help$simulation_seed$content),
		bsPopover(id="sbssampling_seed", title = Help$sampling_seed$title, content=Help$sampling_seed$content),
		bsPopover(id="sbskeep_step", title = Help$keep_step$title, content = Help$keep_step$content, placement = "bottom", trigger = "hover", options = NULL)
	),
	
	
	tabPanel("Big Table",
		dataTableOutput("bigtable_output"),
		verbatimTextOutput("bigtable_selected_simulations"),
		fluidRow(
			column(width=3,
				fluidRow(align="center", actionButton("rem_all_simulations", "Erase all rows")),
				fluidRow(align="center", actionButton("rem_selected_simulations", "Erase selected rows"))
			),
			column(width=3,
				fluidRow(align="center", downloadButton("downloadSimulationTable", "Download simulation table")),
				fluidRow(align="center", downloadButton("downloadSimulationList", "Download simulation data"))
			),
			column(width=3,
				checkboxGroupInput(inputId="compplot_types", label="Plot types", choices=c("Community map", "Distance decay", "Rarefaction curve"), selected="Rarefaction curve")
			),
			column(width=3,
				radioButtons(inputId="compplot_style", label="Plot style", choices=c("Split","Stacked"))
			)
		),
		# actionButton("compare_selected_simulations","Compare selected simulations"),
		# verbatimTextOutput("debugging_simulation_table"),
		plotOutput("comp_plot",
			brush = brushOpts(id = "comparison_plot_brush", resetOnNew = TRUE),
			dblclick = "comparison_plot_dblclick"),
		hr(),

   	bsPopover(id = "bigtable_output", title = Help$bigtable_output$title, Help$bigtable_output$content),
   	bsPopover(id = "downloadSimulationTable", title = Help$downloadSimulationTable$title, content = Help$downloadSimulationTable$content),
   	bsPopover(id = "downloadSimulationList", title = Help$downloadSimulationList$title, content = Help$downloadSimulationList$content)
		
	),
	tabPanel("Plot parameters",
   	sidebarLayout(
   	   sidebarPanel(
            selectInput("color_palette", label="Choose color palette", choices=palette_tab$palette_name, selected = "brewer.paired"),
            radioButtons("dark_background", label = "Plot background color (Not implemented yet)", choices = c("Light","Dark")),
            checkboxInput("CBF_test", label = "Show Colorblindness Suitability"),
            textOutput("clicktext"),
            checkboxGroupInput(inputId = "rarefaction_curves_loglog", label = "Logarithmic axis", choices = c("x","y"), selected = c("x","y"), inline=TRUE)
   	   ),
   	   mainPanel(
   	      column(width=6, plotOutput("discrete_palettes", click = "discrete_palettes_click")),
   	      column(width=6, plotOutput("CBF_test_plot"))
         )
      ),
	   bsPopover(id="color_palette", title = Help$color_palette$title, content = Help$color_palette$content, placement = "top", trigger = "hover", options = NULL)
	)
	
	# tabPanel("Comparison",
		# tableOutput("simtab_output"),
	# )
)	# end of ui