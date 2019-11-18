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
library(mobsim)
library(DT)

# Define UI for slider demo application
  
navbarPage("Visualization of biodiversity pattern", selected="MOBsim",
	tabPanel("Introduction", includeMarkdown("introduction.md")),
	tabPanel("MOBsim", 

		fluidPage(
			fluidRow(
				column(width=3,
					# Slider inputs
					sliderInput("N", "Number of individuals",
									min=10, max=5000, value=2500, step=50, ticks=F),
					
					sliderInput("S", "Species Richness",
									min=5, max=500, value=100, step=5, ticks=F),
					
					selectizeInput("method_type", label="Method", choices=c("Random mother points"="random_mother_points", "Click for mother points"="click_for_mother_points", "User community file"="uploading_community_data"), selected="Random mother points", multiple=FALSE)
					
					# checkboxInput("sample_setting_button", "Sample setting", value=FALSE)
				),
				
				column(width=3,
					uiOutput("select_sad_type"),
					uiOutput("CVslider"),

					uiOutput("text_spat_agg"),
											
					uiOutput("spatdist"),
					uiOutput("spatcoef"),
					
					uiOutput("community_uploading_tool")
				),
				
				column(width=3,
					uiOutput("species_ID_input"),
					plotOutput("on_plot_selection",
						click = "plot_click",
						brush = "plot_brush"
					),
					column(offset=1, width=5, uiOutput("rem_point_button")),
					column(width=6, uiOutput("rem_all_points_button"))
					
					# uiOutput("info")
				),
				
				column(width=3,
					div(style = 'height:50vh; overflow-y: scroll',
						tableOutput("datatable")
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
			checkboxInput(input='keep', label='Keep this simulation plot', value=FALSE)			
		),
		
		fluidRow(align="center",
			# plotOutput("PreviousInteractivePlot", height="600px",width="750px"),
			# plotOutput("InteractivePlot", height="600px",width="750px")
			plotOutput("PreviousInteractivePlot", height="300px",width="1500px"),
			plotOutput("InteractivePlot", height="300px",width="1500px")
		),
		
		fluidRow(align="center",
			# column(width=4, numericInput("plot_saving_width", label="Width in inches", value=15)),
			# column(width=4, numericInput("plot_saving_height", label="Height in inches", value=3)),
			# column(width=4, numericInput("plot_saving_reolution", label="Resolution", value=72)),
			
			column(width=6, selectInput("plot_saving_format", label="Saving format", choices=c("tiff","png"), selected="png")),
			column(width=6, downloadButton("downloadMobPlot", "Download plot"))
		)
	),
	
	tabPanel("Sampling",
		column(width=6,
			column(width=4, selectInput("sampling_method", label="Sampling Method", choices=c("random","grid"), selected="random")), #,"transect"
			column(width=4, numericInput("number_of_quadrats", label="Number of quadrats", value=20, min=1, max=1000, step=1)),
			column(width=4, numericInput("area_of_quadrats", label="Area of quadrats", value=0.005, min=0.00001, max=1, step=0.005)),
			
			# column(width=6, numericInput("nrep_for_sampling_simulation", label="Number of simulation repetitions", value=10, min=5, max=200, step=5)),
			# column(width=6, actionButton("sampling_simulation_button", label="Simulation")),	#, style = "margin-top: 25px;"
			column(width=6, actionButton("new_sampling_button", label="New sampling")),	#, style = "margin-top: 25px;"
			
			fluidRow(
			# Simulation
				# verbatimTextOutput("samplingsimulationsummary", placeholder=FALSE),
				# plotOutput("sampling_hist", height="600px",width="500px")
			# Tables
				tableOutput("sampling_gamma_table"),
				# dataTableOutput("sampling_alpha_table")
			# Plots
				plotOutput("rarefaction_curves",
					dblclick = "rarefaction_curves_dblclick",
					brush = brushOpts(
						id = "rarefaction_curves_brush",
						resetOnNew = TRUE)
				)
			)
		),
		column(width=6,
			tableOutput("community_summary_table"),
			plotOutput("sampling_plot", height="600px",width="600px")
		)
	)
)
