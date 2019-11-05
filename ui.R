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
									min=10, max=5000, value=500, step=50, ticks=F),
					
					sliderInput("S", "Species Richness",
									min=5, max=500, value=5, step=5, ticks=F),
					
					selectizeInput("method_type", label="Method", choices=c("Random mother points"="random_mother_points", "Click for mother points"="click_for_mother_points", "User community file"="uploading_community_data"), selected="Random mother points", multiple=FALSE),
					
					checkboxInput("sample_setting_button", "Sample setting", value=FALSE)
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
		
		
		fluidRow(
			column(width=6, offset=5,
				# Action button
				actionButton(inputId="Restart",label="Restart Simulation"),
				
				# Check box
				checkboxInput(input='keep', label='Keep this simulation', value=FALSE) 
			),
			column(width=1)				
		),
		
		fluidRow(align="center",
			# plotOutput("PreviousInteractivePlot", height="600px",width="750px"),
			# plotOutput("InteractivePlot", height="600px",width="750px")
			plotOutput("PreviousInteractivePlot", height="300px",width="1500px"),
			plotOutput("InteractivePlot", height="300px",width="1500px")
		)
	),
	
	tabPanel("Sampling",
		column(width=6),
		column(width=6,
			plotOutput("sampling_plot", height="500px",width="500px")
		)
	)
)
