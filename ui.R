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

# Define UI for slider demo application
  
navbarPage("Visualization of biodiversity pattern",
	tabPanel("Plot", 

		fluidPage(
			fluidRow(
				column(width=3,
					# Slider inputs
					sliderInput("N", "Number of individuals",
									min=10, max=5000, value=500, step=50, ticks=F),
					
					sliderInput("S", "Species Richness",
									min=5, max=500, value=5, step=5, ticks=F),
					
					selectizeInput("method_type", label="Method", choices=c("Random mother points"="random_mother_points", "Click for mother points"="click_for_mother_points", "User community file"="uploading_community_data"), selected="Random mother points", multiple=FALSE),
					uiOutput("debug_text1"),
					uiOutput("debug_text2"),
					uiOutput("debug_text3"),
					uiOutput("debug_text4")
					
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
					
					uiOutput("rem_point_button"),
					uiOutput("rem_all_points_button")
					
					# uiOutput("info")
				),
				
				column(width=3,
					tableOutput("datatable")
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
		
		fluidRow(
			# plotOutput("PreviousInteractivePlot", height="600px",width="750px"),
			# plotOutput("InteractivePlot", height="600px",width="750px")
			plotOutput("PreviousInteractivePlot", height="300px",width="1500px"),
			plotOutput("InteractivePlot", height="300px",width="1500px")
		)
	),
	
	tabPanel("Introduction", includeMarkdown("introduction.md"))
	# another tabPanel "Saved simulation" with the previous sim?
)