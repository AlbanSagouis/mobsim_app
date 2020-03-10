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
# library(shinyjs)
# library(mobsim)
library(DT)
# library(darkmode)  # not great
source("extras/help/Help.r", local = TRUE)
source("extras/help/Labels.r", local = TRUE)
source("extras/graphical_parameters.R", local = TRUE)
# Define UI for slider demo application
  
navbarPage("Visualization of biodiversity pattern", selected="Species by species simulation",
   shinyjs::useShinyjs(),
           
	tabPanel("Introduction", includeMarkdown("introduction.md")),
	
	tabPanel("1 SAD - Population simulation",
      sidebarLayout(
         sidebarPanel(
         	# Slider inputs
            sliderInput("sadN", label = p("Number of individuals", tags$style(type="text/css", "#sadN_icon {vertical-align: top;}"),
                                         popify(bsButton("sadN_icon", label="", icon=icon("question-circle"), size="extra-small"),
                                                title = Help$N$title, content = Help$N$content, placement = "bottom", trigger = "focus")), # , style="info"
                         min=10, max=5000, value=1000, step=10, ticks=F),
         	
         	sliderInput("sadS", label = p("Species Richness", tags$style(type="text/css", "#sadS_icon {vertical-align: top;}"),
         	                             popify(bsButton("sadS_icon", label="", icon=icon("question-circle"), size="extra-small"),
         	                                    title = Help$S$title, content = Help$S$content, placement = "bottom", trigger = "focus")),
         	             min=5, max=500, value=5, step=5, ticks=F),
         
            selectizeInput("sadsad_type", label = p(Labels$sad_type, tags$style(type="text/css", "#sadsad_type_icon {vertical-align: top;}"),
		                                     popify(bsButton("sadsad_type_icon", label="", icon=icon("question-circle"), size="extra-small"),
		                                            title = Help$select_sad_type$title,
		                                            content = Help$select_sad_type$content,
		                                            placement = "bottom", trigger = "focus")), 
		               choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls"), selected = "lnorm"),
            uiOutput("sadCVslider"),
            actionButton(inputId="sadRestart",label="Restart Simulation"),
            fluidRow(
               column(width = 4, numericInput(inputId = "sadNewS", label = "Number of new species", value = 1, min=1, max=1000, step=1)),
               column(width = 4, numericInput(inputId = "sadNewN", label = Labels$N, value = 100, min=1, max=5000, step=1)),
               column(width = 4, actionButton(inputId = "sadAddSpecies", label = "Add species"))
            )
         ),
         mainPanel(
            plotOutput("sadsad_plots")
         )
      )
   ),
	
	tabPanel("2 Space - Distribution simulation",
      sidebarLayout(
         sidebarPanel(
         	fluidRow(
               column(width = 6, 
                      sliderInput("spaN", label = p("Number of individuals", tags$style(type="text/css", "#spaN_icon {vertical-align: top;}"),
                                            popify(bsButton("spaN_icon", label="", icon=icon("question-circle"), size="extra-small"),
                                                   title = Help$N$title, content = Help$N$content, placement = "bottom", trigger = "focus")),
                            min=10, max=5000, value=1000, step=10, ticks=F)
               ),
            	column(width = 6,
               	sliderInput("spaS", label = p("Species Richness", tags$style(type="text/css", "#spaS_icon {vertical-align: top;}"),
               	                             popify(bsButton("spaS_icon", label="", icon=icon("question-circle"), size="extra-small"),
               	                                    title = Help$S$title, content = Help$S$content, placement = "bottom", trigger = "focus")),
               	             min=5, max=500, value=5, step=5, ticks=F)
         	   )
         	),
         	fluidRow(
         	   column(width = 6,
         	          selectizeInput("spasad_type", label = p(Labels$sad_type,
         	                                                  tags$style(type="text/css", "#spasad_type_icon {vertical-align: top;}"),
         	                                                  popify(bsButton("spasad_type_icon", label="", icon=icon("question-circle"), size="extra-small"),
         	                                                         title = Help$select_sad_type$title,
         	                                                         content = Help$select_sad_type$content,
         	                                                         placement = "bottom", trigger = "focus")),
         	                         choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls"), selected = "lnorm")
         	   ),
         	   column(width = 6, uiOutput("spaCVslider"))
         	),
         	
         	fluidRow(
         	   column(width = 6,
         	          selectizeInput(inputId="spaspatdist", p(Labels$spatdist, tags$style(type="text/css", "#spaspatdist_icon {vertical-align: top;}"),
         	                                                  popify(bsButton("spaspatdist_icon", label="", icon=icon("question-circle"), size="extra-small"),
         	                                                         title = Help$spatdist$title, content = Help$spatdist$content, trigger = "focus")),
         	                         choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster"), selected = "n.mother")
         	   ),
         	   
         	   column(width = 6,
               	textInput(inputId="spaspatcoef",label=p(Labels$spatcoef, tags$style(type="text/css", "#spaspatcoef_icon {vertical-align: top;}"),
               	                                         popify(bsButton("spaspatcoef_icon", label="", icon=icon("question-circle"), size="extra-small"),
               	                                                  title = Help$spatcoef$title, content = Help$spatcoef$content, trigger = "focus")),
               	                value="1")
               )
         	),
         	
         	fluidRow(
         	   column(width = 6, 
         	          textInput(inputId = "spaspatagg", label = p(
         	             Labels$spatagg, tags$style(type="text/css", "#spaspatagg_icon {vertical-align: top;}"),
         	             popify(
         	                bsButton("spaspatagg_icon", label="", icon=icon("question-circle"), size="extra-small"),
         	                title = Help$spatagg$title,
         	                content = Help$spatagg$content, trigger = "focus")), value = 0.1)
         	   ),
         	   column(width = 6,
         	          actionButton(inputId="spaRestart", label="Restart Simulation")
         	   )
         	),
         	plotOutput("spasad_plots"),
         	radioButtons("exercise_number", "Exercise number",
         	             choices = 2:3, selected = 2, inline = TRUE)
         ),
         
         
         mainPanel(
            plotOutput("spacom_plots")
         )
      )
   ),
	
	
	tabPanel("3 Basic Sampling",
	         sidebarLayout(
	            sidebarPanel(
	               tableOutput("bsacommunity_summary_table"),
	               numericInput("bsanumber_of_quadrats", label="Number of quadrats", value=20, min=1, max=1000, step=1),
	               numericInput("bsaarea_of_quadrats", label=p("Area of quadrats", tags$style(type="text/css", "#bsaarea_of_quadrats_icon {vertical-align: top;}"),
	                                                        popify(bsButton("bsaarea_of_quadrats_icon", label="", icon=icon("question-circle"), size="extra-small"),
	                                                               title = Help$area_of_quadrats$title, content = Help$area_of_quadrats$content,
	                                                               trigger= "focus")),
	                            value=0.005, min=0.00001, max=1, step=0.005),
	               actionButton("bsanew_sampling_button", label="New sampling"),
	               tableOutput("bsasampling_gamma_table"),
	               tableOutput("bsasampling_alpha_summary_table")
	            ),
	            
	            mainPanel(
	               fluidRow(
	                  column(width = 6, plotOutput("bsasampling_plot")),
	                  column(width = 6, plotOutput("bsararefaction_curves_plot"))
	               ),
	               fluidRow(
	                  column(width = 6, plotOutput("bsadivar_plot")),
	                  column(width = 6, plotOutput("bsadist_decay_plot"))
	              )
	            )
	         )
	),
	         
	      
	      
	      
	
	
	tabPanel("MOBsim - Simulation", 
      # tags$style(".popover{
      # container: body;
      #            }"),

		fluidPage(
			fluidRow(
				column(width=3,
					# Slider inputs
		         sliderInput("N", label = p("Number of individuals", tags$style(type="text/css", "#N_icon {vertical-align: top;}"),
		                                    popify(bsButton("N_icon", label="", icon=icon("question-circle"), size="extra-small"),
		                                    title = Help$N$title, content=Help$N$content, placement = "bottom", trigger = "focus")), # , style="info"
		                      min=10, max=5000, value=1000, step=10, ticks=F),
					
					sliderInput("S", label = p("Species Richness", tags$style(type="text/css", "#S_icon {vertical-align: top;}"),
					                           popify(bsButton("S_icon", label="", icon=icon("question-circle"), size="extra-small"),
					                                  title = Help$S$title, content=Help$S$content, placement = "bottom", trigger = "focus")),
					             min=5, max=500, value=5, step=5, ticks=F),
					
               selectizeInput("method_type", label=p("Method", tags$style(type="text/css", "#method_type_icon {vertical-align: top;}"),
                                                     popify(bsButton("method_type_icon", label="", icon=icon("question-circle"), size="extra-small"),
                                                             title= Help$method_type$title, content = Help$method_type$content,
                                                             placement="bottom", options = list(container = "body"), trigger = "focus")),
                              choices=c("Random mother points"="random_mother_points", "Click for mother points"="click_for_mother_points", "Click for species ranges"="click_for_species_ranges", "User community file"="uploading_community_data"), selected="Random mother points", multiple=FALSE)
					
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
					plotOutput(outputId="on_plot_selection",
						click = "plot_click",
						brush = brushOpts(id="plot_brush", resetOnNew=TRUE)
					),
					uiOutput("species_range_uploading_tool_icon"),
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

		)
	),
	
	tabPanel("Sampling",
      fluidRow(
         column(width = 3, tableOutput("community_summary_table")),
         column(width = 3, selectInput("sampling_method", label="Sampling Method", choices=c("random","grid"), selected="random")), #,"transect"
         column(width = 3, numericInput("number_of_quadrats", label="Number of quadrats", value=20, min=1, max=1000, step=1)),
         column(width = 3, numericInput("area_of_quadrats", label=p("Area of quadrats", tags$style(type="text/css", "#area_of_quadrats_icon {vertical-align: top;}"),
		                                                         popify(bsButton("area_of_quadrats_icon", label="", icon=icon("question-circle"), size="extra-small"),
		                                                                title = Help$area_of_quadrats$title, content = Help$area_of_quadrats$content,
		                                                                trigger= "focus")),
                                        value=0.005, min=0.00001, max=1, step=0.005))
		   ),
		
		# column(width=6, numericInput("nrep_for_sampling_simulation", label="Number of simulation repetitions", value=10, min=5, max=200, step=5)),
		# column(width=6, actionButton("sampling_simulation_button", label="Simulation")),	#, style = "margin-top: 25px;"
		fluidRow(align = "center",
		   actionButton("new_sampling_button", label="New sampling"),
		   actionButton("keepRarefactionCurvesPlot", label='Keep this sampling design')
		),
		
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
		
		bsPopover(id = "rarefaction_curves_plot", title = Help$rarefaction_curves_plot$title, content = Help$rarefaction_curves_plot$content, trigger = "hover"),
		bsPopover(id = "sampling_plot", title = Help$sampling_plot$title, content = Help$sampling_plot$content, placement = "top", trigger = "hover")
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
						numericInput("sbsS", label = p("Species Richness", tags$style(type="text/css", "#sbsS_icon {vertical-align: top;}"),
						                               popify(bsButton("sbsS_icon", label="", icon=icon("question-circle"), size="extra-small"),
						                                      title = Help$S$title, content = Help$S$content, trigger = "focus")),
						             min=5, max=500, value=50, step=5)
					),
					column(width=6,
						# number of individuals
						numericInput("sbsN", label = p("Number of individuals", tags$style(type="text/css", "#sbsN_icon {vertical-align: top;}"),
						                               popify(bsButton("sbsN_icon", label="", icon=icon("question-circle"), size="extra-small"),
						                               title = Help$N$title, content = Help$N$content, trigger = "focus")),
						             min=10, max=5000, value=1000, step=10)
					)
				),
				selectizeInput("sbssad_type", label = p("SAD Type", tags$style(type="text/css", "#sbssad_type_icon {vertical-align: top;}"),
				                                        popify(bsButton("sbssad_type_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                               title = Help$select_sad_type$title,
				                                               content = Help$select_sad_type$content, trigger = "focus")),
				               choices=c("lognormal"="lnorm","geometric"="geom","Fisher's log-series"="ls")),
				uiOutput("sbsCVslider"),

				selectizeInput(inputId="sbsspatdist", label = p(Labels$spatdist, tags$style(type="text/css", "#sbsspatdist_icon {vertical-align: top;}"),
				                                                popify(bsButton("sbsspatdist_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                                       title = Help$spatdist$title, content = Help$spatdist$content, trigger = "focus")),
				               choices = c("Number of mother points"="n.mother", "Number of clusters"="n.cluster")),
				# helpText("Number of mother points per species OR number of individuals per cluster."),
				textInput(inputId="sbsspatcoef",label = p(Labels$spatcoef, tags$style(type="text/css", "#sbsspatcoef_icon {vertical-align: top;}"),
				                                        popify(bsButton("sbsspatcoef_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                        title = Help$spatcoef$title, content = Help$spatcoef$content, trigger = "focus")),
				          value="0"),
				textInput(inputId="sbsspatagg", label = p(Labels$spatagg, tags$style(type="text/css", "#sbsspatagg_icon {vertical-align: top;}"),
				                                        popify(bsButton("sbsspatagg_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                        title = Help$spatagg$title, content = Help$spatagg$content, trigger = "focus")),
				          value = 0.1),
				textInput("sbssimulation_seed", label = p("Simulation seed", tags$style(type="text/css", "#sbssimulation_seed_icon {vertical-align: top;}"),
				                                        popify(bsButton("sbssimulation_seed_icon", label="",
				                                                        icon=icon("question-circle"), size="extra-small"),
				                                               title = Help$simulation_seed$title, content="content",   # Help$simulation_seed$content
				                                               trigger = "focus")),
				          value="Not specified"),
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
						textInput("sbssampling_seed", label=p("Sampling seed", tags$style(type="text/css", "#sbssampling_seed_icon {vertical-align: top;}"),
						                                      popify(bsButton("sbssampling_seed_icon", label="",
						                                                      icon=icon("question-circle"), size="extra-small"),
						                                             title = Help$sampling_seed$title, content=Help$sampling_seed$content,
						                                             trigger = "focus")),
						          value="Not specified")
					)
				),
				actionButton("sbsnew_sampling_button", label="Restart sampling"),
				# Next step
				actionButton("sbskeep_step", label=p("Next step", tags$style(type="text/css", "#sbskeep_step_icon {vertical-align: top;}"),
				                                     popify(bsButton("sbskeep_step_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                            title = Help$keep_step$title, content = Help$keep_step$content, placement = "bottom", trigger = "focus"))),
				selectizeInput(inputId="sbsplot_choice", label ="Plot type", choices = c("Rarefaction curve"="rarefaction_curve_choice", "Distance decay"="distance_decay_choice"))
			),
			mainPanel(
			# plot of the community
			# distance decay
				uiOutput("sbsfirst_step")
			)
		),

		bsPopover(id="sbskeep_step_icon", title = Help$keep_step$title, content = Help$keep_step$content, placement = "bottom", trigger = "focus", options = NULL)
	),
	
	
	tabPanel("Big Table",
		dataTableOutput("comparativeTable_output"),
		verbatimTextOutput("comparativeTable_selected_simulations"),
		fluidRow(
			column(width=3,
				fluidRow(align="center", actionButton("rem_all_simulations", "Erase all rows")),
				fluidRow(align="center", actionButton("rem_selected_simulations", "Erase selected rows"))
			),
			column(width=3,
				fluidRow(align="center", downloadButton(outputId = "downloadSimulationTable",
				                                        label = p("Download simulation table",
				                                                  tags$style(type="text/css", "#downloadSimulationTable_icon {vertical-align: right;}"),
				                                                  popify(bsButton("downloadSimulationTable_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                                         title = Help$downloadSimulationTable$title,
				                                                         content = Help$downloadSimulationTable$content,
				                                                         placement = "bottom", trigger = "hover")))),
				fluidRow(align="center", downloadButton(outputId = "downloadSimulationList",
				                                        label = p("Download simulation data",
				                                                  tags$style(type="text/css", "#downloadSimulationList_icon {vertical-align: right;}"),
				                                                  popify(bsButton("downloadSimulationList_icon", label="", icon=icon("question-circle"), size="extra-small"),
				                                                         title = Help$downloadSimulationList$title,
				                                                         content = Help$downloadSimulationList$content,
				                                                         placement = "bottom", trigger = "hover"))))
			),
			column(width=3,
				radioButtons(inputId="compplot_style", label="Plot style", choices=c("Split","Stacked"), selected = "Stacked")
			),
			column(width=3,
				uiOutput("compplot_types_selection")
			)
		),
		# actionButton("compare_selected_simulations","Compare selected simulations"),
		# verbatimTextOutput("debugging_simulation_table"),
		fluidRow(align = "center",
		         column(width = 6, offset= 3, plotOutput("comp_plot",
			brush = brushOpts(id = "comparison_plot_brush", resetOnNew = TRUE),
			dblclick = "comparison_plot_dblclick")
		         )
		),
		hr(),

   	bsPopover(id = "comparativeTable_output", title = Help$comparativeTable_output$title, Help$comparativeTable_output$content, trigger = "focus")
		
	),
   tabPanel("Species by species simulation",
            sidebarLayout(
               sidebarPanel(
                  fluidRow(
                     column(width= 6, numericInput(inputId = "speS", label = Labels$S, value = 5, min=2, max=200, step=1)),
                     column(width = 6, actionButton(inputId = "speRestart", label = "New simulation"))
                  ),
                  fluidRow(
                     column(width = 4, numericInput(inputId = "speNewS", label = "Number of new species", value = 1, min=1, max=1000, step=1)),
                     column(width = 4, numericInput(inputId = "speNewN", label = "Number of individuals per species", value = 100, min=1, max=5000, step=1)),
                     column(width = 4, 
                        actionButton(inputId = "speAddSpecies", label = "Add species"),
                        textOutput("speNewStext")
                        )
                  )
                  # show a clickable table with species abundance and a delete species button
               ),
               mainPanel(
                  textOutput(outputId = "speCommunity_text"),
                  plotOutput("spesad_plots")
               )
            )
   ),
	tabPanel("Graphical parameters",
   	sidebarLayout(
   	   sidebarPanel(
   	      # with_darkmode(),
            selectInput("color_palette", label="Choose color palette", choices=palette_tab$palette_name, selected = "brewer.paired"),
            radioButtons("dark_background", label = "Plot background color (Not implemented yet)", choices = c("Light","Dark")),
            checkboxInput("CBF_test", label = "Show Colorblindness Suitability"),
            # textOutput("clicktext"),
            checkboxGroupInput(inputId = "rarefaction_curves_loglog", label = "Logarithmic axis", choices = c("x","y"), selected = c("x","y"), inline=TRUE)
   	   ),
   	   mainPanel(
   	      column(width=6, plotOutput("discrete_palettes", click = "discrete_palettes_click")),
   	      column(width=6, plotOutput("CBF_test_plot"))
         )
      ),
	   bsPopover(id="color_palette", title = Help$color_palette$title, content = Help$color_palette$content, placement = "right", trigger = "focus", options = NULL)
	)
	
	# tabPanel("Comparison",
		# tableOutput("simtab_output"),
	# )
)	# end of ui