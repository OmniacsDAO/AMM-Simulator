## Load libraries
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dygraphs)
library(highcharter)
options(scipen=99)

dashboardPage(
	dashboardHeader(title = "AMM Simulator"),

    dashboardSidebar(
		sidebarMenu(
			id = "tabs",
			menuItem("Start Pool", tabName = "gen_pool", icon = icon("chart-pie")),
			menuItem("Trade Pool", tabName = "trade_pool", icon = icon("money-bill-wave")),
			valueBoxOutput("pool_tok",width = 12),
			valueBoxOutput("pool_eth",width = 12),
			valueBoxOutput("pool_price_iacs",width = 12),
			valueBoxOutput("pool_price_eth",width = 12)
		)
	),

	dashboardBody(
		tabItems(
			tabItem(tabName = "gen_pool",
				column(width = 4,
					box(title = "Pool Start Amounts",width=12,solidHeader=TRUE,status="primary",
						uiOutput("pool_amt"),
						fluidRow(
							column(width=3,),
							column(width=6,
								actionButton("gen_pool_bt", "Generate Pool")
							),
							column(width=3,)
						)
					),
					uiOutput("trades_file_in"),
					uiOutput("eth_price_usd")
				),
				column(width = 8,
					dataTableOutput("trade_table")
				)
			),
			tabItem(tabName = "trade_pool",
				column(width = 3,
							h4("Swap Token"),
							radioGroupButtons(
												inputId = "swap_dir",
												label = NULL,
												choices = c("ETH for IACS"="iacs_eth", "IACS for ETH"="eth_iacs"),
												justified = TRUE,
												checkIcon = list(yes = icon("ok", lib = "glyphicon"))
											),
							fluidRow(
								column(width=2,h5("IACS")),
								column(width=10,uiOutput("swap_iacs_amt"))
							),
							fluidRow(
								column(width=2,h5("ETH")),
								column(width=10,uiOutput("swap_eth_amt"))
							),
							fluidRow(
								column(width=3,),
								column(width=6,actionButton("tr_tok_amt_swap", "Swap from Pool")),
								column(width=3,)
							),
							hr(),
							h4("Add/Remove Liquidity"),
							radioGroupButtons(
												inputId = "liq_dir",
												label = NULL,
												choices = c("Add"="liq_add", "Remove"="liq_rem"),
												justified = TRUE,
												checkIcon = list(yes = icon("ok", lib = "glyphicon"))
											),
							radioGroupButtons(
												inputId = "liq_chc",
												label = NULL,
												choices = c("Specify IACS"="liq_iacs", "Specify ETH"="liq_eth"),
												justified = TRUE,
												checkIcon = list(yes = icon("ok", lib = "glyphicon"))
											),
							fluidRow(
								column(width=2,h5("%")),
								column(width=10,uiOutput("liq_rem_pct"))
							),
							fluidRow(
								column(width=2,h5("IACS")),
								column(width=10,uiOutput("liq_iacs_amt"))
							),
							fluidRow(
								column(width=2,h5("ETH")),
								column(width=10,uiOutput("liq_eth_amt"))
							),
							fluidRow(
								column(width=3,),
								column(width=6,actionButton("liq_tok_amt_swap", "Process to Pool")),
								column(width=3,)
							)
				),
				column(width = 9,
					tabBox(
							id = "tabset1",width=12,side = "left",
							tabPanel(
										"Pool Token Amounts",
										highchartOutput("pool_amt_hc",height = "480px")
							),
							tabPanel(
										"Pool Token Prices",
										highchartOutput("pool_pr_d_hc",height = "240px"),
										highchartOutput("pool_price_hc",height = "240px")
							),	
							tabPanel("Pool Trade Table", dataTableOutput("pool_table"))
							
					)
				)
			)
		)
	)
)
