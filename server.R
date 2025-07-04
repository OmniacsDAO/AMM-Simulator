## Load libraries
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dygraphs)
library(highcharter)
options(scipen=99)

function(input, output, session)
{
	####################################################
	## Generate Pool
	####################################################
	pool_val <- reactiveValues(	pool = data.frame(
													trade=character(),
													tok_amt=numeric(),
													base_amt=numeric(),
													tok_chg=numeric(),
													base_chg=numeric(),
													price_iacs_eth=numeric(),
													price_eth_iacs=numeric()
												)
				)
	trade_val <- reactiveValues(t_tab = NULL,error = NULL)

	output$pool_amt <- renderUI({
									list(
											autonumericInput(
																"tok_amt",
																"Enter $IACS Amount",
																value=1000,
																minimumValue=0,
																decimalPlaces = 18,
																digitGroupSeparator = ",",
																decimalCharacter = ".",
																align = "left",
																allowDecimalPadding=FALSE
															),
											autonumericInput(
																"base_amt",
																"Enter $ETH Amount",
																value=100,
																minimumValue=0,
																decimalPlaces = 18,
																digitGroupSeparator = ",",
																decimalCharacter = ".",
																align = "left",
																allowDecimalPadding=FALSE
															)
										)
								})

	output$pool_tok <- renderInfoBox({
											if(nrow(pool_val$pool)==0)
											{
												return(valueBox(NA, "Pool Balance $IACS", icon = icon("gg")))
											}
											v_out <- round(tail(pool_val$pool$tok_amt,1),4)
											valueBox(tags$p(v_out, style = "font-size: 70%;"), "Pool Balance $IACS", icon = icon("gg"))
									})

	output$pool_eth <- renderInfoBox({
											if(nrow(pool_val$pool)==0)
											{
												return(valueBox(NA, "Pool Balance $ETH", icon = icon("ethereum")))
											}
											v_out <- round(tail(pool_val$pool$base_amt,1),4)
											valueBox(tags$p(v_out, style = "font-size: 70%;"), "Pool Balance $ETH", icon = icon("ethereum"))
									})

	output$pool_price_iacs <- renderInfoBox({
											if(nrow(pool_val$pool)==0)
											{
												return(valueBox(NA, "Price 1 IACS in ETH", icon = icon("dollar-sign")))
											}
											v_out <- round(tail(pool_val$pool$price_iacs_eth,1),5)
											valueBox(tags$p(v_out, style = "font-size: 70%;"), "Price 1 IACS in ETH", icon = icon("dollar-sign"))
									})
	output$pool_price_eth <- renderInfoBox({
											if(nrow(pool_val$pool)==0)
											{
												return(valueBox(NA, "Price 1 ETH in IACS", icon = icon("dollar-sign")))
											}
											v_out <- round(tail(pool_val$pool$price_eth_iacs,1),5)
											valueBox(tags$p(v_out, style = "font-size: 70%;"), "Price 1 ETH in IACS", icon = icon("dollar-sign"))
									})

	observeEvent(input$gen_pool_bt,{
										pool_val$pool <- data.frame(
																		trade = "Start Pool",
																		tok_amt = input$tok_amt,
																		base_amt = input$base_amt,
																		tok_chg = input$tok_amt,
																		base_chg = input$base_amt,
																		price_iacs_eth = ((as.numeric(input$tok_amt)*as.numeric(input$base_amt))/(as.numeric(input$tok_amt)-1))-as.numeric(input$base_amt),
																		price_eth_iacs = ((as.numeric(input$base_amt)*as.numeric(input$tok_amt))/(as.numeric(input$base_amt)-1))-as.numeric(input$tok_amt)
																	)
									}
				)
	####################################################
	####################################################

	####################################################
	## Swap Logic
	####################################################
	output$swap_iacs_amt <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										if(input$swap_dir=="iacs_eth")
										{
											tok_amt <- tail(pool_val$pool$tok_amt,1)
											base_amt <- tail(pool_val$pool$base_amt,1)
											tok_cost = tok_amt-((tok_amt*base_amt)/(base_amt+input$sw_eth_amt))
											return(h5(tok_cost))
										} 
										autonumericInput(
															"sw_tok_amt",
															NULL,
															value=NULL,
															minimumValue=0,
															# maximumValue=tail(pool_val$pool$tok_amt,1),
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
									})
	output$swap_eth_amt <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										if(input$swap_dir!="iacs_eth")
										{
											tok_amt <- as.numeric(tail(pool_val$pool$tok_amt,1))
											base_amt <- as.numeric(tail(pool_val$pool$base_amt,1))
											eth_cost = base_amt-((tok_amt*base_amt)/(tok_amt+input$sw_tok_amt))
											return(h5(eth_cost))
										} 
										autonumericInput(
															"sw_eth_amt",
															NULL,
															value=NULL,
															minimumValue=0,
															# maximumValue=tail(pool_val$pool$base_amt,1),
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
									})
	observeEvent(input$tr_tok_amt_swap,{
											if(nrow(pool_val$pool)>0)
											{
												tok_amt <- as.numeric(tail(pool_val$pool$tok_amt,1))
												base_amt <- as.numeric(tail(pool_val$pool$base_amt,1))
												tok_chg = as.numeric(input$sw_tok_amt)
												base_chg = as.numeric(input$sw_eth_amt)
												if(input$swap_dir=="iacs_eth")
												{
													tok_chg = ((tok_amt*base_amt)/(base_amt+as.numeric(input$sw_eth_amt)))-tok_amt
												}
												if(input$swap_dir!="iacs_eth")
												{
													base_chg = ((tok_amt*base_amt)/(tok_amt+as.numeric(input$sw_tok_amt)))-base_amt
												}
												tok_amt = tail(pool_val$pool$tok_amt,1) + tok_chg
												base_amt = tail(pool_val$pool$base_amt,1) + base_chg
												trade_df = data.frame(
																		trade = ifelse(input$swap_dir=="iacs_eth","ETH for IACS","IACS for ETH"),
																		tok_amt = tok_amt,
																		base_amt = base_amt,
																		tok_chg = tok_chg,
																		base_chg = base_chg,
																		price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																		price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																	)
												if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
											}
										}
				)
	####################################################
	####################################################

	####################################################
	## Liquidity Logic
	####################################################
	output$liq_iacs_amt <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										if(input$liq_dir=="liq_rem")
										{
											tok_amt <- tail(pool_val$pool$tok_amt,1)
											return(h5((input$liq_rem_pct/100)*tok_amt))
										}
										if(input$liq_chc=="liq_eth")
										{
											tok_amt <- tail(pool_val$pool$tok_amt,1)
											base_amt <- tail(pool_val$pool$base_amt,1)
											tok_cost = (tok_amt/base_amt)*input$liq_eth_amt
											return(h5(tok_cost))
										} 
										autonumericInput(
															"liq_tok_amt",
															NULL,
															value=NULL,
															minimumValue=0,
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
									})	
	output$liq_eth_amt <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										if(input$liq_dir=="liq_rem")
										{
											base_amt <- tail(pool_val$pool$base_amt,1)
											return(h5((input$liq_rem_pct/100)*base_amt))
										}
										if(input$liq_chc=="liq_iacs")
										{
											tok_amt <- tail(pool_val$pool$tok_amt,1)
											base_amt <- tail(pool_val$pool$base_amt,1)
											eth_cost = (base_amt/tok_amt)*input$liq_tok_amt
											return(h5(eth_cost))
										}
										autonumericInput(
															"liq_eth_amt",
															NULL,
															value=NULL,
															minimumValue=0,
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
									})
	output$liq_rem_pct <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										if(input$liq_dir!="liq_rem")
										{
											tok_amt <- tail(pool_val$pool$tok_amt,1)
											base_amt <- tail(pool_val$pool$base_amt,1)
											if(input$liq_chc=="liq_iacs")
											{
												return(h5(round(100*(input$liq_tok_amt/(tok_amt+input$liq_tok_amt)),5)))
											}
											if(input$liq_chc=="liq_eth")
											{
												return(h5(round(100*(input$liq_eth_amt/(base_amt+input$liq_eth_amt)),5)))
											}
										}
										autonumericInput(
															"liq_rem_pct",
															NULL,
															value=NULL,
															minimumValue=0,
															maximumValue=100,
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
									})

	observeEvent(input$liq_tok_amt_swap,{
											if(nrow(pool_val$pool)>0)
											{
												tok_amt <- tail(pool_val$pool$tok_amt,1)
												base_amt <- tail(pool_val$pool$base_amt,1)
												if(input$liq_dir=="liq_add")
												{
													if(input$liq_chc=="liq_iacs")
													{
														tok_chg = input$liq_tok_amt
														base_chg = (base_amt/tok_amt)*tok_chg
													}
													if(input$liq_chc=="liq_eth")
													{
														base_chg = input$liq_eth_amt
														tok_chg = (tok_amt/base_amt)*base_chg
													}
												}
												if(input$liq_dir!="liq_add")
												{
													tok_chg = (input$liq_rem_pct/100)*tok_amt*-1
													base_chg = (input$liq_rem_pct/100)*base_amt*-1
												}
												tok_amt = tail(pool_val$pool$tok_amt,1) + tok_chg
												base_amt = tail(pool_val$pool$base_amt,1) + base_chg
												trade_df = data.frame(
																		trade = ifelse(input$liq_dir=="liq_add","Add Liquidity","Remove Liquidity"),
																		tok_amt = tok_amt,
																		base_amt = base_amt,
																		tok_chg = tok_chg,
																		base_chg = base_chg,
																		price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																		price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																	)
												pool_val$pool <- rbind(pool_val$pool,trade_df)
											}
										}
				)
	####################################################
	####################################################

	####################################################
	## Plots
	####################################################
	output$pool_table <- 	renderDataTable({
												if(nrow(pool_val$pool)==0) return(NULL)
												pool_table <- pool_val$pool
												names(pool_table) <- c("Trade","Number IACS","Number ETH","Change IACS","Change ETH","Price IACS","Price ETH")
												pool_table$Trade <- paste0(1:nrow(pool_table),". ",pool_table$Trade)
												pool_table <- as.data.frame(cbind(Trade=pool_table$Trade,round(pool_table[,-1],5)))
												datatable(pool_table,options = list(dom = 'pt',ordering=F,scrollX = TRUE,pageLength = 10),rownames= FALSE)
										})
	output$pool_amt_hc <- 	renderHighchart({
												if(nrow(pool_val$pool)==0) return(NULL)
												pool_table <- pool_val$pool
												pool_tok_ser <- highchart() %>% 
  																hc_yAxis_multiples(
    																				list(lineWidth = 3,title = list(text = "IACS Amount")),
    																				list(opposite = TRUE,title = list(text = "ETH Amount"))
  																				) %>% 
  																hc_add_series(data = pool_table$tok_amt,name = "# IACS",yAxis = 0) %>% 
  																hc_add_series(data = pool_table$base_amt,name = "# ETH", yAxis = 1) %>%
  																hc_legend(enabled = FALSE)
										})
	# output$pool_rat_hc <- 	renderHighchart({
	# 											if(nrow(pool_val$pool)==0) return(NULL)
	# 											pool_table <- pool_val$pool

	# 											pool_tok_ser <- highchart() %>% 
 #  																hc_yAxis_multiples(
 #    																				list(lineWidth = 3,title = list(text = "IACS Amount")),
 #    																				list(opposite = TRUE,title = list(text = "ETH Amount"))
 #  																				) %>% 
 #  																hc_add_series(data = pool_table$tok_amt,name = "# IACS",yAxis = 0) %>% 
 #  																hc_add_series(data = pool_table$base_amt,name = "# ETH", yAxis = 1) %>%
 #  																hc_legend(enabled = FALSE)
	# 									})
	output$pool_price_hc <- renderHighchart({
												if(nrow(pool_val$pool)==0) return(NULL)
												pool_table <- pool_val$pool
												pool_tok_ser <- highchart() %>% 
  																hc_yAxis_multiples(
    																				list(lineWidth = 3,title = list(text = "IACS Price in ETH")),
    																				list(opposite = TRUE,title = list(text = "ETH Price in IACS"))
  																				) %>% 
  																hc_add_series(data = pool_table$price_iacs_eth,name = "IACS Price",yAxis = 0) %>% 
  																hc_add_series(data = pool_table$price_eth_iacs,name = "ETH Price", yAxis = 1) %>% 
  																hc_legend(enabled = TRUE)
										})
	output$pool_pr_d_hc <- renderHighchart({
												if(nrow(pool_val$pool)==0) return(NULL)
												pool_table <- pool_val$pool
												pool_tok_ser <- highchart() %>% 
  																hc_yAxis_multiples(
    																				list(lineWidth = 3,title = list(text = "IACS Price (USD)"))#,
    																				#list(opposite = TRUE,title = list(text = "ETH Price (USD)"))
  																				) %>% 
  																hc_add_series(data = pool_table$price_iacs_eth*input$eth_price_usd,name = "IACS Price (USD)",yAxis = 0) %>% 
  																#hc_add_series(data = rep(input$eth_price_usd,length(pool_table$price_eth_iacs)),name = "ETH Price (USD)", yAxis = 1) %>% 
  																hc_legend(enabled = FALSE)
										})
	####################################################
	####################################################

	####################################################
	## CSV Input
	####################################################
	output$trades_file_in <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										box(title = "Trade File Upload",width=12,solidHeader=TRUE,status="primary",
										fileInput("file_in", NULL, multiple = FALSE, accept = ".csv",buttonLabel = "Browse...", placeholder = "Input trades.csv File")
										)
									})

	observeEvent(input$file_in,{
									data <- read.csv(input$file_in$datapath)
									trade_val$t_tab <- NULL
									trade_val$error <- NULL

									####################################################
									## Quality Check
									####################################################
									trades <- data[,c("iacs_chg","eth_chg","pct_chg")]

									## C1 All rows contain only one value
									c_1 <- apply(trades,1,function(x) sum(!is.na(x)))==1
									if(!all(c_1)) trade_val$error <- "One row contains more/less than 1 values"

									## pct_chg only holds negative value
									if(any(na.omit(trades$pct_chg)>0)) trade_val$error <- "One + value in pct_chg column"

									## If no error
									if(is.null(trade_val$error)) trade_val$t_tab <- trades
									####################################################
									####################################################

									####################################################
									## Parse Trades
									####################################################
									## Parse Table into trades
									t_tab <- trade_val$t_tab
									for(idx in 1:nrow(t_tab))
									{
										c_idx <- which(!is.na(t_tab[idx,]))
										t_idx <- paste0(c_idx,ifelse(t_tab[idx,c_idx]>=0,"P","N"))
										if(!(t_idx %in% c("1N","1P","2N","2P","3N"))) trade_val$error <- "Invalid Trade"
										
										tok_amt <- as.numeric(tail(pool_val$pool$tok_amt,1))
										base_amt <- as.numeric(tail(pool_val$pool$base_amt,1))

										## Swap ETH for IACS
										if(t_idx == "2N")
										{
											base_chg = t_tab[idx,c_idx]
											tok_chg = ((tok_amt*base_amt)/(base_amt+base_chg))-tok_amt
											tok_amt = as.numeric(tail(pool_val$pool$tok_amt,1)) + tok_chg
											base_amt = as.numeric(tail(pool_val$pool$base_amt,1)) + base_chg
											trade_df = data.frame(
																	trade = "IACS for ETH",
																	tok_amt = tok_amt,
																	base_amt = base_amt,
																	tok_chg = tok_chg,
																	base_chg = base_chg,
																	price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																	price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																)
											if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
										}

										## Swap IACS for ETH
										if(t_idx == "1N")
										{
											tok_chg = t_tab[idx,c_idx]
											base_chg = ((tok_amt*base_amt)/(tok_amt+tok_chg))-base_amt
											tok_amt = as.numeric(tail(pool_val$pool$tok_amt,1)) + tok_chg
											base_amt = as.numeric(tail(pool_val$pool$base_amt,1)) + base_chg
											trade_df = data.frame(
																	trade = "ETH for IACS",
																	tok_amt = tok_amt,
																	base_amt = base_amt,
																	tok_chg = tok_chg,
																	base_chg = base_chg,
																	price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																	price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																)
											if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
										}

										## Add Liquidity specifying IACS
										if(t_idx == "1P")
										{
											tok_chg = t_tab[idx,c_idx]
											base_chg = (base_amt/tok_amt)*tok_chg
											tok_amt = as.numeric(tail(pool_val$pool$tok_amt,1)) + tok_chg
											base_amt = as.numeric(tail(pool_val$pool$base_amt,1)) + base_chg
											trade_df = data.frame(
																	trade = "Add Liquidity",
																	tok_amt = tok_amt,
																	base_amt = base_amt,
																	tok_chg = tok_chg,
																	base_chg = base_chg,
																	price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																	price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																)
											if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
										}

										## Add Liquidity specifying ETH
										if(t_idx == "2P")
										{
											base_chg = t_tab[idx,c_idx]
											tok_chg = (tok_amt/base_amt)*base_chg
											tok_amt = as.numeric(tail(pool_val$pool$tok_amt,1)) + tok_chg
											base_amt = as.numeric(tail(pool_val$pool$base_amt,1)) + base_chg
											trade_df = data.frame(
																	trade = "Add Liquidity",
																	tok_amt = tok_amt,
																	base_amt = base_amt,
																	tok_chg = tok_chg,
																	base_chg = base_chg,
																	price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																	price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																)
											if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
										}

										## Remove Liquidity Specifying reductions %ge
										if(t_idx == "3N")
										{
											tok_chg = (t_tab[idx,c_idx]/100)*tok_amt
											base_chg = (t_tab[idx,c_idx]/100)*base_amt
											tok_amt = as.numeric(tail(pool_val$pool$tok_amt,1)) + tok_chg
											base_amt = as.numeric(tail(pool_val$pool$base_amt,1)) + base_chg
											trade_df = data.frame(
																	trade = "Remove Liquidity",
																	tok_amt = tok_amt,
																	base_amt = base_amt,
																	tok_chg = tok_chg,
																	base_chg = base_chg,
																	price_iacs_eth = ((tok_amt*base_amt)/(tok_amt-1))-base_amt,
																	price_eth_iacs = ((base_amt*tok_amt)/(base_amt-1))-tok_amt
																)
											if((tok_chg!=0) & (base_chg!=0)) pool_val$pool <- rbind(pool_val$pool,trade_df)
										}

										if(tok_amt<0 | base_amt<0) trade_val$error <- "Trades Resulted in Negative Balance in Pool"
									}
 							}
				)

	output$trade_table <- 	renderDataTable({
												if(!is.null(trade_val$error)) return(datatable(data.frame(Error=trade_val$error),rownames= FALSE,options = list(dom = 't')))
												if(nrow(pool_val$pool)==0) return(NULL)
												pool_table <- pool_val$pool
												names(pool_table) <- c("Trade","Number IACS","Number ETH","Change IACS","Change ETH","Price IACS","Price ETH")
												pool_table$Trade <- paste0(1:nrow(pool_table),". ",pool_table$Trade)
												pool_table <- as.data.frame(cbind(Trade=pool_table$Trade,round(pool_table[,-1],5)))
												datatable(pool_table,options = list(dom = 'pt',ordering=F,scrollX = TRUE,pageLength = 10),rownames= FALSE)
										})
	####################################################
	####################################################

	####################################################
	## Price Input
	####################################################
	output$eth_price_usd <- renderUI({
										if(nrow(pool_val$pool)==0) return(NULL)
										box(title = "ETH Price in USD",width=12,solidHeader=TRUE,status="primary",
										autonumericInput(
															"eth_price_usd",
															NULL,
															value=1000,
															minimumValue=0,
															decimalPlaces = 18,
															digitGroupSeparator = ",",
															decimalCharacter = ".",
															align = "left",
															allowDecimalPadding=FALSE
														)
										)
									})
}