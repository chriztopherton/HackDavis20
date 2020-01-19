p = plot_ly(df_classroom_cleaned, x = ~Date) %>%
  try(add_lines(y = ~Chilled_water, name = 'Chilled Water')) %>%
  try(add_lines(y = ~Electricity, name = 'Electricity')) %>%
  try(add_lines(y = ~Steam, name = 'Steam')) %>%
  try(add_lines(y = ~Natural_gas, name = 'Natural Gas')) %>%
  try(add_lines(y = ~Solar, name = 'Solar')) %>%
  layout(
    title = "Energy Usage",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    
    yaxis = list(title = "Price"))

p
