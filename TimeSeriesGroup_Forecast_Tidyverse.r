#Forecasting Time Series Groups in the tidyverse

library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

bike_sales
head(bike_sales)

#transform the data set by aggregating by month.

bike_sales_monthly <- bike_sales %>%
  mutate(month = month(order.date, label = TRUE),
         year  = year(order.date)) %>%
  group_by(year, month) %>%
  summarise(total.qty = sum(quantity)) 
bike_sales_monthly

#visualize package with a month plot using the ggplot2 .

bike_sales_monthly %>%
  ggplot(aes(x = month, y = total.qty, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",
       subtitle = "March through July tend to be most active") +
  scale_y_continuous() +
  theme_tq()

#Performing Forecasts on Groups

monthly_qty_by_cat2 <- bike_sales %>%
  mutate(order.month = as_date(as.yearmon(order.date))) %>%
  group_by(category.secondary, order.month) %>%
  summarise(total.qty = sum(quantity))
monthly_qty_by_cat2

monthly_qty_by_cat2_nest <- monthly_qty_by_cat2 %>%
  group_by(category.secondary) %>%
  nest(.key = "data.tbl")
monthly_qty_by_cat2_nest

#-------------------Forecasting Workflow---------------------------#
#Step 1: Coerce to a ts object class
monthly_qty_by_cat2_ts <- monthly_qty_by_cat2_nest %>%
  mutate(data.ts = map(.x       = data.tbl, 
                       .f       = tk_ts, 
                       select   = -order.month, 
                       start    = 2011,
                       freq     = 12))
monthly_qty_by_cat2_ts

#Step 2: Modeling a time series

monthly_qty_by_cat2_fit <- monthly_qty_by_cat2_ts %>%
  mutate(fit.ets = map(data.ts, ets))
monthly_qty_by_cat2_fit

#sw_tidy

monthly_qty_by_cat2_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy, .drop = TRUE) %>%
  spread(key = category.secondary, value = estimate)

#sw_glance
monthly_qty_by_cat2_fit %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance, .drop = TRUE)

#sw_augment

augment_fit_ets <- monthly_qty_by_cat2_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment, .drop = TRUE)

augment_fit_ets

#plot
augment_fit_ets %>%
  ggplot(aes(x = date, y = .resid, group = category.secondary)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ category.secondary, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

#sw_tidy_decomp

monthly_qty_by_cat2_fit %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

#Step 3: Forecasting the model

monthly_qty_by_cat2_fcast <- monthly_qty_by_cat2_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 12))
monthly_qty_by_cat2_fcast

#Step 4: Tidy the forecast

monthly_qty_by_cat2_fcast_tidy <- monthly_qty_by_cat2_fcast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)
monthly_qty_by_cat2_fcast_tidy


#plot

monthly_qty_by_cat2_fcast_tidy %>%
  ggplot(aes(x = index, y = total.qty, color = key, group = category.secondary)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ category.secondary, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

