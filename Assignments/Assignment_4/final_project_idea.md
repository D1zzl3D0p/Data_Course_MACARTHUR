My idea for the final project is to try and measure the correlation between streaming services increasing in price, and piracy rates. It's pretty easy to find streaming price rates, but exact data on piracy will likely be hard to come by due to the nature of the thing. I haven't looked for the piracy data yet, though, so maybe it's easier than I'm thinking. I was planning on using Google Trends Data as a proxy instead. Theoretically, all the data points should have a date associated as well as fields for the streaming service, the pricing at that time, and anywhere from 1-5 measures of piracy at that time. I may also need to introduce some sort of additional massaging into the pricing data, as people are not going to suddenly switch up their streaming habits as quickly as a company switches up their monthly prices. I was thinking of implementing something like a poison distribution to each price hike, to more accurately represent people knowing about the price hike, but maybe just using a different statistical test would be better.

``` r
rdata <- read_csv("../../Data/streaming_service.csv")
stream <- rdata |>
  # you have to use paste0 because default paste adds a space in between
  mutate(date=as.POSIXct(paste0("01-",date),format='%d-%b-%Y'))|> 
  mutate(service = as.factor(service))
  
  stream |>
  arrange(date) |>
  ggplot(aes(x=date,y=price,color=service))+
  geom_point()+
  geom_line()
```
