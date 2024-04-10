
library(tidyverse)
Child_Care<-read_csv("childcare_infants.csv") 

#Which year has the most families with the highest
unique(Child_Care$poverty_families)
pov_fam <- Child_Care |> 
  group_by(study_year) |> 
  summarise(avg_pov_fam = mean(poverty_families))

ggplot(data = pov_fam,
       mapping = aes(x=study_year,
                     y=avg_pov_fam,
                     fill=avg_pov_fam,
                     label=avg_pov_fam))+
  geom_col()+
  geom_label(vjust= 1,
             color="red",
             fill="white")+
 theme_minimal()+
  scale_fill_viridis_c()
#Which year has the most families with the highest average poverty rates?
pov_fam_v2 <- pov_fam |> 
  mutate(aveg_pov_fam_one_digit = number(avg_pov_fam, accuracy=0.1))

ggplot(data = pov_fam_v2,
       mapping = aes(x=study_year,
                     y=avg_pov_fam,
                     fill=avg_pov_fam,
                     label=aveg_pov_fam_one_digit))+
  labs(title = "Plot showing poverty rates for families from 2008 to 2018",
       x="Year",
       y="Average Poverty rates by Family",
       fill="Average Rates")+
  geom_col()+
  geom_label(vjust= 1,
             color="red",
             fill="white")+
  theme_minimal()+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))

#what is the distribution of average median income per state ?
avg_med_income_by_state <- Child_Care |> 
  group_by(state_name,study_year,total_population) |> 
  summarise(avg_med_income = mean(median_income_2018))

ggplot(data = avg_med_income_by_state,
       mapping = aes(x=avg_med_income,
                   fil=state_name))+
  labs(title = "A histogram showing the distribution of Income across states",
       x="Average Median Income")+
  geom_histogram()
  
  
#What is the average price paid Center-based care by state?
  Avg_median_center_family_infant_state <-
    Child_Care |> 
    group_by(state_abbreviation) |> 
    summarise(avg_median_center=mean(median_center_infant),
              avg_median_family=mean(median_family_infant)) |> 
    drop_na()
  
  ggplot(data =  Avg_median_center_family_infant_state ,
         mapping = aes(x=state_abbreviation,
                       y=avg_median_center,
                       fill=avg_median_center))+
    labs(title = "A bar plot showing the Average median price paid for centre-based care across states",
         x="State",
         y="Average Median Price",
         fill="Average Median Price")+
    geom_col()+
    theme_minimal()+
    scale_fill_viridis_c(option = "B")
  
  
#What is the average price paid family-based care by state?
  
  ggplot(data = Avg_median_center_family_infant_state ,
         mapping = aes(x=state_abbreviation,
                       y=avg_median_family,
                       label=avg_median_family,
                    fill=avg_median_family))+
    labs(title = "A plot showing the Average median price for family care by state",
         x = "State",
         y="Average median price by family",
         fill="Average median Price")+
    geom_col()+
    theme_minimal()+
    scale_fill_viridis_c(option="H")

  
  #What is the average price paid family-based care by state
  #Adding teXt to plots
  
  Avg_median_center_family_infant_state_v2<-  Avg_median_center_family_infant_state |> 
    mutate(avg_median_family_one_digit = number(avg_median_family, accuracy=0.1),
           avg_median_centre_one_digit = number(avg_median_center, accuracy=0.1))
  
  ggplot(data = Avg_median_center_family_infant_state_v2,
         mapping = aes(x=state_abbreviation,
                       y=avg_median_family,
                       label=avg_median_family_one_digit,
                       fill=avg_median_family))+
    labs(title = "A plot showing the Average median price for family care by state",
         x = "State",
         y="Average median price by family",
         fill="Average median Price")+
    geom_col()+
    theme_minimal()+
    scale_fill_viridis_c(option="G")+
    geom_label(vjust = 1.0,
               color="red",
               fill="white")
  
   
  
#what is the average poverty rate of individuals across the different years of study?
  
  Pov_Ind_year <- Child_Care |> 
    group_by(study_year) |> 
    summarise(avg_ind_pov=mean(poverty_individuals),
              avg_popn = mean(total_population))
  ggplot(data = Pov_Ind_year,
         mapping = aes(x=study_year,
                       y= avg_ind_pov,
                       fill=avg_ind_pov))+
    labs(title = "Average Poverty rates of Individuals from 2008 to 2018",
         x ="Year",
         y="Average Individual Poverty Rates",
         fill="Avg Poverty Individual rates")+
    
    geom_col()+
    theme_minimal()+
    scale_fill_viridis_c(option = "A")+
    scale_x_continuous(breaks = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
   
  #From the above i would like to make a comparison between individual and family rates
  #across the years using the geom_line function but i have kind of failed to figure it out


#Is there a relationship between poverty rates of individuals and families?
  ggplot(data = Child_Care)+
    geom_point(aes(x=poverty_families,
                   y=poverty_individuals)) +
    scale_color_viridis_c() 
  #I need help to make this graph look better
  #i have tried to put different aes like color=state 
  #but the plot is not coming out the way i expect it to
    
  
# what is the Average population across the different states?   
popn_by_state<- Child_Care|> 
  group_by(state_abbreviation) |> 
summarise(Avg_Popn=mean(total_population))

   ggplot(data =popn_by_state ,
          mapping = aes(x=state_abbreviation,
                        y=Avg_Popn,
                        fill=Avg_Popn))+
     labs(x="State",
          y="Average Total Population",
          fill="Population")+
   geom_col()+
     scale_fill_viridis_c(option="C")+
     theme_minimal()+
  coord_flip()
  #I would like to know how to scale the exponential values properly

   
#WAY FORWARD  
#i have found difficulties in plotting 2 or more variables on a plot 
#even when i have grouped the data,i would like guidance on that and 
#and on using other geom plots like the pie chart, line, etc
   
  
install.packages("quarto")
library(tinytex) 
  
  













