pd_wk_17 <- read_csv("pulse2020_puf_17.csv")


# let's do a facet thing on whether someone has received for UI benefits
# make sure to look at universe
pd_wk_17$UI_RECV
  
pd_wk_17 <- pd_wk_17 %>%
  filter(UI_RECV ==  1 |UI_RECV ==  2 ) %>%
#  filter(MORTCONF == c(1,2,3,4,5))

count(pd_wk_17,UI_RECV)

count(pd_wk_17,MORTCONF)

prop.table(table(pd_wk_17$MORTCONF)) *100

# 1-not     2-slight 3-moderate 4- high   5- deferred 
# 4.760127  7.438865 16.641777 69.908531  1.250700 


ggplot(pd_wk_17, aes(MORTCONF)) +
  geom_bar() + 
  theme_classic()


# pretty small sample here, eh



# FOR RENTERS ONLY

pd_wk_17_rent <- pd_wk_17 %>%
  dplyr::filter(RENTCUR == c(1,2))

count(pd_wk_17_rent , RENTCUR)

# recode for model

pd_wk_17_rent <- pd_wk_17_rent %>%
  dplyr::mutate(cur_rent = ifelse(RENTCUR == 1, 1, 0))

pd_wk_17_rent$cur_rent

?glm


pulse_17_model <- glm(cur_rent ~  UI_RECV, family = binomial(link = "logit") , data = pd_wk_17_rent)


summary(pulse_17_model)

class(pd_wk_17_rent$EST_ST)
# Map people who were current on rent

pd_wk_17_rent$EST_ST <- as.character(pd_wk_17_rent$EST_ST)

pd_wk_17_rent$EST_ST

# need to match FIPS by adding 0's to single digit values






class(pd_wk_17_rent$EST_ST)

pd_wk_17_rent$EST_ST

st_shape_file <- states(cb = TRUE)


st_shape_file




# need to group by fips 

pd_wk_17_nlate_rent <- pd_wk_17_rent %>%
  dplyr::filter(cur_rent == 1)

pd_wk_17_nlate_rent$cur_rent

pd_wk_17_late_rent <- pd_wk_17_rent %>%
  dplyr::filter(cur_rent == 0)

pd_wk_17_late_rent$cur_rent

pd_wk_17_nlate_rent_group <- pd_wk_17_nlate_rent %>%
  dplyr::group_by(EST_ST) %>%
  summarise(nlate = n())
  

pd_wk_17_late_rent_group <- pd_wk_17_late_rent %>%
  dplyr::group_by(EST_ST) %>%
  summarise(late = n())


pd_wk_17_late_rent_group
pd_wk_17_nlate_rent_group

pd_wk_17_tot_group <- full_join(pd_wk_17_late_rent_group, pd_wk_17_nlate_rent_group,
                                by = "EST_ST")


pd_wk_17_tot_group <- pd_wk_17_tot_group %>%
  mutate(pct_late = (late/(late+nlate))*100)


head(pd_wk_17_tot_group)


pd_wk_17_tot_group <- rename(pd_wk_17_tot_group, fips = EST_ST)

pd_wk_17_tot_group
pd_wk_17_tot_group$pct_late

plot_usmap(regions = "states")




font_add_google("Roboto", "Roboto")




newplot <- plot_usmap(data = pd_wk_17_tot_group, values = "pct_late", color = "red") +
  scale_fill_continuous(low = "white", high = "red", name = "Percent Late") +
  labs(title = "PCT LATE")  +
  theme(text = element_text(family = "mono"))

ggplotly(newplot)

st_shape_file

?geo_join

pd_wk_17_tot_group$fips <- as.character(pd_wk_17_tot_group$fips)

pd_wk_17_tot_group$fips

pd_wk_17_tot_group$fips[1] = "01"
pd_wk_17_tot_group$fips[2] = "02"
pd_wk_17_tot_group$fips[3] = "04"
pd_wk_17_tot_group$fips[4] = "05"
pd_wk_17_tot_group$fips[5] = "06"
pd_wk_17_tot_group$fips[6] = "08"
pd_wk_17_tot_group$fips[7] = "09"
pd_wk_17_tot_group$fips[8] = "10"

pd_wk_17_tot_group$fips



rent_map <- tigris::geo_join(st_shape_file, pd_wk_17_tot_group, 'GEOID', 'fips')

rent_map$pct_late

rent_map$pct_late


pal <- colorNumeric("YlOrRd", domain = c(5,25))







mytext <- paste(
  round(rent_map$pct_late, digits = 2),"%") %>%
  lapply(htmltools::HTML)


print(rent_map)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = rent_map , 
              fillColor = ~pal(rent_map$pct_late), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              label = mytext)  %>%
  leaflet::addLegend("bottomright", pal = pal, values = rent_map$pct_late,
            title = "% Of Respondents Late on Rent: Oct. 14- Oct. 26",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1
  )





