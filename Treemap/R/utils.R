# Explanation of amount of layers
# for nested other countries/cities/institutions
# calculation of number of nests: last layer can be fully filled 
# all other have one less to accomodate the link to the next layer
# => n + (n-1)*x <= length ===> ceiling((length - n)/(n-1)) = x 
# with n=number of items per layer and rounded up because we need to include all

countriesFuncSimple <- function(n_items, countries) {
  if (nrow(countries) < n_items) { return(countries) }
  
  countries_copy <- countries[0:n_items, ]
  return(countries_copy)
}

citiesFuncSimple <- function(n_items, cities, countries){
  cities_copy <- cities %>% filter(Country %in% countries$Country)
  
  cities_per_country <- cities_copy %>% count(Country)
  countries_many_cities <- cities_per_country %>% filter(n > n_items)
  
  cities_copy2 <-  cities_copy %>% filter(!Country %in% countries_many_cities$Country)
  cities_copy2$hover_value <- cities_copy2$x
  
  for (c in countries_many_cities$Country) {
    
    temp_country <- cities_copy %>% filter(Country==c)
    temp_country <- temp_country[order(temp_country$x, decreasing = TRUE),]
    temp_country <- temp_country[1:n_items, ]
    
    temp_country$hover_value <- temp_country$x
    
    sum_diff_factor <- countries$x[countries$Country == c] / sum(temp_country$x)
    temp_country$x <- round(temp_country$x * sum_diff_factor)
    temp_country$x[1] <- temp_country$x[1] + (countries$x[countries$Country == c] - sum(temp_country$x))
    
    cities_copy2 <- rbind(cities_copy2, temp_country)
  }
  
  return(cities_copy2)
}

institutionFuncSimple <- function(n_items, institutions, cities, countries){
  
  institutions_copy <- institutions %>% filter(City %in% cities$City)
  
  institutions_per_city <- institutions_copy %>% count(City)
  cities_many_institutions <- institutions_per_city %>% filter(n > n_items)
  
  # new df already contains all cities for which no institution will be changed,
  # all others will be added inside the for loop.
  institution_res <- institutions_copy %>% filter(!City %in% cities_many_institutions$City)
  institution_res$hover_value <- institution_res$x
  
  for (c in cities_many_institutions$City) {
    
    temp_city <- institutions_copy %>% filter(City==c)
    temp_city <- temp_city[order(temp_city$x, decreasing = TRUE),]
    temp_city <- temp_city[1:n_items, ]
    
    temp_city$hover_value <- temp_city$x
    
    sum_diff_factor <- cities$x[cities$City == c] / sum(temp_city$x)
    temp_city$x <- round(temp_city$x * sum_diff_factor)
    temp_city$x[1] <- temp_city$x[1] + (cities$x[cities$City == c] - sum(temp_city$x))
    
    institution_res <- rbind(institution_res, temp_city)
    
  }
  
  for (c in countries$Country) {
    temp <- cities %>% filter(Country==c)
    #countries$x[countries$Country == c] <- sum(temp$x)
  }
  
  return(list(countries, cities, institution_res))
}




countriesFunc <- function(n_items, countries) {

  if (nrow(countries) < n_items) { return(countries) }
 
  
  # Countries is ordered and we want to keep this dataframe
  countries_nested <- countries
  
  # x is the number of layers
  x <- ceiling((nrow(countries_nested) - n_items) / (n_items - 1))
  
  # Calculates the sum of each layer. (Including nested sublayers)
  sums_layer <- rep(0L, x)
  
  # The parent of the first layer is still the root itself (called "Countries")
  parents <- rep("Countries", (n_items - 1))
  
  # Calculate sums of each layer and change parents
  for (i in 1:x) {
    # New parent has format f"Other Countries {i}" and gets replicated as 
    # often as there are items in the layer. (not including the newly calculated
    # item that will be in the layer as well and will be the parent of the next layer)
    parent <- paste("Other Countries", i)
    if (i != x){
      parents <- append(parents, rep(parent, (n_items - 1)))
    } else {
      reps <- nrow(countries_nested) - (x * (n_items-1))
      parents <- append(parents, rep(parent, reps))
    }
    
    # Calculate sum of layer
    idx <- ((n_items-1)*i) + 1
    sums_layer[i] <- sum(countries_nested$x[idx:length(countries_nested$x)])
  }
  # Update the parent row of the dataframe of countries
  countries_nested$Parent <- parents
  
  # Create parents/layer-links. I.e., items f"Other Countries {i}"
  for (i in 1:x) {
    name <- paste("Other Countries", i)
    # parent is the previously created item. Except for the first where it is the root itself
    parent <- paste("Other Countries", i-1)
    if (i == 1){parent <- "Countries"}
    
    # add item to df
    countries_nested[nrow(countries_nested) + 1,] <- list(name, sums_layer[i], parent)
  }
  
  countries_nested <- countries_nested[order(countries_nested$x, decreasing=TRUE), ]
  return(countries_nested)
}
  

citiesFunc <- function(n_items, cities) {

  sending_cities_per_country <- cities %>% count(Country)
  sending_countries_many_cities <- sending_cities_per_country %>% filter(n > n_items)
  
  # new df already contains all countries for which no city will be changed,
  # all others will be added inside the for loop.
  cities_nested <- cities %>% filter(!Country %in% sending_countries_many_cities$Country)
  
  for (c in sending_countries_many_cities$Country) {
    
    temp_country <- cities %>% filter(Country==c)
    temp_country <- temp_country[order(temp_country$x, decreasing = TRUE),]
    
    # x is the number of layers
    x <- ceiling((nrow(temp_country) - n_items) / (n_items - 1))
    
    # calculates the sum of each layer. (Including nested sublayers)
    sums_layer <- rep(0L, x)
    
    # The parent of the first layer is still the country it self
    parents <- rep(c, (n_items - 1))
    
    # calculate sums of each layer and change parents
    for (i in 1:x) {
      # new parent has format f"Other City {Country} {i}" and gets replicated as 
      # often as there are items in the layer. (not including the newly calculated
      # item that will be in the layer as well and will be the parent of the next layer)
      parent <- paste("Other City", c, i)
      if (i != x){
        parents <- append(parents, rep(parent, (n_items - 1)))
      } else {
        reps <- nrow(temp_country) - (x * (n_items-1))
        parents <- append(parents, rep(parent, reps))
      }
      
      # calculate sum of layer
      idx <- ((n_items-1)*i) + 1
      sums_layer[i] <- sum(temp_country$x[idx:length(temp_country$x)])
    }
    # update the parent row of the dataframe of this country
    temp_country$Parent <- parents
    
    # create parents/layer-links. I.e., items f"Other city {Country} {i}"
    for (i in 1:x) {
      name <- paste("Other City", c, i)
      # parent is the previously created item. Except for the first where it is the country itself
      parent <- paste("Other City", c, i-1)
      if (i == 1){parent <- c}
      
      # add item to df
      temp_country[nrow(temp_country) + 1,] <- list(c, name, sums_layer[i], parent)
    }
    
    cities_nested <- rbind(cities_nested, temp_country)
  }
  
  cities_nested <- cities_nested[order(cities_nested$Country), ]
  return(cities_nested)
}

institutionsFunc <- function(n_items, institutions, cities) {

  institutions_per_city <- institutions %>% count(City)
  cities_many_institutions <- institutions_per_city %>% filter(n > n_items)
  
  # new df already contains all cities for which no institution will be changed,
  # all others will be added inside the for loop.
  institution_nested <- institutions %>% filter(!City %in% cities_many_institutions$City)
  
  for (c in cities_many_institutions$City) {
    
    temp_city <- institutions %>% filter(City==c)
    temp_city <- temp_city[order(temp_city$x, decreasing = TRUE),]
    
    # x is the number of layers
    x <- ceiling((nrow(temp_city) - n_items) / (n_items - 1))
    
    # calculates the sum of each layer. (Including nested sublayers)
    sums_layer <- rep(0L, x)
    
    # The parent of the first layer is still the city it self
    parents <- rep(c, (n_items - 1))
    
    # calculate sums of each layer and change parents
    for (i in 1:x) {
      # new parent has format f"Other Institution {City} {i}" and gets replicated as 
      # often as there are items in the layer. (not including the newly calculated
      # item that will be in the layer as well and will be the parent of the next layer)
      parent <- paste("Other Institution", c, i)
      if (i != x){
        parents <- append(parents, rep(parent, (n_items - 1)))
      } else {
        reps <- nrow(temp_city) - (x * (n_items-1))
        parents <- append(parents, rep(parent, reps))
      }
      
      # calculate sum of layer
      idx <- ((n_items-1)*i) + 1
      sums_layer[i] <- sum(temp_city$x[idx:length(temp_city$x)])
    }
    # update the parent row of the dataframe of this country
    temp_city$Parent <- parents
    
    country <- cities %>% filter(City==c)
    country <- country$Country
    country <- country[1]
    
    
    # create parents/layer-links. I.e., items f"Other city {Country} {i}"
    for (i in 1:x) {
      name <- paste("Other Institution", c, i)
      # parent is the previously created item. Except for the first where it is the country itself
      parent <- paste("Other Institution", c, i-1)
      if (i == 1){parent <- c}
      
      # add item to df
      temp_city[nrow(temp_city) + 1,] <- list(country, c, name, sums_layer[i], parent)
    }
    
    institution_nested <- rbind(institution_nested, temp_city)
  }
  
  institution_nested <- institution_nested[order(institution_nested$City), ]
  return(institution_nested)
}