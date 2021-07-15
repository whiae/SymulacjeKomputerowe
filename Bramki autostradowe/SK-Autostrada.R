library(simmer)

results <- c()
for (L in (10:60)) {
  for (s in (c(12345, 393943, 100005, 777999555, 319999772))) {
    set.seed(s)
    means <- c()
    
    customer <- 
      trajectory("Customer's path") %>%
      log_("Here I am") %>%
      set_attribute("start_time", function() {now(highway)}) %>%
      select(c("gate1", "gate2", "gate3", "gate4"), policy="shortest-queue") %>%
      seize_selected() %>%
      log_(function() {paste("Waited: ", now(highway) - get_attribute(highway, "start_time"))}) %>%
      timeout(function() {rnorm (1, mean = 10, sd = 5)}) %>%
      release_selected() %>%
      log_(function() {paste("Finished: ", now(highway))})
    
    customer2 <- 
      trajectory("Customer's path") %>%
      log_("Here I am") %>%
      set_attribute("start_time", function() {now(highway)}) %>%
      select(c("gate2", "gate3", "gate4"), policy="shortest-queue") %>%
      seize_selected() %>%
      log_(function() {paste("Waited: ", now(highway) - get_attribute(highway, "start_time"))}) %>%
      timeout(function() {rnorm (1, mean = 20, sd = 15)}) %>%
      release_selected() %>%
      log_(function() {paste("Finished: ", now(highway))})
    
    highway <-
      simmer("highway") %>%
      add_resource("gate1", 1) %>%
      add_resource("gate2", 1) %>%
      add_resource("gate3", 1) %>%
      add_resource("gate4", 1) %>%
      add_generator("Karta", customer, function() {c(0, rexp(499, 1/L), -1)}) %>%
      add_generator("Gotowka", customer2, function() {c(0, rexp (499, 1/L), -1)})
    
    highway %>% run(until = 50000)
    result <-
      highway %>% get_mon_arrivals() %>%
      transform(waiting_time = end_time - start_time - activity_time)
    means <- c(means, mean(result$waiting_time))
      }
  averaged <- mean(means)
  results <- c(results, averaged)
  means <- c()
  averaged <- 0
}

plot(60:10, results, type='l',
     xlab = "Liczba pojazdów na minutê", ylab = "Œredni czas oczekiwania [min]")


data.frame("L" = 60:10,"Czas" = results) 
