library(tidyverse)
library(geodist)


vrp <- function(dropping_points, vehicle_details, hub_lat_long, max_steps=500){
  number_of_iteration <- max_steps
  
  iteration_counter <- 1
  
  optimal_cost <- NULL
  optimal_vrp_assignment <- data.frame()
  
  t_iteration <- 0
  
  #Code starts
  
  while (iteration_counter <= max_steps) {
    vrp_start_time <- Sys.time()
    
    assigned_vehicle_details <- data.frame(assigned_vehicle=double(), 
                                           id=double(),
                                           time_to_travel=double())
    list_of_ids <- as.vector(vehicle_details$vehicle_id)
    list_of_id <- as.vector(dropping_points$id)
    
    second_nearest_id <- 2
    
    while(nrow(assigned_vehicle_details) <=(nrow(dropping_points)-1)){
      
      selected_vehicle <-ifelse(length(list_of_ids)>1, sample(list_of_ids, 1), list_of_ids)
      list_of_ids<-list_of_ids[list_of_ids!=selected_vehicle]
      selected_vehicle_capacity <- vehicle_details$capacity[vehicle_details$vehicle_id==selected_vehicle]
      
      
      selected_id <- ifelse(length(list_of_id)>1, sample(list_of_id, 1), list_of_id)
      list_of_id<-list_of_id[list_of_id!=selected_id]
      selected_id_demand <- dropping_points$demand[dropping_points$id==selected_id]
      
      
      dropping_points %>%
        filter(id %in% list_of_id) %>%
        slice_min(demand) %>%
        pull(demand)-> minimum_demand
      
      minimum_demand <- ifelse(length(minimum_demand)==0, 0, minimum_demand)
      
      while (selected_vehicle_capacity > (vehicle_details$capacity[vehicle_details$vehicle_id==selected_vehicle] * 0.02) & selected_vehicle_capacity>=minimum_demand) {
        dropping_points %>%
          filter(id %in% selected_id) %>%
          select(long, lat) -> selected_id_details
        
        if(! selected_vehicle %in% assigned_vehicle_details$assigned_vehicle){
          time_to_travel <- (geodist(x=as.matrix(selected_id_details), y=as.matrix(hub_lat_long))/1000)*(60/9)
          temp <- data.frame(cbind(assigned_vehicle=selected_vehicle, id=selected_id, time_to_travel=time_to_travel[1,1]))
          
          assigned_vehicle_details %>%
            bind_rows(temp)-> assigned_vehicle_details
          
          if(nrow(assigned_vehicle_details)==nrow(dropping_points)){
            break
          }
          
          selected_vehicle_capacity <- selected_vehicle_capacity - selected_id_demand
          
          dropping_points %>%
            filter(id %in% list_of_id) %>%
            select(long, lat)-> remaining_ids
          
          dist_from_id<-geodist(x=as.matrix(selected_id_details), y=as.matrix(remaining_ids))/1000
          selected_id <-list_of_id[which.min(dist_from_id)]
          selected_id_demand <- dropping_points$demand[dropping_points$id==selected_id]
        }else{
          if(selected_vehicle_capacity<selected_id_demand){
            selected_id <- list_of_id[order(unlist(dist_from_id))[second_nearest_id]]
            selected_id_demand <- dropping_points$demand[dropping_points$id==selected_id]
            selected_id_demand <- ifelse(length(selected_id_demand)==0, 0, selected_id_demand)
            
            if(selected_vehicle_capacity>selected_id_demand){
              dropping_points %>%
                filter(id %in% selected_id) %>%
                select(long, lat) -> selected_id_details
              
              last_id <- assigned_vehicle_details$id[nrow(assigned_vehicle_details)]
              
              dropping_points %>%
                filter(id %in% last_id) %>%
                select(long, lat) -> last_id_details
              
              time_to_travel <- (geodist(x=as.matrix(selected_id_details), y=as.matrix(last_id_details))/1000)*(60/9)
              temp <- data.frame(cbind(assigned_vehicle=selected_vehicle, id=selected_id, time_to_travel=time_to_travel[1,1]))
              
              assigned_vehicle_details %>%
                bind_rows(temp)-> assigned_vehicle_details
              
              if(nrow(assigned_vehicle_details)==nrow(dropping_points)){
                break
              }
              
              selected_vehicle_capacity <- selected_vehicle_capacity - selected_id_demand
              list_of_id<-list_of_id[list_of_id!=selected_id]
              
              dropping_points %>%
                filter(id %in% list_of_id) %>%
                select(long, lat)-> remaining_ids
              
              dist_from_id<-geodist(x=as.matrix(selected_id_details), y=as.matrix(remaining_ids))/1000
              selected_id <-list_of_id[which.min(dist_from_id)]
              selected_id_demand <- dropping_points$demand[dropping_points$id==selected_id]
              second_nearest_id <- 2
            }else{
              second_nearest_id <- second_nearest_id+1
            }
            
          }else{
            dropping_points %>%
              filter(id %in% selected_id) %>%
              select(long, lat) -> selected_id_details
            
            last_id <- assigned_vehicle_details$id[nrow(assigned_vehicle_details)]
            
            dropping_points %>%
              filter(id %in% last_id) %>%
              select(long, lat) -> last_id_details
            
            time_to_travel <- (geodist(x=as.matrix(selected_id_details), y=as.matrix(last_id_details))/1000)*(60/9)
            temp <- data.frame(cbind(assigned_vehicle=selected_vehicle, id=selected_id, time_to_travel=time_to_travel[1,1]))
            
            assigned_vehicle_details %>%
              bind_rows(temp)-> assigned_vehicle_details
            
            if(nrow(assigned_vehicle_details)==nrow(dropping_points)){
              break
            }
            
            selected_vehicle_capacity <- selected_vehicle_capacity - selected_id_demand
            list_of_id<-list_of_id[list_of_id!=selected_id]
            
            dropping_points %>%
              filter(id %in% list_of_id) %>%
              select(long, lat)-> remaining_ids
            
            dist_from_id<-geodist(x=as.matrix(selected_id_details), y=as.matrix(remaining_ids))/1000
            selected_id <-list_of_id[which.min(dist_from_id)]
            selected_id_demand <- dropping_points$demand[dropping_points$id==selected_id]
          }
        }
        
        dropping_points %>%
          filter(id %in% list_of_id) %>%
          slice_min(demand) %>%
          pull(demand)-> minimum_demand
        
        minimum_demand <- ifelse(length(minimum_demand)==0, 0, minimum_demand)
      }
    }
    
    assigned_vehicle_details %>%
      left_join(dropping_points, by='id') %>%
      mutate(total_time= time_to_travel + time_to_serve)-> assigned_vehicle_details
    
    assigned_vehicle_details %>%
      mutate(total_time= time_to_travel + time_to_serve) %>%
      group_by(assigned_vehicle, by="vehicle_id") %>%
      summarise(vehicle_contract_time= sum(total_time)) %>%
      rename(vehicle_id= assigned_vehicle) %>%
      left_join(vehicle_details) %>%
      mutate(multiplier= capacity/800,
             cost= ceiling(vehicle_contract_time/60)*multiplier) %>%
      pull(cost) %>%
      sum() -> vrp_cost
    
    
    
    if(iteration_counter==1){
      optimal_cost <- vrp_cost
      optimal_vrp_assignment <- assigned_vehicle_details
    }else if(vrp_cost<optimal_cost){
      optimal_cost <- vrp_cost
      optimal_vrp_assignment <- assigned_vehicle_details
    }else if(vrp_cost>optimal_cost){
      iteration_counter
    }
    
    vrp_end_time <- Sys.time()
    
    t_iteration <- (vrp_end_time-vrp_start_time) + t_iteration
    
    print(paste0('The number of iteration: ',iteration_counter,'. Time elapsed: ', t_iteration))
    
    iteration_counter <- iteration_counter + 1
    
  }
  
  return(optimal_vrp_assignment)
}


#Creating the dummy data

vehicle_name <- c('mini truck (ace)', 'truck 14ft', 'truck 9ft')

vehicle_details <- data.frame(vehicle_name)

set.seed(4)
vehicle_details$vehicle_id <- round(runif(nrow(vehicle_details), 100000, 999999))

vehicle_details$capacity <- c(700,3500,2000)


set.seed(4)
dropping_points <- data.frame(lat=runif(50, min=17, max=19), 
                              long=runif(50, min=77, max=78),
                              demand=runif(50, min=80, max=100),
                              time_to_serve=runif(50, min=20, max=40))

dropping_points %>%
  rowid_to_column(var='id')-> dropping_points

hub_lat_long <- data.frame(lat=17.42, long=77.7)


#Running the function

optimal_vrp<-vrp(dropping_points = dropping_points, vehicle_details = vehicle_details, hub_lat_long = hub_lat_long, max_steps = 50)

left_join(dropping_points, optimal_vrp) %>%
  mutate(assigned_vehicle= as.factor(assigned_vehicle))-> full_vehicle_assignment

ggplot()+
  geom_point(data= full_vehicle_assignment, aes(x=long, y=lat, col=assigned_vehicle), size=5)+
  theme_bw()+
  scale_colour_brewer(palette="Set1")+
  theme(legend.position = 'top')+
  coord_equal()
