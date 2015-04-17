#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

statistics=function(total_samples,r,c,p,iterations){
  elements=unlist(sampling(total_samples,r,c,p,iterations))
  jamprop=sum(elements=="Traffic Jam")/total_samples
  names(jamprop)="Estimated Proportion of Traffic Jams"
  
  mixprop=sum(elements=="Mix of Free Flow and Traffic")/total_samples
  names(mixprop)="Estimated Proportion of Mixed Flow"  
  
  flowprop=1-jamprop-mixprop
  names(flowprop)="Estimated Proportion of Free Flowing Traffic"
  
  return(list(jamprop,mixprop,flowprop))
}

varydensity=function(total_samples,low,high,r,c,iterations){
  for(i in seq(low,high,by=0.01)){
    statistics(total_samples,r,c,i,iterations)
  }
}

varygridsize=function(total_samples,r,least_n,most_n,p,iterations){
  for(i in seq(least_c,most_c)){statistics(total_samples,m,i,p,iterations)}
}

varysquare=function(total_samples,least_n,most_n,p,iterations){
  for(i in seq(least_c,most_c)){statistics(total_samples,i,i,p,iterations)}
}

size_test = function() {
  stats_10x10_50 = statistics(20,10,10,0.5,3000)
  stats_25x25_50 = statistics(20,25,25,0.5,3000)
  stats_50x50_50 = statistics(20,50,50,0.5,3000)
  stats_10x10_30 = statistics(20,10,10,0.3,3000)
  stats_25x25_30 = statistics(20,25,25,0.3,3000)
  stats_50x50_30 = statistics(20,50,50,0.3,3000)
  save(stats_10x10_50, stats_25x25_50, stats_50x50_50, stats_10x10_30, stats_25x25_30, stats_50x50_30, file = 'stats.RData')
}