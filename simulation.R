run_model = function(agent_states,edge_list,rounds_of_interaction,influence_rate) {
  history=c()
  for (i in 1:rounds_of_interaction) {
    # print(paste('Running interaction round',i,'of',rounds_of_interaction))
    if (nrow(edge_list)>0) { # in case there are no edges at all!
      pair = edge_list[sample(1:nrow(edge_list),1),]
      agent_1 = pair[1] # choose a random pair
      agent_2 = pair[2]
      connection = connection_matrix[agent_1,agent_2] # what is their connection?
      agent_states[agent_2] = agent_states[agent_2] + 
        influence_rate*agent_states[agent_1]*connection # update agent state
      agent_states[agent_states>1] = 1 # make sure agents don't go above 1
    }
    history = rbind(history,agent_states) # save history
  }
  return(history)
}

make_connections = function(n_agents,n_delete=0,type='random',del_self=TRUE) {
  if (type=='random') {
    connection_matrix = matrix(runif(n_agents*n_agents),nrow=n_agents)
  } else if (type=='ones') {
    connection_matrix = matrix(1,nrow=n_agents,ncol=n_agents)
  } else if (type=='circle') {
    # ixes = sample(1:n_agents,n_agents)
    ixes = 1:n_agents
    connection_matrix = matrix(0,nrow=n_agents,ncol=n_agents)
    for (i in 1:(n_agents-1)) {
      connection_matrix[ixes[i],ixes[i+1]]=1
    }
    connection_matrix[ixes[n_agents],ixes[1]]=1
  }
  
  if (del_self) {
    connection_matrix[diag(n_agents)==1] = 0
  }
  
  connection_matrix[sample(1:(n_agents*n_agents),n_delete)] = 0
  
  return(connection_matrix)
}

plot_history = function(history,colors=NULL,line_width=2,line_type='l',y_axis_range=c(0,1)) {
  if (is.null(colors)) {
    colors = rainbow(ncol(history))
  }
  for (i in 1:ncol(history)) {
    if (i==1) {
      plot(history[,i],type=line_type,lwd=line_width,
           ylim=y_axis_range,xlim=c(1,nrow(history)),
           xlab='Iteration',ylab='Agent state',
           col=colors[i])    
    } else {
      points(history[,i],type=line_type,lwd=line_width,col=colors[i])    
    }
  }
}

plot_graph = function(connection_matrix,vertex_sizes,colors) {
  # par(mfrow=c(1,1)) 
  edge_list = which(connection_matrix>0,arr.ind=TRUE)
  g = graph_from_edgelist(edge_list)
  V(g)$color=colors
  plot(g,layout=layout_in_circle(g),vertex.label.cex=.5,vertex.size=20*vertex_sizes,edge.width=.25,edge.arrow.size=0.1)
}
