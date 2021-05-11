#####################################################
#      round_robin_fixture function                 #
#####################################################

round_robin_fixture<-function(number_of_teams,method)
{
 
  ############################
  #   cyclic method          #
  ###########################
  
  if(method=="cyclic")
  {
    
    
    team=c(1:number_of_teams)
    lteam=length(team)
    
    
    rotater<-function(vec, shift=1)
    {
      n<-length(vec) 
      
      for(i in 1:shift)
      {
        temp=vec[n]
        vec[2:n]<-vec[1:n-1]
        vec[1]<-temp
        
        
      }
      return(vec)
      
    }
    
    
    if(lteam%%2==1)
    {
      fixed="Bye"
      tvector<-c(1:number_of_teams)
      
      nmatches=(lteam*(lteam-1))/2
      nrounds=lteam
      
      message("\n Number of matches: ", nmatches,"\n\n")
      message("\n Number of Rounds: ",nrounds,"\n\n")
      
      
      
      for(i in 1:nrounds)
      {
        message("\n-------------------------------------------\n")
        message("             ROUND ",i,"\n")
        message("-------------------------------------------\n\n")
        
        if(i==1)
        {
          vect=tvector
          
        }  else
        {vect<-rotater(tvector)  
        }
        message("\n Bye -->",appendLF = FALSE)
        
        for(j in 1:length(vect))
        {
          
          if(j==length(vect))
          {
            
            message(vect[j],appendLF = FALSE)
            
          }else
          {message(vect[j]," -->",appendLF = FALSE)
          }
        }
        
        tvector<-c()
        tvector<-vect
        
        
      }
      
    } else  
    {
      
      fixed=1
      nmatches=(lteam*(lteam-1))/2
      nrounds=lteam-1
      
      tvector<-c(2:number_of_teams)
      
      message("\n Number of matches: ", nmatches,"\n\n")
      message("\n Number of Rounds: ",nrounds,"\n\n")
      
      
      
      for(i in 1:nrounds)
      {
        message("\n-------------------------------------------\n")
        message("             ROUND ",i,"\n")
        message("-------------------------------------------\n\n")
        
        if(i==1)
        {
          vect=tvector
          
        }  else
        {vect<-rotater(tvector)  
        }  
        
        message("\n 1 -->",appendLF = FALSE)
        
        for(j in 1:length(vect))
        {
          if(j==length(vect))
          {
            
            message(vect[j],appendLF = FALSE)
            
          }else
          {message(vect[j]," -->",appendLF = FALSE)
          }
        }
        
        tvector<-c()
        tvector<-vect
        
      }
    }
    
    
    
  }
  
  ########################
  #  staircase method    #
  ########################
  
  if(method=="staircase")
  {
    mark=2
    
    for(j in 1:(number_of_teams-1))
    {
      for(i in 1:j)
      {  
        message(" | ",i,"-",mark," | ",appendLF = FALSE)
      }
      
      
      message("\n")
      
      mark=mark+1
    }
    
  }
  
  
}
