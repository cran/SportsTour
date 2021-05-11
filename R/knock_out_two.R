########################################
#   knock_out_two function             #
########################################
knock_out_two<-function(number_of_teams)
{ 
  
  newenv<-new.env()
  a<-log(number_of_teams,2)
  b<-round(a,0)
  c=a-b
  
  #checking whether the number_of_teams is power of 2
  
  if(c>0)
  {
    message("Enter power of 2 as number of teams or use knock_out_fixture method !!!")
    stop()
  }  
  
  assign("match_number",1,envir = newenv)
  assign("round_number",1,envir = newenv)
  
  
  tnames<-c(1:(number_of_teams))
  for(k in 1:(number_of_teams))
  {
    tnames[k]<-paste0("Team ",k)
    
  }
  
  #########################################################
  #  Recursively computing teams for a particular match   #
  #########################################################
  
  knock_out_two_rec <- function(tmps)
  {
    
    if(length(tmps)==1)
    {
      return(message("\n\n Final Winner is: Winner of match ",newenv$match_number-1))
      
    }
    
    message("\n-------------------------------------------\n")
    message("             ROUND ",newenv$round_number,"\n")
    message("-------------------------------------------\n\n")
    newenv$round_number<-newenv$round_number+1
    mcount=1
    etemp<-c(1:(length(tmps)/2))
    
    
    
    rtemp<-c(1:(length(tmps)/2))
    
    for(j in 1:length(tmps))
    {
      if(j %% 2== 0)
        next
      
      
      etemp[mcount]= paste0("\n Match ",newenv$match_number, " between: ", tmps[j], " and ", tmps[j+1])
      
      rtemp[mcount]=paste0(" Winner of Match ",newenv$match_number)
      
      newenv$match_number<-newenv$match_number+1
      mcount= mcount+1
      
    }
    message(etemp)
    
    
    return(knock_out_two_rec(rtemp))
  }
  knock_out_two_rec(tnames)
  
}
