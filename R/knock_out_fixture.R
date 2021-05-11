###############################################
#   knock_out_fixture function                #
##############################################

knock_out_fixture<-function(number_of_teams)
{
  newenv<-new.env()
  team<-c(1:number_of_teams)
  n<-number_of_teams
  
  stat<-rep(" Play",n)
  
   #############################################################
   #  Assigning upper and lower half of the Teams              #
   #############################################################
  
  if(n%%2==1)
  {  
    up_first<-1
    up_last<-(n+1)/2
    lo_first<-up_last+1
    lo_last<-n
    
    upper_half<-(number_of_teams+1)/2
    lower_half<-(number_of_teams-1)/2
  }else
  {
    up_first<-1
    up_last<-(n/2)
    lo_first<-up_last+1
    lo_last<-n
    
    upper_half<-(number_of_teams)/2
    lower_half<-(number_of_teams)/2
    
  }  
  res<-floor(log(n,2))
  
  no_bye=2**(res+1)-n
  
  
  
  message("\n\n Total number of teams participating: ", number_of_teams)
  message("\n\n Total number of byes: ", no_bye)
  message("\n\n Total number of rounds: ", round(log(number_of_teams,2),0)+1)
  message("\n\n Total number of matches: ", number_of_teams-1)
  message("\n\n Total number of teams in upper half: ", upper_half)
  message("\n\n Total number of teams in lower half: ", lower_half)
  
  
  ############################################
  #  Assigning Bye to the Teams              #
  ############################################
  
  while(no_bye>0)
  {
    if(no_bye>0)
    {  
      stat[lo_last]<-" Bye"
      lo_last<-lo_last-1
      no_bye<-no_bye-1
    }
    if(no_bye>0)
    {    
      stat[up_first]<-" Bye"
      up_first<-up_first+1
      no_bye<-no_bye-1
    }
    if(no_bye>0)
    {
      stat[lo_first]<-" Bye"  
      lo_first<-lo_first+1
      no_bye<-no_bye-1
    }    
    if(no_bye>0)
    {
      stat[up_last]<-" Bye"
      up_last<-up_last-1
      no_bye<-no_bye-1
    }  
    
  }
  
  
  
  round_play<-which(stat==" Play")
  
  
  t_size<-(n-length(round_play)/2)
  tmp<-c(1:t_size)
  assign("match_number",1,envir = newenv)
  assign("round_number",1,envir = newenv)
  winner_number=1
  normal_count=1
  flag=0
  
  for(i in 1:length(team))
  {
    if(flag==1)
    {
      flag=0
      next
    }  
    if(stat[i]==" Bye")
    {
      tmp[normal_count]=paste0(" Team ",i)
      normal_count=normal_count+1
      next
      
    }  
    else
    {
      stat[i]=paste0(" Match ",newenv$match_number)
      stat[i+1]=paste0(" Match ",newenv$match_number)
      tmp[normal_count]=paste0(" Winner of Match ",newenv$match_number)
      normal_count=normal_count+1
      flag=1
      newenv$match_number=newenv$match_number+1
    }  
  }  
  
  message("\n----------------------------------------------------------\n")
  message("             IN BEGINNING ORDER-WISE FOR TEAM 1 TO ",number_of_teams,"\n")
  message("------------------------------------------------------------\n\n")
  
  message(stat)
  
  message("\n-------------------------------------------\n")
  message("             ROUND ",newenv$round_number,"\n")
  message("-------------------------------------------\n\n")
  
  message(tmp)
  
  
  knock_out_print <- function(tmps)
  {
    
    if(length(tmps)==1)
    {
      return(message("\n\n Final Winner is: Winner of match ",newenv$match_number-1))
      
    }
    
    newenv$round_number<<-newenv$round_number+1
    message("\n-------------------------------------------\n")
    message("             ROUND ",newenv$round_number,"\n")
    message("-------------------------------------------\n\n")
    
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
    
    
    return(knock_out_print(rtemp))
  }
  
  knock_out_print(tmp)
  
  
}