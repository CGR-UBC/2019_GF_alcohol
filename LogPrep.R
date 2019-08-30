data<-subset(data,Subject!=0) #removes first zero row
colnames(data)[6] <- "CSLO" #change column name
# remove sixes (runs) and fives (streaks)  from analysis
data_for_analysis<-subset(data,Run.Length<6&Win.Length<6&Loss.Length<6)
# create binary code for wins and losses. Wins coded as 1, losses as 0.
data_for_analysis$previous_feedback=0


for (i in 1:length(data_for_analysis$previous_feedback)) {
  data_for_analysis$previous_feedback[i]<-
    if(data_for_analysis$Win.Length[i]>0)data_for_analysis$previous_feedback[i]=
    1 else data_for_analysis$previous_feedback[i]=0
}
#have to use dummy coding on participatns to use it as a categorical variable (not continuous). 

data_for_analysis$Pairing=factor(data_for_analysis$Pairing)
# Make contrast matrix for categorical variable dummy coding 
contrasts(data_for_analysis$Pairing)=contr.treatment(N) #number of paticipants - will get an eror if wrong


#runlength
data_for_analysis$runlengthbinary=0
for (i in 1:length(data_for_analysis$previous_feedback)) {
  data_for_analysis$runlengthbinary[i]<-
    if(data_for_analysis$Run.Length[i]>2)data_for_analysis$runlengthbinary[i]=
    1 else data_for_analysis$runlengthbinary[i]=0
}

#streak

data_for_analysis$streak<- data_for_analysis$Win.Length+ data_for_analysis$Loss.Length

data_for_analysis$streakbinary=0
for (i in 1:length(data_for_analysis$previous_feedback)) {
  data_for_analysis$streakbinary[i]<-
    if(data_for_analysis$streak[i]>2)data_for_analysis$streakbinary[i]=
    1 else data_for_analysis$streakbinary[i]=0
}