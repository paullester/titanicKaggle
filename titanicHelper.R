#helper functions



getTitle <- function(Name) {
  Name <- as.character(Name)
  if (length(grep("Miss.", Name)) >0 ){
    return ("Miss.")
  } else if (length(grep("Master.", Name))>0){
    return("Master.")
  } else if (length(grep("Mrs.", Name))>0){
    return("Mrs.")
  } else if (length(grep("Mr.", Name))>0){
    return("Mr.")
  } else {
    return ("Other")
  }
}


##trash code


#convert ticket data to factor
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#first char of ticket?
ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket,1,1))
unique(ticket.first.char)
#make into factor
data.combined$ticket.first.char  <-as.factor(ticket.first.char)

#high level plot
ggplot(data.combined[1:891,], aes(x=ticket.first.char,fill=Survived))+
  geom_bar(width = 0.5) +
  ggtitle("survivability by ticket first char")+
  xlab("ticket first char")+
  ylab("Total Count")+
  labs(fill="Survived")

#pivot on pclass
ggplot(data.combined[1:891,], aes(x=ticket.first.char,fill=Survived))+
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("survivability by ticket first char")+
  xlab("ticket first char")+
  ylab("Total Count")+
  labs(fill="Survived")



#what about fares
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#can't make fare a factor - lets do histogram
ggplot(data.combined[1:891,], aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  labs(fill="Survived")




#check predictive power
ggplot(data.combined[1:891,], aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("survivability by fare")+
  xlab("fare")+
  ylab("Total Count")+
  labs(fill="Survived")


str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined[which(data.combined$Cabin ==""), "Cabin"] <- "U"

data.combined$Cabin[1:100]
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x=cabin.first.char,fill=Survived))+
  geom_bar() +
  ggtitle("survivability by cabin.first.char")+
  xlab("cabin.first.char")+
  ylab("Total Count")+
  labs(fill="Survived")


ggplot(data.combined[1:891,], aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("survivability by fare")+
  xlab("fare")+
  ylab("Total Count")+
  labs(fill="Survived")

data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))





#is fare an indicator?
ggplot(first.mr.df, aes(x=Fare, fill = Survived)) +
  geom_density(alpha=0.5) +
  ggtitle("1st class mr. survival rates")




install.packages("infotheo")

library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])

