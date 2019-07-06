  
  
  userScore=0
  compScore=0
  i=2
  k=2
  
  
  
    # instance variables
    
    deck
    indexScore <- as.numeric(deck[1,3])
    indexScore
    INIT<-function()
    {
      deck<-matrix(ncol=3,nrow=52)
      dimnames(deck)<-list(c(1:52),c("Card Name","Suit","Points or Rank"))
      
      deck[1:13,1]<-paste(c('Ace of','Two of','Three of','Four of','Five of','Six of','Seven of','Eight of','Nine of','Ten of','Jack of','Queen of','King of'),c(' Clubs'))
      deck[14:26,1]<-paste(c('Ace of','Two of','Three of','Four of','Five of','Six of','Seven of','Eight of','Nine of','Ten of','Jack of','Queen of','King of'),c(' Diamonds'))
      deck[27:39,1]<-paste(c('Ace of','Two of','Three of','Four of','Five of','Six of','Seven of','Eight of','Nine of','Ten of','Jack of','Queen of','King of'),c(' Hearts'))
      deck[40:52,1]<-paste(c('Ace of','Two of','Three of','Four of','Five of','Six of','Seven of','Eight of','Nine of','Ten of','Jack of','Queen of','King of'),c(' Spades'))
      
      deck[1:13,2]<- 'C'
      deck[14:26,2]<- 'D'
      deck[27:39,2]<- 'H'
      deck[40:52,2]<- 'S'
      x<-c(11,2:10,10,10,10)
      as.numeric(x)
      deck[,3]<-x
      return (deck)                      # initializes unique value and name to each card in the Playing card tuple
    }
    #------------------------------------SHUFFELING-----------------------------------
    SHUFFELE<-function(deck)
    {
      for(i in 1:52)
      {
        for(j in i:51)
        {
          temp_matx<-matrix(nrow=1,ncol=3)
          temp_num<-sample(j,1)
          #SWAPING
          temp_matx[1,]<-deck[i,]
          deck[i,]<-deck[temp_num[1],]
          deck[temp_num[1],1:3]<-temp_matx[1,]
          #print(deck)
          
        }
        #return (deck)
      }
      deck
    }
    #-------------------------------------DEAL---------------------------------------
    #------------------Providing the card to the Player (topmost card)--------------
    #-------------Deleting the returned card from the deck of Playing Cards---------
    
    DEAL<-function(deck,j)
    {
      print("YOUR CURRENT PLAYING CARD IS : ")
      index<-matrix(nrow=1,ncol=3)
      index[1,1:3]<-deck[j,1:3]
      print(index[,1])
      print("AND ITS VALUE IS :")
      print(index[1,3]) 
      return(index[1,3])
      # deck<-deck[-1,]
      #number_of_cards_left<-nrow(deck)
      #cat("\nNOW, YOU HAVE %i CARDS LEFT.",number_of_cards_left)
      #return 
    }
    #---------------------------------Main Function : TwentyOne---------------------
  
      TwentyOne<-function(DeckOfPlayingCards)               #creates the deck of cards and allows the user to play the game
    {
      repeat{
      #userScore = 0                       #initial user's score
      #cOmpScore = 0                        #initial computer's score
      print("~~~~~~~~~~~~~~~~~~~WELCOME TO GAME 21~~~~~~~~~~~~~~~~~~~~~~~~~")
      #userScore <- 0
      #compScore <- 0
      choice3<-c("HIT","STAY")
      INIT()
      deck
      deck1<-SHUFFELE(deck)
      print("SHUFFELING....")
      deck1
      print("YOUR TURN")
      print("Initially you have two cards. ")
      #for ( i in 1:2)
      
      indexScore<-DEAL(deck1,1)
      
      userScore <-  as.numeric(indexScore)
      print("Your Score :" )
      print(userScore) 
      indexScore<-DEAL(deck1,2)
      
      userScore <- userScore + as.numeric(indexScore)
      print("Your Score :" )
      print(userScore) 
      
      repeat
      {
      if (userScore<21||userScore == 21)
      {
        prompt <- "Please enter 'HIT' or 'STAY' \n "
        choice3<- as.character(strsplit(readline(prompt)," ")[[1]])
        if ( choice3 == 'HIT')
        {
          i<-i+1
          indexScore<-DEAL(deck1,i)
          userScore = userScore + as.numeric(indexScore)
          print("Your Score :" )
          print(userScore)
          if (userScore==21){
            cat("YOU WIN!!!!")
            break
          }
        }else if (choice3 == 'STAY'){
         
        
          print("COMPUTER'S TURN")
          deck2<-SHUFFELE(deck)
          compScore<-DEAL(deck2,1)
          repeat
          {
            if(compScore<17||compScore==17)
            {
              
              indexScore<-DEAL(deck2,k)
              k=k+1
              compScore <- as.numeric(compScore) + as.numeric(indexScore)
              print("COMPUTER'S SCORE :" )
              print(compScore)
              
            }else{
              break
            }
          }
            if (compScore>17){
            print("YOUR SCORE : ")     
            print(userScore)
            print("COMPUTER'S SCORE : ")
            print(compScore)
            if(userScore>compScore)
            {
              print("YOU WIN!!!!")
            }else if (userScore<compScore)
            {
              print("YOU WIN!!!!")
              break
            }
          }
        }
      }else if (userScore>21)
      {
        print("YOU LOSE")
        break
      }else
      {
        print("WRONG CHOICE!!!......GAME TERMINATED")
        break
      }
    }
      prompt <- "Play again?.... ('Y' or 'N') \n "
      choice4<- as.character(strsplit(readline(prompt)," ")[[1]])
      
      if(choice4 == 'N'|| choice4 == 'n'){
      break
      }
      }
  }
