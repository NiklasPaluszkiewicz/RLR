# Mit dieser Datei k?nnen Strategien in R-Studio entwickelt und getestet werden.

# 1. Mit der Funktion "game.1v1" kann ein einzelnes Spiel zwischen zwei Strategien
# gespielt werden. Die Ausgabe erfolgt tabellarisch in der Konsole. Eingestellt
# werden kann:

# Spielparameter: ?ber die Parameter "err.D.prob" und "delta" der Funktion "make.pd.game"
# Strategien: ?ber den Vektor "strat" (es m?ssen genau 2 Strategien sein)

# 2. Mit der Funktion "game.tournament" kann ein Tournier zwischen mehreren Strategien
# gespielt werden. Die Ausgabe erfolgt in einem Webbrowser. Eingestellt werden kann:

# Spielparameter: ?ber die Parameter "err.D.prob" und "delta" der Funktion "make.pd.game"
# Strategien: ?ber den Vektor "strat" (es m?ssen mindestens 2 Strategien sein)
# Anzahl Spielrunden: ?ber den Parameter "R" der Funktion "run.tournament"

# 3. Jede Strategie ist eine Funktion. Unter "STANDARD STRATEGIEN" findet ihr immer
# einige Beispiele. Unter "EURE STRATEGIEN" k?nnt ihr eure eigenen Strategien
# entwickeln.

# 4. Um eure Strategien zu testen m?sst ihr die Funktionen game.1v1() und/oder
# game.tournament anpassen. Normalerweise m?sst ihr dazu nur den Namen eurer
# Strategie (d.h. der Name der Funktion) in die Liste strat einbringen. Klickt danach
# rechts oben auf "Source" und tippt danach die gew?nschte Funktion in die Konsole ein,
# also entweder "game.1v1()" oder "game.tournament()"

# HINWEIS: Beim Ausf?hren von "Source" k?nnen Fehler auftreten. Wenn diese Fehler eure
# Funktionen betreffen m?sst ihr diese korrigieren bevor ihr sie testen k?nnt. Wenn die
# Fehler durch "game.1v1()" oder "game.tournament()" verursacht werden habt ihr eventuell
# einen Fehler bei der Anpassung gemacht. Ladet im Zweifelsfall diese Datei neu herunter
# und kopiert eure Strategien per Copy and Paste in die neu runtergeladene Version rein.

# Treten Fehler aufgrund fehlender Packages auf sollte die Datei "Packages.R" nochmals
# ausgef?hrt werden.

############################ SCHNELLES 1v1 ###############################

game.1v1 = function() {

  library(StratTourn)

  # HIER die Spielparameter eintragen (Noise und Abbruchwahrscheinlichkeit)
  game = make.pd.game(err.D.prob=0, delta=0.9)

  # HIER die beiden Strategien eintragen die gegeneinander Spielen sollen
  # WICHTIG: Es m?ssen genau 2 Strategien eingetragen werden
  strat = nlist(NN.strat,tit.for.tat)

  run.rep.game(game=game, strat = strat)
}

############################ TOURNIERSPIEL ###############################

game.tournament = function() {

  library(StratTourn)

  # HIER die Spielparameter eintragen (Noise und Abbruchwahrscheinlichkeit)
  game = make.pd.game(err.D.prob=0.0, delta=0.95)

  # HIER alle Strategien eintragen die gegeneinander Spielen sollen
  # WICHTIG: Es k?nnen mehr als 2 Strategien eingetragen werden
  # ServerUltimate:
  #strat = nlist(always.coop, strat1, strat2, strat3, strat4, getrich, meineStrat2, phases, stratego)
  # last Tournament
   #strat = nlist(getrich, Alphabet3, stratego, meineStrat2, nashtag1, pudb.strat2, nottitfortat, prof.strat, schachmatt_tournament, phases, false.friend, Globaler.Tit.4.Tat, viva.PD.Strategy, screams.in.space)
  strat=nlist(strat1, NN.strat)


  tourn = init.tournament(game=game, strat=strat)
  set.storing(FALSE)
  tourn = run.tournament(tourn=tourn, R = 100) #HIER Anzahl der Spiele einstellen (mehr -> besser aber langsamer)

  tourn

  show.tournament(tourn)
}

####################### STANDARD STRATEGIEN ###############################

# A strategy that always cooperates
always.coop = function(obs,i,t,...) {
  return(list(a="C"))
}


always.coop.first= function(obs,i,t,...) {
  if(t<=5){
    return(list(a="C"))
  } else {
    return(list(a="D"))
  }

}

# A strategy that always defects
always.defect = function(obs,i,t,...) {
  return(list(a="D"))
}

# A strategy that randomly chooses an action
random.action = function(obs,i,t,...) {

  a = sample( c("C","D"),  1)
  return(list(a=a))
}

# Tit-For-Tat
tit.for.tat = function(obs,i,t,...) {

  # Kooperiere in der ersten Runde
  if (t==1)
    return(list(a="C"))

  # Die Anweisung "3-i" erzeugt den Spielerindex des Gegners
  # Bin ich i=1 ist er j=2
  # Bin ich i=2 ist er j=1
  j = 3-i

  # Spiele die Antwort des Gegners in der letzten Runde
  list(a=obs$a[j])
}
########################## Server Strats ####################################
strat1 = function(obs,i,t,otherC,mylastobserved,...) {

  # Cooperate in the first period
  if (t==1){
    # we set otherC to 1, meaning we give our opponent initial credit
    return(list(a="C",otherC=1,mylastobserved="A"))
  }

  j=3-i

  # Cooperate in the second period
  if (t==2){
    # we remember whether Villain played C and count it
    if(obs$a[[j]]=="C"){
      mylastobserved=obs$a[[i]]
      return(nlist(a="C",otherC=otherC+1,mylastobserved))
    }
    # we remember our last observed action
    mylastobserved=obs$a[[i]]
    return(nlist(a="C",otherC,mylastobserved))
  }


  # in the next eight rounds, do what Villain did the most.
  if(t>2 && t<11){
    # we remember Villains last action
    if(obs$a[[j]]=="C"){
      otherC= otherC + 1
    }
    # we remember our last action
    mylastobserved=obs$a[[i]]
    # if we mostly observed "D", we play "D".
    if(otherC<0.5*t){
      return(nlist(a="D",otherC,mylastobserved))
    }
    # else we play "C"
    return(nlist(a="C",otherC,mylastobserved))
  }

  # after ten rounds, we make a case-by-case-analysis
  if(t>=11){
    # we remember Villains last action
    if(obs$a[[j]]=="C"){
      otherC= otherC + 1
    }
    # we remember our last action
    mylastobserved=obs$a[[i]]
    # if less than half of our observations are "C", we play "D"
    if(otherC<0.5*t){
      return(nlist(a="D",otherC,mylastobserved))
    }
    # if Villain was nice, we play "C" in most cases but try to to play "D" some of the time too
    if(otherC>=0.5*t && otherC<=0.75*t){
      # if my last observed action was "D", we cooperate
      if(mylastobserved=="D"){
        return(nlist(a="C",otherC,mylastobserved))
      }
      # else we cooperate with 85% probability
      if(mylastobserved=="C"){
        do.cooperate=(runif(1)<0.85)
        if(do.cooperate){
          return(nlist(a="C",otherC,mylastobserved))
        }
        else{
          return(nlist(a="D",otherC,mylastobserved))
        }
      }
    }
    # If Villain is too nice, we try to exploit him by deflecting with a high probability
    if(otherC>0.75*t){
      # if my last observed action was "D", we cooperate
      if(mylastobserved=="D"){
        return(nlist(a="C",otherC,mylastobserved))
      }
      # else we cooperate with 67% probability
      if(mylastobserved=="C"){
        do.cooperate=(runif(1)<0.67)
        if(do.cooperate){
          return(nlist(a="C",otherC,mylastobserved))
        }
        else{
          return(nlist(a="D",otherC,mylastobserved))
        }
      }
    }
  }

}

strat3 = function(obs,i,t,counter=0,standi="G",obsl=c("Start"),obslown=c("Start"),ansl=c("Start"),...) {

  # Set parameters
  er=0.25;
  a=1;
  # other player
  j = 3-i

  # Cooperate in the first period, then decide among the answers what strategy to play  for the next 5 rounds  (depending on the expectet length and observation error of the game)

  # Save observations of every round
  if (t>=1){
    obsl[t]=obs$a[j];
    obslown[t]=obs$a[i];
  }
  # Opening strategy
  if (t==1){
    ansl[t+1]="C";
    return(list(a="C",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
  }

  if (t>=2){

    if (counter>=2){
      counter=counter-1;
    }
    if (counter==1){
      counter=counter-1;
      ansl[t+1]="D";
      return(list(a="D",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
    }
    if (standi=="S"){
      a=sum(ifelse(obsl=="D",1,0))/(length(obsl)-1);
      if (a<=0.4*(1+(2/t))){
        standi="G1"
      }else{
        standi="S";
      }
    }
    if (standi=="G1"){
      if(a<=0.4*(1+(2/t))){
        standi ="G1";
      }else{
        if(obsl[t]=="D"){
          standi="S";
        }else{
          standi="G"
        }
      }
    }
    if (standi=="G"){
      if(obsl[t]=="C"|| (obsl[t]=="D" & obslown[t-1]=="D") || (obsl[t]=="D" & obslown[t]=="D") ){
        standi="G";
      }else{
        standi="G1";
      }
    }

  }

  # The returns for the different states
  if (standi=="G"){
    ansl[t+1]="C";
    return(list(a="C",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
  }
  if (standi=="G1"){

    r=runif(1)
    if(r<(1-(er/2))){
      ansl[t+1]="C";
      return(list(a="C",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
    }
    ansl[t+1]="D";
    return(list(a="D",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
  }
  if (standi=="S"){
    if (counter==0){
      counter=8;
    }
    ansl[t+1]="D";
    return(list(a="D",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))
  }



  # never return nothing (to prevent failure)
  ansl[t]="C";
  return(list(a="C",counter=counter,standi=standi,obsl=obsl,obslown=obslown,ansl=ansl))

}

strat2 = function(obs,i,t,nr.df=0,last.his=NULL,count=0,...){

  j = 3-i

  # Cooperate in the first period
  if (t==1){
    return(list(a="C", nr.df=nr.df,last.his=last.his,count=count))
  }

  last.his[1]=last.his[2] # store the previous last action from other player in last.his[1]
  last.his[2]=obs$a[j] # store the last action from other player in last.his[2]

  # reset the counter and defect number from other player
  if(count==0){
    nr.df<-0
  }

  # rise the counter of receiving "defect"
  if(obs$a[j]=="D")
    nr.df=nr.df +1

  count=count+1

  # reset the counter every 6 steps
  if(count>=7){
    count<-0
  }

  # because of noise we try to be nice
  if(nr.df<=1){ # 0,1
    return(list(a="C",nr.df=nr.df, last.his=last.his,count=count))
  }

  # play the last move from other player
  if (nr.df==2) # 2
    return(list(a=obs$a[j],nr.df=nr.df,last.his=last.his,count=count))

  # predicting "always.defect" type from other player, so we defect
  if (nr.df==6){ # 6
    return(list(a="D",nr.df=nr.df,last.his=last.his,count=count))
  }

  # only defect if we receive 2 times in a row defect, otherwise cooperate
  if (nr.df>=3){ #  3,4,5
    if(last.his[1]=="D" && obs$a[j]=="D"){
      return(list(a="D",nr.df=nr.df, last.his=last.his,count=count))
    }
    else return(list(a="C",nr.df=nr.df, last.his=last.his,count=count))
  }
}

strat4 = function(obs,i,t,itsyou=0,still.defect=3,still.cooperate=3,haveto.cooperate=0,haveto.defect=3,endto.cooperate=3,knowwho=0,...){
  debug.store("strat4",i,t)       # Store each call for each player
  debug.restore("strat4",i=1,t=7) # Restore call for player i in period t

  j = 3-i
  b = obs$a[j]

  if(t==1){
    return(list(a="C",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }

  if(itsyou==1){
    if(b=="D"){
      still.defect=still.defect-1
      if(b=="C"){
        still.defect=still.defect+1
        if(still.defect==4){
          still.defect=still.defect-1
        }
      }

      if(still.defect==0){
        itsyou=0
        return(list(a="D",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
      }
    }
    return(list(a="C",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }
  if(knowwho==3){
    itsyou=1
    return(list(a="C",itsyou=itsyou,still.defect=3,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }

  if(b=="C"){
    knowwho=knowwho+1
  }

  if(b=="D"){
    knowwho=0
  }


  if(endto.cooperate==0){
    return(list(a="D",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=0,knowwho=knowwho))
  }
  if(still.defect==0){
    if(haveto.defect==0){
      endto.cooperate=endto.cooperate-1

      return(list(a="C",itsyou=itsyou,still.defect=3,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
    }
    if(b=="C"){
      haveto.cooperate=haveto.cooperate+1
    }
    if(haveto.cooperate==2){
      do.cooperate=(runif(1)<0.85)
      if(do.cooperate){
        endto.cooperate=endto.cooperate-1
        return(list(a="C",itsyou=itsyou,still.defect=3,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
      } else {
        return(list(a="D",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
      }
    }
    haveto.defect=haveto.defect-1
    return(list(a="D",itsyou=itsyou,still.defect=0,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }

  if(still.cooperate==0){
    if(still.defect<2){
      still.defect=still.defect+1
    }
    return(list(a="C",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }

  if(b=="C"){
    still.cooperate=still.cooperate-1
    if(still.defect<2){
      still.defect=still.defect+1
    }
    return(list(a="C",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
  }

  if(b=="D"){
    still.defect=still.defect-1
    if(still.cooperate<2){
      still.cooperate=still.cooperate+1
    }
    do.defect=(runif(1)<0.85)
    if(do.defect){
      return(list(a="D",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
    } else{
      return(list(a="C",itsyou=itsyou,still.defect=still.defect,still.cooperate=still.cooperate,haveto.cooperate=haveto.cooperate,haveto.defect=haveto.defect,endto.cooperate=endto.cooperate,knowwho=knowwho))
    }
  }
}

getrich = function(obs,i,t,trueself=c(), obsself=c(),DforC=0,...) {



  #ERSTE RUNDE
  if (t==1){
    trueself[t]=1
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  j= 3-i


  #Beobachtung des anderen merken
  if(obs$a[i]=="C"){
    obsself[t-1]=1
  }
  else
    obsself[t-1]=0



  #Der andere hat KOOPERIERT, wir kooperieren grunds?tzlich auch
  if (obs$a[j]=="C") {
    trueself[t]=1
    DforC=0
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  #DforC zu hoch
  if (DforC>3){
    trueself[t]=0
    return(list(a="D",trueself=trueself,obsself=obsself,DforC=DforC))
  }


  #Der andere hat DEFLEKTIERT

  #Beobachtung: Wir hatten zuvor kooperiert
  if (t==2){
    trueself[t]=0
    return (list(a="D",trueself=trueself,obsself=obsself,DforC=DforC))
  }
  if (obsself[t-2]==1){ #Weniger Vertrauen in Mitspieler, deflektieren
    DforC=DforC+1
    trueself[t]=0
    return(list(a="D", trueself=trueself,obsself=obsself,DforC=DforC))
  }
  #Beobachtung:Wir hatten zuvor deflektiert

  if(trueself[t-2]==0){  #Tats?chlich deflektiert, Gegener auch deflektiert
    x=runif(1)


    if (x<0.9-DforC*0.2){ #Je nach Kooperationsstatus des Gegners ?fter wieder kooperieren
      trueself[t]=1
      return(list(a="C", trueself=trueself,obsself=obsself,DforC=DforC))
    }
    else{
      trueself[t]=0
      return(list(a="D", trueself=trueself,obsself=obsself,DforC=DforC))
    }
  }

  if (trueself[t-2]==1){  #Wir hatten eigentlich kooperiert, wurde aber falsch wahrgenommen: kooperieren
    trueself[t]=1
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  #SICHERHEITSOPTION
  trueself[t]= 1
  return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))

}

meineStrat2 = function(obs,i,t, BeoGegner = 0, BeoSelbst = 0, HandlSelbst = 0 , FehlBeo = FALSE, ... ){

  # wie viele Runden sind wir "gutm?tig"?
  gutm = 8

  # j ist Spielerindex des Gegners
  j = 3-i

  #### in Runde 1 kooperieren wir immer
  if (t==1){
    HandlSelbst[t] = "C"
    FehlBeo[t] = FALSE
    return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo =FehlBeo))
  }

  ### Beginne ab Runde 2 die Beobachtungen zu dokumentieren und einige Variablen zu berechnen

  # BeoGegner ist die beobachtete Handlung aus Runde t-1 des Gegners
  # BeoSelbst ist die Beobachtung (des Gegners) der eigenen Handlung
  BeoGegner[t-1] = obs$a[j]
  BeoSelbst[t-1]=obs$a[i]

  # wie oft konnte ich eine Kooperation des Gegners beobachten?
  KoopBeo = length(BeoGegner[BeoGegner =="C"])

  # versuche die tats?chliche Anzahl der Kooperationen zu berechnen, indem die error.prob herausgerechnet wird
  # KoopTat soll die tats?chliche Zahl der Kooperationen des Gegners sein (Approximation, ist bei kleiner Rundenzahl mit Fehler behaftet)
  KoopTat = min((t-1), KoopBeo/0.75)

  # wir befinden uns in Runde [t], nimmt der Gegner in dieser Runde meine Kooperation aus der Vorrunde,
  # also aus [t-1] falsch wahr? T = ja, F = nein
  FehlBeo[t] = (BeoSelbst[t-1] != HandlSelbst[t-1])

  # in wie viel Prozent der Runden habe ich eine Defektion beobachtet?
  AntDefekBeo = (length(BeoGegner[BeoGegner=="D"])/ (t-1))

  # in wie viel Prozent der Runden hat Gegner tats?chlich defektiert? (Versuche  Beobachtungsfehler herauszurechnen)
  AntDefekTats = max(0, (AntDefekBeo - (KoopTat*0.25/(t-1))))

  # wie oft wurde von mir eine Defektion (durch den Gegner) beobachtet?
  DefSelbst = length(BeoSelbst[BeoSelbst=="D"])

  # in wie viel Prozent der Runden wurde von mir eine Defektion beobachtet?
  AntDefSelbst = DefSelbst/(t-1)

  # denken wir, dass der Gegner uns NICHT ausnutzt?
  # ja, wenn wir selbst ?fter defektieren als er, ja = TRUE
  GegnerNett = (AntDefSelbst*1.25 >= AntDefekTats)

  # in Runde 2- gutm
  if (t <= gutm){

    # wenn Gegner Fehlbeobachtung hatte und deswegen defektiert, kooperiere trotzdem
    if (FehlBeo[t-1] & BeoGegner[t-1]== "D"){
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo ))
    }

    # in diesen F?llen, hatten wir wahrscheinlich eine Fehlbeobachtungen und kooperieren deswegen
    else if ( t>2){
      if(BeoGegner[t-1] == "D" & BeoGegner[t-2] != "D"){
        HandlSelbst[t] = "C"
        return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo ))
      }
    }

    # sonst: handle wie Gegner in Runde zuvor
    else{
      HandlSelbst[t] = BeoGegner[t-1]
      return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
    }
  }

  ### ab jetzt (t > gutm) verwenden wir die erhobenen Daten und die berechneten Variablen um Gegner einzusch?tzen

  # in dieser Runde wollen wir Gegner "ausnutzten"
  else if(BeoSelbst[t-1] == "C" & BeoSelbst[t-2]== "C"  & BeoGegner[t-1]== "D" ){
    HandlSelbst[t] = "D"
    return(list(a=  "D", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
  }

  else{
    # wenn Gegner kooperiert, kooperiere
    if(BeoGegner[t-1] == "C"){
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo= FehlBeo))
    }

    # Gegner hatte Fehlbeobachtung, kooperiere trotz seiner Defektion, wenn er nett ist
    else if(FehlBeo[t-1] & GegnerNett) {
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo= FehlBeo))
    }

    # wenn Gegner keine Fehlbeobachtung hatte (und trotzdem defektiert hat)
    else{

      # wenn wir Gegner als nett einsch?tzen und er in Runde [t-2] kooperiert hat, dann kooperiere
      # wir haben seine Defektion wahrscheinlich als falsch wahgenommen
      if( GegnerNett & BeoGegner[t-2] != "D"){
        HandlSelbst[t] = "C"
        return(list(a= "C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
      }

      # wenn wir Gegner als NICHT nett einsch?tzen, nehmen wir seine Defektion wahrscheinlich nicht falsch wahr
      # handle wie er in der Vorrunde
      else if(!GegnerNett){

        # Ausnahme: wenn Gegnger in vorherigen beiden Runden kooperiert hat
        if(BeoGegner[t-2] == "C"& BeoGegner[t-3] == "C"){
          HandlSelbst[t] = "C"
          return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
        }
        else{
          HandlSelbst[t] = "D"
          return(list(a="D", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
        }
      }

      # sonst: handle wie Gegner in Vorrunde
      else{
        HandlSelbst[t] = BeoGegner[t-1]
        return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
      }
    }
  }
  # zur Sicherheit
  HandlSelbst[t] = BeoGegner[t-1]
  return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))

}

phases = function(obs,i,t,equal=TRUE,offender=FALSE,victim=FALSE,...){


  #j beschreibt Index des Gegners
  j=3-i


  # Kooperiere in der ersten Runde
  if(t==1)
    return(list(a="C",offender=offender,equal=equal,victim=victim))

  # Phase equal:
  if(equal){

    #Wir kooperieren wenn unsere beobachteten Handlungen ?bereinstimmen und bleiben in Phase equal.
    if(obs$a[[j]]==obs$a[[i]]){
      return(list(a="C",offender=offender,equal=equal,victim=victim))
    }

    #Spielt der Gegner eine einseitige Defektion, antworten wir mit einer Defektion und wechseln in Phase victim.
    else if(obs$a[[j]]=="D"){
      return(list(a="D",offender=FALSE,equal=FALSE,victim=TRUE))
    }

    #Spielen wir die einseitige Defektion, antworten wir mit einer Kooperation und welchseln in den Status offender.
    else if (obs$a[[i]]=="D"){
      return(list(a="C",victim=FALSE,equal=FALSE,offender=TRUE))
    }
  }

  #Phase victim: Wir defektieren solange bis unser Gegner Koopertiert.
  if(victim){

    # Defektiert der Gegner, defektieren wir zur?ck und bleiben in Phase Victim
    if(obs$a[[j]]=="D"){
      return(list(a="D",offender=offender,equal=equal,victim=victim))
    }

    # Koopertiert der Gegner, kooperieren wir zur?ck und wechseln in Phase equal.
    else{
      return(list(a="C",offender=FALSE,victim=FALSE,equal=TRUE))
    }
  }

  #Phase offender: Wir bleiben solange in dieser Phase und kooperieren, bis eine erfolgreiche Kooperation entsteht.
  if(offender){

    # ?nderung der Phase bei einvernehmlicher Kooperation, wechsel in Phase equal
    if(obs$a[[j]]=="C" && obs$a[[i]]=="C"){
      return(list(a="C",victim=FALSE,offender=FALSE,equal=TRUE))
    }
    # Kooperieren und in Phase bleiben
    else{
      return(list(a="C",offender=offender,equal=equal,victim=victim))
    }
  }

  #nicht notwendig
  else{
    return(list(a="C",offender=offender,equal=equal,victim=victim))
  }
}

stratego = function(obs,i,t,memoryI=0,memoryJ=0,...) {

  # j beschreibt Index des Gegners
  j=3-i

  # ab Runde 2 wird ein Ged?chtnis gebildet, wobei:
  # memoryI=Ged?chtnis mit meinen Spielz?gen
  # memoryJ=Ged?chtnis mit den Spielz?gen des Gegners
  if (t>1)
    memoryI[t-1]=obs$a[i]
  memoryJ[t-1]=obs$a[j]

  # Kooperiere in den ersten f?nf Runden
  if (t<=5) {
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }

  # W?hle in den Runden 6 bis 8 jeweils einen zuf?lligen Spielzug
  if (t>5 && t<=8) {
    a=sample(c("C","D"),1)
    return(list(a=a,memoryI=memoryI,memoryJ=memoryJ))
  }

  # Erzeuge zwei Kurzzeitged?chtnisse der letzten 4 bzw. 8 Runden
  shortmemory=memoryJ[(length(memoryJ)-3):length(memoryJ)]
  longmemory=memoryJ[(length(memoryJ)-7):length(memoryJ)]

  # Defektiere, wenn der Gegner in den letzten 8 Runden ebenfalls nur defektiert hat
  if(length(which(longmemory=="D"))==8) {
    return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
  }

  # Spiele abh?ngig vom Gegnerverhalten in den letzten 8 Runden
  if(length(which(longmemory=="C"))==4 && length(which(shortmemory=="C"))==2) {
    a=sample(c("C","D"),1)
    return(list(a=a,memoryI=memoryI,memoryJ=memoryJ)) # W?hle einen zuf?lligen Spielzug,
    # wenn er sowohl in den letzten 8 als auch in den letzten 4 Runden jeweils gleich oft kooperiert und defektiert hat
  }
  else if(length(which(longmemory=="C"))>=6 && length(which(shortmemory=="C"))==4) {
    return(list(a="D",memoryI=memoryI,memoryJ=memoryJ)) # Defektiere, wenn er in den letzten 4 Runden nur kooperiert hat
    # und gleichzeitig in den letzten 8 Runden mindestens 6 mal ebenfalls kooperiert hat
  }
  else {
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ)) # Kooperiere in allen anderen F?llen
  }
}

getrich = function(obs,i,t,trueself=c(), obsself=c(),DforC=0,...) {



  #ERSTE RUNDE
  if (t==1){
    trueself[t]=1
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  j= 3-i


  #Beobachtung des anderen merken
  if(obs$a[i]=="C"){
    obsself[t-1]=1
  }
  else
    obsself[t-1]=0



  #Der andere hat KOOPERIERT, wir kooperieren grunds?tzlich auch
  if (obs$a[j]=="C") {
    trueself[t]=1
    DforC=0
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  #DforC zu hoch
  if (DforC>3){
    trueself[t]=0
    return(list(a="D",trueself=trueself,obsself=obsself,DforC=DforC))
  }


  #Der andere hat DEFLEKTIERT

  #Beobachtung: Wir hatten zuvor kooperiert
  if (t==2){
    trueself[t]=0
    return (list(a="D",trueself=trueself,obsself=obsself,DforC=DforC))
  }
  if (obsself[t-2]==1){ #Weniger Vertrauen in Mitspieler, deflektieren
    DforC=DforC+1
    trueself[t]=0
    return(list(a="D", trueself=trueself,obsself=obsself,DforC=DforC))
  }
  #Beobachtung:Wir hatten zuvor deflektiert

  if(trueself[t-2]==0){  #Tats?chlich deflektiert, Gegener auch deflektiert
    x=runif(1)


    if (x<0.9-DforC*0.2){ #Je nach Kooperationsstatus des Gegners ?fter wieder kooperieren
      trueself[t]=1
      return(list(a="C", trueself=trueself,obsself=obsself,DforC=DforC))
    }
    else{
      trueself[t]=0
      return(list(a="D", trueself=trueself,obsself=obsself,DforC=DforC))
    }
  }

  if (trueself[t-2]==1){  #Wir hatten eigentlich kooperiert, wurde aber falsch wahrgenommen: kooperieren
    trueself[t]=1
    return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))
  }

  #SICHERHEITSOPTION
  trueself[t]= 1
  return(list(a="C",trueself=trueself,obsself=obsself,DforC=DforC))

}

Alphabet3 = function(obs,i,t,memoryI=0, memoryJ=0,...) {

  library(StratTourn)

  #Der Gegner bekommt den Index j.
  j = 3-i

  # Ab Runde 2 wird das Ged?chtnis gebildet. In memoryI werden die eigenen Handlungen der vorherigen Runde gespeichert. In memoryJ die Handlungen der vorherigen Runde des Gegners.

  if (t>1){
    memoryI[t-1]=obs$a[i]
    memoryJ[t-1]=obs$a[j]
  }

  # Kooperiere in den ersten f?nf Runden.
  if (t<=5){
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))

  }

  # Erzeuge ein Kurzzeitged?chtnis der letzten 5 Runden, in welchem gespeichert      wird, was der Gegner von einem selber beobachtet.
  shortmemoryI=memoryI[(length(memoryI)-4):length(memoryI)]

  if (t==6){
    # Spiele abh?ngig von der Wahrnehmung des Gegners in den letzten f?nf Runden.       Falls der Gegner mehr als eine Defektion beobachtet hat
    # kooperiere, sonst defektiere.
    if(length(which(shortmemoryI=="C"))>=4){
      return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
    }
    else{
      return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
    }
  }
  # Erzeuge ein Kurzzeitged?chtnis der letzten sechs Runden f?r die Z?ge des         Gegners.
  shortmemoryJ=memoryJ[(length(memoryJ)-5):length(memoryJ)]

  if (t>=7){
    if (t%%6>0){
      # In den folgenden 5 Runden spiele jeweils abh?ngig von der Handlung des           Gegners in den letzten sechs Runden.
      # Falls er mehr als f?nf mal defektiert hat, defektiere ebenfalls. Sonst           kooperiere.
      if(length(which(shortmemoryJ=="D"))>=5){
        return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      else{
        return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
      }
    }
    # Defektiere in jeder 6.Runde, au?er der Gegner nimmt jemanden zu aggressiv        wahr oder der Gegner ist zu aggressiv.
    else {
      if(length(which(shortmemoryJ=="D"))>=5){
        return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      else if (length(which(shortmemoryI=="C"))>=4){
        return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      else{
        return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
      }

    }

  }




}

stratego = function(obs,i,t,memoryI=0,memoryJ=0,...) {

  # j beschreibt Index des Gegners
  j=3-i

  # ab Runde 2 wird ein Ged?chtnis gebildet, wobei:
  # memoryI=Ged?chtnis mit meinen Spielz?gen
  # memoryJ=Ged?chtnis mit den Spielz?gen des Gegners
  if (t>1)
    memoryI[t-1]=obs$a[i]
  memoryJ[t-1]=obs$a[j]

  # Kooperiere in den ersten f?nf Runden
  if (t<=5) {
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }

  # W?hle in den Runden 6 bis 8 jeweils einen zuf?lligen Spielzug
  if (t>5 && t<=8) {
    a=sample(c("C","D"),1)
    return(list(a=a,memoryI=memoryI,memoryJ=memoryJ))
  }

  # Erzeuge zwei Kurzzeitged?chtnisse der letzten 4 bzw. 8 Runden
  shortmemory=memoryJ[(length(memoryJ)-3):length(memoryJ)]
  longmemory=memoryJ[(length(memoryJ)-7):length(memoryJ)]

  # Defektiere, wenn der Gegner in den letzten 8 Runden ebenfalls nur defektiert hat
  if(length(which(longmemory=="D"))==8) {
    return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
  }

  # Spiele abh?ngig vom Gegnerverhalten in den letzten 8 Runden
  if(length(which(longmemory=="C"))==4 && length(which(shortmemory=="C"))==2) {
    a=sample(c("C","D"),1)
    return(list(a=a,memoryI=memoryI,memoryJ=memoryJ)) # W?hle einen zuf?lligen Spielzug,
    # wenn er sowohl in den letzten 8 als auch in den letzten 4 Runden jeweils gleich oft kooperiert und defektiert hat
  }
  else if(length(which(longmemory=="C"))>=6 && length(which(shortmemory=="C"))==4) {
    return(list(a="D",memoryI=memoryI,memoryJ=memoryJ)) # Defektiere, wenn er in den letzten 4 Runden nur kooperiert hat
    # und gleichzeitig in den letzten 8 Runden mindestens 6 mal ebenfalls kooperiert hat
  }
  else {
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ)) # Kooperiere in allen anderen F?llen
  }
}

meineStrat2 = function(obs,i,t, BeoGegner = 0, BeoSelbst = 0, HandlSelbst = 0 , FehlBeo = FALSE, ... ){

  # wie viele Runden sind wir "gutmütig"?
  gutm = 8

  # j ist Spielerindex des Gegners
  j = 3-i

  #### in Runde 1 kooperieren wir immer
  if (t==1){
    HandlSelbst[t] = "C"
    FehlBeo[t] = FALSE
    return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo =FehlBeo))
  }

  ### Beginne ab Runde 2 die Beobachtungen zu dokumentieren und einige Variablen zu berechnen

  # BeoGegner ist die beobachtete Handlung aus Runde t-1 des Gegners
  # BeoSelbst ist die Beobachtung (des Gegners) der eigenen Handlung
  BeoGegner[t-1] = obs$a[j]
  BeoSelbst[t-1]=obs$a[i]

  # wie oft konnte ich eine Kooperation des Gegners beobachten?
  KoopBeo = length(BeoGegner[BeoGegner =="C"])

  # versuche die tatsächliche Anzahl der Kooperationen zu berechnen, indem die error.prob herausgerechnet wird
  # KoopTat soll die tatsächliche Zahl der Kooperationen des Gegners sein (Approximation, ist bei kleiner Rundenzahl mit Fehler behaftet)
  KoopTat = min((t-1), KoopBeo/0.75)

  # wir befinden uns in Runde [t], nimmt der Gegner in dieser Runde meine Kooperation aus der Vorrunde,
  # also aus [t-1] falsch wahr? T = ja, F = nein
  FehlBeo[t] = (BeoSelbst[t-1] != HandlSelbst[t-1])

  # in wie viel Prozent der Runden habe ich eine Defektion beobachtet?
  AntDefekBeo = (length(BeoGegner[BeoGegner=="D"])/ (t-1))

  # in wie viel Prozent der Runden hat Gegner tatsächlich defektiert? (Versuche  Beobachtungsfehler herauszurechnen)
  AntDefekTats = max(0, (AntDefekBeo - (KoopTat*0.25/(t-1))))

  # wie oft wurde von mir eine Defektion (durch den Gegner) beobachtet?
  DefSelbst = length(BeoSelbst[BeoSelbst=="D"])

  # in wie viel Prozent der Runden wurde von mir eine Defektion beobachtet?
  AntDefSelbst = DefSelbst/(t-1)

  # denken wir, dass der Gegner uns NICHT ausnutzt?
  # ja, wenn wir selbst öfter defektieren als er, ja = TRUE
  GegnerNett = (AntDefSelbst*1.25 >= AntDefekTats)

  # in Runde 2- gutm
  if (t <= gutm){

    # wenn Gegner Fehlbeobachtung hatte und deswegen defektiert, kooperiere trotzdem
    if (FehlBeo[t-1] & BeoGegner[t-1]== "D"){
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo ))
    }

    # in diesen Fällen, hatten wir wahrscheinlich eine Fehlbeobachtungen und kooperieren deswegen
    else if ( t>2){
      if(BeoGegner[t-1] == "D" & BeoGegner[t-2] != "D"){
        HandlSelbst[t] = "C"
        return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo ))
      }
    }

    # sonst: handle wie Gegner in Runde zuvor
    else{
      HandlSelbst[t] = BeoGegner[t-1]
      return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
    }
  }

  ### ab jetzt (t > gutm) verwenden wir die erhobenen Daten und die berechneten Variablen um Gegner einzuschätzen

  # in dieser Runde wollen wir Gegner "ausnutzten"
  else if(BeoSelbst[t-1] == "C" & BeoSelbst[t-2]== "C"  & BeoGegner[t-1]== "D" ){
    HandlSelbst[t] = "D"
    return(list(a=  "D", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
  }

  else{
    # wenn Gegner kooperiert, kooperiere
    if(BeoGegner[t-1] == "C"){
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo= FehlBeo))
    }

    # Gegner hatte Fehlbeobachtung, kooperiere trotz seiner Defektion, wenn er nett ist
    else if(FehlBeo[t-1] & GegnerNett) {
      HandlSelbst[t] = "C"
      return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo= FehlBeo))
    }

    # wenn Gegner keine Fehlbeobachtung hatte (und trotzdem defektiert hat)
    else{

      # wenn wir Gegner als nett einschätzen und er in Runde [t-2] kooperiert hat, dann kooperiere
      # wir haben seine Defektion wahrscheinlich als falsch wahgenommen
      if( GegnerNett & BeoGegner[t-2] != "D"){
        HandlSelbst[t] = "C"
        return(list(a= "C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
      }

      # wenn wir Gegner als NICHT nett einschätzen, nehmen wir seine Defektion wahrscheinlich nicht falsch wahr
      # handle wie er in der Vorrunde
      else if(!GegnerNett){

        # Ausnahme: wenn Gegnger in vorherigen beiden Runden kooperiert hat
        if(BeoGegner[t-2] == "C"& BeoGegner[t-3] == "C"){
          HandlSelbst[t] = "C"
          return(list(a="C", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
        }
        else{
          HandlSelbst[t] = "D"
          return(list(a="D", BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
        }
      }

      # sonst: handle wie Gegner in Vorrunde
      else{
        HandlSelbst[t] = BeoGegner[t-1]
        return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))
      }
    }
  }
  # zur Sicherheit
  HandlSelbst[t] = BeoGegner[t-1]
  return(list(a= BeoGegner[t-1], BeoGegner = BeoGegner, BeoSelbst = BeoSelbst, HandlSelbst = HandlSelbst, FehlBeo = FehlBeo))

}

squishy.the.octopus = function(obs,i,t,preObs=c("C","C"),ans="C",preAns="C",defectTotal=0,defectConsecutive=0,stopLoss=FALSE,...) {

  # Variable Definitions
  answer="C"
  j = 3-i

  # Constants that control the stopLoss system
  consecutiveStopLossBarrier=3
  if(t>10){
    consecutiveStopLossBarrier=4
  }
  relativeStopLossBarrier=0.25+0.05+max(0,(18-t)*0.02)

  # debug.store("tit.for.tat",i,t) # Store each call for each player
  # debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t

  # Find out if other player punished me because of an observation error
  obserr=FALSE;
  if(preObs[1]=="D" & obs$a[j]=="D" & preAns=="C"){
    obserr=TRUE;
  }

  # Update extra variables
  preObs=c(obs$a[i],obs$a[j])
  preAns=ans

  # Cooperate in the first period
  if (t==1)answer="C"

  # Alway cooperate in the early game
  if(! stopLoss){

    # Register defections that are NOT caused by observation error
    if(obs$a[j]=="D" & (!obserr)){
      defectTotal=defectTotal+1
      defectConsecutive=defectConsecutive+1
    }
    else{
      defectConsecutive=0
    }

    # Activate the StopLoss after x unprovoked defections in a row
    if(defectConsecutive>consecutiveStopLossBarrier){
      stopLoss=TRUE
    }

    # Activate the stop loss if the unprovoked defection quota exceeds the error probability significantly
    if(t>=5 & ((defectTotal/t)>relativeStopLossBarrier)){
      stopLoss=TRUE
    }

    # Play always cooperate until the game ends or the stop loss is triggered
    if(! stopLoss){
      return(list(a="C",preObs=preObs,preAns=preAns,ans="C",defectTotal=defectTotal,defectConsecutive=defectConsecutive,
                  stopLoss=stopLoss))
    }
  }

  # Play noise-safe tit-for-tat after the stop loss barrier was triggered:
  # Be forgiving if a previous defection was caused by an observation error.
  # Otherwise play tit-for-tat
  if (obs$a[j]=="C" | obserr){
    answer="C"
  }
  else{
    answer="D"
  }


  return(list(a=answer,preObs=preObs,preAns=preAns,ans=answer,defectTotal=defectTotal,defectConsecutive=defectConsecutive,
              stopLoss=stopLoss))
}

nashtag1 = function(obs,i,t,state=1,z=0,cop=4,wdh=0,memoryJ=0,...){
  j = 3-i

  #memory aufbauen ab 2. Runde
  if(t>1){memoryJ[t-1]=obs$a[j]}

  # Erzeuge ein Kurzzeitgedächtnis der letzten 30 Runden (ab Runde 30)
  if(t>=30){shortmem=memoryJ[(length(memoryJ)-29):length(memoryJ)]}

  #########################################
  ##State=1: Kooperiere bis Gegener in letzten 30 Fällen 13 mal defektiert hat (Bedingung 1) oder 3 mal in Folge
  # defektiert hat (Bedingung 2). Falls eine Wechselbedingung erfüllt, dann in State 2 wechseln.
  if (state==1){

    #Zähler z für aufeinander folgende Defektierungen des Gegeners
    if(obs$a[j]=="D"){z=z+1} #bei Defektion +1
    else{z=0}                #sonst 0 setzen

    #Bedingungen zum Wechseln in State 2 prüfen
    if (z==3){ #Bedingung 2 prüfen
      state=2
      z=0
    }
    if(t>=30){ #ab Runde 30
      if(length(which(shortmem=="D"))>12){ #Bedingung 1 prüfen
        state=2
        z=0
      }
    }

    #falls State immer noch =1 (keine State-Wechselbedingung erfüllt wurde) => Kooperiere
    if(state==1){
      return(list(a="C",state=state,z=z,cop=cop,wdh=wdh,memoryJ=memoryJ))
    }
  }
  ###################################################################################################################
  ##State=2: "Tit for Tat". Bis Gegner 5 mal in Folge Defektiert hat (Bedingung), dann in State 3 wechseln.
  #########################
  if(state==2){

    #Zähler für aufeinanderfolgende Defektierungen des Gegeners
    if(obs$a[j]=="D"){z=z+1}
    else{z=0}

    #Bedingung zum Wechseln in State 3 prüfen
    if(z==5){
      state=3
      z=0
    }
    #Falls keine Wechselbedingung erfüllt, Tit for Tat Spielen
    else{return(list(a=obs$a[j],state=state,z=z,cop=cop,wdh=wdh,memoryJ=memoryJ))}
  }

  ##################################################################################################################
  ##State=3: "Kooperation": 5 Runden. 3 mal Kooperieren. 1 mal Defektieren. Dannach testen, ob Geg. in den letzten
  ######################### 3 Rd. min. 1 mal kooperierte. Falls ja, in State 2 wechseln, sonst in State 4 wechseln.
  if(state==3){

    #1. Kooperationsrunde
    if(cop==4){ #cop = Zähler für restliche Runden in Kooperations-State
      return(list(a="C",state=state,z=z,cop=cop-1,wdh=wdh,memoryJ=memoryJ))
    }

    #ab Runde 2:
    if(cop>0&cop<4){

      #Zähler z für aufeinanderfolgende Defektionen des Gegeners in den vorherigen Runden aufbauen
      if(obs$a[j]=="D"){z=z+1}
      else{z=0}

      #Runde 4: Defektieren
      if(cop==1){
        return(list(a="D",state=state,z=z,cop=cop-1,wdh=wdh,memoryJ=memoryJ))
      }
      #Runde 2 und 3: Kooperieren
      else{
        return(list(a="C",state=state,z=z,cop=cop-1,wdh=wdh,memoryJ=memoryJ))
      }
    }

    #5. Runde. Hier wird der State gewechselt. Dazu Gegnerverhalten der letzten 3 Runden prüfen.
    else{

      #Bei drei gegnerischen Defektionen in Folge, in State 4 wechseln.
      if(z==3){state=4}

      #Falls mind. 1 mal kooperierte, in State 2 zurück wechseln.
      #Dieses Zurückwechseln ist maximal 5 mal erlaubt. Beim 6. mal wird in State 4 gewechselt.
      else{
        wdh=wdh+1 #Zähler, wie oft in State 2 zurück gewechselt wird
        if(wdh==6){state=4}
        else{return(list(a=obs$a[j],state=2,z=0,cop=4,wdh=wdh,memoryJ=memoryJ))}
      }
    }
  }

  ##################################################
  ##State=4: Immer Defektieren
  #############################
  if(state==4){
    return(list(a="D",state=state,z=z,cop=cop,wdh=wdh,memoryJ=memoryJ))
  }
}

pudb.strat2 = function(obs,i,t,obs.i=0,obs.j=0,beleidigt = FALSE,...){

  # In der ersten Runde kooperieren wir immer
  if(t==1){
    return(list(a="C",obs.i=obs.i,obs.j=obs.j, beleidigt = beleidigt))
  }

  # Ab Runde 2 wird ein Gedächtnis gebildet, wobei 3-i den Gegner angibt
  obs.i[t-1] = obs$a[i]
  obs.j[t-1] = obs$a[3-i]

  # Auch in der zweiten Runde kooperieren wir immer
  if(t == 2){
    return(list(a="C",obs.i = obs.i, obs.j = obs.j,beleidigt = beleidigt))
  }

  # Defektiert der Gegner zu Beginn 2 mal, so sind wir beleidigt und defektieren
  if(t==3){
    if(obs.j[t-1] == "D" & obs.j[t-2] == "D"){
      beleidigt = TRUE
    }
  }

  # kooperiert der Gegner in der vorherigen Runde, so sind wir nicht mehr beleidigt
  if(obs.j[t-1] == "C"){
    beleidigt = FALSE
  }

  # Reagiert der Gegner auf eine Kooperation zu oft mit Defektion, so sind wir ab
  # jetzt beleidigt und defektieren ab jetzt in jeder Runde
  if(t>20){
    sum.def = sum(obs.i[(1):(t-2)] == "C" & obs.j[2:(t-1)]=="D")
    sum.coop = sum(obs.i == "C")
    if(sum.coop>0){
      if((sum.def/sum.coop) > 0.7){
        beleidigt = TRUE
      }
    }
  }

  # defektiere immer, wenn wir einmal beleidigt sind
  if(beleidigt){
    return(list(a="D",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
  }

  # Defektiert der Gegner in den letzten 3 Runden, so defektieren wir nun mit einer
  # Wahrscheinlichkeit von 80% und kooperieren mit Wahrscheinlichkeit 20%
  if(t>3){
    if(obs.j[t-1] == "D" & obs.j[t-2]=="D" & obs.j[t-3]=="D"){
      if(runif(1)<=0.8){
        return(list(a="D",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
      } else{
        return(list(a="C",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
      }
    }

  }


  # Hat der Gegner in den letzten 5 runden mindestens 3 mal defektiert, so
  # defektieren wir mit einer Wahrscheinlichkeit von 15%
  if(t>5){
    sum.def = sum(obs.j[(t-5):(t-1)]=="D")
    if(sum.def >= 3){
      if(runif(1) <= 0.15){
        return(list(a="D",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
      }
    }
  }

  # Regiert der Gegner auf eine Kooperation von uns mit Defektion, so
  # defektiern wir mit einer Wahrscheinlichkeit von 45%
  if(obs.j[t-1] == "D" & obs.i[t-2] == "C"){
    if(runif(1)<=0.45){
      return(list(a="D",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
    }
  }

  # trifft keine der obigen Bedingungen ein, so kooperieren wir in jedem Fall.
  return(list(a="C",obs.i = obs.i, obs.j = obs.j, beleidigt = beleidigt))
}

nottitfortat = function(obs,i,t,CountC=0,CountD=0,...){

  debug.store("nottitfortat",i,t)
  debug.restore("nottitfortat",i=2,t=2)

  j=3-i

  # Kooperiere in der ersten Runde
  if (t==1) {

    return(list(a="C", CountC=CountC, CountD=CountD))
  }

  # Kooperiere in den weiteren 9 Runden und zähle wie oft der Gegner kooperiert (CountC) bzw.
  # defektiert (CountD)
  else if (1 < t && t < 11) {
    if(obs$a[j]=="C"){
      CountC=CountC+1
    }else{
      CountD=CountD+1
    }
    return(list(a="C", CountC=CountC, CountD=CountD))
  }

  # Zähle ab Runde 11 weiter wie oft der Gegner kooperiert bzw. defektiert
  else {
    if (obs$a[j] == "C") {
      CountC = CountC + 1
    }
    else{
      CountD = CountD + 1
    }

    # Defektiere nun, wenn die Summe der gegnerischen Kooperationen aus den vorherigen Runden grö?er ist
    if ( CountC > CountD) {
      return(list(a="D",CountC=CountC, CountD=CountD))
    }

    # Wenn die Summe der gegnerischen Defektionen aus den vorherigen Runden grö?er ist, defektiere zu 80%
    # und kooperiere zu 20%
    else if ( CountC < CountD) {
      if (runif(1)<80) {
        a = "D"
      } else {
        a = "C"
      }
      return(list(a=a,CountC=CountC, CountD=CountD))
    }

    # Defektiere nun, wenn die Summe der gegnerischen Defektionen gleich der gegnerischen Kooperationen ist
    else {
      return(list(a="D",CountC=CountC, CountD=CountD))
    }
  }
}

prof.strat = function(obs,i,t, net.nice=0,k=1, ...) {
  if (t==1) {
    return(nlist(a="C",net.nice,k))
  }
  a = obs$a
  j = 3-i
  a.num = ifelse(a=="C",1,0)
  net.nice = net.nice + a.num[i]-a.num[j]
  if (net.nice <= k) {
    return(nlist(a="C",net.nice,k))
  }
  return(nlist(a="D",net.nice,k))
}

schachmatt_tournament = function(obs,i,t,memoryI=0,memoryJ=0,...) {

  # 1. Runde: Kooperation
  if (t==1) {
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }

  # Beginne ab Runde 2 das Gedaechtnis zu bilden. Gespeichert werden die Handlungen
  # beider Spieler in der vorherigen Runde
  # dabei steht 1 für Kooperation und 0 fuer Defektion
  # Definition der Parameter
  j = 3-i

  if(obs$a[i]=="C"){
    memoryI[t-1]=1
  }
  else{
    memoryI[t-1]=0
  }

  if(obs$a[j]=="C"){
    memoryJ[t-1]=1
  }
  else{
    memoryJ[t-1]=0
  }

  Def=1-mean(memoryJ)    # Def = Prozentzahl der Defektion

  # 2. & 3. Runde: Kooperation
  if(t<4){
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }

  #Runden 4-10:
  #Sammeln von Informationen ueber die andere Strategie
  #dafuer kooperieren wir mit nicht unkooperativen Strategien
  if(t<=10){
    if(Def <= 0.6){
      return (list(a="C",memoryI=memoryI,memoryJ=memoryJ))
    }
    else{
      return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
    }
  }

  #Auswertung der gesammelten Daten
  #Analyse von reaktionären Strategien
  k=5 #Anzahl der Zuege die max betrachtet werden sollen
  mat=matrix(0,nrow=t-2,ncol=k)
  for (l in 1:k){
    for (m in 1:(t-l-1)){
      #wir lassen die neuste Beobachtung des Gegners (unser letzter Zug) weg, da er noch nicht darauf reagiert hat.
      mat[m,l]=mean(memoryI[m:(l+m-1)])
    }
  }
  mat2=matrix(0,nrow=k,ncol=k)
  if (var(mat[,1])*var(memoryJ[2:(t-1)])!=0){
    mat2[1,1]=cor(mat[,1],memoryJ[2:(t-1)],method="pearson")
  }
  else{
    mat2[1,1]=0
  }
  koeff=c(1,1,mat2[1,1])
  # l Anzahl der Runden, die unser Gegner betrachtet
  for(l in 2:k){
    # m Anzahl der Koop davon
    for(m in 1:l){
      if(var(round(mat[-((t-l):(t-2)),l]+0.5001-m/l))*var(memoryJ[(l+1):(t-1)])!=0){
        mat2[m,l]=cor(round(mat[-((t-l):(t-2)),l]+0.5001-m/l),memoryJ[(l+1):(t-1)],method="pearson")
        if(mat2[m,l]>koeff[3]){
          koeff=c(l,m,mat2[m,l])
        }
      }
      else{
        mat2[m,l]=0
      }
    }
  }
  # im Vektor koeff sind drei Zahlen gespeichert
  # koeff[1] gibt die Zahl an, wie viele Runden unser Gegner betrachtet
  # koeff[2] gibt die Zahl an, wie viele daraus Kooperationen sein muessen, damit unser Gegner auch kooperiert
  #          (falls koeff[3]<0)
  # koeff[3] gibt den Korrelationskoeffizienten an. Je groess?er koeff, desto sicherer besteht eine Korrelation.
  #          Falls koeff[3]<0, spielt unser Gegner immer das Gegenteil von dem, was er beobachtet hat.
  #
  # gegen tit for tat kommt beispielsweise (1,1,0.72) zurueck. Unser Gegner betrachtet einen Zug, dabei muss mind. eine
  # Kooperation sein, mit einer "Wahrscheinlichkeit" von 72%
  # gegen drei.aus.fuenf kommt beispielsweise (5,3,0.73) zur?¼ck.


  #Analyse der Strategien, die ein Muster spielen
  # k ist die max Musterlänge, die betrachtet werden soll
  k=min(10,round(t/2.5))
  musterkoeff=c(0,0,0,0)
  #l moegliche Länge solch eines Musters
  for (l in 2:k){
    #m Anzahl der Defektionen pro Muster
    for(m in 1:(l-1)){
      hilfsvektor=rep(c(rep(0,m),rep(1,(l-m))),(round(t/l)+2))
      #n Stelle an der das Muster beginnt
      for (n in 1:l){
        if(var(memoryJ)*var(hilfsvektor[n:(t-2+n)])!=0){
          if(abs(cor(memoryJ,hilfsvektor[n:(t-2+n)],method="pearson"))>musterkoeff[3]){
            musterkoeff=c(l,m,cor(memoryJ,hilfsvektor[n:(t-2+n)],method="pearson"),n)
          }
        }
      }
    }
  }

  # musterkoeff[1] zeigt die Laenge des Musters an
  # wenn musterkoeff[3]>0, zeigt musterkoeff[2] an, mit wie vielen Defektionen das Muster startet
  # wenn musterkoeff[3]<0, zeigt musterkoeff[2] an, mit wie vielen Kooperationen das Muster startet
  # musterkoeff[3] beinhaltet den Korrelationskoeffizienten
  # musterkoeff[4] zeigt an, an welcher Stelle das Muster beginnt


  ###################
  #####Strategie#####
  ###################

  #Absicherung gegen Strategien, die zu oft defektieren
  #zu oft entspricht einer gespielten Kooperationsrate von 25%, d.h. einer beobachteten von 18.75%
  if (Def >= 0.8125 | mean(memoryJ[(t-7):(t-1)])<0.1 ){
    return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
  }

  # Falls wir gegen Tit.for.tat spielen, spielen wir always.coop
  if (koeff[1]==1 & koeff[2]==1 & koeff[3]>=0.6){
    return (list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }

  #gegen Strategien, die ein Muster spielen
  if(koeff[3]-abs(musterkoeff[3])+0.09<0 & abs(musterkoeff[3])>0.6){
    if(musterkoeff[3]>0){
      wert=(t-musterkoeff[1]+musterkoeff[4]-1)/musterkoeff[1]-round((t-musterkoeff[1]+musterkoeff[4]-1)/musterkoeff[1])
      if(wert<=0){
        wert=wert+1
      }
      if (wert==1){
        return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      if(wert>(musterkoeff[2]/musterkoeff[1]+0.01)){
        return (list(a="C",memoryI=memoryI,memoryJ=memoryJ))
      }
      else{
        return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
    }
    else{
      wert=(t-musterkoeff[1]+musterkoeff[4]-1)/musterkoeff[1]-round((t-musterkoeff[1]+musterkoeff[4]-1)/musterkoeff[1])
      if(wert<=0){
        wert=wert+1
      }
      if (wert==1){
        return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      if(wert>(musterkoeff[2]/musterkoeff[1]+0.01)){
        return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
      }
      else{
        return (list(a="C",memoryI=memoryI,memoryJ=memoryJ))
      }
    }
  }


  # Falls wir gegen einen Gegner spielen, der auf unsere Zuege reagiert
  # betrachte die Beobachtungen unseres Gegners unserer letzten Spielzuege
  if (koeff[3]>=0.5 & koeff[3]-abs(musterkoeff[3])+0.09>0){
    if (mean(memoryI[(t-koeff[1]):(t-1)]) > (koeff[2]+1)/koeff[1]){
      # in diesem Fall koennen wir ungestoert defektieren
      return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
    }
    else if (mean(memoryI[(t-koeff[1]):(t-1)]) < (koeff[2]-1)/koeff[1]){
      # in diesem Fall denkt unser Gegner, wir wuerden ihn ausnutzen (was wir ja niiiiieeeee machen wuerden...)
      # doch es lohnt sich auch nicht, mit ihm zu kooperieren, weil wir mind zwei Runden ausgenutzt würden
      return (list(a="D",memoryI=memoryI,memoryJ=memoryJ))
    }
    else{
      # hier koennen wir ohne Probleme kooperieren
      return (list(a="C",memoryI=memoryI,memoryJ=memoryJ))
    }
  }

  #default-Fall: keine unserer Faelle trifft zu
  return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
}

phases = function(obs,i,t,equal=TRUE,offender=FALSE,victim=FALSE,...){


  #j beschreibt Index des Gegners
  j=3-i


  # Kooperiere in der ersten Runde
  if(t==1)
    return(list(a="C",offender=offender,equal=equal,victim=victim))

  # Phase equal:
  if(equal){

    #Wir kooperieren wenn unsere beobachteten Handlungen übereinstimmen und bleiben in Phase equal.
    if(obs$a[[j]]==obs$a[[i]]){
      return(list(a="C",offender=offender,equal=equal,victim=victim))
    }

    #Spielt der Gegner eine einseitige Defektion, antworten wir mit einer Defektion und wechseln in Phase victim.
    else if(obs$a[[j]]=="D"){
      return(list(a="D",offender=FALSE,equal=FALSE,victim=TRUE))
    }

    #Spielen wir die einseitige Defektion, antworten wir mit einer Kooperation und welchseln in den Status offender.
    else if (obs$a[[i]]=="D"){
      return(list(a="C",victim=FALSE,equal=FALSE,offender=TRUE))
    }
  }

  #Phase victim: Wir defektieren solange bis unser Gegner Koopertiert.
  if(victim){

    # Defektiert der Gegner, defektieren wir zurück und bleiben in Phase Victim
    if(obs$a[[j]]=="D"){
      return(list(a="D",offender=offender,equal=equal,victim=victim))
    }

    # Koopertiert der Gegner, kooperieren wir zurück und wechseln in Phase equal.
    else{
      return(list(a="C",offender=FALSE,victim=FALSE,equal=TRUE))
    }
  }

  #Phase offender: Wir bleiben solange in dieser Phase und kooperieren, bis eine erfolgreiche Kooperation entsteht.
  if(offender){

    # ?nderung der Phase bei einvernehmlicher Kooperation, wechsel in Phase equal
    if(obs$a[[j]]=="C" && obs$a[[i]]=="C"){
      return(list(a="C",victim=FALSE,offender=FALSE,equal=TRUE))
    }
    # Kooperieren und in Phase bleiben
    else{
      return(list(a="C",offender=offender,equal=equal,victim=victim))
    }
  }

  #nicht notwendig
  else{
    return(list(a="C",offender=offender,equal=equal,victim=victim))
  }
}

false.friend = function (obs,i,t,memoryJ=0,...) {

  j = 3 - i    #j beschreibt Index des Gegners


  if(t %% 8 == 0){    #schaut ob acht Teiler der aktuellen Rundenzahl ist

    memoryJ[t-1]=obs$a[j]
    return(list(a="D",memoryJ=memoryJ)) # wenn ja: Strategie defektiert

  } else if (t < 6 ) {
    memoryJ[t-1]=obs$a[j]
    return(list(a="C",memoryJ=memoryJ)) # Kooperation in Runden 1-6



    # Kooperation, falls Gegner min. 2 der letzten sechs Runden kooperiert hat
  } else {

    memoryJ[t-1]=obs$a[j]

    # (letzte 6 Aktionen des Gegners werden aus dem Ged?chtnis herausgenommen)
    shortmemory=memoryJ[(length(memoryJ)-5):length(memoryJ)]

    if(length(which(shortmemory=="C"))>=2){
      return(list(a="C",memoryJ=memoryJ))

    } else { # ansonsten defektieren wir
      return(list(a="D",memoryJ=memoryJ))
    }
  }
}

Globaler.Tit.4.Tat = function(obs,i,t,memoryI=0, memoryJ=0,...) {

  j = 3-i

  # Beginne ab Runde 2 das Gedächtnis zu bilden. Gespeichert werden die beobachteten Handlungen
  # beider Spieler in der vorherigen Rund

  if (t>1){
    memoryI[t-1]=obs$a[i]
    memoryJ[t-1]=obs$a[j]
  }

  # Kooperiere in der ersten Runde
  if (t==1)
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))

  # Spiele abhängig vom Gegnerverhalten der bisherigen Spiele
  if((length(which(memoryJ=="C"))>=(length(which(memoryI=="C"))))){

    # Kooperiere,wenn es gleich oder mehr Kooperationen bei dem Gegner gab
    return(list(a="C",memoryI=memoryI,memoryJ=memoryJ))
  }
  else{
    # Defektiere, wenn Gegner weniger Kooperiert als wir
    return(list(a="D",memoryI=memoryI,memoryJ=memoryJ))
  }
}

viva.PD.Strategy = function(obs,i,t,countD=0,countC=0,memoryJ=0,...){
  j=3-i

  # erste Runde, kooperieren, memory auf C setzen
  if(t==1){
    return(list(a="C",countD=countD,countC=countC, memoryJ=memoryJ))
    memoryJ[1]="C"
  }

  # alle weiteren Runden: zunächst Beobachtung der letzten Runde speichern und falls Gegner in der letzten Runde defektiert, erhöhe Zähler für gegnerische Defektierungen bzw. bei Kooperation erhöhe Zähler für Kooperationen um eins
  else{
    memoryJ[t-1]=obs$a[j]
    if(obs$a[j]=="D"){
      countD=countD+1
    }
    else{
      countC=countC+1
    }

    # ab der fünften Runde: prüfe zusätzlich ob Gegner in den letzten fünf Runden mehr als zwei mal defektiert hat und falls ja, defektiere
    if(t>=5){
      shortmemory=memoryJ[(length(memoryJ)-4):length(memoryJ)]
      if(length(which(shortmemory=="D"))>=3){
        return(list(a="D",countD=countD,countC=countC, memoryJ=memoryJ))
      }
    }

    # ermittle gesamte "Defektierungs-" und "Kooperationsquote" des Gegners in allen Runden und prüfe, welche überwiegt. Dann: Falls Defektierungen bisher überwiegen: defektiere, Kooperation analog
    probD=(countD/(countD+countC))
    probC=(countC/(countD+countC))
    if(probC<=probD){
      return(list(a="D",countD=countD,countC=countC, memoryJ=memoryJ))
    }
    else{
      return(list(a="C",countD=countD,countC=countC, memoryJ=memoryJ))
    }
  }
}

########################## EURE STRATEGIEN ##################################
squishy.the.octopus = function(obs,i,t,preObs=c("C","C"),ans="C",preAns="C",defectTotal=0,defectConsecutive=0,stopLoss=FALSE,...) {

  # Variable Definitions
  answer="C"
  j = 3-i

  # Constants that control the stopLoss system
  consecutiveStopLossBarrier=3
  if(t>10){
    consecutiveStopLossBarrier=4
  }
  relativeStopLossBarrier=0.25+0.05+max(0,(18-t)*0.02)

  # debug.store("tit.for.tat",i,t) # Store each call for each player
  # debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t

  # Find out if other player punished me because of an observation error
  obserr=FALSE;
  if(preObs[1]=="D" & obs$a[j]=="D" & preAns=="C"){
    obserr=TRUE;
  }

  # Update extra variables
  preObs=c(obs$a[i],obs$a[j])
  preAns=ans

  # Cooperate in the first period
  if (t==1)answer="C"

  # Alway cooperate in the early game
  if(! stopLoss){

    # Register defections that are NOT caused by observation error
    if(obs$a[j]=="D" & (!obserr)){
      defectTotal=defectTotal+1
      defectConsecutive=defectConsecutive+1
    }
    else{
      defectConsecutive=0
    }

    # Activate the StopLoss after x unprovoked defections in a row
    if(defectConsecutive>consecutiveStopLossBarrier){
      stopLoss=TRUE
    }

    # Activate the stop loss if the unprovoked defection quota exceeds the error probability significantly
    if(t>=5 & ((defectTotal/t)>relativeStopLossBarrier)){
      stopLoss=TRUE
    }

    # Play always cooperate until the game ends or the stop loss is triggered
    if(! stopLoss){
      return(list(a="C",preObs=preObs,preAns=preAns,ans="C",defectTotal=defectTotal,defectConsecutive=defectConsecutive,
                  stopLoss=stopLoss))
    }
  }

  # Play noise-safe tit-for-tat after the stop loss barrier was triggered:
  # Be forgiving if a previous defection was caused by an observation error.
  # Otherwise play tit-for-tat
  if (obs$a[j]=="C" | obserr){
    answer="C"
  }
  else{
    answer="D"
  }


  return(list(a=answer,preObs=preObs,preAns=preAns,ans=answer,defectTotal=defectTotal,defectConsecutive=defectConsecutive,
              stopLoss=stopLoss))
}

into.spaaaace = function(obs,i,t,preObs=c("C","C"),ans="C",preAns="C",...) {

  # Variable Definitions
  answer="C"
  j = 3-i

  # debug.store("tit.for.tat",i,t) # Store each call for each player
  # debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t

  # Find out if other player punished me because of an observation error
  obserr=FALSE;
  if(preObs[1]=="D" & obs$a[j]=="D" & preAns=="C"){
    obserr=TRUE;
  }

  # Cooperate in the first period
  if (t==1)answer="C"

  # Be forgiving if a previous defection was caused by an observation error.
  # Otherwise play tit-for-tat

  if (obs$a[j]=="C" | obserr){
    answer="C"
  }
  else{
    answer="D"
  }

  preObs=c(obs$a[i],obs$a[j])
  preAns=ans

  return(list(a=answer,preObs=preObs,preAns=ans,ans=answer))
}


screams.in.space = function(obs,i,t,preObs=c("C","C"),ans="C",preAns="C", count.devious=0,clueless=0,count.obserr=0,...) {

  # Variable Definitions
  answer="C"
  j = 3-i

  # debug.store("tit.for.tat",i,t) # Store each call for each player
  # debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t
    # Find out if other player punished me because of an observation error
    obserr=FALSE;
    if(preObs[1]=="D" & obs$a[j]=="D" & preAns=="C"){
      obserr=TRUE;
      count.obserr = count.obserr+1
    }

    # Cooperate in the first period
    if (t==1)answer="C"

    # Be forgiving if a previous defection was caused by an observation error.
    # Otherwise play tit-for-tat

    if (obs$a[j]=="C" | obserr){
      if(obs$a[j]=="C" && obserr){ # the other one did not retaliate
        clueless = clueless+1
      }
      answer="C"
    }
    else{
      answer="D"
      if(preObs[1]=="C"){ # I really wanted this to be a nice thing and he saw that
        count.devious = count.devious+1
      }
    }

  if(t>=10){
    if(count.devious/t>0.4){ #the other one is really not nice
      answer="D"
    }
    if(clueless>count.obserr/2){ #more than half of the times there is no retaliation if obs.err
      answer="D"
    }
  }

  preObs=c(obs$a[i],obs$a[j])
  preAns=ans

  return(list(a=answer,preObs=preObs,preAns=ans,ans=answer, count.devious=count.devious, clueless=clueless, count.obserr=count.obserr))
}
