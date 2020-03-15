const int leds[]={0,10,11};
const int rButtonPin=12;
const int lButtonPin=13;
boolean rPressed;
boolean lPressed;
int game[]={1,2,2,1,1,1,2,2,1,2};
void generate(){
  delay(10);
  randomSeed(millis());
  for(int i=0;i<10;i++){
    game[i]=random()%2+1;
  }
}
void setup(){
  rPressed=false;
  lPressed=false;
  generate();
  int sz=sizeof(game)/sizeof(game[0]);
  buttonPress();
  for(int len=1;len<=sz;len++){
    for(int i=0;i<len;i++){
      ledBlink(leds[game[i]]);
    }
    for(int i=0;i<len;i++){
      int react=buttonPress();
      if(react!=game[i]){
        digitalWrite(leds[1],HIGH);
        digitalWrite(leds[2],HIGH);
      }
    }
    delay(1000);
  }
}
void loop(){}
int buttonPress(){
  while(true){
    if(digitalRead(rButtonPin)==LOW&&rPressed==true){
      rPressed=false;
      return 1;
    }
    if(digitalRead(lButtonPin)==LOW&&lPressed==true){
      lPressed=false;
      return 2;
    }
    if(digitalRead(rButtonPin)==HIGH)rPressed=true;
    if(digitalRead(lButtonPin)==HIGH)lPressed=true;
  }
  return 0;
}
void ledBlink(int led){
  digitalWrite(led,HIGH);
  delay(350);
  digitalWrite(led,LOW);
  delay(100);
}
