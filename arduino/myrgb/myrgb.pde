int pins[]={10,9,11};
int on[]={255,255,0};
void setup(){
  for(int i=0;i<3;i++){
    pinMode(pins[i],OUTPUT);
    analogWrite(pins[i],on[i]);
  Serial.begin(9600);
  //up(0);
  //up(1);
}
void up(int i){
 for(on[i]=0;on[i]<256;on[i]++){
  analogWrite(pins[i],on[i]);
  delay(5);
 } 
}
void down(int i){
 for(on[i]=255;on[i]>=0;on[i]--){
  analogWrite(pins[i],on[i]);
  delay(5);
 } 
}
void updown(int i,int j){
 for(int p=0;p<255;p++){
  analogWrite(pins[i],on[i]++);
  analogWrite(pins[j],on[j]--);
  delay(10);
 } 
}
void loop(){
  
  //down(0);
  //up(2);
  updown(2,0);
  //down(1);
  //up(0);
  updown(0,1);
  //down(2);
  //up(1);
  updown(1,2);
}
