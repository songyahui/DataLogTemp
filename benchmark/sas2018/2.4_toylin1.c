// *************************************************************
//
//     Branching-time reasoning for infinite-state systems
//
//              Byron Cook * Eric Koskinen
//                     July 2010
//
// *************************************************************

// -ctl "EF{resp >= 5}"
// -precondition "c >= 5"

int _nondet_int() {}

void main() {
  int c; // assume c > 0
  int servers = 5;
  int resp = 0;
  int curr_serv = servers;

  while(curr_serv > 0) { // term : c >= curr_serv ->,  resp=5 
    if(_nondet_int()) {  // term : c < curr_serv ->  curr_serv-c <= resp <= 5
      c--; 
      curr_serv--;
      resp++;
    } else if (c < curr_serv) {
      curr_serv--;
    }
  }
  while(1) { int ddd; ddd=ddd; } // non-term : true 
}
