// *************************************************************
//
//     Branching-time reasoning for infinite-state systems
//
//              Byron Cook * Eric Koskinen
//                     July 2010
//
// *************************************************************

// -ctl "EF{resp >= 5}"
// -precondition "c > 5"

int _nondet_int() {}

void main() {
  int c; // assume c > 0
  int servers = 4;
  int resp = 0;
  int curr_serv = servers;

  while(curr_serv > 0) {
    if(_nondet_int()) {
      c--; 
      curr_serv--;
      resp++;
    } else if (c < curr_serv) {
      curr_serv--;
    }
  }
  while(1) { int ddd; ddd=ddd; }
}
